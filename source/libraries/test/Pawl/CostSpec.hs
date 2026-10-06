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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Natural as Natural.Extra
import qualified Pawl.Registry as Registry
import qualified Pawl.Scenario as Scenario
import qualified Pawl.Spec as Spec
import Pawl.SpecialActionSpec (humiliatedBoard)
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Activator as Activator
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostDirection as CostDirection
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterSpread as CounterSpread
import qualified Pawl.Types.Departure as Departure
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaOption as ManaOption
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Move as Move
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
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
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.ReturnPermanents as ReturnPermanents
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Seat as Seat
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Staged as Staged
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapPermanents as TapPermanents
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.Zone as Zone

-- The single activated ability of a printing. Total: the fallback is unreachable
-- in these fixtures. Duplicated per this suite's convention of group-local
-- helpers (ActivateSpec and ReplacementSpec each carry their own).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) [] 0 (Face.spell (S.combinedFace p)) [] Activator.Controller Nothing Nothing Nothing

-- CR 607.2d: Doom Cannon's "{3}, {T}, Sacrifice a creature of the chosen type:
-- This artifact deals 3 damage to any target" is linked to its "As this artifact
-- enters, choose a creature type", so the sacrifice pool reads the type the
-- Cannon chose (Oracle checked against Scryfall on 2026-10-01). The choice is
-- stamped rather than cast for; the entry road that writes it is Obelisk of
-- Urd's (Pawl.ProjectionSpec).
--
-- A PAIR OF BOARDS differing only in the type chosen, each with a Goblin Piker
-- and a Hill Giant beside the Cannon: whichever the choice names is the one
-- creature offered, so CR 701.21a's prompt is elided and the one that goes is
-- the one the rule picks. The 3 damage goes to bob.
doomCannonSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
doomCannonSpec s registry =
  Spec.it s "CR 607.2d Doom Cannon sacrifices only a creature of the type it chose" $ do
    cannon <- S.printingOf s registry "Doom Cannon"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    let (cannonId, g0) = S.addPermanent cannon S.alice (S.landsFor plains S.alice 3 (Setup.emptyGame S.bothPlayers))
        (pikerId, g1) = S.addPermanent piker S.alice g0
        (giantId, g2) = S.addPermanent giant S.alice g1
        aimAtBob p = case p of
          Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just S.bob) . Recipient.playerOf) sets
          _ -> S.identityAnswer p
        fired chosen =
          let chose = g2 {GameState.objects = Map.adjust (\o -> o {Object.chosenSubtype = Just chosen}) cannonId (GameState.objects g2)}
           in S.runPure S.identityAnswer (S.runPure aimAtBob chose (Activate.activateAbility S.alice cannonId (theAbility cannon))) Stack.resolveTop
        goblins = fired Subtype.Goblin
        giants = fired Subtype.Giant
    Spec.assertBool s (not (S.onBattlefield pikerId goblins)) "having chosen Goblin, the Cannon sacrifices the Piker"
    Spec.assertBool s (S.onBattlefield giantId goblins) "and keeps the Giant"
    Spec.assertBool s (not (S.onBattlefield giantId giants)) "having chosen Giant, it sacrifices the Giant"
    Spec.assertBool s (S.onBattlefield pikerId giants) "and keeps the Piker"
    Spec.assertEqWith s "the shot resolved at bob" (S.lifeOf S.bob goblins) (Just 17)

doorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
doorSpec s registry =
  Spec.describe s "Door" $ do
    -- CR 118.3's own second example: "a permanent that's already tapped can't
    -- be tapped to pay a cost" (CR 107.5 says the same for the {T} symbol).
    Spec.it s "CR 107.5 TapThis is payable only while the permanent is untapped" $ do
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      let (oid, gs) = S.addPermanent prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
          tapped = S.tapObject oid gs
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice oid CostComponent.TapThis gs) "untapped pays"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice oid CostComponent.TapThis tapped)) "tapped does not"
    -- CR 701.21a: "A player can't sacrifice something that isn't a permanent,
    -- or something that's a permanent they don't control."
    Spec.it s "CR 701.21a SacrificeThis needs a permanent this player controls" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (onField, gs0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
          (inHand, gs1) = S.addHandCard piker S.alice gs0
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice onField CostComponent.SacrificeThis gs1) "a controlled permanent pays"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice inHand CostComponent.SacrificeThis gs1)) "a card in hand does not"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.bob onField CostComponent.SacrificeThis gs1)) "another player's permanent does not"
    -- CR 406.2 charged against the permanent the cost is on: SacrificeThis' two
    -- conjuncts above, on the same three readings, and WITHOUT CR 101.2's
    -- prohibition -- an effect forbidding a sacrifice says nothing about an
    -- exile. Read here rather than at gameplay level because no board Brittle
    -- Effigy can build separates them: Activate already withholds an ability
    -- whose source this player does not control.
    Spec.it s "CR 406.2 ExileThis needs a permanent this player controls" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (onField, gs0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
          (inHand, gs1) = S.addHandCard piker S.alice gs0
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice onField CostComponent.ExileThis gs1) "a controlled permanent pays"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice inHand CostComponent.ExileThis gs1)) "a card in hand does not"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.bob onField CostComponent.ExileThis gs1)) "another player's permanent does not"
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
      let (oid, gs) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice oid (CostComponent.Blight 1) gs) "a creature its payer controls pays"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.bob oid (CostComponent.Blight 1) gs)) "an opponent's creature does not"
      -- CR 122.6 puts any number of counters on any creature, so no N outruns a
      -- 2/1 -- rule 701.68b's "unable" has only the cause the rule itself names.
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice oid (CostComponent.Blight 9) gs) "and no N is too large for a candidate that exists"
    -- CR 702.29a's "Discard this card", the exact mirror of SacrificeThis
    -- above: one names a permanent its controller owns the choice of, the other
    -- names a card in a hand. Asked of the ZONE and the OWNER, because CR 108.4
    -- gives a card in a hand no controller for a control-shaped gate to read.
    --
    -- Tested directly rather than only through cycling, because the two gates
    -- an activation passes -- this one and Activatable.abilitiesFor's -- would
    -- otherwise cover for each other, and either alone would look correct.
    Spec.it s "CR 702.29a DiscardThis needs the card in this player's hand" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (onField, gs0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
          (inHand, gs1) = S.addHandCard piker S.alice gs0
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice inHand (CostComponent.DiscardThis DiscardCause.Ordinary) gs1) "a card in hand pays"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice onField (CostComponent.DiscardThis DiscardCause.Ordinary) gs1)) "a permanent does not"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.bob inHand (CostComponent.DiscardThis DiscardCause.Ordinary) gs1)) "and it is not the other player's to discard"
    -- CR 701.9a through Event.changeZone, the CR 400.7 funnel: the discarded
    -- card lands in its owner's graveyard as a new incarnation, so the old id
    -- is gone rather than moved.
    Spec.it s "CR 701.9a paying DiscardThis puts that card in the graveyard" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (inHand, gs0) = S.addHandCard piker S.alice (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs0 (Cost.payComponent PaymentMoment.OutsideResolution Map.empty S.alice inHand (CostComponent.DiscardThis DiscardCause.Ordinary))
      Spec.assertEqWith s "the hand is empty" (length (Game.zoneMembers Zone.Hand S.alice after)) 0
      Spec.assertEqWith s "and the card is in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    -- CR 118.6 vs CR 118.5a: the distinction the Maybe carries. Nothing is an
    -- unpayable cost; an empty ManaCost is {0} and is payable.
    Spec.it s "CR 118.6 an unpayable cost can never be paid" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs = S.landsInPlay mountain 5
      Spec.assertBool
        s
        (not (Cost.canPay PaymentSubject.ForNeither S.alice S.noSource (Cost.Type.MkCost Nothing []) gs))
        "Nothing is unpayable"
    Spec.it s "CR 118.5a a {0} cost is payable" $
      let gs = Setup.emptyGame S.bothPlayers
       in Spec.assertBool
            s
            (Cost.canPay PaymentSubject.ForNeither S.alice S.noSource (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) gs)
            "an empty ManaCost is {0}"
    -- CR 118.6a: "If an unpayable cost is increased by an effect or an
    -- additional cost is imposed, the cost is still unpayable." total maps over
    -- the Maybe, so there is no special case to get wrong.
    Spec.it s "CR 118.6a Thalia's increase leaves an unpayable cost unpayable" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let base = S.landsInPlay mountain 5
          (_, gs) = S.addPermanent thalia S.alice base
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
          (slaver, gs1) = S.addPermanent mindslaver S.alice base
          (_, gs2) = S.addPermanent thalia S.alice gs1
      Spec.assertBool
        s
        (Activatable.activatable S.alice slaver (theAbility mindslaver) gs2)
        "four Mountains still pay {4}"
    -- Departure 2: an Unpaid payment is a complete no-op, never a partial one.
    Spec.it s "CR 118.6 paying an unpayable cost changes nothing" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs = S.landsInPlay mountain 3
          (outcome, after) = S.runPureWith S.identityAnswer gs (Cost.pay S.manaPerformer gs PaymentMoment.OutsideResolution PaymentSubject.ForNeither Nothing ManaSpending.AsProduced S.alice S.noSource (Cost.Type.MkCost Nothing []))
      Spec.assertEqWith s "Unpaid" outcome Payment.Unpaid
      Spec.assertEqWith s "no land tapped" (S.tappedCount S.alice after) 0
    -- CR 701.21a: enough controlled permanents matching the criterion.
    Spec.it s "CR 118.3 a Sacrifice component counts matching permanents this player controls" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs = S.landsInPlay mountain 2
          two = CostComponent.Sacrifice (Sacrifice.MkSacrifice 2 (Filter.Type.HasSubtype Subtype.Mountain))
          three = CostComponent.Sacrifice (Sacrifice.MkSacrifice 3 (Filter.Type.HasSubtype Subtype.Mountain))
          islands = CostComponent.Sacrifice (Sacrifice.MkSacrifice 1 (Filter.Type.HasSubtype Subtype.Island))
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice S.noSource two gs) "two Mountains pay for two"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice S.noSource three gs)) "but not for three"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice S.noSource islands gs)) "and not for an Island"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.bob S.noSource two gs)) "and bob controls none of them"
    -- CR 118.6: unpayable below the count, payable at or above it -- the same
    -- shape CR 118.3's Sacrifice test above takes, for the SPENT direction of
    -- the player-counter substrate (P10 #37 GainPlayerCounters is the ADD
    -- direction).
    Spec.it s "CR 118.6 PayEnergy is unpayable below the count and payable at or above" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
          two = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice gs0
          one = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice gs0
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice oid (CostComponent.PayEnergy 2) two) "two energy pays PayEnergy 2"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice oid (CostComponent.PayEnergy 2) one)) "one energy cannot"
    -- CR 107.14: paying energy removes exactly that many counters.
    Spec.it s "CR 107.14 paying PayEnergy removes that many energy counters" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
          three = S.addPlayerCounter PlayerCounterKind.Energy 3 S.alice gs0
          after = S.runPure S.identityAnswer three (Monad.void (Cost.payComponent PaymentMoment.OutsideResolution Map.empty S.alice oid (CostComponent.PayEnergy 2)))
      Spec.assertEqWith s "one energy left" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 1
    -- CR 118.12's counter-placing cost (CR 701.63a's endure). The gate is the
    -- permanent still being on the battlefield to take the counters, and NOT
    -- control -- Pawl.ResolveSpec's Fortress Kin-Guard cases prove the two
    -- branches at gameplay level, and this is where the control reading is ruled
    -- out, since on that card the payer is the controller either way.
    Spec.it s "CR 118.12 PutPlusOneCountersOnThis needs the permanent on the battlefield, whoever controls it" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (onField, gs0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
          (inHand, gs1) = S.addHandCard piker S.alice gs0
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice onField (CostComponent.PutPlusOneCountersOnThis 1) gs1) "a permanent on the battlefield pays"
      Spec.assertBool s (Cost.canPayComponent Map.empty S.bob onField (CostComponent.PutPlusOneCountersOnThis 1) gs1) "and pays for a player who does not control it"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice inHand (CostComponent.PutPlusOneCountersOnThis 1) gs1)) "a card in hand does not"

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
      (greedId, gs1) = S.addPermanent greed S.alice base
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
        (not (Activatable.activatable S.alice greedId (theAbility greed) gs))
        "not activatable"
      Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "no Activate action offered"
    Spec.it s "CR 119.4b at 2 life the ability IS offered" $ do
      swamp <- S.printingOf s registry "Swamp"
      greed <- S.printingOf s registry "Greed"
      piker <- S.printingOf s registry "Goblin Piker"
      let (greedId, gs) = greedBoard swamp greed piker 2
      Spec.assertBool
        s
        (Activatable.activatable S.alice greedId (theAbility greed) gs)
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
      addPiker (ids, gs) _ = let (oid, gs5) = S.addPermanent piker S.alice gs in (ids <> [oid], gs5)
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

-- Synthetic Spiteful Rite {B} Instant (data/cards/synthetic-spiteful-rite.json):
-- "As an additional cost to cast this spell, sacrifice a creature other than the
-- target. Destroy target creature." And its activation twin, Synthetic Spiteful
-- Altar {1} Artifact (data/cards/synthetic-spiteful-altar.json): "{T}, Sacrifice
-- a creature other than the target: Destroy target creature."
--
-- Synthetic because no printing's cost names its own target -- Scryfall
-- o:"additional cost" o:"other than the target", 2026-09-01, no hit; the
-- printing that would replace both is one whose additional or activation cost
-- names the spell's or ability's target -- and nothing in the rules forbids one:
-- CR 601.2c chooses the targets, CR 601.2h pays the cost, and CR 602.2b sends
-- an activation through the same steps, so "the target" is bound by the time the
-- cost asks. Pawl.Engine.Cost.pay reads the announced object's bindings for it
-- (Cost.announcedSlots).
--
-- alice controls one Swamp, a Hill Giant and `pikers` Goblin Pikers, and the
-- Rite or the Altar is placed by `place` -- S.addHandCard for the instant,
-- S.addPermanent for the artifact, which puts any printing onto the battlefield
-- (Greed above takes the same road) -- with priority in her own precombat main
-- phase. The Giant is the creature every case targets, so the Pikers are the
-- only permanents the cost may take -- and the graveyard's NAMES say which kind
-- died, which is what a count could not.
spitefulBoard ::
  (Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)) ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
spitefulBoard place swamp giant piker spiteful pikers =
  let base = S.landsInPlay swamp 1
      (giantId, withGiant) = S.addPermanent giant S.alice base
      addPiker g _ = snd (S.addPermanent piker S.alice g)
      withPikers = List.foldl' addPiker withGiant [1 .. pikers]
      (spitefulId, gs) = place spiteful S.alice withPikers
   in ( giantId,
        spitefulId,
        gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

spitefulSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spitefulSpec s registry =
  Spec.describe s "Synthetic Spiteful Rite" $ do
    -- CR 601.2 / 601.3 at the OFFER, which runs before CR 601.2c has bound
    -- anything: the gate's question is whether SOME announcement this player
    -- could make leaves the cost payable, since CR 601.2 makes a casting legal
    -- when the player can comply with every step. Two boards differing in
    -- exactly one Goblin Piker -- one Swamp, one Hill Giant and the Rite in hand
    -- on both, so the mana, the timing and the target slot are the same. With
    -- the Giant the only creature every announcement aims at it, and CR 701.21a
    -- then has nothing left to take.
    Spec.it s "CR 601.2 the cast is offered only where some target choice leaves the cost payable" $ do
      swamp <- S.printingOf s registry "Swamp"
      giant <- S.printingOf s registry "Hill Giant"
      piker <- S.printingOf s registry "Goblin Piker"
      rite <- S.printingOf s registry "Synthetic Spiteful Rite"
      let (_, aloneId, alone) = spitefulBoard S.addHandCard swamp giant piker rite 0
          (_, pairedId, paired) = spitefulBoard S.addHandCard swamp giant piker rite 1
      Spec.assertBool s (not (any (S.isCastOf aloneId) (Action.legalActions S.alice alone))) "with the target the only creature no announcement pays, so the cast is not offered"
      Spec.assertBool s (any (S.isCastOf pairedId) (Action.legalActions S.alice paired)) "and one more creature makes some announcement pay, so it is"
    -- CR 602.2b sends an activation through the same steps, and
    -- Activatable.aimingSomewhere is the gate. The same pair of boards with the
    -- Altar on the battlefield in the Rite's place.
    Spec.it s "CR 602.2b the activation is offered only where some target choice leaves the cost payable" $ do
      swamp <- S.printingOf s registry "Swamp"
      giant <- S.printingOf s registry "Hill Giant"
      piker <- S.printingOf s registry "Goblin Piker"
      altar <- S.printingOf s registry "Synthetic Spiteful Altar"
      let (_, aloneId, alone) = spitefulBoard S.addPermanent swamp giant piker altar 0
          (_, pairedId, paired) = spitefulBoard S.addPermanent swamp giant piker altar 1
      Spec.assertBool s (not (any (isActivateOf aloneId) (Action.legalActions S.alice alone))) "with the target the only creature no announcement pays, so the activation is not offered"
      Spec.assertBool s (any (isActivateOf pairedId) (Action.legalActions S.alice paired)) "and one more creature makes some announcement pay, so it is"

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

-- Cadaverous Bloom {3}{B}{G} Enchantment: "Exile a card from your hand: Add
-- {B}{B} or {G}{G}." (Oracle checked against Scryfall 2026-09-07.)
--
-- The gate card for CostComponent.ExileCardFromHand, the first component that
-- exiles a card out of a HIDDEN zone (CR 400.2) rather than a graveyard. What
-- separates it from CostComponent.DiscardCards is only the destination, so the
-- exile assertion leads every case: a payment that put the card in a graveyard
-- would satisfy every other assertion here.
--
-- Silent Arbiter is {4} and targets nothing as it is cast, so every mana on
-- these boards comes through the Bloom, and the Bloom has no {T} for CR 107.5 to
-- bar a second activation -- two FUEL cards in hand are two activations and
-- {B}{B}{B}{B}. Goblin Piker is the fuel: it is never cast, so nothing but the
-- exile can move it. The Arbiter is in that hand too and is not fuel, CR 601.2a
-- putting it on the stack before the Bloom is asked to pay (Game.beingCast).
cadaverousBloomSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
cadaverousBloomSpec s registry =
  Spec.describe s "Cadaverous Bloom" $ do
    -- The same board one fuel card short, which is the ONE thing that differs:
    -- one activation adds {B}{B} and CR 118.3 refuses the rest, so the cast is
    -- never offered. An attempt made anyway rewinds under CR 601.2, which is what
    -- the three assertions after the offer say.
    Spec.it s "CR 118.3 one fuel card in hand is one activation, and {2} does not pay {4}" $ do
      bloom <- S.printingOf s registry "Cadaverous Bloom"
      piker <- S.printingOf s registry "Goblin Piker"
      arbiter <- S.printingOf s registry "Silent Arbiter"
      let (spell, gs) = cadaverousBloomBoard bloom piker arbiter 1
          cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
      Spec.assertBool s (not (S.castable S.alice spell gs)) "CR 601.2a the cast is not offered: the Arbiter is not fuel for the Bloom"
      Spec.assertEqWith s "nothing was exiled" (length (Game.zoneMembers Zone.Exile S.alice cast)) 0
      Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertEqWith s "and both cards are still in hand" (S.handSize S.alice cast) 2

-- The Bloom on the battlefield, `fuel` Goblin Pikers in hand and the spell on
-- top of them. No lands: every mana here has to come through the Bloom.
cadaverousBloomBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (ObjectId.ObjectId, GameState.GameState)
cadaverousBloomBoard bloom piker spell fuel =
  let base = snd (S.addPermanent bloom S.alice (Setup.emptyGame S.bothPlayers))
      fuelled = List.foldl' (\gs _ -> snd (S.addHandCard piker S.alice gs)) base [1 .. fuel]
      (oid, gs1) = S.addHandCard spell S.alice fuelled
   in ( oid,
        gs1
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

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
      let onBoard board = snd (S.addPermanent urborg S.alice (S.landsFor mountain S.alice 2 (S.landsFor swamp S.alice 1 board)))
          (spell, gs) = frailExhumationBoard swamp nightmare exhumation onBoard []
          cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
      Spec.assertBool s (not (S.castable S.alice spell gs)) "not castable"
      Spec.assertEqWith s "and not offered" (filter (S.isCastOf spell) (Action.legalActions S.alice gs)) []
      Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertEqWith s "and the Nightmare is still in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 1
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
          candidates = Cost.costsFor S.alice (S.printingName fireblastPrinting) fireblast gs
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
    -- CR 113.6d: Yixlid Jailer ("Cards in graveyards lose all abilities.")
    -- wipes the Fireblast where it lies, but its alternative cost functions
    -- on the stack, where the Jailer does not reach. Yawgmoth's Will is the
    -- CR 601.3 permission, held where fireblastBoard puts its card; the
    -- boards differ only in whether bob has the
    -- Jailer, and the tapped Mountains leave the alternative the only route.
    Spec.it s "CR 113.6d under Yixlid Jailer a Fireblast cast from the graveyard still sacrifices two Mountains" $ do
      mountain <- S.printingOf s registry "Mountain"
      swamp <- S.printingOf s registry "Swamp"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      will <- S.printingOf s registry "Yawgmoth's Will"
      jailer <- S.printingOf s registry "Yixlid Jailer"
      let (willId, tapped) = fireblastBoard mountain will 2 True
          (buried, g2) = S.addGraveyardCard fireblastPrinting S.alice (S.landsFor swamp S.alice 3 tapped)
          blasted board =
            let willed = S.runPure S.identityAnswer (S.runPure S.identityAnswer board (S.cast S.alice willId)) Stack.resolveTop
             in S.runPure S.identityAnswer (S.runPure S.identityAnswer willed (S.cast S.alice buried)) Stack.resolveTop
          jailed = blasted (snd (S.addPermanent jailer S.bob g2))
          free = blasted g2
          left g = (length (Game.zoneMembers Zone.Battlefield S.alice g), length (Game.zoneMembers Zone.Graveyard S.alice g))
      Spec.assertEqWith s "CR 113.6d under the Jailer the Fireblast left the graveyard and both Mountains were sacrificed, leaving the three Swamps" (left jailed) (3, 0)
      Spec.assertEqWith s "the control: without the Jailer the same cast leaves the same board" (left free) (3, 0)
    Spec.it s "CR 118.3 one Mountain pays neither cost" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (fireblast, gs) = fireblastBoard mountain fireblastPrinting 1 True
      Spec.assertBool s (not (S.castable S.alice fireblast gs)) "not castable"

-- Phyrexian Tribute {2}{B} Sorcery: "As an additional cost to cast this spell,
-- sacrifice two creatures. Destroy target artifact." Checked against Scryfall
-- 2026-09-22; no rulings. One cost component moving several permanents is one
-- event; the batch triggers that read it are data/scenarios/cost's.
costBatchSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
costBatchSpec s registry =
  let mainPhase gs = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
      resolveAll g = if null (GameState.stack g) then g else resolveAll (S.runPure S.identityAnswer g Stack.resolveTop)
   in Spec.describe s "a cost moving several permanents" $ do
        -- Anafenza, the Foremost (Scryfall 2026-10-01): "If a nontoken creature
        -- an opponent owns would die ..., exile that card instead." Alice
        -- sacrifices her and a Piker she controls but bob owns. One event on one
        -- board, so Anafenza's replacement still applies to the Piker. Anafenza has
        -- the lower id, so she moves first.
        Spec.it s "CR 601.2h Phyrexian Tribute sacrificing Anafenza beside bob's Piker still exiles the Piker" $ do
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          anafenza <- S.printingOf s registry "Anafenza, the Foremost"
          solRing <- S.printingOf s registry "Sol Ring"
          tribute <- S.printingOf s registry "Phyrexian Tribute"
          let (anafenzaId, g0) = S.addPermanent anafenza S.alice (S.landsInPlay swamp 3)
              (pikerId, g1) = S.addPermanent piker S.bob g0
              (_, g2) = S.addPermanent solRing S.bob (S.giveControl pikerId S.alice g1)
              (tributeId, gs) = S.addHandCard tribute S.alice (mainPhase g2)
              cast = S.runPure S.identityAnswer gs (S.cast S.alice tributeId)
              after = resolveAll (S.runPure S.identityAnswer cast Engine.settleForPriority)
          Spec.assertEqWith s "CR 614.1a bob's Piker is exiled, not put into his graveyard" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
          -- The preconditions and proxies, AFTER the assertion above.
          Spec.assertBool s (anafenzaId < pikerId) "Anafenza moves first"
          Spec.assertEqWith s "both were sacrificed" (S.creaturesInPlay S.alice after) 0

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
          manaOf state = fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName asmorPrinting) asmor state)
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
    -- CR 118.6's unpayable cost put through rule 702.24a's multiplier, the half
    -- the cases above do not reach. Pawl.Engine.Cost.repeated answered CR 118.5's
    -- payable {0} at a count of zero whatever it was handed; see #2875.
    --
    -- CR 118.6a is the direction rather than a ruling on the zero count, which no
    -- printing states: it keeps an unpayable cost unpayable through an increase
    -- or an added cost, excepting only an ALTERNATIVE cost. So the fixed reading
    -- is unpayable in, unpayable out.
    --
    -- Asmoranomardicadaistinaculdacar's printed cost is the corpus's real
    -- Nothing, which is why this sits beside its alternative rather than on a
    -- hand-built value: a fixture asserting Nothing about Nothing would prove
    -- only the fixture.
    --
    -- A REGRESSION FENCE and not a gameplay case, and it cannot be more than
    -- that today: a scaled CR 118.12 cost rides Pawl.Types.PayGate.perEach, and
    -- both writers of one offer a cost with a MANA part -- Rakshasa's Disdain's
    -- {1}, rule 702.24a's printed cost -- so a count of zero reaches CR 118.5's
    -- payable {0} above rather than this rule's Nothing.
    Spec.it s "CR 118.6 scaling an unpayable cost by any count leaves it unpayable" $ do
      swamp <- S.printingOf s registry "Swamp"
      asmorPrinting <- S.printingOf s registry "Asmoranomardicadaistinaculdacar"
      vultures <- S.printingOf s registry "Circling Vultures"
      let (asmor, _, gs) = asmorBoard swamp asmorPrinting vultures
      case Cost.costsFor S.alice (S.printingName asmorPrinting) asmor gs of
        [] -> Spec.assertFailure s "the printed cost was not offered at all"
        printed : _ -> do
          Spec.assertEqWith s "the printed cost really is CR 118.6's unpayable one" (Cost.Type.mana printed) Nothing
          -- The assertion this case exists for, ahead of the counts that were
          -- already right: ZERO copies.
          Spec.assertBool s (not (Cost.canPay PaymentSubject.ForNeither S.alice asmor (Cost.repeated 0 printed) gs)) "CR 118.6 no copies of it is still unpayable, not {0}"
          Spec.assertBool s (not (Cost.canPay PaymentSubject.ForNeither S.alice asmor (Cost.repeated 1 printed) gs)) "one copy is unpayable"
          Spec.assertBool s (not (Cost.canPay PaymentSubject.ForNeither S.alice asmor (Cost.repeated 3 printed) gs)) "and three"
          -- The pair that differs in exactly one thing: the same three counts
          -- over a cost whose mana part is Just, where rule 118.5 makes the empty
          -- one payable. Without this the fix could have made every count
          -- unpayable and still been green.
          let payable = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []
          Spec.assertBool s (Cost.canPay PaymentSubject.ForNeither S.alice asmor (Cost.repeated 0 payable) gs) "CR 118.5 no copies of a PAYABLE cost is {0}, which is paid by doing nothing"
          Spec.assertEqWith s "and three copies is {1}{1}{1}" (Cost.Type.mana (Cost.repeated 3 payable)) (Just (ManaCost.MkManaCost (replicate 3 (ManaSymbol.Generic 1))))

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
          (\(oids, g) _ -> let (oid, g2) = S.addPermanent printing S.alice g in (oids <> [oid], g2))
          ([], gs0)
          (replicate n ())
      (asmor, gs1) = S.addPermanent asmorPrinting S.alice (Setup.emptyGame S.bothPlayers)
      (eggs, gs2) = addEach goldenEgg foods gs1
      (_, gs3) = addEach sphere others gs2
      (victim, gs4) = S.addPermanent childOfNight S.bob gs3
   in ( asmor,
        eggs,
        victim,
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

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
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice asmorThree component three) "three Foods pay the component"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice asmorOne component one)) "one Food beside two non-Foods does not"
      Spec.assertBool s (Activatable.activatable S.alice asmorThree ability three) "so the ability is activatable with three"
      Spec.assertBool s (not (Activatable.activatable S.alice asmorOne ability one)) "and is not with one"
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

crossCheckSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
crossCheckSpec s registry =
  Spec.describe s "CrossChecks" $ do
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
          (_, taxedTwo) = S.addPermanent thalia S.alice twoTapped
          (fireblastTwo, gsTwo) = S.addHandCard fireblastPrinting S.alice taxedTwo
          -- The same board plus one UNTAPPED Mountain, which can pay the {1}.
          (_, threeMountains) = S.addPermanent mountain S.alice twoTapped
          (_, taxedThree) = S.addPermanent thalia S.alice threeMountains
          (fireblastThree, gsThree) = S.addHandCard fireblastPrinting S.alice taxedThree
          alternativeOf oid gs = case Cost.costsFor S.alice (S.printingName fireblastPrinting) oid gs of
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
      let (cubId, base) = S.addPermanent longtuskCub S.alice (Setup.emptyGame S.bothPlayers)
          ability = theAbility longtuskCub
          withTwo = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice base
          withOne = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice base
          activated = S.runPure S.identityAnswer withTwo (Activate.activateAbility S.alice cubId ability)
          resolved = S.runPure S.identityAnswer activated Stack.resolveTop
      Spec.assertBool s (Activatable.activatable S.alice cubId ability withTwo) "payable at two"
      Spec.assertBool s (not (Activatable.activatable S.alice cubId ability withOne)) "unpayable at one"
      Spec.assertEqWith s "energy spent" (S.playerCounterOf PlayerCounterKind.Energy S.alice activated) 0
      Spec.assertEqWith s "Cub grew a +1/+1 counter" (fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject cubId resolved)) (Just 1)

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
  let add gs printing = snd (S.addPermanent printing S.alice gs)
      (bayouId, withBayou) = S.addPermanent bayou S.alice (Setup.emptyGame S.bothPlayers)
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

-- The names of alice's objects in one zone, sorted. CR 400.7 mints a fresh id
-- on every zone change, so a permanent that paid a cost has to be found in the
-- graveyard by name rather than by the id it was sacrificed under.
namesIn :: Zone.Zone -> GameState.GameState -> [CardName.CardName]
namesIn zone gs =
  List.sort (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone S.alice gs))

-- Jarad's SECOND activated ability, "Sacrifice a Swamp and a Forest". The file-local
-- `theAbility` names the FIRST, which on this card is the drain the group below
-- proves. Total: the fallback is unreachable on this printing.
swampAndForest :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
swampAndForest p = case Face.activatedAbilities (S.combinedFace p) of
  _ : ability : _ -> ability
  _ -> theAbility p

jaradSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
        (all (\c -> Cost.canPayComponent Map.empty S.alice loneId c lone) (Cost.Type.components cost))
        "each component on its own is payable off the one Bayou"
      Spec.assertBool s (not (Cost.canPay PaymentSubject.ForNeither S.alice loneId cost lone)) "but the cost as a whole is not"
      Spec.assertBool s (Cost.canPay PaymentSubject.ForNeither S.alice pairId cost pair) "and a Forest beside the Bayou pays it"

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
      (jaradId, withJarad) = S.addPermanent jarad S.alice lands
      (preyId, withPrey) = S.addPermanent victim S.alice withJarad
   in (jaradId, preyId, List.foldl' (\g printing -> snd (S.addPermanent printing S.alice g)) withPrey extras)

-- CR 602.2b pays an activation cost at CR 601.2h, so by the time Jarad's drain
-- resolves the creature it sacrificed is a card in a graveyard and CR 608.2h's
-- last known information is the only reading of its power there is.
jaradDrainSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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

-- Chooses this value of X; every other prompt takes the identity fallback, which
-- aims Hatred's one target slot at the only creature on the board.
answerHatredXOf :: Natural.Natural -> Prompt.Prompt r -> r
answerHatredXOf n p = case p of
  Prompt.ChooseX {} -> n
  _ -> S.identityAnswer p

-- Answers Prompt.ChooseX with the bound the prompt carries and RECORDS it. An
-- empty log is how a test sees that the question was never put at all, which is
-- what the mana-cost-only reading of CR 601.2b leaves behind on this card.
answerHatredAtBound :: Prompt.Prompt r -> State.State [Natural.Natural] r
answerHatredAtBound p = case p of
  Prompt.ChooseX _ _ _ _ bound -> do
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
      (pikerId, withPiker) = S.addPermanent piker S.alice base
      (gs, hatredId) = S.handOne hatred withPiker
   in ( pikerId,
        hatredId,
        gs {GameState.players = Map.adjust (\p -> p {Player.life = life}) S.alice (GameState.players gs)}
      )

-- CR 119.4 / 107.1a: "Pay half your life, rounded up" (CostComponent.PayHalfLife),
-- an amount measured against the payer. Odd life totals throughout, so a
-- rounding down is visible.
halfLifeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
halfLifeSpec s registry =
  Spec.describe s "PayHalfLife" $ do
    -- CR 118.3: the half and Mana Confluence's 1 life share one life total.
    -- At 1 life the pair owes 2, so the activation is not offered; at 3 it owes
    -- 3 and is. The pair differs in life alone.
    Spec.it s "CR 118.3 Murderous Betrayal's half and Mana Confluence's life are weighed together" $ do
      let boardAt life = do
            let mine =
                  (S.battlefield S.alice [S.aliased "betrayal" (S.permanent "Murderous Betrayal"), S.settled "swamp" "Swamp", S.settled "confluence" "Mana Confluence"])
                    { Seat.life = life
                    }
            built <- S.buildBoardOrFail s registry (S.board (mine NonEmpty.:| [S.battlefield S.bob [S.permanent "Goblin Piker"]]) S.alice S.precombatMain)
            betrayal <- maybe (Spec.assertFailure s "no Murderous Betrayal") pure (Map.lookup (Label.MkLabel (Text.pack "betrayal")) (Staged.objects built))
            pure (betrayal, (Staged.state built) {GameState.priority = Just S.alice})
      (atOne, one) <- boardAt 1
      (atThree, three) <- boardAt 3
      Spec.assertBool s (not (any (isActivateOf atOne) (Action.legalActions S.alice one))) "CR 118.3: at 1 life, half (1) plus the Confluence's 1 is unpayable, so the activation is not offered"
      Spec.assertBool s (any (isActivateOf atThree) (Action.legalActions S.alice three)) "and at 3 life, half (2) plus 1 is payable, so it is"

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
    -- A bound found past a value that cannot be paid: on four Swamps and three
    -- Pikers, Torgaar, Famine Incarnate's X = 1 leaves {4}{B}{B}, but X = 3
    -- leaves {B}{B}. An ascending climb stops at 0.
    Spec.it s "CR 601.2b Torgaar is asked for X, bounded past the dearer values its own reduction skips" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      torgaar <- S.printingOf s registry "Torgaar, Famine Incarnate"
      let addPiker = snd . S.addPermanent piker S.alice
          (gs, torgaarId) = S.handOne torgaar (addPiker (addPiker (addPiker (S.landsInPlay swamp 4))))
      Spec.assertEqWith s "the bound is 3, one sacrifice per Piker" (State.execState (Engine.runGame answerHatredAtBound gs (S.cast S.alice torgaarId)) []) [3]
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
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice S.noSource CostComponent.PayLifeX gs)) "and it is unpayable until announced"
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice S.noSource (CostComponent.PayLife 20) gs) "while the announced 20 it substitutes to is payable"
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
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice S.noSource CostComponent.BlightX gs)) "unpayable until announced"
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice S.noSource (CostComponent.Blight 1) gs) "an announced 1 is payable"
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice S.noSource (CostComponent.Blight 99) gs) "and so is an announced 99, CR 701.68b naming no number that is too many"
      Spec.assertBool s (Cost.hasVariable (Cost.Type.MkCost Nothing [CostComponent.BlightX])) "it is a CR 107.3 variable"
      Spec.assertBool s (not (Cost.demandGrowsWithX (Cost.Type.MkCost Nothing [CostComponent.BlightX]))) "whose demand never grows, so only CR 101.1 can refuse a value"
    -- The fence one keyword action further over, and the half that differs: a
    -- waterbend cost's demand is the {X} in its own MANA part, so the cost
    -- Katara, Water Tribe's Hope states does grow with the value and the board
    -- can refuse one -- Pawl.CostSpec's "an announced waterbend X larger than the
    -- board is not paid at all" is that refusal at gameplay level. The component
    -- alone still carries no demand, which is what leaves this pair about CR
    -- 601.2b's announcement and nothing else.
    Spec.it s "CR 601.2b an unannounced waterbend X is unpayable, and the mana beside it is what the board refuses" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      hatred <- S.printingOf s registry "Hatred"
      let (_, _, gs) = hatredBoard swamp piker hatred 20
          announced = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Variable])) [CostComponent.WaterbendX]
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice S.noSource CostComponent.WaterbendX gs)) "unpayable until announced"
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice S.noSource (CostComponent.Waterbend 4) gs) "while the announced 4 it substitutes to is payable, rule 701.67a's licence spending nothing of its own"
      Spec.assertBool s (Cost.hasVariable (Cost.Type.MkCost Nothing [CostComponent.WaterbendX])) "it is a CR 107.3 variable on its own"
      Spec.assertBool s (Cost.demandGrowsWithX announced) "and the cost it is printed in grows with the value, the {X} beside it being real mana"
    -- The third substrate, and the classification that separates it from the
    -- blight above: CR 118.3 measures an announced "Pay X {E}" against the
    -- counters the player has, so the demand DOES grow and
    -- Cost.greatestPayableX's climb needs no ceiling to stop. Pawl.CardSpec's CR
    -- 101.1 sweep is the other reader, Sphinx of the Revelation's activation cost
    -- carrying the component with no ceiling beside it.
    Spec.it s "CR 118.3 an unannounced energy X is unpayable, and its demand grows with the value" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      hatred <- S.printingOf s registry "Hatred"
      let (_, _, gs0) = hatredBoard swamp piker hatred 20
          gs = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice gs0
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice S.noSource CostComponent.PayEnergyX gs)) "unpayable until announced"
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice S.noSource (CostComponent.PayEnergy 2) gs) "an announced 2 is payable off two counters"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice S.noSource (CostComponent.PayEnergy 3) gs)) "and an announced 3 is not"
      Spec.assertBool s (Cost.hasVariable (Cost.Type.MkCost Nothing [CostComponent.PayEnergyX])) "it is a CR 107.3 variable"
      Spec.assertBool s (Cost.demandGrowsWithX (Cost.Type.MkCost Nothing [CostComponent.PayEnergyX])) "whose demand grows, so the board itself refuses a big enough value"

-- Living Destiny {3}{G} Instant: "As an additional cost to cast this spell,
-- reveal a creature card from your hand. You gain life equal to the revealed
-- card's mana value." (Oracle checked against Scryfall 2026-09-09.)
--
-- The gate card for CostComponent.RevealCardFromHand, the first component that
-- CHOOSES a card out of a zone and moves nothing (CR 701.20b), and the first
-- spell to read what its own cost bound -- Pawl.Engine.Cast folds the payment's
-- slots onto the spell for it.
--
-- TWO creature cards in hand with DISTINCT mana values, so the payment is a real
-- choice and the life gained names the card revealed rather than "a creature
-- card": Berserkers of Blood Ridge is {4}{R} and Goblin Piker {1}{R}, and 25 and
-- 22 are apart from each other and from the 20 a payment that bound nothing would
-- leave. Neither is ever cast, so nothing but the reveal reads either.
--
-- Four Forests pay {3}{G} on EVERY board here, the refusing one included, so its
-- refusal cannot be unaffordable mana; the two Forests in hand there are what
-- makes the negative differ from the positive in the card TYPE alone.
livingDestinySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
livingDestinySpec s registry =
  Spec.describe s "Living Destiny" $ do
    -- CR 118.3 with the same mana and the same hand size: two LAND cards in place
    -- of the two creature cards, so nothing in the hand answers the criterion and
    -- CR 601.2 rewinds the whole cast.
    Spec.it s "CR 118.3 a hand with no creature card cannot pay" $ do
      forest <- S.printingOf s registry "Forest"
      destiny <- S.printingOf s registry "Living Destiny"
      let (spell, _, _, gs) = livingDestinyBoard forest destiny forest forest
          cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
      Spec.assertEqWith s "CR 118.3 the cast is not offered at all" (filter (S.isCastOf spell) (Action.legalActions S.alice gs)) []
      Spec.assertEqWith s "and taking it anyway gains nothing" (S.lifeOf S.alice cast) (Just 20)
      Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertBool s (List.elem spell (Game.zoneMembers Zone.Hand S.alice cast)) "so the Destiny is still in her hand"

-- The Destiny in alice's hand over `first` and `second`, with four Forests to pay
-- its {3}{G}. Both hand ids come back so a case can pin the payment to one of
-- them BY IDENTITY: an answerer that searched the offer for the card it wanted
-- would find it again after any mutation to what the payment offers.
livingDestinyBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
livingDestinyBoard forest destiny first second =
  let base = S.landsFor forest S.alice 4 (Setup.emptyGame S.bothPlayers)
      (firstId, gs0) = S.addHandCard first S.alice base
      (secondId, gs1) = S.addHandCard second S.alice gs0
      (spellId, gs2) = S.addHandCard destiny S.alice gs1
   in ( spellId,
        firstId,
        secondId,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Caustic Exhale {B} Instant: "As an additional cost to cast this spell, behold
-- a Dragon or pay {1}. Target creature gets -3/-3 until end of turn." (Oracle
-- checked against Scryfall 2026-09-18.)
--
-- The gate card for CostComponent.Behold, the first component whose pool spans
-- TWO zones: CR 701.4a's "reveal a [quality] card from your hand OR choose a
-- [quality] permanent you control on the battlefield". It is also the gate card
-- for Face.additionalCostChoices, CR 118.8's other half: the "or pay {1}" makes
-- the payment a CHOICE, announced at CR 601.2b as one candidate cost per option
-- (Pawl.Engine.Cost.choiceVariants).
--
-- The Dragons are DISTINCT printings, Hoarding Dragon in hand and Exalted Dragon
-- on the battlefield, so a case that reads the offered pool names which zone each
-- candidate came out of. Neither is ever cast, so nothing but the behold reads
-- either.
--
-- ONE Swamp on the behold boards, which is exactly {B}: the {1} option is
-- unpayable there, so those cases read rule 701.4a's half alone and the engine
-- asks nothing. The choice cases get a SECOND Swamp and differ from the refusing
-- case in that one land, which is what makes the {1} the reason a cast the
-- refusing board denies goes through.
--
-- The Goblin Piker in hand is what makes a negative differ from the positive in
-- the card's SUBTYPE alone.
--
-- Armored Galleon is bob's 5/4, which -3\/-3 leaves a 2/1 rather than killing --
-- four values, none of them shared, so the reading is the modification rather
-- than a state-based action.
causticExhaleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
causticExhaleSpec s registry =
  Spec.describe s "Caustic Exhale" $ do
    -- CR 701.4a's battlefield half alone: alice holds no Dragon card, so a
    -- hand-only reading of the rule would refuse the cast outright.
    Spec.it s "CR 701.4a a Dragon on the battlefield pays the cost" $ do
      swamp <- S.printingOf s registry "Swamp"
      exhale <- S.printingOf s registry "Caustic Exhale"
      galleon <- S.printingOf s registry "Armored Galleon"
      dragon <- S.printingOf s registry "Exalted Dragon"
      let (spell, victim, _, dragons, gs) = causticExhaleBoard 1 swamp exhale galleon [] [dragon]
          resolved = S.runPure (targeting victim) (S.runPure (targeting victim) gs (S.cast S.alice spell)) Stack.resolveTop
      Spec.assertEqWith s "CR 601.2f the spell resolved and the Galleon is a 2/1" (S.powerToughnessOf victim resolved) (Just (2, 1))
      Spec.assertBool s (all (\d -> List.elem d (Set.toList (GameState.battlefield resolved))) dragons) "CR 701.4a and the Dragon she beheld is still on the battlefield"
      Spec.assertEqWith s "CR 701.4a choosing a permanent shows nobody anything, so nothing was revealed" (S.revealsOf resolved) []
      Spec.assertBool s (any (S.isCastOf spell) (Action.legalActions S.alice gs)) "and CR 118.3 offered the cast on this board, which is what the refusing case below differs from"
    -- CR 701.4a's hand half alone, the same cast off the other zone: alice
    -- controls no Dragon, so a battlefield-only reading would refuse it.
    Spec.it s "CR 701.4a a Dragon card in hand pays the cost" $ do
      swamp <- S.printingOf s registry "Swamp"
      exhale <- S.printingOf s registry "Caustic Exhale"
      galleon <- S.printingOf s registry "Armored Galleon"
      dragon <- S.printingOf s registry "Hoarding Dragon"
      let (spell, victim, held, _, gs) = causticExhaleBoard 1 swamp exhale galleon [dragon] []
          resolved = S.runPure (targeting victim) (S.runPure (targeting victim) gs (S.cast S.alice spell)) Stack.resolveTop
      Spec.assertEqWith s "CR 601.2f the spell resolved and the Galleon is a 2/1" (S.powerToughnessOf victim resolved) (Just (2, 1))
      Spec.assertBool s (all (\d -> List.elem d (Game.zoneMembers Zone.Hand S.alice resolved)) held) "CR 701.4a and the card she revealed never left her hand"
      Spec.assertEqWith s "CR 701.20a and the table saw it: the hand half of rule 701.4a is a reveal" (S.revealsOf resolved) [(S.alice, Set.singleton (CardName.MkCardName (Text.pack "Hoarding Dragon")))]
    -- CR 118.3 with the same mana and the same hand size: a Goblin Piker in place
    -- of the Dragon card, so nothing in either zone answers the criterion.
    Spec.it s "CR 118.3 neither zone holding a Dragon cannot pay" $ do
      swamp <- S.printingOf s registry "Swamp"
      exhale <- S.printingOf s registry "Caustic Exhale"
      galleon <- S.printingOf s registry "Armored Galleon"
      piker <- S.printingOf s registry "Goblin Piker"
      let (spell, victim, _, _, gs) = causticExhaleBoard 1 swamp exhale galleon [piker] []
          cast = S.runPure (targeting victim) gs (S.cast S.alice spell)
      Spec.assertEqWith s "CR 118.3 the cast is not offered at all" (filter (S.isCastOf spell) (Action.legalActions S.alice gs)) []
      Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertEqWith s "so the Galleon is still bob's 5/4" (S.powerToughnessOf victim cast) (Just (5, 4))
    -- CR 601.2b with BOTH options open -- a Dragon card in hand and two Swamps --
    -- so the answer is the caster's and nothing else differs between the two
    -- runs. The reveal is what tells the answers apart: rule 701.4a's hand half
    -- shows the table a card, and paying the {1} shows it nothing.
    Spec.it s "CR 601.2b which payment is made is the caster's" $ do
      swamp <- S.printingOf s registry "Swamp"
      exhale <- S.printingOf s registry "Caustic Exhale"
      galleon <- S.printingOf s registry "Armored Galleon"
      dragon <- S.printingOf s registry "Hoarding Dragon"
      let (spell, victim, _, _, gs) = causticExhaleBoard 2 swamp exhale galleon [dragon] []
          resolve :: Bool -> GameState.GameState
          resolve beholding = S.runPure (announcing beholding victim) (S.runPure (announcing beholding victim) gs (S.cast S.alice spell)) Stack.resolveTop
          beheld = resolve True
          paid = resolve False
      Spec.assertEqWith s "CR 701.4a the caster who beheld revealed the Dragon" (S.revealsOf beheld) [(S.alice, Set.singleton (CardName.MkCardName (Text.pack "Hoarding Dragon")))]
      Spec.assertEqWith s "CR 118.8 the caster who paid the {1} revealed nothing" (S.revealsOf paid) []
      Spec.assertEqWith s "and either way the Galleon is a 2/1" (fmap (S.powerToughnessOf victim) [beheld, paid]) [Just (2, 1), Just (2, 1)]

-- The Exhale in alice's hand over `inHand` and `onBattlefield`, with `lands`
-- Swamps to pay its cost and bob's Armored Galleon to aim at. Both id lists come
-- back so a case can name the beheld object BY IDENTITY rather than searching the
-- offer for it.
--
-- The land count is the CASE's, not the fixture's: one Swamp is {B} and closes
-- the "or pay {1}" option, two open it, and a pair of cases differing in that
-- number alone is how each option is shown to be a payment of its own.
causticExhaleBoard ::
  Int ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  [Printing.Printing] ->
  [Printing.Printing] ->
  (ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId], GameState.GameState)
causticExhaleBoard lands swamp exhale galleon inHand onBattlefield =
  let base = S.landsFor swamp S.alice lands (Setup.emptyGame S.bothPlayers)
      (victimId, withVictim) = S.addPermanent galleon S.bob base
      addHand (ids, g) printing = let (oid, gN) = S.addHandCard printing S.alice g in (ids <> [oid], gN)
      addField (ids, g) printing = let (oid, gN) = S.addPermanent printing S.alice g in (ids <> [oid], gN)
      (heldIds, withHand) = List.foldl' addHand ([], withVictim) inHand
      (fieldIds, withField) = List.foldl' addField ([], withHand) onBattlefield
      (spellId, gs) = S.addHandCard exhale S.alice withField
   in ( spellId,
        victimId,
        heldIds,
        fieldIds,
        gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Which of CR 118.8's two payments the caster announces at CR 601.2b, FILTERED
-- out of the candidates the engine offered rather than hand-built: the two are
-- told apart by whether the cost states rule 701.4a's behold, which is the only
-- component either of them carries. Targets like `targeting` everywhere else, so
-- a run that was never asked which cost to pay still reaches resolution and is
-- read by the case's own assertion rather than by an unexpected-prompt failure.
announcing :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> r
announcing beholding victim p = case p of
  Prompt.ChooseCost _ _ _ candidates
    | Just cost <- List.find (\c -> not (null (Cost.Type.components c)) == beholding) candidates -> cost
  _ -> targeting victim p

-- Osseous Exhale {1}{W} Instant: "As an additional cost to cast this spell, you
-- may behold a Dragon. Osseous Exhale deals 5 damage to target attacking or
-- blocking creature. If a Dragon was beheld, you gain 2 life." (Oracle checked
-- against Scryfall 2026-09-20.)
--
-- The gate card for CR 701.4b's "if a [quality] was beheld", which is what
-- Binding.beheldObject and Quantity.WasBound exist for, and for CR 118.8b's
-- optional additional cost -- written as a Pawl.Types.CostChoice whose second
-- option is the empty cost, which every board can pay.
--
-- alice attacks with a Hill Giant and then aims her own spell at it: rule
-- 701.4a's pool is read off HER zones and the card's target filter says nothing
-- about who controls the creature, so one attacker is the whole of the combat
-- these cases need. Two Plains is exactly {1}{W}, so the mana window has nothing
-- to decide.
--
-- Each case puts exactly ONE Dragon in reach, so Prompt.ChooseBehold is elided
-- and the only answer a case gives about the cost is CR 601.2b's choice of
-- WHICH option to pay -- which is the one thing the positive and negative boards
-- differ in.
--
-- The numbers are distinct: 5 damage into a 3/3, 2 life onto a 20, two Plains.
osseousExhaleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
osseousExhaleSpec s registry =
  Spec.describe s "Osseous Exhale" $ do
    -- CR 701.4b's battlefield half: the beheld object is a permanent alice
    -- controls, and the linked clause reads that she beheld it at all.
    Spec.it s "CR 701.4b beholding a Dragon permanent gains the 2 life" $ do
      plains <- S.printingOf s registry "Plains"
      exhale <- S.printingOf s registry "Osseous Exhale"
      giant <- S.printingOf s registry "Hill Giant"
      dragon <- S.printingOf s registry "Exalted Dragon"
      let (spell, victim, gs) = osseousExhaleBoard plains exhale giant [] [dragon]
          resolved = S.runPure (announcing True victim) (S.runPure (announcing True victim) gs (S.cast S.alice spell)) Stack.resolveTop
      Spec.assertEqWith s "CR 701.4b a Dragon was beheld, so alice gained the 2 life" (S.lifeOf S.alice resolved) (Just 22)
      Spec.assertEqWith s "CR 601.2f and the spell resolved: 5 damage is marked on the Giant" (S.damageOf victim resolved) (Just 5)
    -- The discriminating twin: the SAME board, the other CR 118.8 option
    -- announced. Nothing differs but the caster's answer, so the life total is
    -- the whole of what the record buys.
    Spec.it s "CR 118.8b declining to behold gains nothing" $ do
      plains <- S.printingOf s registry "Plains"
      exhale <- S.printingOf s registry "Osseous Exhale"
      giant <- S.printingOf s registry "Hill Giant"
      dragon <- S.printingOf s registry "Exalted Dragon"
      let (spell, victim, gs) = osseousExhaleBoard plains exhale giant [] [dragon]
          resolved = S.runPure (announcing False victim) (S.runPure (announcing False victim) gs (S.cast S.alice spell)) Stack.resolveTop
      Spec.assertEqWith s "CR 701.4b no Dragon was beheld, so alice gained nothing" (S.lifeOf S.alice resolved) (Just 20)
      Spec.assertEqWith s "and the spell resolved all the same: 5 damage is marked on the Giant" (S.damageOf victim resolved) (Just 5)
    -- CR 701.4a's other half, which is where a reader that went back to the
    -- BOARD for the beheld object would come apart: the card is in a hidden zone
    -- (CR 400.2), so CR 400.7j's find is refused there and only the binding
    -- itself still answers.
    Spec.it s "CR 701.4b beholding a Dragon card in hand gains the 2 life too" $ do
      plains <- S.printingOf s registry "Plains"
      exhale <- S.printingOf s registry "Osseous Exhale"
      giant <- S.printingOf s registry "Hill Giant"
      dragon <- S.printingOf s registry "Hoarding Dragon"
      let (spell, victim, gs) = osseousExhaleBoard plains exhale giant [dragon] []
          resolved = S.runPure (announcing True victim) (S.runPure (announcing True victim) gs (S.cast S.alice spell)) Stack.resolveTop
      Spec.assertEqWith s "CR 701.4b the Dragon beheld out of her hand counts" (S.lifeOf S.alice resolved) (Just 22)
      Spec.assertEqWith s "CR 701.20a and the table saw it" (S.revealsOf resolved) [(S.alice, Set.singleton (CardName.MkCardName (Text.pack "Hoarding Dragon")))]

-- alice's Hill Giant attacking, her Exhale in hand over `inHand` and
-- `onBattlefield`, and the two Plains that pay for it. The attacker comes back
-- as the victim: it is the only creature CR 508.1k leaves the card's target
-- filter, so `targeting` has one recipient to filter down to.
osseousExhaleBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  [Printing.Printing] ->
  [Printing.Printing] ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
osseousExhaleBoard plains exhale giant inHand onBattlefield =
  let (combat, ours, _) = S.combatBoardOf [giant] []
      giantId = case ours of
        a : _ -> a
        [] -> S.noSource
      addHand (ids, g) printing = let (oid, gN) = S.addHandCard printing S.alice g in (ids <> [oid], gN)
      addField (ids, g) printing = let (oid, gN) = S.addPermanent printing S.alice g in (ids <> [oid], gN)
      (_, withHand) = List.foldl' addHand ([] :: [ObjectId.ObjectId], combat) inHand
      (_, withField) = List.foldl' addField ([] :: [ObjectId.ObjectId], withHand) onBattlefield
      (spellId, withSpell) = S.addHandCard exhale S.alice withField
      withLands = S.landsFor plains S.alice 2 withSpell
   in (spellId, giantId, S.runPure (attackingWith giantId) withLands (Combat.declareAttackers S.manaPerformer S.alice))

-- Champion of the Weird {3}{B} 5/5 Creature -- Goblin Berserker: "As an
-- additional cost to cast this spell, behold a Goblin and exile it. Pay 1 life,
-- Blight 2: Target opponent blights 2. Activate only as a sorcery. When this
-- creature leaves the battlefield, return the exiled card to its owner's hand."
-- (Oracle checked against Scryfall 2026-09-30.)
--
-- The gate card for CostComponent.BeholdAndExile and for CR 607.2q, the link
-- from a card exiled to pay a permanent spell's cost to the permanent that spell
-- becomes.
--
-- TWO Goblins in reach, a Goblin Piker in hand and a Goblin Brawler on the
-- battlefield, so Prompt.ChooseBehold is raised. The pinned answer is the
-- Brawler, the pool's SECOND entry (hand first), so an answerer ignored or a
-- prompt never raised lands on the Piker instead. Four Swamps are exactly {3}{B}.
championOfTheWeirdSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
championOfTheWeirdSpec s registry =
  Spec.describe s "Champion of the Weird" $ do
    Spec.it s "CR 701.4a / 406.2 the beheld Goblin permanent is exiled as the cost" $ do
      (champion, piker, brawler, _, gs) <- championBoard s registry True
      let resolved = S.runPure (beholdingOne brawler) (S.runPure (beholdingOne brawler) gs (S.cast S.alice champion)) Stack.resolveTop
      Spec.assertEqWith s "CR 406.2 the Brawler she beheld is in exile, and nothing else is" (namesIn Zone.Exile resolved) [cardNamed "Goblin Brawler"]
      Spec.assertBool s (List.elem piker (Game.zoneMembers Zone.Hand S.alice resolved)) "and the Piker she did not choose is still in her hand"
      Spec.assertEqWith s "CR 608.3a and the Champion resolved onto the battlefield" (S.countOnBattlefieldByName (cardNamed "Champion of the Weird") S.alice resolved) 1
    Spec.it s "CR 607.2q the Champion leaving returns the exiled card to its owner's hand" $ do
      (champion, _, brawler, _, gs) <- championBoard s registry True
      let resolved = S.runPure (beholdingOne brawler) (S.runPure (beholdingOne brawler) gs (S.cast S.alice champion)) Stack.resolveTop
          onField = filter (\o -> fmap S.nameOf (Game.cardOf o resolved) == Just (cardNamed "Champion of the Weird")) (Set.toList (GameState.battlefield resolved))
          killed = S.runPure S.identityAnswer resolved (Event.destroy Regenerability.Regenerable onField)
          after = S.runPure S.identityAnswer killed Engine.priorityLoop
      Spec.assertEqWith s "CR 607.2q the Brawler is back in alice's hand" (S.countByName (cardNamed "Goblin Brawler") S.alice after) 1
      Spec.assertEqWith s "and nothing is left in exile" (namesIn Zone.Exile after) []
    -- CR 118.3 on the same mana with the Champion the only Goblin: the spell on
    -- the stack cannot behold itself, so the cast is refused.
    Spec.it s "CR 118.3 with no other Goblin the Champion cannot be cast" $ do
      (champion, _, _, _, gs) <- championBoard s registry False
      Spec.assertEqWith s "CR 118.3 the cast is not offered at all" (filter (S.isCastOf champion) (Action.legalActions S.alice gs)) []
    -- The card's second ability, on the first case's board: alice's Brawler is in
    -- exile, so her cost's blight has the Champion alone to take its counters, and
    -- bob's Wall of Stone is the only creature his blight can choose.
    Spec.it s "CR 701.68a Pay 1 life, Blight 2: target opponent blights 2" $ do
      printing <- S.printingOf s registry "Champion of the Weird"
      (champion, _, brawler, wall, gs) <- championBoard s registry True
      let resolved = S.runPure (beholdingOne brawler) (S.runPure (beholdingOne brawler) gs (S.cast S.alice champion)) Stack.resolveTop
          onField = filter (\o -> fmap S.nameOf (Game.cardOf o resolved) == Just (cardNamed "Champion of the Weird")) (Set.toList (GameState.battlefield resolved))
          minusOn o g = fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject o g)
      case onField of
        [permanent] -> do
          let activated = S.runPure (targetingPlayer S.bob) resolved (Activate.activateAbility S.alice permanent (theAbility printing))
              after = S.runPure (targetingPlayer S.bob) activated Stack.resolveTop
          Spec.assertEqWith s "CR 701.68a bob, the target, blighted 2 onto his Wall" (minusOn wall after) (Just 2)
          Spec.assertEqWith s "CR 119.4 alice paid the 1 life" (S.lifeOf S.alice after) (Just 19)
          Spec.assertEqWith s "CR 701.68a and blighted 2 onto the Champion to pay" (minusOn permanent after) (Just 2)
        _ -> Spec.assertFailure s "the Champion should be alone on the battlefield under its name"

-- alice's Champion in hand over four Swamps. `goblins` puts a Goblin Piker in
-- her hand and a Goblin Brawler on her battlefield; without them she holds a
-- Hill Giant instead, so the negative differs in the subtype alone.
-- bob's Wall of Stone, a 0/8, is what the Champion's blight ability aims at.
championBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
championBoard s registry goblins = do
  swamp <- S.printingOf s registry "Swamp"
  champion <- S.printingOf s registry "Champion of the Weird"
  piker <- S.printingOf s registry "Goblin Piker"
  brawler <- S.printingOf s registry "Goblin Brawler"
  giant <- S.printingOf s registry "Hill Giant"
  wall <- S.printingOf s registry "Wall of Stone"
  let (wallId, base) = S.addPermanent wall S.bob (S.landsFor swamp S.alice 4 (Setup.emptyGame S.bothPlayers))
      (pikerId, withHand) = S.addHandCard (if goblins then piker else giant) S.alice base
      (brawlerId, withField) = if goblins then S.addPermanent brawler S.alice withHand else (pikerId, withHand)
      (championId, gs) = S.addHandCard champion S.alice withField
  pure
    ( championId,
      pikerId,
      brawlerId,
      wallId,
      gs
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- CR 701.4a's choice pinned to one object, FILTERED out of the offer.
beholdingOne :: ObjectId.ObjectId -> Prompt.Prompt r -> r
beholdingOne which p = case p of
  Prompt.ChooseBehold _ _ _ candidates
    | List.elem which (NonEmpty.toList candidates) -> which
  _ -> S.identityAnswer p

-- A card name, from its printed spelling.
cardNamed :: String -> CardName.CardName
cardNamed = CardName.MkCardName . Text.pack

-- Kindle the Inner Flame {3}{R} Kindred Sorcery -- Elemental: "Create a token
-- that's a copy of target creature you control, except it has haste and 'At the
-- beginning of the end step, sacrifice this token.' Flashback--{1}{R}, Behold
-- three Elementals." (Oracle checked against Scryfall 2026-09-30.)
--
-- The gate card for a CostComponent.Behold of more than one object: CR 701.4a
-- stated per object, three times over one pool that spans the hand and the
-- battlefield.
--
-- The Kindle is in alice's GRAVEYARD, so every cast here is the flashback one,
-- and two Mountains are exactly {1}{R}: the positive and negative boards have
-- the same mana and differ in ONE hand card, Gaea's Protector (an Elemental) or
-- Goblin Piker (not). The field Elementals are Glacial Crasher and Sickle Ripper,
-- distinct printings; the Ripper is the copy's target, a 2/1 no other creature
-- here shares a name with.
kindleTheInnerFlameSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kindleTheInnerFlameSpec s registry =
  Spec.describe s "Kindle the Inner Flame" $ do
    -- Two Elementals on the battlefield and the third in hand: a one-zone
    -- reading of rule 701.4a finds two and refuses.
    Spec.it s "CR 701.4a three Elementals across both zones pay the flashback cost" $ do
      (kindle, ripper, _, gs) <- kindleBoard s registry "Gaea's Protector" []
      let resolved = S.runPure (targeting ripper) (S.runPure (targeting ripper) gs (S.cast S.alice kindle)) Stack.resolveTop
          minted = Set.toList (GameState.battlefield resolved) List.\\ Set.toList (GameState.battlefield gs)
      Spec.assertEqWith s "CR 707.2 the flashback cast resolved into one token copy of the Ripper" (fmap (\o -> Projection.namesOf o resolved) minted) [Set.singleton (cardNamed "Sickle Ripper")]
      Spec.assertBool s (all (\o -> Projection.hasKeyword Keyword.Haste o resolved) minted) "CR 707.9a and the copy has haste"
      Spec.assertEqWith s "CR 701.4a the Protector was revealed out of her hand" (S.revealsOf resolved) [(S.alice, Set.singleton (cardNamed "Gaea's Protector"))]
      Spec.assertEqWith s "CR 702.34a and the Kindle was exiled" (namesIn Zone.Exile resolved) [cardNamed "Kindle the Inner Flame"]
    -- CR 118.3 on the same mana, the Protector swapped for a Goblin Piker: two
    -- Elementals are one short of three.
    Spec.it s "CR 118.3 two Elementals cannot behold three" $ do
      (kindle, ripper, _, gs) <- kindleBoard s registry "Goblin Piker" []
      let cast = S.runPure (targeting ripper) gs (S.cast S.alice kindle)
      Spec.assertEqWith s "CR 118.3 the flashback cast is not offered at all" (filter (S.isCastOf kindle) (Action.legalActions S.alice gs)) []
      Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack cast)) 0
    -- FOUR Elementals, so each of the three asks is a real choice. The answerer
    -- is hostile: it answers the first ask's first candidate every time. A payment
    -- that let one object be beheld twice would offer it again.
    Spec.it s "CR 701.4a three Elementals are three objects" $ do
      (kindle, ripper, _, gs) <- kindleBoard s registry "Gaea's Protector" ["Sickle Ripper"]
      let offered :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
          offered p = case p of
            Prompt.ChooseBehold _ _ _ candidates -> do
              State.modify' (<> [NonEmpty.toList candidates])
              asked <- State.get
              pure (maybe (NonEmpty.head candidates) NonEmpty.head (NonEmpty.nonEmpty (concat (take 1 asked))))
            _ -> pure (targeting ripper p)
          asks = State.execState (Engine.runGame offered gs (S.cast S.alice kindle)) []
      case asks of
        pool@[first, second, _, _] : _ ->
          Spec.assertEqWith s "CR 701.4a three asks, each without the objects already beheld" asks [pool, pool List.\\ [first], pool List.\\ [first, second]]
        _ -> Spec.assertFailure s ("expected a first ask over four Elementals, got " <> show asks)

-- alice's Kindle in her graveyard over two Mountains, a Glacial Crasher and a
-- Sickle Ripper on her battlefield, `inHand` in her hand, and `extra` further
-- permanents. Returns the Kindle, the Ripper, the hand card and the board.
kindleBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> [String] -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
kindleBoard s registry inHand extra = do
  mountain <- S.printingOf s registry "Mountain"
  kindle <- S.printingOf s registry "Kindle the Inner Flame"
  crasher <- S.printingOf s registry "Glacial Crasher"
  ripper <- S.printingOf s registry "Sickle Ripper"
  held <- S.printingOf s registry inHand
  extras <- traverse (S.printingOf s registry) extra
  let base = S.landsFor mountain S.alice 2 (Setup.emptyGame S.bothPlayers)
      (_, withCrasher) = S.addPermanent crasher S.alice base
      (ripperId, withRipper) = S.addPermanent ripper S.alice withCrasher
      withExtras = List.foldl' (\g printing -> snd (S.addPermanent printing S.alice g)) withRipper extras
      (heldId, withHand) = S.addHandCard held S.alice withExtras
      (kindleId, gs) = S.addGraveyardCard kindle S.alice withHand
  pure
    ( kindleId,
      ripperId,
      heldId,
      gs
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Forensic Researcher {2}{U} Creature -- Merfolk Detective 1/3: "{T}: Untap
-- another target permanent you control. {T}, Collect evidence 3: Tap target
-- creature you don't control." (Oracle checked against Scryfall 2026-09-19.)
--
-- The gate card for CostComponent.CollectEvidence, the first component measured
-- by a TOTAL over the objects it takes rather than by how many -- CR 701.59a's
-- "exile any number of cards from your graveyard with total mana value N or
-- greater". CostComponent.TapForTotalPower is the same question one zone over.
--
-- alice controls the Researcher and NOTHING ELSE, which is what keeps the card's
-- other ability out of every case: "another target permanent you control" has no
-- legal target on these boards, so the only activation ever offered is the one
-- that collects evidence.
--
-- No lands anywhere: the collect-evidence ability's mana part is empty, so no
-- negative here can pass on unaffordable mana.
--
-- The graveyard cards are Lightning Bolt (mana value 1) and Acidic Soil (mana
-- value 3), neither of which is ever cast, so nothing but the payment can move
-- either. bob's Goblin Piker is the 2/1 the ability aims at. The only two
-- numbers here that coincide are the threshold and the Soil's mana value, which
-- is the point of the second case; everything else is distinct.
forensicResearcherSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
forensicResearcherSpec s registry =
  Spec.describe s "Forensic Researcher" $ do
    -- CR 701.59b, against the first case's board one Bolt short: two mana value 1
    -- cards total 2, and a player who cannot reach the total can't choose to
    -- collect evidence at all.
    Spec.it s "CR 701.59b a graveyard totalling 2 cannot collect evidence 3" $ do
      researcher <- S.printingOf s registry "Forensic Researcher"
      bolt <- S.printingOf s registry "Lightning Bolt"
      piker <- S.printingOf s registry "Goblin Piker"
      let (srcId, victimId, buried, gs) = forensicResearcherBoard researcher piker [bolt, bolt]
          (after, _) = afterCollecting srcId buried victimId gs
      Spec.assertEqWith s "CR 701.59b the activation is not offered" (filter (collectsEvidenceFrom srcId) (Action.legalActions S.alice gs)) []
      Spec.assertEqWith s "so bob's Piker is untapped" (fmap Object.tapped (Game.lookupObject victimId after)) (Just TapState.Untapped)
      Spec.assertEqWith s "and nothing was exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 0

-- The Researcher alice's only permanent, bob's Piker the only other creature,
-- and `buried` in alice's graveyard in the order given. No lands and no cards in
-- hand: nothing on this board can pay mana, which is what makes the empty mana
-- part of the ability the only cost in play.
forensicResearcherBoard ::
  Printing.Printing ->
  Printing.Printing ->
  [Printing.Printing] ->
  (ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
forensicResearcherBoard researcher piker buried =
  let (srcId, withSource) = S.addPermanent researcher S.alice (Setup.emptyGame S.bothPlayers)
      (victimId, withVictim) = S.addPermanent piker S.bob withSource
      bury (ids, g) printing = let (oid, gN) = S.addGraveyardCard printing S.alice g in (ids <> [oid], gN)
      (buriedIds, gs) = List.foldl' bury ([], withVictim) buried
   in ( srcId,
        victimId,
        buriedIds,
        gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.remaining = mempty,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Runs one priority loop in which alice takes the collect-evidence activation
-- once, exiles exactly `picks`, and aims at `victim`. The second component is
-- every pool she was offered to collect out of, in order, so a case can say what
-- the choice was BETWEEN and not only what it came to.
afterCollecting :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [[ObjectId.ObjectId]])
afterCollecting srcId picks victim gs =
  let ((_, after), (_, offers)) = State.runState (Engine.runGame (collecting srcId picks victim) gs Engine.priorityLoop) (0, [])
   in (after, offers)

-- Threaded through State rather than pure for Pawl.VanguardSpec's takesOnce'
-- reason: the payment can go Unpaid and rewind, and a "take it whenever offered"
-- answerer would then be offered the same activation forever.
--
-- The picks are FILTERED out of the offered candidates rather than built, so a
-- mutation cannot be silently repaired by an answerer that rediscovers a legal
-- subset; the targets are filtered for that spec's other reason, CR 608.2b
-- re-reading them at resolution.
collecting :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State (Int, [[ObjectId.ObjectId]]) r
collecting srcId picks victim p = case p of
  Prompt.ChooseAction _ _ actions -> do
    (taken, offers) <- State.get
    case filter (collectsEvidenceFrom srcId) actions of
      offer : _ | taken == 0 -> do
        State.put (1, offers)
        pure offer
      _ -> pure Action.Type.Pass
  Prompt.ChooseCollectEvidence _ _ _ candidates _ -> do
    State.modify' (\(taken, offers) -> (taken, offers <> [candidates]))
    pure (Set.fromList (filter (`List.elem` picks) candidates))
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (Set.filter (== Recipient.ToCreature victim) . snd) sets)
  _ -> pure (S.identityAnswer p)

-- Is this menu entry the activation of that permanent's COLLECT EVIDENCE ability?
-- Read off the cost the offer carries rather than off an ability index, so the
-- card's other {T} ability can never be mistaken for it.
collectsEvidenceFrom :: ObjectId.ObjectId -> Action.Type.Action -> Bool
collectsEvidenceFrom oid action = case action of
  Action.Type.Activate srcId ability ->
    srcId == oid
      && any
        (\component -> case component of CostComponent.CollectEvidence _ -> True; _ -> False)
        (Cost.Type.components (ActivatedAbility.cost ability))
  _ -> False

-- Vitu-Ghazi Inspector {1}{G} Creature -- Elf Detective 1/3: "As an additional
-- cost to cast this spell, you may collect evidence 6. / Reach / When this
-- creature enters, if evidence was collected, put a +1/+1 counter on target
-- creature and you gain 2 life." Izoni, Center of the Web {4}{B}{G}: "Whenever
-- Izoni enters or attacks, you may collect evidence 4. If you do, create two 2/1
-- black and green Spider creature tokens with reach and menace." (Oracle checked
-- against Scryfall 2026-09-25.)
--
-- CR 701.59c's linked "if evidence was collected", read by the PERMANENT the
-- spell became (CR 400.7d), and CR 118.12's "you may [cost]. If you do" over the
-- same action paid at resolution. Each pair is one board and differs only in
-- the answer to the optional cost.
--
-- alice's graveyard is two Acidic Soils (mana value 3 each), so it totals 6:
-- enough for either card's collection. bob's Goblin Piker (2/1) is the
-- Inspector's target. Distinct numbers: 2 life onto 20, a +1/+1 counter onto a
-- 2-power Piker, two Spiders.
evidenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
evidenceSpec s registry =
  let play held = S.cast S.alice held >> Stack.resolveTop >> Engine.placePendingTriggers >> Stack.resolveTop
      clues = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Clue Token")) S.alice
   in Spec.describe s "Collect evidence (CR 701.59)" $ do
        Spec.it s "CR 701.59c Vitu-Ghazi Inspector's enters ability reads the evidence its spell collected" $ do
          (gs, held, piker) <- evidenceBoard s registry "Vitu-Ghazi Inspector" 2
          let collected = S.runPure (collectingEvidence True piker) gs (play held)
              declined = S.runPure (collectingEvidence False piker) gs (play held)
          Spec.assertEqWith s "CR 701.59c evidence was collected, so alice gained 2 life" (S.lifeOf S.alice collected) (Just 22)
          Spec.assertEqWith s "CR 118.8b evidence was not collected, so she gained nothing" (S.lifeOf S.alice declined) (Just 20)
          Spec.assertEqWith s "and only the collecting run put a +1/+1 counter on the Piker" (Projection.powerOf piker collected, Projection.powerOf piker declined) (Just 3, Just 2)
        Spec.it s "CR 701.59a Evidence Examiner investigates when the Inspector's cost collects evidence" $ do
          (base, held, piker) <- evidenceBoard s registry "Vitu-Ghazi Inspector" 2
          examiner <- S.printingOf s registry "Evidence Examiner"
          let (_, gs) = S.addPermanent examiner S.alice base
              resolveAll = S.cast S.alice held >> Monad.replicateM_ (3 :: Int) (Engine.settleForPriority >> Stack.resolveTop)
              collected = S.runPure (collectingEvidence True piker) gs resolveAll
              declined = S.runPure (collectingEvidence False piker) gs resolveAll
          Spec.assertEqWith s "CR 701.59a collecting evidence made a Clue, declining made none" (clues collected, clues declined) (1, 0)
          Spec.assertEqWith s "and both runs ended with the stack empty" (length (GameState.stack collected), length (GameState.stack declined)) (0, 0)
        -- CR 609.7a's third class, over the Inspector SPELL: the record its cost
        -- bound says only whether evidence was collected (CR 701.59c), so the
        -- exiled Soils are no object it refers to. bob answers the Inspector with
        -- Healing Grace, and is offered sources on the stack and battlefield.
        Spec.it s "CR 609.7a the evidence the Inspector collected is not a source it refers to" $ do
          (gs, held, piker) <- evidenceBoard s registry "Vitu-Ghazi Inspector" 2
          plains <- S.printingOf s registry "Plains"
          grace <- S.printingOf s registry "Healing Grace"
          let (graceId, withGrace) = S.addHandCard grace S.bob (S.landsFor plains S.bob 1 gs)
              soils = Game.zoneMembers Zone.Graveyard S.alice gs
              offered = snd (State.runState (Engine.runGame (recordingSources True piker) withGrace (S.cast S.alice held >> S.cast S.bob graceId >> Stack.resolveTop)) [])
          Spec.assertEqWith s "CR 609.7a no collected Soil is offered to bob as a source" (concatMap (filter (`elem` soils)) offered) []
          Spec.assertBool s (List.elem piker (concat offered)) "and he was asked, over the Piker among others"

-- `card` in alice's hand over `lands` Forests and a Swamp, two Acidic Soils in
-- her graveyard, and bob's Goblin Piker, in her precombat main phase with
-- priority. Returns the state, the card and the Piker.
evidenceBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> Int -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
evidenceBoard s registry card lands = do
  forest <- S.printingOf s registry "Forest"
  swamp <- S.printingOf s registry "Swamp"
  soil <- S.printingOf s registry "Acidic Soil"
  piker <- S.printingOf s registry "Goblin Piker"
  held <- S.printingOf s registry card
  let withLands = S.landsFor swamp S.alice 1 (S.landsFor forest S.alice (lands - 1) (Setup.emptyGame S.bothPlayers))
      (_, oneSoil) = S.addGraveyardCard soil S.alice withLands
      (_, twoSoils) = S.addGraveyardCard soil S.alice oneSoil
      (pikerId, withPiker) = S.addPermanent piker S.bob twoSoils
      (heldId, gs) = S.addHandCard held S.alice withPiker
  pure
    ( gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice},
      heldId,
      pikerId
    )

-- Collect evidence or decline it, at either moment the card offers it: CR
-- 601.2b's choice among the costs (the Inspector) and CR 118.12's offer at
-- resolution (Izoni). The collection takes every card offered, FILTERED out of
-- the candidates; the Inspector's target is `victim`, `announcing`'s filter.
collectingEvidence :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> r
collectingEvidence collects victim p = case p of
  Prompt.ChooseToPay {} -> if collects then PaymentDecision.Pays else PaymentDecision.Declines
  Prompt.ChooseCollectEvidence _ _ _ candidates _ -> Set.fromList candidates
  _ -> announcing collects victim p

-- collectingEvidence, recording every CR 609.7a source-choice offer in order.
recordingSources :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
recordingSources collects victim p = case p of
  Prompt.ChooseDamageSource _ _ _ offered -> do
    State.modify' (<> [NonEmpty.toList offered])
    pure (NonEmpty.head offered)
  _ -> pure (collectingEvidence collects victim p)

-- CR 115.4's "any target" pointed at a PLAYER, FILTERED out of the offered
-- recipients for `targeting`'s reason: a hand-built Recipient.ToPlayer is a
-- different recipient from the one the engine offered, and CR 608.2b's re-read
-- would drop it with no error.
targetingPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
targetingPlayer who p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, legal) -> Set.filter ((== Just who) . Recipient.playerOf) legal) asked
  _ -> S.identityAnswer p

-- alice casts Flash over exactly two Islands, holding `creature`, with one
-- untapped land of each printing in `spare` left over for the gate. The Islands
-- are tapped by the cast itself, so whatever `spare` holds is ALL the mana the
-- payment can reach -- which is what makes the tap counts below a measurement of
-- the derived cost rather than of the board.
flashCast :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> Printing.Printing -> GameState.GameState -> GameState.GameState
flashCast island flash spare creature base =
  let lands = S.landsFor island S.alice 2 base
      withSpare = List.foldl' (\g land -> S.landsFor land S.alice 1 g) lands spare
      -- The Flash FIRST: S.handOne replaces the hand rather than adding to it,
      -- where S.addHandCard appends, so the creature card has to arrive after.
      (withFlash, flashId) = S.handOne flash withSpare
      (_creature, gs) = S.addHandCard creature S.alice withFlash
   in S.runPure S.identityAnswer gs (S.cast S.alice flashId)

-- Takes CR 603.5's "you may put", then pays whatever CR 118.12 offers.
putsAndPays :: Prompt.Prompt r -> r
putsAndPays p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.ChooseToPay {} -> PaymentDecision.Pays
  _ -> S.identityAnswer p

-- putsAndPays' sibling, differing in NOTHING but the answer to the gate:
-- S.identityAnswer declines a payment.
putsAndDeclines :: Prompt.Prompt r -> r
putsAndDeclines p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- putsAndPays plus a Clone's as-enters choice, for the leg whose put card
-- is a Clone.
putsPaysCopying :: ObjectId.ObjectId -> Prompt.Prompt r -> r
putsPaysCopying wanted p = case p of
  Prompt.ChooseCopyTarget {} -> Just wanted
  _ -> putsAndPays p

-- The pay-or-not answers in a transcript, in order. An EMPTY list is the
-- observation CR 118.6's unpayable cost makes: Cost.canPay refuses ahead of the
-- offer, so nobody is asked at all.
payAnswers :: [Response.Response] -> [Response.Response]
payAnswers = filter (\r -> case r of Response.ChoseToPay _ -> True; _ -> False)

-- CR 118.6 / 118.7 / 118.12: Flash's "You may put a creature card from your hand
-- onto the battlefield. If you do, sacrifice it unless you pay its mana cost
-- reduced by {2}" -- the one card in the pool whose resolution cost is DESCRIBED
-- in terms of another object rather than printed (Pawl.Types.CostBasis).
--
-- Flash {1}{U} Instant (name, cost, type line and Oracle text confirmed by the
-- repository's owner during this unit's review; api.scryfall.com is unreachable
-- from this environment, so this one card is not checked against the API the way
-- the rest of the pool is). Its whole printed text is those two sentences, so
-- nothing else on the card can be what these assertions read.
--
-- Hill Giant {3}{R} is the put creature everywhere but the copy leg, and its
-- cost is what makes the board discriminating: reduced by {2} it is {1}{R}, so
-- TWO lands pay it and one of them must be red. Each leg leaves alice exactly
-- the mana one reading of the sentence needs:
--
--   * A Mountain and a Forest pay {1}{R}. The unreduced {3}{R} could not be
--     paid at all on that board (CR 118.3), so the Giant surviving with exactly
--     two more lands tapped is the reduction.
--   * Two Forests pay {2} but not {1}{R} (CR 118.7a: a generic reduction leaves
--     the coloured component alone), so a cost taken to be the mana VALUE less
--     two -- or reduced through the coloured pip -- would be payable there and
--     is not.
--
-- The COPY leg is the projection: a Clone put onto the battlefield enters as a
-- copy of Tarmogoyf {1}{G} (CR 706.2), whose mana cost reduced by {2} is {G} --
-- payable off the one Forest alice holds, where the printed Clone's {3}{U}
-- reduced to {1}{U} is not payable there at all.
flashSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
flashSpec s registry = Spec.describe s "Flash" $ do
  Spec.it s "CR 118.6 the put creature's own mana cost, reduced by {2}, is what is offered" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    flash <- S.printingOf s registry "Flash"
    giant <- S.printingOf s registry "Hill Giant"
    let cast = flashCast island flash [mountain, forest] giant (Setup.emptyGame S.bothPlayers)
        ((_, after), transcript) = Replay.record putsAndPays cast Stack.resolveTop
    -- The BOARD first, so a mutation of the derivation reddens the gameplay-level
    -- assertion rather than the prompt count ahead of it.
    Spec.assertEqWith s "the Hill Giant stayed on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Hill Giant")) S.alice after) 1
    Spec.assertEqWith s "only Flash is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "alice was asked exactly once, and paid" (payAnswers transcript) [Response.ChoseToPay PaymentDecision.Pays]
    -- Two Islands for the cast and two more lands for {1}{R}: the whole board.
    -- The unreduced {3}{R} would have needed two lands alice does not have.
    Spec.assertEqWith s "paying tapped exactly two more lands" (S.tappedCount S.alice after) 4
  -- The same board and the same cast, differing in NOTHING but the answer.
  Spec.it s "CR 118.12a declining the offer sacrifices the creature" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    flash <- S.printingOf s registry "Flash"
    giant <- S.printingOf s registry "Hill Giant"
    let cast = flashCast island flash [mountain, forest] giant (Setup.emptyGame S.bothPlayers)
        ((_, after), transcript) = Replay.record putsAndDeclines cast Stack.resolveTop
    Spec.assertEqWith s "the Hill Giant is gone from the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Hill Giant")) S.alice after) 0
    Spec.assertEqWith s "and it and Flash are in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2
    -- The offer was real and its answer is what sacrificed the creature: she
    -- could have paid on this board, which the leg above does.
    Spec.assertEqWith s "alice was asked exactly once, and declined" (payAnswers transcript) [Response.ChoseToPay PaymentDecision.Declines]
    Spec.assertEqWith s "declining spent nothing" (S.tappedCount S.alice after) 2
  -- CR 118.7a, the colour leg: the {2} comes off the GENERIC component, so
  -- {3}{R} becomes {1}{R} and not {2}. Two Forests are two mana and pay neither
  -- the {R} nor anything standing in for it.
  Spec.it s "CR 118.7a the reduction leaves the coloured component alone" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    flash <- S.printingOf s registry "Flash"
    giant <- S.printingOf s registry "Hill Giant"
    let cast = flashCast island flash [forest, forest] giant (Setup.emptyGame S.bothPlayers)
        ((_, after), transcript) = Replay.record putsAndPays cast Stack.resolveTop
    -- CR 118.3 ahead of the offer: a cost alice cannot pay is not put to her,
    -- so the empty transcript says the {R} was really demanded.
    Spec.assertEqWith s "alice was never asked" (payAnswers transcript) []
    Spec.assertEqWith s "the Hill Giant was sacrificed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Hill Giant")) S.alice after) 0
    Spec.assertEqWith s "it and Flash are in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2
    Spec.assertEqWith s "and her Forests are untouched" (S.tappedCount S.alice after) 2
  -- CR 706.2: the mana cost read is the permanent's, which for a Clone is the
  -- one it copied. The printed Clone's {3}{U} reduced by {2} is {1}{U}, which
  -- this board cannot pay -- so a read through the printed card sacrifices the
  -- Clone where the copiable {1}{G} reduced to {G} keeps it.
  Spec.it s "CR 706.2 a Clone put by Flash is bought at the copied mana cost" $ do
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    flash <- S.printingOf s registry "Flash"
    clone <- S.printingOf s registry "Clone"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    let (goyfId, withGoyf) = S.addPermanent tarmogoyf S.bob (Setup.emptyGame S.bothPlayers)
        cast = flashCast island flash [forest] clone withGoyf
        ((_, after), transcript) = Replay.record (putsPaysCopying goyfId) cast Stack.resolveTop
    -- CR 400.7 mints a fresh incarnation, so the Clone is found as the one
    -- permanent the resolution added rather than by the id it had in hand.
    case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield cast)) of
      [cloneId] -> do
        -- The guard that the copy happened at all, which is what makes {G} the
        -- cost: read through the projection, since Game.cardOf still answers
        -- Clone.
        Spec.assertBool s (Projection.hasName (CardName.MkCardName (Text.pack "Tarmogoyf")) cloneId after) "the Clone entered as a copy of the Tarmogoyf"
        Spec.assertEqWith s "only Flash is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
        Spec.assertEqWith s "paying tapped exactly one more land" (S.tappedCount S.alice after) 3
        Spec.assertEqWith s "alice was asked exactly once, and paid" (payAnswers transcript) [Response.ChoseToPay PaymentDecision.Pays]
      other -> Spec.assertFailure s ("expected the Clone alone to arrive, got " <> show (length other))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Cost" $ do
  doorSpec s registry
  doomCannonSpec s registry
  jaradSpec s registry
  jaradDrainSpec s registry
  greedSpec s registry
  halfLifeSpec s registry
  hatredSpec s registry
  villageRitesSpec s registry
  spitefulSpec s registry
  headlessSkaabSpec s registry
  cadaverousBloomSpec s registry
  livingDestinySpec s registry
  causticExhaleSpec s registry
  osseousExhaleSpec s registry
  championOfTheWeirdSpec s registry
  kindleTheInnerFlameSpec s registry
  forensicResearcherSpec s registry
  evidenceSpec s registry
  frailExhumationSpec s registry
  everbarkShamanSpec s registry
  putridRaptorSpec s registry
  catharticReunionSpec s registry
  magmaticInsightSpec s registry
  safeholdSentrySpec s registry
  fireblastSpec s registry
  costBatchSpec s registry
  asmorSpec s registry
  asmorFoodSpec s registry
  crossCheckSpec s registry
  longtuskCubSpec s registry
  thrastaSpec s registry
  avengeSpec s registry
  deemInferiorSpec s registry
  synchronizedEvictionSpec s registry
  humiliationSpec s registry
  targetCostSpec s registry
  frogmiteSpec s registry
  mycosynthGolemSpec s registry
  exhalationSpec s registry
  omniscienceSpec s registry
  springleafDrumSpec s registry
  morcantSpec s registry
  unerringSlingSpec s registry
  melokuSpec s registry
  barkhideTrollSpec s registry
  zameckGuildmageCostSpec s registry
  novijenSagesSpec s registry
  retributionSpec s registry
  chatterfangSacrificeSpec s registry
  oozeFluxSpec s registry
  tayamSpec s registry
  soulDivinerSpec s registry
  quillspikeSpec s registry
  millikinSpec s registry
  brittleEffigySpec s registry
  trumpetingCarnosaurSpec s registry
  ashnodsAltarSpec s registry
  announcedReversalSpec s registry
  siegeWurmSpec s registry
  chiefEngineerSpec s registry
  veneratedLoxodonSpec s registry
  convokeWindowSpec s registry
  assistSpec s registry
  treasureCruiseSpec s registry
  geyserLeaperSpec s registry
  kataraSpec s registry
  benevolentRiverSpiritSpec s registry
  waterbendersRestorationSpec s registry
  spiritWaterRevivalSpec s registry
  kataraSeekingRevengeSpec s registry
  hamaSpec s registry
  mindGrindSpec s registry
  flashSpec s registry

-- alice holds `card` and controls `n` untapped Mountains, plus Omniscience when
-- `granted` is True, with priority in her own precombat main phase so a sorcery
-- is castable (CR 307.1). The pairs below vary `granted` and NOTHING else:
-- every board is the same seats, the same stock and the same mana.
omniscienceBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Bool -> (ObjectId.ObjectId, GameState.GameState)
omniscienceBoard mountain omniscience card n granted =
  let base = S.landsInPlay mountain n
      withGrant = if granted then snd (S.addPermanent omniscience S.alice base) else base
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
    -- says, and it is scoped to that player's own HAND, which is what the
    -- sentence says. Asserted on the candidate list rather than on castability,
    -- because only one player holds priority on any one board and an instant bob
    -- cannot cast for want of priority would pass this for the wrong reason.
    --
    -- TWO copies of one card, differing in whose hand they lie in and in nothing
    -- else, both priced for ALICE -- the caster the arm now asks about, see #2169.
    -- So what the second answer isolates is "your hand" and not "you".
    Spec.it s "CR 118.9 the grant reaches its controller's own hand alone" $ do
      mountain <- S.printingOf s registry "Mountain"
      omniscience <- S.printingOf s registry "Omniscience"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (hers, granted) = omniscienceBoard mountain omniscience bolt 0 True
          (his, gs) = S.addHandCard bolt S.bob granted
          manaOf oid = fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName bolt) oid gs)
          red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      Spec.assertEqWith s "alice is offered the printed {R} and the grant's {0}" (manaOf hers) [Just (ManaCost.MkManaCost [red]), Just (ManaCost.MkManaCost [])]
      Spec.assertEqWith s "the identical copy in bob's hand is offered the printed {R} alone" (manaOf his) [Just (ManaCost.MkManaCost [red])]
    -- The other conjunct, on the same shape of board: bob controls the
    -- enchantment and alice does not, so the card in ALICE's hand is priced for
    -- her without the grant. Paired with the case above rather than folded into
    -- it, since one board cannot vary both the hand and the controller.
    Spec.it s "CR 109.5 an opponent's grant does not reach this player's hand" $ do
      mountain <- S.printingOf s registry "Mountain"
      omniscience <- S.printingOf s registry "Omniscience"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (hers, base) = omniscienceBoard mountain omniscience bolt 0 False
          gs = snd (S.addPermanent omniscience S.bob base)
          manaOf pid oid = fmap Cost.Type.mana (Cost.costsFor pid (S.printingName bolt) oid gs)
          red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      Spec.assertEqWith s "alice is offered the printed {R} alone" (manaOf S.alice hers) [Just (ManaCost.MkManaCost [red])]
      Spec.assertEqWith s "and bob, who does control it, is not offered {0} for a card in alice's hand" (manaOf S.bob hers) [Just (ManaCost.MkManaCost [red])]
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
        (fmap shapeOf (Cost.costsFor S.alice (S.printingName fireblastPrinting) fireblast gs))
        [ (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, red, red]), 0),
          (Just (ManaCost.MkManaCost []), 1),
          (Just (ManaCost.MkManaCost []), 0)
        ]
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
    -- CR 107.3b's 0 against CR 101.1's floor: Mind Grind's "X can't be 0" leaves
    -- the grant's {0} no legal X, so the cast is not offered at all (Mind Grind's
    -- Scryfall ruling says the same). A pair differing in the card alone: Blaze's
    -- {X}{R}, with no floor, is offered off the same landless grant.
    Spec.it s "CR 101.1/107.3b the grant does not offer a spell whose X can't be 0" $ do
      mountain <- S.printingOf s registry "Mountain"
      omniscience <- S.printingOf s registry "Omniscience"
      grind <- S.printingOf s registry "Mind Grind"
      blaze <- S.printingOf s registry "Blaze"
      let (grindId, grindGs) = omniscienceBoard mountain omniscience grind 0 True
          (blazeId, blazeGs) = omniscienceBoard mountain omniscience blaze 0 True
      Spec.assertBool s (not (any (S.isCastOf grindId) (Action.legalActions S.alice grindGs))) "CR 101.1 Mind Grind is not offered under the grant"
      Spec.assertBool s (any (S.isCastOf blazeId) (Action.legalActions S.alice blazeGs)) "and Blaze, whose X has no floor, is"

-- CR 601.2f / 800.4i: Avenge ({4}{W}{W}) "costs {2} less to cast if a player
-- attacked you during their last turn". Three seats; bob attacks carol on turn
-- 2 and concedes on turn 3. carol's four Plains pay {2}{W}{W} and not
-- {4}{W}{W}, so castability is the reduction. Asked in carol's main phase on
-- turn 3 and again on turn 5, after alice's turn and bob's skipped seat (CR
-- 800.4k); the whole-card discount is
-- data/scenarios/cost/cr-800-4i-avenge-costs-2-less-after-a-departed-player-attacked.json.
avengeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
avengeSpec s registry =
  Spec.describe s "Avenge" $ do
    Spec.it s "CR 800.4i a departed attacker's last turn counts until their next turn would have begun" $ do
      let islands = Seq.fromList [S.cardSetup "Island", S.cardSetup "Island"]
          seat pid = (S.playerSetup pid) {Seat.library = islands}
          setup =
            S.board
              ( seat S.alice
                  NonEmpty.:| [ (seat S.bob) {Seat.battlefield = Seq.singleton (S.settled "raider" "Goblin Piker")},
                                (seat S.carol)
                                  { Seat.battlefield = Seq.fromList (replicate 4 (S.permanent "Plains")),
                                    Seat.hand = Seq.singleton (S.aliased "avenge" (S.cardSetup "Avenge"))
                                  }
                              ]
              )
              S.alice
              S.precombatMain
          script =
            S.turn 2 [S.on S.declareAttackers S.bob (S.attack [S.aliasRef "raider"]), S.onSource S.declareAttackers S.bob (S.aliasRef "raider") (S.attackPlayer S.carol)]
              <> S.turn 3 [S.on (Phase.Beginning BeginningStep.Upkeep) S.bob Move.Concede]
          untilMain :: Natural.Natural -> Game.Type.Game ()
          untilMain n = do
            gs <- State.get
            Monad.unless (GameState.turnNumber gs == n && GameState.phase gs == Phase.PrecombatMain) (Engine.runStep >> untilMain n)
      built <- S.buildBoardOrFail s registry setup
      avenge <- maybe (Spec.assertFailure s "no avenge") pure (Map.lookup (Label.MkLabel (Text.pack "avenge")) (Staged.objects built))
      (_, third) <- S.runScriptOrFail s script built (untilMain 3)
      (_, fifth) <- S.runScriptOrFail s script built (untilMain 5)
      Spec.assertBool s (not (S.castable S.carol avenge fifth {GameState.priority = Just S.carol})) "turn 5, bob's turn having passed: Avenge keeps its {4}{W}{W} and is refused"
      Spec.assertBool s (S.castable S.carol avenge third {GameState.priority = Just S.carol}) "turn 3, bob gone: Avenge costs {2}{W}{W} and is offered"

-- CR 601.2f / 205.3m: Synchronized Eviction ({4}{U}) "costs {2} less to cast if
-- you control at least two creatures that share a creature type". alice holds it
-- over three Islands, which pay {2}{U} and not {4}{U}; boards differ only in
-- alice's second creature beside a Hill Giant. bob's Goblin Piker is on every
-- board, so alice's own Piker shares a type only with a creature she does not
-- control.
synchronizedEvictionSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
synchronizedEvictionSpec s registry =
  Spec.describe s "Synchronized Eviction" $ do
    Spec.it s "CR 205.3m two creatures alice controls sharing a creature type take {2} off" $ do
      island <- S.printingOf s registry "Island"
      giant <- S.printingOf s registry "Hill Giant"
      piker <- S.printingOf s registry "Goblin Piker"
      elves <- S.printingOf s registry "Llanowar Elves"
      changeling <- S.printingOf s registry "Woodland Changeling"
      eviction <- S.printingOf s registry "Synchronized Eviction"
      let board :: Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
          board second =
            let placed = List.foldl' (\g (p, who) -> snd (S.addPermanent p who g)) (S.landsInPlay island 3) [(giant, S.alice), (second, S.alice), (elves, S.bob), (piker, S.bob)]
                (evictionId, gs) = S.addHandCard eviction S.alice placed
             in (evictionId, gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice})
          (giantsId, giants) = board giant
          (pikerId, pikers) = board piker
          (changelingId, changelings) = board changeling
      Spec.assertBool s (S.castable S.alice giantsId giants) "two Giants share Giant, so it costs {2}{U} and is offered"
      Spec.assertBool s (not (S.castable S.alice pikerId pikers)) "a Giant and a Goblin share nothing, and bob's Goblin is not alice's, so it keeps {4}{U} and is refused"
      Spec.assertBool s (S.castable S.alice changelingId changelings) "CR 702.73a a changeling is a Giant too, so it costs {2}{U} and is offered"

-- CR 601.2f: Deem Inferior ({3}{U}) "costs {1} less to cast for each card you've
-- drawn this turn". alice holds it over two Islands, which pay {1}{U} and not
-- {2}{U}, with a Hill Giant of bob's to target. The turn's draw tally is written
-- straight onto the board: Pawl.EventTriggerSpec proves the draws that fill it.
deemInferiorSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
deemInferiorSpec s registry =
  Spec.describe s "Deem Inferior" $ do
    Spec.it s "CR 601.2f each card alice drew this turn takes {1} off, and bob's draws do not" $ do
      island <- S.printingOf s registry "Island"
      giant <- S.printingOf s registry "Hill Giant"
      deem <- S.printingOf s registry "Deem Inferior"
      let board drawn =
            let (_, gs1) = S.addPermanent giant S.bob (S.landsInPlay island 2)
                (deemId, gs2) = S.addHandCard deem S.alice gs1
             in (deemId, gs2 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice, GameState.drawsThisTurn = Map.fromList drawn})
          (twoId, two) = board [(S.alice, 2)]
          (oneId, one) = board [(S.alice, 1), (S.bob, 2)]
      Spec.assertBool s (S.castable S.alice twoId two) "alice drew two, so it costs {1}{U} and is offered"
      Spec.assertBool s (not (S.castable S.alice oneId one)) "alice drew one and bob two, so it costs {2}{U} and is refused"

-- CR 613.1f / 113.6d: Patriar's Humiliation's perpetual "loses all abilities"
-- follows the card Unsummon returns to alice's hand
-- (Pawl.SpecialActionSpec.humiliatedBoard), so the cost abilities printed on
-- it are gone there. Each pair differs only in whether the Humiliation took the
-- card's abilities or bob's Goblin Piker's; the Plains and Island paid for
-- both spells.
humiliationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
humiliationSpec s registry =
  Spec.describe s "CR 613.1f Patriar's Humiliation" $ do
    let build base victim = do
          plains <- S.printingOf s registry "Plains"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          humiliation <- S.printingOf s registry "Patriar's Humiliation"
          unsummon <- S.printingOf s registry "Unsummon"
          pure (humiliatedBoard base victim plains island piker humiliation unsummon)
    -- Two spells were cast this turn, so a Thrasta keeping its ability costs
    -- {4}{G}{G}, and six Forests pay it.
    Spec.it s "CR 601.2f a Thrasta that perpetually lost all abilities is not reduced" $ do
      forest <- S.printingOf s registry "Forest"
      thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
      board <- build (S.landsInPlay forest 6) thrasta
      let castableIn (mId, gs) = maybe False (\oid -> S.castable S.alice oid gs) mId
      Spec.assertBool s (not (castableIn (board True))) "CR 613.1f the humiliated Thrasta costs {10}{G}{G}, more than six Forests"
      Spec.assertBool s (castableIn (board False)) "the control: with the Piker humiliated instead, six Forests pay {4}{G}{G}"
    -- Asmoranomardicadaistinaculdacar has no mana cost (CR 118.6), so its
    -- alternative cost is the only way to cast it. alice discards a Piker of
    -- her own first, the alternative cost's condition.
    Spec.it s "CR 118.9 an Asmoranomardicadaistinaculdacar that perpetually lost all abilities has no alternative cost" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      asmor <- S.printingOf s registry "Asmoranomardicadaistinaculdacar"
      board <- build (S.landsInPlay swamp 1) asmor
      let castableIn (mId, gs) =
            let (pikerId, held) = S.addHandCard piker S.alice gs
                discarded = S.runPure S.identityAnswer held (Event.discard DiscardCause.Ordinary S.alice pikerId)
             in maybe False (\oid -> S.castable S.alice oid discarded) mId
      Spec.assertBool s (not (castableIn (board True))) "CR 613.1f the humiliated card back in hand cannot be cast for {B/R}"
      Spec.assertBool s (castableIn (board False)) "the control: with the Piker humiliated instead, the Swamp pays {B/R}"

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
      addElf (oids, gs) _ = let (oid, gs3) = S.addHandCard glistenerElf S.alice gs in (oid : oids, gs3)
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
            Nothing
            Nothing
            CostDirection.Less
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

-- alice holds Frogmite and controls `forests` untapped Forests and `golems`
-- Icehide Golems, with priority in her own precombat main phase.
--
-- The Golem is the thing COUNTED and the Forest is what PAYS, kept apart on
-- purpose: an artifact land would be both at once, and then no tapped count could
-- tell a reduction from a mana source.
frogmiteBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId.ObjectId, GameState.GameState)
frogmiteBoard forest icehideGolem frogmitePrinting forests golems =
  let base = S.landsInPlay forest forests
      (frogmite, gs1) = S.addHandCard frogmitePrinting S.alice base
      addGolem gs _ = snd (S.addPermanent icehideGolem S.alice gs)
      gs2 = List.foldl' addGolem gs1 [1 .. golems]
   in ( frogmite,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- CR 702.41a's affinity, which is rule 601.2f's cost reduction stated as a
-- KEYWORD rather than printed out as a sentence: Frogmite is a {4} artifact
-- creature with "affinity for artifacts", so its total steps 4, 3, 2, 1, 0, 0 ...
-- as the artifacts climb.
--
-- Every leg reads the total off S.tappedCount, thrastaSpec's reason: the Forests
-- the payment taps are what tells "reduced once per artifact" from "reduced once",
-- from "not reduced at all", and from a count that swept the Golems' controller
-- or the whole battlefield in.
frogmiteSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
frogmiteSpec s registry =
  Spec.describe s "Frogmite" $ do
    Spec.it s "Frogmite is a {4} with affinity for artifacts" $ do
      frogmitePrinting <- S.printingOf s registry "Frogmite"
      let face = S.combinedFace frogmitePrinting
      Spec.assertEqWith
        s
        "the printed mana cost"
        (Face.manaCost face)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]))
      Spec.assertEqWith
        s
        "and one affinity, for artifacts"
        (Set.toList (Face.keywordSet face))
        [Keyword.Affinity (Filter.Type.HasCardType CardType.Artifact)]
    -- The negative, and the SAME board with ONE thing changed: no Golems. The
    -- same three Forests, the same seats, phase and priority -- so the refusal is
    -- the {2} the two artifacts would have taken off and nothing else.
    Spec.it s "CR 702.41a no artifacts means no reduction, and the printed {4} does not pay" $ do
      forest <- S.printingOf s registry "Forest"
      icehideGolem <- S.printingOf s registry "Icehide Golem"
      frogmitePrinting <- S.printingOf s registry "Frogmite"
      let (frogmite, gs) = frogmiteBoard forest icehideGolem frogmitePrinting 3 0
      Spec.assertBool s (not (S.castable S.alice frogmite gs)) "the printed {4} is out of reach of three Forests"
      Spec.assertEqWith s "and Frogmite is not offered" (filter (S.isCastOf frogmite) (Action.legalActions S.alice gs)) []

-- alice holds `spell` and controls four untapped Forests, two Icehide Golems
-- and `third`, with priority in her own precombat main phase. `third` is
-- Mycosynth Golem or a third Icehide Golem, the one thing the boards differ in:
-- three artifacts either way.
mycosynthBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
mycosynthBoard forest icehideGolem third spellPrinting =
  let (spell, gs1) = frogmiteBoard forest icehideGolem spellPrinting 4 2
      (_, gs2) = S.addPermanent third S.alice gs1
   in (spell, gs2)

-- CR 702.41a / 613.1f: Mycosynth Golem's "artifact creature spells you cast have
-- affinity for artifacts" is a keyword the spell HAS, so its total is reduced
-- like Frogmite's printed one -- at the gate (CR 601.2a has the card on the
-- stack when CR 601.2f totals it) and at the payment alike. Venser's Sliver is a
-- {5} artifact creature with no affinity of its own.
--
-- The Forests tapped separate the readings: 2 for three artifacts' {3}, 4 for a
-- flat {1}, and no cast at all for no reduction.
mycosynthGolemSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mycosynthGolemSpec s registry =
  Spec.describe s "Mycosynth Golem" $ do
    Spec.it s "CR 702.41a a granted affinity takes {3} off a {5} artifact creature spell" $ do
      forest <- S.printingOf s registry "Forest"
      icehideGolem <- S.printingOf s registry "Icehide Golem"
      golem <- S.printingOf s registry "Mycosynth Golem"
      sliver <- S.printingOf s registry "Venser's Sliver"
      let (spell, gs) = mycosynthBoard forest icehideGolem golem sliver
          cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertEqWith s "exactly two Forests paid the reduced {2}" (S.tappedCount S.alice resolved) 2
      Spec.assertBool s (S.castable S.alice spell gs) "and the gate offered it"
      Spec.assertEqWith
        s
        "and Venser's Sliver resolved onto the battlefield"
        (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Venser's Sliver")) S.alice resolved)
        1
    -- The negative: the same three artifacts with no grant among them.
    Spec.it s "CR 702.41a with no grant the printed {5} is out of reach of four Forests" $ do
      forest <- S.printingOf s registry "Forest"
      icehideGolem <- S.printingOf s registry "Icehide Golem"
      sliver <- S.printingOf s registry "Venser's Sliver"
      let (spell, gs) = mycosynthBoard forest icehideGolem icehideGolem sliver
      Spec.assertBool s (not (S.castable S.alice spell gs)) "the unreduced {5} is not castable"
    -- CR 702.41b: Frogmite's printed affinity and the granted one each apply, so
    -- three artifacts take {6} off its {4} and nothing is tapped; one instance
    -- would leave {1}.
    Spec.it s "CR 702.41b a printed and a granted affinity both apply" $ do
      forest <- S.printingOf s registry "Forest"
      icehideGolem <- S.printingOf s registry "Icehide Golem"
      golem <- S.printingOf s registry "Mycosynth Golem"
      frogmitePrinting <- S.printingOf s registry "Frogmite"
      let (spell, gs) = mycosynthBoard forest icehideGolem golem frogmitePrinting
          cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertEqWith s "nothing was tapped to pay {0}" (S.tappedCount S.alice resolved) 0
      Spec.assertEqWith
        s
        "and Frogmite resolved onto the battlefield"
        (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Frogmite")) S.alice resolved)
        1

-- alice holds Sublime Exhalation and controls five untapped Plains, with priority
-- in her own precombat main phase. `seats` is the whole roster, which is the one
-- thing the two boards below differ in.
exhalationBoard :: NonEmpty.NonEmpty PlayerId.PlayerId -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
exhalationBoard seats plains exhalationPrinting =
  let base = S.landsFor plains S.alice 5 (Setup.emptyGame seats)
      (exhalation, gs1) = S.addHandCard exhalationPrinting S.alice base
   in ( exhalation,
        gs1
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- CR 702.125a's undaunted, affinity's sibling with the count over PLAYERS instead
-- of over permanents: Sublime Exhalation is {6}{W} and costs {1} less for each
-- opponent, so alice's total is six mana against one opponent and five against
-- two.
--
-- The pair below is one board with ONE thing changed -- a third seat -- against
-- the same five Plains, so what the second board casts is the {1} carol is worth
-- and nothing else. Three seats also keep "an opponent" from collapsing onto "the
-- other player".
exhalationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exhalationSpec s registry =
  Spec.describe s "Sublime Exhalation" $ do
    Spec.it s "Sublime Exhalation is a {6}{W} with undaunted" $ do
      exhalationPrinting <- S.printingOf s registry "Sublime Exhalation"
      let face = S.combinedFace exhalationPrinting
      Spec.assertEqWith
        s
        "the printed mana cost"
        (Face.manaCost face)
        (Just (ManaCost.MkManaCost [ManaSymbol.Generic 6, ManaSymbol.OfType (ManaType.Colored Color.White)]))
      Spec.assertEqWith
        s
        "and undaunted, which carries no payload"
        (Set.toList (Face.keywordSet face))
        [Keyword.Undaunted]
    Spec.it s "CR 702.125a one opponent takes only {1} off, and that does not pay" $ do
      plains <- S.printingOf s registry "Plains"
      exhalationPrinting <- S.printingOf s registry "Sublime Exhalation"
      let (exhalation, gs) = exhalationBoard S.bothPlayers plains exhalationPrinting
      Spec.assertBool s (not (S.castable S.alice exhalation gs)) "the reduced {5}{W} is out of reach of five Plains"
      Spec.assertEqWith s "and it is not offered" (filter (S.isCastOf exhalation) (Action.legalActions S.alice gs)) []
    -- CR 601.2a / 601.2f / 613.1f: the total is determined with the spell on the
    -- stack, so an effect confined to the zone the card is cast FROM no longer
    -- reaches the keyword that reduces it. Sublime Exhalation in alice's
    -- graveyard, bob's Yixlid Jailer ("cards in graveyards lose all abilities")
    -- entered first, alice's Lier, Disciple of the Drowned (flashback at its mana
    -- cost) after it: the card has flashback and no undaunted where it lies, but
    -- the spell has undaunted, so its {6}{W} flashback costs {5}{W} against one
    -- opponent and six Plains pay it. Priced in the graveyard the gate would ask
    -- {6}{W} and refuse.
    Spec.it s "CR 601.2f undaunted the graveyard card lost applies to the spell" $ do
      plains <- S.printingOf s registry "Plains"
      exhalationPrinting <- S.printingOf s registry "Sublime Exhalation"
      jailer <- S.printingOf s registry "Yixlid Jailer"
      lier <- S.printingOf s registry "Lier, Disciple of the Drowned"
      let base = S.landsFor plains S.alice 6 (Setup.emptyGame S.bothPlayers)
          (_, withJailer) = S.addPermanent jailer S.bob base
          (_, withLier) = S.addPermanent lier S.alice withJailer
          (exhalation, gs1) = S.addGraveyardCard exhalationPrinting S.alice withLier
          gs =
            gs1
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
          cast = S.runPure S.identityAnswer gs (S.cast S.alice exhalation)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertBool s (S.castable S.alice exhalation gs) "the gate offers the {5}{W} flashback"
      Spec.assertEqWith s "and all six Plains paid it" (S.tappedCount S.alice resolved) 6
      Spec.assertEqWith s "and the flashed-back Exhalation was exiled" (length (Game.zoneMembers Zone.Exile S.alice resolved)) 1

-- alice controls a Safehold Sentry and three Plains, all settled. `tapped` says
-- whether the Sentry itself starts tapped -- which for a {Q} cost is the payable
-- state, the exact inverse of every {T} fixture in this file.
sentryBoard :: Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, GameState.GameState)
sentryBoard plains safeholdSentry tapped =
  let base = S.landsInPlay plains 3
      (sentry, gs1) = S.addPermanent safeholdSentry S.alice base
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
    -- CR 107.6's second sentence, and the exact inverse of TapThis: "A permanent
    -- that's already untapped can't be untapped again to pay the cost."
    Spec.it s "CR 107.6 an UNTAPPED Sentry cannot pay {Q}, so the ability is not offered" $ do
      plains <- S.printingOf s registry "Plains"
      safeholdSentry <- S.printingOf s registry "Safehold Sentry"
      let (sentry, gs) = sentryBoard plains safeholdSentry False
      Spec.assertBool s (not (Cost.canPay PaymentSubject.ForNeither S.alice sentry (ActivatedAbility.cost (theAbility safeholdSentry)) gs)) "canPay says no"
      Spec.assertBool s (not (any isActivateAction (Action.legalActions S.alice gs))) "and no Activate is offered"
    -- The issue itself (#204): CR 302.6 names the tap symbol AND the untap
    -- symbol, and only the first half had a producer. A summoning-sick Sentry
    -- must not be able to activate a {Q} ability.
    Spec.it s "CR 302.6 a summoning-sick Sentry's {Q} ability is not offered" $ do
      plains <- S.printingOf s registry "Plains"
      safeholdSentry <- S.printingOf s registry "Safehold Sentry"
      let (sentry, gs) = sentryBoard plains safeholdSentry True
          sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) sentry (GameState.objects gs)}
      Spec.assertBool s (Cost.canPay PaymentSubject.ForNeither S.alice sentry (ActivatedAbility.cost (theAbility safeholdSentry)) sick) "the cost itself is still payable -- it is tapped"
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

-- Cathartic Reunion {1}{R} Sorcery: "As an additional cost to cast this spell,
-- discard two cards. Draw three cards." The card CR 601.2f's "discarding cards"
-- clause was waiting for.
catharticReunionSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
catharticReunionSpec s registry =
  Spec.describe s "Cathartic Reunion" $ do
    Spec.it s "CR 601.2f with only one other card in hand the spell is not castable" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      let (reunion, gs) = catharticBoard mountain piker catharticReunion 1
      -- Read through costsFor, so the assertion is against the cost the engine
      -- would actually offer (mana cost plus the printed additional cost),
      -- never a hand-built one.
      Spec.assertBool s (not (any (\c -> Cost.canPay PaymentSubject.ForNeither S.alice reunion c gs) (Cost.costsFor S.alice (S.printingName catharticReunion) reunion gs))) "no offered cost is payable"
      Spec.assertBool s (not (any (S.isCastOf reunion) (Action.legalActions S.alice gs))) "and no Cast is offered"

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
magmaticInsightSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
magmaticInsightSpec s registry =
  Spec.describe s "Magmatic Insight" $ do
    -- The negative, one card different: a hand with no land card at all. Same
    -- Mountain, same hand size, same phase -- so an unpayable cost is the only
    -- thing that can withhold the cast.
    Spec.it s "CR 601.2f a landless hand cannot pay, however many cards it holds" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      magmaticInsight <- S.printingOf s registry "Magmatic Insight"
      let (insight, _, gs) = magmaticBoard mountain piker magmaticInsight piker
      Spec.assertEqWith s "the hand is the same size as the payable board's" (S.handSize S.alice gs) 3
      Spec.assertBool s (not (any (\c -> Cost.canPay PaymentSubject.ForNeither S.alice insight c gs) (Cost.costsFor S.alice (S.printingName magmaticInsight) insight gs))) "no offered cost is payable"
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
-- keeps a mana ability off the stack, so Activatable.activatable answers False for
-- this ability on every board and no case may route through it.
springleafBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
springleafBoard drum first second =
  let (drumId, gs0) = S.addPermanent drum S.alice (Setup.emptyGame S.bothPlayers)
      (firstId, gs1) = S.addPermanent first S.alice gs0
      (secondId, gs2) = S.addPermanent second S.alice gs1
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
-- cost was PAID: an unpaid one is reversed at CR 601.2h's moment, and
-- S.identityAnswer reverses the mana abilities with it (CR 733.1), so a refused
-- payment adds nothing and taps nothing. announcedReversalSpec below is the
-- payer who keeps them.
pooledFrom :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> Int
pooledFrom answer oid gs = case Game.poolOf S.alice (S.runPure answer gs (S.tapForMana oid)) of
  Mana.Type.MkMana units -> length units

isTapped :: ObjectId.ObjectId -> GameState.GameState -> Bool
isTapped oid gs = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Tapped

-- The board after tapping the Drum for mana with `answer`.
afterDrum :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
afterDrum answer drumId gs = S.runPure answer gs (S.tapForMana drumId)

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
        component = CostComponent.TapPermanents (TapPermanents.MkTapPermanents 1 (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You, Filter.Type.Not Filter.Type.IsTapped]) False)
    Spec.assertBool s (Cost.canPayComponent Map.empty S.alice drumId component payable) "two untapped creatures pay"
    Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice drumId component unpayable)) "two tapped ones do not"
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
-- untapped Elves you control: Proliferate. Activate only as a sorcery." A
-- producer for CostComponent.TapPermanents that exercises a COUNT ABOVE ONE --
-- Springleaf Drum's is one, and so is Hollow Warrior's, where 1 and "some"
-- cannot be told apart. Heritage Druid's is three as well, and is a MANA
-- ability, which is what puts its case in Pawl.ManaSpec instead.
--
-- Morcant is itself an Elf and the cost does not say "another", so it is one of
-- its own candidates. Four candidates against a count of three is what makes the
-- prompt a real choice.
--
-- Not a mana ability, so unlike the Drum this one is legitimately asked of
-- Activatable.activatable (CR 605.3b is what bars that for the Drum).
morcantBoard :: Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
morcantBoard morcant elves =
  let (morcantId, gs0) = S.addPermanent morcant S.alice (Setup.emptyGame S.bothPlayers)
      add (ids, g) p = let (oid, g1) = S.addPermanent p S.alice g in (ids <> [oid], g1)
      (elfIds, gs1) = List.foldl' add ([], gs0) elves
   in ( morcantId,
        elfIds,
        gs1
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

morcantSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
    Spec.assertBool s (Activatable.activatable S.alice enoughId (theAbility morcant) enough) "Morcant and two Elves: activatable"
    Spec.assertBool s (not (Activatable.activatable S.alice shortId (theAbility morcant) short)) "Morcant and one Elf: not"

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
      (slingId, withSling) = S.addPermanent sling S.alice combat
      withLands = S.landsFor forest S.alice 3 withSling
   in (slingId, griffinId, giantId, S.runPure (attackingWith griffinId) withLands (Combat.declareAttackers S.manaPerformer S.alice))

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

-- Meloku the Clouded Mirror {4}{U} 2\/4 Legendary Creature -- Moonfolk Wizard:
-- "Flying. {1}, Return a land you control to its owner's hand: Create a 1\/1
-- blue Illusion creature token with flying." The gate card for CostComponent's
-- ReturnPermanents -- a count plus a criterion over a return to hand, which
-- ReturnThis cannot express (it names the object the cost is on).
--
-- alice's board is Meloku, a Llanowar Elves for the {1}, and the lands the
-- caller names. TWO lands of DIFFERENT printings make the prompt a real choice
-- and make WHICH one went back visible by name -- an id read would not: a
-- permanent returned to hand is a new object (CR 400.7).
--
-- The Elves and not a third land pays the mana, so the negative board below can
-- drop every land alice controls and still afford the mana half.
melokuBoard :: Printing.Printing -> Printing.Printing -> [(Printing.Printing, PlayerId.PlayerId)] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
melokuBoard meloku elves lands =
  let (melokuId, gs0) = S.addPermanent meloku S.alice (Setup.emptyGame S.bothPlayers)
      (_, gs1) = S.addPermanent elves S.alice gs0
      add (ids, gs) (land, pid) = let (oid, gsN) = addLand land pid gs in (ids <> [oid], gsN)
      (landIds, gs2) = List.foldl' add ([], gs1) lands
   in ( melokuId,
        landIds,
        gs2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- One untapped land under pid, and its id -- S.landsFor hands back only a board,
-- so the new object is the one the battlefield gained.
addLand :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addLand land pid gs =
  let after = S.landsFor land pid 1 gs
   in ( case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield gs)) of
          [oid] -> oid
          _ -> S.noSource,
        after
      )

-- The first of a fixture's land ids, S.noSource for the empty list its callers
-- never build -- total, since -Wx-partial refuses `head`.
firstOf :: [ObjectId.ObjectId] -> ObjectId.ObjectId
firstOf = Maybe.fromMaybe S.noSource . Maybe.listToMaybe

-- Answer Prompt.ChooseReturns with nothing at all, which is not a size-1 subset
-- -- the reject-not-repair probe.
returningNothing :: Prompt.Prompt r -> r
returningNothing p = case p of
  Prompt.ChooseReturns {} -> Set.empty
  _ -> S.identityAnswer p

-- The names in pid's hand. NAMES and not ids: CR 400.7 makes the returned
-- permanent a new object, so an id-keyed read would miss it however the cost was
-- paid.
handNames :: PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
handNames pid gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Hand pid gs)

melokuSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
melokuSpec s registry = Spec.describe s "Meloku the Clouded Mirror" $ do
  -- Reject-not-repair, Cost.payComponent's posture: an answer that is not a
  -- size-1 subset leaves the whole cost unpaid, and CR 601.2h's ban on partial
  -- payments is what puts both lands back on the battlefield.
  Spec.it s "CR 601.2h an answer of the wrong size pays nothing at all" $ do
    meloku <- S.printingOf s registry "Meloku the Clouded Mirror"
    elves <- S.printingOf s registry "Llanowar Elves"
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    let (melokuId, _, gs) = melokuBoard meloku elves [(island, S.alice), (mountain, S.alice)]
        after = S.runPure returningNothing gs (Activate.activateAbility S.alice melokuId (theAbility meloku))
    Spec.assertEqWith s "alice's hand is empty" (handNames S.alice after) []
    Spec.assertEqWith s "the Island stayed" (S.countOnBattlefieldByName (S.printingName island) S.alice after) 1
    Spec.assertEqWith s "the Mountain stayed" (S.countOnBattlefieldByName (S.printingName mountain) S.alice after) 1
    Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack after)) 0
  -- The negative, a PAIR of boards differing in exactly one thing: who controls
  -- the Island. The Llanowar Elves pays the {1} on both, so the mana half is
  -- identical and an unpayable component is the only thing that can withhold the
  -- activation.
  Spec.it s "CR 118.3 a land nobody you-control admits means no activation" $ do
    meloku <- S.printingOf s registry "Meloku the Clouded Mirror"
    elves <- S.printingOf s registry "Llanowar Elves"
    island <- S.printingOf s registry "Island"
    let (payableId, landIds, payable) = melokuBoard meloku elves [(island, S.alice)]
        unpayable = S.giveControl (firstOf landIds) S.bob payable
        component = CostComponent.ReturnPermanents (ReturnPermanents.MkReturnPermanents 1 (Filter.Type.And [Filter.Type.HasCardType CardType.Land, Filter.Type.ControlledBy PlayerRelation.You]))
    Spec.assertBool s (Activatable.activatable S.alice payableId (theAbility meloku) payable) "a land alice controls: activatable"
    Spec.assertBool s (not (Activatable.activatable S.alice payableId (theAbility meloku) unpayable)) "the same land under bob: not"
    Spec.assertBool s (Cost.canPayComponent Map.empty S.alice payableId component payable) "and the component itself is payable on the one board"
    Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice payableId component unpayable)) "and not on the other"

-- alice with Everbark Shaman settled on the battlefield, `buried` in her own
-- graveyard, and Maskwood Nexus beside the Shaman when `withNexus`. Priority in
-- her own precombat main phase and an empty stack, on every board.
--
-- The Shaman's ability costs {T} and an exile, with an EMPTY mana part, so no
-- case here can turn on mana. Maskwood Nexus is the only thing a case varies
-- besides what is buried.
everbarkBoard :: Printing.Printing -> Printing.Printing -> Bool -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
everbarkBoard shaman nexus withNexus buried =
  let (shamanId, gs0) = S.addPermanent shaman S.alice (Setup.emptyGame S.bothPlayers)
      gs1 = if withNexus then snd (S.addPermanent nexus S.alice gs0) else gs0
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
      Spec.assertBool s (Activatable.activatable S.alice shamanId ability gs) "activatable"
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
      Spec.assertBool s (not (Activatable.activatable S.alice shamanId (theAbility shaman) gs)) "not activatable"
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
      Spec.assertBool s (not (Activatable.activatable S.alice shamanId (theAbility shaman) gs)) "not activatable"
      Spec.assertEqWith s "and the Bolt is still buried" (length (Game.zoneMembers Zone.Graveyard S.alice gs)) 1

-- alice with a face-down Putrid Raptor on the battlefield, `held` in hand, and
-- Maskwood Nexus beside it when `withNexus`. Three Mountains pay CR 702.37a's
-- {3} for the face-down cast; the morph cost itself has no mana in it, so the
-- turn-up gate cannot be turning on mana. Nothing if the face-down cast did not
-- land.
raptorBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> [Printing.Printing] -> Maybe (ObjectId.ObjectId, GameState.GameState)
raptorBoard mountain raptor nexus withNexus held =
  let (gs0, card) = S.handOne raptor (S.landsInPlay mountain 3)
      gs1 = if withNexus then snd (S.addPermanent nexus S.alice gs0) else gs0
      gs2 = List.foldl' (\gs printing -> snd (S.addHandCard printing S.alice gs)) gs1 held
      before = Set.toList (GameState.battlefield gs2)
      after =
        S.runPure
          S.identityAnswer
          gs2
          (Cast.castSpell S.manaPerformer S.alice card (S.printingName raptor) (Facing.faceDown FaceDownReason.Morphed) >> Stack.resolveTop)
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
          let after = S.runPure S.identityAnswer gs (FaceDown.turnFaceUp S.manaPerformer S.alice TurnUpProcedure.Morph permanent)
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

-- alice active with priority in her own precombat main phase: one Barkhide Troll
-- settled on the battlefield carrying exactly ONE +1/+1 counter, and TWO
-- untapped Forests. Nothing else, and no board for the second seat -- nothing in
-- this rule reads an opponent.
--
-- TWO lands and not one, which is the element that makes the re-offer assertion
-- discriminate: both readings spend {1} on the first activation, so with one land
-- the ability is refused a second time for want of mana whatever the counter did,
-- and the assertion would pass under a payment that removed nothing. With two the
-- mana is there and the counter is the only thing left that can refuse.
--
-- Exactly ONE counter and not two, which closes the same hole from the other
-- side: two would leave a CORRECT payment with a counter still on, so the ability
-- would be re-offered and the Troll would still be bigger than its printed 2/2.
--
-- Forests rather than any land: the cost is {1}, which any land pays, but a green
-- source keeps the fixture honest for the cast case below.
trollBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
trollBoard troll forest =
  let (trollId, gs) = S.addPermanent troll S.alice (S.landsInPlay forest 2)
   in ( trollId,
        (S.addCounter CounterKind.PlusOnePlusOne 1 trollId gs)
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- The CR 122 removals a run recorded. Pawl.Support has zoneChangesOf,
-- damageEventsOf and revealsOf and no counter sibling, so this group keeps its
-- own -- and it reads the CONTENTS rather than a length, since
-- Event.removeCounters records nothing at all when there was nothing to remove.
counterRemovalsOf :: GameState.GameState -> [CounterChange.CounterChange]
counterRemovalsOf gs =
  let removal e = case e of
        GameEvent.CountersRemoved change -> Just change
        _ -> Nothing
   in Maybe.mapMaybe removal (S.eventsOf gs)

-- Barkhide Troll {G}{G} Creature -- Troll 2/2 (Oracle text checked against
-- Scryfall): "This creature enters with a +1/+1 counter on it. {1}, Remove a
-- +1/+1 counter from this creature: This creature gains hexproof until end of
-- turn."
--
-- A producer for CostComponent.RemoveCountersFromThis, CR 118.1's
-- counter removal as an activation cost. The counter is what CR 613.4c's layer 7c
-- reads, so the payment is observable as a SIZE and not only as a map entry: an
-- implementation that does not remove it leaves a 3/3 that can pay again.
barkhideTrollSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
barkhideTrollSpec s registry =
  Spec.describe s "Barkhide Troll" $ do
    -- The gameplay-level assertion is the SIZE, and it comes first: the re-offer
    -- count below it is a proxy a wrong payment reddens too.
    Spec.it s "CR 118.1 / 613.4c the cost removes the +1/+1 counter, so the 3/3 becomes a 2/2 that cannot pay again" $ do
      troll <- S.printingOf s registry "Barkhide Troll"
      forest <- S.printingOf s registry "Forest"
      let (trollId, gs) = trollBoard troll forest
          ability = theAbility troll
          activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice trollId ability)
          after = S.runPure S.identityAnswer activated Stack.resolveTop
      Spec.assertEqWith s "CR 613.4c the counter makes it a 3/3 before any of this" (S.powerToughnessOf trollId gs) (Just (3, 3))
      Spec.assertEqWith s "CR 613.4c the cost spent the counter, so the printed 2/2 is back" (S.powerToughnessOf trollId after) (Just (2, 2))
      Spec.assertEqWith s "CR 122.1 and no +1/+1 counter is left on it" (S.counterOf CounterKind.PlusOnePlusOne trollId after) 0
      Spec.assertEqWith s "CR 118.3 so the ability is not offered again, though a second Forest is still untapped" (length (filter (isActivateOf trollId) (Action.legalActions S.alice after))) 0
      -- SEPARATE from the three above, and that is the point: a payment that
      -- edited Object.counters directly would leave them green and only this red.
      Spec.assertEqWith s "CR 122 the removal went through Event.removeCounters, 1 -> 0" (counterRemovalsOf after) [CounterChange.MkCounterChange trollId CounterKind.PlusOnePlusOne 1 0]
      Spec.assertBool s (Projection.hasKeyword (Keyword.Hexproof Nothing) trollId after) "and the ability resolved, granting hexproof"
    -- The board it was offered on, so the refusal above is a refusal and not an
    -- ability that was never on the menu.
    Spec.it s "CR 118.3 with the counter on it the ability IS offered" $ do
      troll <- S.printingOf s registry "Barkhide Troll"
      forest <- S.printingOf s registry "Forest"
      let (trollId, gs) = trollBoard troll forest
      Spec.assertBool s (Activatable.activatable S.alice trollId (theAbility troll) gs) "activatable"
      Spec.assertEqWith s "and menued exactly once" (length (filter (isActivateOf trollId) (Action.legalActions S.alice gs))) 1

-- Zameck Guildmage {G}{U} Creature -- Elf Wizard 2/2 (Oracle text checked
-- against Scryfall 2026-09-17): "{G}{U}: This turn, each creature you control
-- enters with an additional +1/+1 counter on it. / {G}{U}, Remove a +1/+1
-- counter from a creature you control: Draw a card."
--
-- The producer for CostComponent.RemoveCounters, CR 118.1's counter
-- removal aimed at a permanent the PAYER CHOOSES rather than at the object the
-- cost is on (Barkhide Troll's, above). The first ability is
-- Pawl.EntryReplacementSpec's; only the second is read here.
--
-- THE BOARD: alice controls the Guildmage, a Forest and an Island -- exactly the
-- {G}{U} the ability wants, so nothing below is an unaffordable refusal -- plus a
-- Goblin Piker and a Hill Giant, two DIFFERENT printings so which one paid is
-- visible by name. The Guildmage itself carries no counter and so is not a
-- candidate. alice's library holds two cards, which keeps CR 104.3c out of the
-- draw.
zameckGuildmageCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
zameckGuildmageCostSpec s registry =
  Spec.describe s "Zameck Guildmage" $ do
    -- The gameplay-level assertions come FIRST: which creature lost the counter,
    -- then that the other kept its own, then the draw the ability paid for. The
    -- prompt record below them is a proxy -- a payment that ignored the answer
    -- reddens it too, and it would report itself if it ran first.
    Spec.it s "CR 601.2h the payer chooses which creature the +1/+1 counter comes off, and the ability then draws" $ do
      mage <- S.printingOf s registry "Zameck Guildmage"
      piker <- S.printingOf s registry "Goblin Piker"
      giant <- S.printingOf s registry "Hill Giant"
      forest <- S.printingOf s registry "Forest"
      island <- S.printingOf s registry "Island"
      let (mageId, pikerId, giantId, board) = zameckBoard mage piker giant forest island [1, 1]
          ability = secondAbility mage
          ((_, paid), asked) = State.runState (Engine.runGame (recordingCounterRemovals giantId) board (Activate.activateAbility S.alice mageId ability)) []
          after = S.runPure S.identityAnswer paid Stack.resolveTop
      Spec.assertEqWith s "CR 122.1 the counter came off the creature the payer named" (S.counterOf CounterKind.PlusOnePlusOne giantId after) 0
      Spec.assertEqWith s "and the creature it was chosen over kept its own" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 1
      Spec.assertEqWith s "CR 121.1 and the ability that cost paid for drew a card" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
      Spec.assertEqWith s "CR 122 the removal went through Event.removeCounters, 1 -> 0" (counterRemovalsOf after) [CounterChange.MkCounterChange giantId CounterKind.PlusOnePlusOne 1 0]
      Spec.assertEqWith s "and the payer was asked exactly once, over both counter-bearing creatures" asked [List.sort [pikerId, giantId]]
    -- The elision, on a board differing from the one above in ONE thing: the
    -- Piker carries no counter, so CR 118.3 leaves a single candidate and
    -- performing the payment decides nothing.
    Spec.it s "CR 118.3 one candidate is not a choice, so nothing is asked" $ do
      mage <- S.printingOf s registry "Zameck Guildmage"
      piker <- S.printingOf s registry "Goblin Piker"
      giant <- S.printingOf s registry "Hill Giant"
      forest <- S.printingOf s registry "Forest"
      island <- S.printingOf s registry "Island"
      let (mageId, pikerId, giantId, board) = zameckBoard mage piker giant forest island [0, 1]
          ((_, paid), asked) = State.runState (Engine.runGame (recordingCounterRemovals giantId) board (Activate.activateAbility S.alice mageId (secondAbility mage))) []
      Spec.assertEqWith s "the one candidate paid" (S.counterOf CounterKind.PlusOnePlusOne giantId paid) 0
      Spec.assertEqWith s "the Piker had none to lose either way" (S.counterOf CounterKind.PlusOnePlusOne pikerId paid) 0
      Spec.assertEqWith s "and the payer was asked nothing" asked []
    -- CR 118.3 / 601.2h, over the same board with the counters taken away: the
    -- ability is not OFFERED at all. A PAIR differing in exactly one thing --
    -- the counters -- with the first ability's own offer as the control, since
    -- its {G}{U} is paid out of the same two lands.
    Spec.it s "CR 118.3 with no +1/+1 counter anywhere the ability is not offered" $ do
      mage <- S.printingOf s registry "Zameck Guildmage"
      piker <- S.printingOf s registry "Goblin Piker"
      giant <- S.printingOf s registry "Hill Giant"
      forest <- S.printingOf s registry "Forest"
      island <- S.printingOf s registry "Island"
      let (mageId, _, _, withCounters) = zameckBoard mage piker giant forest island [1, 1]
          (bareMageId, _, _, without) = zameckBoard mage piker giant forest island [0, 0]
          offers oid ability gs = length (filter (isActivateOfAbility oid ability) (Action.legalActions S.alice gs))
      Spec.assertEqWith s "CR 602.2b offered while a creature carries one" (offers mageId (secondAbility mage) withCounters) 1
      Spec.assertEqWith s "and not offered with none" (offers bareMageId (secondAbility mage) without) 0
      Spec.assertEqWith s "the control: the Guildmage's other {G}{U} ability is offered on BOTH boards, so the refusal is the counters' doing" (offers bareMageId (theAbility mage) without) 1

-- The board zameckGuildmageCostSpec's three cases share: alice's Guildmage over
-- a Forest and an Island, her Goblin Piker and Hill Giant carrying the two
-- counter counts given, and two cards in her library. Answers the Guildmage, the
-- Piker and the Giant.
--
-- bob's own Hill Giant carries a +1/+1 counter on EVERY board, and that is what
-- makes the criterion's "you control" load-bearing rather than decorative: a
-- pool read without the payer's perspective would offer it, and the case with
-- alice's counters taken away would find a candidate anyway.
zameckBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> [Natural.Natural] -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
zameckBoard mage piker giant forest island counts =
  let lands = S.landsFor island S.alice 1 (S.landsFor forest S.alice 1 (Setup.emptyGame S.bothPlayers))
      (mageId, withMage) = S.addPermanent mage S.alice lands
      (pikerId, withPiker) = S.addPermanent piker S.alice withMage
      (giantId, withGiant) = S.addPermanent giant S.alice withPiker
      (bobsGiantId, withBobs) = S.addPermanent giant S.bob withGiant
      (_, stocked) = S.addLibraryCard forest S.alice (snd (S.addLibraryCard island S.alice withBobs))
      opposed = S.addCounter CounterKind.PlusOnePlusOne 1 bobsGiantId stocked
      counted = case counts of
        [onPiker, onGiant] -> S.addCounter CounterKind.PlusOnePlusOne onGiant giantId (S.addCounter CounterKind.PlusOnePlusOne onPiker pikerId opposed)
        _ -> opposed
   in ( mageId,
        pikerId,
        giantId,
        counted
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- The SECOND activated ability of a printing, where `theAbility` above takes the
-- first. Total for this group's fixtures; the fallback is unreachable.
secondAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
secondAbility p = case Face.activatedAbilities (S.combinedFace p) of
  _ : ab : _ -> ab
  _ -> theAbility p

-- An Activate of this source AND this ability, where `isActivateOf` above counts
-- every ability of the source. Zameck Guildmage prints two, so the two have to
-- be told apart.
isActivateOfAbility :: ObjectId.ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Action.Type.Action -> Bool
isActivateOfAbility oid ability action = case action of
  Action.Type.Activate src ab -> src == oid && ab == ability
  _ -> False

-- Answers Prompt.ChooseCounterRemoval with the creature named, recording each
-- prompt's candidates ascending. PINNED to one object rather than searching for
-- a legal one, and filtered against the offer so the answer is one the engine
-- put up: Replay.defaultAnswer would take the head, so a mutation that ignores
-- the answer cannot come back green through this answerer.
recordingCounterRemovals :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
recordingCounterRemovals wanted p = case p of
  Prompt.ChooseCounterRemoval _ _ _ candidates -> do
    State.modify' (<> [List.sort (NonEmpty.toList candidates)])
    pure (if List.elem wanted (NonEmpty.toList candidates) then wanted else NonEmpty.head candidates)
  _ -> pure (S.identityAnswer p)

-- Novijen Sages {4}{U}{U} Creature -- Human Advisor Mutant 0/0 (Oracle text
-- checked against Scryfall 2026-09-26): "Graft 4 ... {1}, Remove two +1/+1
-- counters from among creatures you control: Draw a card."
--
-- The producer for CounterSpread.FromAmong, CR 118.1's counter removal DIVIDED
-- among the permanents the criterion admits, the payer choosing the division as
-- they pay (CR 601.2h). Zameck Guildmage's group above is the one-permanent form.
--
-- THE BOARD: alice controls the Sages, an Island for its {1}, a Goblin Piker and
-- a Hill Giant, each of the three carrying the counter count given. bob's own
-- Hill Giant carries three counters on every board, so a pool read without "you
-- control" would find the counters anyway. alice's library holds two cards.
novijenSagesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
novijenSagesSpec s registry =
  Spec.describe s "Novijen Sages" $ do
    -- The gameplay-level assertions come FIRST: where the two counters came off,
    -- then the draw. The prompt record is a proxy after them.
    Spec.it s "CR 601.2h the payer divides the two counters among creatures, one off each of two" $ do
      (sagesId, pikerId, giantId, board) <- sagesBoard s registry [3, 1, 2]
      sages <- S.printingOf s registry "Novijen Sages"
      let answer = Map.fromList [(pikerId, 1), (giantId, 1)]
          ((_, paid), asked) = State.runState (Engine.runGame (recordingSpreadRemovals answer) board (Activate.activateAbility S.alice sagesId (theAbility sages))) []
          after = S.runPure S.identityAnswer paid Stack.resolveTop
      Spec.assertEqWith s "CR 122.1 one counter came off the Piker" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 0
      Spec.assertEqWith s "and one off the Giant, which kept its other" (S.counterOf CounterKind.PlusOnePlusOne giantId after) 1
      Spec.assertEqWith s "and the Sages, given none of the two, kept all three" (S.counterOf CounterKind.PlusOnePlusOne sagesId after) 3
      Spec.assertEqWith s "CR 121.1 and the ability that cost paid for drew a card" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
      Spec.assertEqWith s "and the payer was asked once, over every counter-bearing creature they control" asked [(2, Map.fromList [(sagesId, 3), (pikerId, 1), (giantId, 2)])]
    -- CR 118.3 / 602.2b over the pair of boards differing in one counter: two
    -- creatures with ONE counter each pay a removal of two spread among them, where
    -- the one-permanent form would refuse it; one counter in all does not.
    Spec.it s "CR 118.3 the count is read against the counters the creatures carry between them" $ do
      sages <- S.printingOf s registry "Novijen Sages"
      (sagesId, _, _, spread) <- sagesBoard s registry [1, 1, 0]
      (shortId, _, _, short) <- sagesBoard s registry [1, 0, 0]
      let offers oid gs = length (filter (isActivateOfAbility oid (theAbility sages)) (Action.legalActions S.alice gs))
      Spec.assertEqWith s "CR 602.2b offered with one counter on each of two creatures" (offers sagesId spread) 1
      Spec.assertEqWith s "and not offered with one counter in all" (offers shortId short) 0

-- The board novijenSagesSpec's cases share, described above it. The counts are
-- the Sages', the Piker's and the Giant's, in that order.
sagesBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [Natural.Natural] -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
sagesBoard s registry counts = do
  sages <- S.printingOf s registry "Novijen Sages"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  island <- S.printingOf s registry "Island"
  let lands = S.landsFor island S.alice 1 (Setup.emptyGame S.bothPlayers)
      (sagesId, withSages) = S.addPermanent sages S.alice lands
      (pikerId, withPiker) = S.addPermanent piker S.alice withSages
      (giantId, withGiant) = S.addPermanent giant S.alice withPiker
      (bobsGiantId, withBobs) = S.addPermanent giant S.bob withGiant
      (_, stocked) = S.addLibraryCard island S.alice (snd (S.addLibraryCard island S.alice withBobs))
      opposed = S.addCounter CounterKind.PlusOnePlusOne 3 bobsGiantId stocked
      counted = case counts of
        [onSages, onPiker, onGiant] -> S.addCounter CounterKind.PlusOnePlusOne onGiant giantId (S.addCounter CounterKind.PlusOnePlusOne onPiker pikerId (S.addCounter CounterKind.PlusOnePlusOne onSages sagesId opposed))
        _ -> opposed
  pure
    ( sagesId,
      pikerId,
      giantId,
      counted
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Answers Prompt.ChooseCounterRemovalAmong with the division given, recording
-- each prompt's count and offer. PINNED rather than searching for a legal
-- division, so a payment that ignored the answer cannot come back green.
recordingSpreadRemovals :: Map.Map ObjectId.ObjectId Natural.Natural -> Prompt.Prompt r -> State.State [(Natural.Natural, Map.Map ObjectId.ObjectId Natural.Natural)] r
recordingSpreadRemovals answer p = case p of
  Prompt.ChooseCounterRemovalAmong _ _ _ total offered -> do
    State.modify' (<> [(total, offered)])
    pure answer
  _ -> pure (S.identityAnswer p)

-- Retribution of the Ancients {B} Enchantment (Oracle text checked against
-- Scryfall 2026-09-26): "{B}, Remove X +1/+1 counters from among creatures you
-- control: Target creature gets -X/-X until end of turn."
--
-- The producer for CostComponent.RemovePlusOneCountersX, Novijen Sages' removal
-- with CR 107.3a's X as its count, announced at CR 601.2b and divided at CR
-- 601.2h.
--
-- THE BOARD: alice controls the Retribution over a Swamp, a Goblin Piker with
-- one counter and a Hill Giant with two; bob's Hill Giant, the target, carries
-- four, which a pool read without "you control" would count.
retributionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
retributionSpec s registry =
  Spec.describe s "Retribution of the Ancients" $ do
    -- The gameplay-level assertions come FIRST: which creature paid, then the
    -- -X/-X the announcement bought. The prompt record is a proxy after them.
    Spec.it s "CR 107.3a/601.2h the announced X is divided among creatures and the target gets -X/-X" $ do
      (retributionId, pikerId, giantId, bobsGiantId, board) <- retributionBoard s registry
      retribution <- S.printingOf s registry "Retribution of the Ancients"
      let answer = Map.fromList [(pikerId, 1), (giantId, 1)]
          act = do Activate.activateAbility S.alice retributionId (theAbility retribution); Stack.resolveTop
          (after, asked) = State.runState (fmap snd (Engine.runGame (retributionAnswers 2 bobsGiantId answer) board act)) []
      Spec.assertEqWith s "CR 122.1 the Piker's one counter came off" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 0
      Spec.assertEqWith s "and one of the Giant's two, as the payer divided them" (S.counterOf CounterKind.PlusOnePlusOne giantId after) 1
      Spec.assertEqWith s "CR 613.4c bob's 3/3 Giant with four counters, at -2/-2, is 5/5" (Projection.toughnessOf bobsGiantId after) (Just 5)
      Spec.assertEqWith s "and the payer was asked to divide X = 2 over alice's two creatures" asked [(2, Map.fromList [(pikerId, 1), (giantId, 2)])]
    -- CR 601.2b via 602.2b: the X offered is bounded by the counters alice's
    -- creatures carry between them -- three, not bob's four and not seven.
    Spec.it s "CR 118.3 the ChooseX bound is the counters the creatures carry between them" $ do
      (retributionId, _, _, _, board) <- retributionBoard s registry
      retribution <- S.printingOf s registry "Retribution of the Ancients"
      let bounds = State.execState (Engine.runGame recordXBound board (Activate.activateAbility S.alice retributionId (theAbility retribution))) []
      Spec.assertEqWith s "X is bounded at three" bounds [3]

-- Chatterfang, Squirrel General (Oracle text checked against Scryfall
-- 2026-09-30): "{B}, Sacrifice X Squirrels: Target creature gets +X/-X until end
-- of turn." CostComponent.SacrificeX with a real choice: three Treetop Sentries
-- and Chatterfang itself are Squirrels, so X = 2 asks which two.
--
-- THE BOARD: alice controls Chatterfang, three Sentries and a Swamp; bob's Hill
-- Giant is the target.
chatterfangSacrificeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
chatterfangSacrificeSpec s registry =
  Spec.describe s "Chatterfang, Squirrel General" $ do
    -- The gameplay-level assertions come FIRST: which Squirrels died, then the
    -- +X/-X. The prompt record is a proxy after them.
    Spec.it s "CR 107.3a/701.21a the announced X Squirrels are sacrificed and the target gets +X/-X" $ do
      chatterfang <- S.printingOf s registry "Chatterfang, Squirrel General"
      sentries <- S.printingOf s registry "Treetop Sentries"
      giant <- S.printingOf s registry "Hill Giant"
      swamp <- S.printingOf s registry "Swamp"
      let lands = S.landsFor swamp S.alice 1 (Setup.emptyGame S.bothPlayers)
          (chatterfangId, g1) = S.addPermanent chatterfang S.alice lands
          (firstId, g2) = S.addPermanent sentries S.alice g1
          (secondId, g3) = S.addPermanent sentries S.alice g2
          (thirdId, g4) = S.addPermanent sentries S.alice g3
          (giantId, g5) = S.addPermanent giant S.bob g4
          board = g5 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
          sacrificed = Set.fromList [firstId, thirdId]
          answer :: Prompt.Prompt r -> State.State [(Natural.Natural, [ObjectId.ObjectId])] r
          answer p = case p of
            Prompt.ChooseX {} -> pure 2
            Prompt.ChooseTargets _ _ _ sets -> pure (fmap (Set.filter ((== Just giantId) . Recipient.objectOf) . snd) sets)
            Prompt.ChooseSacrifices _ _ _ offered n _ -> do
              State.modify' (<> [(n, offered)])
              pure sacrificed
            _ -> pure (S.identityAnswer p)
          act = do Activate.activateAbility S.alice chatterfangId (theAbility chatterfang); Stack.resolveTop
          (after, asked) = State.runState (fmap snd (Engine.runGame answer board act)) []
          onBattlefield = GameState.battlefield after
      Spec.assertEqWith s "CR 701.21a the two Sentries chosen left the battlefield" (Set.intersection sacrificed onBattlefield) Set.empty
      Spec.assertBool s (Set.member secondId onBattlefield && Set.member chatterfangId onBattlefield) "and the third Sentry and Chatterfang stayed"
      Spec.assertEqWith s "CR 613.4c bob's 3/3 Giant at +2/-2 is 5/1" (S.powerToughnessOf giantId after) (Just (5, 1))
      Spec.assertEqWith s "and the payer was asked for two of the four Squirrels" asked [(2, List.sort [chatterfangId, firstId, secondId, thirdId])]

-- The board retributionSpec's cases share, described above it. Answers the
-- Retribution, the Piker, alice's Giant and bob's Giant.
retributionBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
retributionBoard s registry = do
  retribution <- S.printingOf s registry "Retribution of the Ancients"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  swamp <- S.printingOf s registry "Swamp"
  let lands = S.landsFor swamp S.alice 1 (Setup.emptyGame S.bothPlayers)
      (retributionId, withRetribution) = S.addPermanent retribution S.alice lands
      (pikerId, withPiker) = S.addPermanent piker S.alice withRetribution
      (giantId, withGiant) = S.addPermanent giant S.alice withPiker
      (bobsGiantId, withBobs) = S.addPermanent giant S.bob withGiant
      counted =
        S.addCounter CounterKind.PlusOnePlusOne 4 bobsGiantId (S.addCounter CounterKind.PlusOnePlusOne 2 giantId (S.addCounter CounterKind.PlusOnePlusOne 1 pikerId withBobs))
  pure
    ( retributionId,
      pikerId,
      giantId,
      bobsGiantId,
      counted
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Announces X, aims the target at the creature named by FILTERING the offered
-- set, and answers Prompt.ChooseCounterRemovalAmong with the division given,
-- recording each such prompt's count and offer.
retributionAnswers :: Natural.Natural -> ObjectId.ObjectId -> Map.Map ObjectId.ObjectId Natural.Natural -> Prompt.Prompt r -> State.State [(Natural.Natural, Map.Map ObjectId.ObjectId Natural.Natural)] r
retributionAnswers x target answer p = case p of
  Prompt.ChooseX {} -> pure x
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (Set.filter ((== Just target) . Recipient.objectOf) . snd) sets)
  _ -> recordingSpreadRemovals answer p

-- Records the bound Prompt.ChooseX carries and announces 0, which refuses nothing
-- the record needs.
recordXBound :: Prompt.Prompt r -> State.State [Natural.Natural] r
recordXBound p = case p of
  Prompt.ChooseX _ _ _ _ bound -> do
    State.modify' (<> [bound])
    pure 0
  _ -> pure (S.identityAnswer p)

-- Ooze Flux {3}{G} Enchantment (Oracle text checked against Scryfall
-- 2026-09-27): "{1}{G}, Remove one or more +1/+1 counters from among creatures
-- you control: Create an X/X green Ooze creature token, where X is the number of
-- +1/+1 counters removed this way."
--
-- The producer for CounterSpread.FromAmongAtLeast and Binding.removedCounters:
-- the payer settles HOW MANY while paying (CR 601.2h), and the token reads the
-- number the payment bound.
--
-- THE BOARD: alice controls the Flux over two Forests, a Goblin Piker and a Hill
-- Giant carrying the counter counts given; bob's Hill Giant carries three on
-- every board. alice's library holds two cards.
oozeFluxSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
oozeFluxSpec s registry =
  Spec.describe s "Ooze Flux" $ do
    -- The gameplay-level assertions come FIRST: the token's size, then where the
    -- counters came off. The prompt record is a proxy after them.
    Spec.it s "CR 601.2h the payer chooses how many counters come off, and the token is that big" $ do
      (fluxId, pikerId, giantId, board) <- oozeFluxBoard s registry [1, 2]
      flux <- S.printingOf s registry "Ooze Flux"
      let answer = Map.singleton giantId 2
          act = do Activate.activateAbility S.alice fluxId (theAbility flux); Stack.resolveTop
          (after, asked) = State.runState (fmap snd (Engine.runGame (recordingAtLeastRemovals answer) board act)) []
          oozes = newPermanents board after
      Spec.assertEqWith s "CR 111.3 the Ooze is 2/2, the two counters removed" (fmap (`Projection.powerOf` after) oozes, fmap (`Projection.toughnessOf` after) oozes) ([Just 2], [Just 2])
      Spec.assertEqWith s "CR 122.1 both came off the Giant" (S.counterOf CounterKind.PlusOnePlusOne giantId after) 0
      Spec.assertEqWith s "and the Piker kept its one" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 1
      Spec.assertEqWith s "the payer was asked once, with a floor of one, over alice's two creatures" asked [(1, Map.fromList [(pikerId, 1), (giantId, 2)])]
    -- CR 118.3 / 602.2b: "one or more" is unpayable with none -- the pair differs
    -- in the Piker's one counter.
    Spec.it s "CR 118.3 with no counter on alice's creatures the ability is not offered" $ do
      flux <- S.printingOf s registry "Ooze Flux"
      (fluxId, _, _, one) <- oozeFluxBoard s registry [1, 0]
      (bareId, _, _, none) <- oozeFluxBoard s registry [0, 0]
      let offers oid gs = length (filter (isActivateOfAbility oid (theAbility flux)) (Action.legalActions S.alice gs))
      Spec.assertEqWith s "CR 602.2b offered with one counter" (offers fluxId one) 1
      Spec.assertEqWith s "and not with none" (offers bareId none) 0

-- The board oozeFluxSpec's cases share, described above it. The counts are the
-- Piker's and the Giant's.
oozeFluxBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [Natural.Natural] -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
oozeFluxBoard s registry counts = do
  flux <- S.printingOf s registry "Ooze Flux"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  forest <- S.printingOf s registry "Forest"
  let lands = S.landsFor forest S.alice 2 (Setup.emptyGame S.bothPlayers)
      (fluxId, withFlux) = S.addPermanent flux S.alice lands
      (pikerId, withPiker) = S.addPermanent piker S.alice withFlux
      (giantId, withGiant) = S.addPermanent giant S.alice withPiker
      (bobsGiantId, withBobs) = S.addPermanent giant S.bob withGiant
      (_, stocked) = S.addLibraryCard forest S.alice (snd (S.addLibraryCard forest S.alice withBobs))
      opposed = S.addCounter CounterKind.PlusOnePlusOne 3 bobsGiantId stocked
      counted = case counts of
        [onPiker, onGiant] -> S.addCounter CounterKind.PlusOnePlusOne onGiant giantId (S.addCounter CounterKind.PlusOnePlusOne onPiker pikerId opposed)
        _ -> opposed
  pure
    ( fluxId,
      pikerId,
      giantId,
      counted
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- The permanents alice controls after a run that she did not before it: the
-- tokens it made.
newPermanents :: GameState.GameState -> GameState.GameState -> [ObjectId.ObjectId]
newPermanents before after = filter (`notElem` Game.zoneMembers Zone.Battlefield S.alice before) (Game.zoneMembers Zone.Battlefield S.alice after)

-- Answers Prompt.ChooseCounterRemovalAtLeast with the division given, recording
-- each prompt's floor and offer. PINNED, recordingSpreadRemovals' posture.
recordingAtLeastRemovals :: Map.Map ObjectId.ObjectId Natural.Natural -> Prompt.Prompt r -> State.State [(Natural.Natural, Map.Map ObjectId.ObjectId Natural.Natural)] r
recordingAtLeastRemovals answer p = case p of
  Prompt.ChooseCounterRemovalAtLeast _ _ _ least offered -> do
    State.modify' (<> [(least, offered)])
    pure answer
  _ -> pure (S.identityAnswer p)

-- Tayam, Luminous Enigma {1}{W}{B}{G} Legendary Creature -- Nightmare Beast 3/3
-- (Oracle text checked against Scryfall 2026-09-27): "Each other creature you
-- control enters with an additional vigilance counter on it. / {3}, Remove three
-- counters from among creatures you control: Mill three cards, then return a
-- permanent card with mana value 3 or less from your graveyard to the
-- battlefield."
--
-- The producer for WhichCounters.OfAnyKind spread FromAmong: CR 122.1 makes
-- counters of different names different things, so the payer divides the three
-- by kind as well as by creature (CR 601.2h).
--
-- THE BOARD: alice controls Tayam over three Swamps, a Goblin Piker carrying the
-- vigilance and +1/+1 counts given and a Hill Giant carrying the +1/+1 count
-- given. bob's Hill Giant carries three +1/+1 counters and a vigilance counter on
-- every board, which a pool read without "you control" would count. alice's
-- graveyard holds a Goblin Piker (mana value 2) and a Hill Giant (mana value 4);
-- her library five Divinations, which mill as nonpermanent cards.
tayamSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tayamSpec s registry =
  Spec.describe s "Tayam, Luminous Enigma" $ do
    -- The gameplay-level assertions come FIRST: which counter of which kind came
    -- off which creature. The prompt record is a proxy after them.
    Spec.it s "CR 122.1 / 601.2h the payer divides the three counters by creature and by kind" $ do
      (tayamId, pikerId, giantId, board) <- tayamBoard s registry (1, 1, 2)
      tayam <- S.printingOf s registry "Tayam, Luminous Enigma"
      let answer = Map.fromList [(pikerId, Map.singleton vigilance 1), (giantId, Map.singleton CounterKind.PlusOnePlusOne 2)]
          act = do Activate.activateAbility S.alice tayamId (theAbility tayam); Stack.resolveTop
          (after, asked) = State.runState (fmap snd (Engine.runGame (recordingMixedRemovals answer) board act)) []
          returned = newPermanents board after
      Spec.assertEqWith s "CR 122.1 the vigilance counter came off the Piker" (S.counterOf vigilance pikerId after) 0
      Spec.assertEqWith s "and the Piker kept its +1/+1 counter, a different kind" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 1
      Spec.assertEqWith s "and both of the Giant's came off" (S.counterOf CounterKind.PlusOnePlusOne giantId after) 0
      -- What the cost paid for: the graveyard's one permanent card of mana value
      -- 3 or less, which Tayam's first ability then marks on its way in.
      Spec.assertEqWith s "CR 701.17a / 400.7 the graveyard's Goblin Piker returned" (fmap (`S.soleFaceName` after) returned) [pikerName]
      Spec.assertEqWith s "CR 614.1c and entered with an additional vigilance counter" (fmap (\oid -> S.counterOf vigilance oid after) returned) [1]
      Spec.assertEqWith s "the payer was asked once, over alice's creatures by kind" asked [(CounterSpread.FromAmong, 3, Map.fromList [(pikerId, Map.fromList [(vigilance, 1), (CounterKind.PlusOnePlusOne, 1)]), (giantId, Map.singleton CounterKind.PlusOnePlusOne 2)])]
    -- CR 118.3 / 602.2b over a pair of boards differing in the Piker's one
    -- vigilance counter: with it, three counters of two kinds pay the cost that a
    -- +1/+1-only reading would refuse at two.
    Spec.it s "CR 118.3 counters of every kind count toward the three" $ do
      tayam <- S.printingOf s registry "Tayam, Luminous Enigma"
      (tayamId, _, _, mixed) <- tayamBoard s registry (1, 0, 2)
      (shortId, _, _, short) <- tayamBoard s registry (0, 0, 2)
      let offers oid gs = length (filter (isActivateOfAbility oid (theAbility tayam)) (Action.legalActions S.alice gs))
      Spec.assertEqWith s "CR 602.2b offered with a vigilance counter and two +1/+1 counters" (offers tayamId mixed) 1
      Spec.assertEqWith s "and not offered with the two +1/+1 counters alone" (offers shortId short) 0
  where
    pikerName = CardName.MkCardName (Text.pack "Goblin Piker")

-- CR 122.1b's vigilance counter.
vigilance :: CounterKind.CounterKind Keyword.Keyword
vigilance = CounterKind.Keyword Keyword.Vigilance

-- The board tayamSpec's cases share, described above it. The counts are the
-- Piker's vigilance and +1/+1 counters and the Giant's +1/+1 counters.
tayamBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> (Natural.Natural, Natural.Natural, Natural.Natural) -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
tayamBoard s registry (pikerVigilance, pikerPlusOne, giantPlusOne) = do
  tayam <- S.printingOf s registry "Tayam, Luminous Enigma"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  swamp <- S.printingOf s registry "Swamp"
  divination <- S.printingOf s registry "Divination"
  let lands = S.landsFor swamp S.alice 3 (Setup.emptyGame S.bothPlayers)
      (tayamId, withTayam) = S.addPermanent tayam S.alice lands
      (pikerId, withPiker) = S.addPermanent piker S.alice withTayam
      (giantId, withGiant) = S.addPermanent giant S.alice withPiker
      (bobsGiantId, withBobs) = S.addPermanent giant S.bob withGiant
      buried = snd (S.addGraveyardCard giant S.alice (snd (S.addGraveyardCard piker S.alice withBobs)))
      stocked = List.foldl' (\g _ -> snd (S.addLibraryCard divination S.alice g)) buried [1 .. (5 :: Int)]
      opposed = S.addCounter vigilance 1 bobsGiantId (S.addCounter CounterKind.PlusOnePlusOne 3 bobsGiantId stocked)
      counted =
        S.addCounter CounterKind.PlusOnePlusOne giantPlusOne giantId
          . S.addCounter CounterKind.PlusOnePlusOne pikerPlusOne pikerId
          . S.addCounter vigilance pikerVigilance pikerId
          $ opposed
  pure
    ( tayamId,
      pikerId,
      giantId,
      counted
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Answers Prompt.ChooseMixedCounterRemoval with the division given, recording
-- each prompt's spread, count and offer. PINNED, recordingSpreadRemovals'
-- posture.
recordingMixedRemovals :: Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural) -> Prompt.Prompt r -> State.State [(CounterSpread.CounterSpread, Natural.Natural, Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural))] r
recordingMixedRemovals answer p = case p of
  Prompt.ChooseMixedCounterRemoval _ _ _ spread owed offered -> do
    State.modify' (<> [(spread, owed, offered)])
    pure answer
  _ -> pure (S.identityAnswer p)

-- Soul Diviner {U}{B} Creature -- Zombie Wizard 2/3 (Oracle text checked against
-- Scryfall 2026-09-27): "{T}, Remove a counter from an artifact, creature, land,
-- or planeswalker you control: Draw a card."
--
-- The producer for WhichCounters.OfAnyKind spread FromOne: ONE counter, so the
-- one-permanent division is the payer's pick of a permanent and a kind at once.
--
-- THE BOARD: alice controls the Diviner and a Hill Giant carrying a vigilance
-- counter and a +1/+1 counter; her library two Islands.
soulDivinerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
soulDivinerSpec s registry =
  Spec.describe s "Soul Diviner" $ do
    Spec.it s "CR 122.1 / 601.2h the payer picks the kind of the one counter" $ do
      diviner <- S.printingOf s registry "Soul Diviner"
      giant <- S.printingOf s registry "Hill Giant"
      island <- S.printingOf s registry "Island"
      let (divinerId, withDiviner) = S.addPermanent diviner S.alice (Setup.emptyGame S.bothPlayers)
          (giantId, withGiant) = S.addPermanent giant S.alice withDiviner
          stocked = snd (S.addLibraryCard island S.alice (snd (S.addLibraryCard island S.alice withGiant)))
          board =
            (S.addCounter vigilance 1 giantId (S.addCounter CounterKind.PlusOnePlusOne 1 giantId stocked))
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
          answer = Map.singleton giantId (Map.singleton vigilance 1)
          act = do Activate.activateAbility S.alice divinerId (theAbility diviner); Stack.resolveTop
          (after, asked) = State.runState (fmap snd (Engine.runGame (recordingMixedRemovals answer) board act)) []
      Spec.assertEqWith s "CR 122.1 the vigilance counter came off" (S.counterOf vigilance giantId after) 0
      Spec.assertEqWith s "and the +1/+1 counter stayed" (S.counterOf CounterKind.PlusOnePlusOne giantId after) 1
      Spec.assertEqWith s "CR 121.1 and the ability drew a card" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
      Spec.assertEqWith s "the payer was asked once, over the Giant's two kinds" asked [(CounterSpread.FromOne, 1, Map.singleton giantId (Map.fromList [(vigilance, 1), (CounterKind.PlusOnePlusOne, 1)]))]

-- Quillspike {2}{B/G} Creature -- Beast 1/1 (Oracle text checked against
-- Scryfall 2026-09-27): "{B/G}, Remove a -1/-1 counter from a creature you
-- control: This creature gets +3/+3 until end of turn."
--
-- The producer for WhichCounters.OfKind at a kind other than +1/+1.
--
-- THE BOARD: alice controls Quillspike over a Swamp, a Hill Giant carrying the
-- -1/-1 count given, and a Goblin Piker carrying a +1/+1 counter, a kind the cost
-- does not reach.
quillspikeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
quillspikeSpec s registry =
  Spec.describe s "Quillspike" $ do
    -- CR 118.3 / 602.2b over the pair differing in the Giant's -1/-1 counter: the
    -- Piker's +1/+1 counter does not pay.
    Spec.it s "CR 118.3 with no -1/-1 counter the ability is not offered" $ do
      quillspike <- S.printingOf s registry "Quillspike"
      (quillId, _, _, withCounter) <- quillspikeBoard s registry 1
      (bareId, _, _, without) <- quillspikeBoard s registry 0
      let offers oid gs = length (filter (isActivateOfAbility oid (theAbility quillspike)) (Action.legalActions S.alice gs))
      Spec.assertEqWith s "CR 602.2b offered with a -1/-1 counter on the Giant" (offers quillId withCounter) 1
      Spec.assertEqWith s "and not with only the Piker's +1/+1 counter" (offers bareId without) 0

-- The board quillspikeSpec's cases share, described above it.
quillspikeBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Natural.Natural -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
quillspikeBoard s registry onGiant = do
  quillspike <- S.printingOf s registry "Quillspike"
  giant <- S.printingOf s registry "Hill Giant"
  piker <- S.printingOf s registry "Goblin Piker"
  swamp <- S.printingOf s registry "Swamp"
  let lands = S.landsFor swamp S.alice 1 (Setup.emptyGame S.bothPlayers)
      (quillId, withQuill) = S.addPermanent quillspike S.alice lands
      (giantId, withGiant) = S.addPermanent giant S.alice withQuill
      (pikerId, withPiker) = S.addPermanent piker S.alice withGiant
      counted = S.addCounter CounterKind.MinusOneMinusOne onGiant giantId (S.addCounter CounterKind.PlusOnePlusOne 1 pikerId withPiker)
  pure
    ( quillId,
      giantId,
      pikerId,
      counted
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Millikin {2} Artifact Creature -- Construct 0/1, "{T}, Mill a card: Add {C}"
-- (Oracle text checked against Scryfall): the pool's producer of a cost that
-- MILLS, and so of CR 601.2h's second pass. Pawl.ManaSpec's Millikin group
-- carries CR 605.1a, which is the other rule its cost reaches.
--
-- Three things are proved here and nowhere else: CR 701.17b's refusal of a mill
-- bigger than the library, the two-pass split -- the payment asks the payer to
-- order NOTHING, where one undivided list would have asked -- and CR 701.17a's
-- record, which is what makes a cost-side mill a mill to anything watching.
millikinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
millikinSpec s registry =
  Spec.describe s "Millikin" $ do
    -- CR 701.17b's last sentence, which is about a COST rather than about an
    -- instruction. The two boards differ in the library and in nothing else, so
    -- the refusal is the library's doing: Millikin is untapped and settled on
    -- both, which is the whole of the other component's payability.
    --
    -- The OFFER is guarded twice over -- Cost.canPayComponent's arm and the
    -- Zone.Library claim Cost.claimOf states, either of which alone withholds it
    -- -- so the two component assertions below are what pin the arm itself.
    Spec.it s "CR 701.17b a mill cost is unpayable out of an empty library, so the ability is not offered" $ do
      millikin <- S.printingOf s registry "Millikin"
      let (stockedId, stocked) = millikinBoard millikin 1
          (emptyId, emptied) = millikinBoard millikin 0
          offers oid gs = length (filter (isActivateOf oid) (Action.legalActions S.alice gs))
      Spec.assertEqWith s "CR 602.2b offered with one card in the library" (offers stockedId stocked) 1
      Spec.assertEqWith s "and not offered with none" (offers emptyId emptied) 0
      Spec.assertBool s (Cost.canPayComponent Map.empty S.alice stockedId (CostComponent.MillCards 1) stocked) "the component alone is payable at one card"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice emptyId (CostComponent.MillCards 1) emptied)) "and unpayable at none"
      Spec.assertBool s (not (Cost.canPayComponent Map.empty S.alice stockedId (CostComponent.MillCards 2) stocked)) "and one card does not pay a two-card mill"

    -- CR 601.2h's two passes. Millikin's cost holds exactly one part of each --
    -- {T} moves no card out of a library, the mill moves one into a public
    -- graveyard (CR 400.2) -- so each pass has a single part and the payer is
    -- given no order to choose. The recorder's control is the second run: the
    -- same board and the same answerer, over two parts that share the FIRST
    -- pass, which is asked.
    Spec.it s "CR 601.2h the {T} and the mill are paid in different passes, so nothing is ordered" $ do
      millikin <- S.printingOf s registry "Millikin"
      let (millikinId, board) = millikinBoard millikin 3
          ability = theAbility millikin
          ((_, after), asked) = State.runState (Engine.runGame recordingOrders board (Activate.activateAbility S.alice millikinId ability)) []
          sharedPass = State.execState (Engine.runGame recordingOrders board (Cost.payComponents PaymentMoment.OutsideResolution Map.empty S.alice millikinId [CostComponent.TapThis, CostComponent.SacrificeThis])) []
      Spec.assertEqWith s "CR 601.2h the payer was asked to order nothing" (length asked) 0
      Spec.assertEqWith s "CR 701.17a and both parts were still paid: one card is in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
      Spec.assertEqWith s "CR 107.5 with Millikin tapped for the other part" (fmap Object.tapped (Game.lookupObject millikinId after)) (Just TapState.Tapped)
      Spec.assertBool
        s
        (Cost.orderObservable [CostComponent.TapThis, CostComponent.MillCards 1])
        "both parts are order-sensitive, so it is the CR 601.2h partition and not orderSensitive that leaves nothing to ask"
      Spec.assertEqWith s "the control: two parts of the ONE pass are ordered by the payer" (length sharedPass) 1

    -- CR 701.17a makes an action a mill wherever it happens, so the payment
    -- records one exactly as a resolution does. The Master, Transcendent's
    -- "target creature card in a graveyard that was milled this turn" is what
    -- reads that record (Filter.MilledThisTurn), and a payment that moved the
    -- card without recording would leave it invisible to that card while sitting
    -- in the same graveyard.
    --
    -- A PAIR differing in one thing: the cards the mill took answer, the ones it
    -- left in the library do not.
    Spec.it s "CR 701.17a a card milled to pay a cost was milled this turn" $ do
      millikin <- S.printingOf s registry "Millikin"
      let (millikinId, board) = millikinBoard millikin 3
          after = S.runPure S.identityAnswer board (Activate.activateAbility S.alice millikinId (theAbility millikin))
          context = Filter.contextFor Teams.none (Just S.alice) Nothing
          milledThisTurn oid = Filter.matches context (Projection.viewOfObject oid after) Filter.Type.MilledThisTurn
      Spec.assertEqWith s "the mill put one card in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
      Spec.assertBool s (all milledThisTurn (Game.zoneMembers Zone.Graveyard S.alice after)) "which answers CR 701.17a's look-back"
      Spec.assertBool s (not (any milledThisTurn (Game.zoneMembers Zone.Library S.alice after))) "where the two cards it left in the library do not"

-- Millikin on the battlefield, settled and untapped, with `cards` copies of it
-- in alice's library, and alice holding priority in her own precombat main
-- phase. Nothing else is on the board, so a refusal below is the library's.
millikinBoard :: Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
millikinBoard millikin cards =
  let (millikinId, g1) = S.addPermanent millikin S.alice (Setup.emptyGame S.bothPlayers)
      stocked = foldr (\p gs -> snd (S.addLibraryCard p S.alice gs)) g1 (replicate cards millikin)
   in ( millikinId,
        stocked
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Every CR 601.2h ordering prompt raised, in order, by the components it
-- offered. A pure answerer cannot tell one prompt from none at all, so this
-- threads State -- Pawl.ManaSpec's recordingManaSources' shape.
recordingOrders :: Prompt.Prompt r -> State.State [[CostComponent.CostComponent Keyword.Keyword]] r
recordingOrders p = case p of
  Prompt.OrderCostComponents _ _ _ components -> do
    State.modify' (<> [components])
    pure (Replay.defaultAnswer p)
  _ -> pure (S.identityAnswer p)

-- Brittle Effigy {1} Artifact: "{4}, {T}, Exile this artifact: Exile target
-- creature." The gate card for CostComponent's ExileThis -- CR 406.2 charged
-- against the permanent the cost is ON, off the BATTLEFIELD, where
-- ExileThisFromGraveyard names a graveyard and so cannot stand in for it.
--
-- alice controls the Effigy, settled and untapped, and exactly four untapped
-- Plains -- the minimum that pays {4}, so the mana window has nothing to decide
-- and a refusal below cannot be an unaffordable one. bob controls a Hill Giant
-- and a Goblin Piker: two creatures, so the target (CR 601.2c, reached for an
-- activated ability by CR 602.2b) is a real choice, and
-- two DIFFERENT printings so which one the ability reached is visible by name.
--
-- bob owns both creatures and alice owns the Effigy, which is what keeps the two
-- exiles apart: Game.zoneMembers indexes exile by OWNER (CR 108.3), so the cost's
-- exile and the effect's land in different reads.
brittleEffigyBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
brittleEffigyBoard effigy plains giant piker =
  let (effigyId, g1) = S.addPermanent effigy S.alice (Setup.emptyGame S.bothPlayers)
      (giantId, g2) = S.addPermanent giant S.bob g1
      (pikerId, g3) = S.addPermanent piker S.bob g2
      withLands = S.landsFor plains S.alice 4 g3
   in ( effigyId,
        giantId,
        pikerId,
        withLands
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

brittleEffigySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
brittleEffigySpec s registry = Spec.describe s "Brittle Effigy" $ do
  -- CR 113.6m from the side that matters for a cost whose zone answer is
  -- Nothing: the ability moves the object off the BATTLEFIELD, where CR 113.6's
  -- default already had it, so the activation is offered there. Answering the
  -- rule with the sibling's Just Zone.Graveyard would withhold it -- that is the
  -- shape Pawl.SpeedSpec's Loxodon Surveyor case shows from the other side.
  Spec.it s "CR 113.6m the ability is offered from the battlefield" $ do
    effigy <- S.printingOf s registry "Brittle Effigy"
    plains <- S.printingOf s registry "Plains"
    giant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    let (effigyId, _, _, gs) = brittleEffigyBoard effigy plains giant piker
    Spec.assertBool s (any (isActivateOf effigyId) (Action.legalActions S.alice gs)) "the activation is a legal action"
    Spec.assertEqWith s "and the Effigy offers exactly one ability" (length (Activatable.abilitiesFor effigyId gs)) 1

-- Hanweir Battlements: a land printing "{T}: Add {C}" beside "{R}, {T}: Target
-- creature gains haste until end of turn". One permanent carrying both a mana
-- ability and a cost that needs its own {T}, which is what puts CR 605.3a's mana
-- window and CR 107.5 on the same object.
--
-- alice controls the Battlements and exactly one untapped Mountain -- the minimum
-- that pays the {R}, and the same one in both cases below, so neither refusal can
-- be an unaffordable cost. bob's Goblin Piker is the target: an opponent's
-- creature, so the grant is visible on a permanent the cost never touches.
hanweirBattlementsBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
hanweirBattlementsBoard battlements mountain piker =
  let (battlementsId, g1) = S.addPermanent battlements S.alice (Setup.emptyGame S.bothPlayers)
      (mountainId, g2) = S.addPermanent mountain S.alice g1
      (pikerId, g3) = S.addPermanent piker S.bob g2
   in ( battlementsId,
        mountainId,
        pikerId,
        g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Hanweir Battlements' SECOND printed activated ability, the "{R}, {T}" grant --
-- `theAbility` above takes the first, which is the mana ability.
hasteAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
hasteAbility p = case Face.activatedAbilities (S.combinedFace p) of
  _ : haste : _ -> Just haste
  _ -> Nothing

-- Trumpeting Carnosaur "{2}{R}, Discard this card: It deals 3 damage to target
-- creature or planeswalker", activated from alice's hand beside her Cadaverous
-- Bloom ("Exile a card from your hand: Add {B}{B} or {G}{G}") and one Mountain.
-- CR 605.3a's window offers the Bloom, and the Bloom's own cost offers every card
-- in that hand -- the Carnosaur included, since it is still there (CR 602.2a
-- moves no card). Exiling it for {B}{B} leaves CR 601.2h nothing to discard, so
-- the order is unpayable and CR 733.1 reverses the activation.
--
-- A PAIR differing in one thing, the card the Bloom exiles: a Mountain card
-- also in hand is the other answer, which pays in full. bob's Goblin Piker is
-- the target, 3 damage being lethal to it.
carnosaurBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
carnosaurBoard carnosaur bloom mountain piker =
  let (bloomId, g1) = S.addPermanent bloom S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addPermanent mountain S.alice g1
      (carnosaurId, g3) = S.addHandCard carnosaur S.alice g2
      (fuelId, g4) = S.addHandCard mountain S.alice g3
      (pikerId, g5) = S.addPermanent piker S.bob g4
   in (carnosaurId, bloomId, fuelId, pikerId, g5 {GameState.priority = Just S.alice})

-- The Bloom first, then anything else; the Bloom exiles `fuel`; every target
-- slot narrowed to `victim` by filtering the offer. The State is whether the
-- Bloom has been named yet, so the second source is the Mountain.
bloomExiling :: ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State Bool r
bloomExiling bloomId fuel victim p = case p of
  Prompt.ChooseManaSource _ _ candidates -> do
    used <- State.get
    let offered = NonEmpty.toList candidates
    if not used && elem bloomId offered
      then do
        State.put True
        pure (Just bloomId)
      else pure (Just (Maybe.fromMaybe (NonEmpty.head candidates) (List.find (/= bloomId) offered)))
  Prompt.ChooseCardInHand _ _ _ candidates -> pure (Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== fuel) (NonEmpty.toList candidates)))
  _ -> pure (targeting victim p)

trumpetingCarnosaurSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trumpetingCarnosaurSpec s registry = Spec.describe s "Trumpeting Carnosaur" $ do
  Spec.it s "CR 601.2h exiling the Carnosaur for mana leaves nothing to discard, and the activation reverses" $ do
    (carnosaurId, _, pikerId, after) <- run True
    Spec.assertEqWith s "the Piker took no damage" (fmap Object.damage (Game.lookupObject pikerId after)) (Just 0)
    Spec.assertEqWith s "CR 733.1 and the Carnosaur is back in alice's hand" (fmap Object.zone (Game.lookupObject carnosaurId after)) (Just Zone.Hand)
    Spec.assertEqWith s "with nothing on the stack" (length (GameState.stack after)) 0
  Spec.it s "CR 605.3a exiling another card pays the same cost and resolves" $ do
    (carnosaurId, fuelId, pikerId, after) <- run False
    Spec.assertEqWith s "CR 120.3e the Piker took the 3 damage" (fmap Object.damage (Game.lookupObject pikerId after)) (Just 3)
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject carnosaurId after)) "and the Carnosaur left the hand as a discard (CR 400.7)"
    Spec.assertEqWith s "landing in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertBool s (Maybe.isNothing (Game.lookupObject fuelId after)) "while the Mountain card paid the Bloom"
  where
    run exileItself = do
      carnosaur <- S.printingOf s registry "Trumpeting Carnosaur"
      bloom <- S.printingOf s registry "Cadaverous Bloom"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (carnosaurId, bloomId, fuelId, pikerId, gs) = carnosaurBoard carnosaur bloom mountain piker
          fuel = if exileItself then carnosaurId else fuelId
          (_, after) = State.evalState (Engine.runGame (bloomExiling bloomId fuel pikerId) gs (Activate.activateAbility S.alice carnosaurId (theAbility carnosaur) >> Stack.resolveTop)) False
      pure (carnosaurId, fuelId, pikerId, after)

-- Ashnod's Altar "Sacrifice a creature: Add {C}{C}": a mana ability with no {T}
-- in its cost, so CR 605.3a's window offers it while ANOTHER permanent's cost is
-- being paid, and the creature it eats can be that permanent. That is CR 118.3
-- on the cost parts that name the permanent the cost is on, on boards where
-- TapThis' guard cannot refuse first -- none of the costs below taps.
--
-- One producer per part, each alice's ONLY creature so the Altar has exactly one
-- thing to eat: Auriok Replica "{W}, Sacrifice this creature" is SacrificeThis,
-- Hanged Executioner "{3}{W}, Exile this creature: Exile target creature" is
-- ExileThis, Grinning Ignus "{R}, Return this creature to its owner's hand: Add
-- {C}{C}{R}" is ReturnThis -- that one a mana ability, so it is CR 602.2b's
-- nested window rather than an announcement's -- and Safehold Sentry "{2}{W},
-- {Q}" is UntapThis, whose permanent the Altar can eat just as well.
--
-- Each is a PAIR differing in one thing, the source the payer names in the
-- window, so no refusal can be an unaffordable cost or a missing activation.
--
-- alice's own precombat main phase with priority and an empty stack, which is
-- what CR 602.5d's "activate only as a sorcery" needs for the Ignus; bob's
-- Goblin Piker is across the table as the Executioner's target.
altarBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
altarBoard subject altar land piker n =
  let (subjectId, g1) = S.addPermanent subject S.alice (Setup.emptyGame S.bothPlayers)
      (altarId, g2) = S.addPermanent altar S.alice g1
      (pikerId, g3) = S.addPermanent piker S.bob g2
      withLands = S.landsFor land S.alice n g3
   in ( subjectId,
        altarId,
        pikerId,
        withLands
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- CR 605.3a's window answered by IDENTITY: the Altar whenever the engine offers
-- it, and otherwise the head of what is left, which is how the rest of the mana
-- still gets paid after the Altar has eaten the permanent. The Altar is offered
-- once at most -- it has no second creature to sacrifice -- so the fallback
-- cannot name it again.
feeding :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
feeding altarId victim p = case p of
  Prompt.ChooseManaSource _ _ candidates ->
    Just (Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== altarId) (NonEmpty.toList candidates)))
  _ -> targeting victim p

-- The other half of every pair: the same board and the same target, naming
-- anything BUT the Altar, and Nothing once the Altar is all that is offered.
sparing :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
sparing altarId victim p = case p of
  Prompt.ChooseManaSource _ _ candidates -> List.find (/= altarId) (NonEmpty.toList candidates)
  _ -> targeting victim p

ashnodsAltarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ashnodsAltarSpec s registry = Spec.describe s "Ashnod's Altar" $ do
  -- CR 118.3 on ReturnThis, reached through CR 602.2b rather than through an
  -- announcement: the Ignus' ability is itself a mana ability, so paying its {R}
  -- opens the window CR 605.3a describes and the Altar is in it. The whole
  -- activation reverses, so the Altar's own sacrifice goes back with it.
  Spec.it s "CR 118.3 the Altar eats the Ignus before its own return is paid" $ do
    ignus <- S.printingOf s registry "Grinning Ignus"
    altar <- S.printingOf s registry "Ashnod's Altar"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (ignusId, altarId, pikerId, gs) = altarBoard ignus altar mountain piker 1
        after = S.runPure (feeding altarId pikerId) gs (S.tapForMana ignusId)
    Spec.assertBool s (S.onBattlefield ignusId after) "the Ignus is back on the battlefield"
    Spec.assertEqWith s "with nothing of alice's in hand" (length (Game.zoneMembers Zone.Hand S.alice after)) 0
    Spec.assertEqWith s "nothing in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    Spec.assertEqWith s "CR 601.2h and no mana floating, the Altar's activation reversed with the rest" (Game.poolOf S.alice after) (Mana.Type.MkMana [])
  Spec.it s "CR 605.3a paying from the Mountain instead returns the Ignus and adds its mana" $ do
    ignus <- S.printingOf s registry "Grinning Ignus"
    altar <- S.printingOf s registry "Ashnod's Altar"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (ignusId, altarId, pikerId, gs) = altarBoard ignus altar mountain piker 1
        after = S.runPure (sparing altarId pikerId) gs (S.tapForMana ignusId)
    Spec.assertBool s (not (S.onBattlefield ignusId after)) "CR 400.3 the Ignus returned to its owner's hand"
    Spec.assertEqWith s "which is alice's" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
    Spec.assertEqWith s "and the ability added its three mana" (poolSize S.alice after) 3

-- How many mana units a player has floating. Duplicated per this suite's
-- convention of group-local helpers (ManaSpec carries its own).
poolSize :: PlayerId.PlayerId -> GameState.GameState -> Int
poolSize pid gs = case Game.poolOf pid gs of
  Mana.Type.MkMana units -> length units

-- CR 605.3a's window answered with `wanted` in order: each is tapped when the
-- engine offers it, and the window closes once none of them is left on offer.
-- CR 733.1's question is answered with `decision` and counted. Everything else
-- goes to `rest`.
keepingOrNot :: OptionalDecision.OptionalDecision -> [ObjectId.ObjectId] -> (forall a. Prompt.Prompt a -> a) -> Prompt.Prompt r -> State.State Int r
keepingOrNot decision wanted rest p = case p of
  Prompt.ChooseManaSource _ _ candidates -> pure (List.find (`elem` NonEmpty.toList candidates) wanted)
  Prompt.ReverseManaAbilities _ player _ | player == S.alice -> do
    State.modify' (+ 1)
    pure decision
  _ -> pure (rest p)

-- Mystic Gate's "{W/U}, {T}" route, with its {W/U} announced blue. The yield is
-- picked out of the offered options, never built.
gateRoute :: Prompt.Prompt r -> r
gateRoute p = case p of
  Prompt.ChooseManaYield _ _ _ candidates -> Maybe.fromMaybe (NonEmpty.head candidates) (List.find (maybe False (not . null . ManaCost.unwrap) . Cost.Type.mana . ManaOption.cost) (NonEmpty.toList candidates))
  Prompt.AnnounceHybridHalf _ _ _ _ offers -> Maybe.fromMaybe (NonEmpty.head offers) (List.find (== ManaType.Colored Color.Blue) (NonEmpty.toList offers))
  _ -> S.identityAnswer p

-- CR 733.1 where the payment is NOT the whole of the action: a cast has put its
-- spell on the stack (CR 601.2a) and an activation its ability (CR 602.2a)
-- before the window opens, and a mana ability's own cost opens a window nested
-- inside another activation. The announcement goes back unasked; the mana
-- abilities are still the payer's to keep.
--
-- Every case is a PAIR on one board differing only in the answer to
-- Prompt.ReverseManaAbilities.
announcedReversalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
announcedReversalSpec s registry = Spec.describe s "Reversal after an announcement" $ do
  -- Hanweir Battlements' "{R}, {T}", paid by tapping the Battlements itself
  -- and then the Mountain, so CR 107.5 refuses the {T}.
  Spec.it s "CR 733.1 an activator who keeps the mana abilities keeps both lands tapped and both mana floating" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    case hasteAbility battlements of
      Nothing -> Spec.assertFailure s "Hanweir Battlements should print two activated abilities"
      Just haste -> do
        let (battlementsId, mountainId, pikerId, gs) = hanweirBattlementsBoard battlements mountain piker
            run decision = State.runState (Engine.runGame (keepingOrNot decision [battlementsId, mountainId] (targeting pikerId)) gs (Activate.activateAbility S.alice battlementsId haste)) 0
            ((_, kept), asked) = run OptionalDecision.Declines
            ((_, reversed), _) = run OptionalDecision.Exercises
        Spec.assertEqWith s "CR 106.4 the {R} and the {C} are still in alice's pool" (poolSize S.alice kept) 2
        Spec.assertBool s (isTapped battlementsId kept && isTapped mountainId kept) "CR 107.5 and both lands stay tapped"
        Spec.assertEqWith s "CR 602.2a's ability object is gone from the stack" (GameState.stack kept) []
        Spec.assertEqWith s "and CR 602.5b's record was never written" (GameState.activatedThisTurn kept) (GameState.activatedThisTurn gs)
        Spec.assertEqWith s "the payer who reverses gets nothing floating" (poolSize S.alice reversed) 0
        Spec.assertBool s (not (isTapped battlementsId reversed || isTapped mountainId reversed)) "and both lands untapped"
        Spec.assertEqWith s "alice was asked once" asked 1

  -- Mystic Gate's "{W/U}, {T}" activated from nothing: its own window taps the
  -- Gate for its "{T}: Add {C}" and then the Island for {U}, so the {W/U} is
  -- paid and the Gate's {T} is not (CR 107.5).
  Spec.it s "CR 733.1 a nested window's mana abilities are the payer's to keep" $ do
    gate <- S.printingOf s registry "Mystic Gate"
    island <- S.printingOf s registry "Island"
    let (gateId, g1) = S.addPermanent gate S.alice (Setup.emptyGame S.bothPlayers)
        (islandId, gs) = S.addPermanent island S.alice g1
        run decision = State.runState (Engine.runGame (keepingOrNot decision [gateId, islandId] gateRoute) gs (S.tapForMana gateId)) 0
        ((paid, kept), asked) = run OptionalDecision.Declines
        ((_, reversed), _) = run OptionalDecision.Exercises
    Spec.assertEqWith s "CR 106.4 the Island's {U} and the Gate's {C} are floating" (poolSize S.alice kept) 2
    Spec.assertBool s (isTapped islandId kept && isTapped gateId kept) "CR 107.5 and both stay tapped"
    Spec.assertEqWith s "the payer who reverses gets nothing floating" (poolSize S.alice reversed) 0
    Spec.assertBool s (not (isTapped islandId reversed || isTapped gateId reversed)) "and both untapped"
    Spec.assertBool s (not paid) "CR 601.2h the {W/U} activation itself was refused"
    Spec.assertEqWith s "alice was asked once" asked 1

-- `n` copies of one printing onto alice's battlefield, ids in creation order.
addPermanents :: Printing.Printing -> Int -> GameState.GameState -> ([ObjectId.ObjectId], GameState.GameState)
addPermanents printing n gs =
  List.foldl'
    (\(ids, acc) _ -> let (oid, next) = S.addPermanent printing S.alice acc in (ids <> [oid], next))
    ([], gs)
    (replicate n ())

-- alice controls `reds` Goblin Pikers and `greens` Giant Spiders, holds `card`,
-- and has priority in her own precombat main phase so a creature spell is
-- castable (CR 302.1). NO LAND ON ANY BOARD, omniscienceBoard's posture: a cast
-- that succeeds can only have been paid by tapping creatures.
convokeBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
convokeBoard piker spider card reds greens =
  let (redIds, gs1) = addPermanents piker reds (Setup.emptyGame S.bothPlayers)
      (_, gs2) = addPermanents spider greens gs1
      (spell, gs3) = S.addHandCard card S.alice gs2
   in ( spell,
        redIds,
        gs3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Answer Prompt.ChooseCost with the tap substitution whose residual mana part is
-- `wanted` (CR 702.51b), and Prompt.ChooseTaps with the named permanents. BOTH
-- FILTERED against the offer, `tapping`'s posture above and for its reason: an
-- answer the engine did not offer is rejected rather than repaired, so filtering
-- is what keeps the assertion about the engine's own candidates. A `wanted` no
-- entry matches leaves Cost.firstOffered's unpayable cost, which fails the
-- payment visibly rather than picking some other entry.
convoking :: ManaCost.ManaCost -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
convoking wanted tapped p = case p of
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered (filter ((== Just wanted) . Cost.Type.mana) candidates)
  Prompt.ChooseTaps _ _ _ candidates _ -> Set.fromList (filter (`elem` tapped) candidates)
  _ -> S.identityAnswer p

-- Siege Wurm {5}{G}{G} Creature -- Wurm 5/5: "Convoke. Trample."
--
-- CR 702.51a's two clauses on one card, and the cheapest printing that states
-- convoke and nothing else. The GENERIC clause is paid by Goblin Pikers, which
-- are red, and the COLORED clause by Giant Spiders, which are green -- so the
-- colour half is what the pair below varies and nothing else is.
siegeWurmSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
siegeWurmSpec s registry = Spec.describe s "Siege Wurm" $ do
  -- The pair, varying the COLOUR of two creatures and nothing else: same seats,
  -- same count of creatures, same absence of mana. CR 702.51a's colored clause
  -- names "an untapped creature of that color", so seven red creatures pay the
  -- {5} and neither {G}.
  Spec.it s "CR 702.51a a colored mana wants a creature of that color" $ do
    wurm <- S.printingOf s registry "Siege Wurm"
    piker <- S.printingOf s registry "Goblin Piker"
    spider <- S.printingOf s registry "Giant Spider"
    let (withGreens, _, greenBoard) = convokeBoard piker spider wurm 5 2
        (allRed, _, redBoard) = convokeBoard piker spider wurm 7 0
    Spec.assertBool s (S.castable S.alice withGreens greenBoard) "five Pikers and two Spiders pay {5}{G}{G}"
    Spec.assertBool s (not (S.castable S.alice allRed redBoard)) "seven Pikers and no green creature do not"

-- CR 702.51a / 613.1f: Chief Engineer's "artifact spells you cast have convoke"
-- is a keyword the spell HAS, so Venser's Sliver, a {5} artifact creature with
-- no convoke of its own, is paid by tapping creatures as a Siege Wurm is -- at
-- the gate and at the payment alike (Cost.spellKeywords). No land on either
-- board, convokeBoard's posture: five creatures either way, and the one thing
-- the boards differ in is whether the fifth is Chief Engineer or a Goblin Piker.
chiefEngineerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
chiefEngineerSpec s registry = Spec.describe s "Chief Engineer" $ do
  Spec.it s "CR 702.51a a granted convoke pays a {5} artifact spell by tapping five creatures" $ do
    sliver <- S.printingOf s registry "Venser's Sliver"
    piker <- S.printingOf s registry "Goblin Piker"
    spider <- S.printingOf s registry "Giant Spider"
    chief <- S.printingOf s registry "Chief Engineer"
    let (spell, pikerIds, board) = convokeBoard piker spider sliver 4 0
        (chiefId, gs) = S.addPermanent chief S.alice board
        answer :: Prompt.Prompt r -> r
        answer = convoking (ManaCost.MkManaCost []) (chiefId : pikerIds)
        cast = S.runPure answer gs (S.cast S.alice spell)
        resolved = S.runPure answer cast Stack.resolveTop
    Spec.assertEqWith s "Venser's Sliver resolved onto the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Venser's Sliver")) S.alice resolved) 1
    Spec.assertEqWith s "and every one of the five creatures is tapped" (S.tappedCount S.alice resolved) 5
  Spec.it s "CR 702.51a with no grant five creatures cannot pay a {5} artifact spell" $ do
    sliver <- S.printingOf s registry "Venser's Sliver"
    piker <- S.printingOf s registry "Goblin Piker"
    spider <- S.printingOf s registry "Giant Spider"
    let (spell, _, gs) = convokeBoard piker spider sliver 5 0
    Spec.assertBool s (not (S.castable S.alice spell gs)) "five Pikers alone do not cast it"

-- CR 601.2g and convoke's reminder text ("Each creature you tap while casting
-- this spell pays for {1} or one mana of that creature's color"): the mana
-- window comes first, and the payer says how much of the cost is convoked once
-- it has closed.
--
-- The BOARD the issue named: a Siege Wurm ({5}{G}{G}), five Goblin Pikers, one
-- Giant Spider and one Birds of Paradise, and no land -- so the one mana ability
-- on the board is a colour choice, which is exactly the thing the payer must be
-- able to make before committing.
--
-- A test-local answerer in State.State, and not a pure one: the subject is the
-- prompt protocol's ORDER, which a `Prompt r -> r` cannot see (Pawl.ManaSpec's
-- countingAnswer is the pattern).
convokeWindowSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
convokeWindowSpec s registry = Spec.describe s "Siege Wurm" $ do
  Spec.it s "CR 601.2g the mana window opens before the payer says how much of a Siege Wurm's cost is convoked" $ do
    wurm <- S.printingOf s registry "Siege Wurm"
    piker <- S.printingOf s registry "Goblin Piker"
    spider <- S.printingOf s registry "Giant Spider"
    birds <- S.printingOf s registry "Birds of Paradise"
    let (pikerIds, gs1) = addPermanents piker 5 (Setup.emptyGame S.bothPlayers)
        (spiderId, gs2) = S.addPermanent spider S.alice gs1
        (birdsId, gs3) = S.addPermanent birds S.alice gs2
        (spell, gs4) = S.addHandCard wurm S.alice gs3
        gs =
          gs4
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        green = Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Green, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing, ManaUnit.spendTrigger = Nothing, ManaUnit.sourceChosenSubtype = Nothing, ManaUnit.sourceLastExiled = Nothing}]
        -- One {G} left to pay with mana, which is what the Birds produced: the
        -- other six symbols are convoked.
        residual = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
        note tag = State.modify' (<> [Text.pack tag])
        answer :: Prompt.Prompt r -> State.State [Text.Text] r
        answer p = case p of
          Prompt.ChooseManaSource _ _ candidates -> do
            note "window"
            pure (if elem birdsId (NonEmpty.toList candidates) then Just birdsId else Nothing)
          Prompt.ChooseManaYield _ _ _ candidates -> pure (S.optionYielding green candidates)
          Prompt.ChooseCost _ _ _ candidates -> do
            note "substitution"
            pure (Cost.firstOffered (filter ((== Just residual) . Cost.Type.mana) candidates))
          Prompt.ChooseTaps _ _ _ candidates n -> pure (Set.fromList (filter (`elem` (if n == 1 then [spiderId] else pikerIds)) candidates))
          _ -> pure (S.identityAnswer p)
        ((_, after), asked) = State.runState (Engine.runGame answer gs (S.cast S.alice spell)) []
    -- THE ORDER. Both decisions are made -- the list holds both tags -- and the
    -- window's is made first, which is what CR 601.2g asks for.
    Spec.assertEqWith s "CR 601.2g the window was offered before the substitution was announced" asked [Text.pack "window", Text.pack "substitution"]
    -- What the order alone does not say: the cast really went through on that
    -- answer, so the tags describe a payment rather than an abandoned one.
    Spec.assertBool s (isTapped birdsId after) "the Birds of Paradise was tapped for its mana"
    -- And the Birds is NOT among the convokers: the window tapped it, so the
    -- offer made afterwards no longer counts it as an untapped creature.
    Spec.assertEqWith s "every other creature was tapped to convoke the rest" (S.tappedCount S.alice after) 7

-- Venerated Loxodon {4}{W} Creature -- Elephant Cleric 4/4: "Convoke. When this
-- creature enters, put a +1/+1 counter on each creature that convoked it."
--
-- CR 702.51c's relation and the only printing that READS it, which is what makes
-- the record observable at gameplay level: nothing else in the pool asks which
-- creatures convoked a spell.
--
-- The board separates the two questions the record could be confused with. The
-- Giant Spider is an untapped creature alice controls that convoked nothing, so
-- an implementation counting "each creature you control" grows it; the Palace
-- Guard pays the {W} where the Pikers pay the {4}, so the two substitution
-- components have to land in the one relation rather than the last one winning.
veneratedLoxodonSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
veneratedLoxodonSpec s registry = Spec.describe s "Venerated Loxodon" $ do
  -- Inspiring Statuary {3} Artifact: "Nonartifact spells you cast have
  -- improvise." The Loxodon then has both keywords, and the payer says how much
  -- of its {4} each pays: two Goblin Pikers convoke {2}, and the Statuary and a
  -- Foundry Assembler -- an artifact CREATURE, which either keyword could tap --
  -- improvise the other {2}.
  Spec.it s "CR 702.51c an artifact creature tapped for improvise did not convoke the Loxodon" $ do
    loxodon <- S.printingOf s registry "Venerated Loxodon"
    piker <- S.printingOf s registry "Goblin Piker"
    palaceGuard <- S.printingOf s registry "Palace Guard"
    statuary <- S.printingOf s registry "Inspiring Statuary"
    assembler <- S.printingOf s registry "Foundry Assembler"
    let (pikerIds, gs1) = addPermanents piker 2 (Setup.emptyGame S.bothPlayers)
        (guardId, gs2) = S.addPermanent palaceGuard S.alice gs1
        (statuaryId, gs3) = S.addPermanent statuary S.alice gs2
        (assemblerId, gs4) = S.addPermanent assembler S.alice gs3
        (spell, gs5) = S.addHandCard loxodon S.alice gs4
        gs =
          gs5
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        -- The entry naming one colored tap and two generic taps per keyword;
        -- the improvise prompt is the one offering no Piker.
        tapCounts candidate = List.sort [TapPermanents.count t | CostComponent.TapPermanents t <- Cost.Type.components candidate]
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered (filter (\c -> Cost.Type.mana c == Just (ManaCost.MkManaCost []) && tapCounts c == [1, 2, 2]) candidates)
          Prompt.ChooseTaps _ _ _ candidates n
            | n == 1 -> Set.fromList (filter (== guardId) candidates)
            | any (`elem` pikerIds) candidates -> Set.fromList (filter (`elem` pikerIds) candidates)
            | otherwise -> Set.fromList (filter (`elem` [statuaryId, assemblerId]) candidates)
          _ -> S.identityAnswer p
        cast = S.runPure answer gs (S.cast S.alice spell)
        entered = S.runPure answer cast (Stack.resolveTop >> Engine.settleForPriority)
        grown = S.runPure answer entered Stack.resolveTop
        counters oid g = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid g)
    Spec.assertEqWith s "CR 702.51c the Foundry Assembler tapped for improvise grew no counter" (counters assemblerId grown) (Just 0)
    Spec.assertEqWith s "the two Goblin Pikers tapped for convoke each grew one" (fmap (`counters` grown) pikerIds) (replicate 2 (Just 1))
    Spec.assertBool s (isTapped assemblerId grown && isTapped statuaryId grown) "the Assembler and the Statuary were tapped to pay for it"

-- alice controls `islands` Islands, her graveyard holds `fuel` Goblin Pikers,
-- her library holds five more, and she holds `card` with priority in her own
-- precombat main phase. ONE mana source at most, convokeBoard's posture: the
-- Island pays the one symbol rule 702.66a does not reach, so anything beyond it
-- can only have been paid by exiling cards. The library is stocked so that CR
-- 104.3c does not decide the game before a draw is read.
delveBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId.ObjectId, GameState.GameState)
delveBoard island piker card islands fuel =
  let (_, gs1) = addPermanents island islands (Setup.emptyGame S.bothPlayers)
      gs2 = List.foldl' (\acc _ -> snd (S.addGraveyardCard piker S.alice acc)) gs1 (replicate fuel ())
      gs3 = List.foldl' (\acc _ -> snd (S.addLibraryCard piker S.alice acc)) gs2 (replicate 5 ())
      (spell, gs4) = S.addHandCard card S.alice gs3
   in ( spell,
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Treasure Cruise {7}{U} Sorcery: "Delve. Draw three cards."
--
-- CR 702.66a's one clause, and the cheapest printing that states delve and
-- nothing else. The graveyard holds one card MORE than the cost can spend, so
-- Prompt.ChooseExilesFromGraveyard is a real choice rather than a forced one.
treasureCruiseSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
treasureCruiseSpec s registry = Spec.describe s "Treasure Cruise" $ do
  -- The pair, varying the ISLAND and nothing else: same graveyard, same hand,
  -- same seats. CR 702.66a names "each generic mana", so eight cards in the
  -- graveyard reach the {7} and never the {U}.
  Spec.it s "CR 702.66a delve reaches a generic mana and not a colored one" $ do
    cruise <- S.printingOf s registry "Treasure Cruise"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    let (withIsland, islandBoard) = delveBoard island piker cruise 1 8
        (landless, landlessBoard) = delveBoard island piker cruise 0 8
    Spec.assertBool s (S.castable S.alice withIsland islandBoard) "eight cards in the graveyard and an Island pay {7}{U}"
    Spec.assertBool s (not (S.castable S.alice landless landlessBoard)) "eight cards and no blue source do not"

-- CR 701.67a's payer answers the same two prompts CR 702.51b's does -- which
-- residual cost the substitution leaves, and then which permanents to tap -- so
-- this is `convoking` under the name of the rule that reaches it here.
waterbending :: ManaCost.ManaCost -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
waterbending = convoking

-- The payer who wants the ability MOST: the offer that leaves the least mana to
-- find, and whatever permanents the engine put in front of them.
--
-- What the negatives below are answered with, and the reason they are not
-- vacuous: a pinned answer naming a route the board cannot pay leaves the
-- activation unpaid whatever the offer was, so such a case would pass against an
-- engine that offered too much. This one fails only where NO offer pays.
waterbendingGreedily :: Prompt.Prompt r -> r
waterbendingGreedily p = case p of
  -- An unpayable mana part (CR 118.6) sorts last rather than first, which is
  -- what Ord would do with it.
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered (List.sortOn (maybe (1 :: Int, 0) ((,) 0 . manaOwed) . Cost.Type.mana) candidates)
  Prompt.ChooseTaps _ _ _ candidates wanted -> Set.fromList (take (Natural.Extra.toIntSaturating wanted) candidates)
  _ -> S.identityAnswer p

-- How much mana a residual cost still asks for, CR 107.4b counting a generic
-- symbol for its own amount and every other symbol for one.
manaOwed :: ManaCost.ManaCost -> Natural.Natural
manaOwed manaCost =
  let sizeOf symbol = case symbol of
        ManaSymbol.Generic n -> n
        _ -> 1
   in sum (fmap sizeOf (ManaCost.unwrap manaCost))

-- alice controls a Geyser Leaper, one permanent per printing in `others` and
-- `lands` Mountains, with two Mountains in her library so the ability's draw
-- neither decks her (CR 104.3c) nor runs out; bob controls one permanent per
-- printing in `theirs`. She has priority in her own precombat main phase, which
-- is when CR 117.1b lets her activate. Returns the Leaper, the `others` in order,
-- and that state.
leaperBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> Int -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
leaperBoard mountain leaper others theirs lands =
  let (leaperId, gs1) = S.addPermanent leaper S.alice (S.landsInPlay mountain lands)
      (otherIds, gs2) = List.foldl' (\(ids, gs) printing -> let (oid, next) = S.addPermanent printing S.alice gs in (ids <> [oid], next)) ([], gs1) others
      gs3 = List.foldl' (\gs printing -> snd (S.addPermanent printing S.bob gs)) gs2 theirs
      (_, gs4) = S.addLibraryCard mountain S.alice gs3
      (_, gs5) = S.addLibraryCard mountain S.alice gs4
   in ( leaperId,
        otherIds,
        gs5
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Geyser Leaper {4}{U} Creature -- Human Warrior Ally 4/3
-- (data/cards/geyser-leaper.json): "Flying / Waterbend {4}: Draw a card, then
-- discard a card."
--
-- CR 701.67a as the WHOLE of an activation cost, which is the position most of
-- rule 701.67's printings put it in; Water Whip's (data/scenarios/cost) is a
-- spell's. The draw and the discard are what a paid cost is read off: a card
-- in alice's graveyard cannot arrive any other way on these boards.
--
-- Every board is LANDLESS unless the case is about CR 701.67b, convokeBoard's
-- posture: an activation that succeeds can only have been paid by tapping.
--
-- The Leaper is itself an untapped creature alice controls, so each board that
-- pays offers more candidates than the cost can take -- a prompt offered exactly
-- as many candidates as it needs is never asked. The boards that do NOT pay are
-- short on purpose, and that is the one thing each varies from its pair: of
-- eligible candidates in the first pair, of mana in the second.
geyserLeaperSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
geyserLeaperSpec s registry = Spec.describe s "Geyser Leaper" $ do
  -- The pair, varying what two of the four permanents ARE and nothing else: same
  -- seats, same count of permanents, same absence of mana. Rule 701.67a names an
  -- artifact or a creature, so two enchantments leave three candidates for a cost
  -- that wants four.
  Spec.it s "CR 701.67a an enchantment is not something a waterbend cost can tap" $ do
    leaper <- S.printingOf s registry "Geyser Leaper"
    piker <- S.printingOf s registry "Goblin Piker"
    crawlspace <- S.printingOf s registry "Crawlspace"
    anthem <- S.printingOf s registry "Glorious Anthem"
    mountain <- S.printingOf s registry "Mountain"
    let (leaperId, _, gs) = leaperBoard mountain leaper [piker, crawlspace, anthem, anthem] [] 0
    unpaidLeaper s leaperId gs
  -- The pair, varying ONE Mountain and nothing else: the same six untapped
  -- artifacts and creatures, the same tax. Rule 701.67b caps the substitution at
  -- the waterbend cost's own {4}, so a sixth tap cannot pay the {2} and one
  -- Mountain is a mana short of it.
  Spec.it s "CR 701.67b six untapped permanents do not pay a waterbend {4} taxed {2} more" $ do
    leaper <- S.printingOf s registry "Geyser Leaper"
    piker <- S.printingOf s registry "Goblin Piker"
    crawlspace <- S.printingOf s registry "Crawlspace"
    field <- S.printingOf s registry "Suppression Field"
    mountain <- S.printingOf s registry "Mountain"
    let (leaperId, _, gs) = leaperBoard mountain leaper [piker, piker, piker, crawlspace, crawlspace, crawlspace] [field] 1
    unpaidLeaper s leaperId gs

-- The negative: alice tries the same activation as greedily as the board allows,
-- and nothing happens. Asserted at GAMEPLAY level rather than off
-- Activatable.activatable -- an unpayable cost is rewound by
-- Activate.activateAbility, so an empty graveyard is the whole of what a player
-- would see.
unpaidLeaper :: (Monad m) => Spec.Spec m n -> ObjectId.ObjectId -> GameState.GameState -> m ()
unpaidLeaper s leaperId gs = case Projection.abilitiesOf leaperId gs of
  ability : _ -> do
    let activated = S.runPure waterbendingGreedily gs (Activate.activateAbility S.alice leaperId ability)
        resolved = S.runPure waterbendingGreedily activated Stack.resolveTop
    Spec.assertEqWith s "the ability was never paid for: alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 0
    Spec.assertEqWith s "and nothing of hers is tapped" (S.tappedCount S.alice resolved) 0
  [] -> Spec.assertFailure s "expected the Leaper to carry an activated ability"

-- Katara, Water Tribe's Hope {2}{W}{U}{U} Legendary Creature -- Human Warrior
-- Ally 3/3 (data/cards/katara-water-tribes-hope.json): "Vigilance / When Katara
-- enters, create a 1/1 white Ally creature token. / Waterbend {X}: Creatures you
-- control have base power and toughness X/X until end of turn. X can't be 0.
-- Activate only during your turn."
--
-- CR 107.3a's variable in a waterbend cost, and ONE announcement fixes both
-- halves of it: the {X} the licence scopes, in the cost's own mana part, and CR
-- 701.67b's ceiling on how much of that generic taps may pay
-- (CostComponent.WaterbendX). The base power and toughness is what a paid cost is
-- read off -- a Goblin Piker is printed 2/1, so neither number the assertions
-- read can be its own.
--
-- "X can't be 0" is ActivatedAbility.minimumX (CR 101.1): the floor pair below
-- proves the announcement side and the offer pair the gate.
--
-- Every board is LANDLESS, geyserLeaperSpec's posture above: an activation that
-- succeeds can only have been paid by tapping. The pair below varies the value
-- ANNOUNCED and nothing else -- same seats, same permanents, same absence of
-- mana -- so what the negative shows is the demand moving with the
-- announcement.
kataraSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kataraSpec s registry = Spec.describe s "Katara, Water Tribe's Hope" $ do
  -- The pair, varying the ANNOUNCED value and nothing else: rule 701.67b caps the
  -- substitution at the waterbend cost's own generic, which is the announcement
  -- itself, so a 6 asks for one more mana than the board can raise however
  -- greedily alice pays.
  Spec.it s "CR 701.67b an announced waterbend X larger than the board is not paid at all" $ do
    katara <- S.printingOf s registry "Katara, Water Tribe's Hope"
    piker <- S.printingOf s registry "Goblin Piker"
    crawlspace <- S.printingOf s registry "Crawlspace"
    mountain <- S.printingOf s registry "Mountain"
    let (kataraId, pikerId, _, gs) = kataraBoard mountain katara piker [crawlspace, crawlspace, crawlspace]
    resolved <- greedyKatara s 6 kataraId gs
    Spec.assertEqWith s "the ability was never paid for: the Piker's power is its printed 2" (Projection.powerOf pikerId resolved) (Just 2)
    Spec.assertEqWith s "and its toughness the printed 1" (Projection.toughnessOf pikerId resolved) (Just 1)
    Spec.assertEqWith s "and nothing of hers is tapped" (S.tappedCount S.alice resolved) 0
  -- The floor, as a pair varying the ANNOUNCED value alone: 0 is what the card
  -- forbids and 1 the least it permits. Reject-not-repair, so the refused 0
  -- leaves the Piker at its printed size rather than a base 0/0.
  Spec.it s "CR 101.1 an announced waterbend X of 0 is refused, and 1 is not" $ do
    katara <- S.printingOf s registry "Katara, Water Tribe's Hope"
    piker <- S.printingOf s registry "Goblin Piker"
    crawlspace <- S.printingOf s registry "Crawlspace"
    mountain <- S.printingOf s registry "Mountain"
    let (kataraId, pikerId, tappable, gs) = kataraBoard mountain katara piker [crawlspace, crawlspace, crawlspace]
    zero <- activatingKatara s 0 (ManaCost.MkManaCost []) [] kataraId gs
    one <- activatingKatara s 1 (ManaCost.MkManaCost []) (take 1 tappable) kataraId gs
    Spec.assertEqWith s "CR 101.1 at X=0 the Piker keeps its printed base power 2, not the 0 alice announced" (Projection.powerOf pikerId zero) (Just 2)
    Spec.assertEqWith s "and its printed toughness 1" (Projection.toughnessOf pikerId zero) (Just 1)
    Spec.assertEqWith s "at X=1 the Piker's base power and toughness are 1/1" (Projection.powerOf pikerId one, Projection.toughnessOf pikerId one) (Just 1, Just 1)
    Spec.assertEqWith s "and the one Crawlspace she tapped for it is tapped" (S.tappedCount S.alice one) 1
  -- The gate's side of the floor: CR 101.1 leaves X=1 the cheapest announcement,
  -- so the ability is offered only where {1} can be waterbent. A pair differing
  -- in whether Katara, the one permanent that could tap for it, is tapped.
  Spec.it s "CR 101.1/602.2 Katara's ability is not offered when no X of 1 or more is payable" $ do
    katara <- S.printingOf s registry "Katara, Water Tribe's Hope"
    mountain <- S.printingOf s registry "Mountain"
    let (kataraId, untapped) = soleKatara mountain katara
        tapped = S.tapObject kataraId untapped
    Spec.assertBool s (not (any (isActivateOf kataraId) (Action.legalActions S.alice tapped))) "with Katara tapped nothing can waterbend {1}, so no activation is offered"
    Spec.assertBool s (any (isActivateOf kataraId) (Action.legalActions S.alice untapped)) "and untapped she can tap for it herself, so one is"

-- Mind Grind {X}{U}{B} Sorcery: "Each opponent reveals cards from the top of
-- their library until they reveal X land cards, then puts all cards revealed
-- this way into their graveyard. X can't be 0." The last sentence is
-- Face.minimumX (CR 101.1): the floor pair proves the announcement side, the
-- offer pair the castability gate. The free cast CR 107.3b fixes at 0 is
-- omniscienceSpec's.
mindGrindSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mindGrindSpec s registry = Spec.describe s "Mind Grind" $ do
  -- A pair varying the ANNOUNCED value alone, on one board. X=1 is answered
  -- with the least value ChooseX carries, so the prompt's floor is read too.
  Spec.it s "CR 101.1 an announced X of 0 is refused, and the floor of 1 is not" $ do
    (spell, gs) <- mindGrindBoard s registry 2
    let castAt :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState
        castAt answer = S.runPure answer gs (do Cast.castSpell S.manaPerformer S.alice spell (S.soleFaceName spell gs) Facing.FaceUp; Stack.resolveTop)
        zero = castAt (\p -> case p of Prompt.ChooseX {} -> 0; _ -> S.identityAnswer p)
        least = castAt (\p -> case p of Prompt.ChooseX _ _ _ floorX _ -> floorX; _ -> S.identityAnswer p)
        graveyard pid = length . Game.zoneMembers Zone.Graveyard pid
    Spec.assertEqWith s "CR 101.1 at X=0 Mind Grind is still in alice's hand" (S.handSize S.alice zero) 1
    Spec.assertEqWith s "and bob milled nothing" (graveyard S.bob zero) 0
    Spec.assertEqWith s "at the prompt's least X bob milled down to his first land" (graveyard S.bob least) 2
    Spec.assertEqWith s "and carol hers" (graveyard S.carol least) 1
    Spec.assertEqWith s "and alice, no opponent, kept her library" (length (Game.zoneMembers Zone.Library S.alice least)) 1
  -- The gate's side: X=1 is the cheapest announcement, so {U}{B} alone does not
  -- offer the cast. A pair differing in one Mountain.
  Spec.it s "CR 101.1/601.2 Mind Grind is not offered when no X of 1 or more is payable" $ do
    (short, shortGs) <- mindGrindBoard s registry 0
    (enough, enoughGs) <- mindGrindBoard s registry 1
    Spec.assertBool s (not (any (S.isCastOf short) (Action.legalActions S.alice shortGs))) "with only {U}{B} no X of 1 is payable, so no cast is offered"
    Spec.assertBool s (any (S.isCastOf enough) (Action.legalActions S.alice enoughGs)) "and one Mountain more pays X=1, so one is"

-- Three seats, so "each opponent" is not each player. alice has an Island, a
-- Swamp and `n` Mountains, one card in her library and Mind Grind in hand; bob's
-- library is Piker, Mountain, Piker from the top and carol's Mountain, Piker.
mindGrindBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m (ObjectId.ObjectId, GameState.GameState)
mindGrindBoard s registry n = do
  grind <- S.printingOf s registry "Mind Grind"
  island <- S.printingOf s registry "Island"
  swamp <- S.printingOf s registry "Swamp"
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  let lands = S.landsFor mountain S.alice n (S.landsFor swamp S.alice 1 (S.landsFor island S.alice 1 S.threePlayerGame))
      stock pid printings g = List.foldl' (\acc p -> snd (S.addLibraryCard p pid acc)) g printings
      -- Each added card goes on top, so these lists read bottom-up.
      stocked = stock S.carol [piker, mountain] (stock S.bob [piker, mountain, piker] (stock S.alice [mountain] lands))
      (spell, gs) = S.addHandCard grind S.alice stocked
  pure
    ( spell,
      gs
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Katara alone on alice's landless battlefield, alice holding priority in her
-- own precombat main phase.
soleKatara :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
soleKatara mountain katara =
  let (kataraId, gs) = S.addPermanent katara S.alice (S.landsInPlay mountain 0)
   in (kataraId, gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice})

-- alice announces `x` for Katara's ability, takes the substitution that leaves
-- `wanted` to pay with mana, taps `tapped` for the rest, and the ability
-- resolves.
activatingKatara :: (Monad m) => Spec.Spec m n -> Natural.Natural -> ManaCost.ManaCost -> [ObjectId.ObjectId] -> ObjectId.ObjectId -> GameState.GameState -> m GameState.GameState
activatingKatara s x wanted tapped = resolvingKatara s (waterbendingX x wanted tapped)

-- The negative: alice announces `x` and pays as greedily as the board allows.
-- Asserted at GAMEPLAY level rather than off Activatable.activatable, unpaidLeaper's
-- reason above -- an unpayable cost is rewound by Activate.activateAbility, so a
-- Piker at its printed size is the whole of what a player would see.
greedyKatara :: (Monad m) => Spec.Spec m n -> Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> m GameState.GameState
greedyKatara s x = resolvingKatara s (waterbendingXGreedily x)

-- The body both share: activate Katara's sole activated ability under `answer`
-- and resolve the stack down.
resolvingKatara :: (Monad m) => Spec.Spec m n -> (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> m GameState.GameState
resolvingKatara s answer kataraId gs = case Projection.abilitiesOf kataraId gs of
  ability : _ ->
    let activated = S.runPure answer gs (Activate.activateAbility S.alice kataraId ability)
     in pure (S.runPure answer activated Stack.resolveTop)
  [] -> Spec.assertFailure s "expected Katara to carry an activated ability"

-- CR 601.2b's announcement in front of `waterbending`'s two prompts. PINNED by
-- value rather than read off the prompt: Prompt.ChooseX carries an advisory bound
-- and nothing filters the answer against it, so an answerer that took the bound
-- would repair a mutation that moved it.
waterbendingX :: Natural.Natural -> ManaCost.ManaCost -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
waterbendingX x wanted tapped p = case p of
  Prompt.ChooseX {} -> x
  _ -> waterbending wanted tapped p

-- The same announcement in front of `waterbendingGreedily`, and what the negative
-- is answered with for that function's reason: it takes whatever the engine
-- offers, so the case fails only where no offer pays.
waterbendingXGreedily :: Natural.Natural -> Prompt.Prompt r -> r
waterbendingXGreedily x p = case p of
  Prompt.ChooseX {} -> x
  _ -> waterbendingGreedily p

-- alice controls Katara, a Goblin Piker and one permanent per printing in
-- `others`, with no land at all -- `mountain` is the printing S.landsInPlay
-- names and none of it is put out. She has priority in her own precombat main
-- phase, which is when CR 117.1b lets her activate and what the ability's own
-- rider requires (CR 602.5). Returns Katara, the Piker, the `others` in order,
-- and that state.
kataraBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
kataraBoard mountain katara piker others =
  let (kataraId, gs1) = S.addPermanent katara S.alice (S.landsInPlay mountain 0)
      (pikerId, gs2) = S.addPermanent piker S.alice gs1
      (otherIds, gs3) = List.foldl' (\(ids, gs) printing -> let (oid, next) = S.addPermanent printing S.alice gs in (ids <> [oid], next)) ([], gs2) others
   in ( kataraId,
        pikerId,
        otherIds,
        gs3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Benevolent River Spirit {U}{U} Creature -- Spirit 4/5
-- (data/cards/benevolent-river-spirit.json): "As an additional cost to cast this
-- spell, waterbend {5}. / Flying, ward {2} / When this creature enters, scry 2."
--
-- Water Whip's cost on a PERMANENT spell, which is where CR 118.8d becomes
-- observable after the cast: the {5} is paid but is not part of the mana cost,
-- so the creature that enters has mana value 2 and not 7 (CR 202.3). Same board
-- as Water Whip's headline, so the {5} can only have been paid by tapping.
benevolentRiverSpiritSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
benevolentRiverSpiritSpec s registry = Spec.describe s "Benevolent River Spirit" $ do
  Spec.it s "CR 118.8d a creature cast with its additional waterbend {5} enters with mana value 2" $ do
    (spell, tappable, gs) <- waterbendSpellBoard s registry "Benevolent River Spirit" 2 False
    let blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
        answer :: Prompt.Prompt r -> r
        answer = waterbending (ManaCost.MkManaCost [blue, blue]) (take 5 tappable)
        resolved = S.runPure answer (S.runPure answer gs (S.cast S.alice spell)) Stack.resolveTop
        entered = filter (\oid -> Game.zoneOf oid resolved == Just Zone.Battlefield) (Scenario.namedObjects (CardName.MkCardName (Text.pack "Benevolent River Spirit")) resolved)
    Spec.assertEqWith s "CR 118.8d the Spirit on the battlefield has its printed mana value 2, not the 7 alice paid" (fmap (\oid -> Filter.manaValue (Projection.viewOfObject oid resolved)) entered) [Just 2]
    -- The taps, which the assertion above does not see: a Spirit whose {5} went
    -- unpaid would also have entered with mana value 2.
    Spec.assertEqWith s "CR 701.67a and seven permanents are tapped -- five for the waterbend cost and both Islands" (S.tappedCount S.alice resolved) 7

-- Waterbender's Restoration {U}{U} Instant -- Lesson
-- (data/cards/waterbenders-restoration.json): "As an additional cost to cast
-- this spell, waterbend {X}. / Exile X target creatures you control. Return
-- those cards to the battlefield under their owner's control at the beginning
-- of the next end step."
--
-- CR 701.67a's {X} form as a spell's additional cost: the X is announced off
-- the additional cost alone (CR 601.2b), since the mana cost states none, and
-- the same announcement fixes the target count (CR 601.2c) and CR 701.67b's
-- ceiling. Two Islands for the {U}{U}, so the {X} can only have been paid by
-- tapping; the taps are Crawlspaces, the targets two of three Goblin Pikers.
waterbendersRestorationSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
waterbendersRestorationSpec s registry = Spec.describe s "Waterbender's Restoration" $ do
  Spec.it s "CR 601.2b an X announced for a spell's additional waterbend {X} is paid by tapping and counts its targets" $ do
    (spell, tappable, gs) <- waterbendSpellBoard s registry "Waterbender's Restoration" 2 False
    let blue = ManaSymbol.OfType (ManaType.Colored Color.Blue)
        (pikers, crawlspaces) = List.splitAt 3 tappable
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (`elem` fmap Recipient.ToCreature (take 2 pikers)) candidates) sets
          _ -> waterbendingX 2 (ManaCost.MkManaCost [blue, blue]) (take 2 crawlspaces) p
        resolved = S.runPure answer (S.runPure answer gs (S.cast S.alice spell)) Stack.resolveTop
    Spec.assertEqWith s "CR 601.2c X = 2: two of alice's three Pikers were exiled" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice resolved, length (Game.zoneMembers Zone.Exile S.alice resolved)) (1, 2)
    -- The taps, which the assertion above does not see: an X paid out of
    -- nowhere would also have exiled two Pikers.
    Spec.assertEqWith s "CR 701.67a and four permanents are tapped -- two Crawlspaces for the waterbend cost and both Islands" (S.tappedCount S.alice resolved) 4

-- Spirit Water Revival {1}{U}{U} Sorcery (data/cards/spirit-water-revival.json):
-- "As an additional cost to cast this spell, you may waterbend {6}. / Draw two
-- cards. If this spell's additional cost was paid, instead shuffle your
-- graveyard into your library, draw seven cards, and you have no maximum hand
-- size for the rest of the game. / Exile Spirit Water Revival."
--
-- CR 118.8b's OPTIONAL additional cost as a two-option choice, the empty option
-- beside the waterbend, and the payment's own record (Binding.waterbendCost) as
-- what "was paid" reads. A pair differing only in which option alice takes.
spiritWaterRevivalSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
spiritWaterRevivalSpec s registry = Spec.describe s "Spirit Water Revival" $ do
  Spec.it s "CR 118.8b the spell reads whether its optional waterbend {6} was paid" $ do
    (spell, _, gs) <- optionalWaterbendBoard s registry "Spirit Water Revival" 3
    let resolved paid = S.runPure (optionalWaterbending paid) (S.runPure (optionalWaterbending paid) gs (S.cast S.alice spell)) Stack.resolveTop
        outcome after = (S.handSize S.alice after, length (Game.zoneMembers Zone.Graveyard S.alice after), length (Game.zoneMembers Zone.Exile S.alice after))
    Spec.assertEqWith s "CR 118.8b paid: her graveyard was shuffled away, she drew seven, and the spell is in exile" (outcome (resolved True)) (7, 0, 1)
    Spec.assertEqWith s "unpaid: she drew two, and her graveyard's Water Whip stayed" (outcome (resolved False)) (2, 1, 1)
    -- The taps, which the assertions above do not see: a {6} paid out of
    -- nowhere would also have drawn seven.
    Spec.assertEqWith s "CR 701.67a paid, nine permanents are tapped -- six for the waterbend cost and all three Islands; unpaid, the Islands alone" (S.tappedCount S.alice (resolved True), S.tappedCount S.alice (resolved False)) (9, 3)

-- Katara, Seeking Revenge {3}{U/B} Legendary Creature -- Human Warrior Ally 3/3
-- (data/cards/katara-seeking-revenge.json): "As an additional cost to cast this
-- spell, you may waterbend {2}. / When Katara enters, draw a card, then discard
-- a card unless her additional cost was paid. / Katara gets +1/+1 for each
-- Lesson card in your graveyard."
--
-- Spirit Water Revival's read on a PERMANENT: CR 400.7d carries the spell's
-- payment record onto Katara (Binding.paidCostRecord), and her enters trigger
-- reads it there. Four Islands pay the {3}{U/B}; the {2} is paid by tapping.
kataraSeekingRevengeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
kataraSeekingRevengeSpec s registry = Spec.describe s "Katara, Seeking Revenge" $ do
  Spec.it s "CR 400.7d her enters trigger reads whether her optional waterbend {2} was paid" $ do
    (spell, _, gs) <- optionalWaterbendBoard s registry "Katara, Seeking Revenge" 4
    let resolved paid =
          let answer :: Prompt.Prompt r -> r
              answer = optionalWaterbending paid
              entered = S.runPure answer (S.runPure answer gs (S.cast S.alice spell)) (Stack.resolveTop >> Engine.settleForPriority)
           in S.runPure answer entered Stack.resolveTop
        katara after = filter (\oid -> Game.zoneOf oid after == Just Zone.Battlefield) (Scenario.namedObjects (CardName.MkCardName (Text.pack "Katara, Seeking Revenge")) after)
    Spec.assertEqWith s "CR 400.7d paid: she drew and kept the card" (S.handSize S.alice (resolved True)) 1
    Spec.assertEqWith s "unpaid: she drew and discarded it" (S.handSize S.alice (resolved False)) 0
    Spec.assertEqWith s "CR 701.67a paid, six permanents are tapped -- two for the waterbend cost and all four Islands" (S.tappedCount S.alice (resolved True)) 6
    -- The graveyard's Water Whip is a Lesson; unpaid, the discarded Island is not.
    Spec.assertEqWith s "and she is 4/4 off the Water Whip in alice's graveyard" (fmap (`S.powerToughnessOf` resolved True) (katara (resolved True))) [Just (4, 4)]

-- Hama, the Bloodbender {2}{U/B}{U/B}{U/B} Legendary Creature -- Human Warlock
-- 3/3 (data/cards/hama-the-bloodbender.json): "When Hama enters, target
-- opponent mills three cards. Exile up to one noncreature, nonland card from
-- that player's graveyard. For as long as you control Hama, you may cast the
-- exiled card during your turn by waterbending {X} rather than paying its mana
-- cost, where X is its mana value."
--
-- CR 118.9's alternative cost stated by a CR 601.3 permission, as a CR 701.67a
-- waterbend scaled to the card: bob mills Think Twice (mana value 2), which
-- alice exiles. An instant, so CR 307.1 would let her cast it on bob's turn and
-- the permission's window is the only thing refusing it there.
hamaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hamaSpec s registry = Spec.describe s "Hama, the Bloodbender" $ do
  -- Hama's five Islands are all tapped, so the {2} can only be paid by tapping
  -- two of the three Pikers.
  Spec.it s "CR 118.9 alice casts the exiled card by waterbending its mana value, during her turn only" $ do
    (exiled, pikers, gs) <- hamaBoard s registry "Think Twice" False 0
    let answer :: Prompt.Prompt r -> r
        answer = waterbending (ManaCost.MkManaCost []) (take 2 pikers)
        resolved = S.runPure answer (S.runPure answer gs (S.cast S.alice exiled)) Stack.resolveTop
        offered board = any (S.isCastOf exiled) (Action.legalActions S.alice board)
    Spec.assertEqWith s "CR 701.67a Think Twice resolved off two Pikers: alice drew a card, and seven permanents are tapped -- Hama's five Islands and two Pikers" (S.handSize S.alice resolved, S.tappedCount S.alice resolved) (1, 7)
    Spec.assertEqWith s "CR 601.3 the cast is offered during alice's turn and not during bob's" (offered gs, offered gs {GameState.activePlayer = S.bob}) (True, False)
  -- CR 701.67b: bob's Thalia taxes the noncreature spell {1} more (CR 601.2f),
  -- so the total is {3} of which the waterbend is {2}. A pair varying one spare
  -- Island and nothing else: the third Piker cannot pay the tax.
  Spec.it s "CR 701.67b the waterbend's taps pay its own {2} and not Thalia's tax" $ do
    (spared, sparedPikers, withIsland) <- hamaBoard s registry "Think Twice" True 1
    (short, _, withoutIsland) <- hamaBoard s registry "Think Twice" True 0
    let answer :: Prompt.Prompt r -> r
        answer = waterbending (ManaCost.MkManaCost [ManaSymbol.Generic 1]) (take 2 sparedPikers)
        paid = S.runPure answer (S.runPure answer withIsland (S.cast S.alice spared)) Stack.resolveTop
        unpaid = S.runPure waterbendingGreedily (S.runPure waterbendingGreedily withoutIsland (S.cast S.alice short)) Stack.resolveTop
    Spec.assertEqWith s "CR 701.67b with a spare Island alice drew a card; with three Pikers and no Island Think Twice is still in exile" (S.handSize S.alice paid, Game.zoneOf short unpaid) (1, Just Zone.Exile)
  -- CR 118.9 against CR 118.8b: Hama's waterbend {3} is an ALTERNATIVE cost, so
  -- Spirit Water Revival cast through it with the optional {6} declined reads
  -- its additional cost as unpaid. Three Pikers pay the {3}.
  Spec.it s "CR 118.8b Spirit Water Revival cast through Hama without its {6} draws two" $ do
    (exiled, _, gs) <- hamaBoard s registry "Spirit Water Revival" False 0
    let answer :: Prompt.Prompt r -> r
        answer = optionalWaterbending False
        resolved = S.runPure answer (S.runPure answer gs (S.cast S.alice exiled)) Stack.resolveTop
    Spec.assertEqWith s "CR 118.8b alice drew two and her graveyard's Water Whip stayed, and eight permanents are tapped -- Hama's five Islands and three Pikers" (S.handSize S.alice resolved, length (Game.zoneMembers Zone.Graveyard S.alice resolved), S.tappedCount S.alice resolved) (2, 1, 8)

-- alice casts Hama off five Islands (plus `spare` more) beside three Goblin
-- Pikers, and her enters trigger mills bob's `milled` card, an Island and a
-- Goblin Piker; she takes the "up to one" and exiles `milled`, the only
-- candidate. bob controls Thalia, Guardian of Thraben where `taxed`. alice has
-- eight Islands in her library for a draw of seven (CR 104.3c), a Water Whip in
-- her graveyard, and priority in her own precombat main phase. Returns the
-- exiled card, the Pikers and that state.
hamaBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> Bool -> Int -> m (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
hamaBoard s registry milled taxed spare = do
  hama <- S.printingOf s registry "Hama, the Bloodbender"
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  card <- S.printingOf s registry milled
  whip <- S.printingOf s registry "Water Whip"
  thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
  let (pikers, gs1) = List.foldl' (\(ids, gs) printing -> let (oid, next) = S.addPermanent printing S.alice gs in (ids <> [oid], next)) ([], S.landsInPlay island (5 + spare)) [piker, piker, piker]
      gs2 = if taxed then snd (S.addPermanent thalia S.bob gs1) else gs1
      stocked = List.foldl' (\acc printing -> snd (S.addLibraryCard printing S.bob acc)) gs2 [card, island, piker]
      drawable = snd (S.addGraveyardCard whip S.alice (List.foldl' (\acc _ -> snd (S.addLibraryCard island S.alice acc)) stocked [1 .. 8 :: Int]))
      (spell, gs3) = S.addHandCard hama S.alice drawable
      gs4 = gs3 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
      exercising :: Prompt.Prompt r -> r
      exercising p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      entered = S.runPure exercising (S.runPure exercising gs4 (S.cast S.alice spell)) (Stack.resolveTop >> Engine.settleForPriority)
      triggered = S.runPure exercising entered Stack.resolveTop
  case Game.zoneMembers Zone.Exile S.bob triggered of
    [exiled] -> pure (exiled, pikers, triggered)
    other -> Spec.assertFailure s ("Hama's trigger should exile bob's Think Twice, exiled: " <> show other)

-- The payer who waterbends where `paid`: the candidate stating the waterbend
-- licence that leaves the least mana to find, its taps the first ones offered.
-- Where not, the candidate stating no waterbend at all -- CR 118.8b's other
-- option. Picked by the licence rather than by mana, since both options can
-- leave the same mana part.
optionalWaterbending :: Bool -> Prompt.Prompt r -> r
optionalWaterbending paid p = case p of
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered (List.sortOn (maybe 0 manaOwed . Cost.Type.mana) (filter ((== paid) . any isWaterbend . Cost.Type.components) candidates))
  Prompt.ChooseTaps _ _ _ candidates wanted -> Set.fromList (take (Natural.Extra.toIntSaturating wanted) candidates)
  _ -> S.identityAnswer p
  where
    isWaterbend component = case component of
      CostComponent.Waterbend _ -> True
      _ -> False

-- waterbendSpellBoard's board with eight Islands in alice's library, for a draw
-- of seven (CR 104.3c), and a Water Whip in her graveyard: a card for a shuffle
-- to move, and a Lesson.
optionalWaterbendBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> Int -> m (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
optionalWaterbendBoard s registry name islands = do
  (spell, tappable, gs) <- waterbendSpellBoard s registry name islands False
  island <- S.printingOf s registry "Island"
  whip <- S.printingOf s registry "Water Whip"
  let stocked = List.foldl' (\acc _ -> snd (S.addLibraryCard island S.alice acc)) gs [1 .. 6 :: Int]
  pure (spell, tappable, snd (S.addGraveyardCard whip S.alice stocked))

-- alice holds the named card, controls `islands` Islands, three Goblin Pikers
-- and three Crawlspaces, and has two Islands in her library for a draw (CR
-- 104.3c); bob controls Thalia, Guardian of Thraben where `taxed`. She has
-- priority in her own precombat main phase with an empty stack, when CR 307.1
-- lets her cast a sorcery and CR 302.1 a creature. Returns the spell, the six
-- tappable permanents and that state.
waterbendSpellBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> Int -> Bool -> m (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
waterbendSpellBoard s registry name islands taxed = do
  card <- S.printingOf s registry name
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  crawlspace <- S.printingOf s registry "Crawlspace"
  thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
  let (tappable, gs1) = List.foldl' (\(ids, gs) printing -> let (oid, next) = S.addPermanent printing S.alice gs in (ids <> [oid], next)) ([], S.landsInPlay island islands) [piker, piker, piker, crawlspace, crawlspace, crawlspace]
      gs2 = if taxed then snd (S.addPermanent thalia S.bob gs1) else gs1
      (_, gs3) = S.addLibraryCard island S.alice gs2
      (_, gs4) = S.addLibraryCard island S.alice gs3
      (spell, gs5) = S.addHandCard card S.alice gs4
  pure (spell, tappable, gs5 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice})

-- alice holds `card`, controls one Forest and `mountains` Mountains, and bob
-- controls `plainses` Plains; she has priority in her own precombat main phase so
-- a creature spell is castable (CR 302.1). Returns the spell, the Forest, bob's
-- Plains and that state.
--
-- Her Mountains and bob's Plains are different printings, so each window's
-- answer is named by its printing and no counting is needed to keep the two
-- apart.
assistBoard :: Int -> Int -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
assistBoard mountains plainses forest mountain plains card =
  let place printing who n gs = List.foldl' (\(ids, acc) _ -> let (oid, next) = S.addPermanent printing who acc in (ids <> [oid], next)) ([], gs) (replicate n ())
      (forestId, gs1) = S.addPermanent forest S.alice (Setup.emptyGame S.bothPlayers)
      (_, gs2) = place mountain S.alice mountains gs1
      (plainsIds, gs3) = place plains S.bob plainses gs2
      (spell, gs4) = S.addHandCard card S.alice gs3
   in ( spell,
        forestId,
        plainsIds,
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Charging Binox {7}{G} Creature -- Beast 7/5: "Assist. Trample."
--
-- CR 702.132a's whole sentence on the cheapest printing that states assist and
-- nothing else: no entry trigger, no target, no second keyword that touches the
-- cast.
--
-- The two cases are ONE board answered two ways -- alice floats a single {G}
-- either way -- so the only thing that differs is whether she named bob.
assistSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
assistSpec s registry = Spec.describe s "Charging Binox" $ do
  -- The negative, one Plains fewer: {7}{G} is out of reach of both players
  -- together, so the cast is not offered.
  Spec.it s "CR 702.132a alice is not offered the Binox when bob could pay only six" $ do
    binox <- S.printingOf s registry "Charging Binox"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    let (spell, _, _, gs) = assistBoard 0 6 forest mountain plains binox
    Spec.assertEqWith s "the cast is not offered" (S.castable S.alice spell gs) False

  -- CR 733.1's "each player may also reverse", with two players who activated
  -- something: alice taps her Forest, bob taps three of his seven Plains and
  -- pays three of the generic, and {G} plus three leaves four unpaid, so CR
  -- 601.2h refuses the cast. One board, the four combinations of answers.
  Spec.it s "CR 733.1 a refused assisted cast asks the helper too, and each keeps only their own" $ do
    binox <- S.printingOf s registry "Charging Binox"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    let (spell, forestId, plainsIds, gs) = assistBoard 0 7 forest mountain plains binox
        run alice bob = State.runState (Engine.runGame (reversingAssist alice bob forestId (take 3 plainsIds)) gs (S.cast S.alice spell)) []
        ((_, bobKeeps), asked) = run OptionalDecision.Exercises OptionalDecision.Declines
        ((_, aliceKeeps), _) = run OptionalDecision.Declines OptionalDecision.Exercises
        ((_, bothKeep), _) = run OptionalDecision.Declines OptionalDecision.Declines
        ((_, neither), _) = run OptionalDecision.Exercises OptionalDecision.Exercises
    -- THE BEHAVIOUR: bob kept his three Plains' mana, and CR 733.1 cancelled
    -- the three he had paid, so it is back in his pool.
    Spec.assertEqWith s "the helper who keeps has his three Plains' mana floating" (poolSize S.bob bobKeeps) 3
    Spec.assertEqWith s "and his three Plains tapped" (S.tappedCount S.bob bobKeeps) 3
    Spec.assertEqWith s "while alice, who reversed, has nothing floating" (poolSize S.alice bobKeeps) 0
    Spec.assertEqWith s "and her Forest untapped" (S.tappedCount S.alice bobKeeps) 0
    Spec.assertEqWith s "the helper who reverses has nothing floating" (poolSize S.bob aliceKeeps) 0
    Spec.assertEqWith s "and every Plains untapped" (S.tappedCount S.bob aliceKeeps) 0
    Spec.assertEqWith s "while alice, who kept, has her {G}" (poolSize S.alice aliceKeeps) 1
    Spec.assertEqWith s "both keeping: bob's three and alice's one" (poolSize S.bob bothKeep, poolSize S.alice bothKeep) (3, 1)
    Spec.assertEqWith s "both reversing: nothing tapped" (S.tappedCount S.bob neither + S.tappedCount S.alice neither) 0
    Spec.assertEqWith s "CR 601.2a the Binox is back in alice's hand whatever they answered" (fmap (\g -> (GameState.stack g, Game.zoneMembers Zone.Hand S.alice g)) [bobKeeps, aliceKeeps, bothKeep, neither]) (replicate 4 ([], [spell]))
    Spec.assertEqWith s "CR 101.4 alice then bob, each once" asked [S.alice, S.bob]
  -- CR 101.4b: bob answers second, knowing alice's answer. The pair differs
  -- only in what alice answered; the board bob is asked on is what shows it.
  Spec.it s "CR 101.4b the helper is asked on the board the caster's answer left" $ do
    binox <- S.printingOf s registry "Charging Binox"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    let (spell, forestId, plainsIds, gs) = assistBoard 0 7 forest mountain plains binox
        run alice bob = State.runState (Engine.runGameAsked (seeingAssist alice bob forestId (take 3 plainsIds)) gs (S.cast S.alice spell)) []
        (_, seenKeep) = run OptionalDecision.Declines OptionalDecision.Declines
        (_, seenReverse) = run OptionalDecision.Exercises OptionalDecision.Declines
        ((_, neither), seenNeither) = run OptionalDecision.Exercises OptionalDecision.Exercises
    Spec.assertEqWith s "bob is asked with alice's reversed Forest untapped" (fmap (isTapped forestId) seenReverse) [False]
    Spec.assertEqWith s "and with her kept Forest tapped" (fmap (isTapped forestId) seenKeep) [True]
    -- CR 104.4b: the stamp bob's question wrote survives the final restore,
    -- which for two reversals goes back to a state from before the cast.
    Spec.assertEqWith s "CR 104.4b the last question's stamp stands" (fmap GameState.lastChoice seenNeither) [GameState.lastChoice neither]
  -- CR 106.6a on the HELPER's mana: bob sacrifices Generator Servant ({T},
  -- Sacrifice: "Add {C}{C}. If any of that mana is spent on a creature spell, it
  -- gains haste until end of turn.") in his assist window and pays seven with its
  -- two and five Plains. The control is the same board with two Plains in the
  -- Servant's place, so the only difference is where two of bob's seven came from.
  Spec.it s "CR 106.6a the Binox bob's Generator Servant helped pay for attacks the turn it arrives" $ do
    binox <- S.printingOf s registry "Charging Binox"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    servant <- S.printingOf s registry "Generator Servant"
    let fought (spell, forestId, sources, gs) =
          let board = gs {GameState.remaining = S.phasesAfter Phase.PrecombatMain}
              answer :: Prompt.Prompt r -> r
              answer = assisting (Just S.bob) 7 forestId sources
              resolved = S.runPure answer (S.runPure answer board (S.cast S.alice spell)) Stack.resolveTop
           in (resolved, S.runPure S.aggressiveAnswer resolved (Monad.replicateM_ 6 Engine.runStep))
        withServant =
          let (spell, forestId, plainsIds, gs) = assistBoard 0 5 forest mountain plains binox
              (servantId, gs') = S.addPermanent servant S.bob gs
           in fought (spell, forestId, servantId : plainsIds, gs')
        withPlains = fought (assistBoard 0 7 forest mountain plains binox)
        spentOn gs = sum (fmap (\oid -> maybe 0 (length . Mana.Type.unwrap . Object.manaSpent) (Game.lookupObject oid gs)) (Game.zoneMembers Zone.Battlefield S.alice gs))
    Spec.assertEqWith s "CR 106.6a bob takes 7 trample from the Binox his Servant's mana helped pay for" (S.lifeOf S.bob (snd withServant)) (Just 13)
    Spec.assertEqWith s "and none from the one his seven Plains paid for" (S.lifeOf S.bob (snd withPlains)) (Just 20)
    Spec.assertEqWith s "CR 400.7d the Binox's record holds alice's {G} and bob's seven" (spentOn (fst withServant)) 8

-- Answer CR 702.132a's two prompts with `helper` and `amount`, and each mana
-- window with the lands that window's player is meant to tap -- alice the one
-- Forest, bob every Plains.
--
-- FILTERED against the offer, `convoking`'s posture and for its reason: an
-- answer the engine did not offer reads as declining, so the assertions stay
-- about the engine's own candidates. alice's Forest drops out of her offer once
-- it is tapped, which is what closes her window after one mana without anything
-- here having to count.
assisting :: Maybe PlayerId.PlayerId -> Natural.Natural -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
assisting helper amount forestId plainsIds p =
  let wanted who = if who == S.bob then plainsIds else [forestId]
      pick who candidates = List.find (`elem` wanted who) (NonEmpty.toList candidates)
   in case p of
        Prompt.ChooseAssistant _ _ _ candidates -> case helper of
          Just who | elem who (NonEmpty.toList candidates) -> Just who
          _ -> Nothing
        Prompt.ChooseAssistAmount {} -> amount
        Prompt.ChooseManaSource _ who candidates -> pick who candidates
        Prompt.ChooseExtraManaSource _ who candidates -> pick who candidates
        _ -> S.identityAnswer p

-- `assisting` with bob chosen to pay as much as his tapped Plains made, and CR
-- 733.1's question answered `alice` or `bob` by whoever is asked, the askers
-- recorded in order.
-- `reversingAssist` over the whole question, recording the game bob is asked
-- in.
seeingAssist :: OptionalDecision.OptionalDecision -> OptionalDecision.OptionalDecision -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Asked.Asked r -> State.State [GameState.GameState] r
seeingAssist alice bob forestId plainsIds asked = case Asked.prompt asked of
  Prompt.ReverseManaAbilities _ player _
    | player == S.bob -> do
        State.modify' (<> [Asked.game asked])
        pure bob
    | otherwise -> pure alice
  p -> pure (assisting (Just S.bob) (Natural.Extra.length plainsIds) forestId plainsIds p)

reversingAssist :: OptionalDecision.OptionalDecision -> OptionalDecision.OptionalDecision -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
reversingAssist alice bob forestId plainsIds p = case p of
  Prompt.ReverseManaAbilities _ player _ -> do
    State.modify' (<> [player])
    pure (if player == S.bob then bob else alice)
  _ -> pure (assisting (Just S.bob) (Natural.Extra.length plainsIds) forestId plainsIds p)

-- A spell whose own cost sentence reads its CR 601.2c targets. Oracle
-- text checked against Scryfall 2026-09-27:
--
--   Bury in Books {4}{U} Instant: "This spell costs {2} less to cast if it
--   targets an attacking creature. Put target creature into its owner's library
--   second from the top."
--   Vanish into Eternity {2}{W} Instant: "This spell costs {3} more to cast if
--   it targets a creature. Exile target nonland permanent."
--   Dragon's Prey {2}{B} Instant: "This spell costs {2} more to cast if it
--   targets a Dragon. Destroy target creature."
--   Seized from Slumber {4}{W} Instant: "This spell costs {3} less to cast if it
--   targets a tapped creature. Destroy target creature."
--
-- alice holds `spell` and `lands` untapped copies of `land`, and has priority;
-- bob controls `victims` and has Lightning Bolt under Unsummon in his library.
-- Each pair of boards differs in one thing: the target, or the one fact about
-- the board the sentence reads.
targetCostBoard :: Printing.Printing -> Int -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
targetCostBoard land lands spell victims library =
  let g0 = S.landsFor land S.alice lands (Setup.emptyGame S.bothPlayers)
      (spellId, g1) = S.addHandCard spell S.alice g0
      (victimIds, g2) = List.foldl' (\(ids, g) p -> let (oid, g') = S.addPermanent p S.bob g in (ids <> [oid], g')) ([], g1) victims
      g3 = List.foldl' (\g p -> snd (S.addLibraryCard p S.bob g)) g2 library
   in (spellId, victimIds, g3 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice})

-- The answerer pinning CR 601.2c's announcement to `aim`, filtering the offered
-- set rather than building a recipient.
aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAt aim p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter ((==) (Just aim) . Recipient.objectOf) offered) asked
  _ -> S.identityAnswer p

-- CR 601.2c / 601.2f: the total is determined after the targets are chosen, so
-- a spell's own sentence may read them; CR 601.2 makes the cast legal when SOME
-- aiming can be paid for (Pawl.Engine.Cast.payableCostAt).
targetCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
targetCostSpec s registry =
  Spec.describe s "a cost that reads the spell's targets" $ do
    let named = Just . CardName.MkCardName . Text.pack
        onBattlefield oid gs = Game.zoneOf oid gs == Just Zone.Battlefield
        -- bob attacks alice with the first Giant when `attacking`; the second
        -- stays home. Declare blockers, alice to act.
        buryBoard lands attacking = do
          island <- S.printingOf s registry "Island"
          bury <- S.printingOf s registry "Bury in Books"
          giant <- S.printingOf s registry "Hill Giant"
          library <- traverse (S.printingOf s registry) ["Lightning Bolt", "Unsummon"]
          let (buryId, giants, gs) = targetCostBoard island lands bury [giant, giant] library
              attackers = if attacking then Map.fromList [(oid, AttackTarget.OfPlayer S.alice) | oid <- take 1 giants] else Map.empty
          pure
            ( buryId,
              giants,
              gs
                { GameState.activePlayer = S.bob,
                  GameState.phase = Phase.Combat CombatStep.DeclareBlockers,
                  GameState.combat = Combat.emptyCombat {Combat.Type.attackers = attackers, Combat.Type.defenders = [S.alice]}
                }
            )
    Spec.it s "CR 601.2 with no creature attacking, the same three Islands cannot cast it" $ do
      (buryId, giants, gs) <- buryBoard 3 False
      case giants of
        [first, _] -> do
          let attempted = S.runPure (aimAt first) gs (S.cast S.alice buryId)
          Spec.assertBool s (onBattlefield first attempted) "the Giant is still on the battlefield"
          Spec.assertEqWith s "the spell went back to alice's hand" (Game.zoneOf buryId attempted) (Just Zone.Hand)
          Spec.assertBool s (not (S.castable S.alice buryId gs)) "it is not offered"
        _ -> Spec.assertFailure s "two Giants"
    let vanishBoard lands victims = do
          plains <- S.printingOf s registry "Plains"
          vanish <- S.printingOf s registry "Vanish into Eternity"
          ps <- traverse (S.printingOf s registry) victims
          pure (targetCostBoard plains lands vanish ps [])
    Spec.it s "CR 601.2f Vanish into Eternity costs {3} more aimed at a creature than at an artifact" $ do
      (vanishId, victims, gs) <- vanishBoard 6 ["Hill Giant", "Sol Ring"]
      case victims of
        [giant, ring] -> do
          let run aim = S.runPure (aimAt aim) (S.runPure (aimAt aim) gs (S.cast S.alice vanishId)) Stack.resolveTop
          Spec.assertEqWith s "Plains tapped aiming at the Giant, then at Sol Ring" (S.tappedCount S.alice (run giant), S.tappedCount S.alice (run ring)) (6, 3)
          Spec.assertEqWith s "CR 406.2 the Giant was exiled" (fmap (\oid -> fmap S.nameOf (Game.cardOf oid (run giant))) (Game.zoneMembers Zone.Exile S.bob (run giant))) [named "Hill Giant"]
        _ -> Spec.assertFailure s "a Giant and a Sol Ring"
    Spec.it s "CR 601.2 three Plains cast Vanish into Eternity at Sol Ring, and refuse it with only the Giant to aim at" $ do
      (withRing, _, ringBoard) <- vanishBoard 3 ["Hill Giant", "Sol Ring"]
      (withoutRing, _, giantBoard) <- vanishBoard 3 ["Hill Giant"]
      Spec.assertEqWith s "offered beside Sol Ring, refused without it" (S.castable S.alice withRing ringBoard, S.castable S.alice withoutRing giantBoard) (True, False)
    let preyBoard lands victims = do
          swamp <- S.printingOf s registry "Swamp"
          prey <- S.printingOf s registry "Dragon's Prey"
          ps <- traverse (S.printingOf s registry) victims
          pure (targetCostBoard swamp lands prey ps [])
    Spec.it s "CR 601.2f Dragon's Prey costs {2} more aimed at a Dragon" $ do
      (preyId, victims, gs) <- preyBoard 5 ["Shivan Dragon", "Hill Giant"]
      case victims of
        [dragon, giant] -> do
          let run aim = S.runPure (aimAt aim) (S.runPure (aimAt aim) gs (S.cast S.alice preyId)) Stack.resolveTop
          Spec.assertEqWith s "Swamps tapped aiming at the Dragon, then at the Giant" (S.tappedCount S.alice (run dragon), S.tappedCount S.alice (run giant)) (5, 3)
          Spec.assertBool s (not (onBattlefield dragon (run dragon))) "CR 701.8a the Dragon was destroyed"
        _ -> Spec.assertFailure s "a Dragon and a Giant"
    Spec.it s "CR 601.2 three Swamps cannot cast Dragon's Prey with only a Dragon to aim at" $ do
      (withGiant, _, giantBoard) <- preyBoard 3 ["Shivan Dragon", "Hill Giant"]
      (onlyDragon, _, dragonBoard) <- preyBoard 3 ["Shivan Dragon"]
      Spec.assertEqWith s "offered beside the Giant, refused without it" (S.castable S.alice withGiant giantBoard, S.castable S.alice onlyDragon dragonBoard) (True, False)
    Spec.it s "CR 601.2f Seized from Slumber is cast off two Plains at a tapped Giant, and refused at an untapped one" $ do
      plains <- S.printingOf s registry "Plains"
      seized <- S.printingOf s registry "Seized from Slumber"
      giant <- S.printingOf s registry "Hill Giant"
      let (seizedId, giants, gs) = targetCostBoard plains 2 seized [giant] []
          tapAll g = g {GameState.objects = List.foldl' (flip (Map.adjust (\o -> o {Object.tapped = TapState.Tapped}))) (GameState.objects g) giants}
      case giants of
        [target] -> do
          let run board = S.runPure (aimAt target) (S.runPure (aimAt target) board (S.cast S.alice seizedId)) Stack.resolveTop
          Spec.assertEqWith s "the Giant is destroyed when tapped, and survives untapped" (onBattlefield target (run (tapAll gs)), onBattlefield target (run gs)) (False, True)
          Spec.assertEqWith s "offered at the tapped Giant, refused at the untapped one" (S.castable S.alice seizedId (tapAll gs), S.castable S.alice seizedId gs) (True, False)
        _ -> Spec.assertFailure s "one Giant"
