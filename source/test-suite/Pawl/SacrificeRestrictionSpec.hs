{-# LANGUAGE RankNTypes #-}

-- Covers: CR 108.3 / 110.2 / 111.2's OWNER, read as Pawl.Types.Filter's OwnedBy
-- atom off Pawl.Engine.Filter.View's `owner` field; and CR 701.21a / CR 101.2's
-- SACRIFICE PROHIBITION -- Pawl.Types.SacrificeRestriction, the set
-- Pawl.Engine.SacrificeRestriction answers, and the two places that set is
-- subtracted (Pawl.Engine.Replacement.sacrificeCandidates for every offer, and
-- Pawl.Engine.Event.sacrifice for every instruction that names a victim without
-- asking).
--
-- Garland, Royal Kidnapper is the fixture, and it is the whole point: "Creatures
-- you control but don't own get +2/+2 and can't be sacrificed" is one Filter
-- feeding both halves, which is why they are one unit.
--
-- THE BOARD SHAPE that makes every case here discriminating: two creatures that
-- AGREE on controller and DISAGREE on owner. A board carrying only a stolen
-- creature would pass under an implementation that read the controller for the
-- owner, since `Not (OwnedBy You)` and `Not (ControlledBy You)` both admit it;
-- alice's own creature beside it is what makes the two readings answer
-- oppositely. Goblin Piker is 2/1, so the anthem's +2/+2 reads 4/3 -- two numbers
-- that differ from the printed pair in both coordinates, so no case passes on an
-- arithmetic coincidence.
--
-- THE SECOND FIXTURE SHAPE is a Garland alice controls but does not own. Garland
-- is a creature, so on any board where alice owns it she can always sacrifice
-- IT -- and then no CR 118.3 unpayability is reachable. Stealing Garland puts it
-- inside its own affected set, which is what the printed sentence says of a
-- stolen Garland and what lets a board have no legal victim at all.
--
-- Not covered: CR 704.5s's Saga sacrifice under a prohibition
-- (Pawl.Engine.Sba filters it out before reporting one, so the CR 704.3 settle
-- loop terminates). No card in the pool gives a player control of another
-- player's Saga, so the board cannot be built from card data.
module Pawl.SacrificeRestrictionSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Zone as Zone

-- "a creature", the criterion Village Rites' additional cost and Diabolic Edict's
-- effect both carry.
anyCreature :: Filter.Filter Keyword.Keyword
anyCreature = Filter.HasCardType CardType.Creature

-- The slot Diabolic Edict's target player is bound in, matching Pawl.ResolveSpec.
victimSlot :: SlotName.SlotName
victimSlot = SlotName.MkSlotName (Text.pack "target")

-- Which of alice's permanents she may sacrifice for "a creature" right now --
-- the offer Prompt.ChooseSacrifices is built from, and the count CR 118.3 reads.
offered :: GameState.GameState -> [ObjectId.ObjectId]
offered = Replacement.sacrificeCandidates S.alice Nothing anyCreature

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "SacrificeRestriction" $ do
  ownerSpec s registry
  offerSpec s registry
  instructionSpec s registry

-- CR 108.3 / 110.2 / 111.2: whose card it is, asked apart from who controls it.
ownerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ownerSpec s registry = Spec.describe s "Owner" $ do
  -- The single most important behavioural claim in the unit: a creature alice
  -- CONTROLS but does not OWN is inside Garland's set, and one she both controls
  -- and owns is outside it. The two Pikers agree on controller and disagree on
  -- owner, so reading the controller where CR 108.3 asks for the owner flips the
  -- SECOND assertion while leaving the first alone.
  Spec.it s "CR 108.3 whole card: Garland pumps the creature alice controls but does not own, and only it" $ do
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature garland S.alice (Setup.emptyGame S.bothPlayers)
        (hers, g1) = S.addCreature piker S.alice g0
        (stolen, g2) = S.addCreature piker S.bob g1
        (his, g3) = S.addCreature piker S.bob g2
        gs = S.giveControl stolen S.alice g3
    Spec.assertEqWith s "CR 613.4c: the stolen Piker is 4/3" (S.powerToughnessOf stolen gs) (Just (4, 3))
    Spec.assertEqWith s "alice's own Piker is untouched at 2/1" (S.powerToughnessOf hers gs) (Just (2, 1))
    -- The other conjunct, so the anthem is not simply "every Piker bob owns":
    -- bob still controls this one, so "you control" excludes it.
    Spec.assertEqWith s "and the Piker bob still controls is 2/1" (S.powerToughnessOf his gs) (Just (2, 1))
  -- CR 111.2: "the player who creates a token is its owner." A token has no card
  -- behind it, so an implementation that fell back to the controller when no card
  -- backs the object would answer wrongly here -- in opposite directions on the
  -- two tokens, which is why both are asserted.
  Spec.it s "CR 111.2 a token's owner is its creator, not whoever controls it now" $ do
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addCreature garland S.alice (Setup.emptyGame S.bothPlayers)
        (mine, g1) = S.addToken (Printing.card piker) S.alice g0
        (taken, g2) = S.addToken (Printing.card piker) S.bob g1
        gs = S.giveControl taken S.alice g2
    Spec.assertEqWith s "alice's own token stays 2/1" (S.powerToughnessOf mine gs) (Just (2, 1))
    Spec.assertEqWith s "bob's token under alice's control is 4/3" (S.powerToughnessOf taken gs) (Just (4, 3))
    -- And the prohibition follows the same owner: alice may sacrifice the token
    -- she made and not the one she took. (Garland is alice's own creature here,
    -- so it is on the offer too; the claim is about the two tokens.)
    Spec.assertBool s (elem mine (offered gs)) "the token alice created is offered"
    Spec.assertBool s (notElem taken (offered gs)) "and the one she took is not"

-- CR 101.2 through the OFFER: what Pawl.Engine.Replacement.sacrificeCandidates
-- answers, which is the CR 118.3 payability count and every sacrifice prompt.
offerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
offerSpec s registry = Spec.describe s "Offer" $ do
  -- Village Rites is {B} "As an additional cost to cast this spell, sacrifice a
  -- creature. Draw two cards." On the stolen-Garland board alice's own Piker is
  -- the ONLY legal victim, so CR 601.2h's choice is forced and the payment is
  -- made with no prompt -- which makes this an assertion about WHICH creature
  -- died rather than about an interpreter's answer.
  Spec.it s "CR 101.2 whole cards: Village Rites eats the Piker alice owns, never the two she stole" $ do
    swamp <- S.printingOf s registry "Swamp"
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    piker <- S.printingOf s registry "Goblin Piker"
    villageRites <- S.printingOf s registry "Village Rites"
    let (stolenGarland, g0) = S.addCreature garland S.bob (S.landsInPlay swamp 1)
        (stolenPiker, g1) = S.addCreature piker S.bob g0
        g2 = S.giveControl stolenPiker S.alice (S.giveControl stolenGarland S.alice g1)
        (hers, g3) = S.addCreature piker S.alice g2
        -- Stocked, so the draw has something to draw and CR 104.3c does not
        -- deck alice before the assertion runs.
        (_, g4) = S.addLibraryCard swamp S.alice g3
        (_, g5) = S.addLibraryCard swamp S.alice g4
        (gs, spell) = S.handOne villageRites g5
        cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "one creature alice owns is the whole offer" (offered gs) [hers]
    Spec.assertBool s (S.castable S.alice spell gs) "the additional cost is payable"
    Spec.assertBool s (not (S.onBattlefield hers resolved)) "alice's own Piker paid the cost"
    Spec.assertBool s (S.onBattlefield stolenPiker resolved) "the stolen Piker is still there"
    Spec.assertBool s (S.onBattlefield stolenGarland resolved) "and so is the stolen Garland"
    Spec.assertEqWith s "the spell resolved and drew two" (length (Game.zoneMembers Zone.Hand S.alice resolved)) 2
  -- CR 118.3's "fully", the half that would otherwise ship broken: a candidate
  -- list filtered only at the prompt still COUNTS the prohibited permanent when
  -- deciding payability, and the spell would announce as castable and then fail.
  --
  -- The pair is the discriminator. Both boards give alice the same Swamp and the
  -- same Village Rites, and they differ in exactly one thing: whether she also
  -- controls a creature she owns. So "not castable" cannot be passing because the
  -- spell was unaffordable.
  Spec.it s "CR 118.3 whole cards: with no creature she owns, alice cannot cast Village Rites at all" $ do
    swamp <- S.printingOf s registry "Swamp"
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    piker <- S.printingOf s registry "Goblin Piker"
    villageRites <- S.printingOf s registry "Village Rites"
    let (stolenGarland, g0) = S.addCreature garland S.bob (S.landsInPlay swamp 1)
        (stolenPiker, g1) = S.addCreature piker S.bob g0
        g2 = S.giveControl stolenPiker S.alice (S.giveControl stolenGarland S.alice g1)
        (barren, spell) = S.handOne villageRites g2
        (hers, withHers) = S.addCreature piker S.alice barren
    Spec.assertEqWith s "nothing alice controls may be sacrificed" (offered barren) []
    Spec.assertBool s (not (S.castable S.alice spell barren)) "so the cost cannot be paid"
    -- The paired positive, on the same mana: one creature alice owns is enough.
    Spec.assertEqWith s "her own creature is the one offer" (offered withHers) [hers]
    Spec.assertBool s (S.castable S.alice spell withHers) "and now the spell is castable"
    -- A stolen Garland is inside its own affected set, which is what the printed
    -- sentence says and what makes the board above possible at all.
    Spec.assertBool s (SacrificeRestriction.prohibited stolenGarland barren) "CR 101.2: even Garland itself can't be sacrificed once stolen"
  -- CR 701.21a's OTHER cost shape, which consults no candidate list at all:
  -- "Sacrifice this permanent" names the permanent paying. Ghitu Fire-Eater's
  -- ability is "{T}, Sacrifice this creature: it deals damage equal to its power
  -- to any target", and it costs no mana -- so a refused activation cannot be a
  -- refusal for want of mana.
  --
  -- Read at Pawl.Engine.Cost as well as at the funnel, because the two must
  -- agree: an ability offered as activatable and then unable to pay would leave
  -- the permanent alive with the ability spent.
  Spec.it s "CR 118.3 whole cards: a stolen Ghitu Fire-Eater cannot pay its own sacrifice cost" $ do
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    fireEater <- S.printingOf s registry "Ghitu Fire-Eater"
    let (_, g0) = S.addCreature garland S.alice (Setup.emptyGame S.bothPlayers)
        (stolen, g1) = S.addCreature fireEater S.bob g0
        (hers, g2) = S.addCreature fireEater S.alice g1
        gs = S.giveControl stolen S.alice g2
    Spec.assertBool s (not (activatable stolen gs)) "CR 101.2: the stolen one can't sacrifice itself"
    -- The paired positive on the SAME board: the identical card, differing only
    -- in who owns it, is activatable.
    Spec.assertBool s (activatable hers gs) "and alice's own copy can"
  -- CR 701.21a's edict, which chooses from what the VICTIM controls: the
  -- prohibition must remove the permanent from that choice too, or an edict
  -- against alice could be answered with a permanent that then does not die.
  Spec.it s "CR 609.3 an edict against alice takes the creature she owns and leaves the ones she does not" $ do
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    piker <- S.printingOf s registry "Goblin Piker"
    let (source, g0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (stolenGarland, g1) = S.addCreature garland S.bob g0
        (stolenPiker, g2) = S.addCreature piker S.bob g1
        g3 = S.giveControl stolenPiker S.alice (S.giveControl stolenGarland S.alice g2)
        (hers, gs) = S.addCreature piker S.alice g3
        edict =
          Resolve.applyEffect
            source
            source
            S.bob
            (Map.singleton victimSlot (Set.singleton (Recipient.ToPlayer S.alice)))
            (Map.singleton victimSlot (Set.singleton (Recipient.ToPlayer S.alice)))
            (Effect.PlayerSacrifices victimSlot anyCreature (Quantity.Literal 1))
        after = S.runPure S.identityAnswer gs edict
        barren = S.runPure S.identityAnswer g3 edict
    Spec.assertBool s (not (S.onBattlefield hers after)) "the creature alice owns is the one that dies"
    Spec.assertBool s (S.onBattlefield stolenPiker after) "the stolen Piker survives"
    Spec.assertBool s (S.onBattlefield stolenGarland after) "and so does the stolen Garland"
    -- Same effect, same seat, board minus the one legal victim: CR 609.3 does as
    -- much as it can, which is nothing.
    Spec.assertBool s (S.onBattlefield stolenPiker barren) "with nothing legal to give, the edict takes nothing"
    Spec.assertBool s (S.onBattlefield stolenGarland barren) "including Garland"

-- CR 101.2 through an INSTRUCTION that names its victim without consulting a
-- candidate list. This is the gate Pawl.Engine.Replacement.sacrificeCandidates
-- cannot supply: nothing here is offered, so nothing here can be filtered out of
-- an offer.
instructionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
instructionSpec s registry = Spec.describe s "Instruction" $ do
  -- Lightning Skelemental prints "At the beginning of the end step, sacrifice
  -- this creature" -- Effect.Sacrifice over the trigger's own source, straight
  -- into the CR 701.21 funnel. Stolen, it is inside Garland's set, so CR 101.2
  -- stops the sacrifice and CR 101.3 ignores the instruction: nothing moves, and
  -- no destruction is substituted.
  Spec.it s "CR 101.2 whole cards: a stolen Lightning Skelemental is not sacrificed at end of turn" $ do
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    skelemental <- S.printingOf s registry "Lightning Skelemental"
    let (_, g0) = S.addCreature garland S.alice (Setup.emptyGame S.bothPlayers)
        (stolen, g1) = S.addCreature skelemental S.bob g0
        gs = S.giveControl stolen S.alice g1
        after = endStepOf gs
    Spec.assertBool s (S.onBattlefield stolen after) "CR 101.3: the instruction is ignored"
    Spec.assertBool
      s
      (notElem (GameEvent.PermanentSacrificed S.alice stolen) (S.eventsOf after))
      "and no sacrifice was recorded"
  -- The control leg, and the one that proves the case above is about the
  -- prohibition rather than about a trigger that never fires: the SAME board with
  -- the Skelemental left under bob, whose own creature it is, sacrifices it.
  Spec.it s "CR 701.21a control: an unstolen Lightning Skelemental IS sacrificed at end of turn" $ do
    garland <- S.printingOf s registry "Garland, Royal Kidnapper"
    skelemental <- S.printingOf s registry "Lightning Skelemental"
    let (_, g0) = S.addCreature garland S.alice (Setup.emptyGame S.bothPlayers)
        (his, gs) = S.addCreature skelemental S.bob g0
        after = endStepOf gs
    Spec.assertBool s (not (S.onBattlefield his after)) "bob sacrifices his own creature"
    Spec.assertBool
      s
      (elem (GameEvent.PermanentSacrificed S.bob his) (S.eventsOf after))
      "and the sacrifice was recorded"

-- Can alice activate this permanent's sole activated ability right now? Taken
-- off the PROJECTION, the route Pawl.Engine.Activate itself reads.
activatable :: ObjectId.ObjectId -> GameState.GameState -> Bool
activatable oid gs = case Projection.abilitiesOf oid gs of
  ability : _ -> Activate.activatable S.alice oid ability gs
  [] -> False

-- Record the end step's beginning, place the triggers it gathers (CR 603.3), and
-- resolve the one on the stack. Every fixture above has exactly one such trigger,
-- so no ordering choice is made.
endStepOf :: GameState.GameState -> GameState.GameState
endStepOf gs =
  let began = S.withEvents [GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice] gs
      placed = S.runPure S.identityAnswer began Engine.settleForPriority
   in S.runPure S.identityAnswer placed Stack.resolveTop
