{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Mana over mana sources and the costs that reach them (CR 605, CR
-- 601.2f): several sources over one creature, the cards from Treasonous Ogre to
-- Quirion Sentinel, and interchangeable sources. Split out of Pawl.ManaSpec,
-- which keeps the machinery.
module Pawl.ManaSourceSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import Pawl.ManaSpec (alicePermanents, atLife, castFrom, isActivationOf, paysColors, poolSize, poolTypes, poolUnits, prefersSource, recordingManaSources, tapEverything, theAbility)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaOption as ManaOption
import qualified Pawl.Types.ManaRestriction as ManaRestriction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 118.3 on the supply side again, for a repeatable ability whose cost spends no
-- OBJECT. Treasonous Ogre ({3}{R} Creature -- Ogre Shaman, dethrone, "Pay 3 life:
-- Add {R}") is the pool's first: its cost holds no {T} for CR 107.5 to bar a
-- second activation and takes nothing out of a zone for the claims to count, so
-- what limits it is CR 119.4's life total and nothing else. Counting it once read a
-- cost only several activations could pay as unpayable, so the cast was never
-- offered (#1132).
--
-- The Ogre is the only mana source on every board below, so every mana comes
-- through it at 3 life apiece and no count here can be met another way. What
-- separates the halves of a case is ONE life, or ONE Ogre, and nothing else.
treasonousOgreSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
treasonousOgreSpec s registry = Spec.describe s "Treasonous Ogre" $ do
  -- ONE board for both halves, so what separates six red mana from seven is only
  -- how many activations CR 119.4 admits at 20 life: six, and not one and not
  -- seven.
  Spec.it s "CR 119.4 the life total is the ceiling" $ do
    board <- ogreBoard s registry 1 20
    Spec.assertBool s (paysColors (replicate 6 Color.Red) board) "six activations pay 18 of her 20 life"
    Spec.assertBool s (not (paysColors (replicate 7 Color.Red) board)) "and a seventh wants 21, which she does not have"

  -- What says the ceiling is arithmetic on the life total rather than some fixed
  -- number: one life is the whole difference between these two boards.
  Spec.it s "CR 119.4 three activations want nine life" $ do
    nine <- ogreBoard s registry 1 9
    eight <- ogreBoard s registry 1 8
    Spec.assertBool s (paysColors (replicate 3 Color.Red) nine) "9 life is three activations exactly"
    Spec.assertBool s (not (paysColors (replicate 3 Color.Red) eight)) "8 life is not"
    Spec.assertBool s (paysColors (replicate 2 Color.Red) eight) "though it is still two"

  -- CR 118.3's "fully" across two sources, the life half of #1126's object half: a
  -- second Ogre adds no life to spend, so the pair is worth no more mana than one.
  Spec.it s "CR 118.3 two Ogres share one life total" $ do
    board <- ogreBoard s registry 2 20
    Spec.assertBool s (paysColors (replicate 6 Color.Red) board) "six activations, however the two Ogres split them"
    Spec.assertBool s (not (paysColors (replicate 7 Color.Red) board)) "and not a seventh: what a second Ogre cannot buy is more life"

  -- The gameplay-level proof (design.md section 4). Hill Giant is {3}{R} with no
  -- abilities and targets nothing as it is cast, so the whole cast turns on the
  -- Ogre being counted four times -- and the life total afterwards is what says
  -- four activations happened rather than three or five.
  Spec.it s "CR 605.3a Hill Giant is cast off four activations of one Ogre" $ do
    hillGiant <- S.printingOf s registry "Hill Giant"
    rich <- ogreBoard s registry 1 20
    poor <- ogreBoard s registry 1 11
    let resolved = castFrom S.identityAnswer rich hillGiant
        short = castFrom S.identityAnswer poor hillGiant
        countOf name = S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack name) S.alice
    Spec.assertEqWith s "the Giant resolved" (countOf "Hill Giant" resolved) 1
    Spec.assertEqWith s "and four activations paid 12 life for it" (S.lifeOf S.alice resolved) (Just 8)
    Spec.assertEqWith s "at 11 life the fourth activation is unpayable, so the cast fails" (countOf "Hill Giant" short) 0
    -- CR 601.2h reverses the whole cast, so the three activations it did make are
    -- rolled back with it.
    Spec.assertEqWith s "and nothing was spent trying" (S.lifeOf S.alice short) (Just 11)

  -- The gate rather than the payment, and the pair that pins it to one life: at 12
  -- the fourth activation pays her to exactly 0, which CR 119.4 allows.
  Spec.it s "CR 118.3 the cast is offered at 12 life and not at 11" $ do
    hillGiant <- S.printingOf s registry "Hill Giant"
    twelve <- ogreBoard s registry 1 12
    eleven <- ogreBoard s registry 1 11
    let offered board =
          let (withSpell, oid) = S.handOne hillGiant board
           in any (S.isCastOf oid) (Action.legalActions S.alice withSpell)
    Spec.assertBool s (offered twelve) "12 life pays for four activations"
    Spec.assertBool s (not (offered eleven)) "11 pays for three, and {3}{R} wants four"

  -- The prompt-level half, since a board cannot say whether the window CLOSED
  -- early: the Ogre has to be offered a fourth time, with the cost still
  -- uncovered, for the fourth activation to happen at all. How many times to
  -- activate a repeatable source is the player's, and this is where they are asked.
  Spec.it s "CR 601.2g the mana window offers the Ogre once per activation" $ do
    hillGiant <- S.printingOf s registry "Hill Giant"
    board <- ogreBoard s registry 1 20
    let (withSpell, oid) = S.handOne hillGiant board
        offers = State.execState (Engine.runGame recordingManaSources withSpell (S.cast S.alice oid)) []
    Spec.assertEqWith s "asked four times, the Ogre the only candidate each time" (fmap length offers) [1, 1, 1, 1]

-- Alice at `life` life with `copies` Treasonous Ogres and nothing else.
ogreBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> Integer -> m GameState.GameState
ogreBoard s registry copies life = do
  ogre <- S.printingOf s registry "Treasonous Ogre"
  pure (atLife life (alicePermanents (replicate copies ogre)))

-- CR 118.3's "fully" across TWO mana sources. Ashnod's Altar ("Sacrifice a
-- creature: Add {C}{C}") and Phyrexian Tower ("{T}: Add {C}", "{T}, Sacrifice a
-- creature: Add {B}{B}") both buy their mana with a creature, and the supply
-- model asked each of them alone against the untouched board -- so one Goblin
-- Piker was counted as a victim twice and the pair read as four mana (#1126).
--
-- Goblin Piker makes no mana, so every mana on these boards comes through the
-- Altar or the Tower and the counts below cannot be met any other way. What
-- separates the two halves of each case is ONE Piker and nothing else.
sharedVictimSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sharedVictimSpec s registry = Spec.describe s "Two mana sources over one creature" $ do
  -- With one Piker the sacrifices are exclusive, so the best board is the
  -- Tower's free {C} beside the Altar's {C}{C}. Four wants both sacrifices.
  Spec.it s "CR 118.3 one creature cannot pay for both sacrifices" $ do
    board <- sharedVictimBoard s registry 1
    Spec.assertBool s (paysGeneric 3 board) "the Tower's {C} and one Altar activation pay {3}"
    Spec.assertBool s (not (paysGeneric 4 board)) "and nothing pays {4}"

  -- The same board with a second Piker, which is what says the refusal above is
  -- about the creature and not about the two sources.
  Spec.it s "CR 118.3 a second creature pays for the second sacrifice" $ do
    board <- sharedVictimBoard s registry 2
    Spec.assertBool s (paysGeneric 5 board) "the Tower's {C} and two Altar activations pay {5}"
    Spec.assertBool s (not (paysGeneric 6 board)) "and there is no third creature, so not {6}"

  -- The colours say WHICH activations a board is made of, where a generic count
  -- only says how many: {B}{B} is the Tower's sacrifice and {C} the Altar's, so
  -- this cost is payable exactly when both sacrifices are.
  Spec.it s "CR 118.3 {B}{B}{1} wants the Tower's sacrifice and the Altar's at once" $ do
    one <- sharedVictimBoard s registry 1
    two <- sharedVictimBoard s registry 2
    let cost = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Black), ManaSymbol.OfType (ManaType.Colored Color.Black), ManaSymbol.Generic 1]
        pays = Mana.canPay Cost.manaActivations S.alice cost
    Spec.assertBool s (not (pays one)) "one creature buys the {B}{B} or the {1}, not both"
    Spec.assertBool s (pays two) "two creatures buy both"

  -- The gameplay-level proof (design.md section 4), and it has to be the OFFER
  -- rather than the outcome: an overstated supply and a correct one both leave
  -- the Arbiter uncast, since the payment that follows a bad offer just fails
  -- (CR 601.2h). What the bug really did was menu a cast that could not be paid.
  Spec.it s "CR 601.2g a {4} spell is not offered off one creature" $ do
    arbiter <- S.printingOf s registry "Silent Arbiter"
    one <- sharedVictimBoard s registry 1
    two <- sharedVictimBoard s registry 2
    let offered board =
          let (withSpell, oid) = S.handOne arbiter board
           in any (S.isCastOf oid) (Action.legalActions S.alice withSpell)
    Spec.assertBool s (not (offered one)) "not offered"
    Spec.assertBool s (offered two) "offered once a second creature can pay for the second sacrifice"

  -- And the window itself, which no board can report: with one Piker the Altar
  -- and the Tower are each still a source on their own (CR 118.3 refuses neither
  -- alone), so both are offered -- and once one has taken the Piker the other is
  -- gone from the next offer.
  Spec.it s "CR 601.2g the window offers both sources, then neither" $ do
    board <- sharedVictimBoard s registry 1
    let cost = ManaCost.MkManaCost [ManaSymbol.Generic 4]
        offers = State.execState (Engine.runGame recordingManaSources board (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice cost)) []
    Spec.assertEqWith s "two candidates, then the one the sacrifice did not spend" (fmap length offers) [2, 1]

-- Alice's Ashnod's Altar, her Phyrexian Tower and `victims` Goblin Pikers.
sharedVictimBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m GameState.GameState
sharedVictimBoard s registry victims = do
  altar <- S.printingOf s registry "Ashnod's Altar"
  tower <- S.printingOf s registry "Phyrexian Tower"
  piker <- S.printingOf s registry "Goblin Piker"
  pure (foldr (\p gs -> snd (S.addCreature p S.alice gs)) (Setup.emptyGame S.bothPlayers) (altar : tower : replicate victims piker))

-- Whether alice could pay {n} off this board.
paysGeneric :: Natural -> GameState.GameState -> Bool
paysGeneric n = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic n])

-- CR 118.3's "fully" over the UNTAPPED permanents, which is sharedVictimSpec's
-- question on the other axis (Pawl.Types.ClaimAxis). Two Springleaf Drums ("{T},
-- Tap an untapped creature you control: Add one mana of any color") beside ONE
-- untapped creature are one mana and not two: whichever Drum is activated first
-- taps the creature, and the other one's cost then has no candidate. Tapping
-- stated no claim, so the supply model counted the creature twice and menued a
-- two-mana cast (#1718).
--
-- Goblin Piker makes no mana, so every mana on these boards comes through a Drum.
-- What separates the halves of each case is ONE Piker and nothing else. Neither
-- Drum is a creature, so CR 302.6 gates nothing here and S.addCreature settles
-- every permanent besides.
sharedTapSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sharedTapSpec s registry = Spec.describe s "Two tapping costs over one creature" $ do
  Spec.it s "CR 118.3 one creature cannot pay for both Drums" $ do
    one <- sharedTapBoard s registry 1
    two <- sharedTapBoard s registry 2
    -- The POSITIVES first: a refusal that came from the Drums not being sources at
    -- all would fail here, which is what keeps the negatives from passing
    -- vacuously.
    Spec.assertBool s (paysGeneric 1 one) "one creature buys one Drum's mana"
    Spec.assertBool s (paysGeneric 2 two) "a second creature buys the second Drum's"
    Spec.assertBool s (not (paysGeneric 2 one)) "and one creature does not buy both"
    Spec.assertBool s (not (paysGeneric 3 two)) "nor do two creatures make a third mana"

  -- The gameplay-level proof (design.md section 4), and it has to be the OFFER
  -- rather than the outcome: sharedVictimSpec's argument unchanged -- a payment
  -- that follows a bad offer just fails at CR 601.2h, so what the bug did was menu
  -- a cast that could not be paid. Mindcrank is a plain {2} artifact.
  Spec.it s "CR 601.2g a {2} spell is not offered off one creature" $ do
    mindcrank <- S.printingOf s registry "Mindcrank"
    one <- sharedTapBoard s registry 1
    two <- sharedTapBoard s registry 2
    Spec.assertBool s (not (offersCast mindcrank one)) "not offered"
    Spec.assertBool s (offersCast mindcrank two) "offered once a second creature can pay the second Drum"

  -- The AXES must not cross, which is the whole reason the tapping claim is not a
  -- claim on Zone.Battlefield. Village Rites ("{B}", "As an additional cost to
  -- cast this spell, sacrifice a creature") beside one Drum and one Piker is
  -- legal: CR 601.2g's window taps the Piker for the {B}, and CR 601.2h's payment
  -- then sacrifices the tapped Piker. Keying the tap claim as a removal from the
  -- battlefield merges the two pools and refuses this cast.
  Spec.it s "CR 601.2h a creature tapped for mana can still be sacrificed" $ do
    rites <- S.printingOf s registry "Village Rites"
    drum <- S.printingOf s registry "Springleaf Drum"
    piker <- S.printingOf s registry "Goblin Piker"
    Spec.assertBool s (offersCast rites (alicePermanents [drum, piker])) "offered"

-- Alice's two Springleaf Drums and `creatures` Goblin Pikers.
sharedTapBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m GameState.GameState
sharedTapBoard s registry creatures = do
  drum <- S.printingOf s registry "Springleaf Drum"
  piker <- S.printingOf s registry "Goblin Piker"
  pure (alicePermanents (drum : drum : replicate creatures piker))

-- CR 118.3's "fully" across a SELF-tap and an OTHER-tap, which is sharedTapSpec's
-- question with one of the two Drums replaced by the creature's own {T}.
-- Springleaf Drum beside ONE Llanowar Elves ("{T}: Add {G}") is one mana and not
-- two: the Drum's payment taps the Elves, or the Elves taps itself, never both.
-- CR 107.5's {T} stated no claim, so the Elves was counted twice; see #1725.
--
-- The Drum's own {T} claims too, on a pool of one -- itself -- that no other
-- claim meets, so it neither pays nor blocks. What separates the halves of each
-- case is ONE Elves and nothing else. S.addCreature settles what it places, so CR
-- 302.6 does not gate the Elves' own {T}; the "one Elves makes one" positive is
-- what would fail if it did.
selfTapSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfTapSpec s registry = Spec.describe s "A self-tap and an other-tap over one creature" $ do
  Spec.it s "CR 118.3 a Drum and one Elves make one mana, not two" $ do
    drum <- S.printingOf s registry "Springleaf Drum"
    elves <- S.printingOf s registry "Llanowar Elves"
    let one = alicePermanents [drum, elves]
        two = alicePermanents [drum, elves, elves]
    -- The POSITIVES first: 1/2 and 2/3 are the two independent readings, and
    -- collapsing them into one assertion would let a numeric coincidence pass.
    Spec.assertBool s (paysGeneric 1 one) "the pair makes one mana"
    Spec.assertBool s (paysGeneric 2 two) "a second Elves makes the second mana"
    Spec.assertBool s (not (paysGeneric 2 one)) "and one Elves does not make two"
    Spec.assertBool s (not (paysGeneric 3 two)) "nor two Elves a third"

  -- The pool is the SOURCE ALONE, which the pair above cannot tell from a wider
  -- one: on a board of one Drum and n Elves a {T} claiming the whole battlefield
  -- admits exactly the same totals, since declining one Elf relieves the
  -- overcount and the Drum eats an Elf either way. TWO Drums and one Elves
  -- separate the readings -- the truth is one mana, whichever of the three is
  -- activated, and a battlefield-wide {T} reads two.
  Spec.it s "CR 118.3 two Drums and one Elves still make one mana" $ do
    drum <- S.printingOf s registry "Springleaf Drum"
    elves <- S.printingOf s registry "Llanowar Elves"
    let board = alicePermanents [drum, drum, elves]
    Spec.assertBool s (paysGeneric 1 board) "one of the three makes one"
    Spec.assertBool s (not (paysGeneric 2 board)) "and no two of them make two"

  -- The OFFER, for sharedTapSpec's reason. Mindcrank is the plain {2} artifact.
  Spec.it s "CR 601.2g a {2} spell is not offered off one Elves" $ do
    mindcrank <- S.printingOf s registry "Mindcrank"
    drum <- S.printingOf s registry "Springleaf Drum"
    elves <- S.printingOf s registry "Llanowar Elves"
    Spec.assertBool s (offersCast mindcrank (alicePermanents [drum, elves, elves])) "offered off two"
    Spec.assertBool s (not (offersCast mindcrank (alicePermanents [drum, elves]))) "not off one"

-- CR 118.3's "fully" across the tapping components of ONE cost, which is
-- sharedTapSpec's question moved inside a single ability. Synthetic Crewed
-- Battery ("{T}, Tap an untapped creature you control, Tap any number of
-- untapped creatures you control with total power 2 or greater: Add {C}") beside
-- ONE Goblin Piker makes no mana: the counted tap and CR 702.122a's threshold tap
-- both want an untapped creature, and one creature pays one of them. The
-- threshold half stated no claim, so the two were counted against the same Piker
-- twice and the cost read as payable; see #1744.
--
-- SYNTHETIC because the shape it needs is one COST carrying a threshold tap
-- beside another tapping component, and the printed producers of a threshold tap
-- are crew (CR 702.122a; `data/cards/consulate-dreadnought.json` is the only one
-- of those in `data/cards/`) and Mossbridge Troll, in each of which it is the
-- whole of its ability's cost, so nothing contends with it. Scryfall
-- `oracle:"total power" oracle:tap`, 2026-08-18, eight cards -- a printing whose
-- one cost taps for a total power AND taps something counted is what would
-- refute this and let the synthetic go.
--
-- The Battery's own {T} is a third claim, on a pool of one -- itself -- that no
-- other claim meets, so it neither pays nor blocks these two.
crewedBatterySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
crewedBatterySpec s registry = Spec.describe s "A threshold tap and a counted tap in one cost" $ do
  Spec.it s "CR 118.3 one creature cannot pay both tapping components" $ do
    one <- crewedBatteryBoard s registry 1
    two <- crewedBatteryBoard s registry 2
    -- The POSITIVE first, sharedTapSpec's posture: a refusal that came from the
    -- Battery not being a source at all would fail here.
    Spec.assertBool s (paysGeneric 1 two) "two creatures buy the Battery's mana"
    Spec.assertBool s (not (paysGeneric 1 one)) "and one creature does not"

  -- The OFFER, for sharedTapSpec's reason: a payment that follows a bad offer
  -- just fails at CR 601.2h, so what the bug does is menu an uncastable spell.
  -- Springleaf Drum is the pool's plain {1} artifact, and it is cast from HAND,
  -- so it adds nothing to either board.
  Spec.it s "CR 601.2g a {1} spell is not offered off one creature" $ do
    drum <- S.printingOf s registry "Springleaf Drum"
    one <- crewedBatteryBoard s registry 1
    two <- crewedBatteryBoard s registry 2
    Spec.assertBool s (offersCast drum two) "offered off two creatures"
    Spec.assertBool s (not (offersCast drum one)) "not off one"

-- Alice's Synthetic Crewed Battery and `creatures` Goblin Pikers. What separates
-- the halves of each case above is ONE Piker and nothing else; the Battery is no
-- creature, so CR 302.6 gates nothing and S.addCreature settles the Pikers.
crewedBatteryBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m GameState.GameState
crewedBatteryBoard s registry creatures = do
  battery <- S.printingOf s registry "Synthetic Crewed Battery"
  piker <- S.printingOf s registry "Goblin Piker"
  pure (alicePermanents (battery : replicate creatures piker))

-- CR 118.3's "fully" across a cost's OWN components and its mana SOURCES, which
-- is the same rule one level up from sharedVictimSpec. Village Rites ("{B}", "As
-- an additional cost to cast this spell, sacrifice a creature") beside Phyrexian
-- Tower and one Goblin Piker: the Piker buys the {B} or pays the additional cost,
-- never both, because CR 601.2g's mana window comes before CR 601.2h's payment
-- and both sacrifices happen. The two halves were asked apart, so the cast was
-- offered and the payment then rolled back (#1134).
villageRitesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
villageRitesSpec s registry = Spec.describe s "A cost's own sacrifice and its source's" $ do
  -- The OFFER and not the outcome: an overstated supply and a correct one both
  -- leave the spell uncast, since the payment after a bad offer just fails. What
  -- the bug did was menu a cast that could not be paid.
  Spec.it s "CR 118.3 one creature cannot pay the {B} and the additional cost" $ do
    rites <- S.printingOf s registry "Village Rites"
    tower <- S.printingOf s registry "Phyrexian Tower"
    piker <- S.printingOf s registry "Goblin Piker"
    let board victims = alicePermanents (tower : replicate victims piker)
    Spec.assertBool s (not (offersCast rites (board 1))) "one Piker is not enough"
    Spec.assertBool s (offersCast rites (board 2)) "a second one pays for the second sacrifice"

  -- The same ONE Piker, with the {B} coming from a Swamp instead: nothing now
  -- contends for it, so the refusal above is about the contention and not about a
  -- creature too few. The Tower stays on the board, so what changes is only that
  -- the mana comes from a source claiming ITSELF (CR 107.5's {T}) rather than the
  -- Piker -- a different pool, which the Rites' sacrifice never meets.
  Spec.it s "CR 601.2g a Swamp frees the creature for the additional cost" $ do
    rites <- S.printingOf s registry "Village Rites"
    tower <- S.printingOf s registry "Phyrexian Tower"
    piker <- S.printingOf s registry "Goblin Piker"
    swamp <- S.printingOf s registry "Swamp"
    Spec.assertBool s (offersCast rites (alicePermanents [tower, piker, swamp])) "offered"

  -- The same question at Cost.canPay, which is the gate CR 118.3 is asked at for
  -- an unlock, a morph's turn-up and an "unless that player pays" -- none of them
  -- reachable with an instant, so it is asked of the Rites' own cost directly.
  -- Read through costsFor so the cost is the one the engine would use.
  Spec.it s "CR 118.3 Cost.canPay asks it too" $ do
    rites <- S.printingOf s registry "Village Rites"
    tower <- S.printingOf s registry "Phyrexian Tower"
    piker <- S.printingOf s registry "Goblin Piker"
    let pays victims =
          let (gs, oid) = S.handOne rites (alicePermanents (tower : replicate victims piker))
           in any (\cost -> Cost.canPay S.alice oid cost gs) (Cost.costsFor S.alice (S.printingName rites) oid gs)
    Spec.assertBool s (not (pays 1)) "one Piker is not enough"
    Spec.assertBool s (pays 2) "two are"

-- CR 605.1a's FOURTH clause, which the 2026-08-07 rules added: an activated
-- ability is a mana ability only if "its cost and effect don't move any card to
-- or from a library". Chromatic Sphere and Chromatic Star are the pair that
-- clause splits, and they were chosen because they differ in one thing and
-- nothing else. Both are {1} artifacts whose only activated ability is
-- "{1}, {T}, Sacrifice this artifact: Add one mana of any color" -- same cost,
-- same production, same seat. The Sphere's ability goes on to say "Draw a card";
-- the Star's draw is a SEPARATE triggered ability off its own death (CR 603),
-- which CR 605.1a never reads. So the Sphere stops being a mana ability under the
-- new clause and the Star does not.
--
-- The Sphere's negative is never routed through Activate.activatable, which
-- answers False for a mana ability on every board (CR 605.3b): every assertion
-- here reads the menu Action.legalActions builds, or the board a priority loop
-- leaves behind.
chromaticSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
chromaticSpec s registry = Spec.describe s "Chromatic Sphere and Chromatic Star" $ do
  Spec.it s "CR 605.1a the Star's ability is a mana ability and the Sphere's draw disqualifies its own" $ do
    star <- S.printingOf s registry "Chromatic Star"
    sphere <- S.printingOf s registry "Chromatic Sphere"
    forest <- S.printingOf s registry "Forest"
    let (starId, starBoard) = chromaticBoard star forest
        (sphereId, sphereBoard) = chromaticBoard sphere forest
    Spec.assertBool s (elem starId (Mana.manaSources Cost.manaActivations S.alice starBoard)) "the Star is a mana source"
    Spec.assertBool s (notElem sphereId (Mana.manaSources Cost.manaActivations S.alice sphereBoard)) "the Sphere is not"

  -- The same split at the menu, which is what the priority loop actually reads.
  -- Both directions are asserted on both boards, so neither half can pass because
  -- the artifact was simply unaffordable: the Sphere IS offered, just under the
  -- other constructor -- CR 605.3b's stack.
  Spec.it s "CR 605.3b the Sphere is offered as an ordinary activation instead" $ do
    star <- S.printingOf s registry "Chromatic Star"
    sphere <- S.printingOf s registry "Chromatic Sphere"
    forest <- S.printingOf s registry "Forest"
    let (starId, starBoard) = chromaticBoard star forest
        (sphereId, sphereBoard) = chromaticBoard sphere forest
        starActions = Action.legalActions S.alice starBoard
        sphereActions = Action.legalActions S.alice sphereBoard
    Spec.assertBool s (elem (Action.Type.ActivateManaAbility starId) starActions) "the Star is menued as a mana ability"
    Spec.assertBool s (notElem (Action.Type.ActivateManaAbility sphereId) sphereActions) "the Sphere is not"
    Spec.assertBool s (any (isActivateOf sphereId) sphereActions) "the Sphere is menued as an ordinary activation"
    Spec.assertBool s (not (any (isActivateOf starId) starActions)) "which a mana ability never is"

  -- End to end. tapEverything takes ONLY mana activations, so what it reaches is
  -- exactly CR 605.3a's window: the Star is sacrificed to its own cost and its
  -- death trigger draws, while the Sphere is never touched at all and its draw
  -- never happens.
  Spec.it s "CR 605.3a the mana window reaches the Star and never the Sphere" $ do
    star <- S.printingOf s registry "Chromatic Star"
    sphere <- S.printingOf s registry "Chromatic Sphere"
    forest <- S.printingOf s registry "Forest"
    let (_, starBoard) = chromaticBoard star forest
        (_, sphereBoard) = chromaticBoard sphere forest
        starAfter = S.runPure tapEverything starBoard Engine.priorityLoop
        sphereAfter = S.runPure tapEverything sphereBoard Engine.priorityLoop
    Spec.assertEqWith s "the Star paid its own sacrifice" (length (Game.zoneMembers Zone.Graveyard S.alice starAfter)) 1
    Spec.assertEqWith s "the Sphere was never activated" (length (Game.zoneMembers Zone.Graveyard S.alice sphereAfter)) 0
    Spec.assertEqWith s "the Star's death trigger drew one card" (length (Game.zoneMembers Zone.Hand S.alice starAfter)) 1
    Spec.assertEqWith s "and nothing drew on the Sphere's board" (length (Game.zoneMembers Zone.Hand S.alice sphereAfter)) 0

-- alice, active, in her precombat main phase: one of the two artifacts, a Forest
-- to pay the {1} activation cost with, and a stocked library so a draw has a card
-- to take and CR 104.3c decides nothing first. Identical for both artifacts, so
-- the printing is the only difference between the two boards.
chromaticBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
chromaticBoard artifact forest =
  let base = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}
      (artifactId, g1) = S.addCreature artifact S.alice base
      (_, g2) = S.addCreature forest S.alice g1
      stocked = foldr (\p gs -> snd (S.addLibraryCard p S.alice gs)) g2 (replicate 3 forest)
   in (artifactId, stocked)

-- Whether an action is the ordinary CR 602 activation of this object's ability,
-- which is the constructor Action.legalActions uses for everything that is NOT a
-- mana ability.
isActivateOf :: ObjectId.ObjectId -> Action.Type.Action -> Bool
isActivateOf oid action = case action of
  Action.Type.Activate other _ -> other == oid
  Action.Type.ActivateManaAbility _ -> False
  Action.Type.Cast {} -> False
  Action.Type.Play {} -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
  Action.Type.Pass -> False

-- CR 605.1a's library clause read of the COST half, which chromaticSpec above
-- reads of the effect half. Millikin ({2} Artifact Creature -- Construct 0/1,
-- Oracle text checked against Scryfall: "{T}, Mill a card: Add {C}. (Activate
-- only as an instant. To mill a card, put the top card of your library into your
-- graveyard.)") is the printing: it adds mana, targets nothing and is no loyalty
-- ability, so every other criterion in rule 605.1a is met and its COST is the
-- whole of why it is not a mana ability. The parenthesis is reminder text --
-- "activate only as an instant" is the CONSEQUENCE of the clause and not a
-- printed rider, so the card authors no ActivationRestriction -- which the
-- characteristics case below asserts, since no board here would tell an
-- instant-speed rider from its absence.
--
-- Sol Ring ({1} Artifact, "{T}: Add {C}{C}") is the control on every board here:
-- the same {T}, the same colourless production, and no mill. It is what says a
-- Millikin assertion below fails on the mill rather than on the board.
millikinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
millikinSpec s registry = Spec.describe s "Millikin" $ do
  Spec.it s "Millikin is a {2} 0/1 Artifact Creature whose one ability mills as a cost" $ do
    millikin <- S.printingOf s registry "Millikin"
    Spec.assertEqWith s "name" (Face.name (S.combinedFace millikin)) (CardName.MkCardName $ Text.pack "Millikin")
    Spec.assertEqWith s "one activated ability" (length (Face.activatedAbilities (S.combinedFace millikin))) 1
    Spec.assertEqWith
      s
      "CR 601.2f its cost is the tap symbol and a one-card mill"
      (Cost.Type.components (ActivatedAbility.cost (theAbility millikin)))
      [CostComponent.TapThis, CostComponent.MillCards 1]
    Spec.assertEqWith s "CR 602.5 and it prints no rider, the reminder text being CR 605.1a's consequence" (length (ActivatedAbility.restrictions (theAbility millikin))) 0

  Spec.it s "CR 605.1a the mill in the cost keeps Millikin off the mana sources" $ do
    millikin <- S.printingOf s registry "Millikin"
    solRing <- S.printingOf s registry "Sol Ring"
    let (millikinId, solRingId, board) = millikinBoard millikin solRing
        sources = Mana.manaSources Cost.manaActivations S.alice board
    Spec.assertBool s (elem solRingId sources) "the Sol Ring is a mana source"
    Spec.assertBool s (notElem millikinId sources) "and Millikin, whose cost mills, is not"

  -- The same split at the menu, which is what the priority loop actually reads.
  -- Both directions on one board, so neither half can pass because Millikin was
  -- simply unaffordable: it IS offered, under the other constructor.
  Spec.it s "CR 605.3b Millikin is offered as an ordinary activation instead" $ do
    millikin <- S.printingOf s registry "Millikin"
    solRing <- S.printingOf s registry "Sol Ring"
    let (millikinId, solRingId, board) = millikinBoard millikin solRing
        actions = Action.legalActions S.alice board
    Spec.assertBool s (elem (Action.Type.ActivateManaAbility solRingId) actions) "the Sol Ring is menued as a mana ability"
    Spec.assertBool s (notElem (Action.Type.ActivateManaAbility millikinId) actions) "Millikin is not"
    Spec.assertBool s (any (isActivateOf millikinId) actions) "Millikin is menued as an ordinary activation"
    Spec.assertBool s (not (any (isActivateOf solRingId) actions)) "which a mana ability never is"

  -- End to end, and the gameplay-level proof (design.md section 4).
  -- tapEverything takes ONLY mana activations, so what it reaches is exactly CR
  -- 605.3a's window: the Sol Ring is tapped for {C}{C} and Millikin is never
  -- touched, so no card is milled and it stays untapped. Under the other reading
  -- of rule 605.1a the window takes Millikin too, and the graveyard says so.
  Spec.it s "CR 605.3a the mana window reaches the Sol Ring and never Millikin" $ do
    millikin <- S.printingOf s registry "Millikin"
    solRing <- S.printingOf s registry "Sol Ring"
    let (millikinId, solRingId, board) = millikinBoard millikin solRing
        after = S.runPure tapEverything board Engine.priorityLoop
    Spec.assertEqWith s "CR 701.17a nothing was milled" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    Spec.assertEqWith s "and Millikin's library is whole" (length (Game.zoneMembers Zone.Library S.alice after)) 3
    Spec.assertEqWith s "Millikin is untapped, never having paid its cost" (fmap Object.tapped (Game.lookupObject millikinId after)) (Just TapState.Untapped)
    Spec.assertEqWith s "the Sol Ring did pay its own" (fmap Object.tapped (Game.lookupObject solRingId after)) (Just TapState.Tapped)
    Spec.assertEqWith s "so the window ran and floated {C}{C}" (poolTypes S.alice after) [ManaType.Colorless, ManaType.Colorless]

  -- The card in its own right: CR 605.3b puts it on the stack, CR 602.2b pays
  -- its cost as it is activated, and the mana arrives only on resolution.
  Spec.it s "CR 602.2b activating Millikin pays the mill up front and adds {C} on resolution" $ do
    millikin <- S.printingOf s registry "Millikin"
    solRing <- S.printingOf s registry "Sol Ring"
    let (millikinId, _, board) = millikinBoard millikin solRing
        activated = S.runPure S.identityAnswer board (Activate.activateAbility S.alice millikinId (theAbility millikin))
        resolved = S.runPure S.identityAnswer activated Stack.resolveTop
    Spec.assertEqWith s "CR 605.3b the ability is on the stack" (length (GameState.stack activated)) 1
    Spec.assertEqWith s "CR 601.2h nothing is in the pool yet" (poolTypes S.alice activated) []
    Spec.assertEqWith s "CR 701.17a the top card of the library is in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice activated)) 1
    Spec.assertEqWith s "which is one card off the library" (length (Game.zoneMembers Zone.Library S.alice activated)) 2
    Spec.assertEqWith s "CR 107.5 and Millikin is tapped" (fmap Object.tapped (Game.lookupObject millikinId activated)) (Just TapState.Tapped)
    Spec.assertEqWith s "the resolution adds {C}" (poolTypes S.alice resolved) [ManaType.Colorless]

-- alice, active, in her precombat main phase, with Millikin and a Sol Ring on
-- the battlefield -- both settled and untapped -- and three cards in her library
-- for a mill to take. Nothing else makes mana, so every mana on this board comes
-- through one of the two.
millikinBoard :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
millikinBoard millikin solRing =
  let base = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.remaining = Seq.empty}
      (millikinId, g1) = S.addCreature millikin S.alice base
      (solRingId, g2) = S.addCreature solRing S.alice g1
      stocked = foldr (\p gs -> snd (S.addLibraryCard p S.alice gs)) g2 (replicate 3 solRing)
   in (millikinId, solRingId, stocked)

-- Whether alice is offered the cast of one card of this printing from her hand.
offersCast :: Printing.Printing -> GameState.GameState -> Bool
offersCast printing board =
  let (withSpell, oid) = S.handOne printing board
   in any (S.isCastOf oid) (Action.legalActions S.alice withSpell)

-- CR 605.1b: a TRIGGERED ability is a mana ability only when it triggers from a
-- mana ability's activation or resolution, or from mana being added to a pool.
-- Burning-Tree Emissary ({R/G}{R/G} 2/2 Human Shaman, "When this creature
-- enters, add {R}{G}") triggers off neither, so its ability is no mana ability:
-- CR 603.3 puts it on the stack and its mana is added when it RESOLVES, through
-- Pawl.Engine.Resolve's Effect.AddMana arm rather than Cost.tapForMana.
--
-- Here rather than in ResolveSpec for the reason the mana window is: the
-- subsystem is mana, and this is the second of the two places mana reaches a
-- pool.
burningTreeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
burningTreeSpec s registry = Spec.describe s "Burning-Tree Emissary" $ do
  Spec.it s "CR 605.1b the enters trigger resolves off the stack and adds {R}{G}" $ do
    bte <- S.printingOf s registry "Burning-Tree Emissary"
    let (_, after) = burningTreeResolved bte S.alice
    Spec.assertEqWith s "alice's pool is {R} then {G}, in printed order (CR 608.2c)" (poolUnits after) [plainRed, plainGreen]
    Spec.assertEqWith s "the stack is empty again" (length (GameState.stack after)) 0

  -- CR 109.5: "add {R}{G}" says "you", which for a triggered ability is the
  -- controller of the object when it triggered -- not the same seat as the active
  -- player. alice is active and holds priority on both boards, and the Emissary's
  -- controller is the only difference between them.
  Spec.it s "CR 109.5 the mana goes to the ability's controller, not the active player" $ do
    bte <- S.printingOf s registry "Burning-Tree Emissary"
    let (_, alices) = burningTreeResolved bte S.alice
        (_, bobs) = burningTreeResolved bte S.bob
    Spec.assertEqWith s "alice's Emissary pays alice" (poolSize S.alice alices, poolSize S.bob alices) (2, 0)
    Spec.assertEqWith s "bob's Emissary pays bob" (poolSize S.alice bobs, poolSize S.bob bobs) (0, 2)

  -- Gameplay level: the floating {R}{G} is ordinary mana, so it pays for a second
  -- Emissary ({R/G}{R/G}) off a board holding no land and no other mana source.
  -- The negative board differs in ONE thing -- the Emissary was arranged onto the
  -- battlefield rather than entering -- so no trigger fired, nothing was added,
  -- and the cast is not offered.
  Spec.it s "CR 106.4 the added mana pays for a second Emissary, and without it the cast is not offered" $ do
    bte <- S.printingOf s registry "Burning-Tree Emissary"
    let (handId, after) = burningTreeResolved bte S.alice
        (noTriggerHandId, noTrigger) = burningTreeArranged bte S.alice
        cast = snd (Engine.runGamePure S.identityAnswer after (S.cast S.alice handId))
    Spec.assertBool s (any (S.isCastOf handId) (Action.legalActions S.alice after)) "the second Emissary is castable off the trigger's mana"
    Spec.assertBool s (not (any (S.isCastOf noTriggerHandId) (Action.legalActions S.alice noTrigger))) "and is not castable when no trigger added any"
    Spec.assertEqWith s "the cast spent the whole pool" (poolSize S.alice cast) 0
    Spec.assertEqWith s "and the second Emissary is on the stack" (length (GameState.stack cast)) 1

-- One Burning-Tree Emissary entering under `pid` with its CR 603.6a enters event,
-- that trigger placed and resolved, plus a second copy in alice's hand -- alice
-- being active with priority in her precombat main phase (S.handOne). No land and
-- no other mana source is on the board, so the only mana anywhere is what the
-- trigger added.
burningTreeResolved :: Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
burningTreeResolved bte pid =
  let (base, handId) = S.handOne bte (Setup.emptyGame S.bothPlayers)
      (_, entered) = S.entersWithTrigger bte pid base
      placed = snd (Engine.runGamePure S.identityAnswer entered Engine.placePendingTriggers)
   in (handId, snd (Engine.runGamePure S.identityAnswer placed Stack.resolveTop))

-- The same board with the Emissary ARRANGED onto the battlefield instead of
-- entering (S.addCreature emits no event), so nothing triggers and no mana is
-- added. Everything else -- seats, phase, priority, the copy in hand, the empty
-- stack -- is burningTreeResolved's.
burningTreeArranged :: Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
burningTreeArranged bte pid =
  let (base, handId) = S.handOne bte (Setup.emptyGame S.bothPlayers)
   in (handId, snd (S.addCreature bte pid base))

-- One green mana with no production tags, plainRed's twin: what the Emissary's
-- trigger adds alongside it.
plainGreen :: ManaUnit.ManaUnit
plainGreen = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Green, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}

-- CR 608.2d's OTHER reading of "choose": a scope that admits the CONTROLLER.
-- Stadium Vendors ({3}{R} 3/3 Creature -- Goblin, "When this creature enters,
-- choose a player. That player adds two mana of any one color they choose")
-- (name, cost, type line, power, toughness and Oracle text checked against
-- api.scryfall.com, 2026-09-04) is the printing, and the whole card is
-- transcribed. Skullwinder's "choose an opponent" is the same opcode one
-- PlayerScope over, and Pawl.MassEffectSpec's Skullwinder group is what holds
-- that arm honest.
--
-- THREE SEATS, because two collapse "a player" onto "an opponent" the moment
-- the controller is picked out: at two seats the pair below could not tell
-- "alice was offered" from "alice was the only candidate left".
--
-- "any ONE color" is the ManaAddition count rather than two AddMana effects: CR
-- 105.4's choice is made once for the instruction, so both units take the
-- colour the ANSWER settled. The colour answerer keys on the seat the
-- ChooseManaType prompt names -- alice red, bob green, carol blue -- so the
-- pool says which seat was instructed as well as how much they got.
stadiumVendorsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stadiumVendorsSpec s registry =
  let -- Pinned to `who` by NAME out of the offer rather than by index, and
      -- FILTERED rather than invented: a scope that never offered them takes the
      -- first candidate instead, so the pool below reads the engine's answer.
      answering :: PlayerId.PlayerId -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
      answering who p = case p of
        Prompt.ChoosePlayer _ _ _ offer -> do
          State.modify' (<> NonEmpty.toList offer)
          pure (Maybe.fromMaybe (NonEmpty.head offer) (List.find (== who) (NonEmpty.toList offer)))
        Prompt.ChooseManaType _ asked _ _
          | asked == S.alice -> pure (ManaType.Colored Color.Red)
          | asked == S.bob -> pure (ManaType.Colored Color.Green)
          | otherwise -> pure (ManaType.Colored Color.Blue)
        _ -> pure (S.identityAnswer p)
      -- alice's Vendors entering with its CR 603.6a event, that trigger placed,
      -- and a Goblin Piker ({1}{R}) in her hand with her active and holding
      -- priority in her own precombat main phase. No land and no other mana
      -- source is anywhere, so the trigger's two mana are the only mana there is.
      arranged vendors piker =
        let (base, pikerId) = S.handOne piker S.threePlayerGame
            (_, entered) = S.entersWithTrigger vendors S.alice base
         in (pikerId, snd (Engine.runGamePure S.identityAnswer entered Engine.placePendingTriggers))
      resolved who (pikerId, placed) =
        let ((_, after), offer) = State.runState (Engine.runGame (answering who) placed Stack.resolveTop) []
         in (pikerId, after, offer)
   in Spec.describe s "Stadium Vendors" $ do
        -- The headline, and the reading "choose an opponent" cannot express:
        -- alice is in the offer and takes it herself.
        Spec.it s "CR 608.2d the controller is offered, and the two mana land in her own pool" $ do
          vendors <- S.printingOf s registry "Stadium Vendors"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, after, offer) = resolved S.alice (arranged vendors piker)
          Spec.assertBool s (any (S.isCastOf pikerId) (Action.legalActions S.alice after)) "alice can cast a {1}{R} Piker, which one mana could not pay for"
          Spec.assertEqWith s "and her pool is {R}{R}, one CR 105.4 choice covering both" (poolOf S.alice after) [plainRed, plainRed]
          Spec.assertEqWith s "neither opponent got anything" (poolOf S.bob after, poolOf S.carol after) ([], [])
          Spec.assertEqWith s "every seat still in the game was offered, the chooser included" (List.sort offer) (List.sort [S.alice, S.bob, S.carol])
          Spec.assertEqWith s "the stack is empty again" (length (GameState.stack after)) 0

        -- The pair's other leg, differing in EXACTLY one thing: which player
        -- alice names. carol answers the colour prompt, which is what "they
        -- choose" says -- a board that asked the controller would float red.
        Spec.it s "CR 106.3 naming an opponent instructs THEM, and they pick the colour" $ do
          vendors <- S.printingOf s registry "Stadium Vendors"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, after, _) = resolved S.carol (arranged vendors piker)
          Spec.assertEqWith s "carol got {U}{U}, the colour SHE named" (poolOf S.carol after) [plainBlue, plainBlue]
          Spec.assertEqWith s "alice, who controls the Goblin, got nothing" (poolOf S.alice after) []
          Spec.assertBool s (not (any (S.isCastOf pikerId) (Action.legalActions S.alice after))) "so the same Piker is not castable"
          Spec.assertEqWith s "and bob, the seat neither of them named, got nothing" (poolOf S.bob after) []

-- plainRed's twin one colour over, so an assertion naming it says WHICH colour
-- the recipient chose and not merely how many units they got.
plainBlue :: ManaUnit.ManaUnit
plainBlue = plainGreen {ManaUnit.manaType = ManaType.Colored Color.Blue}

-- plainGreen's twin, differing in EXACTLY one field: what Shizuko, Caller of
-- Autumn's trigger adds. The pair is what makes the group below discriminating
-- -- ManaUnit derives Eq, so an assertion naming both by name says which units
-- survived a sweep and not merely how many.
retainedGreen :: ManaUnit.ManaUnit
retainedGreen = plainGreen {ManaUnit.retention = ManaRetention.UntilEndOfTurn}

-- CR 106.4's other half, the one CR 109.5 does NOT answer: an effect may name the
-- player whose pool the mana lands in. Shizuko, Caller of Autumn ({1}{G}{G} 2/3
-- Legendary Creature -- Snake Shaman, "At the beginning of each player's upkeep,
-- that player adds {G}{G}{G}. Until end of turn, they don't lose this mana as
-- steps and phases end") is the printing, and the three halves it needs are CR
-- 603.2b's step event binding the active player (Event.eventBindings), the
-- AddMana payload's PlayerRef reading that slot (Resolve's arm), and its
-- ManaRetention riding onto each unit added (Pawl.Types.ManaRetention).
--
-- The third sentence is what puts the retention on the UNIT rather than on the
-- CR 613.11 player axis: "this mana" is the three units this ability added, and
-- carol's fourth green in the same pool is lost at the same moment. The
-- "an ordinary green in the same pool" case is what proves it -- no player-axis
-- implementation can keep three of four identical-but-for-retention units.
--
-- THREE SEATS, because two collapse the reading under test: with alice and bob
-- alone, "that player" and "an opponent of the controller" name the same seat.
-- carol takes the upkeep, alice controls Shizuko, and bob is the third seat that
-- separates them.
shizukoSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
shizukoSpec s registry = Spec.describe s "Shizuko, Caller of Autumn" $ do
  Spec.it s "CR 106.4 the three green land in the UPKEEP player's pool, not the controller's" $ do
    shizuko <- S.printingOf s registry "Shizuko, Caller of Autumn"
    let after = shizukoUpkeep shizuko S.carol
    Spec.assertEqWith s "carol got {G}{G}{G}" (poolOf S.carol after) [retainedGreen, retainedGreen, retainedGreen]
    Spec.assertEqWith s "alice, who controls Shizuko, got nothing" (poolOf S.alice after) []
    Spec.assertEqWith s "and neither did bob" (poolOf S.bob after) []
    Spec.assertEqWith s "the stack is empty again" (length (GameState.stack after)) 0

  -- The other half of the pair, differing in ONE thing: whose upkeep it is. Under
  -- CR 603.2b's EachTurn scope the same ability fires on the controller's own
  -- turn, and then "that player" IS the controller -- a redundancy, not a wrong
  -- answer, and the reading that stops "always the upkeep player" from being
  -- indistinguishable from "always somebody else".
  Spec.it s "CR 603.2b on the controller's own upkeep the same trigger pays the controller" $ do
    shizuko <- S.printingOf s registry "Shizuko, Caller of Autumn"
    let after = shizukoUpkeep shizuko S.alice
    Spec.assertEqWith s "alice got {G}{G}{G}" (poolOf S.alice after) [retainedGreen, retainedGreen, retainedGreen]
    Spec.assertEqWith s "carol got nothing" (poolOf S.carol after) []

  -- Gameplay level: the floated mana is ordinary mana, so carol can spend it. The
  -- two boards differ in ONE thing -- whose upkeep the trigger fired on -- and
  -- share seats, timing, priority and the card in carol's hand; on the second her
  -- pool is empty, so the same cast is not offered. There is no land and no other
  -- mana source anywhere, so the trigger's {G}{G}{G} is the only way to pay.
  Spec.it s "CR 106.4 carol can cast a {1}{G}{G} spell off it, and cannot when the upkeep was alice's" $ do
    shizuko <- S.printingOf s registry "Shizuko, Caller of Autumn"
    let (oid, hers) = S.addHandCard shizuko S.carol (carolMain (shizukoUpkeep shizuko S.carol))
        (otherOid, his) = S.addHandCard shizuko S.carol (carolMain (shizukoUpkeep shizuko S.alice))
    Spec.assertEqWith s "the two boards differ only in whose pool holds the mana" (poolOf S.carol hers, poolOf S.carol his) ([retainedGreen, retainedGreen, retainedGreen], [])
    Spec.assertBool s (any (S.isCastOf oid) (Action.legalActions S.carol hers)) "carol casts a second Shizuko off her own upkeep's mana"
    Spec.assertBool s (not (any (S.isCastOf otherOid) (Action.legalActions S.carol his))) "and cannot when the mana went to alice"

  -- CR 500.5 / 106.4, driven through Engine.runStep so the WHOLE upkeep step
  -- runs
  -- -- CR 603.2b's event, the trigger, the priority round and the step's own
  -- end-of-step mana emptying -- rather than by calling Mana.emptyManaPools.
  --
  -- carol's pool is seeded with one ORDINARY green before the step, so the pool
  -- the sweep sees holds four units identical but for their retention. The two
  -- casts read that difference at gameplay level on one board: Shizuko is
  -- {1}{G}{G} and Giant Spider is {3}{G}, so three green pays the first and not
  -- the second. Keeping nothing fails the first assertion; keeping all four --
  -- which is what a player-axis retention would do -- fails the second.
  Spec.it s "CR 500.5 the three retained green survive the upkeep step's end and the ordinary fourth does not" $ do
    shizuko <- S.printingOf s registry "Shizuko, Caller of Autumn"
    giantSpider <- S.printingOf s registry "Giant Spider"
    let seeded = Mana.addMana S.carol [plainGreen] (shizukoStep shizuko S.carol (Phase.Beginning BeginningStep.Upkeep))
        after = carolMain (S.runPure S.identityAnswer seeded Engine.runStep)
        (shizukoId, withShizuko) = S.addHandCard shizuko S.carol after
        (spiderId, withSpider) = S.addHandCard giantSpider S.carol after
    Spec.assertBool s (any (S.isCastOf shizukoId) (Action.legalActions S.carol withShizuko)) "carol casts a {1}{G}{G} spell in a later phase, off mana the step's end did not take"
    Spec.assertBool s (not (any (S.isCastOf spiderId) (Action.legalActions S.carol withSpider))) "but not a {3}{G} one, because the ordinary fourth green WAS taken"
    Spec.assertEqWith s "exactly the three the trigger added" (poolOf S.carol after) [retainedGreen, retainedGreen, retainedGreen]

  -- CR 514.2 ends the retention, and CR 500.5 then takes the mana. A pair of
  -- boards differing in EXACTLY one thing -- which step Engine.runStep runs --
  -- both starting from the same resolved upkeep trigger.
  --
  -- The end step is the last one the retention outlives: its end runs CR 500.5's
  -- sweep with the retention still standing. The cleanup step's turn-based
  -- actions run CR 514.2 at that step's START (Mana.endManaRetention), so the
  -- same sweep at that step's END finds ordinary mana and takes it. Swapping the
  -- two moments in Engine.hs is what this pair refuses.
  Spec.it s "CR 514.2 the retention outlives the end step and not the cleanup step" $ do
    shizuko <- S.printingOf s registry "Shizuko, Caller of Autumn"
    let floated = shizukoUpkeep shizuko S.carol
        ran phase = carolMain (S.runPure S.identityAnswer (atStep phase floated) Engine.runStep)
        afterEnd = ran (Phase.Ending EndingStep.EndStep)
        afterCleanup = ran (Phase.Ending EndingStep.Cleanup)
        (endId, castableAfterEnd) = S.addHandCard shizuko S.carol afterEnd
        (cleanupId, castableAfterCleanup) = S.addHandCard shizuko S.carol afterCleanup
    Spec.assertBool s (any (S.isCastOf endId) (Action.legalActions S.carol castableAfterEnd)) "carol still has the mana once the end step has ended"
    Spec.assertBool s (not (any (S.isCastOf cleanupId) (Action.legalActions S.carol castableAfterCleanup))) "and no longer does once the cleanup step has ended"
    Spec.assertEqWith s "the pools say the same thing" (poolOf S.carol afterEnd, poolOf S.carol afterCleanup) ([retainedGreen, retainedGreen, retainedGreen], [])

-- One Shizuko on the battlefield under ALICE's control, with @upkeep@'s upkeep
-- beginning (CR 500.1: a step belongs to exactly one turn, so the event names one
-- seat and GameState.activePlayer agrees with it), that trigger placed and
-- resolved. Three seats, no land and no other mana source anywhere, so the only
-- mana on the board is what the trigger added.
shizukoUpkeep :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState
shizukoUpkeep shizuko upkeep =
  let (_, board) = S.addCreature shizuko S.alice S.threePlayerGame
      began =
        S.withEvents
          [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Beginning BeginningStep.Upkeep) upkeep)]
          board {GameState.activePlayer = upkeep, GameState.phase = Phase.Beginning BeginningStep.Upkeep}
      placed = snd (Engine.runGamePure S.identityAnswer began Engine.placePendingTriggers)
   in snd (Engine.runGamePure S.identityAnswer placed Stack.resolveTop)

-- @shizukoUpkeep@'s twin for a runStep-driven case: the same board with NOTHING
-- yet done to it, since Engine.runStep records CR 603.2b's event, places the
-- trigger and runs the priority round that resolves it. The schedule loses its
-- head for Pawl.ActivateSpec's augurUpkeep reason -- Setup.emptyGame's
-- `remaining` still begins with the upkeep step, so a runStep-driven board would
-- otherwise advance back into the step it just ran.
shizukoStep :: Printing.Printing -> PlayerId.PlayerId -> Phase.Phase -> GameState.GameState
shizukoStep shizuko active phase =
  let (_, board) = S.addCreature shizuko S.alice S.threePlayerGame
   in board
        { GameState.activePlayer = active,
          GameState.phase = phase,
          GameState.priority = Just active,
          GameState.remaining = Seq.drop 1 (GameState.remaining board)
        }

-- An already-floated board moved to another step of the SAME turn, so the pair
-- above differs in one field and nothing else. The active player keeps priority,
-- which is what makes Engine.runStep grant a priority round rather than settle.
atStep :: Phase.Phase -> GameState.GameState -> GameState.GameState
atStep phase gs = gs {GameState.phase = phase, GameState.priority = Just (GameState.activePlayer gs)}

-- CR 307.1 / 117.1a: carol active with priority in her own precombat main phase,
-- which is what a sorcery-speed cast of hers needs. Applied to BOTH boards of the
-- pair below, so the seat and the timing are shared and only the pool differs.
carolMain :: GameState.GameState -> GameState.GameState
carolMain gs =
  gs
    { GameState.activePlayer = S.carol,
      GameState.phase = Phase.PrecombatMain,
      GameState.priority = Just S.carol
    }

-- The units of one player's pool, poolUnits generalised over the seat -- three
-- seats being what tells "that player" from "the controller".
poolOf :: PlayerId.PlayerId -> GameState.GameState -> [ManaUnit.ManaUnit]
poolOf pid gs = case Game.poolOf pid gs of
  Mana.Type.MkMana units -> units

-- CR 500.5a's unit-axis half, and ManaRetention's second non-Ordinary arm.
-- Avatar Roku, Firebender ({3}{R}{R}{R} Legendary Creature -- Human Avatar 6/6,
-- "Whenever a player attacks, add six {R}. Until end of combat, you don't lose
-- this mana as steps end. {R}{R}{R}: Target creature gets +3/+0 until end of
-- turn") is the printing. Nothing is omitted from the card, so pawl's Roku is
-- neither stricter nor weaker than printed; Pawl.EventTriggerSpec's group of the
-- same name covers the OTHER sentence, CR 508.3d's "a player" payload.
--
-- The retention rides the UNIT and not CR 613.11's player axis, for Shizuko's
-- reason above: "this mana" is the six units this ability added, and a seventh
-- red in the same pool is lost at the first step end.
--
-- THREE MOMENTS, because a shorter board admits two wrong implementations:
--
--   * the declare blockers step separates the arm from ManaRetention.Ordinary,
--     which is what this card carried while the arm did not exist -- CR 500.5
--     takes the pool as the declare attackers step ends.
--   * the end of combat step separates it from a retention ended at a combat
--     STEP's end. CR 500.5a's own sentence is that the effect lasts through that
--     step, and Pawl.ExpirySpec's "the end of combat STEP ending does not expire
--     it; the PHASE ending does" is the stored-effect twin.
--   * the postcombat main phase separates it from ManaRetention.UntilEndOfTurn,
--     and nothing earlier can. Without it the board proves only "longer than one
--     step".
--
-- Read as a LEGALITY (design.md section 4) with the pool's exact contents
-- alongside, and then SPENT in the second case, because presence is not
-- spendability. alice holds no land and bob no mana source at all, so the
-- trigger's six {R} is the only mana in the game.
avatarRokuSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
avatarRokuSpec s registry =
  let withPriority gs = gs {GameState.priority = Just S.alice}
      -- Declares attackers and blockers and PASSES on every action (identityAnswer
      -- answers Prompt.ChooseAction with Pass, and aggressiveAnswer defers to it),
      -- so nothing is spent and each moment's pool is read clean.
      passing :: Prompt.Prompt r -> r
      passing = S.aggressiveAnswer
      fixture = do
        roku <- S.printingOf s registry "Avatar Roku, Firebender"
        piker <- S.printingOf s registry "Goblin Piker"
        case S.combatBoardOf [roku] [piker] of
          (gs, [rokuId], [pikerId]) -> pure (Just (rokuId, pikerId, gs))
          _ -> pure Nothing
   in Spec.describe s "Avatar Roku, Firebender" $ do
        Spec.it s "CR 500.5a the retained {R} outlive every step of the combat phase, and the phase's end takes them" $ do
          built <- fixture
          case built of
            Just (rokuId, _, gs) -> do
              let blockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) passing gs
                  endOfCombat = S.runToStep (Phase.Combat CombatStep.EndOfCombat) passing blockers
                  postcombat = S.runPure passing endOfCombat Engine.runStep
                  offered g = any (isActivationOf rokuId) (Action.legalActions S.alice (withPriority g))
              -- S.runToStep stops silently once combat is left, so each moment
              -- says which step it is before anything is read off it. None of the
              -- three depends on the retention, so none can absorb a mutation to it.
              Spec.assertEqWith s "the first moment is the declare blockers step" (GameState.phase blockers) (Phase.Combat CombatStep.DeclareBlockers)
              Spec.assertEqWith s "the second is the end of combat step" (GameState.phase endOfCombat) (Phase.Combat CombatStep.EndOfCombat)
              Spec.assertEqWith s "and the third is the postcombat main phase" (GameState.phase postcombat) Phase.PostcombatMain
              Spec.assertBool s (offered blockers) "alice may activate Roku once the declare attackers step has ended"
              Spec.assertBool s (offered endOfCombat) "and still may once the combat damage step has ended"
              Spec.assertBool s (not (offered postcombat)) "and no longer may in the postcombat main phase"
              Spec.assertEqWith s "exactly the six the trigger added, retained" (poolOf S.alice blockers) (replicate 6 retainedRed)
              Spec.assertEqWith s "all six still there as the end of combat step begins" (poolOf S.alice endOfCombat) (replicate 6 retainedRed)
              Spec.assertEqWith s "and none once the combat phase has ended" (poolOf S.alice postcombat) []
            Nothing -> Spec.assertFailure s "fixture should give alice a Roku and bob a Piker"

        -- Presence is not spendability, and only spending reaches CR 106.4's
        -- "used to pay costs". alice takes every activation offered in the
        -- DECLARE BLOCKERS step -- a step whose start is already past the end
        -- that CR 500.5 would have emptied the pool at -- and six {R} pays for
        -- exactly two, so Roku is a 12/6. Ordinary retention leaves it its
        -- printed 6/6.
        Spec.it s "CR 106.4 the retained mana pays for two activations in a LATER step" $ do
          built <- fixture
          case built of
            Just (rokuId, _, gs) -> do
              let activating :: Prompt.Prompt r -> r
                  activating p = case p of
                    Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature rokuId) . snd) sets
                    Prompt.ChooseAction _ _ options -> case filter (isActivationOf rokuId) options of
                      a : _ -> a
                      [] -> Action.Type.Pass
                    _ -> S.aggressiveAnswer p
                  blockers = withPriority (S.runToStep (Phase.Combat CombatStep.DeclareBlockers) passing gs)
                  after = S.runPure activating blockers Engine.runStep
              Spec.assertEqWith s "Roku is its printed 6/6 as the step begins" (S.powerToughnessOf rokuId blockers) (Just (6, 6))
              Spec.assertEqWith s "and a 12/6 once the step has run, off two activations" (S.powerToughnessOf rokuId after) (Just (12, 6))
              Spec.assertEqWith s "which is the whole pool spent" (poolOf S.alice after) []
            Nothing -> Spec.assertFailure s "fixture should give alice a Roku and bob a Piker"

        -- CR 724.2d/e: Mandate of Peace ({1}{W} Instant, "Cast this spell only
        -- during combat. Your opponents can't cast spells this turn. End the
        -- combat phase.") ends the combat phase with its last step never running,
        -- so Turn.phaseEndingAt never reports that end and the CR 500.5 sweep at
        -- the step's own end sees no phase. Pawl.Engine.Resolve's CR 724.2 arm has
        -- to end the retention itself, or the mana outlives the phase it was
        -- scoped to.
        --
        -- Read as the postcombat main phase begins -- one step after the cast,
        -- which is the first moment the two readings differ. Pawl.TurnSpec's
        -- endCombatPhaseSpec covers the same arm on the STORED-effect axis (a Jade
        -- Statue's animation); this is its unit-axis twin.
        --
        -- BOB casts it, and holds the only lands. Every red in the game is the
        -- trigger's, so the cast cannot spend any of it and the "may no longer
        -- activate" read cannot pass because alice ran short.
        Spec.it s "CR 724.2d a combat phase ended part-way through takes the retained mana" $ do
          built <- fixture
          case built of
            Just (rokuId, _, gs) -> do
              plains <- S.printingOf s registry "Plains"
              mandate <- S.printingOf s registry "Mandate of Peace"
              let (spell, staged) = S.addHandCard mandate S.bob (S.landsFor plains S.bob 2 gs)
                  blockers = withPriority (S.runToStep (Phase.Combat CombatStep.DeclareBlockers) passing staged)
                  after = S.runPure (castingOnly spell) blockers Engine.runStep
              Spec.assertEqWith s "the six retained {R} are there when the step begins" (poolOf S.alice blockers) (replicate 6 retainedRed)
              Spec.assertEqWith s "the combat phase is over" (GameState.phase after) Phase.PostcombatMain
              Spec.assertBool s (not (any (isActivationOf rokuId) (Action.legalActions S.alice (withPriority after)))) "and alice may no longer activate Roku off it"
              Spec.assertEqWith s "the pool says the same thing" (poolOf S.alice after) []
            Nothing -> Spec.assertFailure s "fixture should give alice a Roku and bob a Piker"

        -- CR 724.1d's half of the same claim: Time Stop ({4}{U}{U} Instant, "End
        -- the turn.") ends the current phase as well as the step, and the game
        -- skips straight to the cleanup step. The retention is scoped to the
        -- combat phase, which has just ended, so the mana goes at that step's own
        -- CR 500.5 sweep and does NOT wait for CR 514.2.
        --
        -- Read as the cleanup step begins and BEFORE it runs, which is the whole
        -- discrimination: Mana.endManaRetention would end any retention still
        -- standing at that step's start, so a board read after the cleanup step
        -- had run would find an empty pool under both readings.
        --
        -- BOB casts it, the case above's reason and sharper here: {4}{U}{U} is
        -- six mana, and alice paying any of it out of the retained {R} would
        -- leave her under {R}{R}{R} whatever the sweep did.
        Spec.it s "CR 724.1d ending the turn during combat takes the retained mana before cleanup" $ do
          built <- fixture
          case built of
            Just (rokuId, _, gs) -> do
              island <- S.printingOf s registry "Island"
              timeStop <- S.printingOf s registry "Time Stop"
              let (spell, staged) = S.addHandCard timeStop S.bob (S.landsFor island S.bob 6 gs)
                  blockers = withPriority (S.runToStep (Phase.Combat CombatStep.DeclareBlockers) passing staged)
                  after = S.runPure (castingOnly spell) blockers Engine.runStep
              Spec.assertEqWith s "the six retained {R} are there when the step begins" (poolOf S.alice blockers) (replicate 6 retainedRed)
              Spec.assertEqWith s "the game jumped to the cleanup step" (GameState.phase after) (Phase.Ending EndingStep.Cleanup)
              Spec.assertBool s (not (any (isActivationOf rokuId) (Action.legalActions S.alice (withPriority after)))) "and alice may no longer activate Roku off it"
              Spec.assertEqWith s "the pool says the same thing" (poolOf S.alice after) []
            Nothing -> Spec.assertFailure s "fixture should give alice a Roku and bob a Piker"

        -- CR 500.5 / CR 614.1b: the same phase end reached by a SKIP of the
        -- phase's last step rather than by an effect ending the phase. CR 724.2e
        -- is the CR contemplating exactly that pairing -- the combat phase ends
        -- while its end of combat step does not happen -- and Engine.skipStep is
        -- where the phase-grain sweep this retention needs now lives.
        --
        -- Synthetic Truncate the Fray ({1}{U} Instant, "Target player skips their
        -- next end of combat step"), synthetic because nothing printed names that
        -- step: Scryfall o:/skips?.*end of combat/ and o:"end of combat step"
        -- o:skip, 2026-08-27, no hit. Pawl.TurnSpec's SkippedEndOfCombat group is
        -- the stored-effect and combat-record twin of this case.
        --
        -- The skip must be aimed at the ACTIVE player, whose combat phase it is
        -- (Event.beginsPhase asks of GameState.activePlayer), so bob casts it at
        -- alice. BOB casts it for the case above's reason as well: he holds the
        -- only lands, so no part of {1}{U} can come out of the retained {R}.
        --
        -- "The end of combat step never began" is what keeps this from passing
        -- vacuously: a cast that silently did nothing would leave the step to run
        -- normally, and the pool would be empty at the same moment under both
        -- readings.
        Spec.it s "CR 500.5 a skipped end of combat step still takes the retained mana" $ do
          built <- fixture
          case built of
            Just (rokuId, _, gs) -> do
              island <- S.printingOf s registry "Island"
              fray <- S.printingOf s registry "Synthetic Truncate the Fray"
              let (spell, staged) = S.addHandCard fray S.bob (S.landsFor island S.bob 2 gs)
                  blockers = withPriority (S.runToStep (Phase.Combat CombatStep.DeclareBlockers) passing staged)
                  after = S.runToStep Phase.PostcombatMain (castingOnly spell) blockers
                  began = filter (== GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Combat CombatStep.EndOfCombat) S.alice)) (S.eventsOf after)
              Spec.assertEqWith s "the six retained {R} are there when the step begins" (poolOf S.alice blockers) (replicate 6 retainedRed)
              Spec.assertEqWith s "CR 614.6 the end of combat step never began" began []
              Spec.assertEqWith s "CR 511.3 the combat phase is over regardless" (GameState.phase after) Phase.PostcombatMain
              Spec.assertEqWith s "and the retained mana went with the phase" (poolOf S.alice after) []
              Spec.assertBool s (not (any (isActivationOf rokuId) (Action.legalActions S.alice (withPriority after)))) "so alice may no longer activate Roku off it"
            Nothing -> Spec.assertFailure s "fixture should give alice a Roku and bob a Piker"

-- Casts that one object the first time it is offered and passes otherwise,
-- leaving every combat decision to S.aggressiveAnswer. Pinned to the OBJECT
-- rather than to "the first cast on offer", so a mutation cannot be repaired by
-- the answerer finding some other legal spell.
castingOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
castingOnly spell p = case p of
  Prompt.ChooseAction _ _ actions -> case filter (S.isCastOf spell) actions of
    h : _ -> h
    [] -> Action.Type.Pass
  _ -> S.aggressiveAnswer p

-- Roku's {R}, which the trigger's ManaAddition stamps UntilEndOfCombat onto.
retainedRed :: ManaUnit.ManaUnit
retainedRed = plainRed {ManaUnit.retention = ManaRetention.UntilEndOfCombat}

-- CR 106.6: mana that carries a restriction on what it may be spent on. Geosurge
-- ({R}{R}{R}{R} Sorcery, "Add {R}{R}{R}{R}{R}{R}{R}. Spend this mana only to cast
-- artifact or creature spells") is the printing, and the whole card is that one
-- sentence. Its mana comes off the STACK -- a sorcery resolving through
-- Pawl.Engine.Resolve's Effect.AddMana arm. Mishra's Workshop below is the same
-- rule on the inline road (CR 605.3b), which is the pair that says the two
-- producers stamp the same clause.
--
-- Nothing is omitted from the card, so pawl's Geosurge is neither stricter nor
-- weaker than printed.
geosurgeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
geosurgeSpec s registry = Spec.describe s "Geosurge" $ do
  -- The discriminating pair, and it is a pair of SPELLS rather than of boards:
  -- one pool of seven red pays for the artifact spell and not for the instant,
  -- which is the only difference between the two casts. The BEFORE board is the
  -- control that keeps the refusal from passing for an unrelated reason -- with
  -- four untapped Mountains instead of the restricted seven, the very same
  -- Lightning Bolt in the very same hand, at the same phase and with the same
  -- targets available, is castable.
  Spec.it s "CR 106.6 the seven red pay for an artifact spell and not for an instant" $ do
    geosurge <- S.printingOf s registry "Geosurge"
    mountain <- S.printingOf s registry "Mountain"
    solRing <- S.printingOf s registry "Sol Ring"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (before, after) = geosurgeBoards geosurge mountain
        castables gs =
          let (ringId, withRing) = S.addHandCard solRing S.alice gs
              (boltId, withBolt) = S.addHandCard bolt S.alice gs
           in (S.castable S.alice ringId withRing, S.castable S.alice boltId withBolt)
    Spec.assertEqWith s "before Geosurge, four untapped Mountains cast either one" (castables before) (True, True)
    Spec.assertEqWith s "after it, the same seven mana cast the artifact spell and refuse the instant" (castables after) (True, False)
    Spec.assertEqWith s "and the pool is seven red, every one of them restricted" (poolOf S.alice after) (replicate 7 restrictedRed)

  -- The other half: the mana the restriction ADMITS is spent like any other, so
  -- the cast it allows really is paid out of these units and not out of some
  -- other supply. Six restricted red are left, not seven and not zero.
  Spec.it s "CR 106.4 the artifact cast spends one of the seven and leaves six" $ do
    geosurge <- S.printingOf s registry "Geosurge"
    mountain <- S.printingOf s registry "Mountain"
    solRing <- S.printingOf s registry "Sol Ring"
    let after = snd (geosurgeBoards geosurge mountain)
        (ringId, withRing) = S.addHandCard solRing S.alice after
        paid = S.runPure S.identityAnswer withRing (S.cast S.alice ringId)
    Spec.assertEqWith s "Sol Ring is on the stack" (length (GameState.stack paid)) 1
    Spec.assertEqWith s "six restricted red are left" (poolOf S.alice paid) (replicate 6 restrictedRed)

  -- CR 106.4's other half, and the one that keeps the restriction from being a
  -- way to LOSE mana: a cost the seven cannot pay is paid out of something else,
  -- and the seven are still in the pool afterwards. One Mountain added AFTER
  -- Geosurge resolved is the only difference from the board above, so the {R}
  -- Lightning Bolt now has a legal payment that does not touch the restricted
  -- mana -- and pawl must both offer that cast and pay it the way the rule says.
  Spec.it s "CR 106.4 an instant paid from a Mountain leaves all seven restricted red floating" $ do
    geosurge <- S.printingOf s registry "Geosurge"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let spare = S.landsFor mountain S.alice 1 (snd (geosurgeBoards geosurge mountain))
        (boltId, withBolt) = S.addHandCard bolt S.alice spare
        after = S.runPure S.identityAnswer withBolt (S.cast S.alice boltId)
    Spec.assertBool s (S.castable S.alice boltId withBolt) "the spare Mountain makes the instant castable"
    Spec.assertEqWith s "Lightning Bolt is on the stack" (length (GameState.stack after)) 1
    Spec.assertEqWith s "and the seven restricted red are untouched" (poolOf S.alice after) (replicate 7 restrictedRed)

  -- CR 106.6 asked of a payment that is NO cast. Chromatic Star ("{1}, {T},
  -- Sacrifice this artifact: Add one mana of any color") is a mana ability whose
  -- own cost holds mana, and paying it is an activation cost (CR 602.2b) -- so
  -- none of Geosurge's seven may go toward it, and the white pip it would have
  -- minted is out of reach. Doomed Traveler ({W} Creature) is the spell, a
  -- CREATURE spell so the restriction admits the seven to the cast itself: what
  -- separates the two readings is the Star's {1} alone.
  --
  -- Two boards differing in ONE untapped Mountain, which is one unrestricted mana
  -- and exactly what the Star's {1} wants -- so the control casts the same spell
  -- off the same Star, and the refusal cannot be "she has no white".
  Spec.it s "CR 106.6 the restricted seven cannot pay a mana ability's own {1}" $ do
    geosurge <- S.printingOf s registry "Geosurge"
    mountain <- S.printingOf s registry "Mountain"
    star <- S.printingOf s registry "Chromatic Star"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let after = snd (geosurgeBoards geosurge mountain)
        withStar = snd (S.addCreature star S.alice after)
        casts gs =
          let (travelerId, held) = S.addHandCard traveler S.alice gs
           in S.castable S.alice travelerId held
    Spec.assertBool s (not (casts withStar)) "CR 106.6 nothing unrestricted pays the Star's {1}, so its any-colour mana is never made"
    Spec.assertBool s (casts (S.landsFor mountain S.alice 1 withStar)) "one untapped Mountain pays that same {1}, and the Star then mints the {W}"

-- alice with four untapped Mountains and Geosurge in hand, before and after she
-- casts it and the sorcery resolves. The pair differs in nothing a test set up:
-- the second board is the first one played forward.
geosurgeBoards :: Printing.Printing -> Printing.Printing -> (GameState.GameState, GameState.GameState)
geosurgeBoards geosurge mountain =
  let (before, geoId) = S.handOne geosurge (S.landsInPlay mountain 4)
      cast_ = S.runPure S.identityAnswer before (S.cast S.alice geoId)
   in (before, S.runPure S.identityAnswer cast_ Stack.resolveTop)

-- One of Geosurge's seven: red, from a source that is not snow, lost as the
-- phase ends, and spendable only on an artifact or creature spell.
restrictedRed :: ManaUnit.ManaUnit
restrictedRed =
  ManaUnit.MkManaUnit
    { ManaUnit.manaType = ManaType.Colored Color.Red,
      ManaUnit.tags = Set.empty,
      ManaUnit.retention = ManaRetention.Ordinary,
      ManaUnit.restriction =
        Just (ManaRestriction.onlyCasts (Filter.Or [Filter.HasCardType CardType.Artifact, Filter.HasCardType CardType.Creature])),
      ManaUnit.rider = Nothing
    }

-- CR 106.6 on the OTHER road: a mana ability's restricted mana, added inline at
-- payment (CR 605.3b) rather than by a spell resolving off the stack. Mishra's
-- Workshop (Land, "{T}: Add {C}{C}{C}. Spend this mana only to cast artifact
-- spells") is the printing -- Antiquities is the paper set -- and the whole card
-- is that one ability.
--
-- Nothing is omitted from the card, so pawl's Workshop is neither stricter nor
-- weaker than printed.
--
-- Geosurge above is the same rule on the stack road, and the pair is what proves
-- the two roads agree: the restriction rides Pawl.Types.ManaAddition, and
-- Mana.manaOptionsOfGiven is what stamps it onto the units this road adds.
workshopSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
workshopSpec s registry = Spec.describe s "Mishra's Workshop" $ do
  -- The discriminating pair, twice over. Two SPELLS: Sol Ring ({1} Artifact) is
  -- what the restriction admits and Goblin Piker ({1}{R} Creature) is what it
  -- does not, and the Mountain beside the Workshop is what pays the Piker's
  -- {R} -- so its {1} is the only demand the restricted colourless could serve
  -- and the refusal is CR 106.6 rather than a colour the board cannot make. Two
  -- BOARDS: the control swaps the Workshop for three Reliquary Towers, which is
  -- the same three colourless off untapped lands with nothing said about
  -- spending them. The boards differ in how many lands carry that mana, since no
  -- printing in `data/cards/` puts three unrestricted colourless on one
  -- permanent; what they do not differ in is the mana available, which is what
  -- both casts are asked about.
  Spec.it s "CR 106.6 the inline three colourless cast an artifact spell and refuse a creature spell" $ do
    workshop <- S.printingOf s registry "Mishra's Workshop"
    tower <- S.printingOf s registry "Reliquary Tower"
    mountain <- S.printingOf s registry "Mountain"
    solRing <- S.printingOf s registry "Sol Ring"
    piker <- S.printingOf s registry "Goblin Piker"
    let castables board =
          let (withRing, ringId) = S.handOne solRing board
              (pikerId, withBoth) = S.addHandCard piker S.alice withRing
           in (S.castable S.alice ringId withBoth, S.castable S.alice pikerId withBoth)
    Spec.assertEqWith s "three unrestricted colourless and a Mountain cast either one" (castables (S.landsFor tower S.alice 3 (S.landsInPlay mountain 1))) (True, True)
    Spec.assertEqWith s "the Workshop's three cast the artifact spell and not the creature spell" (castables (S.landsFor workshop S.alice 1 (S.landsInPlay mountain 1))) (True, False)

  -- The stamp itself, read off the pool the payment left. One Workshop and
  -- nothing else, so the {1} has one payment and the two unspent mana are
  -- CR 106.4's floating remainder -- restricted, which is what says the inline
  -- path carried the instruction's clause and not just its type.
  Spec.it s "CR 106.4 the artifact cast spends one of the three and leaves two restricted" $ do
    workshop <- S.printingOf s registry "Mishra's Workshop"
    solRing <- S.printingOf s registry "Sol Ring"
    let (before, ringId) = S.handOne solRing (S.landsInPlay workshop 1)
        paid = S.runPure S.identityAnswer before (S.cast S.alice ringId)
    Spec.assertEqWith s "two restricted colourless are left floating" (poolOf S.alice paid) (replicate 2 restrictedColorless)
    Spec.assertEqWith s "Sol Ring is on the stack" (length (GameState.stack paid)) 1
    Spec.assertEqWith s "and the Workshop is the permanent that paid" (S.tappedCount S.alice paid) 1

-- One of the Workshop's three: colourless, from a source that is not snow, lost
-- as the phase ends, and spendable only on an artifact spell.
restrictedColorless :: ManaUnit.ManaUnit
restrictedColorless =
  ManaUnit.MkManaUnit
    { ManaUnit.manaType = ManaType.Colorless,
      ManaUnit.tags = Set.empty,
      ManaUnit.retention = ManaRetention.Ordinary,
      ManaUnit.restriction = Just (ManaRestriction.onlyCasts (Filter.HasCardType CardType.Artifact)),
      ManaUnit.rider = Nothing
    }

-- CR 106.4's retention on the INLINE road (CR 605.3b), the third clause a mana
-- ability's ManaAddition carries onto the units it adds -- the restriction above
-- and Boseiju's rider being the other two. Shizuko, Caller of Autumn is the same
-- clause on the stack road, and the pair is what proves the two roads agree.
--
-- SYNTHETIC, and by census rather than by accident: Scryfall
-- o:"as steps and phases end" returns 30 printings on 2026-09-02, and not one
-- puts the clause on an ACTIVATED ability -- they are player-axis statics
-- (Upwelling, Omnath, Leyline Tyrant), triggered abilities (Savage Ventmaw,
-- Shizuko, Branch of Vitu-Ghazi's turn-up trigger, whose own "{T}: Add {C}"
-- carries no rider) or spells (Rousing Refrain, Tundra Fumarole, The Last Agni
-- Kai). All of those resolve off the stack. Nothing in CR 605.1a forbids the
-- printing: its four criteria say nothing about how long the mana lasts.
--
-- Synthetic Lasting Spring ({2} Artifact, "{T}: Add {C}. Until end of turn, you
-- don't lose this mana as steps and phases end") is that card and nothing else,
-- so the only thing the case can read is the retention.
lastingSpringSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
lastingSpringSpec s registry = Spec.describe s "Synthetic Lasting Spring" $ do
  -- Two boards differing in ONE permanent -- Serum Powder ({3} Artifact, "{T}:
  -- Add {C}") is the same tap for the same one colourless with no rider on it,
  -- and its mulligan action is unreachable from the battlefield. Both are tapped
  -- inline (Cost.tapForMana, the narrowest path to CR 605.3b), both cross the
  -- same upkeep step's end through Engine.runStep -- so the whole step runs,
  -- CR 500.5's sweep included, rather than Mana.emptyManaPools being called --
  -- and both are then asked the same cast in the same later phase.
  --
  -- Sol Ring ({1}) is the gameplay reading: neither board has a land or any
  -- other mana source, so the floated {C} is the only way to pay, and the
  -- control's False says the sweep did run at that boundary.
  Spec.it s "CR 500.5 the inline mana survives the step's end, and an ordinary {C} does not" $ do
    spring <- S.printingOf s registry "Synthetic Lasting Spring"
    powder <- S.printingOf s registry "Serum Powder"
    solRing <- S.printingOf s registry "Sol Ring"
    let ran printing = aliceMain (S.runPure S.identityAnswer (tappedAtUpkeep printing) Engine.runStep)
        castsRing gs =
          let (ringId, withRing) = S.addHandCard solRing S.alice gs
           in S.castable S.alice ringId withRing
    Spec.assertBool s (castsRing (ran spring)) "alice casts a {1} spell in a later phase, off mana the step's end did not take"
    Spec.assertBool s (not (castsRing (ran powder))) "and cannot off an ordinary {C}, which that same step's end took"
    Spec.assertEqWith s "the pools say the same thing" (poolOf S.alice (ran spring), poolOf S.alice (ran powder)) ([retainedColorless], [])

-- `printing`'s one mana ability activated inline during alice's upkeep. The
-- schedule loses its head for shizukoStep's reason: Setup.emptyGame's
-- `remaining` still begins with the upkeep step, so a runStep-driven board would
-- otherwise advance back into the step it just ran.
tappedAtUpkeep :: Printing.Printing -> GameState.GameState
tappedAtUpkeep printing =
  let (oid, board) = S.addCreature printing S.alice (Setup.emptyGame S.bothPlayers)
      upkeep =
        board
          { GameState.phase = Phase.Beginning BeginningStep.Upkeep,
            GameState.priority = Just (GameState.activePlayer board),
            GameState.remaining = Seq.drop 1 (GameState.remaining board)
          }
   in S.runPure S.identityAnswer upkeep (Cost.tapForMana S.manaPerformer oid)

-- carolMain one seat over: alice active with priority in her own precombat main
-- phase, which is what a sorcery-speed cast of hers needs (CR 307.1 / 117.1a).
aliceMain :: GameState.GameState -> GameState.GameState
aliceMain gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.PrecombatMain,
      GameState.priority = Just S.alice
    }

-- plainOf's colourless twin, differing in EXACTLY one field: what the Spring's
-- ability adds and Serum Powder's does not.
retainedColorless :: ManaUnit.ManaUnit
retainedColorless = (plainOf ManaType.Colorless) {ManaUnit.retention = ManaRetention.UntilEndOfTurn}

-- CR 106.6's OTHER subject, and the one no card in the pool reached before: a
-- restriction over ACTIVATIONS rather than over casts. Omen Hawker ({U} 1/1
-- Creature -- Octopus Advisor, "{T}: Add {C}{U}. Spend this mana only to
-- activate abilities") is the printing -- March of the Machine is the paper
-- set -- and it prints no cast half at all, which is why it is the producer: an
-- engine that still read the field as a cast filter cannot come out right here
-- by luck.
--
-- Nothing is omitted from the card, so pawl's Omen Hawker is neither stricter
-- nor weaker than printed.
--
-- CR 602.2b is the rule that makes the two subjects different payments: an
-- ability's cost is paid under CR 601.2h as a spell's is, but the object it is
-- paid for is the ability's source and not a spell.
omenHawkerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
omenHawkerSpec s registry = Spec.describe s "Omen Hawker" $ do
  -- The board is chosen so that the two readings of the field cannot agree:
  -- alice has NO LANDS, so the Hawker is the only mana in the game, and the
  -- equip cost ({1}) and Ancestral Recall ({U}) are both drawn from its one
  -- yield. The Hawker is also the equip's target, so one creature does.
  --
  -- Neither assertion discriminates alone. The POWER separates "refuses the
  -- activation" (1) from "allows it" (3); the CASTABLE separates "allows it
  -- because the restriction is honoured for activations" from "allows it
  -- because no restriction was stamped at all", which would also cast the
  -- instant.
  Spec.it s "CR 106.6 the mana pays an equip cost and casts no spell" $ do
    hawker <- S.printingOf s registry "Omen Hawker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    recall <- S.printingOf s registry "Ancestral Recall"
    solRing <- S.printingOf s registry "Sol Ring"
    let (hawkerId, g1) = S.addCreature hawker S.alice (Setup.emptyGame S.bothPlayers)
        (equipId, g2) = S.addCreature bonesplitter S.alice g1
    -- From the PROJECTION: Bonesplitter declares CR 702.6a's keyword and prints
    -- no activated ability of its own.
    case Projection.abilitiesOf equipId g2 of
      [] -> Spec.assertFailure s "Bonesplitter should offer rule 702.6a's minted equip ability"
      equipAbility : _ -> do
        let -- S.handOne rather than a second S.addHandCard: it is what puts the
            -- board in a MAIN PHASE with priority, which both the equip's
            -- "activate only as a sorcery" (CR 702.6a) and Sol Ring's sorcery
            -- timing need.
            (g3, recallId) = S.handOne recall g2
            (ringId, board) = S.addHandCard solRing S.alice g3
            equipped =
              let activated = S.runPure S.identityAnswer board (Activate.activateAbility S.alice equipId equipAbility)
               in S.runPure S.identityAnswer activated Stack.resolveTop
        Spec.assertEqWith s "unequipped the Hawker is 1/1" (S.powerToughnessOf hawkerId board) (Just (1, 1))
        Spec.assertEqWith s "CR 602.2b the Hawker's own mana pays the equip, so it is 3/1" (S.powerToughnessOf hawkerId equipped) (Just (3, 1))
        Spec.assertBool s (not (S.castable S.alice recallId board)) "CR 106.6 and the same mana casts no spell"
        -- The same refusal asked of the mana that is still FLOATING once the
        -- equip has been paid, and of a COLOURLESS demand so that the colour is
        -- not what refuses: Sol Ring's {1} is exactly what the leftover unit
        -- could serve if nothing were said about it.
        Spec.assertBool s (not (S.castable S.alice ringId equipped)) "CR 106.4 nor does the unit still floating afterwards"
        -- WHICH of the two paid the generic {1} is the payer's
        -- (Mana.spendChosen), and S.identityAnswer takes the first offered, so
        -- the blue went and the colourless floats. Either way the leftover
        -- carries the clause, which is what says the inline road (CR 605.3b)
        -- stamped it onto every unit the instruction added.
        Spec.assertEqWith s "CR 106.4 the unspent one floats, restriction and all" (poolOf S.alice equipped) [hawkerMana ManaType.Colorless]
        -- The BOUNDARY an activation's record must not cross. CR 400.7d's record
        -- on a PERMANENT is the mana that cast the spell it became, and the equip
        -- is an activation, whose units go on the CR 602.2a ability object instead
        -- (Cost.payMana's `record` argument). So the Bonesplitter's own field is
        -- still empty here -- an implementation recording against the SOURCE would
        -- have overwritten it, which is what this pins.
        Spec.assertEqWith s "CR 400.7d the equip records no mana spent on the Bonesplitter" (fmap Object.manaSpent (Game.lookupObject equipId equipped)) (Just (Mana.Type.MkMana []))

  -- The CONTROL, one Island apart. Everything the assertions above rest on that
  -- is not CR 106.6 -- the Hawker being unsick enough to tap (CR 302.6), the
  -- equip's sorcery-speed rider (CR 702.6a), a legal equip target, the phase --
  -- is unchanged here, and both the equip and the cast go through. So the
  -- refusal above is the restriction and not the board.
  Spec.it s "CR 106.6 one Island casts the same spell, so the refusal above is the restriction" $ do
    hawker <- S.printingOf s registry "Omen Hawker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    recall <- S.printingOf s registry "Ancestral Recall"
    solRing <- S.printingOf s registry "Sol Ring"
    island <- S.printingOf s registry "Island"
    let (hawkerId, g1) = S.addCreature hawker S.alice (S.landsInPlay island 1)
        (equipId, g2) = S.addCreature bonesplitter S.alice g1
    case Projection.abilitiesOf equipId g2 of
      [] -> Spec.assertFailure s "Bonesplitter should offer rule 702.6a's minted equip ability"
      equipAbility : _ -> do
        let (g3, recallId) = S.handOne recall g2
            (ringId, board) = S.addHandCard solRing S.alice g3
            equipped =
              let activated = S.runPure S.identityAnswer board (Activate.activateAbility S.alice equipId equipAbility)
               in S.runPure S.identityAnswer activated Stack.resolveTop
        Spec.assertEqWith s "the equip still resolves" (S.powerToughnessOf hawkerId equipped) (Just (3, 1))
        Spec.assertBool s (S.castable S.alice recallId board) "and the Island casts the instant"
        Spec.assertBool s (S.castable S.alice ringId board) "and the artifact spell too, so neither demand is what refused above"

  -- CR 602.2b's own reading, one level in: paying a mana ability's activation
  -- cost is an activation, so mana restricted to activations may pay it. Omen
  -- Hawker taps for {C}{U}, one of those buys Chromatic Star's "{1}, {T},
  -- Sacrifice this artifact", and the any-colour mana the Star mints pays Greed's
  -- "{B}, Pay 2 life: Draw a card". alice has no lands, so the Hawker's yield is
  -- the only mana in the game and the Star's {1} has nothing else to draw on.
  --
  -- Greed's {B} is a colour the Hawker cannot make, which is what stops the
  -- outer cost being payable off the yield directly -- the Star's minted mana is
  -- the only route to it. The CONTROL is one untapped Island, which is one
  -- unrestricted mana and exactly what the Star's {1} wants: it pays that same
  -- {1} by another road, so the refusal above cannot be the phase, the Hawker's
  -- CR 302.6 settle, or a missing black.
  Spec.it s "CR 602.2b the mana pays a nested mana ability's own activation cost" $ do
    hawker <- S.printingOf s registry "Omen Hawker"
    star <- S.printingOf s registry "Chromatic Star"
    greed <- S.printingOf s registry "Greed"
    island <- S.printingOf s registry "Island"
    let (_, g1) = S.addCreature hawker S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addCreature star S.alice g1
        (greedId, board) = S.addCreature greed S.alice g2
        drawable gs = any (\ability -> Activate.activatable S.alice greedId ability gs) (Projection.abilitiesOf greedId gs)
    Spec.assertBool s (drawable board) "CR 106.6 the Hawker's {C} buys the Star, whose mana pays Greed's {B}"
    Spec.assertBool s (drawable (S.landsFor island S.alice 1 board)) "one untapped Island pays that same {1}, so the board is otherwise fine"

  -- The same mana in CR 601.2g's window, which is the board the restriction's
  -- OWN subject does not name: alice is CASTING Doomed Traveler ({W} Creature),
  -- so the Hawker's mana may not pay the spell -- and CR 605.3a still lets her
  -- activate the Star inside that window, where its {1} is an activation cost
  -- (CR 602.2b) the Hawker's mana may pay. Geosurge's "spend this mana only to
  -- cast artifact or creature spells" is the mirror image a few groups up: it
  -- pays the creature spell and refuses the Star's {1}, where these two refuse
  -- the spell and pay the {1}.
  --
  -- Same control, and it discriminates for the same reason: the Island cannot
  -- pay the white pip either, so the Star is still the only source of it.
  Spec.it s "CR 605.3a the same mana pays it inside a cast's mana window" $ do
    hawker <- S.printingOf s registry "Omen Hawker"
    star <- S.printingOf s registry "Chromatic Star"
    traveler <- S.printingOf s registry "Doomed Traveler"
    island <- S.printingOf s registry "Island"
    let (_, g1) = S.addCreature hawker S.alice (Setup.emptyGame S.bothPlayers)
        (_, board) = S.addCreature star S.alice g1
        -- S.handOne rather than S.addHandCard: it is what puts the board in a
        -- MAIN PHASE with priority, which the creature spell's sorcery timing
        -- needs (CR 601.3a).
        casts gs =
          let (held, travelerId) = S.handOne traveler gs
           in S.castable S.alice travelerId held
    Spec.assertBool s (casts board) "CR 605.3a the Star is activated inside the cast's mana window and mints the {W}"
    Spec.assertBool s (casts (S.landsFor island S.alice 1 board)) "one untapped Island pays that same {1}, so the board is otherwise fine"

-- One of the Hawker's two: from a source that is not snow, lost as the phase
-- ends, and spendable only to activate an ability -- any ability, the card
-- naming none, which is Pawl.Types.Filter's trivial @And []@.
hawkerMana :: ManaType.ManaType -> ManaUnit.ManaUnit
hawkerMana manaType =
  ManaUnit.MkManaUnit
    { ManaUnit.manaType = manaType,
      ManaUnit.tags = Set.empty,
      ManaUnit.retention = ManaRetention.Ordinary,
      ManaUnit.restriction =
        Just
          ManaRestriction.MkManaRestriction
            { ManaRestriction.casts = Nothing,
              ManaRestriction.activations = Just (Filter.And [])
            },
      ManaUnit.rider = Nothing
    }

-- CR 106.6's SECOND shape, and the one no card in the pool reached before: a
-- mana-producing ability with "an additional effect that affects the spell or
-- ability that mana is spent on". Boseiju, Who Shelters All (Legendary Land,
-- "Boseiju enters tapped." / "{T}, Pay 2 life: Add {C}. If that mana is spent on
-- an instant or sorcery spell, that spell can't be countered.") is the printing
-- -- Champions of Kamigawa is the paper set.
--
-- Nothing is omitted from the card, so pawl's Boseiju is neither stricter nor
-- weaker than printed.
--
-- THE producer for this rule rather than Delighted Halfling below, for two
-- reasons that are both about what the board can tell apart. Boseiju prints NO
-- CR 106.6 restriction, so an engine that read the restriction's filter as the
-- rider's predicate cannot come out right here by luck; and its rider carries a
-- real predicate -- "an instant or sorcery spell" -- where the Halfling's
-- narrows nothing, so the condition field is proven rather than merely present.
boseijuSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
boseijuSpec s registry = Spec.describe s "Boseiju, Who Shelters All" $ do
  -- The pair, one mana source apart. Divination ({2}{U} Sorcery, "Draw two
  -- cards") is the victim, and alice's three lands are exactly its three mana --
  -- so on the Boseiju board its colourless necessarily goes in, and no answerer
  -- decides that.
  --
  -- The asserted quantity is alice's HAND once the stack is empty, which is
  -- Divination's own effect and not the zone it ends in: CR 701.6a puts a
  -- countered sorcery in the graveyard and CR 608.2n puts a resolved one there
  -- too, so a zone read cannot tell the two apart for an instant or a sorcery --
  -- which is exactly the class Boseiju's rider is about.
  --
  -- The stack is walked ALL the way down rather than resolved once: resolving
  -- only the top object resolves Cancel, and PR #1806's counter case is the
  -- recorded failure of reading the board at that moment.
  Spec.it s "CR 106.6 the rider on the mana that paid stops the sorcery being countered" $ do
    boseiju <- S.printingOf s registry "Boseiju, Who Shelters All"
    island <- S.printingOf s registry "Island"
    divination <- S.printingOf s registry "Divination"
    cancel <- S.printingOf s registry "Cancel"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, withBoseiju) = counteredAfter piker cancel island (S.landsFor boseiju S.alice 1 (S.landsInPlay island 2)) divination
        (_, allIslands) = counteredAfter piker cancel island (S.landsInPlay island 3) divination
    Spec.assertEqWith s "CR 106.6 Cancel counters nothing, so Divination resolves and draws two" (S.handSize S.alice withBoseiju) 2
    Spec.assertEqWith s "and one Island in Boseiju's place lets the same Cancel counter the same sorcery" (S.handSize S.alice allIslands) 0
    -- Only now the proxies, both of them behind the assertion above.
    Spec.assertBool s (not (any isSpellCountered (S.eventsOf withBoseiju))) "CR 701.6a nothing was countered"
    Spec.assertBool s (any isSpellCountered (S.eventsOf allIslands)) "and on the control board something was"

  -- The rider's CONDITION, which the case above cannot reach: its board would
  -- pass with the predicate widened to Pawl.Types.Filter's trivial @And []@.
  -- Erudite Wizard ({2}{U} Creature) costs what Divination costs and is paid the
  -- same way off the same three lands, so the boards differ in the victim's card
  -- type and in nothing else -- and CR 106.6's clause names instants and
  -- sorceries, so this one IS counterable.
  Spec.it s "CR 106.6 the same mana leaves a creature spell counterable" $ do
    boseiju <- S.printingOf s registry "Boseiju, Who Shelters All"
    island <- S.printingOf s registry "Island"
    wizard <- S.printingOf s registry "Erudite Wizard"
    cancel <- S.printingOf s registry "Cancel"
    piker <- S.printingOf s registry "Goblin Piker"
    let alicesLands = S.landsFor boseiju S.alice 1 (S.landsInPlay island 2)
        (_, after) = counteredAfter piker cancel island alicesLands wizard
    Spec.assertEqWith s "CR 701.6a the creature spell was countered, so no creature reached the battlefield" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "and it is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

-- alice casts `victim` off the lands `alicesLands` already gives her, bob
-- answers with Cancel, and the stack is then walked all the way down. bob's three
-- Islands are Cancel's {1}{U}{U} exactly, so the counter never fails for want of
-- mana; alice's library is stocked so that a drawing victim is not decked (CR
-- 104.3c) before the assertion runs.
--
-- S.cast and not S.spellOnStack: that helper bypasses cost payment, which leaves
-- Pawl.Types.Object.manaSpent empty and makes every assertion here vacuous.
counteredAfter ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  GameState.GameState ->
  Printing.Printing ->
  (ObjectId.ObjectId, GameState.GameState)
counteredAfter filler cancel island alicesLands victim =
  let stocked = foldr (\p gs -> snd (S.addLibraryCard p S.alice gs)) alicesLands (replicate 3 filler)
      withBob = S.landsFor island S.bob 3 stocked
      (g1, victimId) = S.handOne victim withBob
      (cancelId, board) = S.addHandCard cancel S.bob g1
      cast_ = S.runPure S.identityAnswer board (S.cast S.alice victimId)
      answered = S.runPure S.identityAnswer cast_ (S.cast S.bob cancelId)
   in (victimId, List.foldl' (\gs _ -> S.runPure S.identityAnswer gs Stack.resolveTop) answered [1 .. 2 :: Int])

-- Was anything countered at all? The proxy the cases above read only after the
-- gameplay-level assertion has already run.
isSpellCountered :: GameEvent.GameEvent -> Bool
isSpellCountered event = case event of
  GameEvent.SpellCountered _ -> True
  _ -> False

-- CR 106.6's two shapes on ONE instruction, which Boseiju cannot show: Delighted
-- Halfling ({G} 1/2 Creature -- Halfling Citizen, "{T}: Add {C}." / "{T}: Add
-- one mana of any color. Spend this mana only to cast a legendary spell, and
-- that spell can't be countered.") is the printing -- The Lord of the Rings:
-- Tales of Middle-earth is the paper set -- and its second ability writes a
-- restriction and a rider at once.
--
-- Nothing is omitted from the card, so pawl's Halfling is neither stricter nor
-- weaker than printed.
--
-- Its rider narrows NOTHING -- "that spell" with no predicate, which is
-- Pawl.Types.Filter's trivial @And []@ -- because the restriction beside it has
-- already forced the mana onto a legendary spell. That is why it cannot stand in
-- for Boseiju above: an implementation reading the RESTRICTION where the rider
-- belongs passes every case here.
delightedHalflingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
delightedHalflingSpec s registry = Spec.describe s "Delighted Halfling" $ do
  -- alice has the Halfling and NO LANDS, so its mana is the only mana in the
  -- game and Tinybones Joins Up ({B} Legendary Enchantment) is paid off it and
  -- off nothing else. An ENCHANTMENT and not an instant, so the countered and
  -- the resolved boards put it in different zones.
  --
  -- The control twin is one Swamp in the Halfling's place: the same {B}, the
  -- same spell, the same Cancel -- and no CR 106.6 clause on it at all.
  Spec.it s "CR 106.6 a restriction and a rider on one instruction are both honoured" $ do
    halfling <- S.printingOf s registry "Delighted Halfling"
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    tinybones <- S.printingOf s registry "Tinybones Joins Up"
    cancel <- S.printingOf s registry "Cancel"
    let withHalfling = snd (S.addCreature halfling S.alice (Setup.emptyGame S.bothPlayers))
        (_, offHalfling) = counteredWith blackYield tinybones cancel island withHalfling
        (_, offSwamp) = counteredWith S.identityAnswer tinybones cancel island (S.landsInPlay swamp 1)
    Spec.assertEqWith s "CR 106.6 the Halfling's mana casts the legendary spell and its rider stops the Cancel" (S.countOnBattlefieldByName (S.printingName tinybones) S.alice offHalfling) 1
    Spec.assertEqWith s "and one Swamp in its place lets the same Cancel counter the same spell" (S.countOnBattlefieldByName (S.printingName tinybones) S.alice offSwamp) 0
    Spec.assertEqWith s "CR 701.6a which put it in alice's graveyard instead" (length (Game.zoneMembers Zone.Graveyard S.alice offSwamp)) 1

  -- The RESTRICTION half of the same instruction, which the case above cannot
  -- separate from the rider: the Halfling's mana is refused to a spell that is
  -- not legendary. Lightning Bolt ({R} Instant) is one mana as Tinybones is, so
  -- what refuses is CR 106.6 and not the amount.
  Spec.it s "CR 106.6 the same mana casts no spell that is not legendary" $ do
    halfling <- S.printingOf s registry "Delighted Halfling"
    tinybones <- S.printingOf s registry "Tinybones Joins Up"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let withHalfling = snd (S.addCreature halfling S.alice (Setup.emptyGame S.bothPlayers))
        (g1, tinybonesId) = S.handOne tinybones withHalfling
        (boltId, board) = S.addHandCard bolt S.alice g1
    Spec.assertBool s (S.castable S.alice tinybonesId board) "the legendary spell is castable off the Halfling"
    Spec.assertBool s (not (S.castable S.alice boltId board)) "CR 106.6 and the one-mana instant beside it is not"

-- counteredAfter's shape for a PERMANENT victim: no library stocking, since
-- nothing draws, and the caster's answerer is the caller's -- the Halfling's
-- any-colour ability needs its colour pinned, a Swamp does not.
counteredWith ::
  (forall r. Prompt.Prompt r -> r) ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  GameState.GameState ->
  (ObjectId.ObjectId, GameState.GameState)
counteredWith answer victim cancel island alicesBoard =
  let withBob = S.landsFor island S.bob 3 alicesBoard
      (g1, victimId) = S.handOne victim withBob
      (cancelId, board) = S.addHandCard cancel S.bob g1
      cast_ = S.runPure answer board (S.cast S.alice victimId)
      answered = S.runPure S.identityAnswer cast_ (S.cast S.bob cancelId)
   in (victimId, List.foldl' (\gs _ -> S.runPure S.identityAnswer gs Stack.resolveTop) answered [1 .. 2 :: Int])

-- Pins CR 105.4's colour choice on the Halfling's second ability to BLACK, by
-- the yield's TYPE alone. Deliberately not by the whole unit the way
-- S.optionYielding matches: the Halfling's units carry a restriction and a
-- rider, so an answerer spelling the expected unit out would have to be edited
-- to keep passing after a mutation dropped either -- which is an answerer
-- repairing the assertion.
blackYield :: Prompt.Prompt r -> r
blackYield p = case p of
  Prompt.ChooseManaYield _ _ _ candidates ->
    let isBlack option = fmap ManaUnit.manaType (Mana.Type.unwrap (ManaOption.yield option)) == [ManaType.Colored Color.Black]
     in Maybe.fromMaybe (NonEmpty.head candidates) (List.find isBlack (NonEmpty.toList candidates))
  _ -> S.identityAnswer p

-- CR 105.4's half of the same arm: an ability that adds mana whose TYPE is not
-- settled. Quirion Sentinel ({1}{G} 2/1 Creature -- Elf Druid, "When this
-- creature enters, add one mana of any color") is the printing, and CR 605.1b
-- keeps it off the mana-ability path for Burning-Tree Emissary's reason -- it
-- triggers from neither a mana ability nor mana being added, so CR 603.3 puts it
-- on the stack and Resolve's Effect.AddMana arm adds the mana.
--
-- Nothing is omitted from the card, so pawl's Sentinel is neither stricter nor
-- weaker than printed.
--
-- The answer is pinned to BLUE, which is not the head of CR 105.1's five (the
-- offer is white, blue, black, red, green): an implementation that picks for the
-- player, or a transcript that ran out and fell back to Replay.defaultAnswer,
-- adds white and fails here.
quirionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
quirionSpec s registry = Spec.describe s "Quirion Sentinel" $ do
  Spec.it s "CR 105.4 the resolving trigger offers the five colours and adds the one answered" $ do
    quirion <- S.printingOf s registry "Quirion Sentinel"
    spy <- S.printingOf s registry "Merfolk Spy"
    let placed = snd (quirionPlaced quirion spy)
        offers = State.execState (Engine.runGame (recordingManaTypes (ManaType.Colored Color.Blue)) placed Stack.resolveTop) []
    Spec.assertEqWith s "asked once, with CR 105.1's five colours" offers [fmap ManaType.Colored [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green]]
    Spec.assertEqWith s "and alice's pool holds the blue that was answered" (poolUnits (quirionResolved (ManaType.Colored Color.Blue) placed)) [plainColor Color.Blue]

  -- The pair that separates "the engine read the answer" from "the engine picked
  -- something". Two boards differing in ONE thing -- the colour answered -- and a
  -- third reading, the CR 105.4 elision, would leave both pools empty.
  Spec.it s "CR 105.4 a different answer is a different pool" $ do
    quirion <- S.printingOf s registry "Quirion Sentinel"
    spy <- S.printingOf s registry "Merfolk Spy"
    let placed = snd (quirionPlaced quirion spy)
    Spec.assertEqWith s "answering red adds red" (poolUnits (quirionResolved (ManaType.Colored Color.Red) placed)) [plainColor Color.Red]
    Spec.assertEqWith s "the stack is empty again" (length (GameState.stack (quirionResolved (ManaType.Colored Color.Red) placed))) 0

  -- Gameplay level: the added mana is ordinary mana, and WHICH colour it is
  -- decides what it pays for. Merfolk Spy costs {U} and the two boards share
  -- seats, timing, priority and the Spy in alice's hand -- the answered colour is
  -- the only difference, and there is no land and no other mana source anywhere.
  Spec.it s "CR 106.4 the blue answer casts a {U} spell and the red one does not" $ do
    quirion <- S.printingOf s registry "Quirion Sentinel"
    spy <- S.printingOf s registry "Merfolk Spy"
    let (spyId, placed) = quirionPlaced quirion spy
        blue = quirionResolved (ManaType.Colored Color.Blue) placed
        red = quirionResolved (ManaType.Colored Color.Red) placed
    Spec.assertBool s (any (S.isCastOf spyId) (Action.legalActions S.alice blue)) "the Spy is castable off the blue"
    Spec.assertBool s (not (any (S.isCastOf spyId) (Action.legalActions S.alice red))) "and is not castable off the red"

-- One Quirion Sentinel entering under alice with its CR 603.6a enters event and
-- that trigger placed on the stack, plus a Merfolk Spy in alice's hand -- alice
-- being active with priority in her precombat main phase (S.handOne). No land and
-- no other mana source is on the board, so the only mana anywhere is what the
-- trigger will add.
--
-- Stops BEFORE the resolution, unlike burningTreeResolved, so the cases below can
-- resolve the one board under several answers.
quirionPlaced :: Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
quirionPlaced quirion spy =
  let (base, spyId) = S.handOne spy (Setup.emptyGame S.bothPlayers)
      (_, entered) = S.entersWithTrigger quirion S.alice base
   in (spyId, snd (Engine.runGamePure S.identityAnswer entered Engine.placePendingTriggers))

-- That board with the trigger resolved, every Prompt.ChooseManaType answered
-- `manaType`.
quirionResolved :: ManaType.ManaType -> GameState.GameState -> GameState.GameState
quirionResolved manaType placed = snd (Engine.runGamePure (answeringManaType manaType) placed Stack.resolveTop)

-- Answers Prompt.ChooseManaType with a NAMED type, deferring everything else to
-- S.identityAnswer. PINNED BY NAME rather than picked out of the candidates, so a
-- mutation to the offer cannot quietly repair the answer.
answeringManaType :: ManaType.ManaType -> Prompt.Prompt r -> r
answeringManaType manaType p = case p of
  Prompt.ChooseManaType {} -> manaType
  _ -> S.identityAnswer p

-- answeringManaType, recording the candidates of every Prompt.ChooseManaType --
-- recordingManaSources' shape, and what tells "asked once with five options" from
-- "asked with one" and from "never asked".
recordingManaTypes :: ManaType.ManaType -> Prompt.Prompt r -> State.State [[ManaType.ManaType]] r
recordingManaTypes manaType p = case p of
  Prompt.ChooseManaType _ _ _ candidates -> do
    State.modify' (<> [NonEmpty.toList candidates])
    pure manaType
  _ -> pure (S.identityAnswer p)

-- One mana of `color` carrying no production tag, plainRed's and plainGreen's
-- generalisation: CR 107.4h reads the SOURCE, and Quirion Sentinel is not snow.
plainColor :: Color.Color -> ManaUnit.ManaUnit
plainColor color = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored color, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}

-- CR 601.2g's window offers one source per interchangeability class rather than
-- one per permanent (#217): three Llanowar Elves that nothing tells apart are one
-- option, and anything that does tell one apart puts it back on the menu.
--
-- Llanowar Elves is the issue's own fixture, and each negative below is a board
-- differing from the positive in exactly one thing. Bonesplitter is the owner's
-- minimal falsifier for collapsing by printed card -- tapping the equipped Elf
-- gives up a 3/1 attacker rather than a 1/1 -- and it separates them through the
-- projection. Elvish Hunter's "target creature doesn't untap during its
-- controller's next untap step" separates them where no projection can see it, on
-- Object.doesNotUntapNext.
interchangeableSourcesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
interchangeableSourcesSpec s registry = Spec.describe s "Interchangeable mana sources" $ do
  Spec.it s "CR 601.2g three indistinguishable Elves are offered as one candidate" $ do
    elf <- S.printingOf s registry "Llanowar Elves"
    let (elves, board) = elfBoard elf 3
        (offers, paid, after) = greenWindow board
    Spec.assertEqWith s "asked once, and the three Elves are one candidate" (fmap length offers) [1]
    Spec.assertBool s paid "the {G} was paid"
    Spec.assertEqWith s "off exactly one Elf" (tappedCount (NonEmpty.toList elves) after) 1

  -- The owner's minimal falsifier for collapsing by printed card: what the
  -- elision must not do is answer "which Elf" for a player whose Elves are a 3/1
  -- and a 1/1. It is caught twice over -- Bonesplitter's +2/+0 is in the
  -- projection, and the Equipment names its host through Object.attachedTo.
  Spec.it s "CR 601.2g an equipped Elf is a candidate of its own" $ do
    elf <- S.printingOf s registry "Llanowar Elves"
    splitter <- S.printingOf s registry "Bonesplitter"
    let (elves, plain) = elfBoard elf 3
        (weapon, armed) = S.addCreature splitter S.alice plain
        board = S.attach weapon (NonEmpty.head elves) armed
        (offers, paid, after) = greenWindow board
    Spec.assertEqWith s "asked once, with the equipped Elf beside the two that are alike" (fmap length offers) [2]
    Spec.assertBool s paid "the {G} was paid"
    Spec.assertEqWith s "off exactly one Elf" (tappedCount (NonEmpty.toList elves) after) 1

  -- The Bonesplitter case above is caught twice over, and this is the one that is
  -- caught ONLY by Object.attachedTo: Betrayal ({U} Enchantment -- Aura, "Enchant
  -- creature an opponent controls / Whenever enchanted creature becomes tapped,
  -- you draw a card.") has no static ability at all, so the Elf it enchants
  -- projects exactly like the two beside it -- and tapping THAT one for mana hands
  -- the Aura's controller a card.
  --
  -- Under BOB, which is where the printed enchant clause puts it: alice is his
  -- opponent, and it is his card the tap would draw.
  --
  -- This is what turns Pawl.Engine.Interchangeable.namedByAnother's attachment arm
  -- from a regression fence into a proved behaviour. Until a trigger condition
  -- watched a tap, every rider in data/cards changed what it was attached to and
  -- the projection refused the pair before that line was reached.
  Spec.it s "CR 601.2g an Elf enchanted by an Aura that changes nothing about it is still a candidate of its own" $ do
    elf <- S.printingOf s registry "Llanowar Elves"
    betrayal <- S.printingOf s registry "Betrayal"
    let (elves, plain) = elfBoard elf 3
        (aura, enchanted) = S.addCreature betrayal S.bob plain
        board = S.attach aura (NonEmpty.head elves) enchanted
        (offers, paid, after) = greenWindow board
    Spec.assertEqWith s "asked once, with the enchanted Elf beside the two that are alike" (fmap length offers) [2]
    -- What makes this case the attachment arm's own: the projection cannot tell
    -- the three apart, so nothing but Object.attachedTo separates them. AFTER the
    -- assertion above, so it cannot absorb a mutation aimed at that arm.
    Spec.assertEqWith
      s
      "and the three Elves project identically, power, toughness and abilities alike"
      (fmap (\oid -> (S.powerToughnessOf oid board, length (Projection.triggeredAbilitiesOf oid board))) (NonEmpty.toList elves))
      (replicate 3 (Just (1, 1), 0))
    Spec.assertBool s paid "the {G} was paid"
    Spec.assertEqWith s "off exactly one Elf" (tappedCount (NonEmpty.toList elves) after) 1

  -- Object.doesNotUntapNext, which Elvish Hunter writes. Nothing about the Elf's
  -- characteristics changes, so this is the case a projection-only test would
  -- collapse: tapping the frozen Elf costs nothing, tapping either other one
  -- costs a whole untap step.
  Spec.it s "CR 601.2g an Elf that will not untap is a candidate of its own" $ do
    elf <- S.printingOf s registry "Llanowar Elves"
    let (elves, plain) = elfBoard elf 3
        board = freeze (NonEmpty.head elves) plain
        (offers, paid, after) = greenWindow board
    Spec.assertEqWith s "asked once, with the frozen Elf beside the two that are alike" (fmap length offers) [2]
    Spec.assertBool s paid "the {G} was paid"
    Spec.assertEqWith s "off exactly one Elf" (tappedCount (NonEmpty.toList elves) after) 1

  -- Object.bindings, the other half of what one object can say about another: a
  -- spell on the stack that took one Elf as its target (CR 601.2c) separates it
  -- from the two it is otherwise identical to, and neither the Elf's own record
  -- nor its projection carries a word about it.
  --
  -- Two steps, because a spell's targets reach Object.bindings only once CR
  -- 601.2i has finished the cast -- the window CR 601.2g opens for that same
  -- cast runs while they are still a local. The Forest pays for the Giant
  -- Growth, so all three Elves are still untapped for the window that follows.
  Spec.it s "CR 601.2g an Elf a spell on the stack targets is a candidate of its own" $ do
    elf <- S.printingOf s registry "Llanowar Elves"
    forest <- S.printingOf s registry "Forest"
    growth <- S.printingOf s registry "Giant Growth"
    let (elves, plain) = elfBoard elf 3
        (land, wooded) = S.addCreature forest S.alice plain
        (board, spell) = S.handOne growth wooded
        aimed = NonEmpty.head elves
        casting :: Prompt.Prompt r -> r
        casting p = case p of
          Prompt.ChooseTargets _ _ _ offer -> S.preferring ((==) (Just aimed) . Recipient.objectOf) offer
          _ -> prefersSource land p
        onStack = S.runPure casting board (S.cast S.alice spell)
        (offers, paid, after) = greenWindow onStack
    -- [1] is what a lost target or a rewound cast would leave, so this is also
    -- the guard that the two steps above did what they say.
    Spec.assertEqWith s "asked once, with the targeted Elf beside the two that are alike" (fmap length offers) [2]
    Spec.assertBool s paid "the {G} was paid"
    Spec.assertEqWith s "off exactly one Elf" (tappedCount (NonEmpty.toList elves) after) 1

  -- The gate over GameState.continuousEffects is still BOARD-WIDE rather than
  -- pairwise, where the id-keyed relations below are searched: a stored
  -- continuous effect anywhere retires the elision, because deciding a pair
  -- against one means asking every effect whether it names one of them, over an
  -- Affected that may be a Filter rather than a set of ids (#1969). Here the
  -- effect sits on the Bonesplitter, which is attached to nothing, so the three
  -- Elves still agree field for field and projection for projection -- and are
  -- still asked about.
  Spec.it s "CR 601.2g a stored continuous effect anywhere retires the elision" $ do
    elf <- S.printingOf s registry "Llanowar Elves"
    splitter <- S.printingOf s registry "Bonesplitter"
    let (elves, plain) = elfBoard elf 3
        (weapon, armed) = S.addCreature splitter S.alice plain
        board = S.withEffect weapon (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 1) (Quantity.Literal 1))) armed
        (offers, paid, after) = greenWindow board
    Spec.assertEqWith s "asked once, with all three Elves on offer" (fmap length offers) [3]
    Spec.assertBool s paid "the {G} was paid"
    Spec.assertEqWith s "off exactly one Elf" (tappedCount (NonEmpty.toList elves) after) 1

  -- GameState.haunting (CR 702.55b) is one of the id-keyed relations
  -- Pawl.Engine.Interchangeable.namedByRelation SEARCHES rather than requires
  -- empty. Here the row names bob's Piker, so it says nothing about any Elf and
  -- the three are still one option -- where the board-wide gate would have
  -- retired the elision for a haunt anywhere on the board.
  --
  -- The control below is the same board with the row's VALUE moved onto an Elf,
  -- which is what makes this a pair differing in exactly one thing rather than a
  -- board that happens to elide.
  Spec.it s "CR 702.55b a haunt row that names no Elf leaves the elision standing" $ do
    elf <- S.printingOf s registry "Llanowar Elves"
    piker <- S.printingOf s registry "Goblin Piker"
    hunter <- S.printingOf s registry "Blind Hunter"
    let (elves, plain) = elfBoard elf 3
        (pikerId, withPiker) = S.addCreature piker S.bob plain
        (hunterId, exiled) = S.addExiledCard hunter S.bob withPiker
        board = haunts hunterId pikerId exiled
        (offers, paid, after) = greenWindow board
    Spec.assertEqWith s "asked once, and the three Elves are one candidate" (fmap length offers) [1]
    Spec.assertBool s paid "the {G} was paid"
    Spec.assertEqWith s "off exactly one Elf" (tappedCount (NonEmpty.toList elves) after) 1

  -- The control, and the load-bearing half: CR 702.55b's "creature it haunts"
  -- names ONE Elf, and nothing about that Elf's own record or projection says so
  -- -- the haunting card is in exile. Tapping it is a different act from tapping
  -- either of the other two, so the prompt must still be asked, with the haunted
  -- Elf beside the two that are alike.
  Spec.it s "CR 702.55b an Elf a haunting card in exile haunts is a candidate of its own" $ do
    elf <- S.printingOf s registry "Llanowar Elves"
    piker <- S.printingOf s registry "Goblin Piker"
    hunter <- S.printingOf s registry "Blind Hunter"
    let (elves, plain) = elfBoard elf 3
        (_, withPiker) = S.addCreature piker S.bob plain
        (hunterId, exiled) = S.addExiledCard hunter S.bob withPiker
        board = haunts hunterId (NonEmpty.head elves) exiled
        (offers, paid, after) = greenWindow board
    Spec.assertEqWith s "asked once, with the haunted Elf beside the two that are alike" (fmap length offers) [2]
    -- After the offer assertion, so it cannot absorb a mutation aimed at the
    -- relation: nothing but GameState.haunting separates these three.
    Spec.assertEqWith
      s
      "and the three Elves project identically, power, toughness and abilities alike"
      (fmap (\oid -> (S.powerToughnessOf oid board, length (Projection.triggeredAbilitiesOf oid board))) (NonEmpty.toList elves))
      (replicate 3 (Just (1, 1), 0))
    Spec.assertBool s paid "the {G} was paid"
    Spec.assertEqWith s "off exactly one Elf" (tappedCount (NonEmpty.toList elves) after) 1

-- Alice's `n` copies of one printing, and their ids in the order they arrived.
elfBoard :: Printing.Printing -> Int -> (NonEmpty.NonEmpty ObjectId.ObjectId, GameState.GameState)
elfBoard printing n =
  let (first, placed) = S.addCreature printing S.alice (Setup.emptyGame S.bothPlayers)
      (rest, final) =
        List.foldl'
          (\(oids, gs) _ -> let (oid, next) = S.addCreature printing S.alice gs in (oids <> [oid], next))
          ([], placed)
          (replicate (n - 1) ())
   in (first NonEmpty.:| rest, final)

-- Alice paying {G} out of CR 601.2g's window: what it offered, whether the cost
-- was paid, and the board it left.
greenWindow :: GameState.GameState -> ([[ObjectId.ObjectId]], Bool, GameState.GameState)
greenWindow board =
  let cost = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]
      ((paid, after), offers) = State.runState (Engine.runGame recordingManaSources board (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice cost)) []
   in (offers, paid, after)

-- How many of these permanents are tapped.
tappedCount :: [ObjectId.ObjectId] -> GameState.GameState -> Int
tappedCount oids gs = length (filter (\oid -> fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Tapped) oids)

-- A fixture write standing in for haunt's dies trigger (CR 702.55b): the row
-- saying which object a card already in exile haunts. Blind Hunter writing one
-- through the real trigger is Pawl.ZoneTriggerSpec's "CR 702.55c whole card"
-- case; only the ROW matters here, and writing it by hand is what keeps the two
-- boards above one field apart.
haunts :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
haunts hauntingCard haunted gs =
  gs
    { GameState.haunting = Map.insert hauntingCard haunted (GameState.haunting gs)
    }

-- A fixture write standing in for Elvish Hunter's resolution, sicken's shape.
freeze :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
freeze oid gs =
  gs
    { GameState.objects = Map.adjust (\o -> o {Object.doesNotUntapNext = True}) oid (GameState.objects gs)
    }

-- One mana of one type carrying no production tag: what a basic land really puts
-- in a pool, and the unit the Celestial Dawn cases below seat directly.
plainOf :: ManaType.ManaType -> ManaUnit.ManaUnit
plainOf manaType = ManaUnit.MkManaUnit {ManaUnit.manaType = manaType, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}

-- A cost of exactly one symbol, so a payability answer is about that symbol and
-- nothing else.
oneSymbol :: ManaSymbol.ManaSymbol -> ManaCost.ManaCost
oneSymbol symbol = ManaCost.MkManaCost [symbol]

-- Alice's board with Celestial Dawn out and `units` seated in her pool, paired
-- with the same board WITHOUT the enchantment. Every case below reads both, so
-- each answer is a pair of boards differing in exactly one permanent.
dawnBoards :: Printing.Printing -> [ManaUnit.ManaUnit] -> (GameState.GameState, GameState.GameState)
dawnBoards dawn units =
  let seated = Mana.setPool S.alice (Mana.Type.MkMana units) (Setup.emptyGame S.bothPlayers)
   in (snd (S.addCreature dawn S.alice seated), seated)

-- Can this player pay this cost on this board? The payABILITY half; the cases
-- that also drive a payment say so.
payable :: PlayerId.PlayerId -> ManaCost.ManaCost -> GameState.GameState -> Bool
payable = Mana.canPay Cost.manaActivations

plainRed :: ManaUnit.ManaUnit
plainRed = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Mana" $ do
  treasonousOgreSpec s registry
  sharedVictimSpec s registry
  sharedTapSpec s registry
  selfTapSpec s registry
  crewedBatterySpec s registry
  villageRitesSpec s registry
  chromaticSpec s registry
  millikinSpec s registry
  burningTreeSpec s registry
  stadiumVendorsSpec s registry
  shizukoSpec s registry
  avatarRokuSpec s registry
  geosurgeSpec s registry
  workshopSpec s registry
  lastingSpringSpec s registry
  omenHawkerSpec s registry
  boseijuSpec s registry
  delightedHalflingSpec s registry
  quirionSpec s registry
  interchangeableSourcesSpec s registry
