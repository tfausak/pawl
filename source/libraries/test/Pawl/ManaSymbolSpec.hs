{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Mana over the symbols themselves (CR 107.4): snow, hybrid,
-- monocolored hybrid and Phyrexian mana, total cost, and choosing which mana to
-- spend. Split out of Pawl.ManaSpec, which keeps the machinery.
module Pawl.ManaSymbolSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Int as Int
import Pawl.ManaSourceSpec (dawnBoards, oneSymbol, payable, plainGreen, plainOf, plainRed, poolOf, retainedGreen)
import Pawl.ManaSpec (atLife, avoidsSource, castOffBoard, poolSize, poolTypes, poolUnits, prefersColor, prefersSource, resolvedCreature, theAbility)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.HybridPayment as HybridPayment
import qualified Pawl.Types.HybridPhyrexian as HybridPhyrexian
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProductionTag as ProductionTag
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEntry as ReplacementEntry
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Zone as Zone

-- CR 609.4b: "If an effect allows a player to spend mana 'as though it were mana
-- of any [type or color],' this affects only how the player may pay a cost. It
-- doesn't change that cost, and it doesn't change what mana was actually spent to
-- pay that cost."
--
-- Celestial Dawn's third sentence prints that rule as a CR 613.11 continuous
-- effect on a PLAYER: "You may spend white mana as though it were mana of any
-- color. You may spend other mana only as though it were colorless mana." Both halves are here because they land together -- the
-- permission alone would leave the card more permissive than printed, which is
-- what pawl's Celestial Dawn used to be.
--
-- Every case is a PAIR of boards differing only in whether the enchantment is
-- out, because a payability answer on one board passes for reasons nobody chose.
-- The pool is seated directly rather than tapped for: Celestial Dawn's own first
-- clause makes every land alice controls a Plains, so a non-white mana cannot
-- come off a land she controls at all, and a fixture that forgot it would assert
-- about white mana twice.
celestialDawnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
celestialDawnSpec s registry = Spec.describe s "Celestial Dawn" $ do
  -- The permission, at the level a player sees it: the mana is spent and the
  -- pool is one unit shorter. Payability is asserted beside it, because
  -- Pawl.Engine.Mana keeps `canPay`'s Hall condition and `spend`'s exact search
  -- in agreement through one `serves`, and a supply rewrite applied to only one
  -- of them would offer a cast it then could not pay.
  Spec.it s "CR 609.4b white mana pays a {U} cost" $ do
    dawn <- S.printingOf s registry "Celestial Dawn"
    let (with, without) = dawnBoards dawn [plainOf (ManaType.Colored Color.White)]
        blue = oneSymbol (ManaSymbol.OfType (ManaType.Colored Color.Blue))
    Spec.assertBool s (payable S.alice blue with) "under Celestial Dawn the {U} cost is payable"
    Spec.assertBool s (not (payable S.alice blue without)) "and without it the same white mana cannot"
    let (paid, after) = S.runPureWith S.identityAnswer with (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice blue)
    Spec.assertBool s paid "the payment goes through, not merely the gate"
    Spec.assertEqWith s "and it spent the white unit" (poolUnits after) []

  -- The half that makes the issue rules-correctness rather than a gap: before
  -- this, alice's red mana paid a {R} cost, which the card forbids. The generic
  -- leg is the control -- "other mana" is still mana, so {1} is still payable --
  -- and the seated unit's own type is asserted so the case cannot pass by having
  -- seated white mana after all.
  Spec.it s "CR 609.4b non-white mana is spendable only as colorless" $ do
    dawn <- S.printingOf s registry "Celestial Dawn"
    let red = plainOf (ManaType.Colored Color.Red)
        (with, without) = dawnBoards dawn [red]
        redCost = oneSymbol (ManaSymbol.OfType (ManaType.Colored Color.Red))
        colorless = oneSymbol (ManaSymbol.OfType ManaType.Colorless)
    Spec.assertEqWith s "the seated unit really is red" (poolUnits with) [red]
    Spec.assertBool s (not (payable S.alice redCost with)) "under Celestial Dawn the {R} cost is not payable"
    Spec.assertBool s (payable S.alice redCost without) "and without it the same mana pays it"
    Spec.assertBool s (payable S.alice colorless with) "the same red mana pays {C}, which is what it may be spent as"
    Spec.assertBool s (payable S.alice (oneSymbol (ManaSymbol.Generic 1)) with) "and {1}, which CR 107.4b lets any mana pay"
    let (paid, _) = S.runPureWith S.identityAnswer with (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice redCost)
    Spec.assertBool s (not paid) "the payment fails too, not only the gate"

  -- CR 609.4b's wording is "mana of any COLOR", which is CR 106.1a's five and not
  -- CR 106.1b's six. The white mana that pays any coloured cost still cannot pay
  -- CR 107.4c's {C}.
  Spec.it s "CR 106.1a any COLOR is five types, so white mana still cannot pay {C}" $ do
    dawn <- S.printingOf s registry "Celestial Dawn"
    let (with, _) = dawnBoards dawn [plainOf (ManaType.Colored Color.White)]
        colorless = oneSymbol (ManaSymbol.OfType ManaType.Colorless)
        green = oneSymbol (ManaSymbol.OfType (ManaType.Colored Color.Green))
    Spec.assertBool s (payable S.alice green with) "the fifth colour is payable"
    Spec.assertBool s (not (payable S.alice colorless with)) "and {C} is not"

  -- CR 107.4h: the {S} symbol constrains where a mana came from, and CR 609.4b
  -- speaks only of types and colors. So the clause widens what a mana may be
  -- spent AS without making a nonsnow mana snow.
  Spec.it s "CR 107.4h the clause does not make a nonsnow white mana pay {S}" $ do
    dawn <- S.printingOf s registry "Celestial Dawn"
    let snowWhite = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.White, ManaUnit.tags = Set.singleton ProductionTag.Snow, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}
        (plainBoard, _) = dawnBoards dawn [plainOf (ManaType.Colored Color.White)]
        (snowBoard, _) = dawnBoards dawn [snowWhite]
    Spec.assertBool s (not (payable S.alice snowCost plainBoard)) "the nonsnow white mana does not pay {S}"
    Spec.assertBool s (payable S.alice snowCost snowBoard) "and the snow one does, so the tags rode through"

  -- CR 613.11 through the carrier's PlayerScope.You: "you may spend" is one
  -- player's permission, and a rewrite applied to the table would be invisible on
  -- a one-seat board. Bob's board is alice's with the pool seated under him
  -- instead, so the two differ only in whose mana it is.
  Spec.it s "CR 613.11 only Celestial Dawn's controller spends mana that way" $ do
    dawn <- S.printingOf s registry "Celestial Dawn"
    let red = plainOf (ManaType.Colored Color.Red)
        white = plainOf (ManaType.Colored Color.White)
        seat pid units = snd (S.addPermanent dawn S.alice (Mana.setPool pid (Mana.Type.MkMana units) (Setup.emptyGame S.bothPlayers)))
        redCost = oneSymbol (ManaSymbol.OfType (ManaType.Colored Color.Red))
        blue = oneSymbol (ManaSymbol.OfType (ManaType.Colored Color.Blue))
    Spec.assertBool s (payable S.bob redCost (seat S.bob [red])) "bob's red mana still pays {R}"
    Spec.assertBool s (not (payable S.alice redCost (seat S.alice [red]))) "where alice's does not"
    Spec.assertBool s (not (payable S.bob blue (seat S.bob [white]))) "and bob's white mana does not pay {U}"
    Spec.assertBool s (payable S.alice blue (seat S.alice [white])) "where alice's does"

  -- CR 613.11 covers every cost this player pays, not only a spell's. Nothing in
  -- Pawl.Engine.Activate threads the clause -- the rewrite is read off the board
  -- inside the payability and payment funnels -- so an activation is covered by
  -- construction, and this is the case that says so rather than the PR body.
  -- Nessian Asp's monstrosity costs {6}{G}, which seven white mana pay only under
  -- the enchantment.
  Spec.it s "CR 613.11 the clause reaches an activated ability's cost" $ do
    dawn <- S.printingOf s registry "Celestial Dawn"
    asp <- S.printingOf s registry "Nessian Asp"
    let seated = Mana.setPool S.alice (Mana.Type.MkMana (replicate 7 (plainOf (ManaType.Colored Color.White)))) (Setup.emptyGame S.bothPlayers)
        (aspId, without) = S.addPermanent asp S.alice seated
        with = snd (S.addPermanent dawn S.alice without)
        canActivate gs = any (\ability -> Activate.activatable S.alice aspId ability gs) (Activate.abilitiesFor aspId gs)
    Spec.assertBool s (canActivate with) "under Celestial Dawn seven white mana pay the {6}{G}"
    Spec.assertBool s (not (canActivate without)) "and without it the same seven cannot"

  -- The BOARD side of the same question. Pawl.Engine.Mana models an untapped
  -- source as a supply of what it could produce, on a path that never touches the
  -- pool, so a rewrite applied to the pool alone answers this one wrongly -- and
  -- the symptom would be a cast offered and then unpayable. Celestial Dawn's own
  -- first clause supplies the fixture: alice's Forest is a Plains and taps for
  -- white.
  Spec.it s "CR 609.4b an untapped land alice controls pays a {U} cost too" $ do
    dawn <- S.printingOf s registry "Celestial Dawn"
    forest <- S.printingOf s registry "Forest"
    let land = S.landsInPlay forest 1
        with = snd (S.addPermanent dawn S.alice land)
        blue = oneSymbol (ManaSymbol.OfType (ManaType.Colored Color.Blue))
    Spec.assertBool s (payable S.alice blue with) "the Forest-turned-Plains pays {U}"
    Spec.assertBool s (not (payable S.alice blue land)) "and without the enchantment it pays neither {U} nor, being a Forest, white"
    let (paid, after) = S.runPureWith S.identityAnswer with (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice blue)
    Spec.assertBool s paid "and the payment really taps it"
    Spec.assertEqWith s "with nothing left floating" (poolSize S.alice after) 0

-- Icehide Golem's whole printed cost. Restated rather than read off the card,
-- for the reason javelinCost gives; Pawl.CardsSpec pins it against
-- data/cards/icehide-golem.json.
snowCost :: ManaCost.ManaCost
snowCost = ManaCost.MkManaCost [ManaSymbol.Snow]

-- One red mana, produced by a snow source and by a nonsnow one. These are what
-- tapping a Snow-Covered Mountain and a Mountain really put in a pool, which is
-- itself one of the assertions below; the payment tests then build a pool out of
-- them directly, because `Mana.spend` is a pure function of one.
snowRed :: ManaUnit.ManaUnit
snowRed =
  ManaUnit.MkManaUnit
    { ManaUnit.manaType = ManaType.Colored Color.Red,
      ManaUnit.tags = Set.singleton ProductionTag.Snow,
      ManaUnit.retention = ManaRetention.Ordinary,
      ManaUnit.restriction = Nothing,
      ManaUnit.rider = Nothing
    }

-- CR 107.4h: "When used in a cost, the snow mana symbol {S} represents a cost
-- that can be paid with one mana of any type produced by a snow source (see rule
-- 106.3). Effects that reduce the amount of generic mana you pay don't affect
-- {S} costs. ... Snow is neither a color nor a type of mana."
--
-- Icehide Golem's entire content is that cost -- its oracle text is nothing but
-- the reminder text for it -- so every test here is about the symbol. The pair
-- that carries the rule is the first two: the same card cast off the same
-- {R}-producing Mountain, differing only in CR 205.4g's supertype.
snowSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
snowSpec s registry = Spec.describe s "Snow" $ do
  Spec.it s "CR 107.4h a Snow-Covered Mountain's mana pays {S}, and Icehide Golem resolves" $ do
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    icehideGolem <- S.printingOf s registry "Icehide Golem"
    let after = resolvedCreature snowMountain icehideGolem 1
    Spec.assertEqWith s "the Golem is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Icehide Golem") S.alice after) 1
    Spec.assertEqWith s "the Snow-Covered Mountain paid for it" (S.tappedCount S.alice after) 1
    Spec.assertEqWith s "nothing left floating" (poolSize S.alice after) 0

  -- The negative half, and it must fail for the RIGHT reason: the board is a
  -- mana source, it is untapped, and it produces exactly the red mana the snow
  -- one does. CR 205.4g's supertype is the only difference, and CR 107.4h asks
  -- for nothing else.
  Spec.it s "CR 107.4h an ordinary Mountain's mana does not pay {S}" $ do
    mountain <- S.printingOf s registry "Mountain"
    icehideGolem <- S.printingOf s registry "Icehide Golem"
    let board = S.landsInPlay mountain 1
        (g, spellId) = S.handOne icehideGolem board
    Spec.assertBool s (not (null (Mana.manaSources Cost.manaActivations S.alice board))) "the Mountain IS a mana source"
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [redSymbol]) board) "and it pays {R}"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice snowCost board)) "but it does not pay {S}"
    Spec.assertBool s (not (S.castable S.alice spellId g)) "so the Golem cannot be cast"

  -- CR 107.4h's second sentence, from the other end: "Effects that reduce the
  -- amount of generic mana you pay don't affect {S} costs." An {S} that were
  -- Generic 1 would be paid by any one mana, so a board that pays {1} six times
  -- over and still cannot pay {S} is what says the two are different symbols.
  Spec.it s "CR 107.4h {S} is not generic: no number of nonsnow Mountains pays it" $ do
    mountain <- S.printingOf s registry "Mountain"
    let board = S.landsInPlay mountain 6
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 6]) board) "six Mountains pay {6}"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice snowCost board)) "and none of them pays {S}"

  -- The tag narrows nothing ELSE. CR 107.4h's last sentence -- "Snow is neither
  -- a color nor a type of mana" -- cuts both ways: a Snow-Covered Mountain's
  -- mana is red mana, so Skred's {R} is paid by it exactly as a Mountain's is.
  Spec.it s "CR 107.4h a snow source's mana is still its own type, and pays {R}" $ do
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    Spec.assertBool
      s
      (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [redSymbol]) (S.landsInPlay snowMountain 1))
      "a Snow-Covered Mountain pays {R}"

  -- CR 106.3: "If mana is produced by an ability, the source of that mana is the
  -- source of that ability." The tag is on the MANA, put there when it was
  -- produced -- which is why this is asked of the pool and not of the land.
  Spec.it s "CR 106.3 the mana a Snow-Covered Mountain adds is tagged snow; a Mountain's is not" $ do
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    mountain <- S.printingOf s registry "Mountain"
    let tapFirst land =
          let board = S.landsInPlay land 1
           in case Game.zoneMembers Zone.Battlefield S.alice board of
                [] -> []
                oid : _ -> poolUnits (S.runPure S.identityAnswer board (Cost.tapForMana S.manaPerformer oid))
    Spec.assertEqWith s "the snow one" (tapFirst snowMountain) [snowRed]
    Spec.assertEqWith s "the plain one" (tapFirst mountain) [plainRed]

  -- The assignment, not merely the count. Both units are red and only one is
  -- snow, so a payment that took the head of the pool would be right half the
  -- time -- hence both orders.
  Spec.it s "CR 107.4h payment spends the snow mana out of a mixed pool, whichever end it is at" $ do
    Spec.assertEqWith
      s
      "snow first"
      (Mana.spend [] ManaSpending.AsProduced 0 snowCost (Mana.Type.MkMana [snowRed, plainRed]))
      (Just (Mana.Type.MkMana [plainRed], 0))
    Spec.assertEqWith
      s
      "snow last"
      (Mana.spend [] ManaSpending.AsProduced 0 snowCost (Mana.Type.MkMana [plainRed, snowRed]))
      (Just (Mana.Type.MkMana [plainRed], 0))

  -- CR 202.2d's colour-granting list names the hybrid and Phyrexian symbols and
  -- not this one, because of CR 107.4h's last sentence: "Snow is neither a color
  -- nor a type of mana." So a card whose whole mana cost is {S} is colorless (CR
  -- 202.2b) -- the sibling of monocoloredHybridSpec's "CR 107.4e a monocolored
  -- hybrid symbol is its coloured half, and only that", which is the same read
  -- taken of a symbol that DOES grant one.
  Spec.it s "CR 202.2b Icehide Golem is colorless: its {S} grants no colour" $ do
    icehideGolem <- S.printingOf s registry "Icehide Golem"
    let (oid, gs) = S.addPermanent icehideGolem S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "colorless" (Projection.colorsOf oid gs) Set.empty

-- One colorless mana carrying no production tag: what CR 106.11 turns an "add
-- {S}" into when the source is not snow, and the unit every assertion below
-- compares against.
plainColorless :: ManaUnit.ManaUnit
plainColorless = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colorless, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}

-- CR 106.11: "If an effect would add mana represented by one or more snow mana
-- symbols to a player's mana pool, that much colorless mana is added to that
-- player's mana pool."
--
-- A rewrite rule, and the two halves of the rewrite are separable. The AMOUNT
-- and the TYPE come from this rule -- one colorless mana per symbol. Whether
-- that mana is SNOW does not: CR 107.4h and CR 106.3 make that a fact about the
-- source, so it is exactly as true of an "add {S}" as of an "add {C}".
--
-- Synthetic Snow Symbol is deliberately a NONSNOW land (search of Scryfall on
-- 2026-08-06 with include_extras, for o:"add {S}", o:"adds {S}" and
-- oracle:"{S}" oracle:add, finds no printing that adds {S} at all, snow or
-- otherwise). Nonsnow is what makes these tests about CR 106.11 rather than
-- about snow sources: a snow printing would let productionTagsGiven, which
-- snowSpec already covers, carry the {S} assertions on its own.
snowSymbolSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
snowSymbolSpec s registry = Spec.describe s "SyntheticSnowSymbol" $ do
  -- The rewrite itself, read off the pool: two symbols, two mana, colorless, and
  -- untagged. The first three are CR 106.11 and each is a separate way to get it
  -- wrong; the fourth is CR 107.4h, and is here because the pool is the only
  -- place a tag stamped by mistake would show.
  Spec.it s "CR 106.11 tapping it adds two colorless mana, neither of them snow" $ do
    snowSymbol <- S.printingOf s registry "Synthetic Snow Symbol"
    let board = S.landsInPlay snowSymbol 1
        tapped = case Game.zoneMembers Zone.Battlefield S.alice board of
          [] -> []
          oid : _ -> poolUnits (S.runPure S.identityAnswer board (Cost.tapForMana S.manaPerformer oid))
    Spec.assertEqWith s "two untagged colorless mana" tapped [plainColorless, plainColorless]

  -- CR 107.4h from the other side, and THE case this card exists for: the symbol
  -- that produced the mana says nothing about whether the mana is snow. The
  -- control inverts both halves at once -- a Snow-Covered Mountain is a snow
  -- source whose mana was never written {S}, and it is the one that pays. So the
  -- negative here cannot be passing merely because the board is short of mana:
  -- the same board casts a {2} spell (the Liquimetal Coating case below).
  Spec.it s "CR 107.4h {S}-produced mana from a nonsnow source does not pay {S}" $ do
    snowSymbol <- S.printingOf s registry "Synthetic Snow Symbol"
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    icehideGolem <- S.printingOf s registry "Icehide Golem"
    let board = S.landsInPlay snowSymbol 1
        (g, spellId) = S.handOne icehideGolem board
        control = S.landsInPlay snowMountain 1
        (cg, controlSpellId) = S.handOne icehideGolem control
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 2]) board) "it pays {2}"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice snowCost board)) "but it does not pay {S}"
    Spec.assertBool s (not (S.castable S.alice spellId g)) "so the Golem cannot be cast off it"
    Spec.assertBool s (S.castable S.alice controlSpellId cg) "though it can off a Snow-Covered Mountain"

  -- CR 106.11's "colorless", pinned where a colour would show: an {S} rewritten
  -- to a coloured mana would pay a coloured symbol, and this board pays none.
  Spec.it s "CR 106.11 the mana is colorless, so it pays no coloured symbol" $ do
    snowSymbol <- S.printingOf s registry "Synthetic Snow Symbol"
    let board = S.landsInPlay snowSymbol 1
        pays color = Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored color)]) board
    Spec.assertBool s (not (any pays [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green])) "no colour is payable"
    Spec.assertBool
      s
      (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.OfType ManaType.Colorless]) board)
      "but {C} is"

  -- The gameplay-level proof (design.md section 4): a real spell cast end to end
  -- off the one synthetic permanent. Liquimetal Coating is a plain {2} Artifact,
  -- so the two mana CR 106.11 produced are the whole of what pays for it.
  Spec.it s "CR 601.2g Liquimetal Coating is cast off a lone Synthetic Snow Symbol" $ do
    snowSymbol <- S.printingOf s registry "Synthetic Snow Symbol"
    liquimetalCoating <- S.printingOf s registry "Liquimetal Coating"
    let resolved = castOffBoard S.identityAnswer [snowSymbol] liquimetalCoating
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "the Coating resolved" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Liquimetal Coating") S.alice resolved) 1
    Spec.assertEqWith s "the land is tapped" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and both mana were spent" (poolSize S.alice resolved) 0

-- alice controls `n` copies of `first` and `m` copies of `second`, and nothing
-- else. Both are lands in every caller, but nothing here requires it.
mixedLands :: Printing.Printing -> Printing.Printing -> Int -> Int -> GameState.GameState
mixedLands first second n m =
  let base = S.landsInPlay first n
   in List.foldl' (\g _ -> snd (S.addPermanent second S.alice g)) base [1 .. m]

redGreen :: ManaSymbol.ManaSymbol
redGreen = ManaSymbol.Hybrid (Hybrid.MkHybrid (ManaType.Colored Color.Red) (ManaType.Colored Color.Green))

redSymbol :: ManaSymbol.ManaSymbol
redSymbol = ManaSymbol.OfType (ManaType.Colored Color.Red)

-- CR 107.4e: "A hybrid symbol such as {W/U} can be paid with either white or blue
-- mana." Its example is exactly this shape: "{G/W}{G/W} can be paid by spending
-- {G}{G}, {G}{W}, or {W}{W}."
hybridSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hybridSpec s registry = Spec.describe s "Hybrid" $ do
  Spec.it s "CR 107.4e one {R/G} is payable from either half, and from neither otherwise" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    let cost = ManaCost.MkManaCost [redGreen]
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice cost (S.landsInPlay mountain 1)) "a Mountain pays it"
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice cost (mixedLands mountain forest 0 1)) "a Forest pays it"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice cost (S.landsInPlay island 1))) "an Island does not"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice cost (S.landsInPlay mountain 0))) "and nothing does not"

  -- THE case a greedy left-to-right match gets wrong, and the reason
  -- Mana.spendDemands searches instead of folding. One Mountain and one
  -- Forest pay {R/G}{R} only if the hybrid takes the GREEN; handing it the
  -- red first strands the {R} with a Forest still untapped.
  Spec.it s "CR 107.4e {R/G}{R} off one Mountain and one Forest: the hybrid must take the GREEN" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let cost = ManaCost.MkManaCost [redGreen, redSymbol]
        gs = mixedLands mountain forest 1 1
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice cost gs) "canPay says yes"
    let (paid, after) = S.runPureWith S.identityAnswer gs (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice cost)
    Spec.assertBool s paid "and it really is paid"
    Spec.assertEqWith s "both lands tapped" (S.tappedCount S.alice after) 2
    Spec.assertEqWith s "nothing left floating" (poolSize S.alice after) 0

  -- The twin: the same cost with no red anywhere is unpayable, so the case
  -- above is not "hybrids always succeed".
  Spec.it s "CR 107.4e {R/G}{R} off two Forests is unpayable -- the {R} has no source" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let cost = ManaCost.MkManaCost [redGreen, redSymbol]
        gs = mixedLands mountain forest 0 2
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice cost gs)) "canPay says no"
    Spec.assertBool s (not (fst (S.runPureWith S.identityAnswer gs (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice cost)))) "and paying fails"
    Spec.assertEqWith s "two {R/G} alone WOULD be payable from them" (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [redGreen, redGreen]) gs) True

  Spec.it s "CR 107.4e whole card: Burning-Tree Emissary casts off RR, GG, or RG" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    burningTreeEmissary <- S.printingOf s registry "Burning-Tree Emissary"
    let castOff reds greens =
          let (gs, spellId) = S.handOne burningTreeEmissary (mixedLands mountain forest reds greens)
              cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
           in length (GameState.stack cast)
    Spec.assertEqWith s "two Mountains" (castOff 2 0) 1
    Spec.assertEqWith s "two Forests" (castOff 0 2) 1
    Spec.assertEqWith s "one of each" (castOff 1 1) 1
    Spec.assertEqWith s "one land is not enough" (castOff 1 0) 0

  -- CR 601.2b's announcement for CR 107.4e's COLOUR/COLOUR half, and the board
  -- that makes the answer observable. Gyre Engineer ("{T}: Add {G}{U}") is the
  -- oversupply: one activation puts BOTH of {G/U}'s halves in the pool, so
  -- Slippery Bogle ({G/U}) spends one and floats the other. Which one floats is
  -- the announcement, and Llanowar Elves ({G}) in hand is what reads it -- with
  -- the Engineer tapped there is no other source, so the Elves are castable
  -- exactly when the Bogle took the BLUE half.
  --
  -- Asserting the BOARD and not merely the prompt: a prompt whose answer
  -- changed nothing would satisfy the transcript legs alone.
  Spec.it s "CR 601.2b whichever half of {G/U} is announced, the OTHER floats" $ do
    gyreEngineer <- S.printingOf s registry "Gyre Engineer"
    slipperyBogle <- S.printingOf s registry "Slippery Bogle"
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    let (_, board) = S.addPermanent gyreEngineer S.alice (Setup.emptyGame S.bothPlayers)
        (withBogle, bogleId) = S.handOne slipperyBogle board
        (elvesId, gs) = S.addHandCard llanowarElves S.alice withBogle
        -- Resolved, not merely cast: the Elves are a creature spell, and CR
        -- 302.1 lets one be cast only "during a main phase of their turn when
        -- the stack is empty", so the Bogle has to leave the stack before the
        -- mana that floated is any use to them.
        castWith half =
          let ((_, cast), asked) = Replay.record (announcesHalf half) gs (S.cast S.alice bogleId)
           in (asked, cast, snd (S.runPureWith (announcesHalf half) cast Stack.resolveTop))
        (askedGreen, castGreen, afterGreen) = castWith greenMana
        (askedBlue, castBlue, afterBlue) = castWith blueMana
    Spec.assertEqWith s "the green half was announced" (halfAnnouncements askedGreen) [greenMana]
    Spec.assertEqWith s "the blue half was announced" (halfAnnouncements askedBlue) [blueMana]
    Spec.assertEqWith s "both casts reached the stack" (length (GameState.stack castGreen), length (GameState.stack castBlue)) (1, 1)
    Spec.assertEqWith s "and both Bogles resolved" (length (GameState.stack afterGreen), length (GameState.stack afterBlue)) (0, 0)
    Spec.assertEqWith s "green paid, so BLUE floats" (poolTypes S.alice afterGreen) [blueMana]
    Spec.assertEqWith s "blue paid, so GREEN floats" (poolTypes S.alice afterBlue) [greenMana]
    Spec.assertBool s (S.castable S.alice elvesId afterBlue) "the floating {G} casts Llanowar Elves"
    Spec.assertBool s (not (S.castable S.alice elvesId afterGreen)) "the floating {U} does not"

  Spec.it s "CR 107.4e a hybrid symbol is ALL of its component colours" $ do
    burningTreeEmissary <- S.printingOf s registry "Burning-Tree Emissary"
    let (oid, gs) = S.addPermanent burningTreeEmissary S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "red AND green, not one or the other"
      (Projection.colorsOf oid gs)
      (Set.fromList [Color.Red, Color.Green])

greenMana, blueMana :: ManaType.ManaType
greenMana = ManaType.Colored Color.Green
blueMana = ManaType.Colored Color.Blue

redMana, whiteMana :: ManaType.ManaType
redMana = ManaType.Colored Color.Red
whiteMana = ManaType.Colored Color.White

-- The `announces` shape for CR 107.4e's COLOUR/COLOUR hybrid: answers
-- Prompt.AnnounceHybridHalf with `half` whenever it is on offer, and defers
-- everything else to S.identityAnswer.
announcesHalf :: ManaType.ManaType -> Prompt.Prompt r -> r
announcesHalf half p = case p of
  Prompt.AnnounceHybridHalf _ _ _ _ offers ->
    if elem half (NonEmpty.toList offers) then half else NonEmpty.head offers
  _ -> S.identityAnswer p

-- Every colour/colour hybrid announcement the engine asked for, in the order it
-- asked -- CR 601.2b's "for each of those symbols" again.
halfAnnouncements :: [Response.Response] -> [ManaType.ManaType]
halfAnnouncements responses =
  let announcement r = case r of
        Response.AnnouncedHybridHalf half -> Just half
        _ -> Nothing
   in Maybe.mapMaybe announcement responses

-- CR 118.13b: "If a cost paid during the resolution of a spell or ability
-- contains a mana symbol that can be paid in multiple ways, the player paying
-- that cost chooses how to pay for that symbol immediately before they pay that
-- cost." The moment CR 118.13a's two announcements do not cover, reached through
-- Pawl.Engine.Resolve.payGatePaidBy rather than through a cast or an activation.
--
-- Shu Yun, the Silent Tempest, {2}{U} 3/2 with prowess and "Whenever you cast a
-- noncreature spell, you may pay {R/W}{R/W}. If you do, target creature gains
-- double strike until end of turn." The "you may pay ... if you do" is CR
-- 118.12's pay gate, so the cost is paid while the TRIGGER resolves, and its two
-- {R/W} are CR 107.4e symbols payable two ways each.
--
-- Synthetic Speed Boost, a {0} sorcery, is the noncreature spell that fires the
-- trigger: it costs no mana, so the four lands below are all still untapped when
-- the gate is offered. Prowess fires off the same cast and changes nothing here.
--
-- Two Mountains and two Plains, and the SAME source answers on both legs -- the
-- head of every Prompt.ChooseManaSource, which is a Mountain while one is
-- untapped -- so the only difference between the legs is which half the payer
-- announced. Announcing red is paid by the two Mountains and the window closes;
-- announcing white leaves the cost uncovered until both Plains are down, and the
-- {R}{R} already in the pool is then what floats. Lightning Bolt in hand reads
-- it: with every land tapped, the floating red is the only thing that could pay
-- for it.
--
-- Mutate the announcement away and the cost stays {R/W}{R/W}, which the two
-- Mountains cover on either leg: the pool is empty both times and the Bolt is
-- castable neither time, so the first assertion below is the one that reddens.
shuYunSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
shuYunSpec s registry = Spec.describe s "Shu Yun, the Silent Tempest" $ do
  Spec.it s "CR 118.13b the half announced as the trigger resolves is the mana that resolution spends" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    shuYun <- S.printingOf s registry "Shu Yun, the Silent Tempest"
    boost <- S.printingOf s registry "Synthetic Speed Boost"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let lands = S.landsFor plains S.alice 2 (S.landsFor mountain S.alice 2 (Setup.emptyGame S.bothPlayers))
        (shuYunId, withShuYun) = S.addPermanent shuYun S.alice lands
        (boostId, withBoost) = S.addHandCard boost S.alice withShuYun
        (boltId, withBolt) = S.addHandCard bolt S.alice withBoost
        board =
          withBolt
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        -- Pays the gate, announces `half` wherever it is on offer, and aims the
        -- trigger at Shu Yun itself. Everything else is S.identityAnswer, whose
        -- Prompt.ChooseManaSource answer is the head candidate.
        answering :: ManaType.ManaType -> Prompt.Prompt r -> r
        answering half p = case p of
          Prompt.ChooseToPay {} -> PaymentDecision.Pays
          Prompt.AnnounceHybridHalf _ _ _ _ offers ->
            if elem half (NonEmpty.toList offers) then half else NonEmpty.head offers
          Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just shuYunId) . Recipient.objectOf) legal) sets
          _ -> S.identityAnswer p
        -- Cast, then let the step's priority round put both triggers on the
        -- stack and resolve them. NOT a step advance: CR 500.5 empties the pool
        -- as a step ends, and the pool is what the assertions read.
        legOf half = S.runPure (answering half) (S.runPure (answering half) board (S.cast S.alice boostId)) Engine.priorityLoop
        redLeg = legOf redMana
        whiteLeg = legOf whiteMana
    -- THE assertion, and the one the announcement decides: what the payment left
    -- behind, read as a cast the player can now make.
    Spec.assertBool s (S.castable S.alice boltId whiteLeg) "white was announced, so the unspent {R}{R} floats and pays for Lightning Bolt"
    Spec.assertBool s (not (S.castable S.alice boltId redLeg)) "red was announced, so it was spent and the two untapped Plains cannot pay for it"
    -- The gate was paid on BOTH legs, so the difference above is the announcement
    -- and not one leg failing to pay at all.
    Spec.assertBool s (Projection.hasKeyword Keyword.DoubleStrike shuYunId redLeg) "the red route paid, so CR 118.12's IfPaid branch ran"
    Spec.assertBool s (Projection.hasKeyword Keyword.DoubleStrike shuYunId whiteLeg) "and the white route paid too"
    Spec.assertEqWith s "the red route closed the window with the two Mountains down" (S.tappedCount S.alice redLeg) 2
    Spec.assertEqWith s "the white route kept tapping until both Plains were down" (S.tappedCount S.alice whiteLeg) 4
    Spec.assertEqWith s "and what floats is exactly the red the white route did not spend" (poolTypes S.alice whiteLeg) [redMana, redMana]

twoOrRed :: ManaSymbol.ManaSymbol
twoOrRed = ManaSymbol.MonocoloredHybrid (ManaType.Colored Color.Red)

-- Flame Javelin's printed cost. Restated rather than read off the card so that
-- the payment assertions below say what they mean; CardSpec is what pins this
-- against data/cards/flame-javelin.json.
javelinCost :: ManaCost.ManaCost
javelinCost = ManaCost.MkManaCost [twoOrRed, twoOrRed, twoOrRed]

-- CR 107.4e's other half: "a monocolored hybrid symbol such as {2/B} can be paid
-- with either one black mana or two mana of any type."
--
-- Flame Javelin ({2/R}{2/R}{2/R}) throughout, because the symbol only becomes
-- interesting in bulk: one of them is barely distinguishable from {R}, three of
-- them span {R}{R}{R} to {6}.
monocoloredHybridSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
monocoloredHybridSpec s registry = Spec.describe s "MonocoloredHybrid" $ do
  let -- How many objects the stack holds after alice tries to cast the Javelin
      -- with `gs` already on the battlefield: 1 when the cost was paid, 0 when
      -- CR 601.2h rolled the whole attempt back.
      castsOff javelin gs =
        let (g, spellId) = S.handOne javelin gs
         in length (GameState.stack (snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))))

  Spec.it s "CR 107.4e one {2/R} takes one Mountain OR two Islands, and one Island is not enough" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    let cost = ManaCost.MkManaCost [twoOrRed]
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice cost (S.landsInPlay mountain 1)) "one Mountain pays it"
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice cost (S.landsInPlay island 2)) "two Islands pay it"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice cost (S.landsInPlay island 1))) "one Island does not"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice cost (S.landsInPlay island 0))) "and nothing does not"

  -- The coloured route, end to end. Three lands for three symbols is the
  -- reading a payment path that charged every symbol one mana would also
  -- get right, so this is the control the cases below discriminate
  -- against -- and the tap count is what says the route was really taken:
  -- six Mountains would still be three taps, because Cost.payMana stops
  -- as soon as the cost is payable.
  Spec.it s "CR 107.4e whole card: Flame Javelin casts off three Mountains, {R} per symbol" $ do
    mountain <- S.printingOf s registry "Mountain"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let gs = S.landsInPlay mountain 3
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice javelinCost gs) "canPay says yes"
    Spec.assertEqWith s "and it casts" (castsOff flameJavelin gs) 1
    let (paid, after) = S.runPureWith S.identityAnswer (S.landsInPlay mountain 6) (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice javelinCost)
    Spec.assertBool s paid "six Mountains pay it too"
    Spec.assertEqWith s "and only three of them are tapped" (S.tappedCount S.alice after) 3
    Spec.assertEqWith s "with nothing left floating" (poolSize S.alice after) 0

  -- The generic route, with no red mana anywhere on the board.
  Spec.it s "CR 107.4e whole card: Flame Javelin casts off six Islands, two generic per symbol" $ do
    island <- S.printingOf s registry "Island"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let gs = S.landsInPlay island 6
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice javelinCost gs) "canPay says yes"
    Spec.assertEqWith s "and it casts" (castsOff flameJavelin gs) 1

  -- THE discriminating negative. Five Islands is one short of the {6} the
  -- all-generic route needs, and a payment path that charged one mana per
  -- {2/R} would call three of them sufficient, let alone five.
  Spec.it s "CR 107.4e five Islands cannot cast Flame Javelin -- {6} is one mana away" $ do
    island <- S.printingOf s registry "Island"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let gs = S.landsInPlay island 5
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice javelinCost gs)) "canPay says no"
    Spec.assertEqWith s "and it does not cast" (castsOff flameJavelin gs) 0
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice javelinCost (S.landsInPlay island 3))) "three Islands are nowhere near"

  -- CR 107.4e symbol by symbol, which the card's own ruling spells out:
  -- "you can pay for Flame Javelin by spending {R}{R}{R}, {2}{R}{R},
  -- {4}{R}, or {6}." So the routes are chosen per symbol, and a search
  -- that picked one route for the whole cost would reject both of these.
  Spec.it s "CR 107.4e each symbol picks its own route: {R}{R}{2} and {R}{4}" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let cost = javelinCost
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice cost (mixedLands mountain island 2 2)) "two Mountains and two Islands: {R}{R}{2}"
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice cost (mixedLands mountain island 1 4)) "one Mountain and four Islands: {R}{4}"
    Spec.assertEqWith s "and that one really casts" (castsOff flameJavelin (mixedLands mountain island 1 4)) 1
    -- One short of {R}{4} and one red short of {R}{R}{2}: four mana with
    -- only one red pays no route at all.
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice cost (mixedLands mountain island 1 3))) "one Mountain and three Islands: no route"

  -- The gameplay-level proof (design.md section 4): the whole card, cast
  -- and resolved off the all-generic route, doing what it says.
  Spec.it s "CR 107.4e Flame Javelin cast off six Islands resolves for 4 damage" $ do
    island <- S.printingOf s registry "Island"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let (g, spellId) = S.handOne flameJavelin (S.landsInPlay island 6)
        cast = snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "every Island tapped" (S.tappedCount S.alice resolved) 6
    Spec.assertEqWith s "nothing left floating" (poolSize S.alice resolved) 0
    -- S.identityAnswer takes the least Recipient on offer, which with no
    -- creatures anywhere is alice herself. Who it hits is the answer's
    -- business; that it hits for 4 is the card's.
    Spec.assertEqWith s "4 damage to the chosen target" (S.lifeOf S.alice resolved) (Just 16)

  -- What CR 118.13a's announcement leaves behind. Both halves are payable
  -- out of this pool and they leave DIFFERENT pools behind, so the choice
  -- is observable and `spend` makes it: it takes the fewest units. No gameplay
  -- road reaches it any more -- a cast, an activation, a CR 118.12 pay gate, a
  -- special action, a combat toll and a mana ability's own activation cost all
  -- settle every {2/X} through `announce` first -- so this calls `spend`
  -- directly, as a fence under the rule Mana.resolutions still states.
  Spec.it s "CR 601.2b with nothing announced, spend takes a {2/R}'s one-mana half" $
    let red = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}
        colorless = ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colorless, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}
     in Spec.assertEqWith
          s
          "the {R} is spent and both {C} remain -- the other half would spend both {C} and leave the {R}"
          (Mana.spend [] ManaSpending.AsProduced 0 (ManaCost.MkManaCost [twoOrRed]) (Mana.Type.MkMana [red, colorless, colorless]))
          (Just (Mana.Type.MkMana [colorless, colorless], 0))

  -- CR 601.2b: "If a cost that will be paid as the spell is being cast
  -- includes hybrid mana symbols, the player announces the nonhybrid
  -- equivalent cost they intend to pay." CR 118.13a places that as the
  -- controller PROPOSES the spell.
  --
  -- THE proving pair, and both legs are needed: SIX Mountains, so either
  -- route is payable and "the Javelin was cast" is true under both. What
  -- discriminates is the mana actually SPENT -- three taps against six.
  Spec.it s "CR 118.13a Flame Javelin announced as {R}{R}{R} spends three of six Mountains" $ do
    mountain <- S.printingOf s registry "Mountain"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let (gs, spellId) = S.handOne flameJavelin (S.landsInPlay mountain 6)
        (asked, resolved) = castAndResolve (announcesHybrid HybridPayment.PaysTyped) gs spellId
    Spec.assertEqWith s "one announcement per symbol, all coloured" (hybridAnnouncements asked) (replicate 3 HybridPayment.PaysTyped)
    Spec.assertEqWith s "the Javelin resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "three Mountains paid for it" (S.tappedCount S.alice resolved) 3
    Spec.assertEqWith s "with nothing left floating" (poolSize S.alice resolved) 0

  Spec.it s "CR 118.13a the same Javelin announced as {6} spends all six" $ do
    mountain <- S.printingOf s registry "Mountain"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let (gs, spellId) = S.handOne flameJavelin (S.landsInPlay mountain 6)
        (asked, resolved) = castAndResolve (announcesHybrid HybridPayment.PaysGeneric) gs spellId
    Spec.assertEqWith s "one announcement per symbol, all generic" (hybridAnnouncements asked) (replicate 3 HybridPayment.PaysGeneric)
    Spec.assertEqWith s "the Javelin resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "all six Mountains paid for it" (S.tappedCount S.alice resolved) 6
    Spec.assertEqWith s "with nothing left floating" (poolSize S.alice resolved) 0

  -- CR 601.2f's reduction meeting CR 601.2b's announcement, which is Flame
  -- Javelin's own ruling: "a generic cost reduction applies to a
  -- monocolored hybrid spell only if you've chosen a method of paying for
  -- it that includes generic mana." Baral, Chief of Compliance is the
  -- reducer -- "Instant and sorcery spells you cast cost {1} less to
  -- cast" -- and Flame Javelin is an Instant.
  --
  -- The announcement is what makes the ruling expressible: CR 118.7a's
  -- reduction comes off the GENERIC component, and a symbol still spelled
  -- {2/R} has none. Announced as {2}{2}{2} it is {6}, which Baral takes to
  -- {5}; announced as {R}{R}{R} there is nothing generic for Baral to
  -- bite, and the ruling says so.
  Spec.it s "CR 118.7a Baral takes the announced {6} to {5}, and leaves {R}{R}{R} alone" $ do
    mountain <- S.printingOf s registry "Mountain"
    baral <- S.printingOf s registry "Baral, Chief of Compliance"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let (_, board) = S.addPermanent baral S.alice (S.landsInPlay mountain 6)
        (gs, spellId) = S.handOne flameJavelin board
        (askedGeneric, afterGeneric) = castAndResolve (announcesHybrid HybridPayment.PaysGeneric) gs spellId
        (askedTyped, afterTyped) = castAndResolve (announcesHybrid HybridPayment.PaysTyped) gs spellId
    Spec.assertEqWith s "the generic route was announced three times" (hybridAnnouncements askedGeneric) (replicate 3 HybridPayment.PaysGeneric)
    Spec.assertEqWith s "and cost FIVE Mountains, not six" (S.tappedCount S.alice afterGeneric) 5
    Spec.assertEqWith s "the coloured route was announced three times" (hybridAnnouncements askedTyped) (replicate 3 HybridPayment.PaysTyped)
    Spec.assertEqWith s "and still cost three -- Baral has nothing generic to reduce" (S.tappedCount S.alice afterTyped) 3
    Spec.assertEqWith s "both resolved" (length (GameState.stack afterGeneric), length (GameState.stack afterTyped)) (0, 0)

  -- The elision, both directions. Where only ONE route is payable there is
  -- nothing to ask, and the interpreter asking for the other route does
  -- not get it -- CR 601.2b's "previously made choices ... may restrict
  -- the player's options" arriving as a board that offers one option.
  Spec.it s "CR 118.13a six Islands offer no coloured route, so nothing is asked" $ do
    island <- S.printingOf s registry "Island"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let (gs, spellId) = S.handOne flameJavelin (S.landsInPlay island 6)
        (asked, resolved) = castAndResolve (announcesHybrid HybridPayment.PaysTyped) gs spellId
    Spec.assertEqWith s "no choice existed, so none was asked" (hybridAnnouncements asked) []
    Spec.assertEqWith s "the Javelin resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "off all six Islands" (S.tappedCount S.alice resolved) 6

  Spec.it s "CR 118.13a three Mountains cannot afford any {2} half, so nothing is asked" $ do
    mountain <- S.printingOf s registry "Mountain"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let (gs, spellId) = S.handOne flameJavelin (S.landsInPlay mountain 3)
        (asked, resolved) = castAndResolve (announcesHybrid HybridPayment.PaysGeneric) gs spellId
    Spec.assertEqWith s "no choice existed, so none was asked" (hybridAnnouncements asked) []
    Spec.assertEqWith s "the Javelin resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "off all three Mountains" (S.tappedCount S.alice resolved) 3

  -- CR 601.2b names the nonhybrid equivalent cost, and CR 601.2f totals
  -- THAT minus every reduction: announced as {2}{2}{2} the Javelin is
  -- {6}, which Baral takes to {5}. The castability gate asks the same
  -- question over the same completions, so a Javelin payable only through
  -- the reduced generic route is offered -- and the reduction the gate
  -- sees is the one the payment takes. Flame Javelin's own ruling is that
  -- side of it: a generic cost reduction applies to a monocolored hybrid
  -- only where the announced payment includes generic mana.
  Spec.it s "CR 601.2f Baral's reduction reaches the castability gate for a {2/R}" $ do
    island <- S.printingOf s registry "Island"
    baral <- S.printingOf s registry "Baral, Chief of Compliance"
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let withBaral n = S.handOne flameJavelin (snd (S.addPermanent baral S.alice (S.landsInPlay island n)))
        (four, fourId) = withBaral 4
        (five, fiveId) = withBaral 5
        (six, sixId) = withBaral 6
    Spec.assertBool s (S.castable S.alice fiveId five) "five Islands: offered, since CR 601.2f's total is {5}"
    -- The gate is EXACT and not merely looser: the cast it admits
    -- completes, and it completes off all five Islands and no more.
    let (_, offFive) = castAndResolve (announcesHybrid HybridPayment.PaysGeneric) five fiveId
    Spec.assertEqWith s "and the Javelin resolved" (length (GameState.stack offFive)) 0
    Spec.assertEqWith s "off exactly five Islands" (S.tappedCount S.alice offFive) 5
    -- The discriminating negative on the same axis. One Island fewer and
    -- {5} is out of reach through EVERY completion -- {6} through the
    -- generic route, {2}{2}{R} and its siblings through the typed ones,
    -- none of which four Islands pay -- so the gate still refuses, and the
    -- attempted cast still puts nothing on the stack.
    Spec.assertBool s (not (S.castable S.alice fourId four)) "four Islands: still refused, since CR 601.2f's total is {5}"
    let (_, offFour) = castAndResolve (announcesHybrid HybridPayment.PaysGeneric) four fourId
    Spec.assertEqWith s "and nothing was cast" (length (GameState.stack offFour)) 0
    -- The control, and it is what makes the legs above about the
    -- reduction rather than about the board: one more Island and the cast
    -- still pays FIVE, leaving the sixth untapped.
    Spec.assertBool s (S.castable S.alice sixId six) "six Islands: offered"
    let (_, resolved) = castAndResolve (announcesHybrid HybridPayment.PaysGeneric) six sixId
    Spec.assertEqWith s "and only five of the six are tapped" (S.tappedCount S.alice resolved) 5

  -- CR 107.4e's last sentence, as CR 202.2d restates it for the whole
  -- object: a monocolored hybrid's other component is generic mana, which
  -- is no colour, so only the named half counts. Flame Javelin is red
  -- even when six Islands paid for it.
  Spec.it s "CR 107.4e a monocolored hybrid symbol is its coloured half, and only that" $ do
    flameJavelin <- S.printingOf s registry "Flame Javelin"
    let (oid, gs) = S.addPermanent flameJavelin S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "red, not colourless" (Projection.colorsOf oid gs) (Set.singleton Color.Red)

-- Mutagenic Growth's printed cost. Restated rather than read off the card, for
-- the reason javelinCost gives; CardSpec pins it against
-- data/cards/mutagenic-growth.json.
phyrexianCost :: ManaCost.ManaCost
phyrexianCost = ManaCost.MkManaCost [ManaSymbol.Phyrexian Color.Green]

-- Answers Prompt.AnnouncePhyrexianPayment with `way` whenever it is on offer,
-- and defers everything else to S.identityAnswer -- the prefersSource shape. The
-- "whenever it is on offer" is what makes the two elision cases below
-- discriminating: an interpreter asking for the life route on a board that does
-- not offer it must not get it.
announces :: PhyrexianPayment.PhyrexianPayment -> Prompt.Prompt r -> r
announces way p = case p of
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers ->
    if elem way (NonEmpty.toList offers) then way else NonEmpty.head offers
  _ -> S.identityAnswer p

-- The `announces` shape for CR 107.4e's monocolored hybrid: answers
-- Prompt.AnnounceHybridPayment with `way` whenever it is on offer, and defers
-- everything else to S.identityAnswer. The "whenever it is on offer" is what
-- makes the elision cases discriminating -- an interpreter asking for a route the
-- board does not offer must not get it.
announcesHybrid :: HybridPayment.HybridPayment -> Prompt.Prompt r -> r
announcesHybrid way p = case p of
  Prompt.AnnounceHybridPayment _ _ _ _ offers ->
    if elem way (NonEmpty.toList offers) then way else NonEmpty.head offers
  _ -> S.identityAnswer p

-- Every monocolored hybrid announcement the engine asked for, in the order it
-- asked -- CR 601.2b's "for each of those symbols", so the LENGTH is how many of
-- a cost's {2/X} symbols were a real choice and how many were forced.
hybridAnnouncements :: [Response.Response] -> [HybridPayment.HybridPayment]
hybridAnnouncements responses =
  let announcement r = case r of
        Response.AnnouncedHybridPayment way -> Just way
        _ -> Nothing
   in Maybe.mapMaybe announcement responses

-- Was CR 118.13a's announcement actually asked for, or did the engine decide?
wasAskedHowToPayPhyrexian :: [Response.Response] -> Bool
wasAskedHowToPayPhyrexian = not . null . phyrexianAnnouncements

-- Every announcement the engine asked for, in the order it asked -- which is CR
-- 601.2b's "for each of those symbols", so the LENGTH is how many of a cost's
-- Phyrexian symbols were a real choice and how many were forced.
phyrexianAnnouncements :: [Response.Response] -> [PhyrexianPayment.PhyrexianPayment]
phyrexianAnnouncements responses =
  let announcement r = case r of
        Response.AnnouncedPhyrexianPayment way -> Just way
        _ -> Nothing
   in Maybe.mapMaybe announcement responses

-- The board issue #361 named: alice controls one untapped Forest and a Goblin Piker for
-- Mutagenic Growth to target, and holds Mutagenic Growth ({G/P}) and Llanowar
-- Elves ({G}). ONE green source and two spells that want it, so which way CR
-- 107.4f's symbol is paid decides whether the Elves can be cast at all -- the
-- most direct observation there is that the choice is not the engine's.
phyrexianBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
phyrexianBoard forest piker growth elves =
  let (_, withPiker) = S.addPermanent piker S.alice (S.landsInPlay forest 1)
      (withGrowth, growthId) = S.handOne growth withPiker
      (elvesId, gs) = S.addHandCard elves S.alice withGrowth
   in (growthId, elvesId, gs)

-- Cast `oid` under `answer` and resolve what it put on the stack, returning the
-- transcript of everything the engine asked alongside the final state.
castAndResolve ::
  (forall r. Prompt.Prompt r -> r) ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  ([Response.Response], GameState.GameState)
castAndResolve answer gs oid =
  let ((_, cast), asked) = Replay.record answer gs (S.cast S.alice oid)
   in (asked, snd (S.runPureWith answer cast Stack.resolveTop))

-- alice at `n` life and nothing else on the board.
aliceAt :: Integer -> GameState.GameState
aliceAt n = atLife n (Setup.emptyGame S.bothPlayers)

-- CR 107.4f: "A Phyrexian mana symbol represents a cost that can be paid either
-- with one mana of its color or by paying 2 life."
--
-- Mutagenic Growth ({G/P}) throughout -- a plain pump, so every assertion below
-- is about the symbol and nothing else.
--
-- TWO PATHS, and which one a case takes decides who chooses. A case calling
-- Cost.payMana directly pays an UNANNOUNCED cost, where the least-life rule still
-- decides -- a fence under that rule rather than a gameplay road, no engine path
-- leaving a Phyrexian symbol unannounced; a case going through Cast.castSpell announces first, under CR
-- 118.13a, and the player decides. The CR 118.13a cases at the end of this group
-- are the second path.
phyrexianSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phyrexianSpec s registry = Spec.describe s "Phyrexian" $ do
  -- The mana route. The life assertion is what makes this discriminating:
  -- both routes are open here, and paying life as WELL as the mana, or
  -- INSTEAD of it, would each read as "paid" without it.
  --
  -- It also pins what Cost.payMana does with an UNANNOUNCED cost, which is
  -- what this and the four cases after it exercise: they call Cost.payMana
  -- directly, so no announcement has happened and the least-life
  -- rule still decides, which here means none. A cast goes through
  -- Cast.castSpell instead and asks -- see the CR 118.13a cases at the end of
  -- this group.
  Spec.it s "CR 107.4f one {G/P} is paid with one green mana and no life" $ do
    forest <- S.printingOf s registry "Forest"
    let gs = S.landsInPlay forest 1
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice phyrexianCost gs) "canPay says yes"
    let (paid, after) = S.runPureWith S.identityAnswer gs (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice phyrexianCost)
    Spec.assertBool s paid "and it really is paid"
    Spec.assertEqWith s "the Forest is tapped" (S.tappedCount S.alice after) 1
    Spec.assertEqWith s "nothing left floating" (poolSize S.alice after) 0
    Spec.assertEqWith s "life untouched" (S.lifeOf S.alice after) (Just 20)

  -- The life route, with a land on the battlefield that cannot help. The tap
  -- count is the discriminator: a payment path that tapped the Mountain
  -- first and then paid life would still leave alice at 18.
  Spec.it s "CR 107.4f one {G/P} is paid by 2 life when no green mana can be made" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice phyrexianCost gs) "canPay says yes"
    let (paid, after) = S.runPureWith S.identityAnswer gs (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice phyrexianCost)
    Spec.assertBool s paid "and it really is paid"
    Spec.assertEqWith s "exactly 2 life" (S.lifeOf S.alice after) (Just 18)
    Spec.assertEqWith s "the Mountain is untouched" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "nothing left floating" (poolSize S.alice after) 0

  -- CR 119.4: "the player may do so only if their life total is greater than
  -- or equal to the amount of the payment." Two is the boundary, and the
  -- payment that takes alice to exactly 0 is legal -- CR 704.5a's loss is a
  -- state-based action afterwards, not a bar on the payment.
  Spec.it s "CR 119.4 a {G/P} is payable at 2 life and unpayable at 1" $ do
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice phyrexianCost (aliceAt 2)) "2 life is enough"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice phyrexianCost (aliceAt 1))) "1 life is not"
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice phyrexianCost (aliceAt 0))) "0 life is not"
    let (paid, after) = S.runPureWith S.identityAnswer (aliceAt 2) (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice phyrexianCost)
    Spec.assertBool s paid "paying at 2 really works"
    Spec.assertEqWith s "and takes her to 0" (S.lifeOf S.alice after) (Just 0)
    let (failed, unchanged) = S.runPureWith S.identityAnswer (aliceAt 1) (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice phyrexianCost)
    Spec.assertBool s (not failed) "at 1 the payment fails"
    Spec.assertEqWith s "and CR 601.2h leaves the life total alone" (S.lifeOf S.alice unchanged) (Just 1)

  -- The gameplay-level proof (design.md section 4), mana route: the whole
  -- card, cast off one Forest and resolved. Goblin Piker is 2/1, so +2/+2 is
  -- 4/3.
  Spec.it s "CR 107.4f whole card: Mutagenic Growth casts off one Forest for +2/+2" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    mutagenicGrowth <- S.printingOf s registry "Mutagenic Growth"
    let (pikerId, withPiker) = S.addPermanent piker S.alice (S.landsInPlay forest 1)
        (g, spellId) = S.handOne mutagenicGrowth withPiker
        cast = snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "power" (Projection.powerOf pikerId resolved) (Just 4)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf pikerId resolved) (Just 3)
    Spec.assertEqWith s "the Forest paid for it" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "so no life was" (S.lifeOf S.alice resolved) (Just 20)

  -- The same card with NO lands anywhere. Castability has to see the life
  -- route or this never reaches the stack at all.
  Spec.it s "CR 107.4f whole card: Mutagenic Growth casts with no mana at all, for 2 life" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mutagenicGrowth <- S.printingOf s registry "Mutagenic Growth"
    let (pikerId, withPiker) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (g, spellId) = S.handOne mutagenicGrowth withPiker
    Spec.assertBool s (S.castable S.alice spellId g) "castable with an empty battlefield but for the Piker"
    let cast = snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "stack empty" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "power" (Projection.powerOf pikerId resolved) (Just 4)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf pikerId resolved) (Just 3)
    Spec.assertEqWith s "exactly 2 life paid" (S.lifeOf S.alice resolved) (Just 18)

  -- THE discriminating negative: neither route open. One life short, and no
  -- green mana on the board.
  Spec.it s "CR 119.4 Mutagenic Growth is uncastable at 1 life with no green mana" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mutagenicGrowth <- S.printingOf s registry "Mutagenic Growth"
    let inHandAt n =
          let (_, withPiker) = S.addPermanent piker S.alice (aliceAt n)
           in S.handOne mutagenicGrowth withPiker
        castableAt n = let (g, spellId) = inHandAt n in S.castable S.alice spellId g
        castsAt n =
          let (g, spellId) = inHandAt n
           in length (GameState.stack (snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))))
    Spec.assertBool s (not (castableAt 1)) "at 1 life it is not castable"
    Spec.assertEqWith s "and it does not cast" (castsAt 1) 0
    Spec.assertBool s (castableAt 2) "at 2 life it is -- the Piker it targets has not moved"
    Spec.assertEqWith s "and it does cast" (castsAt 2) 1

  -- CR 107.4f's FIRST clause, the one a payment-only reading loses:
  -- "Phyrexian mana symbols are colored mana symbols: ... {G/P} is green."
  -- CR 202.2d says the same of the object: "An object with one or more
  -- hybrid mana symbols and/or Phyrexian mana symbols in its mana cost is all
  -- of the colors of those mana symbols, in addition to any other colors the
  -- object might be."
  Spec.it s "CR 107.4f/202.2d a Phyrexian mana symbol is a COLOURED mana symbol" $ do
    mutagenicGrowth <- S.printingOf s registry "Mutagenic Growth"
    let (oid, gs) = S.addPermanent mutagenicGrowth S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "green, not colourless" (Projection.colorsOf oid gs) (Set.singleton Color.Green)

  -- And the colour survives the route that produces no green mana at all --
  -- the reading that would call the card colourless is exactly the one a
  -- life-paid cast tempts.
  Spec.it s "CR 202.2d Mutagenic Growth is green on the stack even when 2 life paid for it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mutagenicGrowth <- S.printingOf s registry "Mutagenic Growth"
    let (_, withPiker) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (g, spellId) = S.handOne mutagenicGrowth withPiker
        cast = snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice spellId))
    Spec.assertEqWith s "2 life paid, no green mana ever made" (S.lifeOf S.alice cast) (Just 18)
    case GameState.stack cast of
      [sid] -> Spec.assertEqWith s "and the spell is still green" (Projection.colorsOf sid cast) (Set.singleton Color.Green)
      _ -> Spec.assertFailure s "expected exactly one spell on the stack"

  -- Mana.resolutions' SORT, pinned -- the least-life rule has to hold across
  -- symbols and not merely within one, and the per-symbol product alone does
  -- not give that. CR 601.2b's nonhybrid equivalents of {2/R}{G/P} leave the
  -- product in the order 0, 2, 0, 2 life, so unsorted the first PAYABLE entry
  -- on this board is the 2-life one.
  --
  -- A lone Birds of Paradise and two Islands make all four orderings matter:
  -- {R}{G} is impossible (one Birds makes one mana), {R} plus 2 life works,
  -- {G} plus {2} works and costs nothing, and {2} plus 2 life works. The
  -- least is zero, and pawl must find it.
  Spec.it s "CR 107.4f the least-life route is found across symbols, not only within one" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let cost = ManaCost.MkManaCost [twoOrRed, ManaSymbol.Phyrexian Color.Green]
    Spec.assertEqWith
      s
      "the {G} plus {2} route, costing no life"
      (Mana.lifeNeeded PaymentSubject.ForNeither Cost.manaActivations ManaSpending.AsProduced S.alice cost (mixedLands island birds 2 1))
      (Just 0)
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice cost (mixedLands island birds 2 1)) "and it is payable"
    -- The discriminator: the same cost and the same three permanents, but a
    -- Mountain in the Birds' place makes no green, so every surviving route
    -- costs 2 life and the answer really does depend on the board.
    Spec.assertEqWith
      s
      "with a Mountain instead, 2 life is the cheapest there is"
      (Mana.lifeNeeded PaymentSubject.ForNeither Cost.manaActivations ManaSpending.AsProduced S.alice cost (mixedLands island mountain 2 1))
      (Just 2)
    Spec.assertEqWith
      s
      "and a lone {G/P} off nothing at all is 2 as well"
      (Mana.lifeNeeded PaymentSubject.ForNeither Cost.manaActivations ManaSpending.AsProduced S.alice phyrexianCost (Setup.emptyGame S.bothPlayers))
      (Just 2)
    Spec.assertEqWith
      s
      "while a lone {G/P} with a Forest is 0"
      (Mana.lifeNeeded PaymentSubject.ForNeither Cost.manaActivations ManaSpending.AsProduced S.alice phyrexianCost (S.landsInPlay forest 1))
      (Just 0)

  -- The budget is recomputed as sources are tapped, not fixed when the
  -- payment starts, and a Birds of Paradise is what makes the difference
  -- observable: it COULD make green, so pawl starts with a budget of zero
  -- life and taps it -- and when the player names blue instead, the mana way
  -- is gone and CR 107.4f's 2 life is all that is left. pawl pays it rather
  -- than failing the payment, which is the same MORE PERMISSIVE posture
  -- Cost.payMana's haddock takes towards a mis-tapped colour. Reached only
  -- because this calls Cost.payMana directly, with nothing announced.
  Spec.it s "CR 107.4f a Birds tapped for blue still pays a {G/P}, out of life" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    let (_, gs) = S.addPermanent birds S.alice (Setup.emptyGame S.bothPlayers)
        (paidBlue, afterBlue) = S.runPureWith (prefersColor Color.Blue) gs (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice phyrexianCost)
    Spec.assertBool s paidBlue "the cost is still paid"
    Spec.assertEqWith s "by 2 life" (S.lifeOf S.alice afterBlue) (Just 18)
    Spec.assertEqWith s "the Birds was tapped on the way" (S.tappedCount S.alice afterBlue) 1
    Spec.assertEqWith s "and its blue mana is still floating" (poolSize S.alice afterBlue) 1
    -- The control: the same board and the same card, one different answer.
    let (paidGreen, afterGreen) = S.runPureWith (prefersColor Color.Green) gs (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice phyrexianCost)
    Spec.assertBool s paidGreen "green pays it too"
    Spec.assertEqWith s "and costs no life at all" (S.lifeOf S.alice afterGreen) (Just 20)
    Spec.assertEqWith s "with nothing left floating" (poolSize S.alice afterGreen) 0

  -- CR 118.13a: "If the mana cost of a spell ... contains a mana symbol that
  -- can be paid in multiple ways, the choice of how to pay for that symbol is
  -- made as its controller proposes that spell or ability (see rule 601.2b)."
  --
  -- THE proving scenario (#361), and the reason the choice is not the
  -- engine's to make conservatively: a player holding one Forest can cast
  -- Mutagenic Growth AND Llanowar Elves only by announcing 2 life for the
  -- {G/P}. The Elves' castability is the discriminator -- a life total alone
  -- could be produced by paying life on TOP of the mana.
  Spec.it s "CR 118.13a announcing 2 life keeps the Forest, so Llanowar Elves is still castable" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    growth <- S.printingOf s registry "Mutagenic Growth"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (growthId, elvesId, gs) = phyrexianBoard forest piker growth elves
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs growthId
    Spec.assertBool s (wasAskedHowToPayPhyrexian asked) "the engine asked rather than deciding"
    Spec.assertEqWith s "the Growth resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "exactly 2 life" (S.lifeOf S.alice resolved) (Just 18)
    Spec.assertEqWith s "and the Forest is untapped" (S.tappedCount S.alice resolved) 0
    Spec.assertBool s (S.castable S.alice elvesId resolved) "so the Elves can still be cast"

  -- The control, one answer different on the same board: the mana route
  -- spends the Forest and the Elves are stranded. Both legs are needed --
  -- either alone would pass against an engine that ignored the answer.
  Spec.it s "CR 118.13a announcing coloured mana taps the Forest, and the Elves are stranded" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    growth <- S.printingOf s registry "Mutagenic Growth"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (growthId, elvesId, gs) = phyrexianBoard forest piker growth elves
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs growthId
    Spec.assertBool s (wasAskedHowToPayPhyrexian asked) "the engine asked here too"
    Spec.assertEqWith s "life untouched" (S.lifeOf S.alice resolved) (Just 20)
    Spec.assertEqWith s "the Forest paid for it" (S.tappedCount S.alice resolved) 1
    Spec.assertBool s (not (S.castable S.alice elvesId resolved)) "and the Elves cannot be cast"

  -- The elision, both directions. Where only ONE route is payable there is
  -- nothing to ask, and the interpreter asking for the other route does not
  -- get it -- CR 601.2b's "previously made choices ... may restrict the
  -- player's options" arriving as a board that offers one option.
  Spec.it s "CR 118.13a no green source: the life route is taken and nothing is asked" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    growth <- S.printingOf s registry "Mutagenic Growth"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (_, withPiker) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (withGrowth, growthId) = S.handOne growth withPiker
        (_, gs) = S.addHandCard elves S.alice withGrowth
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs growthId
    Spec.assertBool s (not (wasAskedHowToPayPhyrexian asked)) "no choice existed, so none was asked"
    Spec.assertEqWith s "the Growth resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "and 2 life paid for it" (S.lifeOf S.alice resolved) (Just 18)

  -- CR 119.4's floor closing the life route instead: one Forest, one life.
  -- The interpreter asks for life and must not be given it.
  Spec.it s "CR 119.4 at 1 life the life route is not offered, and nothing is asked" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    growth <- S.printingOf s registry "Mutagenic Growth"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (growthId, _, board) = phyrexianBoard forest piker growth elves
        gs = atLife 1 board
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs growthId
    Spec.assertBool s (not (wasAskedHowToPayPhyrexian asked)) "no choice existed, so none was asked"
    Spec.assertEqWith s "the Growth resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "the Forest paid for it" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and the life total is untouched" (S.lifeOf S.alice resolved) (Just 1)

-- `printing` on the battlefield, settled and untapped, on a board of `n`
-- `land`s -- the shape every board below wants and none of Support's helpers
-- spells directly.
withPermanent :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
withPermanent land printing n = snd (S.addPermanent printing S.alice (S.landsInPlay land n))

-- CR 601.2f: "The total cost is the mana cost or alternative cost (as determined
-- in rule 601.2b), plus all additional costs and cost increases, and minus all
-- cost reductions."
--
-- So CR 118.13a's announcement (CR 601.2b) comes FIRST and the total comes after
-- it -- but the routes the announcement may take are decided by what the TOTAL
-- will cost, not by the printed cost. Getting that backwards is the engine
-- choosing again, one step further on than #361 reached.
--
-- Two directions, and only one of them is merely untidy:
--
--   * a REDUCTION makes the printed cost dearer than the total, so a route the
--     total could pay reads as unpayable. Where that leaves one route standing,
--     the prompt is elided and the engine pays for the player. Sapphire Medallion
--     and Spined Thopter, below.
--   * an INCREASE makes the printed cost cheaper, so a route the total cannot pay
--     is offered. The player answers it and the payment fails, which CR 601.2's
--     own "the game returns to the moment before the casting of that spell was
--     proposed" would also do -- but the answer was never a real option. Thalia
--     and Mutagenic Growth, below.
totalCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
totalCostSpec s registry = Spec.describe s "TotalCost" $ do
  -- THE reduction case. Sapphire Medallion is "Blue spells you cast cost {1}
  -- less to cast", Spined Thopter is {2}{U/P}, and two Islands are exactly
  -- the board where the reduction decides the question: the total {1}{U/P}
  -- can be paid with {1}{U} off both Islands, while the printed {2}{U/P}
  -- cannot be paid with mana at all. So the coloured-mana route IS available
  -- and the player must be asked for it.
  Spec.it s "CR 601.2f a reduction opens the coloured-mana route, so the announcement is asked" $ do
    island <- S.printingOf s registry "Island"
    medallion <- S.printingOf s registry "Sapphire Medallion"
    thopter <- S.printingOf s registry "Spined Thopter"
    let (gs, thopterId) = S.handOne thopter (withPermanent island medallion 2)
    Spec.assertBool s (S.castable S.alice thopterId gs) "castable"
    let (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs thopterId
    -- The outcome first, because it is the thing that was wrong: the engine
    -- used to take CR 107.4f's life route here without asking.
    Spec.assertEqWith s "no life was paid" (S.lifeOf S.alice resolved) (Just 20)
    Spec.assertEqWith s "both Islands paid the reduced {1}{U}" (S.tappedCount S.alice resolved) 2
    Spec.assertBool s (wasAskedHowToPayPhyrexian asked) "and the engine asked rather than deciding"
    Spec.assertEqWith s "the Thopter resolved" (length (GameState.stack resolved)) 0

  -- The control, one answer different on the same board: CR 107.4f's life
  -- route leaves an Island up. Both legs are needed -- either alone would
  -- pass against an engine that ignored the answer.
  Spec.it s "CR 601.2f the same board's life route pays 2 and spares an Island" $ do
    island <- S.printingOf s registry "Island"
    medallion <- S.printingOf s registry "Sapphire Medallion"
    thopter <- S.printingOf s registry "Spined Thopter"
    let (gs, thopterId) = S.handOne thopter (withPermanent island medallion 2)
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs thopterId
    Spec.assertBool s (wasAskedHowToPayPhyrexian asked) "asked here too"
    Spec.assertEqWith s "the Thopter resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "one Island paid the reduced {1}" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and 2 life paid the symbol" (S.lifeOf S.alice resolved) (Just 18)

  -- THE increase case. Thalia is "Noncreature spells cost {1} more to cast",
  -- Mutagenic Growth is an instant, and one Forest is exactly the board where
  -- the increase decides the question: the total {1}{G/P} cannot be paid with
  -- {1}{G} off one Forest, so the coloured-mana route is NOT available and
  -- must not be offered. The interpreter asks for it and does not get it.
  Spec.it s "CR 601.2f an increase closes the coloured-mana route, so nothing is asked" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    growth <- S.printingOf s registry "Mutagenic Growth"
    -- The Piker goes down BEFORE Thalia, because S.identityAnswer's
    -- ChooseTargets takes the lowest object id and both are 2/1 creatures --
    -- so with Thalia first the Growth would pump HER and the assertion below
    -- would be reading the wrong permanent.
    let (_, withPiker) = S.addPermanent piker S.alice (S.landsInPlay forest 1)
        (_, withThalia) = S.addPermanent thalia S.alice withPiker
        (gs, growthId) = S.handOne growth withThalia
    Spec.assertBool s (S.castable S.alice growthId gs) "castable, by CR 107.4f's life route"
    let (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs growthId
    -- The outcome first again: answering the route the engine used to offer
    -- made the whole cast a no-op, so the Piker went unpumped and no life was
    -- paid at all.
    Spec.assertEqWith s "2 life paid the symbol" (S.lifeOf S.alice resolved) (Just 18)
    Spec.assertEqWith s "the Piker really was pumped" (Projection.powerOf (pikerOn resolved) resolved) (Just 4)
    Spec.assertEqWith s "the Forest paid Thalia's {1}" (S.tappedCount S.alice resolved) 1
    Spec.assertBool s (not (wasAskedHowToPayPhyrexian asked)) "and no route existed, so none was asked"
    Spec.assertEqWith s "the Growth resolved rather than evaporating" (length (GameState.stack resolved)) 0

  -- The increase again, with TWO symbols, which is what makes it a cast lost
  -- rather than a cast made awkwardly: Dismember's total under Thalia is
  -- {2}{B/P}{B/P}, and two Swamps pay that only by CR 107.4f's life route
  -- twice. Measured against the printed {1}{B/P}{B/P} the first symbol looks
  -- like a real choice, and taking its mana route strands the payment.
  Spec.it s "CR 601.2f Dismember under Thalia forces both symbols to life, and the cast survives" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    dismember <- S.printingOf s registry "Dismember"
    -- Piker before Thalia, for the reason the case above gives: both are 2/1
    -- creatures and identityAnswer targets the lowest object id.
    let (_, withPiker) = S.addPermanent piker S.alice (S.landsInPlay swamp 2)
        (_, withThalia) = S.addPermanent thalia S.alice withPiker
        (gs, dismemberId) = S.handOne dismember withThalia
    Spec.assertBool s (S.castable S.alice dismemberId gs) "castable, by two life routes"
    let (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
    Spec.assertEqWith s "4 life paid both symbols" (S.lifeOf S.alice resolved) (Just 16)
    Spec.assertEqWith s "both Swamps paid Thalia's {2}" (S.tappedCount S.alice resolved) 2
    Spec.assertEqWith s "and neither symbol was a choice" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "Dismember resolved rather than evaporating" (length (GameState.stack resolved)) 0

-- Dismember ({1}{B/P}{B/P}) -- the first card in the pool with more than one
-- Phyrexian mana symbol, and so the first to exercise CR 601.2b's "for each of
-- those symbols" at all. Everything Mutagenic Growth proves about ONE symbol it
-- proves once; what only two symbols can show is the LOOP: one prompt per symbol
-- in printed order, each asked knowing the answers before it, and an earlier
-- answer narrowing a later one's offer -- CR 601.2b's last sentence, "previously
-- made choices ... may restrict the player's options when making these choices."
dismemberSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dismemberSpec s registry = Spec.describe s "Dismember" $ do
  -- Two Swamps: the first symbol is a real choice, and answering MANA leaves
  -- {1}{B} to pay off two Swamps -- which the second symbol's mana route
  -- would push to three. So the second symbol is forced to life and is not
  -- asked. One prompt, not two, and that count is the assertion.
  Spec.it s "CR 601.2b announcing mana for the first {B/P} forces the second to life" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    dismember <- S.printingOf s registry "Dismember"
    let (gs, dismemberId) = dismemberBoard swamp piker dismember 2
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
    Spec.assertEqWith s "one symbol was a choice, the other was forced" (phyrexianAnnouncements asked) [PhyrexianPayment.PaysMana]
    Spec.assertEqWith s "both Swamps paid {1}{B}" (S.tappedCount S.alice resolved) 2
    Spec.assertEqWith s "and 2 life paid the second symbol" (S.lifeOf S.alice resolved) (Just 18)

  -- The same board, the other answer: paying the first symbol with life keeps
  -- both Swamps available, so the SECOND symbol is a real choice too and is
  -- asked. Two prompts, and the answers are 2 life each.
  Spec.it s "CR 601.2b announcing life for the first {B/P} leaves the second a real choice" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    dismember <- S.printingOf s registry "Dismember"
    let (gs, dismemberId) = dismemberBoard swamp piker dismember 2
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs dismemberId
    Spec.assertEqWith
      s
      "both symbols were asked, in printed order"
      (phyrexianAnnouncements asked)
      [PhyrexianPayment.PaysLife, PhyrexianPayment.PaysLife]
    Spec.assertEqWith s "one Swamp paid the {1}" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and 4 life paid both symbols" (S.lifeOf S.alice resolved) (Just 16)

  -- One Swamp: neither symbol can be paid with mana, since the Swamp is
  -- needed for the {1}. Nothing is asked at all, and CR 107.4f's example
  -- arithmetic for two symbols -- 4 life -- is what comes out.
  Spec.it s "CR 107.4f off one Swamp both symbols are forced to life, for 4" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    dismember <- S.printingOf s registry "Dismember"
    let (gs, dismemberId) = dismemberBoard swamp piker dismember 1
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
    Spec.assertEqWith s "no choice existed either time" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "the Swamp paid the {1}" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "and 4 life paid both symbols" (S.lifeOf S.alice resolved) (Just 16)

  -- The gameplay-level proof (design.md section 4): the whole card, cast and
  -- resolved. A Goblin Piker is 2/1, so -5/-5 is -3/-4 and CR 704.5f's
  -- state-based action puts it into its owner's graveyard.
  Spec.it s "CR 107.4f whole card: Dismember kills a Goblin Piker for 4 life" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    dismember <- S.printingOf s registry "Dismember"
    let (gs, dismemberId) = dismemberBoard swamp piker dismember 1
        (_, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dismemberId
        pikerId = pikerOn resolved
    Spec.assertEqWith s "-5/-5 applied" (Projection.powerOf pikerId resolved) (Just (-3))
    Spec.assertEqWith s "toughness too" (Projection.toughnessOf pikerId resolved) (Just (-4))
    let settled = S.settleSba resolved
    Spec.assertBool s (not (Set.member pikerId (GameState.battlefield settled))) "CR 704.5f buried it"
    Spec.assertEqWith s "and 4 life paid for it" (S.lifeOf S.alice settled) (Just 16)

-- Alice with `n` Swamps, a Goblin Piker for Dismember to target, and Dismember in
-- hand.
dismemberBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (GameState.GameState, ObjectId.ObjectId)
dismemberBoard swamp piker dismember n =
  let (_, withPiker) = S.addPermanent piker S.alice (S.landsInPlay swamp n)
   in S.handOne dismember withPiker

-- The one Goblin Piker on the battlefield. The Piker is added before the spell is
-- cast and never moves, so this is a lookup and not a choice; a board with no
-- Piker would fail the assertion that reads it.
pikerOn :: GameState.GameState -> ObjectId.ObjectId
pikerOn gs =
  let isPiker oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Goblin Piker")
   in case filter isPiker (Set.toAscList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> ObjectId.MkObjectId 0

-- Synthetic Phyrexian Toll ({G/P} Instant, "As an additional cost to cast this
-- spell, pay 2 life. Target creature gets +2/+2 until end of turn.") -- Mutagenic
-- Growth with CR 118.8's additional cost bolted on, and the card CR 118.3 was
-- waiting for.
--
-- CR 107.4f's Phyrexian symbol is the only MANA symbol that spends life, so it is
-- the only way one cost can demand life TWICE: once through the mana part and
-- once through a component. CR 118.3 measures the demand whole -- "a player can't
-- pay a cost without having the necessary resources to pay it fully" -- so the
-- question this card asks, and no other card in the pool can, is whether pawl
-- adds the two before comparing them to a life total.
--
-- SYNTHETIC, since no printing pairs them on one card, and legitimate because
-- nothing in the CR keeps them apart: CR 118.8's additional cost and CR 107.4f's
-- symbol are independent, and the Defiler cycle already puts a 2-life additional
-- cost onto spells that may carry the symbol -- Defiler of Vigor onto Birthing
-- Pod ({3}{G/P}, a green permanent spell by CR 202.2d). What keeps that pairing
-- out of this spec is that the Defiler's cost is OPTIONAL (CR 118.8b) and its
-- reduction is conditional on having paid it, so the board would have to answer
-- a prompt before CR 118.3 was asked anything -- two questions where this spec
-- wants one.
--
-- The board is phyrexianSpec's throughout -- a Goblin Piker to target, and the
-- Toll in hand -- so the only thing that varies between these cases is alice's
-- life total and whether a Forest is out.
phyrexianTollSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phyrexianTollSpec s registry = Spec.describe s "SyntheticPhyrexianToll" $ do
  -- THE case. With no green source the symbol costs 2 life and the additional
  -- cost costs 2 more, so CR 118.3's "fully" is 4 -- and 3 is not 4, however
  -- comfortably it covers each half on its own.
  --
  -- 4 life is the control, and it is what makes this about the SUM rather than
  -- about the board: one more life and the same card off the same empty
  -- battlefield casts. It is also CR 119.4's boundary, where the payment takes
  -- alice to exactly 0 -- legal, with CR 704.5a's loss a state-based action
  -- afterwards rather than a bar on the payment.
  Spec.it s "CR 118.3 a {G/P} beside an additional 2 life costs 4, so 3 life is not enough" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    toll <- S.printingOf s registry "Synthetic Phyrexian Toll"
    let inHandAt n =
          let (_, withPiker) = S.addPermanent piker S.alice (aliceAt n)
           in S.handOne toll withPiker
        castableAt n = let (g, tollId) = inHandAt n in S.castable S.alice tollId g
        castsAt n =
          let (g, tollId) = inHandAt n
           in length (GameState.stack (snd (Engine.runGamePure S.identityAnswer g (S.cast S.alice tollId))))
    Spec.assertBool s (not (castableAt 3)) "at 3 life it is not castable, though either half alone would be payable"
    Spec.assertEqWith s "and it does not cast" (castsAt 3) 0
    Spec.assertBool s (castableAt 4) "at 4 life it is"
    Spec.assertEqWith s "and it does cast" (castsAt 4) 1

  -- The same sum reaching CR 601.2b's announcement. A Forest opens the mana
  -- route, so at 3 life the cast is legal -- but only that way, because the life
  -- route would still want 2 on top of the component's 2. The interpreter asks
  -- for the life route and must not be given it, exactly as the CR 119.4 floor
  -- case in phyrexianSpec does with a bare {G/P} at 1 life.
  Spec.it s "CR 601.2b at 3 life the additional cost closes the life route, and nothing is asked" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    toll <- S.printingOf s registry "Synthetic Phyrexian Toll"
    let (_, withPiker) = S.addPermanent piker S.alice (S.landsInPlay forest 1)
        (gs, tollId) = S.handOne toll (atLife 3 withPiker)
    Spec.assertBool s (S.castable S.alice tollId gs) "castable, by the mana route alone"
    let (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs tollId
    Spec.assertEqWith s "no choice existed, so none was asked" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "the Forest paid the symbol" (S.tappedCount S.alice resolved) 1
    Spec.assertEqWith s "so only the additional cost's 2 life was paid" (S.lifeOf S.alice resolved) (Just 1)
    Spec.assertEqWith s "the Toll resolved" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "and the Piker really was pumped" (Projection.powerOf (pikerOn resolved) resolved) (Just 4)

  -- The control for the case above, and the guard against a floor that simply
  -- refuses: two more life is enough for BOTH routes, so the choice comes back
  -- and the engine asks. Answering life leaves the Forest untapped, which is the
  -- discriminator -- a life total alone could be produced by paying life on TOP
  -- of the mana.
  Spec.it s "CR 118.13a at 5 life both routes are payable again, and the player is asked" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    toll <- S.printingOf s registry "Synthetic Phyrexian Toll"
    let (_, withPiker) = S.addPermanent piker S.alice (S.landsInPlay forest 1)
        (gs, tollId) = S.handOne toll (atLife 5 withPiker)
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysLife) gs tollId
    Spec.assertEqWith s "the engine asked rather than deciding" (phyrexianAnnouncements asked) [PhyrexianPayment.PaysLife]
    Spec.assertEqWith s "the Forest is untapped" (S.tappedCount S.alice resolved) 0
    Spec.assertEqWith s "and 4 life paid the whole cost" (S.lifeOf S.alice resolved) (Just 1)
    Spec.assertEqWith s "the Toll resolved" (length (GameState.stack resolved)) 0

-- Moltensteel Dragon ({4}{R/P}{R/P}, with "{R/P}: This creature gets +1/+0 until
-- end of turn") -- the first card in the pool with a Phyrexian mana symbol
-- OUTSIDE a spell's mana cost.
--
-- CR 602.2b: "The remainder of the process for activating an ability is identical
-- to the process for casting a spell listed in rules 601.2b-i", and CR 118.13a
-- names "the activation cost of an activated ability" in its own words -- so the
-- announcement happens at CR 601.2b's position for an activation too. Until this
-- card there was nothing in the pool for Pawl.Engine.Activate's Cost.announce call to do.
moltensteelSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
moltensteelSpec s registry = Spec.describe s "Moltensteel" $ do
  -- The activation cost's symbol IS a choice off a Mountain, and answering
  -- mana taps it. CR 118.13b/c are not what governs this -- the cost is an
  -- activation cost, so CR 118.13a is, and the choice belongs at proposal
  -- rather than at payment. Rule 118.13b announces at its own site
  -- (Pawl.Engine.Resolve.payGatePaidBy, the Shu Yun group above); rule
  -- 118.13c's special action announces at its own
  -- (Pawl.FaceDownSpec's Dog Walker case).
  Spec.it s "CR 118.13a/602.2b an activation cost's {R/P} is asked, and mana taps the Mountain" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Moltensteel Dragon"
    let (dragonId, gs) = dragonBoard mountain dragon 1
        (asked, activated) = activateAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId (theAbility dragon)
    Spec.assertEqWith s "one symbol, one prompt" (phyrexianAnnouncements asked) [PhyrexianPayment.PaysMana]
    Spec.assertEqWith s "the Mountain paid it" (S.tappedCount S.alice activated) 1
    Spec.assertEqWith s "no life paid" (S.lifeOf S.alice activated) (Just 20)
    Spec.assertEqWith s "+1/+0" (Projection.powerOf dragonId activated) (Just 5)
    Spec.assertEqWith s "toughness unchanged" (Projection.toughnessOf dragonId activated) (Just 4)

  -- The control, one answer different on the same board: 2 life instead, and
  -- the Mountain is still up for something else.
  Spec.it s "CR 118.13a the same activation's life route spares the Mountain" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Moltensteel Dragon"
    let (dragonId, gs) = dragonBoard mountain dragon 1
        (asked, activated) = activateAndResolve (announces PhyrexianPayment.PaysLife) gs dragonId (theAbility dragon)
    Spec.assertEqWith s "asked here too" (phyrexianAnnouncements asked) [PhyrexianPayment.PaysLife]
    Spec.assertEqWith s "the Mountain is untouched" (S.tappedCount S.alice activated) 0
    Spec.assertEqWith s "2 life paid it" (S.lifeOf S.alice activated) (Just 18)
    Spec.assertEqWith s "+1/+0 all the same" (Projection.powerOf dragonId activated) (Just 5)

  -- No red source: CR 107.4f's mana route cannot be completed, so there is
  -- nothing to ask and the life route is taken. The activation still happens,
  -- which is the half a payment-time reading would get right by accident.
  Spec.it s "CR 118.13a with no red source the activation's life route is forced" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Moltensteel Dragon"
    let (dragonId, gs) = dragonBoard mountain dragon 0
        (asked, activated) = activateAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId (theAbility dragon)
    Spec.assertEqWith s "no choice existed, so none was asked" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "2 life paid it" (S.lifeOf S.alice activated) (Just 18)
    Spec.assertEqWith s "+1/+0" (Projection.powerOf dragonId activated) (Just 5)

  -- The gameplay-level proof, and the second board where two symbols in ONE
  -- cost are both real choices: six Mountains pay {4}{R}{R} outright, so both
  -- announcements are asked and neither costs life.
  Spec.it s "CR 107.4f whole card: Moltensteel Dragon casts off six Mountains for no life" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Moltensteel Dragon"
    let (gs, dragonId) = S.handOne dragon (S.landsInPlay mountain 6)
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId
    Spec.assertEqWith
      s
      "both symbols were asked"
      (phyrexianAnnouncements asked)
      [PhyrexianPayment.PaysMana, PhyrexianPayment.PaysMana]
    Spec.assertEqWith s "all six Mountains paid {4}{R}{R}" (S.tappedCount S.alice resolved) 6
    Spec.assertEqWith s "and no life did" (S.lifeOf S.alice resolved) (Just 20)
    Spec.assertEqWith s "a 4/4 arrived" (Projection.powerOf (dragonOn resolved) resolved) (Just 4)
    Spec.assertEqWith s "CR 202.2d: red, from the Phyrexian symbols" (Projection.colorsOf (dragonOn resolved) resolved) (Set.singleton Color.Red)

  -- Four Mountains cannot pay {4}{R}{R}, so both symbols are forced to life
  -- and CR 107.4f's arithmetic for two symbols is 4 -- the same card, one
  -- fewer land, and the whole announcement disappears.
  Spec.it s "CR 107.4f whole card: off four Mountains both symbols are forced, for 4 life" $ do
    mountain <- S.printingOf s registry "Mountain"
    dragon <- S.printingOf s registry "Moltensteel Dragon"
    let (gs, dragonId) = S.handOne dragon (S.landsInPlay mountain 4)
        (asked, resolved) = castAndResolve (announces PhyrexianPayment.PaysMana) gs dragonId
    Spec.assertEqWith s "no choice existed either time" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "all four Mountains paid the {4}" (S.tappedCount S.alice resolved) 4
    Spec.assertEqWith s "and 4 life paid both symbols" (S.lifeOf S.alice resolved) (Just 16)
    Spec.assertEqWith s "a 4/4 arrived all the same" (Projection.powerOf (dragonOn resolved) resolved) (Just 4)

-- Alice with a settled Moltensteel Dragon on the battlefield, `n` Mountains, and
-- priority -- which Activate.activateAbility needs and Setup.emptyGame leaves
-- unset.
dragonBoard :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
dragonBoard mountain dragon n =
  let (dragonId, gs) = S.addPermanent dragon S.alice (S.landsInPlay mountain n)
   in (dragonId, gs {GameState.priority = Just S.alice})

-- The one Moltensteel Dragon on the battlefield -- pikerOn's shape, for the card
-- a cast has just put there under a fresh CR 400.7 id.
dragonOn :: GameState.GameState -> ObjectId.ObjectId
dragonOn gs =
  let isDragon oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Moltensteel Dragon")
   in case filter isDragon (Set.toAscList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> ObjectId.MkObjectId 0

-- Activate `ability` on `oid` under `answer` and resolve what it put on the
-- stack, returning the transcript alongside the final state -- castAndResolve for
-- an activation.
activateAndResolve ::
  (forall r. Prompt.Prompt r -> r) ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) ->
  ([Response.Response], GameState.GameState)
activateAndResolve answer gs oid ability =
  let ((_, activated), asked) = Replay.record answer gs (Activate.activateAbility S.alice oid ability)
   in (asked, snd (S.runPureWith answer activated Stack.resolveTop))

-- CR 107.4f's second half: "There are also ten hybrid Phyrexian mana symbols. A
-- hybrid Phyrexian mana symbol represents a cost that can be paid with one mana
-- of either of its component colors or by paying 2 life. A hybrid Phyrexian mana
-- symbol is both of its component colors."
--
-- Tamiyo, Compleated Sage ({2}{G}{G/U/P}{U} Legendary Planeswalker) is the
-- producer, and the pool's only kind: every printed hybrid Phyrexian symbol is on
-- one of the four compleated planeswalkers (Scryfall `mana:{G/U/P}` and its nine
-- siblings, 2026-08-20 -- Ajani, Sleeper Agent, Lukka, Bound to Ruin and Nahiri,
-- the Unforgiving are the other three, and any of them would refute a claim that
-- this symbol reaches some other card type).
--
-- THREE WAYS, so two prompts: Prompt.AnnouncePhyrexianPayment settles mana
-- against life and Prompt.AnnounceHybridHalf then settles which colour, each
-- elided where the board leaves one answer. The pair is what
-- Pawl.Engine.Mana.announce's HybridPhyrexian arm builds; before it existed the
-- symbol would have ridden that function's `other` catch-all straight into
-- Pawl.Engine.Cost.payMana and been resolved by the least-life rule with no
-- prompt at all -- the behaviour #361 removed for the monocoloured symbol.
--
-- The loyalty assertions are CR 702.150a's compleated, which is why this group
-- and not a spec of its own carries them: the keyword reads what CR 601.2b
-- announced about this symbol, so the two rules are only observable together.
--
-- Not implemented: her second and third loyalty abilities, which
-- data/cards/tamiyo-compleated-sage.json omits -- a loyalty cost of -X (#1997),
-- and a token minted from a card's own text (#3049). Stricter than printed, and
-- no clause below rests on either.
tamiyoSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tamiyoSpec s registry = Spec.describe s "Tamiyo, Compleated Sage" $ do
  -- Off the card and off no board at all: CR 202.2d's colours and CR 202.3g's
  -- mana value, both of which a single-colour Phyrexian constructor could not
  -- have said.
  Spec.it s "CR 107.4f/202.2d a hybrid Phyrexian symbol is BOTH of its colours" $ do
    tamiyo <- S.printingOf s registry "Tamiyo, Compleated Sage"
    let (oid, gs) = S.addPermanent tamiyo S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "green AND blue, not one or the other"
      (Projection.colorsOf oid gs)
      (Set.fromList [Color.Green, Color.Blue])
    Spec.assertEqWith s "CR 202.3g: the symbol contributes 1, so {2}{G}{G/U/P}{U} is 5" (sum (fmap Quantity.manaValueOf (Game.manaCostFacesOf oid gs))) 5

  -- The gameplay-level pair, one board and two answers. Five lands pay the whole
  -- cost, so BOTH routes are live and the prompt is a real question; the answer
  -- moves three things at once -- the life total, how many lands were needed,
  -- and rule 702.150a's loyalty.
  Spec.it s "CR 107.4f/702.150a the mana route costs a fifth land and leaves loyalty at 5" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    tamiyo <- S.printingOf s registry "Tamiyo, Compleated Sage"
    let (gs, tamiyoId) = S.handOne tamiyo (mixedLands forest island 3 2)
        (asked, resolved) = castAndResolve (announcesBoth PhyrexianPayment.PaysMana greenMana) gs tamiyoId
    Spec.assertEqWith s "CR 702.150a: no life was paid, so she enters with her printed 5" (S.counterOf CounterKind.Loyalty (tamiyoOn resolved) resolved) 5
    Spec.assertEqWith s "the engine asked which way rather than deciding" (phyrexianAnnouncements asked) [PhyrexianPayment.PaysMana]
    Spec.assertEqWith s "and asked which half, both being payable here" (halfAnnouncements asked) [greenMana]
    Spec.assertEqWith s "all five lands paid {2}{G}{G}{U}" (S.tappedCount S.alice resolved) 5
    Spec.assertEqWith s "and no life did" (S.lifeOf S.alice resolved) (Just 20)
    Spec.assertEqWith s "she resolved" (length (GameState.stack resolved)) 0

  -- The same board, one answer different. This is the case rule 702.150a exists
  -- for, and the loyalty assertion is the one that can DIFFER: an engine that
  -- forgot the keyword, or forgot which way the symbol was paid, leaves 5 here.
  Spec.it s "CR 107.4f/702.150a the life route spares a land and takes two loyalty counters" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    tamiyo <- S.printingOf s registry "Tamiyo, Compleated Sage"
    let (gs, tamiyoId) = S.handOne tamiyo (mixedLands forest island 3 2)
        (asked, resolved) = castAndResolve (announcesBoth PhyrexianPayment.PaysLife greenMana) gs tamiyoId
    Spec.assertEqWith s "CR 702.150a: 5 minus two for the one symbol life paid for" (S.counterOf CounterKind.Loyalty (tamiyoOn resolved) resolved) 3
    Spec.assertEqWith s "the engine asked here too" (phyrexianAnnouncements asked) [PhyrexianPayment.PaysLife]
    Spec.assertEqWith s "and never asked which half, life naming no colour" (halfAnnouncements asked) []
    Spec.assertEqWith s "four lands paid {2}{G}{U}, the fifth is up" (S.tappedCount S.alice resolved) 4
    Spec.assertEqWith s "and CR 107.4f's 2 life paid the symbol" (S.lifeOf S.alice resolved) (Just 18)
    Spec.assertEqWith s "she resolved all the same" (length (GameState.stack resolved)) 0

  -- The blue half, on the board that admits it: two Islands pay the {U} and the
  -- symbol, and the three Forests cover {G} and the {2}. Same total, other
  -- colour -- which is what makes the half a choice rather than a label.
  Spec.it s "CR 107.4f the OTHER component colour pays it just as well" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    tamiyo <- S.printingOf s registry "Tamiyo, Compleated Sage"
    let (gs, tamiyoId) = S.handOne tamiyo (mixedLands forest island 3 2)
        (asked, resolved) = castAndResolve (announcesBoth PhyrexianPayment.PaysMana blueMana) gs tamiyoId
    Spec.assertEqWith s "the blue half was announced" (halfAnnouncements asked) [blueMana]
    Spec.assertEqWith s "CR 702.150a: still no life, still 5" (S.counterOf CounterKind.Loyalty (tamiyoOn resolved) resolved) 5
    Spec.assertEqWith s "five lands again" (S.tappedCount S.alice resolved) 5
    Spec.assertEqWith s "and no life" (S.lifeOf S.alice resolved) (Just 20)

  -- The elision, and it DISCRIMINATES: one Forest and four Islands leave the
  -- green half unpayable -- {G} takes the Forest and there is no second one --
  -- so the half prompt is not raised at all and the blue half is forced. An
  -- offer built from the printed symbol rather than from the board would ask.
  Spec.it s "CR 601.2b a half the board cannot pay is not offered" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    tamiyo <- S.printingOf s registry "Tamiyo, Compleated Sage"
    let (gs, tamiyoId) = S.handOne tamiyo (mixedLands forest island 1 4)
        (asked, resolved) = castAndResolve (announcesBoth PhyrexianPayment.PaysMana greenMana) gs tamiyoId
    Spec.assertEqWith s "no half was a choice, so none was asked" (halfAnnouncements asked) []
    Spec.assertEqWith s "the cast completed off the blue one anyway" (length (GameState.stack resolved)) 0
    Spec.assertEqWith s "five lands paid it" (S.tappedCount S.alice resolved) 5
    Spec.assertEqWith s "and no life" (S.lifeOf S.alice resolved) (Just 20)

  -- The other elision: four lands cannot reach {2}{G}{G/U/P}{U} through any
  -- mana route, so CR 107.4f's life route is forced and neither prompt is
  -- raised. Rule 702.150a still applies -- the reduction is not a consequence of
  -- being ASKED.
  Spec.it s "CR 107.4f/702.150a with no mana route the life route is forced, and still compleats her" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    tamiyo <- S.printingOf s registry "Tamiyo, Compleated Sage"
    let (gs, tamiyoId) = S.handOne tamiyo (mixedLands forest island 2 2)
        (asked, resolved) = castAndResolve (announcesBoth PhyrexianPayment.PaysMana greenMana) gs tamiyoId
    Spec.assertEqWith s "CR 702.150a: 3, life having paid the symbol" (S.counterOf CounterKind.Loyalty (tamiyoOn resolved) resolved) 3
    Spec.assertEqWith s "no route existed, so nothing was asked" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "nor about a half" (halfAnnouncements asked) []
    Spec.assertEqWith s "all four lands paid {2}{G}{U}" (S.tappedCount S.alice resolved) 4
    Spec.assertEqWith s "and 2 life paid the symbol" (S.lifeOf S.alice resolved) (Just 18)

  -- CR 616.1e: compleated and CR 614.16's multiplier are SIBLINGS on one event,
  -- not CR 616.1g's nesting -- Tamiyo, Compleated Sage's third Gatherer ruling
  -- says so outright: "Any other replacement effect that would apply to the
  -- number of loyalty counters it enters the battlefield with will apply as
  -- normal." So the controller picks, and the two orders are two boards.
  --
  -- ONE prompt, and CR 616.2 is why: on the loop's first pass nothing is pending,
  -- so neither compleated nor Doubling Season's entry-level row is applicable and
  -- CR 306.5b's is the lone candidate, applied unprompted. The other two become
  -- applicable together only once it has placed something.
  --
  -- The mana route is the SAME BOARD with one answer different, and is the
  -- negative that proves rule 702.150a's "chose to pay life": no compleated row
  -- is minted at all, so the multiplier races nothing and no order is asked. A
  -- row minted for zero symbols would subtract nothing and still cost the
  -- controller a prompt the rules do not owe.
  Spec.it s "CR 616.1e compleated and Doubling Season are ordered against each other" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    tamiyo <- S.printingOf s registry "Tamiyo, Compleated Sage"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    let (seasonId, board) = S.addPermanent doublingSeason S.alice (mixedLands forest island 3 2)
        (gs, tamiyoId) = S.handOne tamiyo board
        (askedFirst, compleatedFirst) = castAndResolveRecording (racesCompleated True seasonId) gs tamiyoId
        (_, doubledFirst) = castAndResolveRecording (racesCompleated False seasonId) gs tamiyoId
        (askedMana, manaRoute) = castAndResolveRecording (announcesBoth PhyrexianPayment.PaysMana greenMana) gs tamiyoId
    Spec.assertEqWith s "CR 702.150a then CR 614.16: 5 less two is 3, doubled is 6" (S.counterOf CounterKind.Loyalty (tamiyoOn compleatedFirst) compleatedFirst) 6
    Spec.assertEqWith s "CR 614.16 then CR 702.150a: 5 doubled is 10, less two is 8" (S.counterOf CounterKind.Loyalty (tamiyoOn doubledFirst) doubledFirst) 8
    Spec.assertEqWith s "CR 306.5b's row had no rival on the first pass, so exactly one order was asked" (length (filter wasReplacementChoice askedFirst)) 1
    Spec.assertEqWith s "the same board paying MANA: rule 702.150a never applies, so 5 doubled is 10" (S.counterOf CounterKind.Loyalty (tamiyoOn manaRoute) manaRoute) 10
    Spec.assertEqWith s "and nothing raced the multiplier, so no order was asked at all" (length (filter wasReplacementChoice askedMana)) 0

-- castAndResolve with the RESOLUTION recorded too. The entry loop runs while the
-- permanent spell resolves (CR 608.3), which is after the transcript
-- castAndResolve keeps has ended, so a CR 616.1 order asked there is invisible to
-- it. AuraSpec records the pair the same way.
castAndResolveRecording ::
  (forall r. Prompt.Prompt r -> r) ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  ([Response.Response], GameState.GameState)
castAndResolveRecording answer gs oid =
  let ((_, resolved), asked) = Replay.record answer gs (S.cast S.alice oid >> Stack.resolveTop)
   in (asked, resolved)

-- Pay the {G/U/P} with life, and answer the CR 616.1e race by SOURCE -- Doubling
-- Season's row names the enchantment, compleated's names Tamiyo -- so the
-- assertion does not rest on the engine's canonical candidate order.
-- Pawl.DamageReplacementSpec.raceIsSelf is the same idiom.
racesCompleated :: Bool -> ObjectId.ObjectId -> Prompt.Prompt r -> r
racesCompleated wantCompleated seasonId p = case p of
  Prompt.ChooseReplacement _ _ entries ->
    maybe
      (error "Pawl.ManaSymbolSpec.racesCompleated: no matching row offered")
      Int.toNaturalSaturating
      (List.findIndex ((== wantCompleated) . (/= seasonId) . ReplacementEntry.source) entries)
  _ -> announcesBoth PhyrexianPayment.PaysLife greenMana p

-- Was this recorded answer a CR 616.1 ordering choice?
-- Pawl.PreventionSpec.wasAskedToReplace as a per-response predicate, so the
-- case above can COUNT the orders asked rather than only notice one.
wasReplacementChoice :: Response.Response -> Bool
wasReplacementChoice r = case r of
  Response.ChoseReplacement _ -> True
  _ -> False

-- Answers BOTH of a hybrid Phyrexian symbol's announcements -- `way` for
-- Prompt.AnnouncePhyrexianPayment and `half` for Prompt.AnnounceHybridHalf --
-- whenever each is on offer, deferring everything else to S.identityAnswer.
-- `announces` and `announcesHalf` composed, and the "whenever it is on offer"
-- half of each is what makes the elision cases above discriminating.
announcesBoth :: PhyrexianPayment.PhyrexianPayment -> ManaType.ManaType -> Prompt.Prompt r -> r
announcesBoth way half p = case p of
  Prompt.AnnouncePhyrexianPayment {} -> announces way p
  Prompt.AnnounceHybridHalf {} -> announcesHalf half p
  _ -> S.identityAnswer p

-- Synthetic Hybrid Phyrexian Sigil ({G/U/P} Instant, "Target creature gets +1/+1
-- until end of turn") -- a card whose WHOLE mana cost is one hybrid Phyrexian
-- symbol, which no printing is.
--
-- SYNTHETIC because the rule is otherwise unobservable, not because no card
-- prints the symbol. All four printings are compleated planeswalkers and every
-- one of them prints BOTH of its symbol's colours elsewhere in the same cost
-- ({2}{G}{G/U/P}{U}, {1}{G}{G/W/P}{W}, {2}{R}{R/G/P}{G}, {1}{R}{R/W/P}{W}), so
-- CR 202.2d's clause about the SYMBOL cannot be told apart from the printed
-- coloured symbols beside it -- Tamiyo is green and blue either way. The same
-- goes for CR 202.3g's contribution of 1, which on a five-symbol cost is one
-- fifth of the answer. Mutagenic Growth plays exactly this role for the
-- monocoloured symbol one group up.
--
-- It also makes the half a BOARD fact rather than a transcript one: a Gyre
-- Engineer's "{T}: Add {G}{U}" oversupplies the cost, and which of the two
-- floats afterwards is which half was announced.
sigilSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sigilSpec s registry = Spec.describe s "SyntheticHybridPhyrexianSigil" $ do
  Spec.it s "CR 107.4f/202.2d a hybrid Phyrexian symbol makes its object BOTH colours" $ do
    sigil <- S.printingOf s registry "Synthetic Hybrid Phyrexian Sigil"
    let (oid, gs) = S.addPermanent sigil S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith
      s
      "green AND blue, from the one symbol"
      (Projection.colorsOf oid gs)
      (Set.fromList [Color.Green, Color.Blue])
    Spec.assertEqWith s "CR 202.3g: one symbol, mana value 1" (sum (fmap Quantity.manaValueOf (Game.manaCostFacesOf oid gs))) 1

  -- The half, at board level. Gyre Engineer ("{T}: Add {G}{U}") is the
  -- oversupply the Slippery Bogle case one group up uses for CR 107.4e's
  -- hybrid: one activation puts BOTH halves in the pool, the Sigil spends one,
  -- and WHICH ONE FLOATS is the announcement. An assertion that could not
  -- differ on Tamiyo's board, where every route taps all five lands.
  Spec.it s "CR 601.2b whichever half of {G/U/P} is announced, the OTHER floats" $ do
    gyreEngineer <- S.printingOf s registry "Gyre Engineer"
    piker <- S.printingOf s registry "Goblin Piker"
    sigil <- S.printingOf s registry "Synthetic Hybrid Phyrexian Sigil"
    let (_, withEngineer) = S.addPermanent gyreEngineer S.alice (Setup.emptyGame S.bothPlayers)
        (_, withPiker) = S.addPermanent piker S.alice withEngineer
        (gs, sigilId) = S.handOne sigil withPiker
        castWith half = castAndResolve (announcesBoth PhyrexianPayment.PaysMana half) gs sigilId
        (askedGreen, afterGreen) = castWith greenMana
        (askedBlue, afterBlue) = castWith blueMana
    Spec.assertEqWith s "green paid, so BLUE floats" (poolTypes S.alice afterGreen) [blueMana]
    Spec.assertEqWith s "blue paid, so GREEN floats" (poolTypes S.alice afterBlue) [greenMana]
    Spec.assertEqWith s "and both halves were really asked" (halfAnnouncements askedGreen, halfAnnouncements askedBlue) ([greenMana], [blueMana])
    Spec.assertEqWith s "neither cost life" (S.lifeOf S.alice afterGreen, S.lifeOf S.alice afterBlue) (Just 20, Just 20)

  -- Pawl.Engine.Mana.waysOf's three rows, read directly off an UNANNOUNCED
  -- cost -- no gameplay road leaves one unannounced, so this is the only
  -- reading that asks this function what the symbol
  -- costs rather than what CR 601.2b left behind. The Phyrexian group's
  -- least-life case one group up is the same reading for the monocoloured
  -- symbol.
  --
  -- Three boards, and the answers differ on all three: either component colour
  -- pays it for nothing, and with neither in play CR 107.4f's 2 life is what is
  -- left.
  Spec.it s "CR 107.4f an unannounced {G/U/P} costs no life off EITHER colour, and 2 off neither" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    let lifeOff = Mana.lifeNeeded PaymentSubject.ForNeither Cost.manaActivations ManaSpending.AsProduced S.alice hybridPhyrexianCost
    Spec.assertEqWith s "a Forest pays the green half" (lifeOff (S.landsInPlay forest 1)) (Just 0)
    Spec.assertEqWith s "an Island pays the blue half" (lifeOff (S.landsInPlay island 1)) (Just 0)
    Spec.assertEqWith s "a Mountain pays neither, so 2 life" (lifeOff (S.landsInPlay mountain 1)) (Just 2)
    Spec.assertEqWith s "and off an empty board it is 2 as well" (lifeOff (Setup.emptyGame S.bothPlayers)) (Just 2)
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice hybridPhyrexianCost (S.landsInPlay mountain 1)) "and the life route is payable at 20"

  -- CR 107.4f's third way, and the whole cost is it: no land at all, so the
  -- mana route is unpayable, nothing is asked, and 2 life casts the spell.
  Spec.it s "CR 107.4f off no land at all, 2 life pays the whole cost" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    sigil <- S.printingOf s registry "Synthetic Hybrid Phyrexian Sigil"
    let (_, withPiker) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (gs, sigilId) = S.handOne sigil withPiker
        (asked, resolved) = castAndResolve (announcesBoth PhyrexianPayment.PaysMana greenMana) gs sigilId
    Spec.assertEqWith s "CR 107.4f: 2 life" (S.lifeOf S.alice resolved) (Just 18)
    Spec.assertEqWith s "no mana route existed, so nothing was asked" (phyrexianAnnouncements asked) []
    Spec.assertEqWith s "nor about a half" (halfAnnouncements asked) []
    Spec.assertEqWith s "and the Sigil resolved" (length (GameState.stack resolved)) 0

-- The Sigil's printed cost, restated rather than read off the card -- the
-- phyrexianCost posture, and Pawl.CardSpec pins it against
-- data/cards/synthetic-hybrid-phyrexian-sigil.json.
hybridPhyrexianCost :: ManaCost.ManaCost
hybridPhyrexianCost = ManaCost.MkManaCost [ManaSymbol.HybridPhyrexian (HybridPhyrexian.MkHybridPhyrexian Color.Green Color.Blue)]

-- Tamiyo on the battlefield, `dragonOn`'s shape: the permanent the resolved
-- spell became, which is a fresh object and so not the id that was cast.
tamiyoOn :: GameState.GameState -> ObjectId.ObjectId
tamiyoOn gs =
  let isTamiyo oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Tamiyo, Compleated Sage")
   in case filter isTamiyo (Set.toAscList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> ObjectId.MkObjectId 0

-- CR 601.2h: "The player pays the total cost." Which mana leaves their pool is
-- part of that payment, and CR 107.4b makes a generic symbol one of the same
-- choices, since any type pays it.
--
-- Two units a rule can tell apart are what makes the choice observable at all.
-- The board has both axes: a Snow-Covered Mountain and a Mountain each add one
-- red mana and CR 107.4h reads the difference, while CR 106.4's retention --
-- Shizuko, Caller of Autumn's -- separates two green.
spendChoiceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spendChoiceSpec s registry = Spec.describe s "Choosing which mana to spend" $ do
  -- Gameplay level, and the assertion that can DIFFER is the second payment:
  -- the pool is two red mana, {R} takes one, and only a pool that still holds
  -- the snow one pays {S}. Both lands are tapped before the payment, so CR
  -- 601.2g's window has nothing to offer and the only question asked is which
  -- mana to spend.
  Spec.it s "CR 601.2h paying {R} out of a snow and a nonsnow red spends the one the payer names" $ do
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    mountain <- S.printingOf s registry "Mountain"
    let floated = twoRedFloated snowMountain mountain
        spends :: ManaUnit.ManaUnit -> (forall r. Prompt.Prompt r -> r)
        spends unit p = case p of
          Prompt.ChooseManaToSpend {} -> unit
          _ -> S.identityAnswer p
        payRed unit = S.runPureWith (spends unit) floated (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice redOnly)
        thenSnow gs = fst (S.runPureWith S.identityAnswer gs (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice snowCost))
        (sparedPaid, spared) = payRed plainRed
        (spentPaid, spent) = payRed snowRed
    Spec.assertBool s (thenSnow spared) "sparing the snow mana, {S} is still paid out of what is left"
    Spec.assertBool s (not (thenSnow spent)) "spending it, {S} cannot be paid"
    Spec.assertBool s (sparedPaid && spentPaid) "both boards paid the {R}"
    Spec.assertEqWith s "and the pools say which unit went" (poolOf S.alice spared, poolOf S.alice spent) ([snowRed], [plainRed])

  -- The other axis, same shape: CR 106.4's retention rides one unit, so the
  -- mana the payer spares is the mana CR 500.5's sweep does or does not take.
  Spec.it s "CR 601.2h paying {G} out of a retained and an ordinary green spends the one the payer names" $ do
    let floated = seededPool [retainedGreen, plainGreen]
        spends :: ManaUnit.ManaUnit -> (forall r. Prompt.Prompt r -> r)
        spends unit p = case p of
          Prompt.ChooseManaToSpend {} -> unit
          _ -> S.identityAnswer p
        payGreen unit = S.runPureWith (spends unit) floated (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice greenOnly)
        thenGreen gs = fst (S.runPureWith S.identityAnswer (Mana.emptyManaPools gs) (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice greenOnly))
        (sparedPaid, spared) = payGreen plainGreen
        (spentPaid, spent) = payGreen retainedGreen
    Spec.assertBool s (thenGreen spared) "sparing the retained green, a second {G} is paid after the pool empties"
    Spec.assertBool s (not (thenGreen spent)) "spending it, nothing survives to pay the second"
    Spec.assertBool s (sparedPaid && spentPaid) "both boards paid the first {G}"
    Spec.assertEqWith s "and the pools say which unit went" (poolOf S.alice spared, poolOf S.alice spent) ([retainedGreen], [plainGreen])

  -- The elision, and the reason it is sound: ManaUnit derives Eq, and two units
  -- that are equal are the same thing. Nothing the game can ask distinguishes
  -- them, so asking would be noise.
  Spec.it s "CR 601.2h two equal units raise no question" $ do
    Spec.assertEqWith s "paid, and asked nothing" (payCounting [plainRed, plainRed] redOnly) (True, 0)

  -- The elision that carries the predicate. These two units ARE distinguishable
  -- -- one is snow -- but {2} spends both whichever order they go in, so every
  -- way of paying leaves the same pool and there is no observable choice. A
  -- predicate written over the units rather than over the OUTCOMES would ask
  -- here.
  Spec.it s "CR 107.4b {2} spends the whole pool however it is paid, so nothing is asked" $ do
    Spec.assertEqWith s "paid, and asked nothing" (payCounting [snowRed, plainRed] (ManaCost.MkManaCost [ManaSymbol.Generic 2])) (True, 0)

  -- And the choice IS asked once the same pool has one unit to spare, which is
  -- what says the case above turns on the outcome rather than on the cost's
  -- shape.
  Spec.it s "CR 107.4b paying {1} out of the same two units asks which" $ do
    Spec.assertEqWith s "paid, and asked once" (payCounting [snowRed, plainRed] (ManaCost.MkManaCost [ManaSymbol.Generic 1])) (True, 1)

  -- FILTERED, NOT TRUSTED, Cost.chooseSource's posture: an answer naming a unit
  -- that was not offered must not spend one. The payment still happens, out of
  -- the first offered unit.
  Spec.it s "CR 601.2h an answer outside the offered set does not spend it" $ do
    let liar p = case p of
          Prompt.ChooseManaToSpend {} -> plainGreen
          _ -> S.identityAnswer p
        (paid, after) = S.runPureWith liar (seededPool [snowRed, plainRed]) (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 1]))
    Spec.assertBool s paid "the {1} is paid"
    Spec.assertEqWith s "out of an offered unit, and the green was never in the pool" (poolOf S.alice after) [snowRed]

-- A pool and nothing else: no permanent anywhere, so CR 601.2g's window has no
-- source to offer and the only question a payment asks is which mana to spend.
seededPool :: [ManaUnit.ManaUnit] -> GameState.GameState
seededPool units = Mana.addMana S.alice units (Setup.emptyGame S.bothPlayers)

-- Both lands tapped for their one red each, so the pool holds CR 107.4h's pair
-- as production really makes it.
twoRedFloated :: Printing.Printing -> Printing.Printing -> GameState.GameState
twoRedFloated snowMountain mountain =
  let base = Setup.emptyGame S.bothPlayers
      (snowId, withSnow) = S.addPermanent snowMountain S.alice base
      (plainId, withBoth) = S.addPermanent mountain S.alice withSnow
   in S.runPure S.identityAnswer withBoth (Cost.tapForMana S.manaPerformer snowId *> Cost.tapForMana S.manaPerformer plainId)

-- Pay `cost` out of `units`, counting the questions asked about which mana goes.
payCounting :: [ManaUnit.ManaUnit] -> ManaCost.ManaCost -> (Bool, Int)
payCounting units cost =
  let answerer :: Prompt.Prompt r -> State.State Int r
      answerer p = case p of
        Prompt.ChooseManaToSpend _ _ candidates -> do
          State.modify' (+ 1)
          pure (NonEmpty.head candidates)
        _ -> pure (S.identityAnswer p)
   in case State.runState (Engine.runGame answerer (seededPool units) (Cost.payMana S.manaPerformer PaymentSubject.ForNeither ManaSpending.AsProduced S.alice cost)) 0 of
        ((paid, _), asked) -> (paid, asked)

-- {R} and {G} on their own, the one-symbol costs the pairs above pay.
redOnly :: ManaCost.ManaCost
redOnly = ManaCost.MkManaCost [redSymbol]

greenOnly :: ManaCost.ManaCost
greenOnly = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Green)]

-- CR 107.4h's THIRD sentence: "The {S} symbol can also be used to refer to mana
-- of any type produced by a snow source spent to pay a cost." A retrospective
-- reading, where the first sentence's is a payability one -- so what it needs is
-- a record of the payment, which is Pawl.Types.Object.manaSpent, written by
-- Pawl.Engine.Cost's mana window and read back through
-- Pawl.Engine.Filter.View.manaSpentTags by Quantity.TagWasSpent.
--
-- Berg Strider is the card: "When this creature enters, tap target artifact or
-- creature an opponent controls. If {S} was spent to cast this spell, that
-- permanent doesn't untap during its controller's next untap step." Two clauses,
-- and only the second is conditioned -- so the tap happens on both boards and
-- the untap step is where they part.
--
-- CR 400.7d is load-bearing here and not incidental: the clause is an ability of
-- the PERMANENT, and what it asks about is the spell that became it, so the
-- record has to survive the one zone change CR 400.7 otherwise forgets
-- everything across.
bergStriderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bergStriderSpec s registry = Spec.describe s "BergStrider" $ do
  -- The unit's whole claim, on a PAIR of boards that differ in one thing: the
  -- four lands paying the {4} are Snow-Covered Mountains on one and Mountains on
  -- the other. Same seats, same Island paying the {U}, same Berg Strider, same
  -- target, same untap step -- so a permanent that stays tapped on one board and
  -- untaps on the other did so because of where its payment's mana came from.
  --
  -- The two behavioural assertions come first, and both can differ: an
  -- implementation that never records the payment unt-taps the first board's
  -- victim, and one that always reports snow leaves the second's tapped.
  Spec.it s "CR 107.4h whole card: Berg Strider's victim does not untap when snow mana paid for it, and does when it did not" $ do
    (snowVictim, snowBoard) <- bergBoard s registry True
    (plainVictim, plainBoard) <- bergBoard s registry False
    Spec.assertBool s (Game.isTapped snowVictim (bergUntapStep snowBoard)) "{S} was spent, so the permanent does not untap during its controller's untap step"
    Spec.assertBool s (not (Game.isTapped plainVictim (bergUntapStep plainBoard))) "and off the nonsnow board, which is the same board otherwise, it untaps"
    -- The supporting checks, and they are insensitive to the record by
    -- construction: the tap is the trigger's FIRST clause, which carries no
    -- condition at all. They are here so the assertion above cannot pass on a
    -- board where nothing was ever tapped, or where the spell never resolved.
    Spec.assertBool s (Game.isTapped snowVictim snowBoard && Game.isTapped plainVictim plainBoard) "both triggers tapped their target before either untap step"
    Spec.assertEqWith s "and both Berg Striders resolved" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Berg Strider") S.alice snowBoard, S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Berg Strider") S.alice plainBoard) (1, 1)

  -- The record itself, off the PERMANENT rather than off the spell -- CR 400.7d's
  -- exception, which the case above reads only through its consequence. The
  -- nonsnow board is the control: it spends five mana too, so a non-empty record
  -- carrying no snow tag is what says the tags come from the sources and not from
  -- the fact that something was spent.
  Spec.it s "CR 400.7d the permanent carries the mana its spell was paid with" $ do
    (_, snowBoard) <- bergBoard s registry True
    (_, plainBoard) <- bergBoard s registry False
    Spec.assertEqWith s "the snow board's Berg Strider remembers a snow tag" (bergSpentTags snowBoard) (Set.singleton ProductionTag.Snow)
    Spec.assertEqWith s "the nonsnow board's remembers the payment with no tag on it" (bergSpentTags plainBoard) Set.empty
    Spec.assertEqWith s "and both spent five mana" (bergSpentCount snowBoard, bergSpentCount plainBoard) (5, 5)

-- Alice casts Berg Strider off five untapped lands and its CR 603.6a trigger
-- resolves against bob's Goblin Piker, the only artifact or creature an opponent
-- controls. `snowy` picks what the four lands paying the {4} are and decides
-- NOTHING else; the Island paying the {U} is on both boards.
--
-- Returns bob's Piker and the board with bob made active, which is whose untap
-- step the card's second clause speaks about.
bergBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (ObjectId.ObjectId, GameState.GameState)
bergBoard s registry snowy = do
  island <- S.printingOf s registry "Island"
  filler <- S.printingOf s registry (if snowy then "Snow-Covered Mountain" else "Mountain")
  strider <- S.printingOf s registry "Berg Strider"
  piker <- S.printingOf s registry "Goblin Piker"
  let (victim, board) = S.addPermanent piker S.bob (S.landsFor filler S.alice 4 (S.landsInPlay island 1))
      (gs, spellId) = S.handOne strider board
      resolved = S.runPure S.identityAnswer gs (S.cast S.alice spellId *> Stack.resolveTop *> Engine.placePendingTriggers *> Stack.resolveTop)
  pure (victim, resolved {GameState.activePlayer = S.bob})

-- The untap step's turn-based actions, run for whoever the state says is active
-- (CR 502.3) -- Pawl.UntapRestrictionSpec's `untapStep`, kept here rather than
-- shared, since Pawl.Support rebuilds every spec in the tree.
bergUntapStep :: GameState.GameState -> GameState.GameState
bergUntapStep gs = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))

-- Alice's Berg Strider on the battlefield, or a placeholder id that reads as
-- nothing -- so a board where the spell never resolved answers empty rather than
-- throwing.
bergStriderOn :: GameState.GameState -> ObjectId.ObjectId
bergStriderOn gs =
  let isBerg oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Berg Strider")
   in case filter isBerg (Set.toAscList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> ObjectId.MkObjectId 0

bergSpent :: GameState.GameState -> [ManaUnit.ManaUnit]
bergSpent gs = foldMap (Mana.Type.unwrap . Object.manaSpent) (Game.lookupObject (bergStriderOn gs) gs)

bergSpentTags :: GameState.GameState -> Set.Set ProductionTag.ProductionTag
bergSpentTags = foldMap ManaUnit.tags . bergSpent

bergSpentCount :: GameState.GameState -> Int
bergSpentCount = length . bergSpent

-- CR 602.2a's ability object as CR 400.7d's other record-keeper: "That ability is
-- created on the stack as an object that's not a card." An ACTIVATION's mana is
-- recorded there and not on the source permanent, whose own record is the mana
-- that cast the spell it became -- so activating an ability cannot overwrite what
-- Berg Strider above reads.
--
-- Forsworn Paladin is the card: "{1}{B}, {T}, Pay 1 life: Create a Treasure
-- token." and "{2}{B}: Target creature gets +2/+0 until end of turn. If mana from
-- a Treasure was spent to activate this ability, that creature also gains
-- deathtouch until end of turn." Two clauses on the second ability, and only the
-- second is conditioned -- so the pump happens on both boards and the deathtouch
-- is where they part.
--
-- The card reaches the record through Quantity.AgainstSlot aimed at
-- Binding.thisAbility, which Pawl.Engine.Activate stamps: the clause is gated
-- against the SOURCE (Resolve.gateHolds), and the source is the Paladin rather
-- than the ability whose payment the printed sentence asks about.
forswornPaladinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
forswornPaladinSpec s registry = Spec.describe s "ForswornPaladin" $ do
  -- ONE board, run twice with answerers that differ in a single choice: whether
  -- the Treasure is tapped for one of the {2}{B} or spared and a fourth Swamp
  -- tapped instead. Same seats, same Treasure sitting on the battlefield, same
  -- target, same life total, same everything else -- so a creature that gains
  -- deathtouch on one run and not the other did so because of where the mana came
  -- from.
  --
  -- The two behavioural assertions come first and both can differ: an engine that
  -- records nothing for an activation leaves the first run without deathtouch, and
  -- one that tags every mana leaves the second with it.
  Spec.it s "CR 602.2a whole card: Forsworn Paladin's target gains deathtouch when a Treasure's mana paid for the ability, and not when Swamps did" $ do
    swamp <- S.printingOf s registry "Swamp"
    paladin <- S.printingOf s registry "Forsworn Paladin"
    piker <- S.printingOf s registry "Goblin Piker"
    case Face.activatedAbilities (S.combinedFace paladin) of
      makeTreasure : pump : _ -> do
        let (paladinId, g1) = S.addPermanent paladin S.alice (S.landsInPlay swamp 6)
            (victim, g2) = S.addPermanent piker S.bob g1
            armed = S.runPure S.identityAnswer g2 (Activate.activateAbility S.alice paladinId makeTreasure *> Stack.resolveTop)
            treasure = treasureIn armed
            act = Activate.activateAbility S.alice paladinId pump *> Stack.resolveTop
            spending = S.runPure (aimedAtSpending victim treasure) armed act
            sparing = S.runPure (aimedAtSparing victim treasure) armed act
        Spec.assertBool s (Projection.hasKeyword Keyword.Deathtouch victim spending) "the Treasure's mana paid the activation, so the Piker gains deathtouch"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Deathtouch victim sparing)) "and on the same board with the Treasure spared, which differs in nothing else, it does not"
        -- The supporting checks, after the behaviour. The pump carries no
        -- condition at all, so it is insensitive to the record by construction and
        -- says only that both runs resolved the ability against the same target.
        Spec.assertEqWith s "setup: both runs ran the unconditional clause, so the 2/1 Piker is 4/1 on each" (S.powerToughnessOf victim spending, S.powerToughnessOf victim sparing) (Just (4, 1), Just (4, 1))
        -- And that the Treasure was really there to be chosen, so the second run's
        -- "no deathtouch" is the answer and not an empty offer.
        Spec.assertBool s (Set.member treasure (GameState.battlefield armed)) "setup: the first ability really made a Treasure, so both answerers were offered it"
        Spec.assertBool s (not (Set.member treasure (GameState.battlefield spending))) "setup: the run that spent it sacrificed it, and the run that spared it did not"
        Spec.assertBool s (Set.member treasure (GameState.battlefield sparing)) "setup: the Treasure survives the run that spared it"
      _ -> Spec.assertFailure s "Forsworn Paladin should print two activated abilities"

  -- The record itself, off the ABILITY OBJECT rather than off the permanent --
  -- which the case above reads only through its consequence. The Paladin is the
  -- control: it was never cast on this board, so an activation that recorded
  -- against its source would show up here as a non-empty record.
  Spec.it s "CR 400.7d an activation's record goes on the ability object, not on its source" $ do
    swamp <- S.printingOf s registry "Swamp"
    paladin <- S.printingOf s registry "Forsworn Paladin"
    piker <- S.printingOf s registry "Goblin Piker"
    case Face.activatedAbilities (S.combinedFace paladin) of
      makeTreasure : pump : _ -> do
        let (paladinId, g1) = S.addPermanent paladin S.alice (S.landsInPlay swamp 6)
            (_, g2) = S.addPermanent piker S.bob g1
            armed = S.runPure S.identityAnswer g2 (Activate.activateAbility S.alice paladinId makeTreasure *> Stack.resolveTop)
            treasure = treasureIn armed
            onStack = S.runPure (aimedAtSpending paladinId treasure) armed (Activate.activateAbility S.alice paladinId pump)
            abilityId = case GameState.stack onStack of
              oid : _ -> oid
              [] -> ObjectId.MkObjectId 0
            tagsOn oid gs = foldMap ManaUnit.tags (foldMap (Mana.Type.unwrap . Object.manaSpent) (Game.lookupObject oid gs))
        Spec.assertEqWith s "the ability object remembers a Treasure tag" (tagsOn abilityId onStack) (Set.singleton ProductionTag.Treasure)
        Spec.assertEqWith s "and the Paladin, whose ability it is, remembers nothing" (tagsOn paladinId onStack) Set.empty
        Spec.assertEqWith s "and three mana paid the {2}{B}" (length (foldMap (Mana.Type.unwrap . Object.manaSpent) (Game.lookupObject abilityId onStack))) 3
        Spec.assertBool s (abilityId /= paladinId) "setup: CR 602.2a's ability object is not the permanent whose ability it is"
      _ -> Spec.assertFailure s "Forsworn Paladin should print two activated abilities"

-- CR 601.2c's target aimed at `victim` and CR 602.2b's mana window aimed at the
-- Treasure -- the two choices Forsworn Paladin's second ability offers, answered
-- in one interpreter so the pair of runs below can differ in the second alone.
--
-- The offer is FILTERED and not answered with a hand-built recipient (S.preferring),
-- so a slot that never offered the Piker takes the fallback rather than the
-- assertion's own object.
aimedAtSpending :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAtSpending victim wanted p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (\r -> Recipient.objectOf r == Just victim) sets
  _ -> prefersSource wanted p

-- aimedAtSpending's twin, differing in the mana source alone.
aimedAtSparing :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAtSparing victim unwanted p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (\r -> Recipient.objectOf r == Just victim) sets
  _ -> avoidsSource unwanted p

-- The Treasure token on the battlefield, or a placeholder id that reads as
-- nothing -- bergStriderOn's shape, so a board where the first ability never
-- resolved answers empty rather than throwing.
treasureIn :: GameState.GameState -> ObjectId.ObjectId
treasureIn gs =
  let isTreasure oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Treasure Token")
   in case filter isTreasure (Set.toAscList (GameState.battlefield gs)) of
        oid : _ -> oid
        [] -> ObjectId.MkObjectId 0

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Mana" $ do
  hybridSpec s registry
  shuYunSpec s registry
  monocoloredHybridSpec s registry
  phyrexianSpec s registry
  totalCostSpec s registry
  dismemberSpec s registry
  phyrexianTollSpec s registry
  moltensteelSpec s registry
  tamiyoSpec s registry
  sigilSpec s registry
  snowSpec s registry
  snowSymbolSpec s registry
  celestialDawnSpec s registry
  spendChoiceSpec s registry
  bergStriderSpec s registry
  forswornPaladinSpec s registry
