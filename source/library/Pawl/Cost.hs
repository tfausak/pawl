-- CR 118: what a cost IS, and everything the closed half needs to do with one --
-- what the candidates are (costsFor), what the total is (total, CR 601.2f),
-- whether it can be paid (canPay, CR 118.3) and paying it (pay, CR 601.2g/h).
-- Pawl.Mana keeps pools, production and spending; this module keeps the cost.
--
-- The SOLE casing home for Pawl.Type.CostComponent. Pawl.Cast and Pawl.Activate
-- learn nothing about which components exist: they ask "can this be paid" and
-- "pay it", and read one classification (requiresTapSymbol) for CR 302.6.
module Pawl.Cost where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.PlayerEffect as PlayerEffect
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.Card as Card
import Pawl.Type.Cost (Cost)
import qualified Pawl.Type.Cost as Cost
import qualified Pawl.Type.CostComponent as CostComponent
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Payment as Payment
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Zone as Zone

-- CR 118.6: the cost of an object with no mana cost. Also the total answer the
-- ChooseCost fallback needs when no candidate was offered -- a state the engine
-- never produces, because the prompt is issued only with two or more payable
-- candidates, and an answer outside the offered set is rejected anyway.
unpayable :: Cost
unpayable = Cost.MkCost {Cost.mana = Nothing, Cost.components = []}

-- The first offered candidate, or `unpayable` when none was offered. The one
-- total, documented answer every ChooseCost fallback uses.
firstOffered :: [Cost] -> Cost
firstOffered candidates = case candidates of
  c : _ -> c
  [] -> unpayable

-- The candidate costs for CASTING this object (CR 601.2b), printed one first.
-- Empty for anything that is not a card: a token is created onto the
-- battlefield and never cast, and an ability on the stack is not a spell.
--
-- A LAND yields one candidate whose mana part is Nothing -- CR 202.1's "a card's
-- mana cost", absent -- which CR 118.6 makes unpayable, so canPay says False and
-- Cast.castable never offers it. That is the same answer the retired
-- Cast.costOf's Nothing gave, arrived at by the rule instead of by a special
-- case.
costsFor :: ObjectId -> GameState -> [Cost]
costsFor oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Object.source obj of
    Source.OfCard printing ->
      let card = Printing.card printing
       in [Cost.MkCost {Cost.mana = Card.manaCost card, Cost.components = []}]
    Source.OfToken _ -> []
    Source.OfAbility _ _ -> []
    Source.OfTrigger _ _ -> []

-- CR 601.2f: "The total cost is the mana cost or alternative cost (as determined
-- in rule 601.2b), plus all additional costs and cost increases, and minus all
-- cost reductions." `cost` arrives with X already substituted, because CR 601.2b
-- precedes 601.2f.
--
-- The mana part alone is adjusted, and the components are carried through
-- untouched: every increase and reduction CR 601.2f describes, and every one P7
-- can express, is an amount of mana (CR 118.7a routes it to the generic
-- component). Nor is the result ever "locked in": CR 601.2f's own last sentence
-- makes the total fixed once determined, but this is recomputed fresh from the
-- current game state on every call (#94).
--
-- CR 118.6a's first sentence needs no special case: "If an unpayable cost is
-- increased by an effect or an additional cost is imposed, the cost is still
-- unpayable" -- fmap over the Maybe leaves Nothing as Nothing.
total :: PlayerId -> ObjectId -> Cost -> GameState -> Cost
total pid oid cost gs =
  cost {Cost.mana = fmap (applyAdjustments (PlayerEffect.costAdjustments pid oid gs)) (Cost.mana cost)}

-- CR 601.2b: substitute the chosen value of X into the mana part. Identity on a
-- Variable-free cost, and on an unpayable one.
substituteX :: Natural -> Cost -> Cost
substituteX x cost = cost {Cost.mana = fmap (Mana.substituteX x) (Cost.mana cost)}

-- Does this cost's mana part contain an {X} (CR 107.3)? What decides whether the
-- caster is asked for a value at CR 601.2b -- a spell with no {X} is not asked.
hasVariable :: Cost -> Bool
hasVariable cost = case Cost.mana cost of
  Nothing -> False
  Just (ManaCost.MkManaCost symbols) -> elem ManaSymbol.Variable symbols

-- CR 302.6 / 107.5: does paying this cost require tapping the object it is on?
-- The CLASSIFICATION Pawl.Activate reads for the summoning-sickness gate, so
-- that this module stays the only one matching a CostComponent constructor.
requiresTapSymbol :: Cost -> Bool
requiresTapSymbol cost = elem CostComponent.TapThis (Cost.components cost)

-- CR 118.3: "A player can't pay a cost without having the necessary resources to
-- pay it fully." The mana part AND every component, measured against the CURRENT
-- state -- before any part of the cost is paid. That is CR-correct rather than
-- convenient: CR 601.2g gives the mana window BEFORE CR 601.2h's payment, so a
-- Mountain tapped for mana is still on the battlefield to be sacrificed
-- afterwards, and sacrificing a permanent never retroactively unmakes the mana
-- it produced.
--
-- CR 118.6: an unpayable cost is never payable ("attempting to pay an unpayable
-- cost is an illegal action").
canPay :: PlayerId -> ObjectId -> Cost -> GameState -> Bool
canPay pid oid cost gs = case Cost.mana cost of
  Nothing -> False
  Just manaCost ->
    Mana.canPay pid manaCost gs
      && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)

canPayComponent :: PlayerId -> ObjectId -> CostComponent.CostComponent -> GameState -> Bool
canPayComponent pid oid component gs = case component of
  -- CR 107.5: "A permanent that's already tapped can't be tapped again to pay
  -- the cost." CR 118.3 gives the same example.
  CostComponent.TapThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Object.zone obj == Zone.Battlefield && Object.tapped obj == TapState.Untapped
  -- CR 701.21a: only a permanent, and only one this player controls.
  CostComponent.SacrificeThis ->
    Set.member oid (GameState.battlefield gs) && Projection.controllerOf oid gs == Just pid

-- CR 601.2g then 601.2h: the mana window first, then the payment. Components are
-- paid in PRINTED order; CR 601.2h lets the player pay in any order, which is an
-- elision here (#N) -- no component in this vocabulary changes another's
-- payability.
--
-- All or nothing. CR 601.2h: "Partial payments are not allowed." The entry state
-- is captured and restored on any rejection, so an Unpaid result is a complete
-- no-op even though paying is monadic and a component may prompt.
pay :: PlayerId -> ObjectId -> Cost -> Game Payment.Payment
pay pid oid cost = do
  before <- State.get
  case Cost.mana cost of
    -- CR 118.6: attempting to pay an unpayable cost is an illegal action.
    Nothing -> pure Payment.Unpaid
    Just manaCost -> case Mana.payCost pid manaCost before of
      Nothing -> pure Payment.Unpaid
      Just afterMana -> do
        State.put afterMana
        outcome <- payComponents pid oid (Cost.components cost)
        case outcome of
          Payment.Paid -> pure Payment.Paid
          Payment.Unpaid -> do
            State.put before
            pure Payment.Unpaid

payComponents :: PlayerId -> ObjectId -> [CostComponent.CostComponent] -> Game Payment.Payment
payComponents pid oid components = case components of
  [] -> pure Payment.Paid
  component : rest -> do
    outcome <- payComponent pid oid component
    case outcome of
      Payment.Unpaid -> pure Payment.Unpaid
      Payment.Paid -> payComponents pid oid rest

payComponent :: PlayerId -> ObjectId -> CostComponent.CostComponent -> Game Payment.Payment
payComponent _ oid component = case component of
  CostComponent.TapThis -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)})
    pure Payment.Paid
  -- Through Event.sacrifice, the CR 701.21 funnel, and never a direct zone poke:
  -- a cost payment is a game event, so dies-triggers, replacement effects and the
  -- turn history all see it.
  CostComponent.SacrificeThis -> do
    Event.sacrifice oid
    pure Payment.Paid

-- The arithmetic half, pure and board-free.
--
-- 1. Every INCREASE is added to the generic component (CR 601.2f's order, and
--    Thalia's own ruling: "add any cost increases, then apply any cost
--    reductions").
-- 2. Every REDUCTION comes off the generic component ONLY (CR 118.7a: "Effects
--    that reduce a cost by an amount of generic mana affect only the generic
--    mana component of that cost. They can't affect the colored or colorless
--    mana components."), floored at zero -- a reduction with no generic left to
--    take is simply lost.
-- 3. CR 601.2f's "if the mana component of the total cost is reduced to nothing
--    ... it is considered to be {0}. It can't be reduced to less than {0}" needs
--    no special case: ManaCost is a list of symbols and the empty list IS {0}.
--
-- Reductions are SUMMED rather than applied one at a time. CR 601.2f's "if
-- multiple cost reductions apply, the player may apply them in any order" is a
-- prompt in the rules and an elision here (#88): every reduction P7 can express
-- is an amount of generic mana routed to the same component by CR 118.7a, so
-- summing is not merely equivalent to some order -- it is equivalent to EVERY
-- order.
--
-- The result is CANONICAL: one leading Generic symbol carrying the whole generic
-- component (omitted entirely when it is zero), then the printed typed symbols in
-- their original order. Presentation, not semantics -- Mana.spend sums every
-- generic symbol and matches typed symbols first -- but it is what makes a total
-- cost comparable, so "{U} taxed and then discounted is exactly {U}" is a
-- statement a test can make.
applyAdjustments :: ([Natural], [Natural]) -> ManaCost.ManaCost -> ManaCost.ManaCost
applyAdjustments adjustments cost =
  let (increases, reductions) = adjustments
      ManaCost.MkManaCost symbols = cost
      genericOf symbol = case symbol of
        ManaSymbol.Generic n -> n
        ManaSymbol.OfType _ -> 0
        -- Unreachable: CR 601.2b precedes 601.2f, so Mana.substituteX has
        -- already replaced every Variable before a total cost is computed. The
        -- match must be total, so a bare {X} contributes 0 generic.
        ManaSymbol.Variable -> 0
      isTyped symbol = case symbol of
        ManaSymbol.Generic _ -> False
        ManaSymbol.OfType _ -> True
        -- Unreachable for the same reason genericOf's Variable arm is: kept
        -- (retained, not stripped) so that if it ever were reachable, a bare
        -- {X} would still be treated as typed and survive the filter below.
        ManaSymbol.Variable -> True
      raised = sum (map genericOf symbols) + sum increases
      taken = sum reductions
      -- Natural subtraction is PARTIAL (it throws on underflow), so the CR
      -- 601.2f floor is also what keeps this total.
      lowered = if raised >= taken then raised - taken else 0
      leading = if lowered == 0 then [] else [ManaSymbol.Generic lowered]
   in ManaCost.MkManaCost (leading ++ filter isTyped symbols)
