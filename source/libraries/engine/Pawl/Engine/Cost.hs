-- CR 118: what a cost IS, and everything the closed half needs to do with one --
-- what the candidates are (costsFor), what the total is (total, CR 601.2f),
-- whether it can be paid (canPay, CR 118.3) and paying it (pay).
-- Pawl.Engine.Mana keeps pools, production and spending; this module keeps the cost.
--
-- `pay` serves TWO contexts, and only the first is CR 601.2g/h: a cost paid as a
-- spell is cast or an ability activated, and CR 118.12's cost paid when one
-- RESOLVES (Pawl.Engine.Resolve.paid). `total`'s CR 601.2f adjustments reach only
-- the first, because that rule totals the cost of a spell being cast or an ability
-- being activated and a resolution cost is neither.
--
-- The SOLE casing home for Pawl.Types.CostComponent. Pawl.Engine.Cast,
-- Pawl.Engine.Activate and Pawl.Engine.Resolve learn nothing about which
-- components exist: they ask "can this be paid" and "pay it", and read one
-- classification (requiresSicknessCheck) for CR 302.6.
module Pawl.Engine.Cost where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.CardName as CardName
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 118.6: the cost of an object with no mana cost. Also the total answer the
-- ChooseCost fallback needs when no candidate was offered -- a state the engine
-- never produces, because the prompt is issued only with two or more payable
-- candidates, and an answer outside the offered set is rejected anyway.
unpayable :: Cost Keyword.Type.Keyword
unpayable = Cost.MkCost {Cost.mana = Nothing, Cost.components = []}

-- The first offered candidate, or `unpayable` when none was offered. The one
-- total, documented answer every ChooseCost fallback uses.
firstOffered :: [Cost Keyword.Type.Keyword] -> Cost Keyword.Type.Keyword
firstOffered candidates = case candidates of
  c : _ -> c
  [] -> unpayable

-- The candidate costs for CASTING this object (CR 601.2b) -- from hand, the
-- printed one first and then each alternative. Empty for anything that is not a
-- card: a token is created onto the battlefield and never cast, and an ability
-- on the stack is not a spell.
--
-- A LAND yields one candidate whose mana part is Nothing (CR 202.1's "a card's
-- mana cost", absent), which CR 118.6 makes unpayable, so canPay says False and
-- Cast.castable never offers it -- the rule rather than a special case.
--
-- The candidates depend on the ZONE the object is being cast from, because CR
-- 702.34a's permission and its cost are one sentence. From a graveyard the
-- flashback cost is the ONLY candidate -- nothing permits casting the card from
-- there for its printed mana cost -- and a card with no flashback yields no
-- candidate at all, which is CR 601.3's default prohibition arriving through
-- Cast.castable's affordability gate as well as through its permission gate.
--
-- They also depend on WHICH FACE is being cast (CR 709.3a: "Only the chosen
-- half is evaluated to see if it can be cast"), which is why the name arrives
-- as an argument rather than being read off the object. CR 709.4b's combined
-- mana cost is what a split card HAS outside the stack, and is emphatically not
-- what casting one half pays.
costsFor :: CardName.CardName -> ObjectId -> GameState -> [Cost Keyword.Type.Keyword]
costsFor name oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Object.source obj of
    Source.OfCard printing ->
      let face = Game.resolveFace (Just name) (Printing.card printing)
          printed = Cost.MkCost {Cost.mana = Face.manaCost face, Cost.components = Face.additionalCosts face}
          -- CR 118.9d: an alternative replaces only the MANA cost; every
          -- additional cost still applies. The increases and reductions are
          -- Pawl.Engine.Cost.total's job, called on whichever candidate is
          -- chosen. CR 702.34a's own last sentence sends flashback through the
          -- same rules, so its cost is wrapped identically.
          withAdditional alternative =
            alternative {Cost.components = Cost.components alternative <> Face.additionalCosts face}
       in case Object.zone obj of
            Zone.Graveyard -> fmap withAdditional (Maybe.maybeToList (Keyword.flashbackCost (Face.keywords face)))
            _ -> printed : fmap withAdditional (Face.alternativeCosts face)
    Source.OfToken _ -> []
    Source.OfAbility _ _ -> []
    Source.OfTrigger _ _ -> []
    Source.OfEmblem _ -> []
    Source.OfInherentTrigger _ _ -> []

-- CR 601.2f: the mana or alternative cost, plus all additional costs and cost
-- increases, minus all cost reductions. `cost` arrives with X already
-- substituted, because CR 601.2b precedes 601.2f.
--
-- The mana part alone is adjusted, and the components are carried through
-- untouched: every increase and reduction pawl can express is an amount of MANA,
-- so there is nothing for a CostComponent to absorb. WHICH part of the mana cost
-- each one lands on is applyAdjustments's business. Nor is the result ever
-- "locked in": CR 601.2f fixes the total once determined, but this is recomputed
-- fresh from the current game state on every call (#94).
--
-- CR 118.6a's first sentence needs no special case: fmap over the Maybe leaves
-- Nothing as Nothing.
total :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Cost Keyword.Type.Keyword
total pid oid cost gs = cost {Cost.mana = fmap (totalMana pid oid gs) (Cost.mana cost)}

-- CR 601.2f's totalling of the MANA part alone, curried so that it is a function
-- of one mana cost. `total` above is this fmapped over a whole Cost's mana part;
-- what wants it separately is `announce`, which has to ask "what will this cost
-- once 601.2f has run?" of candidate costs that do not exist yet and never become
-- a Cost of their own.
totalMana :: PlayerId -> ObjectId -> GameState -> ManaCost.ManaCost -> ManaCost.ManaCost
totalMana pid oid gs = applyAdjustments (PlayerEffect.costAdjustments pid oid gs)

-- CR 601.2f's ADDITIONAL-COSTS clause alone, bolted onto one candidate -- the
-- shape CR 702.42a's entwine needs. Pawl.Engine.Cast applies it to whichever
-- candidate the caster announced, exactly as CR 118.9d says an additional cost
-- applies to an alternative one; the increases and reductions stay `total`'s job,
-- run on the result.
--
-- The mana parts CONCATENATE (CR 601.2f's totalling later pools the generic
-- symbols), and the components are appended in the same order, so `pay` charges
-- the base cost's components before the additional one's.
--
-- CR 118.6a: either side being Nothing leaves the whole thing unpayable, which
-- the applicative on Maybe gives for free.
plus :: Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
plus base extra =
  let combine (ManaCost.MkManaCost xs) (ManaCost.MkManaCost ys) = ManaCost.MkManaCost (xs <> ys)
   in Cost.MkCost
        { Cost.mana = combine <$> Cost.mana base <*> Cost.mana extra,
          Cost.components = Cost.components base <> Cost.components extra
        }

-- CR 601.2b: substitute the chosen value of X into the mana part. Identity on a
-- Variable-free cost, and on an unpayable one.
substituteX :: Natural -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
substituteX x cost = cost {Cost.mana = fmap (Mana.substituteX x) (Cost.mana cost)}

-- Does this cost's mana part contain an {X} (CR 107.3)? What decides whether the
-- caster is asked for a value at CR 601.2b -- a spell with no {X} is not asked,
-- and CR 602.2b sends an activation cost through the same question.
hasVariable :: Cost Keyword.Type.Keyword -> Bool
hasVariable cost = case Cost.mana cost of
  Nothing -> False
  Just (ManaCost.MkManaCost symbols) -> elem ManaSymbol.Variable symbols

-- CR 601.2b: the greatest value of X this player could actually pay for -- what
-- Prompt.ChooseX carries -- found by ASCENDING SEARCH from 0 over the caller's
-- own payability-at-X predicate. Advisory, and nothing here clamps: see
-- Prompt.ChooseX for why announcing past this is legal and what it costs.
--
-- The PREDICATE is the caller's, because the two callers measure different costs:
-- a spell's goes through CR 601.2f's totalling (Cast.payableCostAt), an
-- activation cost is not routed through `total` at all (Activate.payableCostAt,
-- #90). What each must hand in is the SAME predicate its own castability /
-- activatability gate asked at CR 601.2b's X=0 floor, so that what a gate
-- measures and what a bound reports cannot drift apart.
--
-- SOUND AND TERMINATING only because payability is MONOTONE in X -- unpayable at
-- n means unpayable at every value above n, while the demand grows without bound
-- and the supplies are finite. That is a property of the PREDICATE and is
-- discharged at the call site: Cast.affordableX carries the argument in full, and
-- Activate.affordableX's cost is the same one with CR 601.2f's totalling taken
-- out, which can only shorten it.
--
-- Answers 0 for a cost with no {X} in it, a totality guard rather than a rule:
-- the climb would never end and there is no variable to report a greatest value
-- of. Neither caller asks -- both gate the prompt on the same `hasVariable`. Also
-- 0 for a cost unpayable even at X=0, the least misleading number to report.
greatestPayableX :: (Natural -> Bool) -> Cost Keyword.Type.Keyword -> Natural
greatestPayableX payableAt cost =
  let climb x = if payableAt (x + 1) then climb (x + 1) else x
   in if hasVariable cost then climb 0 else 0

-- CR 118.13a: a mana symbol that can be paid in multiple ways has its payment
-- chosen as the spell or ability is proposed (CR 601.2b). The seam
-- Pawl.Engine.Cast and Pawl.Engine.Activate call at exactly that moment, one step
-- before CR 601.2f's total.
--
-- Only CR 107.4f's Phyrexian symbol is announced. CR 107.4e's hybrids are the
-- other "paid in multiple ways" symbols and they are NOT announced here (#261):
-- Pawl.Engine.Mana still resolves them at payment.
--
-- The life the announcement committed becomes a CostComponent.PayLife, which is
-- the rule's own words rather than a re-encoding: CR 107.4f pays 2 life for the
-- symbol and CR 119.4 governs paying life wherever it comes from. That makes the
-- returned cost CR 601.2b's "nonhybrid equivalent cost" in full. Omitted entirely
-- at zero, so a cost with no Phyrexian symbol comes back untouched. APPENDED
-- rather than merged into a PayLife the cost already carries, so #365 survives
-- unaltered: Cost.canPay measures each component separately, and two PayLife
-- components of one cost can together outrun a life total admitting each.
--
-- `total` is CR 601.2f's totalling, the CALLER's to supply because the two
-- callers genuinely differ. Pawl.Engine.Cast passes `totalMana`, so a spell's
-- announcement is measured against the same adjusted cost `Cast.payableCost`
-- gated on -- against the printed cost instead, a reduction could hide a route
-- and this function elide the prompt. Pawl.Engine.Activate passes `id`, because
-- an activation cost is deliberately not routed through `total` at all (#90).
-- When #90 lands both sites change together, which is the point of the parameter.
--
-- The COST that arrives already carries CR 601.2b's announced value of X, since
-- that rule puts the value of the variable before this announcement. Named
-- `total_` only because this module's own `total` is in scope.
announce :: PlayerId -> ObjectId -> (ManaCost.ManaCost -> ManaCost.ManaCost) -> Cost Keyword.Type.Keyword -> Game (Cost Keyword.Type.Keyword)
announce pid oid total_ cost = case Cost.mana cost of
  -- CR 118.6: an object with no mana cost has no mana symbols to announce.
  Nothing -> pure cost
  Just manaCost -> do
    (announced, life) <- Mana.announcePhyrexian pid oid total_ manaCost
    pure
      cost
        { Cost.mana = Just announced,
          Cost.components =
            Cost.components cost <> (if life > 0 then [CostComponent.PayLife life] else [])
        }

-- CR 302.6: does paying this cost put the object's ability behind the
-- summoning-sickness gate? The CLASSIFICATION Pawl.Engine.Activate reads, so that
-- this module stays the only one matching a CostComponent constructor.
--
-- BOTH symbols, because CR 302.6 names both: CR 107.5's tap symbol and CR 107.6's
-- untap symbol, from each of which CR 702.10c's haste grants the same exemption.
-- Named for the RULE it answers rather than for one of the two symbols.
requiresSicknessCheck :: Cost Keyword.Type.Keyword -> Bool
requiresSicknessCheck cost =
  any (\c -> elem c (Cost.components cost)) [CostComponent.TapThis, CostComponent.UntapThis]

-- CR 606.2: an activated ability with a loyalty symbol in its cost is a loyalty
-- ability. The CLASSIFICATION Pawl.Engine.Activate reads for CR 606.3's window
-- and once-per-turn limit, the requiresSicknessCheck shape.
--
-- Derived from the cost rather than stored on the ability, because CR 606.2 is a
-- rule about what a cost CONTAINS and not a rider a card prints. That is also
-- why Jace Beleren's abilities carry no ActivationTiming.SorcerySpeed: the
-- sorcery-speed half of CR 606.3 is the rules core's to know, and a card file
-- claiming a rider it does not print would be the open half teaching the closed
-- half a rule it already has.
isLoyaltyCost :: Cost Keyword.Type.Keyword -> Bool
isLoyaltyCost cost = any isLoyaltyComponent (Cost.components cost)

isLoyaltyComponent :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
isLoyaltyComponent component = case component of
  CostComponent.AddLoyaltyToThis _ -> True
  CostComponent.RemoveLoyaltyFromThis _ -> True
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.SacrificeThis -> False
  CostComponent.PayLife _ -> False
  CostComponent.Sacrifice _ _ -> False
  CostComponent.DiscardCards _ -> False
  CostComponent.DiscardThis -> False
  CostComponent.PayEnergy _ -> False

-- CR 306.5c: a planeswalker's loyalty is the number of loyalty counters on it.
-- Zero for an object with none, which CR 704.5i then reads as loyalty 0 -- so
-- this is deliberately only ever asked of something already known to be a
-- planeswalker.
loyaltyCountersOn :: ObjectId -> GameState -> Natural
loyaltyCountersOn oid gs =
  maybe 0 (Map.findWithDefault 0 CounterKind.Loyalty . Object.counters) (Game.lookupObject oid gs)

addLoyalty :: Natural -> Object.Object -> Object.Object
addLoyalty n obj = obj {Object.counters = Map.insertWith (+) CounterKind.Loyalty n (Object.counters obj)}

removeLoyalty :: Natural -> Object.Object -> Object.Object
removeLoyalty n obj =
  let have = Map.findWithDefault 0 CounterKind.Loyalty (Object.counters obj)
   in obj {Object.counters = Map.insert CounterKind.Loyalty (Natural.minusSaturating have n) (Object.counters obj)}

-- Which permanents a Filter admits, matched through the PROJECTION and never
-- against printed characteristics: card types and subtypes are CR 613.1d layer 4,
-- so Blood Moon changes the answer. A sacrifice cost frames no player, so the
-- perspective is Nothing (its filters never reference one).
--
-- The lower Pawl.Engine.Filter is the ONE matcher: Pawl.Engine.Replacement
-- narrows its permanents through the same call, so there is no duplicate to keep
-- in step and no Cost->Replacement cycle to avoid (#111).
matchesFilter :: GameState -> Filter.Type.Filter Keyword.Type.Keyword -> ObjectId -> Bool
matchesFilter gs filter_ oid =
  -- No source in scope at this site.
  Filter.matches (Filter.MkContext Nothing Nothing) (Projection.viewOfObject oid gs) filter_

-- The permanents this player may sacrifice for a Filter, ascending -- the order
-- ChooseSacrifices offers them in, which is what makes both the elision test and
-- the transcript fallback deterministic.
sacrificeCandidates :: PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
sacrificeCandidates pid filter_ gs =
  List.sort (filter (matchesFilter gs filter_) (Projection.controls pid gs))

-- The cards this player may discard to pay a cost on `oid`: their hand, in its
-- own order, minus `oid` itself. See canPayComponent's DiscardCards arm for why
-- the exclusion is CR 601.2a and not a convenience. Hand order rather than
-- sorted, unlike sacrificeCandidates: Game.zoneMembers already returns a hand in
-- a fixed order, which Prompt.ChooseDiscard offers it in.
discardCandidates :: PlayerId -> ObjectId -> GameState -> [ObjectId]
discardCandidates pid oid gs = filter (/= oid) (Game.zoneMembers Zone.Hand pid gs)

-- CR 118.3: a player can't pay a cost without the resources to pay it fully. The
-- mana part AND every component, measured against the CURRENT state -- before any
-- part of the cost is paid. That is CR-correct rather than convenient: CR 601.2g
-- gives the mana window BEFORE CR 601.2h's payment, so a Mountain tapped for mana
-- is still on the battlefield to be sacrificed afterwards.
--
-- CR 118.6: an unpayable cost is never payable.
--
-- The mana part and the components are measured SEPARATELY, so a resource both
-- could claim is counted twice. CR 107.4f's Phyrexian symbol is the first mana
-- symbol that spends life, so a cost holding one alongside a PayLife component
-- can read as payable when CR 118.3 says it is not (#365).
canPay :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPay pid oid cost gs = case Cost.mana cost of
  Nothing -> False
  Just manaCost ->
    Mana.canPay pid manaCost gs
      && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)

canPayComponent :: PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> GameState -> Bool
canPayComponent pid oid component gs = case component of
  -- CR 107.5: a permanent that's already tapped can't be tapped again to pay the
  -- cost.
  CostComponent.TapThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Object.zone obj == Zone.Battlefield && Object.tapped obj == TapState.Untapped
  -- CR 107.6: the exact mirror of TapThis above, and the reason a {Q} ability is
  -- one a player uses on a creature they left tapped.
  CostComponent.UntapThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Object.zone obj == Zone.Battlefield && Object.tapped obj == TapState.Tapped
  -- CR 701.21a: only a permanent, and only one this player controls.
  CostComponent.SacrificeThis ->
    Set.member oid (GameState.battlefield gs) && Projection.controllerOf oid gs == Just pid
  -- CR 119.4: payable only if the life total is at least the amount. Shared with
  -- CR 107.4f's Phyrexian mana symbol, which pays life for a MANA symbol and so
  -- reads the same floor from inside Pawl.Engine.Mana.
  CostComponent.PayLife n -> Mana.canPayLife pid n gs
  -- CR 701.21a: this player must control at least `n` matching permanents.
  -- CR 118.10's "each payment of a cost applies to only one spell, ability, or
  -- effect" is not enforced across two components of ONE cost (#104).
  CostComponent.Sacrifice n criterion ->
    Natural.length (sacrificeCandidates pid criterion gs) >= n
  -- CR 601.2f: payable only if the hand holds at least that many cards.
  --
  -- `oid` is excluded, and that is CR 601.2a, not a convenience: the card moves
  -- to the stack at step (a), so by the time 601.2f determines the total cost the
  -- spell is NOT in its controller's hand and cannot be discarded to pay its own
  -- additional cost. Pawl.Engine.Cast pays one step earlier than that, while the
  -- object is still in hand (#89), so without this filter a hand of "Cathartic
  -- Reunion plus one other card" would read as payable and the Reunion could
  -- discard itself. The exclusion is a no-op the moment #89 lands.
  CostComponent.DiscardCards n ->
    Natural.length (discardCandidates pid oid gs) >= n
  -- CR 702.29a: payable only while the card is in the paying player's hand, which
  -- is where that rule's zone restriction is enforced for the COST half. Asked of
  -- the zone and the owner rather than of control, because CR 108.4 gives a card
  -- in a hand no controller to ask about -- and the owner is the right player,
  -- since CR 400.3 sends every card that would go to a hand to its OWNER's.
  CostComponent.DiscardThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Object.zone obj == Zone.Hand && Object.owner obj == pid
  -- CR 107.14 / CR 118.3: payable only if the player has at least that many
  -- energy counters. GainPlayerCounters (#37) adds them; this spends them.
  CostComponent.PayEnergy n -> case Map.lookup pid (GameState.players gs) of
    Nothing -> False
    Just player -> Map.findWithDefault 0 PlayerCounterKind.Energy (Player.counters player) >= n
  -- CR 606.4: a cost that PUTS loyalty counters on the permanent. Always
  -- payable -- CR 606.6 gates only the removing half -- but the permanent still
  -- has to be one this player controls on the battlefield, the SacrificeThis
  -- floor, since CR 606.4 puts the counters on "that permanent" and a permanent
  -- that has left cannot take them.
  CostComponent.AddLoyaltyToThis _ ->
    Set.member oid (GameState.battlefield gs) && Projection.controllerOf oid gs == Just pid
  -- CR 606.6: a negative loyalty cost can't be activated unless the permanent has
  -- at least that many loyalty counters. Jace Beleren's -10 at 3 loyalty is not
  -- merely unpaid, it is never OFFERED, because
  -- Pawl.Engine.Activate.activatableGiven has canPay as a conjunct and
  -- Pawl.Engine.Engine.priorityLoop rejects an action it did not offer.
  --
  -- "At least that many" is >=, so a -1 at exactly 1 loyalty IS activatable, and
  -- CR 704.5i then buries the planeswalker on the next state-based-action check.
  CostComponent.RemoveLoyaltyFromThis n ->
    Set.member oid (GameState.battlefield gs)
      && Projection.controllerOf oid gs == Just pid
      && loyaltyCountersOn oid gs >= n

-- CR 601.2g then 601.2h: the mana window first, then the payment. Components are
-- paid in PRINTED order; CR 601.2h lets the player pay in any order, which is an
-- elision here (#105) -- no component in this vocabulary changes another's
-- payability.
--
-- All or nothing (CR 601.2h: partial payments are not allowed). The entry state
-- is captured and restored on any rejection, so an Unpaid result is a complete
-- no-op even though paying is monadic and a component may prompt. That no-op is
-- what CR 118.12's resolution-time caller rests on too, where an Unpaid result
-- lands on the same branch as a refusal (Pawl.Engine.Resolve.paid).
pay :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game Payment.Payment
pay pid oid cost = do
  before <- State.get
  case Cost.mana cost of
    -- CR 118.6: attempting to pay an unpayable cost is an illegal action.
    Nothing -> pure Payment.Unpaid
    -- CR 601.2g: Mana.payCost now PROMPTS for which sources to activate, so it is
    -- monadic and restores the pre-payment state itself when it cannot be paid.
    Just manaCost -> do
      paidMana <- Mana.payCost pid manaCost
      if not paidMana
        then pure Payment.Unpaid
        else do
          outcome <- payComponents pid oid (Cost.components cost)
          case outcome of
            Payment.Paid -> pure Payment.Paid
            Payment.Unpaid -> do
              State.put before
              pure Payment.Unpaid

payComponents :: PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> Game Payment.Payment
payComponents pid oid components = case components of
  [] -> pure Payment.Paid
  component : rest -> do
    outcome <- payComponent pid oid component
    case outcome of
      Payment.Unpaid -> pure Payment.Unpaid
      Payment.Paid -> payComponents pid oid rest

payComponent :: PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> Game Payment.Payment
payComponent pid oid component = case component of
  CostComponent.TapThis -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)})
    pure Payment.Paid
  -- CR 107.6: a direct edit like TapThis above, and not through any funnel. That
  -- is an implementation choice and NOT a distinction CR 701.26b draws. Nothing
  -- in the pool watches for an untap, so the two routes are observationally
  -- identical; the first card that triggers on untapping is what would force this
  -- through the funnel.
  CostComponent.UntapThis -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Untapped}) oid (GameState.objects gs)})
    pure Payment.Paid
  -- Through Event.sacrifice, the CR 701.21 funnel, and never a direct zone poke:
  -- a cost payment is a game event, so dies-triggers, replacement effects and the
  -- turn history all see it.
  CostComponent.SacrificeThis -> do
    -- CR 701.21a's "a permanent they don't control" guard lives in the funnel, and
    -- `pid` is the player paying this cost -- who, for "sacrifice this permanent",
    -- is its controller.
    Event.sacrifice pid oid
    pure Payment.Paid
  -- CR 119.4: the payment is subtracted from the life total. Shared with CR
  -- 107.4f's Phyrexian mana symbol, exactly as the payability check above is.
  CostComponent.PayLife n -> do
    State.modify' (Mana.payLife pid n)
    pure Payment.Paid
  -- CR 701.21a: the player chooses which of their permanents dies, so this is a
  -- prompt. Elided only when forced -- exactly as many candidates as the count.
  -- Three payable Mountains and a count of two IS asked: they differ in tap
  -- state, counters and attached auras, so "they are all the same" is not a claim
  -- this engine may make.
  --
  -- Reject-not-repair: an answer that is not a size-`n` subset of the offered
  -- candidates makes the whole payment Unpaid, which pay's restore turns into a
  -- no-op.
  CostComponent.Sacrifice n criterion -> do
    gs <- State.get
    let candidates = sacrificeCandidates pid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length candidates <= n
        then pure (Set.fromList candidates)
        else Trans.lift (Program.prompt (Prompt.ChooseSacrifices decider pid oid candidates n))
    if Set.isSubsetOf chosen (Set.fromList candidates) && Natural.length chosen == n
      then do
        Monad.mapM_ (Event.sacrifice pid) (Set.toAscList chosen)
        pure Payment.Paid
      else pure Payment.Unpaid
  -- CR 701.9b: the discarding player chooses which cards, so this is a prompt.
  -- Elided only when forced -- exactly as many cards in hand as the count (the
  -- same elision the Discard EFFECT makes, #63).
  --
  -- Reject-not-repair, matching Sacrifice above and deliberately NOT matching the
  -- Discard effect, which after #245 completes an undersized answer: a cost may
  -- simply go unpaid, and `pay` restores the entry state so Unpaid is a complete
  -- no-op. An effect has no such out, which is why the two paths differ.
  --
  -- What "reject" means precisely, since the answer's SHAPE differs from
  -- Sacrifice's: it is read as a SET of card ids, and rejected unless that set is
  -- exactly `n` cards drawn from `held`. So [a,a] for n=2 names one card and is
  -- rejected, while [a,a,b] names two and is accepted -- which `List.nub` makes
  -- identical to what the Set-answered Sacrifice arm above already accepts,
  -- rather than the repair it can look like.
  --
  -- CR 701.9a's move is made through Event.discard, the shared discard funnel, so
  -- the card gets a CR 400.7 incarnation, Rest in Peace's redirect composes, and
  -- the discard is recorded for a CR 701.9a trigger to read.
  CostComponent.DiscardCards n -> do
    gs <- State.get
    let held = discardCandidates pid oid gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length held <= n
        then pure held
        else Trans.lift (Program.prompt (Prompt.ChooseDiscard decider pid held n))
    let distinct = List.nub chosen
    if all (\c -> List.elem c held) distinct && Natural.length distinct == n
      then do
        Monad.mapM_ (Event.discard DiscardCause.Ordinary pid) distinct
        pure Payment.Paid
      else pure Payment.Unpaid
  -- CR 701.9a's move, through Event.discard -- the same funnel DiscardCards uses
  -- above, so a cycled card gets a CR 400.7 incarnation and Rest in Peace's
  -- redirect composes. No prompt: the cost names this card.
  --
  -- The card is in the GRAVEYARD (or wherever the funnel redirected it) by the
  -- time the ability resolves, which is what CR 702.29c means by triggering "from
  -- whatever zone the card winds up in after it's cycled", and the same thing
  -- SacrificeThis already does to Ghitu Fire-Eater. Pawl.Engine.Event's scan reads
  -- that zone. CR 702.29c is also why the cycling TRIGGER fires from here: the
  -- cause travels with the discard recorded off the COST rather than off the
  -- ability resolving.
  --
  -- The one thing this site cannot see is CR 702.29c's "of a CYCLING ability": a
  -- cost component knows it was paid, not which ability it belonged to.
  -- Keyword.cycling is the only producer of DiscardThis, so the two name the same
  -- event today. Faerie Macabre prints "Discard this card:" as an ability of its
  -- own and would break that, firing every cycling trigger on the board; the
  -- event has to carry which ability paid it before that card can exist (#319).
  CostComponent.DiscardThis -> do
    Event.discard DiscardCause.ToPayCyclingCost pid oid
    pure Payment.Paid
  -- CR 107.14: paying energy removes that many energy counters from the player.
  -- Natural subtraction is PARTIAL (it throws on underflow), so `left` is guarded
  -- exactly like applyAdjustments's `lowered`. canPayComponent guarantees `have >=
  -- n` at pay time in practice; the guard keeps this function total regardless.
  CostComponent.PayEnergy n -> do
    let spend player =
          let have = Map.findWithDefault 0 PlayerCounterKind.Energy (Player.counters player)
              left = if have >= n then have - n else 0
           in player {Player.counters = Map.insert PlayerCounterKind.Energy left (Player.counters player)}
    State.modify' (\gs -> gs {GameState.players = Map.adjust spend pid (GameState.players gs)})
    pure Payment.Paid
  -- CR 606.4: put the loyalty counters on. A DIRECT edit and deliberately NOT
  -- through Replacement.putCounters, which is the CR 614 funnel: CR 614.16 admits
  -- a counter-scaling replacement (Doubling Season, Hardened Scales) only where a
  -- resolving spell or ability's EFFECT puts the counter on, and a cost is not an
  -- effect. The counters CR 306.5b's enters-with replacement places DO go through
  -- that funnel, by CR 614.16's next clause -- which is what makes Doubling Season
  -- double a planeswalker's starting loyalty and leave its +1 alone.
  CostComponent.AddLoyaltyToThis n -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (addLoyalty n) oid (GameState.objects gs)})
    pure Payment.Paid
  -- CR 606.4's other half. Natural subtraction is PARTIAL, so the floor is
  -- guarded exactly as PayEnergy's is above: canPayComponent's CR 606.6 check
  -- guarantees `have >= n` at pay time, and the guard keeps this total anyway.
  CostComponent.RemoveLoyaltyFromThis n -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (removeLoyalty n) oid (GameState.objects gs)})
    pure Payment.Paid

-- The arithmetic half, pure and board-free.
--
-- 1. Every INCREASE is added to the generic component (CR 601.2f's order, and
--    Thalia's own ruling: increases first, then reductions).
-- 2. A REDUCTION is an amount of mana, read component by component. Its GENERIC
--    part comes off the generic component only (CR 118.7a), floored at zero -- a
--    generic reduction with no generic left to take is simply lost. Its TYPED
--    part cancels matching typed symbols in the cost, one for one (Edgewalker: a
--    {W}{B} reduction takes one white and one black out of a Cleric's cost).
-- 3. An EXCESS typed symbol -- one whose type the cost has already run out of --
--    is DROPPED, not spilled onto the generic component. That is the card text
--    CR 101.1 lets override the rules, not CR 118.7b-d: every reducer that names
--    a type reduces only coloured mana, and Edgewalker's reminder text settles
--    what that means -- a {1}{W} Cleric spell costs {1}, so the stranded {B}
--    leaves the {1} alone. CR 118.7b-d's spill has no producer here (#309).
-- 4. CR 601.2f's floor at {0} needs no special case: ManaCost is a list of
--    symbols and the empty list IS {0}.
--
-- Reductions are POOLED rather than applied one at a time. CR 601.2f lets the
-- player apply multiple reductions in any order, which is a prompt in the rules
-- and an elision here (#88): the generic parts all route to the one generic
-- component, and a typed part only ever removes symbols of its own type -- and
-- one {W} in a cost is indistinguishable from another. Pooling is not merely
-- equivalent to some order, it is equivalent to EVERY order. That every reduction
-- applies at all, rather than one of them, is Edgewalker's own ruling.
--
-- The result is CANONICAL: one leading Generic symbol carrying the whole generic
-- component (omitted entirely when it is zero), then the SURVIVING printed typed
-- symbols in their original order. Presentation, not semantics -- Mana.spend sums
-- every generic symbol and matches typed symbols first -- but it is what makes a
-- total cost comparable.
applyAdjustments :: ([Natural], [ManaCost.ManaCost]) -> ManaCost.ManaCost -> ManaCost.ManaCost
applyAdjustments adjustments cost =
  let (increases, reductions) = adjustments
      ManaCost.MkManaCost symbols = cost
      genericOf symbol = case symbol of
        ManaSymbol.Generic n -> n
        ManaSymbol.OfType _ -> 0
        -- CR 107.4e: a colour/colour hybrid is paid with one mana of a stated
        -- type, so it is no part of the generic component CR 118.7a reductions
        -- come off.
        ManaSymbol.Hybrid _ _ -> 0
        -- A monocolored hybrid's {2} half IS generic mana once CR 601.2b's
        -- nonhybrid equivalent names it, so this arm is decided by the elision
        -- and not by the rule: pawl makes no such announcement (#261), and Flame
        -- Javelin's ruling applies a generic reduction only where the chosen
        -- payment includes generic mana. With no choice recorded there is nothing
        -- for CR 118.7a to come off, so the symbol is left whole.
        ManaSymbol.MonocoloredHybrid _ -> 0
        -- CR 107.4f makes this a COLOURED mana symbol, and its other half is 2
        -- life rather than any amount of mana, so there is no generic component
        -- for CR 118.7a's reduction to come off either way.
        ManaSymbol.Phyrexian _ -> 0
        -- CR 107.4h says outright that generic reductions don't affect {S} costs,
        -- which is the whole reason it is not spelled Generic 1.
        --
        -- Right for the COST and wrong for a REDUCTION, the split the Phyrexian
        -- arm below carries: CR 118.7g makes an {S} in a reduction reduce the cost
        -- by that much generic mana, so this arm owes 1 there and gives 0 (#516).
        ManaSymbol.Snow -> 0
        -- Unreachable: CR 601.2b precedes 601.2f, so Mana.substituteX has
        -- already replaced every Variable before a total cost is computed. The
        -- match must be total, so a bare {X} contributes 0 generic.
        ManaSymbol.Variable -> 0
      -- "Typed" for this function's purpose means "not generic": everything but
      -- Generic survives the filter and keeps its printed position, which is what
      -- "the SURVIVING printed typed symbols in their original order" above
      -- promises and the only way an unreducible symbol reaches Mana.spend
      -- intact. Variable is unreachable for the reason genericOf's arm gives, and
      -- is kept rather than stripped so that it would still survive if it were.
      isTyped symbol = case symbol of
        ManaSymbol.Generic _ -> False
        ManaSymbol.OfType _ -> True
        ManaSymbol.Hybrid _ _ -> True
        ManaSymbol.MonocoloredHybrid _ -> True
        ManaSymbol.Phyrexian _ -> True
        ManaSymbol.Snow -> True
        ManaSymbol.Variable -> True
      -- Which ONE mana type a symbol names, for both sides of the cancellation:
      -- in a reduction it is the type being taken away, and in the cost it is
      -- the type that can be taken. Nothing means the symbol is not a
      -- one-type-one-mana symbol and so plays no part in it.
      manaTypeOf symbol = case symbol of
        ManaSymbol.Generic _ -> Nothing
        ManaSymbol.OfType manaType -> Just manaType
        -- CR 107.4e names TWO types, so neither side of the cancellation can read
        -- one off it. In the COST that is the same elision genericOf's
        -- MonocoloredHybrid arm makes -- pawl announces no choice of half (#261)
        -- -- and Edgewalker's ruling is what the elision costs. In a REDUCTION it
        -- is CR 118.7e, where the choice belongs to the player paying, and which
        -- nothing produces (#309).
        ManaSymbol.Hybrid _ _ -> Nothing
        -- Same two reasons: the {2} half is generic mana and the other half is
        -- one colour, and nothing has announced which is being paid.
        ManaSymbol.MonocoloredHybrid _ -> Nothing
        -- Nothing for two different reasons, one per side. In the COST the symbol
        -- is necessarily UNANNOUNCED, or it would not be a Phyrexian symbol any
        -- more: CR 601.2b's announcement precedes CR 601.2f's total. Neither
        -- caller that reaches this arm has established that there is a green mana
        -- here to cancel, and Edgewalker's ruling read the right way round makes
        -- an uncommitted symbol getting no reduction the rule rather than an
        -- elision. In a REDUCTION, CR 118.7f needs no announcement at all and
        -- this arm is simply wrong for it, which nothing in the pool can show
        -- because no card emits a reduction containing a Phyrexian symbol (#362).
        ManaSymbol.Phyrexian _ -> Nothing
        -- CR 107.4h: {S} is paid with one mana of ANY type, so it names no one
        -- type on either side of the cancellation. In a COST that is exact rather
        -- than an elision: a reduction of one white mana cannot single out an {S}
        -- the way it singles out a {W}. In a REDUCTION it is right too, but for a
        -- different reason -- CR 118.7g makes an {S} there a GENERIC reduction, so
        -- it belongs to genericOf above, whose own Snow arm answers 0 and is wrong
        -- for a reduction (#516). That gap looks INERT rather than merely
        -- unreached: no printed card states a reduction in {S} (Scryfall, all 44
        -- cards carrying {S} in oracle text state a cost).
        ManaSymbol.Snow -> Nothing
        -- Unreachable for the reason genericOf's Variable arm gives; {X} names
        -- no mana type either way.
        ManaSymbol.Variable -> Nothing
      reducingSymbols = concatMap (\(ManaCost.MkManaCost xs) -> xs) reductions
      raised = sum (fmap genericOf symbols) + sum increases
      taken = sum (fmap genericOf reducingSymbols)
      -- Natural subtraction is PARTIAL (it throws on underflow), so the CR
      -- 601.2f floor is also what keeps this total.
      lowered = if raised >= taken then raised - taken else 0
      leading = if lowered == 0 then [] else [ManaSymbol.Generic lowered]
      -- Each reducing symbol cancels ONE matching symbol in the cost. Walks the
      -- printed symbols in their printed order, so the survivors keep it;
      -- `unspent` is the bag of reducing types that have not found a match yet,
      -- and whatever is left in it when the walk ends is the excess that (#309)
      -- drops rather than spilling onto `lowered`.
      cancel unspent remaining = case remaining of
        [] -> []
        symbol : rest -> case manaTypeOf symbol of
          Just manaType | elem manaType unspent -> cancel (List.delete manaType unspent) rest
          _ -> symbol : cancel unspent rest
   in ManaCost.MkManaCost (leading <> cancel (Maybe.mapMaybe manaTypeOf reducingSymbols) (filter isTyped symbols))
