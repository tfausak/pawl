-- CR 118: what a cost IS, and everything the closed half needs to do with one --
-- what the candidates are (costsFor), what the total is (total, CR 601.2f),
-- whether it can be paid (canPay, CR 118.3) and paying it (pay, CR 601.2g/h).
-- Pawl.Mana keeps pools, production and spending; this module keeps the cost.
--
-- The SOLE casing home for Pawl.Type.CostComponent. Pawl.Cast and Pawl.Activate
-- learn nothing about which components exist: they ask "can this be paid" and
-- "pay it", and read one classification (requiresSicknessCheck) for CR 302.6.
module Pawl.Cost where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Keyword as Keyword
import qualified Pawl.Mana as Mana
import qualified Pawl.PlayerEffect as PlayerEffect
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.Card as Card
import Pawl.Type.Cost (Cost)
import qualified Pawl.Type.Cost as Cost
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.Filter as Filter.Type
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Payment as Payment
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
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

-- The candidate costs for CASTING this object (CR 601.2b) -- from hand, the
-- printed one first and then each alternative. Empty for anything that is not a
-- card: a token is created onto the battlefield and never cast, and an ability
-- on the stack is not a spell.
--
-- A LAND yields one candidate whose mana part is Nothing -- CR 202.1's "a card's
-- mana cost", absent -- which CR 118.6 makes unpayable, so canPay says False and
-- Cast.castable never offers it. That is the same answer the retired
-- Cast.costOf's Nothing gave, arrived at by the rule instead of by a special
-- case.
--
-- The candidates depend on the ZONE the object is being cast from, because rule
-- 702.34a's permission and its cost are one sentence: "You may cast this card
-- from your graveyard ... by paying [cost] rather than paying its mana cost."
-- From a graveyard, therefore, the flashback cost is the ONLY candidate -- the
-- printed mana cost is not among them, since nothing permits casting the card
-- from there for it -- and a card with no flashback yields no candidate at all,
-- which is CR 601.3's default prohibition arriving through Cast.castable's
-- affordability gate as well as through its permission gate.
costsFor :: ObjectId -> GameState -> [Cost]
costsFor oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Object.source obj of
    Source.OfCard printing ->
      let card = Printing.card printing
          printed = Cost.MkCost {Cost.mana = Card.manaCost card, Cost.components = Card.additionalCosts card}
          -- CR 118.9d in one line: "If an alternative cost is being paid to cast
          -- a spell, any additional costs, cost increases, and cost reductions
          -- that affect that spell are applied to that alternative cost." An
          -- alternative replaces only the MANA cost; every additional cost still
          -- applies. The increases and reductions are Pawl.Cost.total's job,
          -- called on whichever candidate is chosen. CR 702.34a's own last
          -- sentence sends flashback through the same rules, so its cost is
          -- wrapped identically.
          withAdditional alternative =
            alternative {Cost.components = Cost.components alternative <> Card.additionalCosts card}
       in case Object.zone obj of
            Zone.Graveyard -> fmap withAdditional (Maybe.maybeToList (Keyword.flashbackCost (Card.keywords card)))
            _ -> printed : fmap withAdditional (Card.alternativeCosts card)
    Source.OfToken _ -> []
    Source.OfAbility _ _ -> []
    Source.OfTrigger _ _ -> []
    Source.OfEmblem _ -> []
    Source.OfInherentTrigger _ _ -> []

-- CR 601.2f: "The total cost is the mana cost or alternative cost (as determined
-- in rule 601.2b), plus all additional costs and cost increases, and minus all
-- cost reductions." `cost` arrives with X already substituted, because CR 601.2b
-- precedes 601.2f.
--
-- The mana part alone is adjusted, and the components are carried through
-- untouched: every increase and reduction pawl can express is an amount of MANA,
-- so there is nothing for a CostComponent to absorb. WHICH part of the mana cost
-- each one lands on is applyAdjustments's business -- an increase and a
-- reduction's generic half both go to the generic component, and a reduction
-- that names a mana type goes to the cost's matching symbols. Nor is the result
-- ever "locked in": CR 601.2f's own last sentence
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

-- CR 302.6: does paying this cost put the object's ability behind the
-- summoning-sickness gate? The CLASSIFICATION Pawl.Activate reads, so that this
-- module stays the only one matching a CostComponent constructor.
--
-- BOTH symbols, because CR 302.6 names both: "A creature's activated ability with
-- the tap symbol or the untap symbol in its activation cost can't be activated
-- unless the creature has been under its controller's control continuously since
-- their most recent turn began." CR 107.5 is the tap symbol and CR 107.6 the
-- untap symbol, and CR 702.10c grants haste the same exemption from each.
--
-- Named for the RULE it answers rather than for one of the two symbols: the
-- previous name, requiresTapSymbol, made the untap half read like an oversight
-- at the call site instead of a question the function had never been asked.
requiresSicknessCheck :: Cost -> Bool
requiresSicknessCheck cost =
  any (\c -> elem c (Cost.components cost)) [CostComponent.TapThis, CostComponent.UntapThis]

-- Which permanents a Filter admits, matched through the PROJECTION and never
-- against printed characteristics: a card type is CR 613.1d layer 4 and a
-- subtype is layer 4 too, so Blood Moon changes the answer. A sacrifice cost
-- frames no player, so the perspective is Nothing (its filters never reference
-- one).
--
-- The lower Pawl.Filter is the ONE matcher: Pawl.Replacement narrows its
-- permanents through the same call, so there is no duplicate to keep in step and
-- no Cost->Replacement cycle to avoid (#111).
matchesFilter :: GameState -> Filter.Type.Filter -> ObjectId -> Bool
matchesFilter gs filter_ oid =
  -- No source in scope at this site.
  Filter.matches (Filter.MkContext Nothing Nothing) (Projection.viewOfObject oid gs) filter_

-- The permanents this player may sacrifice for a Filter, ascending -- the order
-- ChooseSacrifices offers them in, which is what makes both the elision test and
-- the transcript fallback deterministic.
sacrificeCandidates :: PlayerId -> Filter.Type.Filter -> GameState -> [ObjectId]
sacrificeCandidates pid filter_ gs =
  List.sort (filter (matchesFilter gs filter_) (Projection.controls pid gs))

-- The cards this player may discard to pay a cost on `oid`: their hand, in its
-- own order, minus `oid` itself. See canPayComponent's DiscardCards arm for why
-- the exclusion is CR 601.2a and not a convenience. Hand order rather than
-- sorted, unlike sacrificeCandidates: Game.zoneMembers already returns a hand in
-- a fixed order, and Prompt.ChooseDiscard offers it in exactly that order both
-- here and in the Discard effect.
discardCandidates :: PlayerId -> ObjectId -> GameState -> [ObjectId]
discardCandidates pid oid gs = filter (/= oid) (Game.zoneMembers Zone.Hand pid gs)

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
  -- CR 107.6: "A permanent that's already untapped can't be untapped again to pay
  -- the cost" -- the exact mirror of TapThis above, and the reason a {Q} ability
  -- is one a player uses on a creature they left tapped.
  CostComponent.UntapThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Object.zone obj == Zone.Battlefield && Object.tapped obj == TapState.Tapped
  -- CR 701.21a: only a permanent, and only one this player controls.
  CostComponent.SacrificeThis ->
    Set.member oid (GameState.battlefield gs) && Projection.controllerOf oid gs == Just pid
  -- CR 119.4: payable only if the life total is at least the amount. CR 119.4b:
  -- "Players can always pay 0 life, no matter what their ... life total is" --
  -- which falls out of >= rather than needing a case.
  CostComponent.PayLife n -> case Map.lookup pid (GameState.players gs) of
    Nothing -> False
    Just player -> Player.life player >= toInteger n
  -- CR 701.21a: this player must control at least `n` matching permanents.
  -- CR 118.10's "each payment of a cost applies to only one spell, ability, or
  -- effect" is not enforced across two components of ONE cost (#104).
  CostComponent.Sacrifice n criterion ->
    Natural.length (sacrificeCandidates pid criterion gs) >= n
  -- CR 601.2f: payable only if the hand holds at least that many cards.
  --
  -- `oid` is excluded, and that is CR 601.2a, not a convenience: "the card …
  -- becomes a spell and is moved to the stack" is step (a) of casting, so by the
  -- time 601.2f determines the total cost the spell is NOT in its controller's
  -- hand and cannot be discarded to pay its own additional cost. Pawl.Cast pays
  -- one step earlier than that, while the object is still in hand (#89), so
  -- without this filter a hand of "Cathartic Reunion plus one other card" would
  -- read as payable and the Reunion could discard itself. The exclusion is a
  -- no-op the moment #89 lands.
  CostComponent.DiscardCards n ->
    Natural.length (discardCandidates pid oid gs) >= n
  -- CR 702.29a: payable only while the card is in the paying player's hand --
  -- which is where rule 702.29a's "functions only while the card with cycling is
  -- in a player's hand" is enforced for the COST half. Asked of the zone and the
  -- owner rather than of control, because CR 108.4 gives a card in a hand no
  -- controller to ask about.
  --
  -- The owner is the right player: CR 400.3 sends every card that would go to a
  -- hand to its OWNER's, so "in this player's hand" and "owned by this player and
  -- in a hand" are the same question asked twice.
  CostComponent.DiscardThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Object.zone obj == Zone.Hand && Object.owner obj == pid
  -- CR 107.14 / CR 118.6: payable only if the player has at least that many
  -- energy counters. The bidirectional proof: GainPlayerCounters (P10 #37)
  -- adds energy, this component spends it.
  CostComponent.PayEnergy n -> case Map.lookup pid (GameState.players gs) of
    Nothing -> False
    Just player -> Map.findWithDefault 0 PlayerCounterKind.Energy (Player.counters player) >= n

-- CR 601.2g then 601.2h: the mana window first, then the payment. Components are
-- paid in PRINTED order; CR 601.2h lets the player pay in any order, which is an
-- elision here (#105) -- no component in this vocabulary changes another's
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

payComponents :: PlayerId -> ObjectId -> [CostComponent.CostComponent] -> Game Payment.Payment
payComponents pid oid components = case components of
  [] -> pure Payment.Paid
  component : rest -> do
    outcome <- payComponent pid oid component
    case outcome of
      Payment.Unpaid -> pure Payment.Unpaid
      Payment.Paid -> payComponents pid oid rest

payComponent :: PlayerId -> ObjectId -> CostComponent.CostComponent -> Game Payment.Payment
payComponent pid oid component = case component of
  CostComponent.TapThis -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)})
    pure Payment.Paid
  -- CR 107.6: "Untap this permanent." A direct edit like TapThis above, and not
  -- through any funnel: untapping to pay a cost is not CR 701.20's untap EVENT
  -- (Effect.Untap), which is what a spell or ability does TO a permanent.
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
  -- CR 119.4: "the payment is subtracted from their life total; in other words,
  -- the player loses that much life." A direct subtraction, and the CR 704.5a
  -- state-based action that may follow is the existing one in Pawl.Sba.
  CostComponent.PayLife n -> do
    State.modify' (\gs -> gs {GameState.players = Map.adjust (\p -> p {Player.life = Player.life p - toInteger n}) pid (GameState.players gs)})
    pure Payment.Paid
  -- CR 701.21a: the player chooses which of their permanents dies, so this is a
  -- prompt. Elided only when forced -- exactly as many candidates as the count.
  -- Three payable Mountains and a count of two is a real choice and IS asked:
  -- Mountains differ in tap state, counters and attached auras, so "they are all
  -- the same" is not a claim this engine may make.
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
  -- Elided only when forced -- exactly as many cards in hand as the count, where
  -- there is nothing to ask (the same elision the Discard EFFECT makes, #63).
  --
  -- Reject-not-repair, matching Sacrifice above and deliberately NOT matching the
  -- Discard effect, which after #245 completes an undersized answer: a cost may
  -- simply go unpaid, and `pay` restores the entry state so Unpaid is a complete
  -- no-op. An effect has no such out, which is why the two paths differ.
  --
  -- What "reject" means precisely, because the answer's SHAPE differs from
  -- Sacrifice's and the difference is easy to misread: the answer is read as a
  -- SET of card ids, and is rejected unless that set is exactly `n` cards drawn
  -- from `held`. So [a,a] for n=2 names one card and is rejected, while [a,a,b]
  -- names two and is accepted.
  --
  -- That is not the repair it can look like. Prompt.ChooseSacrifices is answered
  -- with a Set, so an interpreter meaning [a,a,b] builds Set.fromList [a,a,b] --
  -- which IS {a,b} -- and the Sacrifice arm above accepts it. `List.nub` is what
  -- makes this list-shaped answer behave identically rather than more strictly.
  -- Repair would mean COMPLETING a short answer from cards the interpreter never
  -- named, which is exactly what this does not do.
  --
  -- CR 701.9a's move is made through Event.changeZone, the CR 400.7 funnel, so a
  -- discarded card gets a new incarnation and Rest in Peace's redirect composes --
  -- the same call the Discard effect makes.
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
        Monad.mapM_ (\c -> Event.changeZone c Zone.Graveyard) distinct
        pure Payment.Paid
      else pure Payment.Unpaid
  -- CR 701.9a's move, through Event.changeZone -- the same CR 400.7 funnel
  -- DiscardCards uses above, so a cycled card gets a new incarnation and Rest in
  -- Peace's redirect composes. No prompt: the cost names this card.
  --
  -- The card is in the GRAVEYARD (or wherever the funnel redirected it) by the
  -- time the ability resolves, which is not a problem to route around: it is what
  -- CR 702.29c means by "these abilities trigger from whatever zone the card
  -- winds up in after it's cycled" (#314), and the same thing SacrificeThis
  -- already does to Ghitu Fire-Eater.
  --
  -- CR 702.29c: this is also where a cycling TRIGGER fires from -- "'When you
  -- cycle this card' means 'When you discard this card to pay an activation cost
  -- of a cycling ability'" -- so the event is recorded here, off the cost, and
  -- carries the id the funnel just minted rather than the one that was in hand.
  --
  -- The one thing this site cannot see is rule 702.29c's "of a CYCLING ability":
  -- a cost component knows it was paid, not which ability it belonged to.
  -- Keyword.cycling is the only producer of DiscardThis, so "this component was
  -- paid" and "a cycling ability's cost was paid" name the same event today. The
  -- first card that prints a "Discard this card:" ability of its own would break
  -- that, and the event would then have to carry which ability paid it (#314).
  CostComponent.DiscardThis -> do
    moved <- Event.changeZoneReturning oid Zone.Graveyard
    case moved of
      Nothing -> pure ()
      Just newId -> State.modify' (Event.recordEvent (GameEvent.Cycled newId))
    pure Payment.Paid
  -- CR 107.14: paying energy removes that many energy counters from the
  -- player. Natural subtraction is PARTIAL (it throws on underflow), so `left`
  -- is guarded exactly like Cost.applyAdjustments's `lowered` -- the `have - n`
  -- subtraction is only ever forced in the branch where `have >= n` already
  -- holds. canPayComponent guarantees that at pay time in practice; the guard
  -- keeps this function total regardless.
  CostComponent.PayEnergy n -> do
    let spend player =
          let have = Map.findWithDefault 0 PlayerCounterKind.Energy (Player.counters player)
              left = if have >= n then have - n else 0
           in player {Player.counters = Map.insert PlayerCounterKind.Energy left (Player.counters player)}
    State.modify' (\gs -> gs {GameState.players = Map.adjust spend pid (GameState.players gs)})
    pure Payment.Paid

-- The arithmetic half, pure and board-free.
--
-- 1. Every INCREASE is added to the generic component (CR 601.2f's order, and
--    Thalia's own ruling: "add any cost increases, then apply any cost
--    reductions").
-- 2. A REDUCTION is an amount of mana, read component by component. Its GENERIC
--    part comes off the generic component only (CR 118.7a: "Effects that reduce
--    a cost by an amount of generic mana affect only the generic mana component
--    of that cost. They can't affect the colored or colorless mana
--    components."), floored at zero -- a generic reduction with no generic left
--    to take is simply lost. Its TYPED part cancels matching typed symbols in
--    the cost, one symbol for one symbol (Edgewalker: a {W}{B} reduction takes
--    one white and one black out of a Cleric spell's cost).
-- 3. An EXCESS typed symbol -- one whose type the cost has already run out of --
--    is DROPPED, not spilled onto the generic component. That is the card text
--    CR 101.1 lets override the rules, not CR 118.7b-d: every reducer that names
--    a type prints "This effect reduces only the amount of colored mana you
--    pay", and Edgewalker's own reminder text settles what that means -- "if you
--    cast a Cleric spell with mana cost {1}{W}, it costs {1} to cast", so the
--    stranded {B} leaves the {1} alone. CR 118.7b-d's spill has no producer
--    here (#309).
-- 4. CR 601.2f's "if the mana component of the total cost is reduced to nothing
--    ... it is considered to be {0}. It can't be reduced to less than {0}" needs
--    no special case: ManaCost is a list of symbols and the empty list IS {0}.
--
-- Reductions are POOLED rather than applied one at a time. CR 601.2f's "if
-- multiple cost reductions apply, the player may apply them in any order" is a
-- prompt in the rules and an elision here (#88): the generic parts all route to
-- the one generic component, and a typed part only ever removes symbols of its
-- own type -- and one {W} in a cost is indistinguishable from another, so which
-- reduction took which is not a distinction the result can carry. Pooling is not
-- merely equivalent to some order -- it is equivalent to EVERY order. That every
-- reduction applies at all, rather than one of them, is Edgewalker's own ruling:
-- "if you have more than one of these on the battlefield, the cost reduction is
-- cumulative."
--
-- The result is CANONICAL: one leading Generic symbol carrying the whole generic
-- component (omitted entirely when it is zero), then the SURVIVING printed typed
-- symbols in their original order. Presentation, not semantics -- Mana.spend sums
-- every generic symbol and matches typed symbols first -- but it is what makes a
-- total cost comparable, so "{U} taxed and then discounted is exactly {U}" is a
-- statement a test can make.
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
        -- Javelin's own ruling is that a generic reduction "applies to a
        -- monocolored hybrid spell only if you've chosen a method of paying for
        -- it that includes generic mana". With no choice recorded there is
        -- nothing for CR 118.7a to come off, so the symbol is left whole.
        ManaSymbol.MonocoloredHybrid _ -> 0
        -- Unreachable: CR 601.2b precedes 601.2f, so Mana.substituteX has
        -- already replaced every Variable before a total cost is computed. The
        -- match must be total, so a bare {X} contributes 0 generic.
        ManaSymbol.Variable -> 0
      isTyped symbol = case symbol of
        ManaSymbol.Generic _ -> False
        ManaSymbol.OfType _ -> True
        -- Typed for this function's purpose: it survives the filter and keeps
        -- its printed position, which is what "the SURVIVING printed typed
        -- symbols in their original order" above promises. It survives the
        -- cancellation too, for the reason manaTypeOf's own Hybrid arm gives.
        ManaSymbol.Hybrid _ _ -> True
        -- Typed for the same reason: it survives the filter and keeps its
        -- printed position, which is the only way an unreducible symbol can
        -- reach Mana.spend intact.
        ManaSymbol.MonocoloredHybrid _ -> True
        -- Unreachable for the same reason genericOf's Variable arm is: kept
        -- (retained, not stripped) so that if it ever were reachable, a bare
        -- {X} would still be treated as typed and survive the filter below.
        ManaSymbol.Variable -> True
      -- Which ONE mana type a symbol names, for both sides of the cancellation:
      -- in a reduction it is the type being taken away, and in the cost it is
      -- the type that can be taken. Nothing means the symbol is not a
      -- one-type-one-mana symbol and so plays no part in it.
      manaTypeOf symbol = case symbol of
        ManaSymbol.Generic _ -> Nothing
        ManaSymbol.OfType manaType -> Just manaType
        -- CR 107.4e names TWO types, so neither side of the cancellation can
        -- read one off it. In the COST that is the same elision genericOf's
        -- MonocoloredHybrid arm makes -- pawl announces no choice of half
        -- (#261) -- and Edgewalker's ruling is what the elision costs: "if you
        -- choose to pay such a cost with {W} or {B}, Edgewalker can reduce that
        -- part of the cost." In a REDUCTION it is CR 118.7e, where the choice
        -- belongs to the player paying "at the time the cost reduction is
        -- applied", and which nothing produces (#309).
        ManaSymbol.Hybrid _ _ -> Nothing
        -- Same two reasons: the {2} half is generic mana and the other half is
        -- one colour, and nothing has announced which is being paid.
        ManaSymbol.MonocoloredHybrid _ -> Nothing
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
