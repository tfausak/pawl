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
-- components exist: they ask "can this be paid" and "pay it", and read the
-- classifications this module derives -- requiresSicknessCheck for CR 302.6,
-- isLoyaltyCost for CR 606.2/606.3, and zoneFunctionedFrom for CR 113.6m.
module Pawl.Engine.Cost where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Claim as Claim
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Engine.Summoning as Summoning
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Claim (Claim)
import qualified Pawl.Types.Claim as Claim.Type
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaOption as ManaOption
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
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

-- CR 118.9: the alternative cost "applied to it from another effect" that the
-- rule's own second phrasing names -- "You may cast [this object] without paying
-- its mana cost". Everything about the printed cost survives except the mana
-- part, which becomes {0}.
--
-- Just an EMPTY ManaCost and never Nothing, which is the whole of what makes it
-- payable: Pawl.Types.Cost's Nothing is CR 118.6's unpayable cost, and
-- `Just (MkManaCost [])` is {0} (CR 118.5, CR 118.5a). Ornithopter and Ancestral
-- Vision are the two spellings, and this rule produces the first.
--
-- The additional costs ride along, which is CR 118.9d in as many words: "an
-- alternative cost doesn't change a spell's mana cost, only what its controller
-- has to pay", and "if an alternative cost is being paid to cast a spell, any
-- additional costs ... that affect that spell are applied to that alternative
-- cost". `costsFor`'s `withAdditional` wraps the card's own alternatives the
-- same way, for the same rule.
--
-- The face is the one being CAST (CR 709.3a / 712.11a), so an offer to cast a
-- back face free carries that face's additional costs and not the front's.
withoutPayingManaCost :: Face.Face card -> Cost Keyword.Type.Keyword
withoutPayingManaCost face =
  Cost.MkCost
    { Cost.mana = Just (ManaCost.MkManaCost []),
      Cost.components = Face.additionalCosts face
    }

-- The first offered candidate, or `unpayable` when none was offered. The one
-- total, documented answer every ChooseCost fallback uses.
firstOffered :: [Cost Keyword.Type.Keyword] -> Cost Keyword.Type.Keyword
firstOffered candidates = case candidates of
  c : _ -> c
  [] -> unpayable

-- CR 702.37a: what a morph cast pays -- "by paying {3} rather than paying its
-- mana cost". An alternative cost (CR 118.9) written into rule 702.37a itself
-- rather than onto any card, which is why it is minted here and not read off
-- Keyword.Morph: that constructor carries the cost of CR 702.37e's special
-- action, and the two are different amounts on every printing.
--
-- No additional costs ride along, where `costsFor`'s `withAdditional` adds the
-- card's to every other alternative. CR 702.37c is explicit that the face-down
-- cast is measured against the face-down characteristics -- "any effects or
-- prohibitions that would apply to casting a card with THESE characteristics
-- (and not the face-up card's characteristics)" -- and CR 708.2a leaves those
-- characteristics with no text for an additional cost to be printed in.
faceDownCost :: Cost Keyword.Type.Keyword
faceDownCost =
  Cost.MkCost
    { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]),
      Cost.components = []
    }

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
--
-- And on the object's FACING. A cast Pawl.Engine.Cast has proposed face down
-- (Cast.asProposed stamps it) pays rule 702.37a's {3} and nothing else: the
-- printed mana cost is what the alternative replaces, and the card's own
-- alternatives are text the face-down object does not have (CR 708.2a). Asked
-- ahead of the zone case, because CR 702.37a's morph ability "functions in any
-- zone from which you could play the card it's on" -- the zone question is
-- Cast.castableZones's, and that gate reads the face-down face, which permits
-- only the hand.
costsFor :: CardName.CardName -> ObjectId -> GameState -> [Cost Keyword.Type.Keyword]
costsFor name oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj | Object.facing obj == Facing.FaceDown -> [faceDownCost]
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
          -- CR 604.2: an alternative cost whose "as long as" clause does not hold
          -- is not offered at all -- Asmoranomardicadaistinaculdacar is uncastable
          -- until its controller has discarded a card this turn, and its printed
          -- cost is unpayable (CR 118.6), so the whole card is.
          --
          -- CR 109.5's "you" is the OWNER, as Activate.graveyardAbilitiesOf reads it
          -- for a condition on a card outside the battlefield: pawl has no way to
          -- cast a card from another player's hand, so the owner and the caster are
          -- the same player wherever this is asked (#96).
          --
          -- Projection.fullView for that function's reason too -- nothing here is
          -- inside the layer fold, so there is no circularity to bound against --
          -- and CR 604.7 is satisfied by construction: the card is in a zone, so
          -- there is no last known information to fall back on.
          available alternative = case AlternativeCost.condition alternative of
            Nothing -> True
            Just cond ->
              Condition.holds
                (Projection.fullView gs)
                (Filter.contextFor (Just (Object.owner obj)) (Just oid))
                gs
                oid
                cond
          alternatives = fmap (withAdditional . AlternativeCost.cost) (filter available (Face.alternativeCosts face))
       in case Object.zone obj of
            -- Rule 702.34a's "if the resulting spell is an instant or sorcery
            -- spell" is NOT re-asked here. It gates the PERMISSION
            -- (Pawl.Engine.Keyword.permissionsFor), and Pawl.Engine.Cast.castable
            -- demands that permission alongside an affordable candidate from this
            -- list, so a candidate offered here can never carry a graveyard cast
            -- on its own.
            -- CR 702.127a pays the PRINTED cost, which is the whole difference
            -- between aftermath and flashback: rule 702.34a supplies an
            -- alternative cost and this supplies none, so the half is cast from a
            -- graveyard for exactly what it says. `printed` and not
            -- `withAdditional printed` -- that wrapper exists to bolt the face's
            -- additional costs onto an ALTERNATIVE, and `printed` already carries
            -- them.
            --
            -- CR 702.133a pays the PRINTED cost plus a discard, which is the
            -- third of the three shapes this arm offers: flashback replaces the
            -- mana cost, aftermath replaces nothing, and jump-start ADDS to it
            -- ("by discarding a card as an additional cost to cast it", CR
            -- 601.2b/601.2f-h). So the component is appended to `printed`, which
            -- already carries the face's own additional costs -- and not through
            -- `withAdditional`, which exists to bolt those onto an ALTERNATIVE.
            --
            -- One discard however many jump-start abilities the card has: see
            -- Pawl.Engine.Keyword.hasJumpStart.
            Zone.Graveyard ->
              fmap withAdditional (Maybe.maybeToList (Keyword.flashbackCost (Face.keywords face)))
                <> [printed | Keyword.hasAftermath (Face.keywords face)]
                <> [ printed {Cost.components = Cost.components printed <> [CostComponent.DiscardCards 1]}
                   | Keyword.hasJumpStart (Face.keywords face)
                   ]
            _ -> printed : alternatives
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
total pid oid cost gs = totalWith (allAdjustments pid oid gs) cost

-- CR 601.2f's increases and reductions: the ones CARDS generate
-- (Pawl.Engine.PlayerEffect) plus the one the RULES do, CR 903.8's commander tax.
--
-- The tax joins the increases rather than being added to the printed mana cost,
-- because rule 903.8 words it "plus {2} for each previous time" -- an increase
-- applied during rule 601.2f, so a cost reduction still applies to the total
-- afterwards in the order rule 601.2f fixes. Folding it into the cost would put
-- it before the reductions instead.
--
-- Zero for every spell that is not a commander being cast from the command zone,
-- and Pawl.Engine.Commander.tax short-circuits on that, so an ordinary game pays
-- nothing to ask.
allAdjustments :: PlayerId -> ObjectId -> GameState -> ([Natural], [ManaCost.ManaCost])
allAdjustments pid oid gs =
  let (increases, reductions) = PlayerEffect.costAdjustments pid oid gs
      commanderTax = Commander.tax pid oid gs
   in (if commanderTax == 0 then increases else commanderTax : increases, reductions)

-- Every way CR 118.7e's choice could resolve the reductions that apply, one
-- entry per combination -- `announceReductions` below with the prompt replaced
-- by the list of halves, and reusing `reductionHalvesOf` so that the two cannot
-- offer different halves. The GATE's shape: nobody has been asked yet, so the
-- honest question is whether SOME resolution pays (CR 601.2f).
--
-- The INCREASES ride through untouched, and a symbol with no halves contributes
-- itself exactly once, both for the reasons `announceReductions` gives.
--
-- One entry for adjustments with no hybrid symbol in them, so this is the
-- identity case for every cost that never had a choice to make, and at most
-- 2^(hybrid symbols across the reductions) otherwise.
adjustmentResolutions :: PlayerId -> ObjectId -> GameState -> [([Natural], [ManaCost.ManaCost])]
adjustmentResolutions pid oid gs =
  let (increases, reductions) = allAdjustments pid oid gs
      resolveOne symbol = case reductionHalvesOf symbol of
        Nothing -> [symbol]
        Just [] -> [symbol]
        Just halves -> halves
      resolveAll (ManaCost.MkManaCost symbols) = fmap ManaCost.MkManaCost (traverse resolveOne symbols)
   in fmap ((,) increases) (traverse resolveAll reductions)

-- The same totalling over adjustments the CALLER already has, which is what CR
-- 118.7e's prompt needs: `announceReductions` asks the payer which half of each
-- hybrid symbol in a reduction it takes, and the answers have to reach
-- applyAdjustments rather than being read out of the game state a second time.
-- `total` above is this over the adjustments as they stand unannounced, which no
-- caller in the engine asks for; `totalManas` below is it over every resolution.
totalWith :: ([Natural], [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
totalWith adjustments cost = cost {Cost.mana = fmap (applyAdjustments adjustments) (Cost.mana cost)}

-- CR 601.2f's totalling of the MANA part alone, curried so that it is a function
-- of one mana cost. `total` above is this fmapped over a whole Cost's mana part;
-- what wants it separately is `announce` and `canPaySomeCompletion`, which both
-- have to ask "what will this cost once 601.2f has run?" of candidate costs that
-- do not exist yet and never become a Cost of their own.
--
-- MANY answers rather than one, because CR 118.7e leaves a choice inside the
-- reduction and this runs before anyone has made it: one total per resolution,
-- and a caller asks `any` of them. A reduction with no hybrid symbol in it --
-- every printed one -- gives exactly one, so this is the old single answer
-- wherever the choice does not arise.
--
-- The resolutions are computed ONCE and shared across the candidate costs a
-- caller measures, which is what the partial application buys.
totalManas :: PlayerId -> ObjectId -> GameState -> ManaCost.ManaCost -> [ManaCost.ManaCost]
totalManas pid oid gs =
  let resolutions = adjustmentResolutions pid oid gs
   in \manaCost -> fmap (`applyAdjustments` manaCost) resolutions

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
-- Every mana symbol payable in multiple ways is announced here: CR 107.4f's
-- Phyrexian symbol, and both of CR 107.4e's hybrids -- the monocolored {2/R} and
-- the colour/colour {W/U}.
--
-- The life the announcement committed becomes a CostComponent.PayLife, which is
-- the rule's own words rather than a re-encoding: CR 107.4f pays 2 life for the
-- symbol and CR 119.4 governs paying life wherever it comes from. That makes the
-- returned cost CR 601.2b's "nonhybrid equivalent cost" in full. Omitted entirely
-- at zero, so a cost with no Phyrexian symbol comes back untouched. APPENDED
-- rather than merged into a PayLife the cost already carries, which costs nothing
-- now that both are measured against one life total: `lifeOwedBy` sums them, and
-- CR 118.3 is asked of the sum.
--
-- That sum is also what goes IN, as the life this cost owes OUTSIDE its mana
-- part. Without it CR 601.2b's offer is measured against a cost half as
-- expensive as the one that will be paid, and a route the player cannot afford
-- gets offered -- the same failure the `total` parameter below exists to prevent,
-- one resource over. It is the components as they arrive that are summed, since
-- the PayLife appended above is precisely the announcement being measured.
--
-- `total` is CR 601.2f's totalling, the CALLER's to supply because the two
-- callers genuinely differ. Pawl.Engine.Cast passes `totalManas`, so a spell's
-- announcement is measured against the same adjusted cost `Cast.payableCost`
-- gated on -- against the printed cost instead, a reduction could hide a route
-- and this function elide the prompt. Pawl.Engine.Activate passes `pure`, because
-- an activation cost is deliberately not routed through `total` at all (#90).
-- When #90 lands both sites change together, which is the point of the parameter.
--
-- It answers a LIST because CR 118.7e's choice of half is not made until CR
-- 601.2f, one step after this: a route is offered where SOME resolution of the
-- reductions pays it, which is exactly what the gate asks.
--
-- The COST that arrives already carries CR 601.2b's announced value of X, since
-- that rule puts the value of the variable before this announcement. Named
-- `total_` only because this module's own `total` is in scope.
announce :: PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> Game (Cost Keyword.Type.Keyword)
announce pid oid total_ cost = case Cost.mana cost of
  -- CR 118.6: an object with no mana cost has no mana symbols to announce.
  Nothing -> pure cost
  Just manaCost -> do
    -- The components' claims are read here rather than inside Mana.announce,
    -- which cannot reach removalClaim: this module imports that one, not the
    -- other way about. Nothing announcing changes the board, so reading them once
    -- is the same answer every offer would have got.
    gs <- State.get
    (announced, life) <- Mana.announce manaActivations pid oid total_ (lifeOwedBy (Cost.components cost)) (claimsOf pid oid (Cost.components cost) gs) manaCost
    pure
      cost
        { Cost.mana = Just announced,
          Cost.components =
            Cost.components cost <> (if life > 0 then [CostComponent.PayLife life] else [])
        }

-- CR 118.7e: "If a cost is reduced by an amount of mana represented by a hybrid
-- mana symbol, the player paying that cost chooses one half of that symbol at
-- the time the cost reduction is applied (see rule 601.2f)." This is that
-- choice, made for every hybrid symbol in every reduction that applies to `oid`,
-- and the answers come back as the adjustments `totalWith` then applies.
--
-- A SECOND SEAM rather than part of `announce` above, because the two are two
-- rules at two moments. CR 601.2b's announcement is about the symbols of the
-- cost being paid and happens before the total exists; this is about the symbols
-- of a REDUCTION and happens as CR 601.2f applies it, which the rule says
-- outright. Pawl.Engine.Cast calls them in that order.
--
-- The answer is the nonhybrid symbol the chosen half resolves to, so what
-- reaches applyAdjustments is a reduction with no hybrid symbol left in it --
-- the same posture Mana.announce takes toward a cost, leaving an OfType behind
-- where a {2/R} stood. That is why applyAdjustments' own Hybrid arms still take
-- nothing: neither path that measures a cost leaves a hybrid symbol in a
-- reduction for them to read -- this one answers it, and the gate enumerates it
-- (`adjustmentResolutions`).
--
-- NOT FILTERED BY PAYABILITY, unlike `announce`: CR 118.7e attaches no condition
-- to the choice, and a player may take the half that reduces nothing. What that
-- costs is that an answer here can strand a payment the gate allowed on the
-- strength of the OTHER half -- reject-not-repair, the posture Cast.castSpell
-- takes toward an announced X the player cannot afford, and CR 601.2h's failed
-- payment is what reverses it.
--
-- The INCREASES ride through untouched. CR 118.7e is a rule about reductions,
-- and pawl's increases are amounts of generic mana with no symbol to choose
-- halves of.
announceReductions :: PlayerId -> ObjectId -> GameState -> Game ([Natural], [ManaCost.ManaCost])
announceReductions pid oid gs =
  let (increases, reductions) = PlayerEffect.costAdjustments pid oid gs
      chooseOne symbol = case reductionHalvesOf symbol of
        -- Not a hybrid symbol, so CR 118.7e has nothing to ask about it.
        Nothing -> pure symbol
        -- Unreachable: reductionHalvesOf answers Just only where it has halves
        -- to offer. Left rather than made partial, and the symbol survives.
        Just [] -> pure symbol
        -- One half, which is the degenerate `Hybrid t t` no card prints. Both
        -- halves are the same symbol, so the answer cannot be observed and
        -- asking would be a prompt with one button.
        Just [only] -> pure only
        Just halves@(first : others) -> do
          answer <-
            Game.choose
              (Prompt.ChooseReductionHalf (Decide.deciderFor pid gs) pid oid symbol (first NonEmpty.:| others))
          -- FILTERED, NOT TRUSTED, the Mana.announce posture: an answer that is
          -- not one of the offered halves falls back to the first.
          pure (if elem answer halves then answer else first)
      chooseAll (ManaCost.MkManaCost symbols) = fmap ManaCost.MkManaCost (traverse chooseOne symbols)
   in fmap ((,) increases) (traverse chooseAll reductions)

-- CR 118.7e's "one half of that symbol", written as the reduction each half
-- would be: "if a colored or colorless half is chosen, the cost is reduced by
-- one mana of that type" is an OfType, and "if a generic half is chosen, the
-- cost is reduced by an amount of generic mana equal to that half's number" is a
-- Generic. Nothing for a symbol with no halves to choose between, which is every
-- symbol CR 107.4e does not call hybrid.
--
-- DEDUPLICATED, so `Hybrid t t` -- degenerate rather than illegal, per
-- Pawl.Types.ManaSymbol -- offers one half instead of the same one twice.
--
-- CR 107.4f's Phyrexian symbol is NOT here, and needs no issue for it: CR 118.7f
-- gives such a reduction one mana of the symbol's colour with no choice at all,
-- which reducingManaTypeOf reads directly. The ten HYBRID Phyrexian symbols that
-- rule also names would have a half to choose, and Pawl.Types.ManaSymbol cannot
-- say one: its Phyrexian carries a single Color.
reductionHalvesOf :: ManaSymbol.ManaSymbol -> Maybe [ManaSymbol.ManaSymbol]
reductionHalvesOf symbol = case symbol of
  ManaSymbol.Generic _ -> Nothing
  ManaSymbol.OfType _ -> Nothing
  ManaSymbol.Hybrid a b -> Just (List.nub [ManaSymbol.OfType a, ManaSymbol.OfType b])
  ManaSymbol.MonocoloredHybrid manaType ->
    Just [ManaSymbol.OfType manaType, ManaSymbol.Generic Mana.monocoloredHybridGeneric]
  ManaSymbol.Phyrexian _ -> Nothing
  ManaSymbol.Snow -> Nothing
  -- Unreachable for the reason applyAdjustments' Variable arms give: CR 601.2b
  -- precedes CR 601.2f, so no {X} survives into a total cost. It names no halves
  -- either way.
  ManaSymbol.Variable -> Nothing

-- CR 302.6: does paying this cost put the object's ability behind the
-- summoning-sickness gate? The CLASSIFICATION Pawl.Engine.Activate reads, so that
-- this module stays the only one matching a CostComponent constructor.
--
-- BOTH symbols, because CR 302.6 names both: CR 107.5's tap symbol and CR 107.6's
-- untap symbol, from each of which CR 702.10c's haste grants the same exemption.
-- Named for the RULE it answers rather than for one of the two symbols.
--
-- CR 702.122a's TapForTotalPower is deliberately NOT here, and the omission is
-- the rule rather than an oversight: that component taps OTHER permanents, and
-- rule 302.6 gates a creature's own activated ability with the tap symbol in it.
-- A Vehicle that arrived this turn may be crewed; rule 302.6's second sentence
-- still stops it attacking.
requiresSicknessCheck :: Cost Keyword.Type.Keyword -> Bool
requiresSicknessCheck cost =
  any (\c -> elem c (Cost.components cost)) [CostComponent.TapThis, CostComponent.UntapThis]

-- CR 302.6 asked of one activation COST -- the whole of what the rule reads, so
-- an ability charging anything else is not gated at all. The ONE reading, asked
-- on both paths an activated ability takes: Pawl.Engine.Activate for an ability
-- that uses the stack, and manaActivations below for one that does not
-- (CR 605.3b). Two readings could disagree, and the mana one used to, by gating
-- every source of a controller's whether its cost held {T} or not (#1116).
--
-- Reads PROJECTED creature-ness, so a plain land is never sick-gated and an
-- animated one is. Keyed to `pid`, the player trying to activate: CR 302.6 asks
-- about THEIR control since THEIR most recent turn began, so a settle recorded
-- for anyone else does not answer it (#198). CR 702.10c's haste exemption comes
-- with it, from Summoning.settledOrHastyGiven.
sicknessOkGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Bool
sicknessOkGiven pcs pid oid cost gs =
  not (requiresSicknessCheck cost)
    || not (Set.member CardType.Creature (Projection.cardTypesGiven pcs oid gs))
    || Summoning.settledOrHastyGiven pcs pid oid gs

-- CR 606.2: an activated ability with a loyalty symbol in its cost is a loyalty
-- ability. The CLASSIFICATION Pawl.Engine.Activate reads for CR 606.3's window
-- and once-per-turn limit, the requiresSicknessCheck shape.
--
-- Derived from the cost rather than stored on the ability, because CR 606.2 is a
-- rule about what a cost CONTAINS and not a rider a card prints. That is also
-- why Jace Beleren's abilities carry no ActivationRestriction.SorcerySpeed: the
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
  CostComponent.TapForTotalPower _ _ -> False
  CostComponent.DiscardCards _ -> False
  CostComponent.DiscardThis -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileCardsFromGraveyard _ _ -> False
  CostComponent.ExileTopFromGraveyard _ -> False

-- CR 113.6m's COST half: "an ability whose cost or effect specifies that it
-- moves the object it's on out of a particular zone functions only in that
-- zone". The CLASSIFICATION Pawl.Engine.Activate reads to decide WHERE an
-- ability may be activated from, the requiresSicknessCheck shape -- so that
-- module still learns nothing about which components exist. The "or effect" half
-- is Pawl.Engine.EffectZone, and Pawl.Engine.Activate.zoneFunctionedFrom is
-- where the two meet.
--
-- Nothing means the cost names no zone, which leaves the effect half to answer
-- and CR 113.6's own default in place if it does not: the ability functions on
-- the battlefield. That is the answer for SacrificeThis too, and deliberately --
-- CR 701.21a moves the object off the battlefield, which is where CR 113.6
-- already had it, so naming it here would change no reader's answer while
-- claiming a rule this cost does not need.
--
-- One zone and never a set: every component that names a zone names exactly one,
-- and CR 113.6m is about "a particular zone". Two components naming DIFFERENT
-- zones would make the ability unpayable in either, so the FIRST one found is the
-- answer and a disagreement is a card-data error rather than a rules question.
zoneFunctionedFrom :: Cost Keyword.Type.Keyword -> Maybe Zone.Zone
zoneFunctionedFrom cost = Maybe.listToMaybe (Maybe.mapMaybe zoneOfComponent (Cost.components cost))

zoneOfComponent :: CostComponent.CostComponent Keyword.Type.Keyword -> Maybe Zone.Zone
zoneOfComponent component = case component of
  -- CR 702.29a's "Discard this card": the hand, which is where cycling functions.
  -- Not read by anything today -- Pawl.Engine.Keyword.handAbilitiesOf mints the
  -- cycling ability into the hand from rule 702.29a directly, which is the same
  -- answer by a shorter route -- and written because CR 113.6m says it.
  CostComponent.DiscardThis -> Just Zone.Hand
  CostComponent.ExileThisFromGraveyard -> Just Zone.Graveyard
  CostComponent.TapThis -> Nothing
  CostComponent.UntapThis -> Nothing
  CostComponent.SacrificeThis -> Nothing
  CostComponent.PayLife _ -> Nothing
  CostComponent.Sacrifice _ _ -> Nothing
  -- CR 702.122a taps permanents on the battlefield and moves nothing out of any
  -- zone, so CR 113.6m says nothing and CR 113.6's default stands.
  CostComponent.TapForTotalPower _ _ -> Nothing
  -- Nothing, and NOT Just Zone.Graveyard -- the one place this component parts
  -- from ExileThisFromGraveyard above. CR 113.6m is about an ability that "moves
  -- THE OBJECT IT'S ON out of a particular zone"; this one moves OTHER cards,
  -- chosen from the payer's graveyard, and leaves the object carrying the cost
  -- exactly where it was. So CR 113.6m does not reach it and CR 113.6's default
  -- stands -- the same reading TapForTotalPower gets just above. Answering the
  -- graveyard here would make an activated ability with this cost unactivatable
  -- from the battlefield, which no rule asks for.
  CostComponent.ExileCardsFromGraveyard _ _ -> Nothing
  -- ExileCardsFromGraveyard's answer for its reason: this too moves cards other
  -- than the object the cost is on, so CR 113.6m does not reach it.
  CostComponent.ExileTopFromGraveyard _ -> Nothing
  CostComponent.DiscardCards _ -> Nothing
  CostComponent.PayEnergy _ -> Nothing
  CostComponent.AddLoyaltyToThis _ -> Nothing
  CostComponent.RemoveLoyaltyFromThis _ -> Nothing
  -- CR 122.6 puts counters on a permanent already where it is, so nothing moves
  -- out of any zone and CR 113.6m does not reach this either.
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing

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

-- The cards this player may discard to pay a cost on `oid`: their hand, in its
-- own order, minus `oid` itself. See canPayComponent's DiscardCards arm for why
-- the exclusion is CR 601.2a and not a convenience. Hand order rather than
-- sorted, unlike Replacement.sacrificeCandidates: Game.zoneMembers already
-- returns a hand in a fixed order, which Prompt.ChooseDiscard offers it in.
discardCandidates :: PlayerId -> ObjectId -> GameState -> [ObjectId]
discardCandidates pid oid gs = filter (/= oid) (Game.zoneMembers Zone.Hand pid gs)

-- The cards this player may exile to pay an ExileCardsFromGraveyard component:
-- their OWN graveyard, in its own order, narrowed by the criterion.
--
-- Per-owner, and that is CR 400.3 with CR 108.4: a graveyard is not a shared
-- zone, and a card in one has no controller for a control-shaped gate to read,
-- so Game.zoneMembers Zone.Graveyard pid is the whole of "your graveyard".
--
-- Matched against the PRINTED card and never a projection: nothing off the
-- battlefield is projected (#160), so Projection.viewOfCardIn is the view --
-- printed on every axis but CR 208.2a's characteristic-defining power, which
-- functions in a graveyard too -- and a candidate whose card cannot be found
-- matches nothing. The context carries the
-- payer as its perspective and no source -- the criterion narrows a card by its
-- own qualities, and CR 601.2a has already moved the spell being cast to the
-- stack, so IsSource would have nothing in this pool to compare against anyway.
--
-- No `oid` exclusion, unlike discardCandidates above, and none is owed: that
-- filter is CR 601.2a's consequence for a component that reads a HAND -- the
-- zone the spell being cast has just left, so without it a card could be
-- discarded to pay its own additional cost. CR 601.2a has the same effect here
-- for free: a spell being cast is on the STACK, so it is not in this pool to
-- exclude, whichever zone it was cast from.
--
-- Graveyard order rather than sorted, discardCandidates' choice and for its
-- reason: Game.zoneMembers already returns a fixed order.
exileCandidates :: PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
exileCandidates pid criterion gs =
  let context = Filter.contextFor (Just pid) Nothing
      matches candidate = case Game.faceOf candidate gs of
        Nothing -> False
        Just face -> Filter.matches context (Projection.viewOfCardIn gs candidate face) criterion
   in filter matches (Game.zoneMembers Zone.Graveyard pid gs)

-- The one card an ExileTopFromGraveyard component takes: the TOP matching card
-- of this player's graveyard, or Nothing where it holds none.
--
-- The LAST of exileCandidates' answer is the top. CR 404.1 puts an arrival "on
-- top of its owner's graveyard" and Pawl.Engine.Game.insertIntoZone appends it,
-- so the most recent card is last -- the opposite end from a library, whose head
-- Event.drawCard takes as CR 121.1's top card. Reading the head here would exile
-- the OLDEST matching card, which no rule asks for.
--
-- No prompt, and that is CR 404.2 rather than an elision: a player "normally
-- can't change" a graveyard's order, so "the top creature card" names exactly
-- one card and there is nothing to choose.
topExileCandidate :: PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> Maybe ObjectId
topExileCandidate pid criterion gs =
  Maybe.listToMaybe (reverse (exileCandidates pid criterion gs))

-- The permanents this player may tap to pay a TapForTotalPower component on
-- `oid`: every battlefield object matching the criterion, ascending, the order
-- Replacement.sacrificeCandidates offers its own in.
--
-- NOT Replacement.sacrificeCandidates, and the difference is the CONTEXT. That
-- one matches with NO PERSPECTIVE, which makes ControlledBy vacuous, and
-- pre-narrows to `Projection.controls pid` structurally instead. CR 702.122a's
-- criterion needs the atom that context throws away -- "you control" is
-- `ControlledBy You`, beside the `Not IsSource` both spell the same way -- so
-- here the perspective is the PAYER and the source is the permanent whose
-- ability is being paid for. Getting that wrong is not a subtlety here: without
-- the source, a Vehicle that has already become a creature could crew itself.
--
-- The whole battlefield rather than `Projection.controls pid`: the criterion
-- says who must control the candidate, so narrowing first would decide it twice
-- and silently make a component whose Filter says otherwise mean something else.
tapCandidates :: PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
tapCandidates pid oid criterion gs =
  let context = Filter.contextFor (Just pid) (Just oid)
      matches candidate =
        Filter.matches context (Projection.viewOfObject candidate gs) criterion
   in List.sort (filter matches (Set.toList (GameState.battlefield gs)))

-- The power a candidate contributes to CR 702.122a's total. Zero for a permanent
-- with no power at all, which after CR 208.3 is every noncreature one -- so an
-- uncrewed Vehicle standing beside the one being crewed adds nothing even where
-- a criterion admits it.
tapPower :: ObjectId -> GameState -> Integer
tapPower candidate gs = Maybe.fromMaybe 0 (Projection.powerOf candidate gs)

-- CR 701.26a: tap one permanent. A direct edit and not a funnel -- see
-- payComponent's TapThis arm for why -- shared by the two components that tap,
-- so the day an Event.tap exists there is one call site to move.
tapObject :: ObjectId -> Game ()
tapObject target =
  State.modify'
    (\gs -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) target (GameState.objects gs)})

-- What this component takes OUT of a zone: which zone it draws from, which
-- objects are in that pool for it, and how many of them it claims. Nothing for a
-- component that removes nothing, which is what leaves `jointlyPayable` below
-- asking only about the components that can actually contend for one object.
--
-- TAPPING IS NOT A REMOVAL, and that is a rules fact rather than a scope cut. CR
-- 601.2h pays a cost's parts "in any order", so a payer facing a cost that both
-- taps and sacrifices taps first and sacrifices second; both payments are
-- performed, and CR 118.11 confirms a cost is paid by the actions it calls for.
-- A tapped permanent is still on the battlefield, so tapping takes nothing out
-- of anybody's pool -- counting it as a claim would REFUSE costs the rules
-- allow. TapForTotalPower is out for that reason and one more: its Natural is a
-- THRESHOLD on an aggregate rather than a count of objects, so it has no claim
-- of this shape to state at all.
--
-- The ZONE alone is a sound key even though a hand and a graveyard are
-- per-player (CR 400.3, CR 108.4): every claim below is on `pid`'s own copy --
-- discardCandidates and exileCandidates read `pid`'s zone, and each `*This` arm
-- demands that `pid` control or own the object -- so two claims on one zone are
-- always two claims on one pool.
--
-- A `*This` arm whose own guard fails answers an EMPTY pool rather than Nothing,
-- which keeps the two readings in agreement: canPayComponent refuses such a
-- component and so does Hall's condition below, instead of the joint check
-- quietly dropping the claim.
--
-- EXHAUSTIVE with no wildcard, this module's posture for every CostComponent
-- match: a new constructor that removes objects from a zone has to answer here,
-- and -Werror is what makes it.
removalClaim :: PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> GameState -> Maybe Claim
removalClaim pid oid component gs = case component of
  -- CR 701.21a: the permanents this player controls that match the criterion.
  CostComponent.Sacrifice n criterion ->
    claim Zone.Battlefield (Set.fromList (Replacement.sacrificeCandidates pid (Just oid) criterion gs)) n
  CostComponent.SacrificeThis ->
    claim
      Zone.Battlefield
      -- CR 101.2's prohibition, exactly as canPayComponent reads it below --
      -- the two answers have to agree, since this arm's empty pool is how the
      -- joint check spells the same refusal.
      ( itself
          ( Set.member oid (GameState.battlefield gs)
              && Projection.controllerOf oid gs == Just pid
              && not (SacrificeRestriction.prohibited oid gs)
          )
      )
      1
  CostComponent.DiscardCards n ->
    claim Zone.Hand (Set.fromList (discardCandidates pid oid gs)) n
  CostComponent.DiscardThis -> claim Zone.Hand (itself (isOwnedIn Zone.Hand)) 1
  CostComponent.ExileCardsFromGraveyard n criterion ->
    claim Zone.Graveyard (Set.fromList (exileCandidates pid criterion gs)) n
  -- A pool of at most ONE, and the claim is on that one card rather than on a
  -- choice among several: CR 404.2's order picks it. An empty pool is how this
  -- arm spells the refusal canPayComponent gives below.
  CostComponent.ExileTopFromGraveyard criterion ->
    claim Zone.Graveyard (Set.fromList (Maybe.maybeToList (topExileCandidate pid criterion gs))) 1
  CostComponent.ExileThisFromGraveyard -> claim Zone.Graveyard (itself (isOwnedIn Zone.Graveyard)) 1
  CostComponent.TapThis -> Nothing
  CostComponent.UntapThis -> Nothing
  CostComponent.TapForTotalPower _ _ -> Nothing
  CostComponent.PayLife _ -> Nothing
  CostComponent.PayEnergy _ -> Nothing
  CostComponent.AddLoyaltyToThis _ -> Nothing
  CostComponent.RemoveLoyaltyFromThis _ -> Nothing
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing
  where
    claim z p n = Just (Claim.Type.MkClaim {Claim.Type.zone = z, Claim.Type.pool = p, Claim.Type.count = n})
    itself condition = if condition then Set.singleton oid else Set.empty
    -- canPayComponent's own guard for the two `*This` arms that read a zone
    -- rather than control, and asked here for its reason: CR 108.4 gives a card
    -- outside the battlefield no controller, and CR 400.3 puts it in its OWNER's
    -- zone.
    isOwnedIn zone = case Game.lookupObject oid gs of
      Nothing -> False
      Just obj -> Object.zone obj == zone && Object.owner obj == pid

-- CR 118.3's "fully", asked of a cost's components TOGETHER rather than one at a
-- time. CR 601.2h pays them "in any order", so the question is whether there is
-- SOME assignment of distinct objects to the components under which every one of
-- them is paid in full -- not whether each, asked alone against the untouched
-- board, could find enough.
--
-- Jarad, Golgari Lich Lord's "Sacrifice a Swamp and a Forest" is the printed
-- case, and one Bayou (Land -- Forest Swamp) is the board that tells the two
-- readings apart: each component alone finds a candidate, and there is only one
-- land to give.
--
-- Pawl.Engine.Claim.satisfiable is the reading, and it carries the per-zone
-- grouping and Hall's condition; what is this module's is which components make
-- a claim at all (removalClaim). The same reading is asked across the SOURCES of
-- one mana payment (Pawl.Engine.Mana.payableResolutionsGiven), which is why it
-- lives there rather than here.
jointlyPayable :: PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> GameState -> Bool
jointlyPayable pid oid components gs = Claim.satisfiable (claimsOf pid oid components gs)

-- Everything these components will take out of a zone. What `jointlyPayable`
-- asks Hall's condition of, and what the MANA side is handed so it can ask the
-- same question of these claims and its sources' together (#1134).
claimsOf :: PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> GameState -> [Claim]
claimsOf pid oid components gs = Maybe.mapMaybe (\component -> removalClaim pid oid component gs) components

-- CR 118.3: a player can't pay a cost without the resources to pay it fully. The
-- mana part AND every component, measured against the CURRENT state -- before any
-- part of the cost is paid. That is CR-correct rather than convenient: CR 601.2g
-- gives the mana window BEFORE CR 601.2h's payment, so a Mountain tapped for mana
-- is still on the battlefield to be sacrificed afterwards.
--
-- CR 118.6: an unpayable cost is never payable.
--
-- LIFE is the one resource measured across the two halves rather than within
-- each, and CR 107.4f's Phyrexian symbol is why it has to be: it is the only MANA
-- symbol that spends life, so it is the only way one cost can demand life twice.
-- Measured separately, a cost of {G/P} plus "pay 2 life" reads as payable at 3
-- life, because 3 covers each 2 -- and CR 118.3's "fully" is about the whole cost,
-- not about its parts. So the components' life is handed to the mana side as
-- already committed, which is exactly what CR 119.4's floor is then asked of. It
-- subsumes the per-component check the loop below still makes, and two PayLife
-- components of one cost are added the same way.
--
-- OBJECTS are the other resource measured across components rather than within
-- each, and `jointlyPayable` is where: two components that each remove an object
-- from a zone cannot both claim the one Bayou, which is again CR 118.3's "fully"
-- read over the whole cost. CR 118.10 is NOT that rule and never was -- it
-- governs two DIFFERENT spells or abilities each paying its own cost, and says
-- nothing about two parts of one. CR 601.2h's "partial payments are not allowed"
-- is the other half of the reading.
--
-- The objects are handed ACROSS the two halves for the same reason the life is,
-- and CR 601.2g is why they have to be: the mana window comes before CR 601.2h's
-- payment, so a Phyrexian Tower tapped for {B} has already eaten the creature
-- Village Rites' additional cost then wants. `jointlyPayable` above reads the
-- components alone; the mana side reads them beside its sources' own claims and
-- is the stricter of the two.
--
-- What is left counted twice over is a component that spends MANA, and no such
-- component exists: the mana part spends nothing but mana and life, and both of
-- those are already totalled across the two halves above.
canPay :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPay pid oid cost gs = case Cost.mana cost of
  Nothing -> False
  Just manaCost ->
    Mana.canPayCommitting manaActivations pid (lifeOwedBy (Cost.components cost)) (claimsOf pid oid (Cost.components cost) gs) manaCost gs
      && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)
      && jointlyPayable pid oid (Cost.components cost) gs

-- How many times may this player activate this mana ability, right now? The
-- capacity (Mana.Capacity) threaded through Pawl.Engine.Mana's supply model and
-- the CR 601.2g window, and the reason a source answered 0 is neither offered nor
-- counted. CR 605.3b keeps a mana ability off the stack, so nothing here comes
-- from Activate.activatable and every restriction that window applies has to be
-- applied here instead.
--
-- Two of them, both read off the ability's OWN activation cost, which CR 602.2b
-- makes the activation pay: CR 118.3, can the cost be paid; and CR 302.6, is the
-- creature settled enough to pay it. NEITHER is a fact about the permanent alone
-- -- CR 107.5 bars a tapped permanent from paying {T} and says nothing about a
-- cost without one, and CR 302.6 gates only a cost holding {T} or {Q} -- which is
-- why both are asked per ROUTE here rather than of the source in
-- Mana.manaSourcesGiven (#1116).
--
-- `canPay` above without its mana half, which is not an omission: the MANA part
-- is asked about only for CR 118.6, since the supply walk is what asks this and
-- asking it back would not terminate. Exact for the pool, where every mana
-- ability's mana part is empty -- Cabal Coffers is the card that would make the
-- difference visible, and `payActivation` defers to the same issue (#1120).
--
-- And WHAT ONE ACTIVATION TAKES, alongside the count, because the count alone is
-- a fact about this source in isolation: two sources whose costs both sacrifice a
-- creature each answer 1 beside one creature, and only the claims say they cannot
-- both have it (#1126). The claims are one activation's, unscaled -- the reader
-- multiplies by however many it takes.
--
-- `pcs` is the pre-projected board CR 302.6's two reads want; Map.empty asks for
-- a fresh projection, which is what a caller with no sweep in hand passes (#200).
manaActivations :: Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> (Natural, [Claim])
manaActivations pcs pid oid cost gs =
  if Maybe.isJust (Cost.mana cost)
    && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)
    && jointlyPayable pid oid (Cost.components cost) gs
    && sicknessOkGiven pcs pid oid cost gs
    then (repeatsOf pid oid cost gs, Maybe.mapMaybe (\c -> removalClaim pid oid c gs) (Cost.components cost))
    else (0, [])

-- How many times IN A ROW a cost already known to be payable once could be paid,
-- which is what makes Ashnod's Altar beside two creatures two mana activations
-- rather than one (#1128).
--
-- Counted off `removalClaim` alone, and answered 1 for anything else. What CR
-- 118.3's "fully" limits a repetition by is the resources it consumes, and the
-- claims are the ones this module knows how to count: a cost every component of
-- which takes objects out of a zone can be paid exactly as many times as those
-- zones hold objects for it. A component that spends something else -- CR 107.5's
-- tap, CR 119.4's life, CR 107.14's energy -- caps the answer at 1 instead of
-- being counted, which is exact for {T} (a tapped permanent cannot pay it again)
-- and an understatement for the rest (#1132). A MANA part does the same, since
-- repeating it would spend mana this walk has not measured (#1120).
--
-- Understating is the safe direction and the reason the uncounted components cap
-- rather than divide: a supply short by one refuses a cost the payment loop could
-- have paid, while a supply too large offers a cast that then cannot be paid --
-- and an offer that changes nothing is offered again forever.
--
-- Pawl.Engine.Claim.repeats is the arithmetic, and it agrees with
-- Claim.satisfiable by construction: k repetitions ask for k times each claim,
-- and `repeats` is the largest k that Hall's condition still admits. That is what
-- lets Pawl.Engine.Mana take this count as a source's ceiling and then re-ask the
-- joint question across sources without the two disagreeing about one source.
--
-- The pool is read ONCE, off the untouched board, which is exact for every
-- criterion in the pool: taking one creature out of it leaves the rest
-- creatures.
repeatsOf :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Natural
repeatsOf pid oid cost gs = case (Cost.mana cost, traverse (\c -> removalClaim pid oid c gs) (Cost.components cost)) of
  (Just (ManaCost.MkManaCost []), Just claims) -> Claim.repeats claims
  _ -> 1

-- CR 118.3 asked one step later than `canPay` asks it: is SOME nonhybrid
-- equivalent of this cost (CR 601.2b) payable, measured at CR 601.2f's total?
-- That is the castability / activatability gate's question, and it is the same
-- question Pawl.Engine.Mana.announce's own `stillPayable` asks of the routes it
-- offers -- this is that predicate with nothing announced yet, so a gate built on
-- it cannot refuse a cast the announcement would have had an offer for, and
-- cannot offer one the announcement could not complete.
--
-- Why the COMPLETION has to come first: CR 118.7a's reductions come off the
-- generic mana component, and a symbol still spelled {2/R} has no generic
-- component for them to bite (applyAdjustments' MonocoloredHybrid arm). Totalling
-- the printed cost therefore loses the reduction that CR 601.2b's {2}{2}{2}
-- announcement would have exposed, which is Flame Javelin's own ruling read
-- backwards. Completing first and totalling each completion is what puts the two
-- readings in agreement.
--
-- LIFE is threaded, not dropped. `completions` returns the life each route
-- commits -- CR 107.4f's 2 for a Phyrexian symbol paid that way -- having already
-- removed the symbol that commits it, so nothing double-counts; what would go
-- wrong is the other direction, since a route measured without its life is a
-- route offered to a player who cannot afford it. CR 118.3 makes it one demand on
-- one life total together with the CR 119.4 payments the components owe, so
-- `lifeOwedBy` rides on every completion.
--
-- `total` is CR 601.2f's totalling of a mana cost, the CALLER's to supply for the
-- reason Pawl.Engine.Cost.announce's is: Pawl.Engine.Cast passes `totalManas` and
-- Pawl.Engine.Activate passes `pure`, since an activation cost is deliberately not
-- routed through `total` at all (#90).
--
-- It answers MANY totals, one per CR 118.7e resolution of the reductions
-- (`totalManas`), and this asks `any` of them: the choice of half belongs to the
-- player paying and is not made until CR 601.2f, so a cost this gate refuses has
-- to be one NO half of the reduction could have paid. That makes the answer a
-- product of two enumerations -- the cost's completions and the reductions'
-- resolutions -- each of which is 1 where no hybrid symbol appears on that side,
-- so no cost in the pool pays for the second one. The gate's cost is #595's
-- subject.
--
-- The COMPONENTS are asked exactly as `canPay` asks them, and no completion
-- touches them: `completions` rewrites mana symbols only.
canPaySomeCompletion :: PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPaySomeCompletion pid oid total_ cost gs = canPaySomeCompletionGiven (Projection.controlGrants gs) (Projection.projectAll gs) pid oid total_ cost gs

-- The same question given a board the CALLER has already walked. The wrapper
-- above reaches Mana.canPayCommitting, which takes one control-grant walk and
-- one whole-board projection per call; Action.legalActions' activation gate asks
-- it once per permanent, so that is a whole-board sweep per permanent (#716).
-- Handing the board in changes no answer -- see Mana.payableResolutionsGiven and
-- the snapshot argument at Projection.projectGiven.
--
-- ONLY the mana half gets the pre-walked board. The COMPONENTS are still
-- asked through canPayComponent, whose Sacrifice and TapForTotalPower arms make
-- per-object walks of their own (#1073); no activation cost in the pool carries
-- one. Their CLAIMS do reach the mana side, for `canPay`'s reason (#1134).
canPaySomeCompletionGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPaySomeCompletionGiven grants pcs pid oid total_ cost gs = case Cost.mana cost of
  Nothing -> False
  Just (ManaCost.MkManaCost symbols) ->
    let outside = lifeOwedBy (Cost.components cost)
        claimed = claimsOf pid oid (Cost.components cost) gs
        payable (completed, life) =
          any
            (\totalled -> Mana.canPayCommittingGiven manaActivations grants pcs pid (outside + life) claimed totalled gs)
            (total_ (ManaCost.MkManaCost completed))
     in any payable (Mana.completions symbols)
          && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)
          && jointlyPayable pid oid (Cost.components cost) gs

-- CR 119.4's payments a cost owes OUTSIDE its mana part, added up -- what CR
-- 118.3 makes the mana part's own life share a total with. A cost with no PayLife
-- component owes 0, which is what leaves every such cost's answer exactly as it
-- was.
--
-- The only component that spends life: this module is the one place that matches
-- a CostComponent constructor, and the match is total so a new life-spending
-- component cannot be added without answering here.
lifeOwedBy :: [CostComponent.CostComponent Keyword.Type.Keyword] -> Natural
lifeOwedBy = sum . fmap lifeOwedByComponent

lifeOwedByComponent :: CostComponent.CostComponent Keyword.Type.Keyword -> Natural
lifeOwedByComponent component = case component of
  CostComponent.PayLife n -> n
  CostComponent.TapThis -> 0
  CostComponent.UntapThis -> 0
  CostComponent.SacrificeThis -> 0
  CostComponent.Sacrifice _ _ -> 0
  CostComponent.TapForTotalPower _ _ -> 0
  CostComponent.DiscardCards _ -> 0
  CostComponent.DiscardThis -> 0
  CostComponent.PayEnergy _ -> 0
  CostComponent.AddLoyaltyToThis _ -> 0
  CostComponent.RemoveLoyaltyFromThis _ -> 0
  CostComponent.PutPlusOneCountersOnThis _ -> 0
  CostComponent.ExileThisFromGraveyard -> 0
  CostComponent.ExileCardsFromGraveyard _ _ -> 0
  CostComponent.ExileTopFromGraveyard _ -> 0

canPayComponent :: PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> GameState -> Bool
canPayComponent pid oid component gs = case component of
  -- CR 107.5: a permanent that's already tapped can't be tapped again to pay the
  -- cost.
  CostComponent.TapThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Set.member oid (GameState.battlefield gs) && Object.tapped obj == TapState.Untapped
  -- CR 107.6: the exact mirror of TapThis above, and the reason a {Q} ability is
  -- one a player uses on a creature they left tapped.
  CostComponent.UntapThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Set.member oid (GameState.battlefield gs) && Object.tapped obj == TapState.Tapped
  -- CR 701.21a: only a permanent, and only one this player controls -- and CR
  -- 101.2, only one no effect in force says can't be sacrificed.
  --
  -- The prohibition is read in this module and not left to the funnel, because a
  -- cost announced as payable and then unpayable would spend an activation and
  -- leave the permanent alive; CR 118.3's "fully" is what forbids that. WHICH of
  -- this module's two readings does it is not decided here: `removalClaim` above
  -- asks the same question and `canPay` conjoins both answers, so either alone
  -- refuses the cost. Stated twice for the Sacrifice arm's reason -- a component
  -- is asked about on its own terms here -- and the two must not disagree.
  CostComponent.SacrificeThis ->
    Set.member oid (GameState.battlefield gs)
      && Projection.controllerOf oid gs == Just pid
      && not (SacrificeRestriction.prohibited oid gs)
  -- CR 119.4: payable only if the life total is at least the amount. Shared with
  -- CR 107.4f's Phyrexian mana symbol, which pays life for a MANA symbol and so
  -- reads the same floor from inside Pawl.Engine.Mana.
  --
  -- This component ALONE, which is not CR 118.3's question and is not what
  -- decides it: canPay above hands `lifeOwedBy`'s sum to the mana side, where the
  -- same floor is read of the whole cost's life at once. Kept because a component
  -- is asked about on its own terms here, and it can only ever be the weaker of
  -- the two.
  CostComponent.PayLife n -> Event.canPayLife pid n gs
  -- CR 701.21a: this player must control at least `n` matching permanents.
  --
  -- This component ALONE, which is not the whole of CR 118.3's question, exactly
  -- as the PayLife arm above is not: two Sacrifice components of one cost can
  -- each find the same permanent here, and `jointlyPayable` is what asks them
  -- together. Kept because a component is asked about on its own terms here, and
  -- it can only ever be the weaker of the two -- it is the singleton subset of
  -- Hall's condition, spelled out.
  CostComponent.Sacrifice n criterion ->
    Natural.length (Replacement.sacrificeCandidates pid (Just oid) criterion gs) >= n
  -- CR 702.122a: payable iff SOME subset of the candidates reaches the
  -- threshold. Which is decided without enumerating one, because the greatest
  -- total any subset can reach is the sum of the candidates' POSITIVE powers:
  -- adding a candidate with power 0 or less can only leave the total where it
  -- was or lower it, and the player is never obliged to add one. So this is
  -- exact rather than a bound, and it is `>=` because CR 702.122a says "or
  -- greater".
  --
  -- A threshold of 0 is payable by the empty set, which this answers True for
  -- without a candidate on the board. No printing has crew 0; the arithmetic
  -- simply does not need a special case.
  CostComponent.TapForTotalPower n criterion ->
    sum (fmap (max 0 . (`tapPower` gs)) (tapCandidates pid oid criterion gs)) >= toInteger n
  -- CR 601.2f: payable only if the hand holds at least that many cards.
  --
  -- `oid` is excluded, and that is CR 601.2a, not a convenience: the card moves
  -- to the stack at step (a), so by the time 601.2f determines the total cost the
  -- spell is NOT in its controller's hand and cannot be discarded to pay its own
  -- additional cost. A NO-OP for the payment, which Pawl.Engine.Cast runs on the
  -- CR 400.7 stack incarnation, and load-bearing for the OFFER, which
  -- Pawl.Engine.Cast.castable measures while the card is still where it was:
  -- without this filter a hand of "Cathartic Reunion plus one other card" would
  -- read as payable and the Reunion would be offered on the strength of
  -- discarding itself.
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
  -- CR 406.2: payable only while the card is in the paying player's graveyard.
  -- DiscardThis's shape verbatim, and for its reason: CR 108.4 gives a card in a
  -- graveyard no controller, and CR 400.3 puts it in its OWNER's graveyard, so the
  -- pair of facts to ask about is the zone and the owner.
  --
  -- This is the whole of CR 113.6m's enforcement for the COST: an ability whose
  -- cost names the graveyard is unpayable anywhere else, so the zone gate
  -- Pawl.Engine.Activate applies to the OFFER cannot be the only thing standing
  -- between a Loxodon Surveyor on the battlefield and a free draw.
  CostComponent.ExileThisFromGraveyard -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Object.zone obj == Zone.Graveyard && Object.owner obj == pid
  -- CR 118.3: "a player can't pay a cost without having the necessary resources
  -- to pay it fully", so this is payable only if this player's own graveyard
  -- holds at least that many matching cards. Headless Skaab with an empty
  -- graveyard is not merely unpaid, it is never OFFERED -- Pawl.Engine.Cast.castable has
  -- canPay as a conjunct, which is what puts the additional cost INSIDE the
  -- total cost the way CR 601.2f says rather than after announcement.
  --
  -- Sacrifice's floor above, over a different pool -- and this component ALONE
  -- for Sacrifice's reason, with `jointlyPayable` asking the several
  -- object-removing components of one cost together.
  CostComponent.ExileCardsFromGraveyard n criterion ->
    Natural.length (exileCandidates pid criterion gs) >= n
  -- CR 118.3 again: payable only if the graveyard holds a matching card at all,
  -- since the top one is then determined.
  CostComponent.ExileTopFromGraveyard criterion ->
    Maybe.isJust (topExileCandidate pid criterion gs)
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
  -- CR 701.63a puts the counters on "that permanent", so the one thing that can
  -- make this unpayable is the permanent no longer being there -- which is the
  -- only reason CR 701.63a's and CR 702.123a's rulings name ("if you can't put
  -- +1/+1 counters on the creature for any reason, for example if it's no longer
  -- on the battlefield, you'll just create a Spirit token"). Deliberately NOT
  -- gated on control, unlike the two loyalty arms above: CR 701.63a fixes the
  -- payer as the permanent's controller when the ability triggers, and CR 122.6
  -- lets counters go onto a permanent whoever controls it by the time that
  -- ability resolves.
  CostComponent.PutPlusOneCountersOnThis _ -> Set.member oid (GameState.battlefield gs)

-- CR 601.2g then 601.2h: the mana window first, then the payment. Components are
-- paid in PRINTED order; CR 601.2h lets the player pay in any order, which is an
-- elision here (#105).
--
-- That elision is OBSERVABLE, and this is where: paying one object-removing
-- component takes an object the next one could have used, so a payer who would
-- have ordered the two differently -- or answered the first prompt differently
-- -- can lose a payment `canPay` correctly called payable. The board never goes
-- illegal for it; the restore below makes an Unpaid payment a complete no-op, so
-- what is lost is the activation rather than the game state.
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
    -- CR 601.2g: payMana PROMPTS for which sources to activate, so it is monadic
    -- and restores the pre-payment state itself when it cannot be paid.
    Just manaCost -> do
      paidMana <- payMana pid manaCost
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

-- CR 601.2g: if the total cost includes a mana payment, the player then has a
-- chance to activate mana abilities. Reached from an ability too, by CR 602.2b.
--
-- HERE rather than in Pawl.Engine.Mana, which keeps pools, production and
-- spending, because CR 602.2b makes the mana window recursive: a mana ability is
-- activated by paying ITS cost (tapForMana below), and that cost's non-mana
-- components are paid by payComponents just above. Mana cannot reach this module
-- -- Pawl.Engine.Cost imports it -- so the whole mutual recursion lives on this
-- side of the edge.
--
-- Returns whether it was paid; on failure nothing is spent, which is CR 601.2h's
-- bar on partial payments rather than mere tidiness. The prompts themselves are
-- NOT rolled back -- they live in the Program, outside the state -- so a failed
-- payment still asked its questions.
--
-- Failure is REACHABLE two ways. canPay asks whether SOME sequence of choices
-- pays the cost; this asks the player to make them, and they may tap their only
-- Birds of Paradise for green and then be unable to pay {B}, or decline to tap
-- anything at all (CR 118.3c). The engine must let them do either: choosing
-- badly is a choice, and second-guessing it here would be the engine playing the
-- game.
--
-- One prompt per source tapped, against a shrinking candidate list, rather than
-- one prompt for a whole subset: a cost needing {G}{G} is two decisions, and the
-- second is made knowing the first.
--
-- The window CLOSES when the player says so, not when the cost is covered. CR
-- 605.3a's permission to activate a mana ability while casting is not rationed
-- by what the cost needs, so the loop keeps asking once the pool covers it --
-- Omnath, Locus of Mana is the pool's reason to say yes -- and CR 118.3c's "not
-- mandatory" lets the answer be none at all, which fails the payment. Which of
-- those two questions is asked is exactly whether the pool covers the cost yet,
-- since that is what the player's silence would mean.
--
-- Ordering the window as "cover the cost, then float" restricts nothing: a
-- player may still tap any source at either point, so every subset of their
-- sources is still reachable. What the split buys is a sane default for a
-- caller with no player attached (Pawl.Engine.Replay.defaultAnswer).
--
-- `refused` is what keeps that loop finite now that activating a mana ability can
-- FAIL: a source whose own activation cost went unpaid is dropped from the
-- candidates for the rest of this payment, since re-offering an untapped source
-- that just refused to pay would ask the same question forever. What reaches it
-- is a payment REFUSED and not one that was never payable: CR 118.3's gate
-- (manaActivations) keeps an unpayable option off the offer to begin with.
--
-- The life budget only ever binds a cost NOTHING ANNOUNCED for: a cast (CR
-- 601.2b) and an activation (CR 602.2b) both run `announce` first, so the cost
-- arriving here holds no Phyrexian symbol and the budget is 0. What is left
-- under it is CR 118.13b/c, a cost paid during a resolution or for a special
-- action, where pawl still chooses (#373).
--
-- It is recomputed on EVERY pass rather than fixed at entry, because a tap can
-- change it: a Birds of Paradise tapped for blue takes the mana way to an
-- unannounced {G/P} off the board, and the cost is then payable only by CR
-- 107.4f's 2 life. Recomputing means pawl pays it, rather than failing the
-- payment the way the paragraph above lets a mis-tapped {B} fail -- the same MORE
-- PERMISSIVE posture, and reachable only where nothing announced (#373). Zero
-- when the cost is unpayable outright.
payMana :: PlayerId -> ManaCost.ManaCost -> Game Bool
payMana pid cost = do
  before <- State.get
  paid <- window Set.empty
  Monad.unless paid (State.put before)
  pure paid
  where
    -- What the pool would leave if the cost were paid out of it right now.
    settlement gs = Mana.spend (Maybe.fromMaybe 0 (Mana.lifeNeeded manaActivations pid cost gs)) cost (Game.poolOf pid gs)
    window refused = do
      gs <- State.get
      let covered = Maybe.isJust (settlement gs)
      case filter (`Set.notMember` refused) (Mana.manaSources manaActivations pid gs) of
        [] -> settle
        candidate : rest -> do
          answer <- chooseSource covered pid (candidate NonEmpty.:| rest) gs
          case answer of
            Nothing -> settle
            Just oid -> do
              produced <- tapForMana oid
              window (if produced then refused else Set.insert oid refused)
    -- CR 601.2h: the window is closed, so the cost is paid out of what is there
    -- -- and simply is not paid when the player floated too little.
    settle :: Game Bool
    settle = do
      gs <- State.get
      case settlement gs of
        Nothing -> pure False
        Just (left, life) -> do
          State.put (Event.payLife pid life (Mana.setPool pid left gs))
          pure True

-- Which source to tap next, or none. `covered` says whether the pool already
-- pays the cost, which picks between CR 118.3c's question and CR 601.2g's.
--
-- Asked on every pass, and NEVER elided, not even for a single candidate:
-- declining is an answer on every board, so there is always a choice to make.
-- What makes a lone candidate worth asking about is Mana Confluence -- "{T}, Pay
-- 1 life" is a cost a player at 1 life would rather not pay, and it is often
-- their only source, so eliding here would tap it for them.
--
-- Same-card candidates are not collapsed either. Two Llanowar Elves are one
-- card, but one may be equipped or enchanted, one may carry +1/+1 counters, one
-- may be borrowed until end of turn, and one may be blocking (CR 506.4 does not
-- remove a creature from combat for tapping). `Game.cardOf` compares PRINTED
-- identity and cannot see any of it, so collapsing them would suppress exactly
-- the prompts the invariant exists to force (#217).
--
-- FILTERED, NOT TRUSTED, the posture Combat.declareAttackers and payComponents
-- already take. Beyond hygiene, an answer outside the offered set is one payMana
-- would spend a whole pass of its loop on for nothing: an unknown or mana-less
-- id, or a source its `refused` set has already given up on. An unrecognised id
-- reads as declining rather than as the head candidate, since the fallback must
-- not tap something on the player's behalf.
chooseSource :: Bool -> PlayerId -> NonEmpty.NonEmpty ObjectId -> GameState -> Game (Maybe ObjectId)
chooseSource covered pid candidates gs = do
  let decider = Decide.deciderFor pid gs
  answer <-
    Game.choose $
      if covered
        then Prompt.ChooseExtraManaSource decider pid candidates
        else Prompt.ChooseManaSource decider pid candidates
  pure $ case answer of
    Just oid | List.elem oid (NonEmpty.toList candidates) -> Just oid
    _ -> Nothing

-- CR 106.12's "tap [a permanent] for mana" -- activate one of its mana abilities,
-- which by CR 602.2b means paying that ability's whole cost and then adding what
-- it yields. CR 605.3b: a mana ability does not use the stack, so this is
-- immediate -- which is also why the colour choice is made HERE and not by
-- Pawl.Engine.Resolve.
--
-- Monadic because of that choice. A Mountain offers one yield and is never
-- asked; Birds of Paradise (CR 105.4) and an Urborg'd Mountain (CR 305.6/305.7)
-- offer several, and the engine never picks for the player. Which SOURCE to tap
-- is a separate question, answered differently in CR 605.3a's three windows:
-- payMana asks it (Prompt.ChooseManaSource) because a payment has to keep
-- tapping until the cost is met, while a player with priority has already
-- answered it by choosing an Action.ActivateManaAbility off the menu.
--
-- The whole yield lands, so Sol Ring's "{T}: Add {C}{C}" adds two units from one
-- activation. The TAP is no longer written here: it is the CR 107.5 component of
-- the cost being paid (Mana.intrinsicManaCost for CR 305.6's ability), so a cost
-- that also charges life charges it, and Mana Confluence pays 1.
--
-- Answers whether mana was actually added, which payMana's loop reads.
--
-- CR 118.3 GATES the options before any of this: an option whose activation cost
-- the controller cannot pay is not offered and not paid, so a tapped permanent
-- adds nothing (CR 107.5) and Phyrexian Tower with no creature offers only its
-- {C}. Asked here as well as at the offer (Mana.manaSourcesGiven) because this
-- is where the payment happens, and the two questions differ: a source is
-- offered on having SOME payable option, and this picks among exactly those.
--
-- Not implemented: the ability's non-mana clauses -- Ancient Tomb's "deals 2
-- damage to you". Running them needs the effect executor, which is
-- Pawl.Engine.Resolve, above this module (#1118).
tapForMana :: ObjectId -> Game Bool
tapForMana oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure False
    Just obj -> do
      -- CR 109.4a/110.2: mana goes to the mana ability's controller, which is
      -- the permanent's controller (CR 106.4 only says it lands in "a player's
      -- mana pool", not whose) -- and that same player makes the colour choice
      -- and pays the cost. Falls back to owner in the impossible case
      -- lookupObject just proved oid exists but controllerOf returns Nothing.
      let controller = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
      case filter (\option -> fst (manaActivations Map.empty controller oid (ManaOption.cost option) gs) > 0) (Mana.manaOptionsOf oid gs) of
        [] -> pure False
        first : rest -> do
          chosen <- chooseManaYield controller oid (first NonEmpty.:| rest) gs
          outcome <- payActivation controller oid (ManaOption.cost chosen)
          case outcome of
            Payment.Unpaid -> pure False
            Payment.Paid -> do
              State.modify' (Mana.addMana controller (Mana.unitsOf (ManaOption.yield chosen)))
              pure True

-- CR 602.2b sends an activation cost through CR 601.2b-i, so a mana ability pays
-- its whole cost. All or nothing, `pay`'s posture and for CR 601.2h's reason.
--
-- COMPONENTS FIRST, where `pay` opens the CR 601.2g mana window first. That
-- inverts CR 601.2g/h, and the reason is termination: {T} is a component, so
-- paying components first takes this source off its own mana window's candidate
-- list before payMana goes looking (manaSourcesGiven keeps only untapped
-- permanents). Left in rule order, a mana ability whose cost held mana would tap
-- itself to pay itself, forever.
--
-- Unobservable in this pool, and the short-circuit below is why: every mana
-- ability in `data/cards/` has an EMPTY mana part, so no window opens and there
-- is no order to get wrong. The first mana ability charging mana -- Cabal
-- Coffers' "{2}, {T}" -- is what would make the inversion visible, and wants CR
-- 601.2g put back with a different guard (#1120).
--
-- That short-circuit is a performance call as well: payMana would answer True at
-- once on {0}, but its first act is a whole-board payability walk, and this is on
-- the path of every tap for mana (#200, #716).
payActivation :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game Payment.Payment
payActivation pid oid cost = do
  before <- State.get
  outcome <- payComponents pid oid (Cost.components cost)
  paid <- case (outcome, Cost.mana cost) of
    (Payment.Paid, Just (ManaCost.MkManaCost [])) -> pure True
    (Payment.Paid, Just manaCost) -> payMana pid manaCost
    -- CR 118.6: attempting to pay an unpayable cost is an illegal action.
    _ -> pure False
  Monad.unless paid (State.put before)
  pure (if paid then Payment.Paid else Payment.Unpaid)

-- Which way this source is tapped -- which of its mana abilities, in which mode,
-- and which colour each of that mode's AddMana effects makes, asked as ONE
-- question because the answer is one activation.
--
-- The COST rides along with the yield (Pawl.Types.ManaOption) rather than the
-- yield going alone, because two of one permanent's mana abilities can add the
-- same mana for different costs: an Urborg'd Mana Confluence adds {B} for {T},
-- and adds {B} for {T} plus a life. A yield-only answer names both, and the
-- engine picking either is it deciding what the player pays.
--
-- Elided exactly when the source offers ONE option, where no choice exists --
-- Mana.manaOptionsOf has already collapsed routes alike in cost and yield, so
-- what arrives here is distinct and a list of two is two real options.
--
-- FILTERED, NOT TRUSTED, the posture chooseSource and payComponents take. Here
-- that is not merely hygiene -- honouring an option the source does not offer
-- would mint mana out of nothing, or charge the wrong cost for it.
chooseManaYield :: PlayerId -> ObjectId -> NonEmpty.NonEmpty ManaOption.ManaOption -> GameState -> Game ManaOption.ManaOption
chooseManaYield pid oid candidates gs = case candidates of
  only NonEmpty.:| [] -> pure only
  _ -> do
    answer <- Game.choose (Prompt.ChooseManaYield (Decide.deciderFor pid gs) pid oid candidates)
    pure $
      if List.elem answer (NonEmpty.toList candidates)
        then answer
        else NonEmpty.head candidates

payComponent :: PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> Game Payment.Payment
payComponent pid oid component = case component of
  CostComponent.TapThis -> do
    tapObject oid
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
    State.modify' (Event.payLife pid n)
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
    let candidates = Replacement.sacrificeCandidates pid (Just oid) criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length candidates <= n
        then pure (Set.fromList candidates)
        else Game.choose (Prompt.ChooseSacrifices decider pid oid candidates n)
    if Set.isSubsetOf chosen (Set.fromList candidates) && Natural.length chosen == n
      then do
        Monad.mapM_ (Event.sacrifice pid) (Set.toAscList chosen)
        pure Payment.Paid
      else pure Payment.Unpaid
  -- CR 702.122a: the payer chooses WHICH permanents to tap and HOW MANY, so this
  -- is a prompt, and unlike Sacrifice above it is NEVER elided. Whether the
  -- answer is forced is a question about subsets rather than about a count --
  -- crew 6 with a 6-power and a 7-power creature has two legal answers and crew
  -- 6 with a 4 and a 3 has one -- so eliding would mean enumerating subsets to
  -- find out, and getting it wrong in the second direction decides for the
  -- player. Asking a forced question costs a redundant prompt and decides
  -- nothing. See Pawl.Types.Prompt.ChooseTapsForTotalPower.
  --
  -- Reject-not-repair, Sacrifice's posture: an answer that is not a subset of the
  -- offered candidates, or whose total power falls short, makes the whole payment
  -- Unpaid, which `pay`'s restore turns into a no-op. The total is summed over
  -- the answer as given, INCLUDING any negative power in it -- CR 702.122a
  -- measures the creatures that were tapped, not a best case.
  --
  -- The tap is a direct edit, TapThis' route and for TapThis' stated reason:
  -- nothing in the pool watches for a permanent becoming tapped, so the funnel
  -- CR 701.26a would justify has no observer to serve yet.
  --
  -- Not implemented: CR 702.122b/c's "crews a Vehicle" and "crewed by" relation,
  -- and so CR 702.122e's "becomes crewed" trigger and CR 702.122d's "can't crew
  -- Vehicles" restriction -- the chosen set is spent here and recorded nowhere
  -- (#915).
  CostComponent.TapForTotalPower n criterion -> do
    gs <- State.get
    let candidates = tapCandidates pid oid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <- Game.choose (Prompt.ChooseTapsForTotalPower decider pid oid candidates n)
    let totalPower = sum (fmap (`tapPower` gs) (Set.toAscList chosen))
    if Set.isSubsetOf chosen (Set.fromList candidates) && totalPower >= toInteger n
      then do
        Monad.mapM_ tapObject (Set.toAscList chosen)
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
        else Game.choose (Prompt.ChooseDiscard decider pid held n)
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
  -- through Event.putCounters, which is the CR 614 funnel: CR 614.16 admits
  -- a counter-scaling replacement (Doubling Season, Hardened Scales) only where a
  -- resolving spell or ability's EFFECT puts the counter on, and CR 602.2b pays
  -- an activation cost as part of ACTIVATING the ability (CR 601.2h) rather than
  -- as part of resolving it -- so CR 609.1's "when a spell, activated ability, or
  -- triggered ability resolves, it may create one or more ... effects" has no
  -- resolution to hang this placement on. The counters CR 306.5b's
  -- enters-with replacement places DO go through that funnel, by CR 614.16's next
  -- clause -- which is what makes Doubling Season double a planeswalker's starting
  -- loyalty and leave its +1 alone. Contrast PutPlusOneCountersOnThis below, the
  -- one component paid DURING a resolution.
  CostComponent.AddLoyaltyToThis n -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (addLoyalty n) oid (GameState.objects gs)})
    pure Payment.Paid
  -- CR 606.4's other half. Natural subtraction is PARTIAL, so the floor is
  -- guarded exactly as PayEnergy's is above: canPayComponent's CR 606.6 check
  -- guarantees `have >= n` at pay time, and the guard keeps this total anyway.
  CostComponent.RemoveLoyaltyFromThis n -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (removeLoyalty n) oid (GameState.objects gs)})
    pure Payment.Paid
  -- CR 122.6's placement, through the Event.putCounters funnel as
  -- CounterCause.ByEffect -- the opposite call from AddLoyaltyToThis above, and
  -- the difference is WHEN the cost is paid. CR 118.12 pays this one "when the
  -- spell or ability resolves", so the counters land as part of that resolution,
  -- which is what CR 609.1 calls an effect and so what CR 614.16 reaches: "the
  -- effect of a resolving spell or ability puts a counter on a permanent".
  -- Hardened Scales therefore sees endure's counter; it still does not see a
  -- planeswalker's +1.
  --
  -- Paid whatever the funnel then places. CR 118.3's "fully" is answered by
  -- canPayComponent above, and a replacement that grows or erases the placement
  -- afterwards is CR 614's business rather than the payment's -- the counters
  -- were put on, so the "if you don't" branch does not run.
  CostComponent.PutPlusOneCountersOnThis n -> do
    -- CR 609.1: the player putting them is the one whose resolution this is, which
    -- for a cost paid during a resolution is the player paying it.
    Monad.void (Event.putCounters (CounterCause.ByEffect pid) oid CounterKind.PlusOnePlusOne n)
    pure Payment.Paid
  -- CR 406.2's move, through Event.changeZone -- the shared zone-change funnel, so
  -- the card gets a CR 400.7 incarnation and anything watching a graveyard-to-exile
  -- move sees it. No prompt: the cost names this card, exactly as DiscardThis does.
  --
  -- The card is in EXILE by the time the ability resolves, which is what makes CR
  -- 113.7a's "once activated, an ability exists on the stack independently of its
  -- source" load-bearing here: Loxodon Surveyor's draw resolves off a source that
  -- has already left the graveyard the cost read.
  CostComponent.ExileThisFromGraveyard -> do
    Event.changeZone oid Zone.Exile
    pure Payment.Paid
  -- CR 406.2's move again, through the same Event.changeZone funnel, but for
  -- CHOSEN cards: the payer picks which, so this is a prompt and never an engine
  -- pick. Elided only when forced -- no more candidates than the count -- which
  -- is Sacrifice's elision and ChooseSacrifices' documented rule.
  --
  -- Reject-not-repair, Sacrifice's posture verbatim: an answer that is not a
  -- size-`n` subset of the offered candidates makes the whole payment Unpaid,
  -- which `pay`'s restore turns into a no-op.
  --
  -- The candidates are read ONCE, before the prompt, so the answer is checked
  -- against the same list the player was offered.
  CostComponent.ExileCardsFromGraveyard n criterion -> do
    gs <- State.get
    let candidates = exileCandidates pid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length candidates <= n
        then pure (Set.fromList candidates)
        else Game.choose (Prompt.ChooseExilesFromGraveyard decider pid oid candidates n)
    if Set.isSubsetOf chosen (Set.fromList candidates) && Natural.length chosen == n
      then do
        Monad.mapM_ (\c -> Event.changeZone c Zone.Exile) (Set.toAscList chosen)
        pure Payment.Paid
      else pure Payment.Unpaid
  -- CR 406.2 with no prompt at all: the card is determined by CR 404.2's order,
  -- so this reads it and exiles it. Unpaid where the graveyard holds no matching
  -- card, which agrees with canPayComponent above and leaves `pay`'s restore to
  -- undo the rest.
  CostComponent.ExileTopFromGraveyard criterion -> do
    gs <- State.get
    case topExileCandidate pid criterion gs of
      Nothing -> pure Payment.Unpaid
      Just candidate -> do
        Event.changeZone candidate Zone.Exile
        pure Payment.Paid

-- The arithmetic half, pure and board-free.
--
-- 1. Every INCREASE is added to the generic component (CR 601.2f's order, and
--    Thalia's own ruling: increases first, then reductions).
-- 2. A REDUCTION is an amount of mana, read component by component. Its GENERIC
--    part comes off the generic component only (CR 118.7a), floored at zero -- a
--    generic reduction with no generic left to take is simply lost. Its TYPED
--    part cancels matching typed symbols in the cost, one for one (Edgewalker: a
--    {W}{B} reduction takes one white and one black out of a Cleric's cost). CR
--    118.7f puts a PHYREXIAN symbol on that typed side as well -- a reduction
--    written {G/P} takes one green mana, and needs no announcement to do it --
--    which is where the two sides of the cancellation stop agreeing. CR 118.7g
--    sends a SNOW symbol the other way: an {S} in a reduction is that much
--    generic mana, even though an {S} in a cost is no part of the generic
--    component at all (CR 107.4h). Both readings of both symbols depend on which
--    SIDE the symbol is on, which is why each of the two questions this function
--    asks is asked by two functions.
-- 3. An EXCESS typed symbol -- one whose type the cost has already run out of --
--    is DROPPED, not spilled onto the generic component. That is the card text
--    CR 101.1 lets override the rules, not CR 118.7b-d: every PRINTED reducer
--    that names a type reduces only coloured mana, and Edgewalker's reminder
--    text settles what that means -- a {1}{W} Cleric spell costs {1}, so the
--    stranded {B} leaves the {1} alone. CR 118.7b-d's spill has no printed
--    producer (#309). The pool's reducers WITHOUT that sentence are the four
--    synthetics CR 118.7e-g needed, and no test aims a TYPED one at a cost the
--    spill would reach: the {S} and {2/B} reductions name no type for the spill
--    to strand, and the {G/P} and {W/B} ones are aimed at costs with no generic
--    component for CR 118.7b-c to move the stranded mana onto.
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
      costGenericOf symbol = case symbol of
        ManaSymbol.Generic n -> n
        ManaSymbol.OfType _ -> 0
        -- CR 107.4e: a colour/colour hybrid is paid with one mana of a stated
        -- type, so it is no part of the generic component CR 118.7a reductions
        -- come off.
        ManaSymbol.Hybrid _ _ -> 0
        -- A monocolored hybrid's {2} half IS generic mana once CR 601.2b's
        -- nonhybrid equivalent names it -- but a symbol still spelled {2/R} is one
        -- CR 601.2b has NOT named, so there is nothing yet for CR 118.7a to come
        -- off and the symbol is left whole. That is Flame Javelin's own ruling: a
        -- generic cost reduction applies to it only where the announced payment
        -- includes generic mana. Pawl.Engine.Mana.announce makes that
        -- announcement, so the cost actually paid reaches here as a Generic and is
        -- reduced -- and so does the CASTABILITY GATE's, since
        -- canPaySomeCompletion completes the cost before totalling each
        -- completion, which is what stopped the two disagreeing. What still
        -- arrives unannounced is CR 118.13b/c's costs, which have no announcement
        -- at all (#373).
        ManaSymbol.MonocoloredHybrid _ -> 0
        -- CR 107.4f makes this a COLOURED mana symbol, and its other half is 2
        -- life rather than any amount of mana, so there is no generic component
        -- for CR 118.7a's reduction to come off either way.
        ManaSymbol.Phyrexian _ -> 0
        -- CR 107.4h says outright that generic reductions don't affect {S} costs,
        -- which is the whole reason it is not spelled Generic 1. The one arm
        -- where this function and reducingGenericOf part company, and the
        -- Adjustments case "CR 107.4h a generic reduction does not affect an {S}
        -- in the cost" is what proves this side of it.
        ManaSymbol.Snow -> 0
        -- Unreachable: CR 601.2b precedes 601.2f, so Mana.substituteX has
        -- already replaced every Variable before a total cost is computed. The
        -- match must be total, so a bare {X} contributes 0 generic.
        ManaSymbol.Variable -> 0
      -- The REDUCTION's generic amount, and two functions rather than one for
      -- the reason costManaTypeOf and reducingManaTypeOf are two: CR 118.7g
      -- makes the two sides read an {S} differently. Every other arm agrees with
      -- costGenericOf's, and agreeing is not sharing -- which side a symbol is
      -- on is not a property of the symbol.
      reducingGenericOf symbol = case symbol of
        -- CR 118.7a's amount of generic mana, which is what this whole side is.
        ManaSymbol.Generic n -> n
        -- CR 118.7b-d would turn a typed reduction the cost cannot use into
        -- generic mana; pawl drops it instead, for the reason the header gives
        -- (#309). Either way it is the typed side, not this one, that reads an
        -- OfType.
        ManaSymbol.OfType _ -> 0
        -- CR 107.4e's colour/colour hybrid has no generic half at all, so
        -- whichever way CR 118.7e's choice went it is the typed side below that
        -- reads the answer.
        ManaSymbol.Hybrid _ _ -> 0
        -- A symbol still spelled {2/R} HERE is one CR 118.7e's choice has not
        -- been made for -- announceReductions leaves a Generic behind when the
        -- {2} half is taken, which the arm above reads, and the gate enumerates
        -- the same halves. What reaches this arm is `total`'s unannounced
        -- reading, which nothing in the engine asks for.
        ManaSymbol.MonocoloredHybrid _ -> 0
        -- CR 118.7f gives a Phyrexian reduction to the typed side whole --
        -- "one mana of that symbol's color" -- so it takes no generic mana.
        ManaSymbol.Phyrexian _ -> 0
        -- CR 118.7g: "If a cost is reduced by an amount of mana represented by
        -- one or more snow mana symbols, the cost is reduced by that much
        -- generic mana." THE arm this side exists for. CR 107.4h's sentence
        -- about {S} costs is about the other side and does not reach here.
        ManaSymbol.Snow -> 1
        -- Unreachable for the reason costGenericOf's Variable arm gives; {X} is
        -- no amount of mana until CR 601.2b names one.
        ManaSymbol.Variable -> 0
      -- "Typed" for this function's purpose means "not generic": everything but
      -- Generic survives the filter and keeps its printed position, which is what
      -- "the SURVIVING printed typed symbols in their original order" above
      -- promises and the only way an unreducible symbol reaches Mana.spend
      -- intact. Variable is unreachable for the reason costGenericOf's arm gives, and
      -- is kept rather than stripped so that it would still survive if it were.
      isTyped symbol = case symbol of
        ManaSymbol.Generic _ -> False
        ManaSymbol.OfType _ -> True
        ManaSymbol.Hybrid _ _ -> True
        ManaSymbol.MonocoloredHybrid _ -> True
        ManaSymbol.Phyrexian _ -> True
        ManaSymbol.Snow -> True
        ManaSymbol.Variable -> True
      -- The two SIDES of the cancellation, and they are two functions because CR
      -- 118.7f makes them disagree: which one mana type a printed COST symbol
      -- offers up, and which one a REDUCTION's symbol takes away. Nothing means
      -- the symbol plays no part in the cancellation from that side.
      --
      -- Which side a symbol is on is not a property of the symbol, so nothing
      -- here can be shared: {G/P} names green when a reduction says it and names
      -- nothing yet when a cost does, and the group SyntheticPhyrexianDiscount
      -- in Pawl.PlayerEffectSpec proves both halves against cards. The
      -- costGenericOf/reducingGenericOf pair above splits the generic question
      -- the same way, for CR 118.7g's sake.
      costManaTypeOf symbol = case symbol of
        ManaSymbol.Generic _ -> Nothing
        ManaSymbol.OfType manaType -> Just manaType
        -- CR 107.4e names TWO types, so no one type can be read off it -- and a
        -- symbol still spelled {G/U} here is one CR 601.2b has not named, since
        -- Pawl.Engine.Mana.announce leaves an OfType behind when it does and the
        -- arm above reads that. What still arrives unannounced is CR 118.13b/c's
        -- costs (#373), and Edgewalker's ruling is what that costs.
        ManaSymbol.Hybrid _ _ -> Nothing
        -- Same reason: a symbol still spelled {2/R} here is one CR 601.2b has not
        -- named -- Pawl.Engine.Mana.announce leaves an OfType behind when it
        -- does, which the arm above reads -- so there is nothing yet to cancel
        -- against.
        ManaSymbol.MonocoloredHybrid _ -> Nothing
        -- EXACT rather than an elision. The symbol is necessarily UNANNOUNCED, or
        -- it would not be a Phyrexian symbol any more: CR 601.2b's announcement
        -- precedes CR 601.2f's total, and it leaves behind either an OfType or a
        -- payment of life. No caller that reaches this arm has established that
        -- there is a green mana here to cancel, and Edgewalker's ruling read the
        -- right way round says so outright -- "if you choose to pay such a cost
        -- with {W} or {B}, Edgewalker can reduce that part of the cost".
        ManaSymbol.Phyrexian _ -> Nothing
        -- CR 107.4h: {S} is paid with one mana of ANY type, so it names no one
        -- type. Exact rather than an elision too: a reduction of one white mana
        -- cannot single out an {S} the way it singles out a {W}.
        ManaSymbol.Snow -> Nothing
        -- Unreachable for the reason costGenericOf's Variable arm gives; {X} names
        -- no mana type.
        ManaSymbol.Variable -> Nothing
      reducingManaTypeOf symbol = case symbol of
        -- CR 118.7a's half of a reduction, which reducingGenericOf above already
        -- counted.
        ManaSymbol.Generic _ -> Nothing
        ManaSymbol.OfType manaType -> Just manaType
        -- CR 118.7e: the choice of half belongs to the PLAYER PAYING the cost,
        -- so answering it here would be the engine making it. A symbol still
        -- spelled {W/U} at this point is one nobody has been asked about --
        -- announceReductions leaves the chosen half's OfType behind when they
        -- have -- and what reaches this arm is `total`'s unannounced reading,
        -- which nothing in the engine asks for.
        ManaSymbol.Hybrid _ _ -> Nothing
        -- CR 118.7e's other shape, unread here for the same reason. Whichever
        -- half of a {2/R} the payer takes, announceReductions leaves behind the
        -- symbol that half is -- an OfType this arm reads, or a Generic
        -- reducingGenericOf does.
        ManaSymbol.MonocoloredHybrid _ -> Nothing
        -- CR 118.7f: "If a cost is reduced by an amount of mana represented by a
        -- Phyrexian mana symbol, the cost is reduced by one mana of that symbol's
        -- color." The one arm where the two sides part company -- unlike CR
        -- 118.7e's hybrid this asks the player nothing, because the symbol names
        -- exactly one colour and the life half is no part of a reduction.
        ManaSymbol.Phyrexian color -> Just (ManaType.Colored color)
        -- CR 118.7g turns an {S} reduction into that much GENERIC mana, so it is
        -- no part of the typed cancellation: reducingGenericOf's Snow arm is
        -- where such a reduction lands.
        ManaSymbol.Snow -> Nothing
        -- Unreachable for the reason costGenericOf's Variable arm gives; {X} is no
        -- amount of mana until CR 601.2b names one.
        ManaSymbol.Variable -> Nothing
      reducingSymbols = concatMap (\(ManaCost.MkManaCost xs) -> xs) reductions
      raised = sum (fmap costGenericOf symbols) + sum increases
      taken = sum (fmap reducingGenericOf reducingSymbols)
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
        symbol : rest -> case costManaTypeOf symbol of
          Just manaType | elem manaType unspent -> cancel (List.delete manaType unspent) rest
          _ -> symbol : cancel unspent rest
   in ManaCost.MkManaCost (leading <> cancel (Maybe.mapMaybe reducingManaTypeOf reducingSymbols) (filter isTyped symbols))
