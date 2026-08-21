-- CR 118: what a cost IS, and everything the closed half needs to do with one --
-- the candidates (costsFor), the total (total, CR 601.2f), whether it can be
-- paid (canPay, CR 118.3) and paying it (pay). Pawl.Engine.Mana keeps pools,
-- production and spending; this module keeps the cost.
--
-- `pay` serves two contexts: CR 601.2g/h's payment as a spell is cast or an
-- ability activated, and CR 118.12's payment when one RESOLVES. `total`'s CR
-- 601.2f adjustments reach only the first.
--
-- The SOLE casing home for Pawl.Types.CostComponent; every other module reads
-- the classifications derived here instead -- requiresSicknessCheck (CR 302.6),
-- isLoyaltyCost (CR 606.2/606.3), zoneFunctionedFrom (CR 113.6m),
-- statesHiddenQuality (CR 118.8c).
module Pawl.Engine.Cost where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Blight as Blight
import qualified Pawl.Engine.Claim as Claim
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Detain as Detain
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Interchangeable as Interchangeable
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Engine.Summoning as Summoning
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Activations as Activations
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.AppliedReduction as AppliedReduction
import qualified Pawl.Types.CandidateCost as CandidateCost
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Claim (Claim)
import qualified Pawl.Types.Claim as Claim.Type
import qualified Pawl.Types.ClaimAxis as ClaimAxis
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostAdjustments as CostAdjustments
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaOption as ManaOption
import qualified Pawl.Types.ManaSpending as ManaSpending
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
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TapPermanents as TapPermanents
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 118.6: the cost of an object with no mana cost. Also the ChooseCost
-- fallback's answer when no candidate was offered.
unpayable :: Cost Keyword.Type.Keyword
unpayable = Cost.MkCost {Cost.mana = Nothing, Cost.components = []}

-- CR 118.9's "without paying its mana cost": the printed cost with the mana part
-- replaced by an EMPTY ManaCost, which is {0} (CR 118.5a) and never Nothing, CR
-- 118.6's unpayable cost. The additional costs ride along (CR 118.9d), and the
-- face is the one being CAST (CR 709.3a / 712.11a).
withoutPayingManaCost :: Face.Face card -> Cost Keyword.Type.Keyword
withoutPayingManaCost face =
  Cost.MkCost
    { Cost.mana = Just (ManaCost.MkManaCost []),
      Cost.components = Face.additionalCosts face
    }

-- A candidate no keyword ability offered: the printed cost, a printed
-- alternative, or a cost an effect applied (CR 118.9).
untagged :: Cost Keyword.Type.Keyword -> CandidateCost.CandidateCost
untagged = CandidateCost.MkCandidateCost Nothing

-- The first offered candidate, or `unpayable` when none was offered.
firstOffered :: [Cost Keyword.Type.Keyword] -> Cost Keyword.Type.Keyword
firstOffered candidates = case candidates of
  c : _ -> c
  [] -> unpayable

-- CR 702.37a: what a morph cast pays. An alternative cost (CR 118.9) stated by
-- the rule rather than by a card, which is why it is minted here and not read
-- off Keyword.Morph -- that constructor carries CR 702.37e's special-action
-- cost, a different amount on every printing. No additional costs ride along: CR
-- 702.37c measures the cast against the face-down characteristics, which CR
-- 708.2a leaves no text to print one in.
faceDownCost :: Cost Keyword.Type.Keyword
faceDownCost =
  Cost.MkCost
    { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]),
      Cost.components = []
    }

-- The candidate costs for CASTING this object (CR 601.2b) -- from hand, the
-- printed one first, then each alternative, and last any standing CR 118.9
-- grant. Empty for anything that is not a card; a LAND yields one candidate whose
-- mana part is Nothing (CR 202.1), which CR 118.6 makes unpayable.
--
-- The candidates depend on the ZONE, CR 702.34a's permission and its cost being
-- one sentence; on WHICH FACE is being cast (CR 709.3a), which is why the name
-- arrives as an argument; and on the object's FACING, a face-down proposal
-- paying rule 702.37a's {3} and nothing else (CR 708.2a). The facing is asked
-- ahead of the zone case, that ability functioning in any zone the card could be
-- played from. Defined in terms of candidateCostsFor below so the two cannot
-- drift.
costsFor :: CardName.CardName -> ObjectId -> GameState -> [Cost Keyword.Type.Keyword]
costsFor name oid gs = fmap CandidateCost.cost (candidateCostsFor name oid gs)

-- costsFor's list with WHICH ability offered each candidate recorded -- the fact
-- CR 702.34a's "if the flashback cost was paid" and CR 702.133a's jump-start
-- clause are conditioned on. THE GRAVEYARD ARM is the only one that tags: those
-- two are the only abilities asking which candidate cost was the one paid, where
-- CR 702.127a's aftermath asks about the ZONE instead.
candidateCostsFor :: CardName.CardName -> ObjectId -> GameState -> [CandidateCost.CandidateCost]
candidateCostsFor name oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj | Facing.isFaceDown (Object.facing obj) -> [untagged faceDownCost]
  Just obj -> case Object.source obj of
    Source.OfCard printing ->
      let face = Game.resolveFace (Just name) (Printing.card printing)
          printed = Cost.MkCost {Cost.mana = Face.manaCost face, Cost.components = Face.additionalCosts face}
          -- CR 118.9d: an alternative replaces only the MANA cost; every
          -- additional cost still applies. CR 702.34a's last sentence sends
          -- flashback through the same rules, so its cost is wrapped the same.
          withAdditional alternative =
            alternative {Cost.components = Cost.components alternative <> Face.additionalCosts face}
          -- CR 604.2: an alternative cost whose "as long as" clause does not
          -- hold is not offered at all.
          --
          -- CR 109.5's "you" is the OWNER: CR 400.1 files a hand and a graveyard
          -- by player and Cast.zoneCandidates hands out only the caster's own, so
          -- owner and caster coincide wherever this is asked. The graveyard arm's
          -- CR 601.3 permission below reads the owner for the same reason.
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
            -- Four shapes, differing in what they do to the printed cost.
            -- Flashback (CR 702.34a) REPLACES the mana cost, so it is wrapped by
            -- `withAdditional`; aftermath (CR 702.127a) replaces nothing, so it
            -- is `printed`; jump-start (CR 702.133a) ADDS a discard to `printed`,
            -- one however many such abilities the card has; and CR 601.3 /
            -- Yawgmoth's Will is an EFFECT stating no cost, offering the hand's
            -- list BESIDE the three rather than instead of them. Rule 702.34a's
            -- "if the resulting spell is an instant or sorcery spell" gates the
            -- PERMISSION (Keyword.permissionsFor) and is not re-asked here.
            Zone.Graveyard ->
              let -- CR 613.1: the keywords the card HAS in the graveyard, not
                  -- the ones it prints, an ability granted there (CR 113.6f)
                  -- stating rule 702.34a's cost as much as a printed one. Read
                  -- off the OBJECT, so the caller's CR 709.3a half is measured.
                  keywords = Map.keysSet (Projection.keywordsOf oid gs)
                  -- The flashback keyword AS IT WAS READ: rule 702.34a's ability
                  -- and its cost are one sentence, so the cost is what
                  -- distinguishes one instance from another.
                  flashback cost = CandidateCost.MkCandidateCost (Just (Keyword.Type.Flashback cost)) (withAdditional cost)
               in fmap flashback (Keyword.flashbackCosts keywords)
                    <> [CandidateCost.MkCandidateCost (Just Keyword.Type.Aftermath) printed | Keyword.hasAftermath keywords]
                    <> [ CandidateCost.MkCandidateCost
                           (Just Keyword.Type.JumpStart)
                           -- CR 702.133a's cost names no quality -- "discard a
                           -- card" -- so the criterion admits everything.
                           printed {Cost.components = Cost.components printed <> [CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))]}
                       | Keyword.hasJumpStart keywords
                       ]
                    -- UNTAGGED: an effect's permission states no cost, so
                    -- neither rule 702.34a's clause nor rule 702.133a's is
                    -- satisfied by paying it.
                    <> (if PlayerEffect.mayCastFromGraveyard (Object.owner obj) oid gs then fmap untagged (printed : alternatives) else [])
            -- CR 702.170d: a PLOTTED card is cast "without paying its mana
            -- cost", CR 118.9's alternative cost. INSTEAD of the printed cost,
            -- rule 702.170d being the only thing permitting this cast. The zone's
            -- other permissions (CR 715.3d, Effect.GrantPlayFromExile) state no
            -- cost and fall through to the `_` arm.
            Zone.Exile
              | Maybe.isJust (Object.plotted obj) -> [untagged (withoutPayingManaCost face)]
            -- CR 702.143a: a FORETOLD card is cast for its foretell cost, CR
            -- 118.9's alternative cost, wrapped by withAdditional as flashback's
            -- is. INSTEAD of the printed cost, the plotted arm's reason.
            --
            -- Not implemented: CR 702.143d's card foretold with NO foretell cost,
            -- unreachable from this module's own writer since CR 116.2h exiles
            -- only a card with foretell (#1486).
            Zone.Exile
              | Maybe.isJust (Object.foretold obj) ->
                  fmap (untagged . withAdditional) (Maybe.maybeToList (Keyword.foretellCost (Face.keywords face)))
            -- CR 118.9's other half, "applied to it from another effect", as a
            -- STANDING grant (Omniscience): a player-scoped alternative cost no
            -- per-card list can hold. APPENDED to the hand's ordinary list rather
            -- than replacing it, CR 118.9a letting the controller announce which
            -- single alternative they pay, and last so that `firstOffered` still
            -- reads the printed cost. Untagged, `untagged`'s reason.
            --
            -- CR 107.3b's "the only legal choice for X is 0" falls out rather
            -- than being enforced: withoutPayingManaCost carries an empty
            -- ManaCost, which has no variable to prompt for.
            Zone.Hand ->
              fmap untagged (printed : alternatives)
                <> [untagged (withoutPayingManaCost face) | PlayerEffect.mayCastFromHandWithoutPayingManaCost (Object.owner obj) oid gs]
            _ -> fmap untagged (printed : alternatives)
    Source.OfToken _ -> []
    Source.OfAbility _ _ -> []
    Source.OfTrigger _ _ -> []
    Source.OfEmblem _ -> []
    Source.OfInherentTrigger _ _ -> []

-- CR 601.2f: the mana or alternative cost, plus additional costs and increases,
-- minus reductions. `cost` arrives with X already substituted (CR 601.2b precedes
-- 601.2f), and the mana part alone is adjusted, every increase and reduction pawl
-- can express being an amount of MANA.
--
-- CR 601.2f's LOCK-IN is the CALLER's, not this function's: each caller totals
-- once per announcement and hands the VALUE to `pay`, which never re-reads the
-- state. Pawl.CostSpec's Altar's Reap group proves it -- the creature paying the
-- additional cost is the cost reducer, so a re-read after CR 601.2h's sacrifice
-- costs a mana more.
total :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Cost Keyword.Type.Keyword
total pid oid cost gs = totalWith (spellAdjustments pid oid gs) cost

-- CR 601.2f's increases and reductions for a SPELL being cast: the ones CARDS
-- generate (Pawl.Engine.PlayerEffect, plus the spell's own text through
-- selfReductions below) plus CR 903.8's commander tax. The tax joins the
-- INCREASES rather than the printed mana cost, rule 903.8 wording it "plus {2}
-- for each previous time", so a reduction still applies afterwards.
spellAdjustments :: PlayerId -> ObjectId -> GameState -> CostAdjustments.CostAdjustments
spellAdjustments pid oid gs =
  let adjustments = PlayerEffect.spellCostAdjustments pid oid gs
      withSelf =
        adjustments
          { CostAdjustments.reductions =
              -- Floored at zero and never confined to coloured mana: Thrasta's
              -- sentence states neither restriction, so CR 601.2f's own {0} and
              -- CR 118.7b-d's spill both stand.
              CostAdjustments.reductions adjustments
                <> fmap (\amount -> AppliedReduction.MkAppliedReduction amount 0 False) (selfReductions pid oid gs)
          }
      commanderTax = Commander.tax pid oid gs
   in if commanderTax == 0
        then withSelf
        else withSelf {CostAdjustments.increases = commanderTax : CostAdjustments.increases withSelf}

-- CR 601.2f / 113.6d: the reductions a spell's OWN printed text applies to its
-- own cost (Thrasta, Tempest's Roar), each Quantity evaluated and its amount
-- REPEATED that many times rather than multiplied, so a typed amount falls out
-- with no arithmetic. A NEGATIVE or UNDETERMINABLE Quantity contributes nothing,
-- the direction that leaves the spell dearer.
--
-- The face comes straight off the object rather than through a projection, for
-- Pawl.Types.CostReduction's reason (#1859): it is the half Cast.asProposed
-- already stamped (CR 709.3b).
selfReductions :: PlayerId -> ObjectId -> GameState -> [ManaCost.ManaCost]
selfReductions pid oid gs =
  let -- CR 109.5: the perspective is the would-be controller, `pid` -- not
      -- Projection.controllerOf, which answers Nothing for a card in a hand.
      -- The source is the spell itself, the reduction being printed on it.
      context = Filter.contextFor (Just pid) (Just oid)
      scaled reduction =
        let copies = Quantity.evaluate (Projection.fullView gs) context gs oid (CostReduction.perEach reduction)
            -- Saturating rather than partial: an Int cannot hold every Integer.
            -- A negative saturates to 0, the floor the header states.
            times n = concat (replicate (max 0 (Integer.toIntSaturating n)) (ManaCost.unwrap (CostReduction.amount reduction)))
         in fmap (ManaCost.MkManaCost . times) copies
   in case Game.faceOf oid gs of
        Nothing -> []
        Just face -> Maybe.mapMaybe scaled (Face.costReductions face)

-- CR 601.2f's adjustments for an ACTIVATION cost, which CR 602.2b routes
-- through rule 601.2b-i like a spell's. No commander tax: CR 903.8 taxes
-- CASTING a commander, and an activation is not a cast.
--
-- Not reached by a MANA ability's cost, which pays through manaActivations --
-- unobservably, every mana ability in `data/cards/` having an empty mana part
-- for a reduction to take from (#1120).
activationAdjustments :: PlayerId -> ObjectId -> GameState -> CostAdjustments.CostAdjustments
activationAdjustments = PlayerEffect.activationCostAdjustments

-- Every way CR 118.7e's choice could resolve the reductions that apply --
-- `announceReductions` below with the prompt replaced by the list of halves,
-- sharing `reductionHalvesOf` so the two cannot offer different ones. The GATE's
-- shape: nobody has been asked yet, so the honest question is whether SOME
-- resolution pays (CR 601.2f). One entry where no reduction holds a hybrid
-- symbol, at most 2^(hybrid symbols) otherwise.
adjustmentResolutions :: CostAdjustments.CostAdjustments -> [CostAdjustments.CostAdjustments]
adjustmentResolutions adjustments =
  let resolveOne symbol = case reductionHalvesOf symbol of
        Nothing -> [symbol]
        Just [] -> [symbol]
        Just halves -> halves
      resolveAll reduction =
        fmap
          (\xs -> reduction {AppliedReduction.amount = ManaCost.MkManaCost xs})
          (traverse resolveOne (ManaCost.unwrap (AppliedReduction.amount reduction)))
   in fmap
        (\reductions -> adjustments {CostAdjustments.reductions = reductions})
        (traverse resolveAll (CostAdjustments.reductions adjustments))

-- CR 601.2f's "if multiple cost reductions apply, the player may apply them in any
-- order", enumerated: each order the payer could pick paired with the total it
-- reaches, DEDUPLICATED by that total and CHEAPEST FIRST (CR 202.3's mana value is
-- the key, through Quantity.symbolValue).
--
-- Deduplicated because a fold's only observable IS the total -- applyAdjustments
-- answers a cost and nothing else -- so two orders reaching one total are one
-- outcome, and offering both would be a prompt with nothing to ask.
--
-- ONE entry, with no permutation enumerated at all, wherever every reduction states
-- the SAME RESTRICTIONS -- the same floor and the same answer to CR 101.1's
-- coloured-mana confinement. With one floor F a step is `\g -> if g >= F then
-- max (g - a) F else g`, and two of those commute -- max (max (g - b) F - a) F =
-- max (g - a - b) F, symmetric in a and b -- so the fold is order-free, and a
-- uniform confinement leaves each reducing symbol taking one mana by the same
-- rule wherever it sits in the order.
--
-- MIXED confinements genuinely do not commute, which is why the prune reads both
-- fields: against {1}{W}, Edgewalker's confined {W} applied first takes the white
-- symbol and leaves {1}, whereupon an unconfined {W} finds no white and CR 118.7b
-- spills it onto the {1}, for {0} -- run the other way the unconfined one takes
-- the white symbol and Edgewalker's half strands with nothing to do and nothing
-- to spill onto, for {1}. CR 601.2f makes that difference the payer's to choose,
-- so pruning it away would be the engine choosing.
--
-- The search branches over DISTINCT remaining reductions, so two copies of one
-- reducer cost nothing, and the uniform-restriction prune ends every tail; what is
-- left to enumerate is the interleaving of reductions that genuinely differ.
reductionOrders :: CostAdjustments.CostAdjustments -> ManaCost.ManaCost -> NonEmpty.NonEmpty (CostAdjustments.CostAdjustments, ManaCost.ManaCost)
reductionOrders adjustments manaCost =
  let restrictionsOf r = (AppliedReduction.atLeast r, AppliedReduction.coloredOnly r)
      uniform rs = case fmap restrictionsOf rs of
        [] -> True
        restrictions : rest -> all (== restrictions) rest
      orders rs =
        if uniform rs
          then [rs]
          else concatMap (\r -> fmap (r :) (orders (List.delete r rs))) (List.nub rs)
      withTotal order =
        let reordered = adjustments {CostAdjustments.reductions = order}
         in (reordered, applyAdjustments reordered manaCost)
      manaValue (_, ManaCost.MkManaCost symbols) = sum (fmap Quantity.symbolValue symbols)
      candidates = List.sortOn manaValue (fmap withTotal (orders (CostAdjustments.reductions adjustments)))
   in case List.nubBy (\x y -> snd x == snd y) candidates of
        entry : rest -> entry NonEmpty.:| rest
        -- Unreachable: `orders` answers at least one order for every list, the
        -- empty one included, so the deduplication has something to keep. Left
        -- rather than made partial, and the answer is the unreordered fold.
        [] -> (adjustments, applyAdjustments adjustments manaCost) NonEmpty.:| []

-- The same totalling over adjustments the CALLER already has, which is what CR
-- 118.7e's prompt needs: `announceReductions`' answers have to reach
-- applyAdjustments rather than being read out of the game state a second time.
totalWith :: CostAdjustments.CostAdjustments -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
totalWith adjustments cost = cost {Cost.mana = fmap (applyAdjustments adjustments) (Cost.mana cost)}

-- CR 601.2f's "plus all additional costs", the half that is not mana: the
-- components an effect ADDS to a cost, appended to the printed ones (Brutal
-- Suppression's "Sacrifice a land" onto a Rebel's activation cost, CR 602.2b).
--
-- SEPARATE from `totalWith` above, which is what keeps the components from being
-- added twice: the gate measures a cost before CR 601.2b's completion while
-- `totalWith` runs on the announced cost afterwards, and both need the
-- components, so this is applied at the earlier moment. APPENDED, so a printed
-- component is paid before an added one absent a payer's reordering (CR 601.2h);
-- LOYALTY components are merged instead by `combineLoyalty` here, CR 606.5
-- making them one cost, and this is the single funnel the gate and `pay` share.
--
-- The SCALE is cashed here, the only place CR 601.2f's addition meets the cost
-- it is added to. Drought counts CR 107.4a's coloured mana symbol through
-- Projection.symbolColors, so CR 107.4e's and CR 107.4f's count too, on THIS
-- cost before any reduction -- CR 601.2f's order, argued rather than tested.
--
-- Applied AFTER `substituteX`, so an ADDED component may not carry CR 601.2b's
-- X: a CostComponent.PayLifeX arriving this way would never be substituted and
-- `canPayComponent` would refuse the whole cost. A bound on the open half rather
-- than an elision.
plusComponents :: CostAdjustments.CostAdjustments -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
plusComponents adjustments cost =
  let symbols = foldMap ManaCost.unwrap (Cost.mana cost)
      repeats scale = case scale of
        CostScale.Once -> 1
        CostScale.PerColoredSymbol color -> length (filter (elem color . Projection.symbolColors) symbols)
      expand (scale, component) = replicate (repeats scale) component
      added = concatMap expand (CostAdjustments.components adjustments)
   in cost {Cost.components = combineLoyalty (Cost.components cost <> added)}

-- CR 601.2f's totalling of the MANA part alone, curried over one mana cost --
-- what `announce` and `canPaySomeCompletion` need of candidates that never
-- become a Cost of their own. MANY answers, because CR 118.7e leaves a choice
-- inside the reduction and CR 601.2f leaves the ORDER of the reductions outside
-- it, and this runs before either has been made: every total some pair of choices
-- reaches, and a caller asks `any` of them. Takes the ADJUSTMENTS rather than
-- gathering them, so the two moments CR 601.2f reaches share one totalling.
totalManas :: CostAdjustments.CostAdjustments -> ManaCost.ManaCost -> [ManaCost.ManaCost]
totalManas adjustments =
  let resolutions = adjustmentResolutions adjustments
   in \manaCost -> concatMap (NonEmpty.toList . fmap snd . (`reductionOrders` manaCost)) resolutions

-- CR 601.2f's ADDITIONAL-COSTS clause alone, bolted onto one candidate -- the
-- shape CR 702.42a's entwine needs. Applied to whichever candidate the caster
-- announced, per CR 118.9d. The mana parts CONCATENATE and the components are
-- appended in order; CR 118.6a leaves the whole thing unpayable if either side
-- is Nothing, which the applicative on Maybe gives free.
plus :: Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
plus base extra =
  let combine (ManaCost.MkManaCost xs) (ManaCost.MkManaCost ys) = ManaCost.MkManaCost (xs <> ys)
   in Cost.MkCost
        { Cost.mana = combine <$> Cost.mana base <*> Cost.mana extra,
          Cost.components = Cost.components base <> Cost.components extra
        }

-- CR 601.2b: substitute the chosen value of X everywhere in this cost -- the mana
-- part's ManaSymbol.Variable, and the components' CostComponent.PayLifeX. BOTH
-- halves, CR 107.3a giving one announced value to the whole cost, so Hatred's X
-- is the same X whichever half it sits in (CR 107.3i).
substituteX :: Natural -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
substituteX x cost =
  cost
    { Cost.mana = fmap (Mana.substituteX x) (Cost.mana cost),
      Cost.components = fmap (substituteXInComponent x) (Cost.components cost)
    }

-- EXHAUSTIVE with no wildcard, this module's posture for every CostComponent
-- match: a new component owes an answer here, and -Werror is what makes it.
substituteXInComponent :: Natural -> CostComponent.CostComponent Keyword.Type.Keyword -> CostComponent.CostComponent Keyword.Type.Keyword
substituteXInComponent x component = case component of
  CostComponent.PayLifeX -> CostComponent.PayLife x
  CostComponent.PayLife _ -> component
  CostComponent.TapThis -> component
  CostComponent.UntapThis -> component
  CostComponent.SacrificeThis -> component
  CostComponent.Sacrifice {} -> component
  CostComponent.TapForTotalPower {} -> component
  CostComponent.TapPermanents {} -> component
  CostComponent.DiscardCards {} -> component
  CostComponent.DiscardThis _ -> component
  CostComponent.PayEnergy _ -> component
  CostComponent.AddLoyaltyToThis _ -> component
  CostComponent.RemoveLoyaltyFromThis _ -> component
  CostComponent.PutPlusOneCountersOnThis _ -> component
  CostComponent.Blight _ -> component
  -- PayLifeX's rewrite one keyword action over: CR 107.3a gives ONE announced
  -- value to the whole cost, so Soul Immolation's "blight X" takes the same X a
  -- mana cost's {X} would have taken.
  CostComponent.BlightX -> CostComponent.Blight x
  CostComponent.ExileThisFromGraveyard -> component
  CostComponent.ExileCardsFromGraveyard {} -> component
  CostComponent.ExileTopFromGraveyard _ -> component

-- Does this cost contain an X (CR 107.3)? What decides whether the caster is
-- asked for a value at CR 601.2b. BOTH HALVES: CR 601.2b names the mana cost as
-- an EXAMPLE and CR 107.3a lists the additional cost beside it, so Hatred, whose
-- only X is in "pay X life", is asked exactly as Blaze is.
hasVariable :: Cost Keyword.Type.Keyword -> Bool
hasVariable cost = manaHasVariable cost || any componentHasVariable (Cost.components cost)

-- Does the MANA half of this cost carry CR 107.3's {X}? Nothing is CR 118.6's
-- unpayable cost, which declares nothing.
manaHasVariable :: Cost Keyword.Type.Keyword -> Bool
manaHasVariable cost = case Cost.mana cost of
  Nothing -> False
  Just (ManaCost.MkManaCost symbols) -> elem ManaSymbol.Variable symbols

-- substituteXInComponent's predicate half, and exhaustive for its reason. The
-- two must agree: a component this answers False for is one no announcement
-- will ever substitute.
componentHasVariable :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
componentHasVariable component = case component of
  CostComponent.PayLifeX -> True
  CostComponent.PayLife _ -> False
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.SacrificeThis -> False
  CostComponent.Sacrifice {} -> False
  CostComponent.TapForTotalPower {} -> False
  CostComponent.TapPermanents {} -> False
  CostComponent.DiscardCards {} -> False
  CostComponent.DiscardThis _ -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.AddLoyaltyToThis _ -> False
  CostComponent.RemoveLoyaltyFromThis _ -> False
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.Blight _ -> False
  CostComponent.BlightX -> True
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileCardsFromGraveyard {} -> False
  CostComponent.ExileTopFromGraveyard _ -> False

-- CR 601.2b: the greatest value of X this player could legally announce -- what
-- Prompt.ChooseX carries -- found by ASCENDING SEARCH from 0 over the caller's
-- own payability-at-X predicate, stopping at CR 101.1's card-stated ceiling if
-- the face prints one. Advisory, and nothing here clamps the ANSWER. The
-- predicate must be the SAME one the caller's own gate asked at X=0, so what a
-- gate measures and what a bound reports cannot drift apart.
--
-- SOUND only because payability is MONOTONE in X -- a property of the
-- PREDICATE, discharged at the call site. `substituteX` is what makes the demand
-- grow; Pawl.CostSpec's "Hatred is asked for X, bounded by the life its cost can
-- pay" stops running at all if the life half ever stops charging.
--
-- TERMINATING on either of two grounds, and a cost needs one of them:
-- `mCeiling`, or a demand that GROWS without bound (`demandGrowsWithX` below).
-- Neither is redundant -- Toxic Deluge's "pay X life" states no ceiling and is
-- stopped by CR 119.4's life total, while Soul Immolation's "blight X" is
-- payable at every X (rule 701.68b names no number) and is stopped only by its
-- own sentence. Pawl.CardSpec's "CR 101.1 every printing whose X the board
-- cannot refuse states a maximum for it" is what keeps a card with neither out
-- of the pool.
--
-- Answers 0 for a cost with no X, a totality guard.
greatestPayableX :: Maybe Natural -> (Natural -> Bool) -> Cost Keyword.Type.Keyword -> Natural
greatestPayableX mCeiling payableAt cost =
  let climb x
        | Just c <- mCeiling, x >= c = x
        | payableAt (x + 1) = climb (x + 1)
        | otherwise = x
   in if hasVariable cost then climb 0 else 0

-- Does a large enough X eventually make this cost UNPAYABLE? What decides
-- whether `greatestPayableX`'s ascending search needs CR 101.1's ceiling to
-- stop. NOT the same question as `hasVariable`: a cost can carry an X whose
-- demand never grows.
-- The mana half's two questions have ONE answer, which is CR 107.4b: {X} is a
-- generic symbol, so an announced X is that much more mana to find and a board
-- produces finitely much. `manaHasVariable` therefore answers both, and the
-- COMPONENTS are where the two questions come apart.
demandGrowsWithX :: Cost Keyword.Type.Keyword -> Bool
demandGrowsWithX cost = manaHasVariable cost || any componentDemandGrowsWithX (Cost.components cost)

-- `componentHasVariable`'s question sharpened, and exhaustive for its reason: a
-- new X-carrying component owes an answer here as well as there.
componentDemandGrowsWithX :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
componentDemandGrowsWithX component = case component of
  -- CR 119.4: payable only out of a life total at least that large, so a big
  -- enough X refuses.
  CostComponent.PayLifeX -> True
  -- FALSE, and that is CR 701.68b rather than an omission: the rule refuses a
  -- blight only where the player controls no creature, and names no number of
  -- counters that is too many. So a Soul Immolation announcement is refused by
  -- CR 101.1's sentence alone.
  CostComponent.BlightX -> False
  CostComponent.PayLife _ -> False
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.SacrificeThis -> False
  CostComponent.Sacrifice {} -> False
  CostComponent.TapForTotalPower {} -> False
  CostComponent.TapPermanents {} -> False
  CostComponent.DiscardCards {} -> False
  CostComponent.DiscardThis _ -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.AddLoyaltyToThis _ -> False
  CostComponent.RemoveLoyaltyFromThis _ -> False
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.Blight _ -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileCardsFromGraveyard {} -> False
  CostComponent.ExileTopFromGraveyard _ -> False

-- CR 101.1: the ceiling this face's own words put on CR 601.2b's announced X --
-- Soul Immolation's "X can't be greater than the greatest toughness among
-- creatures you control". Nothing where the face states none, which is every
-- other printing in `data/cards/`.
--
-- Evaluated ONCE, here, against the board as it stands at the announcement, and
-- never re-read: CR 601.2b names the value and no later rule revisits it, so a
-- creature that leaves in response does not shrink an X already announced.
--
-- `oid` is the spell on the stack (CR 601.2a has already moved it), which is
-- both the source the Quantity is evaluated against and CR 109.5's perspective
-- through `pid` -- selfReductions' pairing, one announcement step later.
--
-- A Quantity that does not evaluate, or evaluates NEGATIVE, floors at 0: CR
-- 101.2 makes the printed "can't" beat the permission, so an unreadable ceiling
-- refuses rather than permits.
maximumX :: PlayerId -> ObjectId -> Face.Face card -> GameState -> Maybe Natural
maximumX pid oid face gs =
  let context = Filter.contextFor (Just pid) (Just oid)
      ceilingOf quantity = Integer.toNaturalSaturating (Maybe.fromMaybe 0 (Quantity.evaluate (Projection.fullView gs) context gs oid quantity))
   in fmap ceilingOf (Face.maximumX face)

-- CR 118.13a: a mana symbol payable in multiple ways has its payment chosen as
-- the spell or ability is proposed (CR 601.2b) -- CR 107.4f's Phyrexian symbol
-- and both of CR 107.4e's hybrids -- one step before CR 601.2f's total.
--
-- The life the announcement committed becomes a CostComponent.PayLife, making
-- the returned cost CR 601.2b's "nonhybrid equivalent cost" in full (CR 107.4f,
-- CR 119.4). `lifeOwedBy`'s sum also goes IN, as the life this cost owes OUTSIDE
-- its mana part -- without it a route the player cannot afford gets offered.
--
-- `total` is CR 601.2f's totalling, the CALLER's to supply, and it must be the
-- SAME cost the caller's own gate measured: against the printed cost a reduction
-- could hide a route and this function elide the prompt. It answers a LIST
-- because CR 118.7e's choice of half is not made until CR 601.2f. `spending` is
-- CR 118.14's permission, here for the same reason.
announce :: Maybe ObjectId -> ManaSpending.ManaSpending -> PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> Game (Cost Keyword.Type.Keyword)
announce casting spending pid oid total_ cost = case Cost.mana cost of
  -- CR 118.6: an object with no mana cost has no mana symbols to announce.
  Nothing -> pure cost
  Just manaCost -> do
    -- The claims are read here rather than inside Mana.announce, which cannot
    -- reach claimOf -- this module imports that one, not the other way about.
    gs <- State.get
    (announced, life) <- Mana.announce casting manaActivations spending pid oid total_ (lifeOwedBy (Cost.components cost)) (claimsOf pid oid (Cost.components cost) gs) manaCost
    pure
      cost
        { Cost.mana = Just announced,
          Cost.components =
            Cost.components cost <> (if life > 0 then [CostComponent.PayLife life] else [])
        }

-- CR 118.7e: the payer chooses one half of each hybrid symbol in a reduction, as
-- the reduction is applied, and the answers come back as the adjustments
-- `totalWith` then applies. A SECOND SEAM rather than part of `announce` above,
-- which is CR 601.2b's announcement of the COST's symbols. The answer is the
-- nonhybrid symbol the chosen half resolves to, so what reaches applyAdjustments
-- holds no hybrid symbol -- which is why its Hybrid arms still take nothing.
--
-- NOT FILTERED BY PAYABILITY, unlike `announce`: CR 118.7e attaches no condition
-- to the choice, so a player may take the half that reduces nothing and strand a
-- payment the gate allowed on the strength of the other; CR 601.2h reverses it.
--
-- The INCREASES and the FLOOR ride through untouched: an increase is generic
-- mana with no halves, and a floor is a limit rather than an amount of mana.
--
-- CR 601.2f's OTHER choice rides here too, after the halves and for the same
-- reason -- "if multiple cost reductions apply, the player may apply them in any
-- order" is the payer's, and applying them in an order pawl picked would be the
-- engine making it. Asked as the TOTAL each order reaches (`reductionOrders`),
-- which is the whole of what an order does, and asked only where two totals differ:
-- a floored reduction beside an unfloored one on one cost is what separates them
-- (Heartstone and Blossoming Tortoise on an animated Mishra's Foundry), and the
-- cheapest is offered first so it is also the default a short transcript replays.
--
-- NOT FILTERED BY PAYABILITY, `chooseOne` above verbatim: the costlier order is a
-- legal choice CR 601.2f grants outright, and CR 601.2h reverses a payment it
-- strands.
--
-- Takes the ADJUSTMENTS the caller gathered, which is what makes the announced
-- reduction the one that will be applied, and the ANNOUNCED COST, which must be
-- the one `totalWith` is about to be handed: the order is chosen against the cost
-- it will be applied to, and a different cost could rank the orders differently.
announceReductions :: PlayerId -> ObjectId -> GameState -> Cost Keyword.Type.Keyword -> CostAdjustments.CostAdjustments -> Game CostAdjustments.CostAdjustments
announceReductions pid oid gs cost adjustments =
  let chooseOne symbol = case reductionHalvesOf symbol of
        -- Not a hybrid symbol, so CR 118.7e has nothing to ask about it.
        Nothing -> pure symbol
        -- Unreachable: reductionHalvesOf answers Just only where it has halves
        -- to offer. Left rather than made partial, and the symbol survives.
        Just [] -> pure symbol
        -- The degenerate `Hybrid t t`: both halves are the same symbol, so the
        -- answer cannot be observed and asking would be a prompt with one button.
        Just [only] -> pure only
        Just halves@(first : others) -> do
          answer <-
            Game.choose
              (Prompt.ChooseReductionHalf (Decide.deciderFor pid gs) pid oid symbol (first NonEmpty.:| others))
          -- FILTERED, NOT TRUSTED, the Mana.announce posture: an answer that is
          -- not one of the offered halves falls back to the first.
          pure (if elem answer halves then answer else first)
      chooseAll reduction =
        fmap
          (\xs -> reduction {AppliedReduction.amount = ManaCost.MkManaCost xs})
          (traverse chooseOne (ManaCost.unwrap (AppliedReduction.amount reduction)))
   in do
        halved <-
          fmap
            (\reductions -> adjustments {CostAdjustments.reductions = reductions})
            (traverse chooseAll (CostAdjustments.reductions adjustments))
        case Cost.mana cost of
          -- CR 118.6: an object with no mana cost has no generic component for a
          -- reduction to come off, so no order changes anything.
          Nothing -> pure halved
          Just manaCost -> case reductionOrders halved manaCost of
            -- One total, so every order CR 601.2f allows pays the same mana and
            -- the prompt would have one outcome. Elided, and the reductions keep
            -- the order they were gathered in.
            only NonEmpty.:| [] -> pure (fst only)
            first NonEmpty.:| others -> do
              answer <-
                Game.choose
                  (Prompt.ChooseReducedCost (Decide.deciderFor pid gs) pid oid (fmap snd (first NonEmpty.:| others)))
              -- FILTERED, NOT TRUSTED, `chooseOne`'s posture again: an answer that
              -- is not one of the offered totals falls back to the cheapest.
              pure (maybe (fst first) fst (List.find ((==) answer . snd) (first : others)))

-- CR 118.7e's "one half of that symbol", written as the reduction each half
-- would be: a coloured or colourless half is an OfType, a generic half a
-- Generic. Nothing for every symbol CR 107.4e does not call hybrid, and
-- DEDUPLICATED so the degenerate `Hybrid t t` offers one half, not two.
--
-- CR 107.4f's Phyrexian symbol is NOT here: CR 118.7f gives such a reduction one
-- mana of the symbol's colour with no choice, which reducingManaTypeOf reads
-- directly. That rule's ten HYBRID Phyrexian symbols would have a half to
-- choose, and Pawl.Types.ManaSymbol's Phyrexian carries a single Color.
reductionHalvesOf :: ManaSymbol.ManaSymbol -> Maybe [ManaSymbol.ManaSymbol]
reductionHalvesOf symbol = case symbol of
  ManaSymbol.Generic _ -> Nothing
  ManaSymbol.OfType _ -> Nothing
  ManaSymbol.Hybrid (Hybrid.MkHybrid a b) -> Just (List.nub [ManaSymbol.OfType a, ManaSymbol.OfType b])
  ManaSymbol.MonocoloredHybrid manaType ->
    Just [ManaSymbol.OfType manaType, ManaSymbol.Generic Mana.monocoloredHybridGeneric]
  ManaSymbol.Phyrexian _ -> Nothing
  ManaSymbol.Snow -> Nothing
  -- Unreachable, applyAdjustments' Variable arms: CR 601.2b precedes CR 601.2f,
  -- so no {X} survives into a total cost.
  ManaSymbol.Variable -> Nothing

-- CR 302.6: does paying this cost put the object's ability behind the
-- summoning-sickness gate? The CLASSIFICATION Pawl.Engine.Activate reads.
--
-- BOTH symbols, CR 302.6 naming CR 107.5's tap symbol and CR 107.6's untap one.
-- TapForTotalPower and TapPermanents are deliberately NOT here: those tap OTHER
-- permanents by written instruction, so a Vehicle that arrived this turn may be
-- crewed and a summoning-sick creature tapped for Springleaf Drum.
requiresSicknessCheck :: Cost Keyword.Type.Keyword -> Bool
requiresSicknessCheck cost =
  any (\c -> elem c (Cost.components cost)) [CostComponent.TapThis, CostComponent.UntapThis]

-- CR 302.6 asked of one activation COST, so an ability charging anything else is
-- not gated at all. The ONE reading, asked on both paths an activated ability
-- takes: Pawl.Engine.Activate for one that uses the stack, manaActivations below
-- for one that does not (CR 605.3b).
--
-- Reads PROJECTED creature-ness, so a plain land is never sick-gated and an
-- animated one is. Keyed to `pid`: CR 302.6 asks about THEIR control since THEIR
-- most recent turn began, so a settle recorded for anyone else does not answer it.
sicknessOkGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Bool
sicknessOkGiven pcs pid oid cost gs =
  not (requiresSicknessCheck cost)
    || not (Set.member CardType.Creature (Projection.cardTypesGiven pcs oid gs))
    || Summoning.settledOrHastyGiven pcs pid oid gs

-- CR 606.2: an activated ability with a loyalty symbol in its cost is a loyalty
-- ability. The CLASSIFICATION Pawl.Engine.Activate reads for CR 606.3's window
-- and once-per-turn limit. Derived from the cost rather than stored on the
-- ability -- CR 606.2 is a rule about what a cost CONTAINS and not a rider a
-- card prints, which is why Jace Beleren's abilities carry no
-- ActivationRestriction.SorcerySpeed.
isLoyaltyCost :: Cost Keyword.Type.Keyword -> Bool
isLoyaltyCost cost = any isLoyaltyComponent (Cost.components cost)

isLoyaltyComponent :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
isLoyaltyComponent = Maybe.isJust . loyaltyAmountOf

-- The SIGNED amount of loyalty a component moves, positive for CR 606.4's adding
-- half and negative for the removing one. `isLoyaltyComponent` above is this
-- without the number, so the two cannot drift apart. An Integer and not a
-- Natural: CR 606.5's combining sums the two halves against each other.
--
-- EXHAUSTIVE with no wildcard, `orderSensitive`'s posture and for its reason.
loyaltyAmountOf :: CostComponent.CostComponent Keyword.Type.Keyword -> Maybe Integer
loyaltyAmountOf component = case component of
  CostComponent.AddLoyaltyToThis n -> Just (toInteger n)
  CostComponent.RemoveLoyaltyFromThis n -> Just (negate (toInteger n))
  CostComponent.TapThis -> Nothing
  CostComponent.UntapThis -> Nothing
  CostComponent.SacrificeThis -> Nothing
  CostComponent.PayLife _ -> Nothing
  CostComponent.PayLifeX -> Nothing
  CostComponent.Sacrifice {} -> Nothing
  CostComponent.TapForTotalPower {} -> Nothing
  CostComponent.TapPermanents {} -> Nothing
  CostComponent.DiscardCards {} -> Nothing
  CostComponent.DiscardThis _ -> Nothing
  CostComponent.PayEnergy _ -> Nothing
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing
  CostComponent.Blight _ -> Nothing
  CostComponent.BlightX -> Nothing
  CostComponent.ExileThisFromGraveyard -> Nothing
  CostComponent.ExileCardsFromGraveyard {} -> Nothing
  CostComponent.ExileTopFromGraveyard _ -> Nothing

-- CR 606.5: multiple costs to add or remove loyalty counters are combined into a
-- single one. Carth the Lion's added [+1] on Jace Beleren's printed [-10] is one
-- cost of -9, which 9 loyalty pays -- where the pair asked separately is
-- refused, canPayComponent's CR 606.6 arm measuring each against the counters
-- present before any of the cost is paid.
--
-- ONE component whenever the cost had any, even at a net of zero, so that CR
-- 606.4's battlefield-and-control floor is still asked and `isLoyaltyCost` stays
-- true of the totalled cost. It takes the FIRST loyalty component's position, so
-- the printed order survives and CR 601.2h's prompt sees the list it saw before.
combineLoyalty :: [CostComponent.CostComponent Keyword.Type.Keyword] -> [CostComponent.CostComponent Keyword.Type.Keyword]
combineLoyalty components = case break isLoyaltyComponent components of
  (_, []) -> components
  (before, _ : after) ->
    let net = sum (Maybe.mapMaybe loyaltyAmountOf components)
        combined =
          if net < 0
            then CostComponent.RemoveLoyaltyFromThis (Integer.toNaturalSaturating (negate net))
            else CostComponent.AddLoyaltyToThis (Integer.toNaturalSaturating net)
     in before <> (combined : filter (not . isLoyaltyComponent) after)

-- CR 113.6m's COST half: an ability whose cost moves the object it's on out of a
-- particular zone functions only in that zone. The "or effect" half is
-- Pawl.Engine.EffectZone, and Activate.zoneFunctionedFrom joins them.
--
-- Nothing means the cost names no zone, leaving the effect half to answer and CR
-- 113.6's battlefield default otherwise -- SacrificeThis' answer too, CR 701.21a
-- moving the object off the battlefield where CR 113.6 already had it.
--
-- One zone and never a set (CR 113.6m: "a particular zone"). Two components
-- naming DIFFERENT zones would make the ability unpayable in either, so the
-- FIRST is the answer and a disagreement is a card-data error.
zoneFunctionedFrom :: Cost Keyword.Type.Keyword -> Maybe Zone.Zone
zoneFunctionedFrom cost = Maybe.listToMaybe (Maybe.mapMaybe zoneOfComponent (Cost.components cost))

zoneOfComponent :: CostComponent.CostComponent Keyword.Type.Keyword -> Maybe Zone.Zone
zoneOfComponent component = case component of
  -- CR 702.29a's "Discard this card": the hand, where cycling functions.
  -- LOAD-BEARING since CR 702.29b and CR 702.77b put the minted cycling and
  -- reinforce abilities into the projection for every zone -- this is what keeps
  -- a Rustic Clachan on the battlefield from offering its reinforce ability.
  CostComponent.DiscardThis _ -> Just Zone.Hand
  CostComponent.ExileThisFromGraveyard -> Just Zone.Graveyard
  CostComponent.TapThis -> Nothing
  CostComponent.UntapThis -> Nothing
  CostComponent.SacrificeThis -> Nothing
  CostComponent.PayLife _ -> Nothing
  CostComponent.PayLifeX -> Nothing
  CostComponent.Sacrifice {} -> Nothing
  -- These tap permanents that stay on the battlefield, so nothing moves out of
  -- any zone and CR 113.6's default stands.
  CostComponent.TapForTotalPower {} -> Nothing
  CostComponent.TapPermanents {} -> Nothing
  -- Nothing, and NOT Just Zone.Graveyard: CR 113.6m is about an ability that
  -- moves THE OBJECT IT'S ON, and these move OTHER cards.
  CostComponent.ExileCardsFromGraveyard {} -> Nothing
  CostComponent.ExileTopFromGraveyard _ -> Nothing
  CostComponent.DiscardCards {} -> Nothing
  CostComponent.PayEnergy _ -> Nothing
  CostComponent.AddLoyaltyToThis _ -> Nothing
  CostComponent.RemoveLoyaltyFromThis _ -> Nothing
  -- CR 122.6 puts counters on a permanent already where it is, so nothing moves
  -- out of any zone.
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing
  CostComponent.Blight _ -> Nothing
  CostComponent.BlightX -> Nothing

-- CR 118.8c: does this cost include "actions involving cards with a stated
-- quality in a hidden zone"? What Resolve.offerCast reads to decide whether a
-- cast an effect INSTRUCTS "if able" is excused. Two conjuncts, BOTH required:
-- the zone must be hidden (CR 400.2 makes only library and hand so), and the
-- cards must be described by a STATED QUALITY rather than a bare quantity --
-- Filter.statesAQuality, written for CR 701.23b/701.23d's identical phrase. NOT
-- zoneOfComponent, which answers CR 113.6m's different question.
--
-- EXHAUSTIVE with no wildcard, loyaltyAmountOf's posture.
statesHiddenQuality :: Cost Keyword.Type.Keyword -> Bool
statesHiddenQuality cost = any componentStatesHiddenQuality (Cost.components cost)

componentStatesHiddenQuality :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
componentStatesHiddenQuality component = case component of
  -- The one True-capable arm: CR 701.9a discards from the HAND, CR 400.2's
  -- hidden zone, and the criterion is the rule's stated quality -- Magmatic
  -- Insight's "discard a land card" states one, Cathartic Reunion's "discard two
  -- cards" does not.
  CostComponent.DiscardCards d -> Filter.statesAQuality (DiscardCards.whichCards d)
  -- The hidden zone WITHOUT a quality: CR 702.29a names the object the cost is
  -- on, so no card is described and the player has none to fail to find.
  CostComponent.DiscardThis _ -> False
  -- Cards, but in a PUBLIC zone (CR 400.2), so the first conjunct fails however
  -- specific the filter is: the battlefield, then the graveyard.
  CostComponent.Sacrifice {} -> False
  CostComponent.TapForTotalPower {} -> False
  CostComponent.TapPermanents {} -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileCardsFromGraveyard {} -> False
  CostComponent.ExileTopFromGraveyard _ -> False
  -- No cards at all, so there is no "action involving cards" to classify.
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.SacrificeThis -> False
  CostComponent.PayLife _ -> False
  CostComponent.PayLifeX -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.AddLoyaltyToThis _ -> False
  CostComponent.RemoveLoyaltyFromThis _ -> False
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.Blight _ -> False
  CostComponent.BlightX -> False

-- CR 306.5c: a planeswalker's loyalty is the number of loyalty counters on it.
-- Zero for an object with none, which CR 704.5i reads as loyalty 0 -- so this is
-- only ever asked of something already known to be a planeswalker.
loyaltyCountersOn :: ObjectId -> GameState -> Natural
loyaltyCountersOn oid gs =
  maybe 0 (Map.findWithDefault 0 CounterKind.Loyalty . Object.counters) (Game.lookupObject oid gs)

addLoyalty :: Natural -> Object.Object -> Object.Object
addLoyalty n obj = obj {Object.counters = Map.insertWith (+) CounterKind.Loyalty n (Object.counters obj)}

-- The cards this player may discard to pay a cost on `oid`: their hand, in its
-- own order, narrowed by the criterion and minus `oid` itself -- see
-- canPayComponent's DiscardCards arm for why that exclusion is CR 601.2a.
--
-- Matched through the card's own CR 613 projection: rule 613.1 names no zone, so
-- a card in a hand is folded exactly as a permanent is, and Putrid Raptor's
-- "discard a Zombie card" morph cost is payable with a creature card printed as
-- something else under Maskwood Nexus (Pawl.CostSpec's Putrid Raptor pair).
discardCandidates :: PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
discardCandidates pid oid criterion gs =
  let context = Filter.contextFor (Just pid) Nothing
      matches candidate = Filter.matches context (Projection.viewOfObject candidate gs) criterion
   in filter (\candidate -> candidate /= oid && matches candidate) (Game.zoneMembers Zone.Hand pid gs)

-- The cards this player may exile to pay an ExileCardsFromGraveyard component:
-- their OWN graveyard, in its own order, narrowed by the criterion. Per-owner by
-- CR 400.3 with CR 108.4, a card in a graveyard having no controller.
--
-- Matched through the card's own CR 613 projection, discardCandidates' reading;
-- CR 208.2a's characteristic-defining power rides along at layer 7a
-- (Pawl.CostSpec's Everbark Shaman and Frail Exhumation cases). Every member of
-- this pool has a face: CR 111.7 with CR 704.5d makes a token in a graveyard
-- cease to exist, and an ability exists only on the stack (CR 113.7a).
--
-- No `oid` exclusion, unlike discardCandidates above, and none is owed: CR
-- 601.2a has put the spell being cast on the STACK.
exileCandidates :: PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
exileCandidates pid criterion gs =
  let context = Filter.contextFor (Just pid) Nothing
      matches candidate = Filter.matches context (Projection.viewOfObject candidate gs) criterion
   in filter matches (Game.zoneMembers Zone.Graveyard pid gs)

-- The one card an ExileTopFromGraveyard component takes: the TOP matching card
-- of this player's graveyard, or Nothing where it holds none.
--
-- The LAST of exileCandidates' answer is the top: CR 404.1 puts an arrival on
-- top and Game.insertIntoZone appends, the opposite end from a library. No
-- prompt, and that is CR 404.2 rather than an elision: a graveyard's order is not
-- the player's to change, so "the top creature card" names exactly one.
topExileCandidate :: PlayerId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> Maybe ObjectId
topExileCandidate pid criterion gs =
  Maybe.listToMaybe (reverse (exileCandidates pid criterion gs))

-- The permanents this player may tap to pay a TapForTotalPower or TapPermanents
-- component on `oid`: every battlefield object matching the criterion, ascending.
--
-- NOT Replacement.sacrificeCandidates, and the difference is the CONTEXT: that
-- one matches with no perspective and pre-narrows to `Projection.controls pid`,
-- where CR 702.122a's criterion needs the atoms that throws away. So the
-- perspective is the PAYER and the source is the permanent whose ability is being
-- paid for -- without it a Vehicle that has already become a creature could crew
-- itself.
tapCandidates :: PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
tapCandidates pid oid criterion gs =
  let context = Filter.contextFor (Just pid) (Just oid)
      matches candidate =
        Filter.matches context (Projection.viewOfObject candidate gs) criterion
   in List.sort (filter matches (Set.toList (GameState.battlefield gs)))

-- The power a candidate contributes to CR 702.122a's total. Zero for a permanent
-- with no power at all, which after CR 208.3 is every noncreature one.
tapPower :: ObjectId -> GameState -> Integer
tapPower candidate gs = Maybe.fromMaybe 0 (Projection.powerOf candidate gs)

-- CR 701.26a: tap one permanent. A direct edit and not a funnel -- see
-- payComponent's TapThis arm -- shared by every component that taps, so an
-- Event.tap would have one call site to move.
tapObject :: ObjectId -> Game ()
tapObject target =
  State.modify'
    (\gs -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) target (GameState.objects gs)})

-- What this component SPENDS out of a pool of objects: which resource it draws
-- on (Pawl.Types.ClaimAxis), which objects are in that pool, and how many it
-- claims. Nothing for a component that spends no object.
--
-- TAPPING IS NOT A REMOVAL, a rules fact rather than a scope cut: a tapped
-- permanent is still on the battlefield (CR 601.2h, CR 118.11), so keying its
-- claim as a Removal from Zone.Battlefield would merge it with Sacrifice's pool
-- and REFUSE costs the rules allow. What it spends is UNTAPPED-ness, CR 118.3's
-- own example of scarcity, hence ClaimAxis.Tapping.
--
-- The ZONE alone keys a Removal soundly even though a hand and a graveyard are
-- per-player (CR 400.3, CR 108.4): every such claim below is on `pid`'s own copy.
-- A `*This` arm whose own guard fails answers an EMPTY pool rather than Nothing,
-- which keeps this in agreement with canPayComponent.
--
-- EXHAUSTIVE with no wildcard, this module's posture, and -Werror makes it.
claimOf :: PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> GameState -> Maybe Claim
claimOf pid oid component gs = case component of
  -- CR 701.21a: the permanents this player controls that match the criterion.
  CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) ->
    claim (ClaimAxis.Removal Zone.Battlefield) (Set.fromList (Replacement.sacrificeCandidates pid (Just oid) criterion gs)) n
  CostComponent.SacrificeThis ->
    claim
      (ClaimAxis.Removal Zone.Battlefield)
      -- CR 101.2's prohibition, as canPayComponent reads it below; the two
      -- answers have to agree.
      ( itself
          ( Set.member oid (GameState.battlefield gs)
              && Projection.controllerOf oid gs == Just pid
              && not (SacrificeRestriction.prohibited oid gs)
          )
      )
      1
  CostComponent.DiscardCards (DiscardCards.MkDiscardCards n criterion) ->
    claim (ClaimAxis.Removal Zone.Hand) (Set.fromList (discardCandidates pid oid criterion gs)) n
  CostComponent.DiscardThis _ -> claim (ClaimAxis.Removal Zone.Hand) (itself (isOwnedIn Zone.Hand)) 1
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) ->
    claim (ClaimAxis.Removal Zone.Graveyard) (Set.fromList (exileCandidates pid criterion gs)) n
  -- A pool of at most ONE, CR 404.2's order having picked it.
  CostComponent.ExileTopFromGraveyard criterion ->
    claim (ClaimAxis.Removal Zone.Graveyard) (Set.fromList (Maybe.maybeToList (topExileCandidate pid criterion gs))) 1
  CostComponent.ExileThisFromGraveyard -> claim (ClaimAxis.Removal Zone.Graveyard) (itself (isOwnedIn Zone.Graveyard)) 1
  -- CR 107.5: {T} spends exactly the untapped-ness the TapPermanents arm below
  -- claims, so it is the same axis, on a pool of one.
  CostComponent.TapThis ->
    claim
      ClaimAxis.Tapping
      -- canPayComponent's own guard for this component, read below; the two
      -- answers have to agree.
      ( itself
          ( Set.member oid (GameState.battlefield gs)
              && fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Untapped
          )
      )
      1
  -- Nothing, and no printing can observe it: CR 107.6's {Q} spends TAPPED-ness,
  -- a third axis, and names the object the cost is on, so two such claims could
  -- only come from one cost carrying {Q} twice.
  CostComponent.UntapThis -> Nothing
  -- ONE, and deliberately not the Natural: that number is a THRESHOLD on an
  -- aggregate rather than a count of objects, so how many permanents a payment
  -- taps is not settled until the payer picks them. A threshold above 0 needs
  -- some permanent of positive power (canPayComponent below), so one is a LOWER
  -- BOUND on what the payment taps and can never over-refuse; a threshold of 0 is
  -- paid by the empty set, taps nothing and claims nothing.
  --
  -- The pool is tapCandidates', TapPermanents' below: tapped candidates included,
  -- the same permissive reading and for its reason. CR 702.122a's own criterion
  -- excludes them (Pawl.Engine.Keyword's crew), so a crew cost's pool is the
  -- untapped creatures exactly.
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower threshold criterion)
    | threshold > 0 -> claim ClaimAxis.Tapping (Set.fromList (tapCandidates pid oid criterion gs)) 1
    | otherwise -> Nothing
  -- CR 601.2f's "tapping permanents", on the TAPPING axis rather than a zone's,
  -- for the header's reason. ManaSpec's "a creature tapped for mana can still be
  -- sacrificed" is the case that proves the axes stay apart.
  --
  -- The pool is every candidate the criterion admits, tapped ones included --
  -- the PERMISSIVE reading where a criterion omits "untapped", unobservable
  -- since an already-tapped candidate spends no untapped-ness.
  CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion) ->
    claim ClaimAxis.Tapping (Set.fromList (tapCandidates pid oid criterion gs)) n
  CostComponent.PayLife _ -> Nothing
  CostComponent.PayLifeX -> Nothing
  CostComponent.PayEnergy _ -> Nothing
  CostComponent.AddLoyaltyToThis _ -> Nothing
  CostComponent.RemoveLoyaltyFromThis _ -> Nothing
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing
  -- Nothing, though this one DOES pick an object out of a pool: CR 701.68a takes
  -- nothing out of a zone. Two blights in one cost may choose the same creature,
  -- which is right -- CR 122.6 stacks counters.
  CostComponent.Blight _ -> Nothing
  CostComponent.BlightX -> Nothing
  where
    claim a p n = Just (Claim.Type.MkClaim {Claim.Type.axis = a, Claim.Type.pool = p, Claim.Type.count = n})
    itself condition = if condition then Set.singleton oid else Set.empty
    -- canPayComponent's own guard for the two `*This` arms that read a zone
    -- rather than control: CR 108.4 gives a card outside the battlefield no
    -- controller, and CR 400.3 puts it in its OWNER's zone.
    isOwnedIn zone = case Game.lookupObject oid gs of
      Nothing -> False
      Just obj -> Object.zone obj == zone && Object.owner obj == pid

-- CR 118.3's "fully", asked of a cost's components TOGETHER rather than one at a
-- time: CR 601.2h pays them in any order, so the question is whether SOME
-- assignment of distinct objects pays every one in full. Jarad, Golgari Lich
-- Lord's "Sacrifice a Swamp and a Forest" beside one Bayou tells the two readings
-- apart. Pawl.Engine.Claim.satisfiable carries the per-axis grouping and Hall's
-- condition; this module's part is which components claim, and on which axis.
jointlyPayable :: PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> GameState -> Bool
jointlyPayable pid oid components gs = Claim.satisfiable (claimsOf pid oid components gs)

-- Everything these components will spend out of a pool of objects, on whichever
-- axis each spends it (Pawl.Types.ClaimAxis) -- what `jointlyPayable` asks
-- Hall's condition of, and what the MANA side is handed to ask it of these
-- claims and its sources' together.
claimsOf :: PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> GameState -> [Claim]
claimsOf pid oid components gs = Maybe.mapMaybe (\component -> claimOf pid oid component gs) components

-- CR 118.3: a player can't pay a cost without the resources to pay it fully. The
-- mana part AND every component, measured against the CURRENT state, before any
-- part is paid -- CR 601.2g gives the mana window BEFORE CR 601.2h's payment, so
-- a Mountain tapped for mana is still there to be sacrificed afterwards.
--
-- LIFE is measured across the two halves rather than within each, CR 107.4f's
-- Phyrexian symbol being the only MANA symbol that spends life: measured
-- separately, {G/P} plus "pay 2 life" reads as payable at 3. OBJECTS go the same
-- way, through `jointlyPayable` -- and CR 118.10 is NOT that rule, governing two
-- DIFFERENT spells each paying its own cost. Both are handed ACROSS the halves,
-- since a Phyrexian Tower tapped for {B} has already eaten the creature Village
-- Rites' additional cost then wants.
canPay :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPay pid oid cost gs = case Cost.mana cost of
  Nothing -> False
  Just manaCost ->
    -- CR 118.14's permission is a CAST's, and no caller of this one is casting --
    -- what reaches here is a special action's cost and CR 118.12's
    -- resolution-time payment -- so the mana is spent as it is, and CR
    -- 106.6-restricted mana is no supply for any of them (`casting` is Nothing).
    Mana.canPayCommitting Nothing manaActivations ManaSpending.AsProduced pid (lifeOwedBy (Cost.components cost)) (claimsOf pid oid (Cost.components cost) gs) manaCost gs
      && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)
      && jointlyPayable pid oid (Cost.components cost) gs

-- How many times may this player activate this mana ability, right now, and what
-- does one activation spend? CR 605.3b keeps a mana ability off the stack, so
-- nothing here comes from Activate.activatable and every restriction that window
-- applies has to be applied here instead.
--
-- Two restrictions, both read off the ability's OWN activation cost (CR 602.2b):
-- CR 118.3's payability and CR 302.6's settle. NEITHER is a fact about the
-- permanent alone -- CR 107.5 bars a tapped permanent from paying {T} and says
-- nothing about a cost without one, and CR 302.6 gates only a cost holding {T}
-- or {Q} -- so both are asked per ROUTE rather than of the source.
--
-- `canPay` above without its mana half, the supply walk being what asks this and
-- asking back not terminating. The MANA part is read only for CR 118.6, exact
-- for a pool in which every mana ability's mana part is empty (#1120).
--
-- The CLAIMS and the LIFE ride along with the count, which alone is a fact about
-- this source in isolation: two sources whose costs both sacrifice a creature
-- each answer 1 beside one creature. Both are ONE activation's, unscaled.
manaActivations :: Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Activations.Activations
manaActivations pcs pid oid cost gs =
  if Maybe.isJust (Cost.mana cost)
    && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)
    && jointlyPayable pid oid (Cost.components cost) gs
    && sicknessOkGiven pcs pid oid cost gs
    -- CR 701.35a's "its activated abilities can't be activated", which reaches a
    -- mana ability too. Here rather than in Mana.manaSourcesGiven, because this
    -- is what BOTH of CR 605.3a's windows consult -- sickness's position above.
    && not (Detain.detained oid gs)
    then
      Activations.MkActivations
        { Activations.times = repeatsOf pid oid cost gs,
          Activations.claims = claimsOf pid oid (Cost.components cost) gs,
          Activations.life = lifeOwedBy (Cost.components cost)
        }
    else Activations.MkActivations {Activations.times = 0, Activations.claims = [], Activations.life = 0}

-- How many times IN A ROW a cost already known to be payable once could be paid
-- -- what makes Ashnod's Altar beside two creatures two mana activations, and
-- Treasonous Ogre ("Pay 3 life: Add {R}") at 20 life six.
--
-- The SMALLEST ceiling the cost's resources impose (CR 118.3's "fully"). Two are
-- counted, each totalled over the WHOLE cost: OBJECTS, through
-- Pawl.Engine.Claim.repeats, so two components drawing on one pool do not each
-- get it; and LIFE, through CR 119.4, so the ceiling is the life total divided
-- by `lifeOwedBy`.
--
-- Anything else caps the answer at 1 (`uncountedCeiling`), and so does a MANA
-- part, repeating which would spend mana this walk has not measured (#1120).
-- Understating is the safe direction, and why the uncounted components cap
-- rather than divide: a supply too large offers a cast that then cannot be paid,
-- and an offer that changes nothing is offered again forever.
--
-- Both ceilings are this source asked ALONE against the untouched board; where
-- something else IS spending they are loose, and Pawl.Engine.Mana's joint
-- question across sources tightens them.
repeatsOf :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Natural
repeatsOf pid oid cost gs = case Cost.mana cost of
  Just (ManaCost.MkManaCost []) -> case ceilings of
    [] -> 1
    limits -> minimum limits
  _ -> 1
  where
    components = Cost.components cost
    claims = claimsOf pid oid components gs
    objectCeiling = if null claims then [] else [Claim.repeats claims]
    lifeCeiling = case lifeOwedBy components of
      0 -> []
      owed -> [div (lifeTotalOf pid gs) owed]
    ceilings = objectCeiling <> lifeCeiling <> Maybe.mapMaybe uncountedCeiling components

-- The ceiling ONE component imposes that `repeatsOf`'s two totals do not already
-- carry, or Nothing where one of them does. 1 for every resource this module
-- cannot count, and for two it need not. EXACT
-- for CR 107.5's {T}, CR 107.6's {Q} and CR 606.4's loyalty (CR 606.3 allows one
-- loyalty ability per turn whatever the counters allow); an UNDERSTATEMENT for
-- CR 107.14's energy, for a counter put on the source, and for the two
-- components that tap OTHER permanents (#1280).
--
-- EXHAUSTIVE with no wildcard, this module's posture, and -Werror makes it.
uncountedCeiling :: CostComponent.CostComponent Keyword.Type.Keyword -> Maybe Natural
uncountedCeiling component = case component of
  -- Counted by `objectCeiling`. TapPermanents states a claim too and is still
  -- capped at 1 below, for the header's understatement reason.
  CostComponent.Sacrifice {} -> Nothing
  CostComponent.SacrificeThis -> Nothing
  CostComponent.DiscardCards {} -> Nothing
  CostComponent.DiscardThis _ -> Nothing
  CostComponent.ExileCardsFromGraveyard {} -> Nothing
  CostComponent.ExileTopFromGraveyard _ -> Nothing
  CostComponent.ExileThisFromGraveyard -> Nothing
  -- Counted by `lifeCeiling`, CR 119.4.
  CostComponent.PayLife _ -> Nothing
  -- Zero, not the 1 the uncounted components take: an unannounced X cannot be
  -- paid even once (`canPayComponent`). Unreachable, since `manaActivations`
  -- asks canPayComponent of every component before reaching `repeatsOf`.
  CostComponent.PayLifeX -> Just 0
  CostComponent.TapThis -> Just 1
  CostComponent.UntapThis -> Just 1
  CostComponent.TapForTotalPower {} -> Just 1
  CostComponent.TapPermanents {} -> Just 1
  CostComponent.PayEnergy _ -> Just 1
  CostComponent.AddLoyaltyToThis _ -> Just 1
  CostComponent.RemoveLoyaltyFromThis _ -> Just 1
  CostComponent.PutPlusOneCountersOnThis _ -> Just 1
  -- An UNDERSTATEMENT: a player controlling a creature can blight as often as
  -- they can pay the rest of the cost.
  CostComponent.Blight _ -> Just 1
  -- Zero, PayLifeX's answer above and for its reason: an unannounced X cannot be
  -- paid even once.
  CostComponent.BlightX -> Just 0

-- This player's life total as an amount that could be PAID (CR 119.4), floored
-- at zero: a player at or below 0 life can pay nothing but CR 119.4b's zero.
lifeTotalOf :: PlayerId -> GameState -> Natural
lifeTotalOf pid gs = case Map.lookup pid (GameState.players gs) of
  Nothing -> 0
  Just player -> Integer.toNaturalSaturating (Player.life player)

-- CR 118.3 asked one step later than `canPay` asks it: is SOME nonhybrid
-- equivalent of this cost (CR 601.2b) payable, measured at CR 601.2f's total?
-- The castability / activatability gate's question, and the same predicate
-- Mana.announce's `stillPayable` asks of the routes it offers.
--
-- The COMPLETION has to come first: CR 118.7a's reductions come off the generic
-- mana component, and a symbol still spelled {2/R} has none for them to bite, so
-- totalling the printed cost loses the reduction CR 601.2b's {2}{2}{2}
-- announcement would have exposed -- Flame Javelin's ruling read backwards.
--
-- LIFE is threaded, not dropped: `completions` returns the life each route
-- commits (CR 107.4f's 2), having already removed the symbol that commits it, so
-- nothing double-counts and CR 118.3 makes it one demand with the components'.
-- `total` answers MANY totals, one per CR 118.7e resolution, and this asks `any`
-- of them: a cost this gate refuses has to be one NO half could have paid (#595).
canPaySomeCompletion :: Maybe ObjectId -> ManaSpending.ManaSpending -> PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPaySomeCompletion casting spending pid oid total_ cost gs =
  let pcs = Projection.projectAll gs
   in canPaySomeCompletionGiven casting spending (activationManaSourcesGiven (Projection.controlGrants gs) pcs pid gs) pcs pid oid total_ cost gs

-- The mana sources an ACTIVATION payment is judged against. ONE function pairing
-- `manaActivations` with the sweep taken under it, so a hoisted list cannot be
-- built under a capacity the gate does not read -- the invariant
-- Mana.payableResolutionsGiven states and its type cannot.
activationManaSourcesGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
activationManaSourcesGiven = Mana.manaSourcesGiven manaActivations

-- The same question given a board the CALLER has already walked; handing the
-- board in changes no answer. Build `sources` with activationManaSourcesGiven
-- above and nothing else. ONLY the mana half gets the pre-walked board -- the
-- COMPONENTS are still asked through canPayComponent, whose Sacrifice and
-- TapForTotalPower arms make per-object walks of their own (#1448).
canPaySomeCompletionGiven :: Maybe ObjectId -> ManaSpending.ManaSpending -> [ObjectId] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPaySomeCompletionGiven casting spending sources pcs pid oid total_ cost gs = case Cost.mana cost of
  Nothing -> False
  Just (ManaCost.MkManaCost symbols) ->
    let outside = lifeOwedBy (Cost.components cost)
        claimed = claimsOf pid oid (Cost.components cost) gs
        payable (completed, life) =
          any
            (\totalled -> Mana.canPayCommittingGiven casting manaActivations spending sources pcs pid (outside + life) claimed totalled gs)
            (total_ (ManaCost.MkManaCost completed))
     in any payable (Mana.completions symbols)
          && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)
          && jointlyPayable pid oid (Cost.components cost) gs

-- CR 119.4's payments a cost owes OUTSIDE its mana part, added up -- what CR
-- 118.3 makes the mana part's own life share a total with. Total, so a new
-- life-spending component cannot be added without answering here.
lifeOwedBy :: [CostComponent.CostComponent Keyword.Type.Keyword] -> Natural
lifeOwedBy = sum . fmap lifeOwedByComponent

lifeOwedByComponent :: CostComponent.CostComponent Keyword.Type.Keyword -> Natural
lifeOwedByComponent component = case component of
  CostComponent.PayLife n -> n
  -- 0, an unannounced X naming no amount to owe. Not a claim that this component
  -- is free: `canPayComponent` refuses it outright.
  CostComponent.PayLifeX -> 0
  CostComponent.TapThis -> 0
  CostComponent.UntapThis -> 0
  CostComponent.SacrificeThis -> 0
  CostComponent.Sacrifice {} -> 0
  CostComponent.TapForTotalPower {} -> 0
  CostComponent.TapPermanents {} -> 0
  CostComponent.DiscardCards {} -> 0
  CostComponent.DiscardThis _ -> 0
  CostComponent.PayEnergy _ -> 0
  CostComponent.AddLoyaltyToThis _ -> 0
  CostComponent.RemoveLoyaltyFromThis _ -> 0
  CostComponent.PutPlusOneCountersOnThis _ -> 0
  CostComponent.Blight _ -> 0
  CostComponent.BlightX -> 0
  CostComponent.ExileThisFromGraveyard -> 0
  CostComponent.ExileCardsFromGraveyard {} -> 0
  CostComponent.ExileTopFromGraveyard _ -> 0

canPayComponent :: PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> GameState -> Bool
canPayComponent pid oid component gs = case component of
  -- CR 107.5: a permanent that's already tapped can't be tapped again to pay the
  -- cost.
  CostComponent.TapThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Set.member oid (GameState.battlefield gs) && Object.tapped obj == TapState.Untapped
  -- CR 107.6: the exact mirror of TapThis above.
  CostComponent.UntapThis -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Set.member oid (GameState.battlefield gs) && Object.tapped obj == TapState.Tapped
  -- CR 701.21a: only a permanent, and only one this player controls -- and CR
  -- 101.2, only one no effect says can't be sacrificed. Read here and not left to
  -- the funnel, a cost announced as payable and then unpayable spending an
  -- activation for nothing (CR 118.3). `claimOf` must agree.
  CostComponent.SacrificeThis ->
    Set.member oid (GameState.battlefield gs)
      && Projection.controllerOf oid gs == Just pid
      && not (SacrificeRestriction.prohibited oid gs)
  -- CR 119.4: payable only if the life total is at least the amount. This
  -- component ALONE, which is not CR 118.3's question -- canPay hands
  -- `lifeOwedBy`'s sum to the mana side, and this can only be the weaker check.
  CostComponent.PayLife n -> Event.canPayLife pid n gs
  -- CR 601.2b: this is the component BEFORE X is announced, so there is no
  -- amount to measure against CR 119.4 -- CR 601.2 reverses a casting a player
  -- cannot comply with rather than choosing a value for them. Unreachable from
  -- either cast path, both of which substitute before they measure or pay; a
  -- fence, with Pawl.CostSpec's "an unannounced X is unpayable" as the test.
  CostComponent.PayLifeX -> False
  -- CR 701.21a: this player must control at least `n` matching permanents. This
  -- component ALONE, PayLife's caveat -- two Sacrifice components of one cost can
  -- each find the same permanent here, and `jointlyPayable` asks them together.
  CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) ->
    Natural.length (Replacement.sacrificeCandidates pid (Just oid) criterion gs) >= n
  -- CR 702.122a: payable iff SOME subset of the candidates reaches the
  -- threshold, decided without enumerating one -- the greatest total any subset
  -- can reach is the sum of the candidates' POSITIVE powers, since adding one of
  -- power 0 or less cannot raise it and the player is never obliged to. Exact
  -- rather than a bound, and `>=` because CR 702.122a says "or greater". A
  -- threshold of 0 is payable by the empty set, with no special case.
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n criterion) ->
    sum (fmap (max 0 . (`tapPower` gs)) (tapCandidates pid oid criterion gs)) >= toInteger n
  -- Sacrifice's arm read over tapping: the count is HOW MANY, so the question is
  -- a size and not a sum. "Untapped" is not asked here and is not missing -- CR
  -- 107.5's exclusion is not this component's, so a card that wants it prints it
  -- (Springleaf Drum's criterion carries `Not IsTapped`). This component ALONE,
  -- Sacrifice's caveat; ManaSpec's "one creature cannot pay for both Drums" is
  -- the test that `jointlyPayable` asks them together.
  CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion) ->
    Natural.length (tapCandidates pid oid criterion gs) >= n
  -- CR 601.2f: payable only if the hand holds at least that many cards the
  -- criterion admits -- Magmatic Insight is uncastable out of a landless hand
  -- however many cards it holds.
  --
  -- `oid` is excluded, and that is CR 601.2a rather than a convenience: the card
  -- moves to the stack at step (a), so it cannot be discarded to pay its own
  -- additional cost. Load-bearing for the OFFER, which Cast.castable measures
  -- while the card is still in hand -- without it a hand of "Cathartic Reunion
  -- plus one other card" would offer the Reunion on the strength of itself.
  CostComponent.DiscardCards (DiscardCards.MkDiscardCards n criterion) ->
    Natural.length (discardCandidates pid oid criterion gs) >= n
  -- CR 702.29a: payable only while the card is in the paying player's hand.
  -- Asked of the zone and the owner rather than of control, CR 108.4 giving a
  -- card in a hand no controller and CR 400.3 putting it in its OWNER's.
  CostComponent.DiscardThis _ -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Object.zone obj == Zone.Hand && Object.owner obj == pid
  -- CR 406.2: payable only while the card is in the paying player's graveyard,
  -- DiscardThis' shape. This is CR 113.6m's whole enforcement for the COST, so
  -- the zone gate Activate applies to the OFFER is not the only thing between a
  -- Loxodon Surveyor on the battlefield and a free draw.
  CostComponent.ExileThisFromGraveyard -> case Game.lookupObject oid gs of
    Nothing -> False
    Just obj -> Object.zone obj == Zone.Graveyard && Object.owner obj == pid
  -- CR 118.3: payable only if this player's own graveyard holds at least that
  -- many matching cards. Headless Skaab with an empty graveyard is never OFFERED
  -- rather than merely unpaid, which is what puts the additional cost INSIDE the
  -- total cost as CR 601.2f says. This component ALONE, Sacrifice's caveat.
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) ->
    Natural.length (exileCandidates pid criterion gs) >= n
  -- CR 118.3 again: payable only if the graveyard holds a matching card at all,
  -- since the top one is then determined.
  CostComponent.ExileTopFromGraveyard criterion ->
    Maybe.isJust (topExileCandidate pid criterion gs)
  -- CR 107.14 / CR 118.3: payable only if the player has at least that many
  -- energy counters.
  CostComponent.PayEnergy n -> energyOf pid gs >= n
  -- CR 606.4: always payable, CR 606.6 gating only the removing half -- but the
  -- permanent must still be one this player controls on the battlefield, rule
  -- 606.4 putting the counters on "that permanent".
  CostComponent.AddLoyaltyToThis _ ->
    Set.member oid (GameState.battlefield gs) && Projection.controllerOf oid gs == Just pid
  -- CR 606.6: a negative loyalty cost can't be activated unless the permanent
  -- has at least that many loyalty counters. "At least that many" is >=, so a -1
  -- at exactly 1 loyalty IS activatable and CR 704.5i then buries the
  -- planeswalker. Rule 606.6's "taking into account any additional costs" is
  -- already answered, `plusComponents` having combined the symbols (CR 606.5).
  CostComponent.RemoveLoyaltyFromThis n ->
    Set.member oid (GameState.battlefield gs)
      && Projection.controllerOf oid gs == Just pid
      && loyaltyCountersOn oid gs >= n
  -- CR 701.63a puts the counters on "that permanent", so the only thing that can
  -- make this unpayable is the permanent no longer being there. Deliberately NOT
  -- gated on control, unlike the loyalty arms above: rule 701.63a fixes the payer
  -- as the controller when the ability TRIGGERS, and CR 122.6 lets the counters
  -- go on whoever controls it when it resolves.
  CostComponent.PutPlusOneCountersOnThis _ -> Set.member oid (GameState.battlefield gs)
  -- CR 701.68b: a player unable to put the counters on a creature they control
  -- can't choose to blight. Nothing about `oid` and nothing about N -- rule
  -- 701.68a's candidate is qualified by CONTROL alone, the whole difference from
  -- PutPlusOneCountersOnThis above.
  CostComponent.Blight _ -> Blight.canBlight pid gs
  -- CR 601.2b: the component BEFORE X is announced, so there is no number of
  -- counters to measure rule 701.68b against -- PayLifeX's arm above, verbatim.
  -- Unreachable from either cast path, both of which substitute before they
  -- measure or pay; a fence, with Pawl.CostSpec's "an unannounced blight X is
  -- unpayable" as the test.
  CostComponent.BlightX -> False

-- CR 601.2g then 601.2h: the mana window first, then the payment, whose order is
-- the PAYER's (payComponents below).
--
-- The cost is the one the CALLER determined, taken as a value and never re-read
-- -- CR 601.2f's lock-in (see `total`). CR 118.14's `spending` arrives the same
-- way, and for a sharper reason: CR 601.2a moved the card to the stack, so the
-- object that granted the permission is not the one being paid for.
--
-- A payment can still go Unpaid where `canPay` called the cost payable: an order,
-- or an answer to a component's own prompt, that spends the wrong object loses it.
--
-- All or nothing (CR 601.2h). The entry state is captured and restored on any
-- rejection, so an Unpaid result is a complete no-op even though paying is
-- monadic -- which CR 118.12's resolution-time caller rests on too.
--
-- `casting` is the SPELL this payment is for, and it is Just at exactly one
-- caller: Pawl.Engine.Cast. It is not `oid` under another name -- `oid` is
-- whatever object the cost belongs to, which for a special action or CR 118.12's
-- resolution-time payment is not a spell being cast -- and CR 106.6's
-- restrictions all read "spend this mana only to CAST", so the two questions are
-- different ones. Mana.spendableFor is what reads it.
pay :: Maybe ObjectId -> ManaSpending.ManaSpending -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game Payment.Payment
pay casting spending pid oid cost = do
  before <- State.get
  case Cost.mana cost of
    -- CR 118.6: attempting to pay an unpayable cost is an illegal action.
    Nothing -> pure Payment.Unpaid
    -- CR 601.2g: payMana PROMPTS for which sources to activate, so it is monadic
    -- and restores the pre-payment state itself when it cannot be paid.
    Just manaCost -> do
      paidMana <- payMana casting spending pid manaCost
      if not paidMana
        then pure Payment.Unpaid
        else do
          outcome <- payComponents pid oid (Cost.components cost)
          case outcome of
            -- The components' bound slots ride out unchanged: the mana window
            -- above binds none, and a caller that has a binding environment to
            -- write them into is the only thing between here and CR 608.2h.
            Payment.Paid _ -> pure outcome
            Payment.Unpaid -> do
              State.put before
              pure Payment.Unpaid

-- CR 601.2h: the parts are paid "in any order", and the ORDER IS THE PAYER'S.
-- Observable: Jarad, Golgari Lich Lord's "Sacrifice a Swamp and a Forest" beside
-- one Bayou and one plain Swamp is payable, and a payer who spends the Bayou on
-- the Swamp half loses the cost.
--
-- Asked ONCE for the whole cost; each component's own prompts are still issued
-- as it is paid. ONE pass, where CR 601.2h states two: the second takes parts
-- involving a random element or moving an object from a library to a public
-- zone, and this vocabulary has none (`orderSensitive` below).
--
-- FILTERED, NOT TRUSTED: Game.permute keeps the printed order for an answer that
-- is not a permutation of the offered indices.
payComponents :: PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> Game Payment.Payment
payComponents pid oid components =
  if orderObservable components
    then do
      gs <- State.get
      answer <- Game.choose (Prompt.OrderCostComponents (Decide.deciderFor pid gs) pid oid components)
      payInOrder pid oid (Game.permute components answer)
    else payInOrder pid oid components

payInOrder :: PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> Game Payment.Payment
payInOrder pid oid components = case components of
  [] -> pure bindsNothing
  component : rest -> do
    outcome <- payComponent pid oid component
    case outcome of
      Payment.Unpaid -> pure Payment.Unpaid
      Payment.Paid bound -> fmap (mergeBound bound) (payInOrder pid oid rest)

-- The slots two components of one cost bound, in one map. Set-UNIONED per slot
-- rather than left-biased: Jarad, Golgari Lich Lord's "Sacrifice a Swamp and a
-- Forest" is two Sacrifice components writing one reserved name, and what that
-- names is the pair -- which Pawl.Engine.Binding.onlyOne then declines to read as
-- a single object, rather than silently answering with whichever component was
-- paid first.
mergeBound :: Map.Map SlotName.SlotName (Set.Set Recipient.Recipient) -> Payment.Payment -> Payment.Payment
mergeBound bound outcome = case outcome of
  Payment.Unpaid -> Payment.Unpaid
  Payment.Paid rest -> Payment.Paid (Map.unionWith Set.union bound rest)

-- A component that bound no slot, which is every component but Sacrifice.
bindsNothing :: Payment.Payment
bindsNothing = Payment.Paid Map.empty

-- CR 601.2h: can this cost's payer tell one order from another? Two conditions,
-- and the prompt above is asked only when both hold.
--
-- TWO OR MORE parts that touch objects (`orderSensitive` below): a part that
-- spends only a per-player scalar is inert with respect to every other, nothing
-- here reading a life total or an energy count.
--
-- And NOT ALL EQUAL, Prompt.OrderTriggers' `interchangeable` elision: equal
-- parts draw on one pool for one count each and are asked their own choices when
-- their turn comes. A FENCE rather than proven behaviour -- no card in
-- `data/cards/` prints two identical order-sensitive parts, so dropping this
-- conjunct leaves the suite green.
orderObservable :: [CostComponent.CostComponent Keyword.Type.Keyword] -> Bool
orderObservable components = case filter orderSensitive components of
  first : rest@(_ : _) -> not (all (== first) rest)
  _ -> False

-- Can paying this part change what another part of the same cost can pay with?
-- True for every part that moves an object out of a zone, taps or untaps one, or
-- changes the counters on one; False for the per-player scalars.
--
-- EXHAUSTIVE with no wildcard, `claimOf`'s posture. A component that involved a
-- random element, or moved an object from a library to a public zone, would want
-- CR 601.2h's second pass as well as an answer here.
orderSensitive :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
orderSensitive component = case component of
  CostComponent.Sacrifice {} -> True
  CostComponent.SacrificeThis -> True
  CostComponent.DiscardCards {} -> True
  CostComponent.DiscardThis _ -> True
  CostComponent.ExileCardsFromGraveyard {} -> True
  CostComponent.ExileTopFromGraveyard _ -> True
  CostComponent.ExileThisFromGraveyard -> True
  CostComponent.TapThis -> True
  CostComponent.UntapThis -> True
  CostComponent.TapForTotalPower {} -> True
  CostComponent.TapPermanents {} -> True
  CostComponent.AddLoyaltyToThis _ -> True
  CostComponent.RemoveLoyaltyFromThis _ -> True
  CostComponent.PutPlusOneCountersOnThis _ -> True
  CostComponent.Blight _ -> True
  -- The arm above's classification, which is what CR 601.2b turns this into.
  -- Unreachable unsubstituted: `pay` runs on the announced cost.
  CostComponent.BlightX -> True
  CostComponent.PayLife _ -> False
  CostComponent.PayLifeX -> False
  CostComponent.PayEnergy _ -> False

-- CR 601.2g: if the total cost includes a mana payment, the player then has a
-- chance to activate mana abilities. Reached from an ability too, by CR 602.2b.
--
-- HERE rather than in Pawl.Engine.Mana because CR 602.2b makes the window
-- recursive -- a mana ability is activated by paying ITS cost -- and Mana cannot
-- reach this module, so the mutual recursion lives on this side of the edge.
--
-- Returns whether it was paid; on failure nothing is spent (CR 601.2h), though
-- the prompts are NOT rolled back -- they live in the Program, outside the state.
--
-- Failure is REACHABLE: canPay asks whether SOME sequence of choices pays the
-- cost, and this asks the player to make them, so they may tap their only Birds
-- of Paradise for green and then be unable to pay {B}, or decline to tap anything
-- (CR 118.3c). One prompt per source tapped, against a shrinking candidate list;
-- the window CLOSES when the player says so and not when the cost is covered, CR
-- 605.3a not being rationed by what the cost needs.
--
-- `refused` keeps the loop finite now that activating a mana ability can FAIL:
-- re-offering an untapped source that just refused to pay would ask the same
-- question forever. What reaches it is a payment REFUSED and not one that was
-- never payable, CR 118.3's gate keeping an unpayable option off the offer.
--
-- The life budget only ever binds a cost NOTHING ANNOUNCED for, since a cast and
-- an activation both run `announce` first; what is left is CR 118.13b/c, where
-- pawl still chooses (#373). Recomputed on EVERY pass, a tap being able to change
-- it -- a Birds of Paradise tapped for blue takes the mana way to an unannounced
-- {G/P} off the board, leaving CR 107.4f's 2 life.
payMana :: Maybe ObjectId -> ManaSpending.ManaSpending -> PlayerId -> ManaCost.ManaCost -> Game Bool
payMana casting spending pid cost = do
  before <- State.get
  paid <- window Set.empty
  Monad.unless paid (State.put before)
  pure paid
  where
    -- What the pool would leave if the cost were paid out of it right now.
    --
    -- CR 609.4b's clauses are resolved from the board on EVERY pass rather than
    -- captured at entry: they are a CR 613.11 continuous effect and not a
    -- permission the cast carried in, so a Celestial Dawn that leaves
    -- mid-payment stops applying (CR 604.2) -- the opposite of `spending`, which
    -- rule 118.14 fixes when the cast was permitted.
    settlement gs = Mana.spend (PlayerEffect.spendManaAsThough pid gs) spending (Maybe.fromMaybe 0 (Mana.lifeNeeded casting manaActivations spending pid cost gs)) cost (Mana.Type.MkMana (fst (Mana.spendableFor casting pid gs)))
    window refused = do
      gs <- State.get
      let covered = Maybe.isJust (settlement gs)
          -- One projection per pass, shared by the enumeration and the
          -- interchangeability test rather than computed twice: Mana.manaSources
          -- is this same call.
          pcs = Projection.projectAll gs
      case filter (`Set.notMember` refused) (Mana.manaSourcesGiven manaActivations (Projection.controlGrants gs) pcs pid gs) of
        [] -> settle
        candidate : rest -> do
          answer <- chooseSource covered pid (Interchangeable.representatives pcs gs (candidate NonEmpty.:| rest)) gs
          case answer of
            Nothing -> settle
            Just oid -> do
              produced <- tapForMana oid
              window (if produced then refused else Set.insert oid refused)
    -- CR 601.2h: the window is closed, so the cost is paid out of what is there
    -- -- and simply is not paid when the player floated too little.
    --
    -- WHICH mana goes is the payer's (Mana.spendChosen), so this asks rather
    -- than reading `settlement`'s assignment: that one answers only whether the
    -- pool pays.
    settle :: Game Bool
    settle = do
      gs <- State.get
      -- CR 106.6: the payment sees only the mana it may spend, and the rest of
      -- the pool goes back beside what it leaves (CR 106.4 -- unspent mana stays
      -- unspent, it does not vanish because one cost could not use it).
      let (available, withheld) = Mana.spendableFor casting pid gs
      case Mana.plan (PlayerEffect.spendManaAsThough pid gs) spending (Maybe.fromMaybe 0 (Mana.lifeNeeded casting manaActivations spending pid cost gs)) cost (Mana.Type.MkMana available) of
        Nothing -> pure False
        Just (steps, life) -> do
          Mana.Type.MkMana left <- Mana.spendChosen pid (PlayerEffect.spendManaAsThough pid gs) steps (Mana.Type.MkMana available)
          State.modify' (Event.payLife pid life . Mana.setPool pid (Mana.Type.MkMana (withheld <> left)))
          pure True

-- Which source to tap next, or none. `covered` says whether the pool already
-- pays the cost, which picks between CR 118.3c's question and CR 601.2g's.
--
-- Asked on every pass, and NEVER elided, not even for a single candidate:
-- declining is an answer on every board, and Mana Confluence's "{T}, Pay 1 life"
-- is a cost a player at 1 life would rather not pay.
--
-- The CANDIDATES are collapsed, one per interchangeability class
-- (Pawl.Engine.Interchangeable.representatives), which is a different question:
-- two Llanowar Elves are one option only where nothing on the board tells them
-- apart, and printed identity is nowhere near enough to say so.
--
-- FILTERED, NOT TRUSTED. An unrecognised id reads as declining rather than as
-- the head candidate, since the fallback must not tap something for the player.
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

-- CR 106.12's "tap [a permanent] for mana" -- activate one of its mana
-- abilities, which by CR 602.2b means paying that ability's whole cost and then
-- adding what it yields. CR 605.3b keeps it off the stack, so this is immediate,
-- which is also why the colour choice is made HERE and not by Resolve.
--
-- Monadic because of that choice: a Mountain offers one yield and is never
-- asked, while Birds of Paradise (CR 105.4) and an Urborg'd Mountain (CR
-- 305.6/305.7) offer several. The whole yield lands, so Sol Ring's "{T}: Add
-- {C}{C}" adds two units from one activation; the TAP is the CR 107.5 component
-- of the cost being paid, so Mana Confluence's life is charged with it.
--
-- CR 118.3 GATES the options first, so a tapped permanent adds nothing and
-- Phyrexian Tower with no creature offers only its {C}. Asked here as well as at
-- the offer (Mana.manaSourcesGiven), the two differing: a source is offered on
-- having SOME payable option, and this picks among those.
--
-- Not implemented: the ability's non-mana clauses -- Ancient Tomb's "deals 2
-- damage to you". Running them needs Pawl.Engine.Resolve, above this module
-- (#1118).
tapForMana :: ObjectId -> Game Bool
tapForMana oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure False
    Just obj -> do
      -- CR 109.4a/110.2: mana goes to the mana ability's controller, which is
      -- the permanent's controller, and that same player makes the colour choice
      -- and pays the cost. Falls back to owner in the impossible case where
      -- lookupObject found the object but controllerOf answers Nothing.
      --
      -- Not implemented: the recipient an AddMana payload may NAME (CR 106.4).
      -- This path adds the whole yield to `controller`; a resolving ability
      -- reads the reference instead (#1673).
      let controller = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
      case filter (\option -> Activations.times (manaActivations Map.empty controller oid (ManaOption.cost option) gs) > 0) (Mana.manaOptionsOf oid gs) of
        [] -> pure False
        first : rest -> do
          chosen <- chooseManaYield controller oid (first NonEmpty.:| rest) gs
          outcome <- payActivation controller oid (ManaOption.cost chosen)
          case outcome of
            Payment.Unpaid -> pure False
            -- CR 605.3b: a mana ability's cost binds nothing this path could
            -- read. It lifts the AddMana yield out rather than resolving the
            -- ability, so there is no ability object to write a slot onto (#1118).
            Payment.Paid _ -> do
              State.modify' (Mana.addMana controller (Mana.unitsOf (ManaOption.yield chosen)))
              pure True

-- CR 602.2b sends an activation cost through CR 601.2b-i, so a mana ability pays
-- its whole cost. All or nothing, `pay`'s posture and for CR 601.2h's reason.
--
-- COMPONENTS FIRST, where `pay` opens the CR 601.2g mana window first. That
-- inverts CR 601.2g/h for termination: {T} is a component, so paying components
-- first takes this source off its own mana window's candidate list before
-- payMana goes looking. Left in rule order, a mana ability whose cost held mana
-- would tap itself to pay itself, forever.
--
-- Unobservable, since every mana ability in `data/cards/` has an EMPTY mana part
-- and the short-circuit below opens no window. Cabal Coffers' "{2}, {T}" is what
-- would make the inversion visible, and wants CR 601.2g put back with a
-- different guard (#1120). That short-circuit is also a performance call, this
-- being on the path of every tap for mana.
payActivation :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game Payment.Payment
payActivation pid oid cost = do
  before <- State.get
  outcome <- payComponents pid oid (Cost.components cost)
  paid <- case (outcome, Cost.mana cost) of
    (Payment.Paid _, Just (ManaCost.MkManaCost [])) -> pure True
    -- CR 118.14's permission is granted to CAST a spell and never to activate an
    -- ability, so an activation cost is paid with the mana it is -- and CR
    -- 106.6-restricted mana cannot pay it at all, which is the same sentence
    -- read the other way (`casting` is Nothing).
    (Payment.Paid _, Just manaCost) -> payMana Nothing ManaSpending.AsProduced pid manaCost
    -- CR 118.6: attempting to pay an unpayable cost is an illegal action.
    _ -> pure False
  Monad.unless paid (State.put before)
  -- `outcome` and not a fresh Paid: the components' bound slots survive the mana
  -- half, which binds none of its own.
  pure (if paid then outcome else Payment.Unpaid)

-- Which way this source is tapped -- which mana ability, in which mode, and
-- which colour each of that mode's AddMana effects makes -- asked as ONE
-- question because the answer is one activation.
--
-- The COST rides along with the yield (Pawl.Types.ManaOption): two of one
-- permanent's mana abilities can add the same mana for different costs, an
-- Urborg'd Mana Confluence adding {B} for {T} and {B} for {T} plus a life, so a
-- yield-only answer names both.
--
-- Elided exactly when the source offers ONE option -- Mana.manaOptionsOf has
-- already collapsed routes alike in cost and yield.
--
-- FILTERED, NOT TRUSTED: honouring an option the source does not offer would
-- mint mana out of nothing, or charge the wrong cost for it.
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
    pure bindsNothing
  -- CR 107.6: a direct edit like TapThis above, not a distinction CR 701.26b
  -- draws. Nothing in `data/cards/` watches for an untap, so the two routes are
  -- observationally identical; the first card that triggers on untapping would
  -- force this through the funnel.
  CostComponent.UntapThis -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Untapped}) oid (GameState.objects gs)})
    pure bindsNothing
  -- Through Event.sacrifice, the CR 701.21 funnel, and never a direct zone poke:
  -- a cost payment is a game event, so dies-triggers, replacement effects and the
  -- turn history all see it.
  CostComponent.SacrificeThis -> do
    -- CR 701.21a's "a permanent they don't control" guard lives in the funnel;
    -- `pid` is the player paying, who for "sacrifice this" is its controller.
    Event.sacrifice pid oid
    pure bindsNothing
  -- CR 119.4: the payment is subtracted from the life total, shared with CR
  -- 107.4f's Phyrexian symbol as the payability check above is.
  CostComponent.PayLife n -> do
    State.modify' (Event.payLife pid n)
    pure bindsNothing
  -- Unpayable, `canPayComponent`'s answer and for its reason. Unpaid rather than
  -- a guessed 0, which CR 601.2h turns into the reversal of the whole casting.
  CostComponent.PayLifeX -> pure Payment.Unpaid
  -- CR 701.21a: the player chooses which of their permanents dies, so this is a
  -- prompt. Elided only when forced -- exactly as many candidates as the count.
  -- Three payable Mountains and a count of two IS asked: they differ in tap
  -- state, counters and attached auras.
  --
  -- Reject-not-repair: an answer that is not a size-`n` subset of the offered
  -- candidates makes the whole payment Unpaid, which pay's restore turns into a
  -- no-op.
  CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) -> do
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
        -- CR 608.2h: the permanents are gone by the time anything this cost paid
        -- for resolves, so an effect that reads one ("the sacrificed creature's
        -- power") needs a name for it. The ONE component that binds a slot; every
        -- other returns bindsNothing.
        --
        -- Bound under the id it had on the battlefield, which is the id
        -- Event.changeZone files its last known information under -- the graveyard
        -- incarnation is a different object (CR 400.7) and carries none of it.
        pure (Payment.Paid (Map.singleton Binding.sacrificedPermanent (Set.map Recipient.ToObject chosen)))
      else pure Payment.Unpaid
  -- CR 702.122a: the payer chooses WHICH permanents to tap and HOW MANY, so this
  -- is a prompt, and unlike Sacrifice above it is NEVER elided -- whether the
  -- answer is forced is a question about subsets rather than a count, and getting
  -- it wrong decides for the player.
  --
  -- Reject-not-repair, Sacrifice's posture. The total is summed over the answer
  -- as given, INCLUDING any negative power: CR 702.122a measures the creatures
  -- that were tapped, not a best case. The tap is a direct edit, TapThis' route.
  --
  -- Not implemented: CR 702.122b/c's "crews a Vehicle" and "crewed by" relation,
  -- and so CR 702.122e's trigger and CR 702.122d's restriction -- the chosen set
  -- is spent here and recorded nowhere (#915).
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n criterion) -> do
    gs <- State.get
    let candidates = tapCandidates pid oid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <- Game.choose (Prompt.ChooseTapsForTotalPower decider pid oid candidates n)
    let totalPower = sum (fmap (`tapPower` gs) (Set.toAscList chosen))
    if Set.isSubsetOf chosen (Set.fromList candidates) && totalPower >= toInteger n
      then do
        Monad.mapM_ tapObject (Set.toAscList chosen)
        pure bindsNothing
      else pure Payment.Unpaid
  -- The payer chooses WHICH permanents to tap, so this is a prompt. Sacrifice's
  -- posture rather than TapForTotalPower's: the count is exact, so as many
  -- candidates as the count leaves one legal answer and the prompt is elided,
  -- where a THRESHOLD would have left a choice among subsets.
  --
  -- Reject-not-repair, Sacrifice's posture again; the tap is a direct edit,
  -- TapThis' route.
  CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion) -> do
    gs <- State.get
    let candidates = tapCandidates pid oid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length candidates <= n
        then pure (Set.fromList candidates)
        else Game.choose (Prompt.ChooseTaps decider pid oid candidates n)
    if Set.isSubsetOf chosen (Set.fromList candidates) && Natural.length chosen == n
      then do
        Monad.mapM_ tapObject (Set.toAscList chosen)
        pure bindsNothing
      else pure Payment.Unpaid
  -- CR 701.9b: the discarding player chooses which cards, so this is a prompt.
  -- Elided only when forced -- as many MATCHING cards in hand as the count, which
  -- the criterion decides and not the hand's size: Magmatic Insight beside one
  -- land and three other cards asks nothing.
  --
  -- Reject-not-repair, matching Sacrifice above and deliberately NOT matching the
  -- Discard effect, which completes an undersized answer: a cost may simply go
  -- unpaid, where an effect has no such out. The answer is read as a SET of card
  -- ids and rejected unless that set is exactly `n` cards drawn from `held`, so
  -- `List.nub` is what the Set-answered Sacrifice arm already accepts rather than
  -- the repair it looks like.
  --
  -- CR 701.9a's move goes through Event.discard, so the card gets a CR 400.7
  -- incarnation, Rest in Peace's redirect composes, and the discard is recorded
  -- for a rule 701.9a trigger to read.
  CostComponent.DiscardCards (DiscardCards.MkDiscardCards n criterion) -> do
    gs <- State.get
    let held = discardCandidates pid oid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length held <= n
        then pure held
        else Game.choose (Prompt.ChooseDiscard decider pid held n)
    let distinct = List.nub chosen
    if all (\c -> List.elem c held) distinct && Natural.length distinct == n
      then do
        Monad.mapM_ (Event.discard DiscardCause.Ordinary pid) distinct
        pure bindsNothing
      else pure Payment.Unpaid
  -- CR 701.9a's move, through the funnel DiscardCards uses above. No prompt: the
  -- cost names this card.
  --
  -- The card is in the GRAVEYARD (or wherever the funnel redirected it) by the
  -- time the ability resolves, which is CR 702.29c's "from whatever zone the card
  -- winds up in after it's cycled".
  --
  -- CR 702.29c's "of a CYCLING ability" is the one thing this site cannot see for
  -- itself, so the cause rides on the component: Keyword.cycling mints rule
  -- 702.29a's discard as ToPayCyclingCost and Keyword.reinforce mints rule
  -- 702.77a's as Ordinary, rule 702.77 never making reinforce a cycling ability.
  -- Pawl.ActivateSpec's "CR 702.77a a reinforce discard is not a cycle" proves it.
  CostComponent.DiscardThis cause -> do
    Event.discard cause pid oid
    pure bindsNothing
  -- CR 107.14: paying energy removes that many energy counters from the player.
  -- Natural subtraction is PARTIAL, so `left` is guarded; canPayComponent
  -- guarantees `have >= n` at pay time, and the guard keeps this total anyway.
  CostComponent.PayEnergy n -> do
    spendEnergy pid n
    pure bindsNothing
  -- CR 606.4: put the loyalty counters on. A DIRECT edit and deliberately NOT
  -- through Event.putCounters, the CR 614 funnel: CR 614.16 admits a
  -- counter-scaling replacement only where a resolving spell or ability's EFFECT
  -- puts the counter on, and CR 602.2b pays an activation cost as part of
  -- ACTIVATING (CR 601.2h), which CR 609.1 gives no resolution to hang it on.
  -- That is what makes Doubling Season double a planeswalker's starting loyalty
  -- (CR 306.5b, through the funnel) and leave its +1 alone.
  CostComponent.AddLoyaltyToThis n -> do
    State.modify' (\gs -> gs {GameState.objects = Map.adjust (addLoyalty n) oid (GameState.objects gs)})
    pure bindsNothing
  -- CR 606.4's other half, and NOT the direct edit its sibling above is: it goes
  -- through Event.removeCounters, CR 122's removal funnel, so the removal is
  -- recorded as a GameEvent.CountersRemoved a trigger can see (Chandra, Fire
  -- Artisan's "whenever one or more loyalty counters are removed from Chandra",
  -- which her own -7 fires).
  --
  -- That asymmetry with AddLoyaltyToThis is not a hole in the argument above.
  -- CR 614.16 is about REPLACING a placement, and Event.removeCounters runs no
  -- replacement loop at all -- its own Haddock gives the reason, that no
  -- ReplacementEffect class in Pawl.Types.ReplacementEffect pairs with a removal.
  -- So routing this half changes nothing about which counters Doubling Season
  -- doubles; what it adds is the record.
  --
  -- The funnel saturates, which is the floor the direct write here used to apply
  -- itself: CR 606.6 has already refused an activation the permanent cannot pay
  -- for, so a saturating removal is unreachable through this door anyway.
  CostComponent.RemoveLoyaltyFromThis n -> do
    Event.removeCounters oid CounterKind.Loyalty n
    pure bindsNothing
  -- CR 122.6's placement, through the Event.putCounters funnel as
  -- CounterCause.ByEffect -- the opposite call from AddLoyaltyToThis above, and
  -- the difference is WHEN the cost is paid. CR 118.12 pays this one as the spell
  -- or ability RESOLVES, which is what CR 609.1 calls an effect and so what CR
  -- 614.16 reaches: Hardened Scales sees endure's counter, and still not a
  -- planeswalker's +1. Paid whatever the funnel then places.
  CostComponent.PutPlusOneCountersOnThis n -> do
    -- CR 609.1: the player putting them is the one whose resolution this is,
    -- which for a cost paid during a resolution is the player paying it.
    Monad.void (Event.putCounters (CounterCause.ByEffect pid) oid CounterKind.PlusOnePlusOne n)
    pure bindsNothing
  -- CR 701.68a's whole procedure, which Pawl.Engine.Blight owns. Unpaid on rule
  -- 701.68b's board, which canPayComponent has already refused, so reaching it
  -- means the creature left between the check and the payment.
  --
  -- ByEffect, and the one place this module's CR 614.16 story is not exact: a
  -- blight paid under CR 601.2h has no resolution for the placement to be the
  -- effect of. Unobservable, rule 614.16's effect-grain patterns in `data/cards/`
  -- all naming +1/+1 counters (gap #1647).
  CostComponent.Blight n -> do
    blighted <- Blight.blight pid oid n
    pure (if blighted then bindsNothing else Payment.Unpaid)
  -- Unpayable, `canPayComponent`'s answer and for its reason -- PayLifeX's arm
  -- above, verbatim.
  CostComponent.BlightX -> pure Payment.Unpaid
  -- CR 406.2's move, through the Event.changeZone funnel, so the card gets a CR
  -- 400.7 incarnation and anything watching a graveyard-to-exile move sees it.
  -- No prompt: the cost names this card.
  --
  -- The card is in EXILE by the time the ability resolves, which is what makes CR
  -- 113.7a load-bearing here -- Loxodon Surveyor's draw resolves off a source
  -- that has already left the graveyard the cost read.
  CostComponent.ExileThisFromGraveyard -> do
    Event.changeZone oid Zone.Exile
    pure bindsNothing
  -- CR 406.2's move again, for CHOSEN cards: the payer picks which, so this is a
  -- prompt. Elided only when forced, Sacrifice's elision.
  --
  -- Reject-not-repair, Sacrifice's posture verbatim. The candidates are read
  -- ONCE, before the prompt, so the answer is checked against the same list the
  -- player was offered.
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) -> do
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
        pure bindsNothing
      else pure Payment.Unpaid
  -- CR 406.2 with no prompt: CR 404.2's order determines the card. Unpaid where
  -- the graveyard holds no matching card, agreeing with canPayComponent above.
  CostComponent.ExileTopFromGraveyard criterion -> do
    gs <- State.get
    case topExileCandidate pid criterion gs of
      Nothing -> pure Payment.Unpaid
      Just candidate -> do
        Event.changeZone candidate Zone.Exile
        pure bindsNothing

-- The arithmetic half, pure and board-free.
--
-- 1. Every INCREASE is added to the generic component (CR 601.2f's order, and
--    Thalia's own ruling: increases first, then reductions).
-- 2. A REDUCTION is an amount of mana. Its GENERIC part comes off the generic
--    component only (CR 118.7a), floored at zero; its TYPED part cancels
--    matching typed symbols one for one (Edgewalker). CR 118.7f puts a PHYREXIAN
--    symbol on the typed side, and CR 118.7g sends a SNOW symbol the other way,
--    where CR 107.4h keeps an {S} in a COST out of the generic component -- so
--    each question below is asked by two functions, one per SIDE.
-- 3. An EXCESS typed symbol -- one the cost has no matching symbol for -- comes
--    off the GENERIC component instead, one generic mana per symbol (CR
--    118.7b-d). A reduction whose own text confines it to the coloured mana paid
--    DROPS the excess rather than spilling it, which is card text CR 101.1 lets
--    override the rules: Edgewalker's reminder text makes a {1}{W} Cleric spell
--    cost {1}, not {0}.
-- 4. CR 601.2f's floor at {0} needs no special case: the empty list IS {0}.
-- 5. A REDUCING EFFECT'S OWN FLOOR is applied as that reduction lands, never as
--    a clamp on the pooled result -- Heartstone's sentence says THIS EFFECT (CR
--    101.1), and Heartstone beside Blossoming Tortoise on an animated Mutavault's
--    {1} tells the two readings apart at {0} against {1}. The shortfall is
--    GENERIC mana, and it NEVER RAISES a cost already below the floor.
--
-- Reductions are FOLDED one at a time, in the LIST's own order, which is CR
-- 601.2f's order as the PAYER chose it: `announceReductions` reorders them to the
-- answer it got before this ever runs, and nothing here sorts. A caller that
-- reaches this without that seam is asking a different question -- the GATE does,
-- through `totalManas`, and enumerates the orders itself rather than picking one.
--
-- Every step's result is CANONICAL: one leading Generic symbol carrying the whole
-- generic component (omitted at zero), then the SURVIVING printed typed symbols
-- in their original order -- presentation, but it is what the next reduction in
-- the fold reads.
applyAdjustments :: CostAdjustments.CostAdjustments -> ManaCost.ManaCost -> ManaCost.ManaCost
applyAdjustments adjustments cost =
  let increases = CostAdjustments.increases adjustments
      reductions = CostAdjustments.reductions adjustments
      costGenericOf symbol = case symbol of
        ManaSymbol.Generic n -> n
        ManaSymbol.OfType _ -> 0
        -- CR 107.4e: a colour/colour hybrid is paid with one mana of a stated
        -- type, so it is no part of the generic component.
        ManaSymbol.Hybrid {} -> 0
        -- A monocolored hybrid's {2} half IS generic mana once CR 601.2b's
        -- nonhybrid equivalent names it, and a symbol still spelled {2/R} is one
        -- CR 601.2b has NOT named -- Flame Javelin's own ruling. What still
        -- arrives unannounced is CR 118.13b/c's costs (#373).
        ManaSymbol.MonocoloredHybrid _ -> 0
        -- CR 107.4f makes this a COLOURED symbol whose other half is life.
        ManaSymbol.Phyrexian _ -> 0
        -- CR 107.4h: generic reductions don't affect {S} costs, which is why it
        -- is not spelled Generic 1. The one arm where this function and
        -- reducingGenericOf part company; the Adjustments case "CR 107.4h a
        -- generic reduction does not affect an {S} in the cost" proves this side.
        ManaSymbol.Snow -> 0
        -- Unreachable: CR 601.2b precedes 601.2f, so every Variable is already
        -- substituted.
        ManaSymbol.Variable -> 0
      -- The REDUCTION's generic amount, two functions rather than one because CR
      -- 118.7g makes the two sides read an {S} differently. Every other arm
      -- agrees with costGenericOf's, and agreeing is not sharing.
      reducingGenericOf symbol = case symbol of
        -- CR 118.7a's amount of generic mana, which is what this side is.
        ManaSymbol.Generic n -> n
        -- The typed side reads an OfType, and CR 118.7b-d spill what it strands
        -- back here -- point 3 above, counted after the cancellation and not by
        -- this function, which cannot see the cost.
        ManaSymbol.OfType _ -> 0
        -- CR 107.4e's colour/colour hybrid has no generic half at all, and a
        -- symbol still spelled {2/R} is one CR 118.7e's choice has not been made
        -- for -- announceReductions leaves a Generic behind when the {2} half is
        -- taken, and the gate enumerates the same halves.
        ManaSymbol.Hybrid {} -> 0
        ManaSymbol.MonocoloredHybrid _ -> 0
        -- CR 118.7f gives a Phyrexian reduction to the typed side whole.
        ManaSymbol.Phyrexian _ -> 0
        -- CR 118.7g: a snow-symbol reduction is that much GENERIC mana. THE arm
        -- this side exists for; CR 107.4h is about the other side.
        ManaSymbol.Snow -> 1
        -- Unreachable, costGenericOf's Variable arm.
        ManaSymbol.Variable -> 0
      -- "Typed" here means "not generic": everything but Generic survives and
      -- keeps its printed position, which is the only way an unreducible symbol
      -- reaches Mana.spend intact.
      isTyped symbol = case symbol of
        ManaSymbol.Generic _ -> False
        ManaSymbol.OfType _ -> True
        ManaSymbol.Hybrid {} -> True
        ManaSymbol.MonocoloredHybrid _ -> True
        ManaSymbol.Phyrexian _ -> True
        ManaSymbol.Snow -> True
        ManaSymbol.Variable -> True
      -- The two SIDES of the cancellation, two functions because CR 118.7f makes
      -- them disagree: which one mana type a printed COST symbol offers up, and
      -- which one a REDUCTION's symbol takes away. Nothing can be shared -- {G/P}
      -- names green when a reduction says it and nothing when a cost does, and
      -- Pawl.PlayerEffectSpec's SyntheticPhyrexianDiscount group proves both
      -- halves against cards.
      costManaTypeOf symbol = case symbol of
        ManaSymbol.Generic _ -> Nothing
        ManaSymbol.OfType manaType -> Just manaType
        -- CR 107.4e names TWO types, and a symbol still spelled {G/U} here is one
        -- CR 601.2b has not named -- Mana.announce leaves an OfType behind when
        -- it does. What still arrives unannounced is CR 118.13b/c's costs (#373).
        ManaSymbol.Hybrid {} -> Nothing
        ManaSymbol.MonocoloredHybrid _ -> Nothing
        -- EXACT rather than an elision: the symbol is necessarily UNANNOUNCED, CR
        -- 601.2b's announcement leaving behind either an OfType or a payment of
        -- life, so no caller reaching this arm has established that there is a
        -- green mana here to cancel. Edgewalker's ruling says so outright.
        ManaSymbol.Phyrexian _ -> Nothing
        -- CR 107.4h: {S} is paid with one mana of ANY type, so it names none, and
        -- a reduction of one white mana cannot single it out.
        ManaSymbol.Snow -> Nothing
        -- Unreachable, costGenericOf's Variable arm; {X} names no mana type.
        ManaSymbol.Variable -> Nothing
      reducingManaTypeOf symbol = case symbol of
        -- CR 118.7a's half, which reducingGenericOf above already counted.
        ManaSymbol.Generic _ -> Nothing
        ManaSymbol.OfType manaType -> Just manaType
        -- CR 118.7e: the choice of half belongs to the PLAYER PAYING, so
        -- answering it here would be the engine making it. A symbol still spelled
        -- {W/U} or {2/R} is one nobody has been asked about; announceReductions
        -- leaves the chosen half's symbol behind when they have.
        ManaSymbol.Hybrid {} -> Nothing
        ManaSymbol.MonocoloredHybrid _ -> Nothing
        -- CR 118.7f: reduced by one mana of that symbol's colour. The one arm
        -- where the two sides part company -- unlike CR 118.7e's hybrid this asks
        -- the player nothing, the symbol naming exactly one colour.
        ManaSymbol.Phyrexian color -> Just (ManaType.Colored color)
        -- CR 118.7g makes an {S} reduction GENERIC mana, so reducingGenericOf's
        -- Snow arm is where it lands.
        ManaSymbol.Snow -> Nothing
        -- Unreachable, costGenericOf's Variable arm.
        ManaSymbol.Variable -> Nothing
      -- The canonical form point 5's header describes: the generic component as
      -- one leading symbol, then the typed symbols left.
      canonical generic typed = ManaCost.MkManaCost ((if generic == 0 then [] else [ManaSymbol.Generic generic]) <> typed)
      -- Point 1: every increase, onto the generic component, before any reduction.
      raise (ManaCost.MkManaCost symbols) =
        canonical (sum (fmap costGenericOf symbols) + sum increases) (filter isTyped symbols)
      -- ONE reduction, with the floor its own effect states.
      reduce (ManaCost.MkManaCost symbols) reduction =
        let reducingSymbols = ManaCost.unwrap (AppliedReduction.amount reduction)
            floor_ = AppliedReduction.atLeast reduction
            generic = sum (fmap costGenericOf symbols)
            typed = filter isTyped symbols
            (survivors, unspent) = cancel (Maybe.mapMaybe reducingManaTypeOf reducingSymbols) typed
            -- Point 3: the typed reduction the cost had nothing to give to comes
            -- off the GENERIC component instead, one generic mana per stranded
            -- symbol (CR 118.7b-d), unless this effect's own text confines it to
            -- the coloured mana paid (Edgewalker, CR 101.1).
            spilled = if AppliedReduction.coloredOnly reduction then 0 else Natural.length unspent
            taken = sum (fmap reducingGenericOf reducingSymbols) + spilled
            -- Natural subtraction is PARTIAL, so CR 601.2f's floor is also what
            -- keeps this total.
            lowered = if generic >= taken then generic - taken else 0
            -- Point 5 above. Every typed symbol is at least one mana (CR
            -- 107.4e/107.4f/107.4h), so the mana left in the cost is `lowered`
            -- plus how many survivors there are.
            typedCount = Natural.length survivors
            required = min floor_ (generic + Natural.length typed)
            floored = if lowered + typedCount >= required then lowered else required - typedCount
         in canonical floored survivors
      -- Each reducing symbol cancels ONE matching symbol in the cost, walking the
      -- printed order so the survivors keep it. `unspent` is the bag of reducing
      -- types that have not found a match yet; the survivors come back beside
      -- whatever is left of it when the walk ends, which is point 3's excess.
      cancel unspent remaining = case remaining of
        [] -> ([], unspent)
        symbol : rest -> case costManaTypeOf symbol of
          Just manaType | elem manaType unspent -> cancel (List.delete manaType unspent) rest
          _ -> let (survivors, left) = cancel unspent rest in (symbol : survivors, left)
   in List.foldl' reduce (raise cost) reductions

-- CR 107.14: how many energy counters this player has, which is CR 118.3's
-- ceiling on what they can pay. Player.counters' absent-means-zero convention,
-- and a player who is not in the game has none.
--
-- SHARED rather than inlined at its readers: canPayComponent's PayEnergy gate,
-- spendEnergy below, and Pawl.Engine.Resolve's Effect.PayAnyEnergy bound must
-- agree about what "how much {E} do you have" means, and the last of those is a
-- number shown to the payer rather than a predicate.
energyOf :: PlayerId -> GameState -> Natural
energyOf pid gs =
  maybe 0 (Map.findWithDefault 0 PlayerCounterKind.Energy . Player.counters) (Map.lookup pid (GameState.players gs))

-- CR 107.14: pay this much {E} -- remove that many energy counters from the
-- player. Natural subtraction is PARTIAL, so the floor is explicit; every caller
-- has already measured against energyOf, and the guard keeps this total anyway.
--
-- The one writer, so Pawl.Engine.Resolve's Effect.PayAnyEnergy spends through
-- exactly the same edit CostComponent.PayEnergy does.
spendEnergy :: PlayerId -> Natural -> Game ()
spendEnergy pid n =
  let spend player =
        let have = Map.findWithDefault 0 PlayerCounterKind.Energy (Player.counters player)
            left = if have >= n then have - n else 0
         in player {Player.counters = Map.insert PlayerCounterKind.Energy left (Player.counters player)}
   in State.modify' (\gs -> gs {GameState.players = Map.adjust spend pid (GameState.players gs)})
