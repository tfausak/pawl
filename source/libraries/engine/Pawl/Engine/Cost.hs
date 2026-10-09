-- CR 118: what a cost IS, and everything the closed half needs to do with one --
-- the candidates (costsFor), the total (total, CR 601.2f), whether it can be
-- paid (canPay, CR 118.3) and paying it (pay). Pawl.Engine.Mana keeps pools,
-- production and spending; this module keeps the cost.
--
-- `pay` serves two contexts: CR 601.2g/h's payment as a spell is cast or an
-- ability activated, and CR 118.12's payment when one RESOLVES. `total`'s CR
-- 601.2f adjustments reach only the first.
--
-- The casing home for Pawl.Types.CostComponent; every other module reads the
-- classifications derived here instead -- requiresSicknessCheck (CR 302.6),
-- isLoyaltyCost (CR 606.2/606.3), zoneFunctionedFrom (CR 113.6m),
-- statesHiddenQuality (CR 118.8c). The one classification that is NOT here is
-- CR 605.1a's library clause: Pawl.Engine.ManaAbility.costMovesLibraryCard
-- answers it, this module being unreachable from there (Cost -> Mana ->
-- Projection -> ManaAbility).
module Pawl.Engine.Cost where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Semigroup as Semigroup
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.ActivationProhibition as ActivationProhibition
import qualified Pawl.Engine.ActivationRestriction as ActivationRestriction
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Blight as Blight
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Claim as Claim
import qualified Pawl.Engine.Coin as Coin
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.CrewRestriction as CrewRestriction
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Detain as Detain
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Forage as Forage
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Interchangeable as Interchangeable
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.ManaRider as ManaRider
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Reversal as Reversal
import qualified Pawl.Engine.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Engine.SourceContext as SourceContext
import qualified Pawl.Engine.Subtype as Subtype
import qualified Pawl.Engine.Summoning as Summoning
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Activations as Activations
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.AppliedReduction as AppliedReduction
import qualified Pawl.Types.Behold as Behold
import qualified Pawl.Types.Binding as Binding.Type
import qualified Pawl.Types.CandidateCost as CandidateCost
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Claim (Claim)
import qualified Pawl.Types.Claim as Claim.Type
import qualified Pawl.Types.ClaimAxis as ClaimAxis
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostAdjustments as CostAdjustments
import qualified Pawl.Types.CostChoice as CostChoice
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostDirection as CostDirection
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.CostScale as CostScale
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterSpread as CounterSpread
import qualified Pawl.Types.CountersFromPermanents as CountersFromPermanents
import qualified Pawl.Types.CountersFromThis as CountersFromThis
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Emerge as Emerge
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.ExileLink as ExileLink
import qualified Pawl.Types.ExileMaterials as ExileMaterials
import qualified Pawl.Types.ExilePermanents as ExilePermanents
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.ForetellCost as ForetellCost
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Hybrid as Hybrid
import qualified Pawl.Types.HybridPhyrexian as HybridPhyrexian
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.LoyaltyKind as LoyaltyKind
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaAbilityPerformer as ManaAbilityPerformer
import qualified Pawl.Types.ManaAbilityResolved as ManaAbilityResolved
import qualified Pawl.Types.ManaActivation as ManaActivation
import qualified Pawl.Types.ManaAdded as ManaAdded
import qualified Pawl.Types.ManaAddedCause as ManaAddedCause
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaOption as ManaOption
import qualified Pawl.Types.ManaSegment as ManaSegment
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.ManaWindow as ManaWindow
import qualified Pawl.Types.Milled as Milled
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.PermissionCost as PermissionCost
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerEffect as PlayerEffect.Type
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Prototype as Prototype
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReturnPermanents as ReturnPermanents
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.Rounding as Rounding
import Pawl.Types.RowSource (RowSource)
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpendTrigger as SpendTrigger
import qualified Pawl.Types.Subtype as Subtype.Type
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TapPermanents as TapPermanents
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TappedForMana as TappedForMana
import qualified Pawl.Types.Threshold as Threshold
import qualified Pawl.Types.VariableChoice as VariableChoice
import qualified Pawl.Types.WhichCounters as WhichCounters
import qualified Pawl.Types.Zone as Zone

-- CR 118.6: the cost of an object with no mana cost. Also the ChooseCost
-- fallback's answer when no candidate was offered.
unpayable :: Cost Keyword.Type.Keyword
unpayable = Cost.MkCost {Cost.mana = Nothing, Cost.components = []}

-- CR 118.9a's alternative cost: the printed cost with the mana part replaced by
-- the amount the effect stated, and never Nothing, CR 118.6's unpayable cost. The
-- additional costs ride along (CR 118.9d), and the face is the one being CAST (CR
-- 709.3a / 712.11a).
--
-- The face's CHOICE costs do not ride here, one of them being more than one
-- cost: every caller passes the result through `choiceVariants`.
insteadOfManaCost :: ManaCost.ManaCost -> Face.Face card -> Cost Keyword.Type.Keyword
insteadOfManaCost mana face =
  Cost.MkCost
    { Cost.mana = Just mana,
      Cost.components = Face.additionalCosts face
    }

-- CR 118.9a: the alternative cost a CR 601.3 permission states, settled against
-- the face being cast.
--
-- A waterbend {X} is that much generic mana plus CR 701.67b's licence scoped to
-- it, the shape a printed waterbend cost takes (CostComponent.WaterbendInstead). X is
-- read off the FACE and not the card, so a split card's half is priced at its
-- own mana value (CR 202.3d, 709.3b), and X in that face's own cost counts 0
-- (CR 202.3e); the ruling's "the only legal choice for X is 0" is CR 107.3b
-- and falls out, this cost having no variable.
permissionCost :: PermissionCost.PermissionCost -> Face.Face card -> Cost Keyword.Type.Keyword
permissionCost alternative face = case alternative of
  PermissionCost.InsteadOfManaCost mana -> insteadOfManaCost mana face
  PermissionCost.WaterbendManaValue ->
    let amount = Integer.toNaturalSaturating (maybe 0 Quantity.manaCostValue (Face.manaCost face))
     in Cost.MkCost
          { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic amount | amount > 0]),
            Cost.components = CostComponent.WaterbendInstead amount : Face.additionalCosts face
          }

-- CR 118.9's "without paying its mana cost", which is insteadOfManaCost of an
-- EMPTY ManaCost -- {0} (CR 118.5a).
withoutPayingManaCost :: Face.Face card -> Cost Keyword.Type.Keyword
withoutPayingManaCost = insteadOfManaCost (ManaCost.MkManaCost [])

-- A candidate no keyword ability offered: the printed cost, a printed
-- alternative, or a cost an effect applied (CR 118.9).
untagged :: Cost Keyword.Type.Keyword -> CandidateCost.CandidateCost
untagged = CandidateCost.plain Nothing

-- The first offered candidate, or `unpayable` when none was offered.
firstOffered :: [Cost Keyword.Type.Keyword] -> Cost Keyword.Type.Keyword
firstOffered candidates = case candidates of
  c : _ -> c
  [] -> unpayable

-- CR 702.37a and CR 702.168a: what a face-down cast pays. Both rules fix it at
-- {3}, so one value serves both. An alternative cost (CR 118.9) stated by the
-- rule rather than by a card, which is why it is minted here and not read off
-- Keyword.Morph or Keyword.Disguise -- those constructors carry CR 702.37e's and
-- CR 702.168d's special-action costs, a different amount on every printing. No
-- additional costs ride along: CR 702.37c and CR 702.168b measure the cast
-- against the face-down characteristics, which neither rule's listing prints one
-- in.
faceDownCost :: Cost Keyword.Type.Keyword
faceDownCost =
  Cost.MkCost
    { Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]),
      Cost.components = []
    }

-- CR 702.143d's "that effect may give the card a foretell cost", as every
-- printing states it: the mana cost of the face being cast, reduced by the amount
-- the effect stated (Ethereal Valkyrie's {2}). Nothing when no effect gave this
-- foretold card a cost.
--
-- Settled HERE, at CR 601.2b where the candidate costs are named, rather than
-- when the effect resolved. Two rules push it here and neither is about
-- convenience:
--
--   * CR 712.11b chooses which face of a modal double-faced card is being cast,
--     and rule 702.143d's cost is that face's mana cost reduced -- the Valkyrie's
--     own ruling says the foretell cost is based on the mana cost of the face
--     cast from exile. A cost settled when the card was exiled could only be one
--     face's.
--   * It is still a COST and never a CR 601.2f reduction, which is what settling
--     it before that rule buys: rule 601.2f applies the board's increases before
--     its reductions, so a {W}{U} card given "mana cost reduced by {2}" under a
--     {3} tax costs {3}{W}{U} here, where a reduction carried into rule 601.2f
--     would have taken the {2} off the tax and left {1}{W}{U}.
--
-- Nothing for a face with no mana cost -- a land (CR 202.1b) -- which leaves the
-- card foretold with no foretell cost of its own and so with no cast to price
-- here. That is rule 702.143d's "for any foretell cost it has" read at zero.
--
-- No additional costs ride along: the caller wraps this in `withAdditional`, CR
-- 118.9d, exactly as it wraps a printed foretell cost.
grantedForetellCost :: Face.Face card -> Object.Object -> Maybe (Cost Keyword.Type.Keyword)
grantedForetellCost face obj = do
  amount <- Object.foretellCostReduction obj
  foretellCostFor face (ForetellCost.ManaCostReducedBy amount)

-- CR 702.143a: a foretell keyword's payload settled against the face being cast
-- -- the stated cost as printed, or that face's mana cost reduced (CR 118.7),
-- grantedForetellCost's reading and for its reasons. Nothing for a reduction off
-- a face with no mana cost (CR 202.1b).
foretellCostFor :: Face.Face card -> ForetellCost.ForetellCost Keyword.Type.Keyword -> Maybe (Cost Keyword.Type.Keyword)
foretellCostFor face payload = case payload of
  ForetellCost.Stated cost -> Just cost
  ForetellCost.ManaCostReducedBy amount -> do
    manaCost <- Face.manaCost face
    pure
      Cost.MkCost
        { Cost.mana = Just (reducedManaCost amount manaCost),
          Cost.components = []
        }

-- CR 118.7: one object's mana cost with an amount taken off it, which is the
-- whole of what "its mana cost reduced by {2}" names. Rule 118.7a-g's spill is
-- `applyAdjustments`', a reduction arriving here being a reduction like any
-- other; an empty amount is {0} and takes nothing off (CR 118.5).
--
-- SHARED by the two provenances that describe a cost this way, so neither can
-- drift from the other: CR 702.143d's foretell cost above, settled as the cast
-- is proposed, and CR 118.6's resolution cost (Pawl.Types.CostBasis), settled
-- as Pawl.Engine.Resolve offers the gate. Both stay COSTS rather than becoming
-- CR 601.2f reductions -- grantedForetellCost's haddock is where that is
-- argued.
reducedManaCost :: ManaCost.ManaCost -> ManaCost.ManaCost -> ManaCost.ManaCost
reducedManaCost amount = applyAdjustments (plusReductions [amount] noAdjustments)

-- CR 601.2f with nothing in it: the board's own increases and reductions are the
-- caster's business further down this module, and rule 702.143d's amount is the
-- only thing grantedForetellCost applies.
noAdjustments :: CostAdjustments.CostAdjustments
noAdjustments =
  CostAdjustments.MkCostAdjustments
    { CostAdjustments.increases = [],
      CostAdjustments.reductions = [],
      CostAdjustments.components = []
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
costsFor :: PlayerId -> CardName.CardName -> ObjectId -> GameState -> [Cost Keyword.Type.Keyword]
costsFor pid name oid gs = fmap CandidateCost.cost (candidateCostsFor pid name oid gs)

-- costsFor's list with WHICH ability offered each candidate recorded -- the fact
-- CR 702.34a's "if the flashback cost was paid", CR 702.133a's jump-start clause
-- and CR 702.103b's rewrite of a spell cast bestowed are conditioned on. The
-- GRAVEYARD arm tags the first two, and `bestowed` below tags from every zone
-- the printed cost is offered in, which is the zone half of rule 702.103a; CR
-- 702.127a's aftermath asks about the ZONE instead and needs no tag of its own.
candidateCostsFor :: PlayerId -> CardName.CardName -> ObjectId -> GameState -> [CandidateCost.CandidateCost]
candidateCostsFor = candidateCostsGiven False

-- CR 712.11d / 613.1f: the keywords a double-faced card's FRONT face has where
-- the card lies -- its projection with the object stamped as that face, face up
-- -- rather than the ones it prints, so a disturb or more than meets the eye an
-- effect grants counts and one a layer-6 removal takes away does not. A card
-- exiled face down is read turned up, since CR 406.3a turns it up before it is
-- played: Pawl.CastPermissionSpec's "CR 406.3a / 702.162a a Ratchet exiled face
-- down with Urianger is offered and cast converted". Read by
-- the converted-face offer and price alike (Pawl.Engine.Cast.castableFacesFor,
-- candidateCostsGiven below), so the two cannot disagree. Pawl.TransformSpec's
-- "CR 702.146a a disturb granted to a card in a graveyard casts it transformed"
-- proves the grant.
frontFaceKeywords :: ObjectId -> Card.Type.Card -> GameState -> Set.Set Keyword.Type.Keyword
frontFaceKeywords oid card gs =
  let front o = o {Object.face = Just (Face.name (Card.frontFace card)), Object.facing = Facing.FaceUp, Object.exiledFaceDown = False}
   in Map.keysSet (Projection.keywordsOf oid gs {GameState.objects = Map.adjust front oid (GameState.objects gs)})

-- CR 118.8 / 601.2b: one candidate per way of paying this face's CHOICE costs --
-- Caustic Exhale's "behold a Dragon or pay {1}". Folded into every candidate
-- rather than into the printed cost alone, `withAdditional`'s reason one field
-- over: CR 118.9d sends an additional cost through an alternative one unchanged.
--
-- The PRODUCT over the face's choices, which is CR 118.8a's "any number of
-- additional costs" read at more than one: two choices on one face are four
-- candidates, each of them a whole total for CR 601.2f to lock in.
--
-- Announced and not paid: the branch is settled at CR 601.2b, by
-- Prompt.ChooseCost beside every other candidate, which is what keeps the choice
-- the CASTER's and leaves an option the board cannot pay out of the offer (CR
-- 118.3) instead of stranding the cast at CR 601.2h.
--
-- The chosen branch is not TAGGED: CandidateCost.keyword names the keyword
-- ability that offered a cost (CR 702.34a), and a choice printed inside an
-- additional cost is not one.
--
-- Nor is the branch recorded as a branch: what CR 701.4b's "if a [quality] was
-- beheld" reads is the SLOT the chosen option's own payment bound
-- (Binding.beheldObject), so no clause has to ask which candidate carried it.
choiceVariants :: Face.Face card -> CandidateCost.CandidateCost -> [CandidateCost.CandidateCost]
choiceVariants face =
  let variants candidates choice =
        [ candidate {CandidateCost.cost = plus (CandidateCost.cost candidate) option}
        | candidate <- candidates,
          option <- NonEmpty.toList (CostChoice.unwrap choice)
        ]
   in \candidate -> List.foldl' variants [candidate] (Face.additionalCostChoices face)

-- | CR 702.48a: offering, an optional ADDITIONAL cost, so a candidate is offered
-- as it stands and once more per sacrificeable [quality] permanent, which
-- carries the widened window. CR 118.9d applies it to an alternative cost too,
-- so an effect's applied cost (Pawl.Engine.Resolve.Effect's OfferCast) goes
-- through this as the board's candidates do. The victim is named outright for
-- `emerged`'s reason (CR 702.48b), and its mana cost is read off the
-- projection, so a Clone reduces by what it copied; CR 702.48c sends that
-- through CR 118.7. The offering keyword is `spellKeywords`', rule 702.48a's
-- ability functioning while the spell is on the stack.
withOffering :: PlayerId -> ObjectId -> GameState -> CandidateCost.CandidateCost -> [CandidateCost.CandidateCost]
withOffering pid oid gs candidate =
  candidate
    : [ candidate
          { CandidateCost.cost = (CandidateCost.cost candidate) {Cost.components = Cost.components (CandidateCost.cost candidate) <> [CostComponent.Sacrifice (Sacrifice.MkSacrifice 1 (Filter.Type.IsObject vid))]},
            CandidateCost.reductions = CandidateCost.reductions candidate <> [Maybe.fromMaybe (ManaCost.MkManaCost []) (Filter.manaCost (Projection.viewOfObject vid gs))],
            CandidateCost.instantSpeed = True
          }
      | quality <- Keyword.offeringQualities (Map.keysSet (spellKeywords pid oid gs)),
        vid <- Replacement.sacrificeCandidates (Just pid) Map.empty pid (Just oid) quality gs
      ]

-- | candidateCostsFor, told whether CR 601.3's permission comes from the EFFECT
-- rather than from the board.
--
-- One arm reads that rule, and so one arm needs telling: a card in a GRAVEYARD
-- is offered the printed cost and its alternatives only where some permission
-- lets this player cast it from there, since without one there is no cast to
-- price. CR 608.2g's offer is such a permission -- the effect naming the object
-- IS it, which is why Pawl.Engine.Cast.castableWhenOffered asks no zone -- and
-- it is not written anywhere on the board for
-- Pawl.Engine.PlayerEffect.mayCastFrom to find. Without this the offer priced
-- the card at nothing at all and no cast was made, which is what kept anything
-- from being cast out of a graveyard on that road (#2795).
--
-- A permission states no cost, so what it adds is exactly what a board-written
-- permission adds: the printed cost first, then each alternative (CR 118.9a),
-- untagged.
candidateCostsGiven :: Bool -> PlayerId -> CardName.CardName -> ObjectId -> GameState -> [CandidateCost.CandidateCost]
candidateCostsGiven permitted pid name oid gs =
  let -- CR 613.1: the keywords the card HAS where it lies (CR 113.6f), read
      -- once for every keyword-offered cost below rather than once per keyword
      -- -- each read projects the card, and that gathers the whole board (#435).
      keywords = Map.keysSet (Projection.keywordsOf oid gs)
      -- CR 113.6d / 601.2a: what the SPELL has once it is on the stack, which
      -- is where an ability offering an alternative cost functions -- so a
      -- grant to "spells you cast" (Hunting Velociraptor) reaches it. Read by
      -- the arms whose keyword's own rule says it functions on the stack;
      -- evoke, bestow, prototype and the graveyard's keep `keywords`, their
      -- rules naming the zone the card is cast from. Pawl.CastSpec's "CR
      -- 113.6d a Hunting Velociraptor's granted prowl buys a Ridgetop Raptor
      -- for {2}{R}" proves it.
      spell = asSpellProjected pid oid gs
      onStack = Map.keysSet (PC.keywords spell)
      -- The card-backed body the two printing-carrying arms above share. CR 601.2
      -- is why they share it rather than the copy getting a price of its own: a
      -- cast copy goes through that rule's steps like any other spell, so every
      -- alternative cost, every additional cost and every zone clause below reads
      -- the same for both.
      costsOfPrinting obj printingId = case Game.cardOfPrinting printingId gs of
        -- Unreachable: a PrintingId is minted only by Game.intern, which inserts.
        Nothing -> []
        Just card ->
          let -- CR 707.2: the costs are copiable values, so a card carrying a
              -- copy stamp or a conjured duplicate's values is priced off those
              -- (Game.castingFaceOf).
              face = Game.castingFaceNamed obj card name
              printed = Cost.MkCost {Cost.mana = Face.manaCost face, Cost.components = Face.additionalCosts face}
              -- CR 118.9d: an alternative replaces only the MANA cost; every
              -- additional cost still applies. CR 702.34a's last sentence sends
              -- flashback through the same rules, so its cost is wrapped the same.
              withAdditional alternative =
                alternative {Cost.components = Cost.components alternative <> Face.additionalCosts face}
              -- CR 604.2: an alternative cost whose "as long as" clause does not
              -- hold is not offered at all.
              --
              -- CR 109.5's "you" is the CASTER, who a card in a hand or a graveyard
              -- has no controller to supply (CR 108.4). The two coincided while
              -- Cast.zoneCandidates handed out only the caster's own pile; a
              -- permission naming somebody else's hand separates them (see #2169), and
              -- the CR 601.3 permission and the CR 118.9 grant below read the caster
              -- for the same reason.
              available alternative = case AlternativeCost.condition alternative of
                Nothing -> True
                Just cond ->
                  Condition.holds
                    (Projection.fullView gs)
                    (Filter.contextFor (Game.teams gs) (Just pid) (Just oid))
                    gs
                    oid
                    cond
              -- CR 113.6d / 613.1f: a printed alternative cost is an ability that
              -- functions on the stack, so a layer-6 wipe takes it only where
              -- it reaches the SPELL (`spell`): a perpetual one does, one
              -- confined to graveyards does not. The projection is asked only
              -- of a face printing one. Pawl.CostSpec's "CR 118.9 an
              -- Asmoranomardicadaistinaculdacar that perpetually lost all
              -- abilities has no alternative cost" and "CR 113.6d under Yixlid
              -- Jailer a Fireblast cast from the graveyard still sacrifices two
              -- Mountains" prove both.
              --
              -- A GRANTED alternative cost (Mine Security's perpetual "You may
              -- pay {0} rather than pay this spell's mana cost") is read off
              -- `spell` too, CR 113.6d's standing; the projection has already
              -- applied any wipe to it. The scenario
              -- cr-118-9-the-kavu-mine-security-conjured-is-cast-for-0 proves it.
              alternatives =
                ( case Face.alternativeCosts face of
                    [] -> []
                    printedAlternatives ->
                      if PC.lostAllAbilities spell
                        then []
                        else fmap (withAdditional . AlternativeCost.cost) (filter available printedAlternatives)
                )
                  <> fmap (withAdditional . AlternativeCost.cost) (filter available (PC.grantedAlternativeCosts spell))
              -- CR 702.103a: bestow, offered from EVERY zone the printed cost is
              -- -- "a static ability that functions in any zone from which you
              -- could play the card it's on" -- so it joins `ordinary` below
              -- rather than one arm of the case, and after the printed cost, so
              -- `firstOffered` still reads that.
              --
              -- CR 118.9d wraps it in `withAdditional`, flashback's reason: an
              -- alternative replaces only the mana cost.
              --
              -- The keywords are read off the PROJECTION for the graveyard arm's CR
              -- 613.1 reason -- an ability granted where the card lies states rule
              -- 702.103a's cost as much as a printed one -- which is the read the
              -- hand arm below does not take for its own printed alternatives.
              bestowed =
                fmap
                  (\cost -> CandidateCost.plain (Just (Keyword.Type.Bestow cost)) (withAdditional cost))
                  (Keyword.bestowCosts keywords)
              -- CR 702.160a / CR 718.3: prototype, offered from EVERY zone for
              -- bestow's reason -- CR 113.6e classes an ability that modifies how
              -- its own object can be cast as functioning "in any zone from which
              -- it could be played or cast" -- so it is appended beside the zone's
              -- own list rather than replacing it, and LAST, so `firstOffered`
              -- still reads the printed cost.
              --
              -- CR 118.9d wraps it in `withAdditional` for flashback's reason. The
              -- inset frame's POWER AND TOUGHNESS ride the tag rather than the
              -- cost: nothing about the price depends on them, and
              -- Pawl.Engine.Projection.View.withPrototype reads them back off
              -- the keyword once Pawl.Engine.Cast has stamped the choice.
              --
              -- The keywords are read off the PROJECTION, bestow's read and for
              -- rule 613.1's reason.
              prototyped =
                fmap
                  (\prototype -> CandidateCost.plain (Just (Keyword.Type.Prototype prototype)) (withAdditional Cost.MkCost {Cost.mana = Just (Prototype.cost prototype), Cost.components = []}))
                  (Keyword.prototypes keywords)
              -- CR 702.140a: mutate, offered from EVERY zone for bestow's reason
              -- -- rule 702.140a's static ability "functions while the spell with
              -- mutate is on the stack", which CR 113.6e reaches from wherever the
              -- cast begins -- so it joins the zone's own list rather than
              -- replacing it, and LAST, so `firstOffered` still reads the printed
              -- cost.
              --
              -- CR 118.9d wraps it in `withAdditional` for flashback's reason, and
              -- rule 702.140a says the same in its own words: "casting a spell
              -- using its mutate ability follows the rules for paying alternative
              -- costs".
              --
              -- The keywords are `onStack`'s, rule 702.140a's static ability
              -- functioning on the stack.
              mutated =
                fmap
                  (\cost -> CandidateCost.plain (Just (Keyword.Type.Mutate cost)) (withAdditional cost))
                  (Keyword.mutateCosts onStack)
              -- CR 702.74a, 702.109a, 702.113a, 702.148a and 702.152a: evoke,
              -- dash, blitz, cleave and awaken, offered from EVERY zone for
              -- bestow's reason -- "a static ability that functions in any zone
              -- from which the card with evoke can be cast" -- wrapped in
              -- `withAdditional` for flashback's, and tagged with the keyword
              -- itself. Evoke is read off `keywords`, bestow's read; the rest
              -- off `onStack`, each of their rules saying it functions while
              -- the spell is on the stack. The scenario
              -- cr-702-74a-an-evoke-aquatic-subtlety-perpetually-grants-in-a-hand-is-offered
              -- proves evoke GRANTED to a card in a hand is offered.
              isEvoke keyword = case keyword of
                Keyword.Type.Evoke _ -> True
                _ -> False
              evoked =
                fmap
                  (\(keyword, cost) -> CandidateCost.plain (Just keyword) (withAdditional cost))
                  (Keyword.plainAlternativeCosts (Set.filter isEvoke keywords) <> Keyword.plainAlternativeCosts (Set.filter (not . isEvoke) onStack))
              -- CR 702.119a and CR 702.119b: emerge, evoked's offer with rule
              -- 702.119a's two clauses attached -- the sacrifice in the candidate's
              -- components, the generic reduction in `CandidateCost.reductions`.
              -- Offered from every zone for bestow's reason, rule 702.119a's
              -- abilities functioning "while the spell with emerge is on the
              -- stack", which CR 113.6e reaches from wherever the cast begins; read
              -- off `onStack` for that reason and wrapped in
              -- `withAdditional` for flashback's, rule 702.119a sending the cast
              -- through CR 601.2f-h in its own words.
              --
              -- THE POOL IS RULE 702.119b's [quality] WHERE THE PAYLOAD CARRIES
              -- ONE and rule 702.119a's creatures where it does not. Rule
              -- 702.119b is the only difference between the two spellings: its
              -- victim is a [quality] PERMANENT rather than a creature, and its
              -- reduction is word for word rule 702.119a's "an amount of generic
              -- mana equal to the sacrificed permanent's mana value", so the same
              -- offer serves both. Proved by Pawl.CastSpec's "CR 702.119b the
              -- emerge cost sacrifices a permanent of the stated quality, not a
              -- creature".
              --
              -- ONE CANDIDATE PER SACRIFICEABLE VICTIM, each naming that one
              -- permanent outright (Filter.IsObject): CR 702.119c chooses the
              -- permanent "as you choose to pay a spell's emerge cost (see rule
              -- 601.2b)" and sacrifices THAT permanent at CR 601.2h, so picking
              -- the candidate is rule 702.119c's choice and the component it
              -- carries admits nothing else. CR 601.2f's amount rides the same
              -- pick, which is why it has to be made here: the reduction is the
              -- chosen permanent's mana value and the total locks before CR 601.2h.
              --
              -- What the identity buys over the mana value the amount needs: a
              -- victim eaten between the two steps -- spent to a mana ability at
              -- CR 601.2g -- leaves CR 601.2h with nothing to sacrifice and the
              -- cast reverses (CR 733.1), where a candidate naming a mana value
              -- would let a second permanent of that value pay instead
              -- (Pawl.CastSpec's "CR 702.119c an Ashnod's Altar that eats the
              -- chosen creature reverses the cast").
              --
              -- Two permanents of one mana value are therefore two candidates
              -- rather than one, and Prompt.ChooseCost tells them apart -- they
              -- differ in the object their sacrifice component names.
              --
              -- The pool is Replacement.sacrificeCandidates, so CR 701.21a's
              -- restrictions are asked once, here, rather than left to surprise
              -- CR 601.2h. Rule 701.21a is also why the criterion states no
              -- control clause -- "a player can't sacrifice ... something that's a
              -- permanent they don't control", which that pool is already the
              -- caster's, so a ControlledBy atom would only repeat it.
              emerged =
                let victims criterion =
                      Maybe.mapMaybe
                        (\vid -> fmap ((,) vid) (Filter.manaValue (Projection.viewOfObject vid gs)))
                        (Replacement.sacrificeCandidates (Just pid) Map.empty pid (Just oid) criterion gs)
                    offer emerge (vid, n) =
                      CandidateCost.MkCandidateCost
                        (Just (Keyword.Type.Emerge emerge))
                        (withAdditional (Emerge.cost emerge) {Cost.components = Cost.components (Emerge.cost emerge) <> [CostComponent.Sacrifice (Sacrifice.MkSacrifice 1 (Filter.Type.IsObject vid))]})
                        [ManaCost.MkManaCost [ManaSymbol.Generic (Integer.toNaturalSaturating n)]]
                        False
                 in concatMap
                      (\emerge -> fmap (offer emerge) (victims (Maybe.fromMaybe (Filter.Type.HasCardType CardType.Creature) (Emerge.quality emerge))))
                      (Keyword.emergeCosts onStack)
              -- CR 702.117a and CR 702.137a: surge and spectacle, evoked's offer
              -- with a GATE -- read off `onStack`, wrapped by
              -- `withAdditional` and tagged with the keyword for that list's
              -- reasons, and offered from every zone because rule 702.117a's
              -- ability functions "while the spell with surge is on the stack"
              -- and rule 702.137a's "on the stack", which CR 113.6e reaches from
              -- wherever the cast begins.
              --
              -- A clause that does not hold is no offer at all rather than an
              -- offer withheld, which is the shape `available` gives a printed
              -- alternative cost's condition and the mayhem arm below gives rule
              -- 702.187b's. Both clauses are read HERE, live at CR 601.2b, rather
              -- than off anything captured earlier.
              --
              -- The player each clause asks about is the CASTER, rule 702.117a's
              -- and rule 702.137a's "you".
              surged =
                if Game.yourTeamCastASpellThisTurn pid gs
                  then fmap (\cost -> CandidateCost.plain (Just (Keyword.Type.Surge cost)) (withAdditional cost)) (Keyword.surgeCosts onStack)
                  else []
              spectacled =
                if Game.opponentLostLifeThisTurn pid gs
                  then fmap (\cost -> CandidateCost.plain (Just (Keyword.Type.Spectacle cost)) (withAdditional cost)) (Keyword.spectacleCosts onStack)
                  else []
              -- CR 702.76a and CR 702.173a: prowl and freerunning, surged's shape
              -- with a clause of their own -- a player dealt combat damage this
              -- turn by something that was then yours. Offered from every zone
              -- for that pair's reason: both rules' static abilities function on
              -- the stack, which CR 113.6e reaches from wherever the cast begins.
              --
              -- Rule 702.76a's clause names THIS SPELL's creature types, so the
              -- gate is passed the subtypes `spell` projects on the stack rather
              -- than the printed face's -- CR 613.1, and a CR 612.2 text change
              -- or a CR 205.1b type-changing effect on the spell moves what
              -- prowl asks about. Filtered to creature types because rule 702.76a says
              -- creature types; a Kindred card's other subtypes are not offered
              -- to the comparison.
              prowled =
                if Game.prowlDamageThisTurn pid (Set.filter Subtype.isCreatureType (PC.subtypes spell)) gs
                  then fmap (\cost -> CandidateCost.plain (Just (Keyword.Type.Prowl cost)) (withAdditional cost)) (Keyword.prowlCosts onStack)
                  else []
              freerun =
                if Game.freerunningDamageThisTurn pid gs
                  then fmap (\cost -> CandidateCost.plain (Just (Keyword.Type.Freerunning cost)) (withAdditional cost)) (Keyword.freerunningCosts onStack)
                  else []
              -- CR 702.185a: warp, evoked's offer with ONE ZONE. Rule 702.185a's
              -- first static ability says "you may cast this card FROM YOUR
              -- HAND", where evoke's, dash's and blitz's name none, so this is
              -- joined at the hand arm below rather than into `ordinary`. That
              -- is what keeps a warped card in exile priced at its printed cost:
              -- rule 702.185a's second ability grants that cast a PERMISSION and
              -- states no cost of its own.
              --
              -- Read off `onStack`, rule 702.185a's abilities functioning on the
              -- stack, wrapped by `withAdditional` for evoked's reason, and
              -- tagged with the keyword itself, which is
              -- what Keyword.resolutionDelayedAbility reads back off
              -- Object.castUsing to arm rule 702.185a's exile.
              warped =
                fmap
                  (\cost -> CandidateCost.plain (Just (Keyword.Type.Warp cost)) (withAdditional cost))
                  (Keyword.warpCosts onStack)
              -- CR 712.11d: the face this card may be cast TRANSFORMED or CONVERTED
              -- as, which is what Pawl.Engine.Card.convertedFaceGiven answers and
              -- what Pawl.Engine.Cast.castableFacesFor offers. Asked through that
              -- function, of the same front-face keywords, rather than against
              -- Card.backFace so the pricing below cannot come to disagree with the
              -- offer about which half either rule reaches.
              front = frontFaceKeywords oid card gs
              isConvertedFace = fmap Face.name (Card.convertedFaceGiven front card) == Just (Face.name face)
              -- CR 702.162a: more than meets the eye, read from EVERY zone for
              -- bestow's reason -- "a static ability that functions in any zone from
              -- which the spell may be cast".
              --
              -- Offered only for the BACK face, which is what rule 702.162a buys:
              -- "you may cast this card CONVERTED by paying [cost]", and CR 712.11a
              -- says a card cast converted is put on the stack with its back face
              -- up. Pawl.Engine.Card.castableFaces is what puts that face on the
              -- table (CR 712.11d); this prices it.
              --
              -- The keywords are the FRONT face's, which is CR 712.11d's own scope:
              -- the ability is "an ability of a double-faced card's front face". So
              -- they are `front`, the projection with the object stamped as that
              -- face, rather than the projection of the half being proposed, which
              -- carries the BACK face's keywords.
              converted =
                if isConvertedFace
                  then
                    fmap
                      (\cost -> CandidateCost.plain (Just (Keyword.Type.MoreThanMeetsTheEye cost)) (withAdditional cost))
                      (Keyword.moreThanMeetsTheEyeCosts front)
                  else []
              -- CR 702.146a: disturb, `converted`'s offer with rule 702.146a's ZONE
              -- attached -- "you may cast this card transformed FROM YOUR GRAVEYARD
              -- by paying [cost] rather than its mana cost". Read off `front` and
              -- scoped to the back face for that list's CR 712.11d reasons, and
              -- wrapped in `withAdditional` for flashback's.
              --
              -- The graveyard half is asked at `orConverted` below rather than here,
              -- beside the zone it names; the CR 601.3 permission that matches it is
              -- Pawl.Engine.Cast.permitsDisturb's.
              disturbed =
                fmap
                  (\cost -> CandidateCost.plain (Just (Keyword.Type.Disturb cost)) (withAdditional cost))
                  (Keyword.disturbCosts front)
              -- The converted face's candidates REPLACE the zone's own list rather
              -- than joining it: the back face is a candidate at all only because
              -- rule 702.162a's or rule 702.146a's permission put it there, so that
              -- permission's cost is the only route to casting it. CR 712.11 makes
              -- the FRONT face the default and CR 712.11a names the transformed cast
              -- as the way to a back face; no rule offers a nonmodal back face for
              -- its own printed cost. Reached only for that face, so each zone arm
              -- below is otherwise exactly as it was.
              --
              -- Rule 702.146a's cost is offered in a GRAVEYARD and nowhere else,
              -- which is the clause rule 702.162a does not state; a disturb card's
              -- back face proposed from a hand is therefore offered nothing and is
              -- not castable, where its front face is priced by the `_` arm below
              -- like any other card in a hand.
              --
              -- Dropping the printed cost is unobservable in this pool either way:
              -- no nonmodal back face prints a mana cost, so the candidate this drops
              -- would have been CR 118.6's unpayable one. Written because the
              -- alternative is a cast the rules do not permit; a printing whose back
              -- face had a mana cost would be the card that told the two apart.
              orConverted zoneCandidates =
                if isConvertedFace
                  then converted <> (if Game.zoneOf oid gs == Just Zone.Graveyard then disturbed else [])
                  else zoneCandidates
              -- CR 118.9a / 601.2b: the keyword alternatives ride BESIDE the printed
              -- cost, wherever the permission that lets the card be cast admits
              -- paying its printed cost or an alternative -- the hand, the `_` arm,
              -- and a graveyard some permission opens. Not where the permission
              -- itself fixes the cost: a plotted card (CR 702.170d), a foretold one
              -- (CR 702.143a), a free CR 118.9 grant, flashback's or escape's own
              -- cost, or rule 702.162a's converted cast. Pawl.CastSpec's "CR
              -- 702.170d a Mulldrifter Aven Interrupter plotted is offered no evoke
              -- cost" proves it.
              ordinary = fmap untagged (printed : alternatives) <> bestowed <> prototyped <> mutated <> evoked <> emerged <> surged <> spectacled <> prowled <> freerun
           in -- CR 118.8 / 601.2b: every candidate below owes this face's choice
              -- costs, whichever of them the caster announces, so the expansion
              -- wraps the whole list rather than any one arm of it. The zone is
              -- Game.zoneOf's, so a CR 707.13 copy outside the game (CR 400.11)
              -- takes the `_` arm. A regression fence: none of Garth One-Eye's
              -- six cards prints a cost the graveyard arm offers.
              concatMap (withOffering pid oid gs) . concatMap (choiceVariants face) . orConverted $ case Game.zoneOf oid gs of
                -- Three shapes, differing in what they do to the printed cost, plus
                -- an effect's permission. Flashback (CR 702.34a) REPLACES the mana
                -- cost, so it is wrapped by `withAdditional`, and escape (CR
                -- 702.138a) and mayhem (CR 702.187b) are that same shape in their
                -- own rules' words; aftermath (CR 702.127a) replaces nothing, so it
                -- is `printed`; jump-start (CR 702.133a) and retrace (CR 702.81a)
                -- ADD a discard to `printed`, one however many such abilities the
                -- card has; and CR 601.3 / Yawgmoth's Will is an EFFECT stating no
                -- cost, offering the hand's list BESIDE the rest rather than instead
                -- of them. Rule 702.34a's "if the resulting spell is an instant or
                -- sorcery spell" gates the PERMISSION (Keyword.permissionsFor) and is
                -- not re-asked here; rule 702.138a and rule 702.81a state no such
                -- clause to gate, and rule 702.187b's own clause is asked here
                -- rather than there, at the mayhem offer below.
                Just Zone.Graveyard ->
                  let -- `keywords` is what the card HAS in the graveyard, not
                      -- what it prints: an ability granted there (CR 113.6f)
                      -- states rule 702.34a's cost as much as a printed one. Read
                      -- off the OBJECT, so the caller's CR 709.3a half is measured.
                      --
                      -- The flashback keyword AS IT WAS READ: rule 702.34a's ability
                      -- and its cost are one sentence, so the cost is what
                      -- distinguishes one instance from another.
                      flashback cost = CandidateCost.plain (Just (Keyword.Type.Flashback cost)) (withAdditional cost)
                      -- CR 702.138a's cost, read as flashback's is and wrapped by
                      -- `withAdditional` for the same reason -- rule 702.138a replaces
                      -- the mana cost and CR 601.2f-h still adds the card's additional
                      -- costs on top. Its "exile N other cards from your graveyard"
                      -- rides in the Cost's own components, which is why no arm here
                      -- spells it; the "other" needs no exclusion, exileCandidates'
                      -- CR 601.2a note below.
                      escape cost = CandidateCost.plain (Just (Keyword.Type.Escape cost)) (withAdditional cost)
                      -- CR 702.187b's cost, read as flashback's is and wrapped by
                      -- `withAdditional` for the same reason, rule 702.187b sending
                      -- the cast through CR 601.2f-h in its own words. Offered only
                      -- while its own "as long as you discarded this card this turn"
                      -- holds -- the shape `available` gives a printed alternative
                      -- cost's condition above: a clause that does not hold is not an
                      -- offer withheld, it is no offer at all. The discarder asked
                      -- about is the CASTER, rule 702.187b's "you".
                      mayhem cost = CandidateCost.plain (Just (Keyword.Type.Mayhem (Just cost))) (withAdditional cost)
                      -- CR 702.180a's FIRST and SECOND static abilities, `emerged`
                      -- above one characteristic over: the tap rides in the
                      -- candidate's components and the generic reduction in
                      -- CandidateCost.reductions. Read off the projection and
                      -- wrapped in `withAdditional` for flashback's reason, rule
                      -- 702.180a's last sentence sending the cast through CR
                      -- 601.2f-h in its own words.
                      --
                      -- ONE CANDIDATE PER TAPPABLE CREATURE, `emerged`'s offer
                      -- above one keyword over and for its reasons: CR 702.180b
                      -- chooses the creature "as you choose to pay a spell's
                      -- harmonize cost (see rule 601.2b)" and taps THAT creature
                      -- as the cost is paid, so the criterion names it outright
                      -- (Filter.IsObject) and CR 601.2f's amount -- its power --
                      -- rides the same pick. A creature tapped for mana at CR
                      -- 601.2g therefore reverses the cast (CR 733.1) rather than
                      -- letting a second creature of that power pay
                      -- (Pawl.CastSpec's "CR 702.180b tapping the chosen creature
                      -- for mana reverses the cast").
                      --
                      -- The identity does not replace the description: the
                      -- criterion keeps `untappedYours`, so the payment re-asks
                      -- rule 702.180a's "untapped creature you control" of the
                      -- named creature at CR 601.2h.
                      --
                      -- PLUS the zero-creature candidate, first, which is rule
                      -- 702.180a's "UP TO one untapped creature you control": the
                      -- cost unreduced, tapping nothing. `emerged` has no analogue,
                      -- rule 702.119a's sacrifice not being optional.
                      --
                      -- A creature with power 0 or less still gets a candidate of
                      -- its own, reducing by nothing: rule 702.180a's reduction is
                      -- "an amount of generic mana", and CR 107.1b uses
                      -- zero where a calculation yields a negative number, so the
                      -- amount saturates there while the criterion still pins that
                      -- creature. Unobservable in this
                      -- pool -- such a candidate costs what the zero-creature one
                      -- costs and differs only in the tap -- and a card that cared
                      -- whether a creature was tapped would be what told them
                      -- apart.
                      --
                      -- The pool is `tapCandidates`, the same one the payment draws
                      -- on, so the creatures offered are exactly the creatures
                      -- payable; its perspective is the payer, which is what lets
                      -- rule 702.180a's "you control" be an atom here.
                      harmonized =
                        let untappedYours = [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not Filter.Type.IsTapped, Filter.Type.ControlledBy PlayerRelation.You]
                            criterion vid = Filter.Type.And (untappedYours <> [Filter.Type.IsObject vid])
                            tappable =
                              Maybe.mapMaybe
                                (\vid -> fmap ((,) vid) (Filter.power (Projection.viewOfObject vid gs)))
                                (tapCandidates Map.empty pid oid (Filter.Type.And untappedYours) gs)
                            tappingOne cost (vid, n) =
                              CandidateCost.MkCandidateCost
                                (Just (Keyword.Type.Harmonize cost))
                                (withAdditional cost {Cost.components = Cost.components cost <> [CostComponent.TapPermanents (TapPermanents.MkTapPermanents 1 (criterion vid) False)]})
                                [ManaCost.MkManaCost [ManaSymbol.Generic (Integer.toNaturalSaturating n)]]
                                False
                            tappingNone cost = CandidateCost.plain (Just (Keyword.Type.Harmonize cost)) (withAdditional cost)
                         in concatMap
                              (\cost -> tappingNone cost : fmap (tappingOne cost) tappable)
                              (Keyword.harmonizeCosts keywords)
                   in fmap flashback (Keyword.flashbackCosts keywords)
                        <> fmap escape (Keyword.escapeCosts keywords)
                        <> (if Game.discardedThisTurnBy pid oid gs then fmap mayhem (Keyword.mayhemCosts keywords) else [])
                        <> harmonized
                        <> ( if Keyword.hasRetrace keywords
                               then
                                 [ CandidateCost.plain
                                     (Just Keyword.Type.Retrace)
                                     -- CR 702.81a ADDS to the printed cost rather than
                                     -- replacing it, jump-start's shape below, and
                                     -- names a QUALITY where rule 702.133a names none.
                                     printed {Cost.components = Cost.components printed <> [CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.HasCardType CardType.Land))]}
                                 ]
                               else []
                           )
                        <> (if Keyword.hasAftermath keywords then [CandidateCost.plain (Just Keyword.Type.Aftermath) printed] else [])
                        <> ( if Keyword.hasJumpStart keywords
                               then
                                 [ CandidateCost.plain
                                     (Just Keyword.Type.JumpStart)
                                     -- CR 702.133a's cost names no quality -- "discard a
                                     -- card" -- so the criterion admits everything.
                                     printed {Cost.components = Cost.components printed <> [CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))]}
                                 ]
                               else []
                           )
                        -- UNTAGGED: an effect's permission states no cost, so
                        -- neither rule 702.34a's clause nor rule 702.133a's is
                        -- satisfied by paying it. CR 702.187c's costless mayhem
                        -- states no cost either, so it opens the same list. A
                        -- regression fence: its one printing is a land, which is
                        -- played rather than cast (MTGJSON 2026-08-23, text
                        -- "Mayhem (You may play": Oscorp Industries alone).
                        <> (if permitted || PlayerEffect.mayCastFrom pid Zone.Graveyard oid gs || PlayerEffect.mayPlayByMayhem pid oid gs then ordinary else [])
                -- CR 702.170d: a PLOTTED card is cast "without paying its mana
                -- cost", CR 118.9's alternative cost. INSTEAD of the printed cost,
                -- rule 702.170d being the only thing permitting this cast. CR
                -- 715.3d's permission states no cost and falls through to the `_`
                -- arm; Effect.GrantPlayFromExile's states one only when it carries
                -- an alternative cost, which is the arm two below.
                Just Zone.Exile
                  | Maybe.isJust (Object.plotted obj) -> [untagged (withoutPayingManaCost face)]
                -- CR 702.143a: a FORETOLD card is cast for its foretell cost, CR
                -- 118.9's alternative cost, wrapped by withAdditional as flashback's
                -- is. INSTEAD of the printed cost, the plotted arm's reason.
                --
                -- "ANY foretell cost it has" is plural, and CR 702.143d is why: an
                -- effect may give a foretold card a cost of its own (Ethereal
                -- Valkyrie), which grantedForetellCost settles from the reduction
                -- that effect stated, beside whatever the card's own foretell
                -- keyword prints, each of which is its own cost (Synthetic
                -- Twice-Foretold Omen). All are offered, the granted one first. A
                -- card foretold with NEITHER is offered nothing and is not
                -- castable, which is that clause read at zero.
                --
                -- The granted cost is settled against THIS face, which is the whole
                -- reason the object carries the reduction rather than a cost: CR
                -- 712.11b lets a modal double-faced card be cast as either face, and
                -- the two faces print different mana costs (Birgi, God of
                -- Storytelling // Harnfel, Horn of Bounty). `face` is the face this
                -- call was asked about.
                Just Zone.Exile
                  | Maybe.isJust (Object.foretold obj) ->
                      fmap
                        (untagged . withAdditional)
                        (Maybe.maybeToList (grantedForetellCost face obj) <> Maybe.mapMaybe (foretellCostFor face) (Keyword.foretellCosts (Face.keywordSet face)))
                -- CR 118.9a: a CR 601.3 permission that states an alternative
                -- cost -- "without paying its mana cost" (Extract Power), rule
                -- 701.65a's {2}, or a waterbend (Hama, the Bloodbender) --
                -- REPLACES the printed cost for the plotted arm's reason: that
                -- permission is the only thing making this cast legal, so the
                -- cost it states is the only route to it. CR 118.5
                -- still makes the caster announce and pay it.
                --
                -- Scoped to the permission's OWN holder, whom
                -- Pawl.Types.ExilePlayPermission baked in, and to its window
                -- (Expiry.permissionOpen): a player casting the same card under
                -- some other permission is priced by the `_` arm below.
                Just Zone.Exile
                  | Just permission <- Object.playableFromExile obj,
                    Expiry.permissionOpen pid permission gs,
                    Just alternative <- ExilePlayPermission.alternativeCost permission ->
                      [untagged (permissionCost alternative face)]
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
                Just Zone.Hand ->
                  ordinary
                    <> warped
                    <> (if PlayerEffect.mayCastFromHandWithoutPayingManaCost pid oid gs then [untagged (withoutPayingManaCost face)] else [])
                _ -> ordinary
   in case Game.lookupObject oid gs of
        Nothing -> []
        Just obj | Facing.isFaceDown (Object.facing obj) -> [untagged faceDownCost]
        Just obj -> case Object.source obj of
          Source.OfCard printingId -> costsOfPrinting obj printingId
          -- CR 701.42a puts a melded permanent onto the battlefield rather than onto
          -- the stack, so it is never announced and there is no cost to offer for it.
          Source.OfMeld _ -> []
          -- CR 730.2 merges an object into a permanent on the battlefield, so a merged
          -- permanent is never announced either.
          Source.OfMerge _ -> []
          Source.OfToken _ -> []
          Source.OfAbility _ -> []
          Source.OfTrigger _ -> []
          Source.OfEmblem _ -> []
          -- CR 707.10: "a copy of a spell isn't cast", so it is never announced and
          -- there is no cost to offer for it.
          Source.OfSpellCopy _ -> []
          -- A copy of a card is CAST -- CR 722.3c's "the prepared permanent's
          -- controller may cast the copy", CR 707.12's "cast a copy of an object" --
          -- so CR 601.2 announces it like any other spell and it is priced exactly as
          -- the card-backed arm above prices a card, off whichever printing its source
          -- names.
          Source.OfCardCopy printingId -> costsOfPrinting obj printingId
          Source.OfInherentTrigger _ -> []

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
total pid oid cost gs = totalWith (spellAdjustments Set.empty pid oid gs) cost

-- CR 601.2f: the reductions a CANDIDATE COST brings with it
-- (Pawl.Types.CandidateCost's @reductions@) folded into the adjustments the board
-- states -- rule 702.119a's, rule 702.180a's and rule 702.48a's.
--
-- Unfloored and unconfined, `spellAdjustments`' reading of Thrasta's sentence:
-- rule 702.119a states neither restriction, so CR 601.2f's own {0} and CR
-- 118.7b-d's spill both stand.
plusReductions :: [ManaCost.ManaCost] -> CostAdjustments.CostAdjustments -> CostAdjustments.CostAdjustments
plusReductions amounts adjustments =
  adjustments
    { CostAdjustments.reductions =
        CostAdjustments.reductions adjustments
          <> fmap (\amount -> AppliedReduction.MkAppliedReduction amount 0 False) amounts
    }

-- CR 601.2f's increases and reductions for a SPELL being cast: the ones CARDS
-- generate (Pawl.Engine.PlayerEffect, plus the spell's own text through
-- selfReductions below) plus CR 903.8's commander tax and the increase an exile
-- permission states (permissionIncrease). The tax joins the
-- INCREASES rather than the printed mana cost, rule 903.8 wording it "plus {2}
-- for each previous time", so a reduction still applies afterwards.
--
-- `targets` is CR 601.2c's announcement, which the spell's own sentence may
-- read (selfReductions), and so may another object's per-target change
-- (PlayerEffect.perTargetCount); empty for a caller standing before CR 601.2c,
-- where neither applies.
spellAdjustments :: Set.Set Recipient.Recipient -> PlayerId -> ObjectId -> GameState -> CostAdjustments.CostAdjustments
spellAdjustments targets pid oid gs =
  let adjustments = PlayerEffect.spellCostAdjustments targets pid oid gs
      self = selfReductions (Set.fromList (Maybe.mapMaybe Recipient.objectOf (Set.toList targets))) pid oid gs
      withSelf =
        adjustments
          { CostAdjustments.reductions =
              -- Floored at zero and never confined to coloured mana: Thrasta's
              -- and Ertai's Scorn's sentences state neither restriction, so CR
              -- 601.2f's own {0} and CR 118.7b-d's spill both stand.
              CostAdjustments.reductions adjustments
                <> [AppliedReduction.MkAppliedReduction amount 0 False | (CostDirection.Less, amount) <- self],
            -- Dragon's Prey's "costs {2} more": generic mana, the only kind an
            -- increase carries (Pawl.Types.CostAdjustments.increases).
            CostAdjustments.increases =
              CostAdjustments.increases adjustments
                <> [sum [n | ManaSymbol.Generic n <- ManaCost.unwrap amount] | (CostDirection.More, amount) <- self]
          }
      owed = filter (/= 0) [Commander.tax pid oid gs, permissionIncrease pid oid gs]
   in withSelf {CostAdjustments.increases = owed <> CostAdjustments.increases withSelf}

-- CR 601.2f: the increase the CR 601.3 permission an exiled `oid` carries puts on
-- `pid`'s cast of it -- Lightstall Inquisitor's "each spell cast this way costs
-- {1} more". Zero for a card anywhere else, or one whose permission is not
-- `pid`'s or not open (Expiry.permissionOpen), candidateCostsGiven's exile arm's
-- scoping.
--
-- Asked of the PRE-MOVE card, Commander.tax's reason: the permission is on the
-- card in exile and CR 400.7's spell on the stack has none, so this answers the
-- gate here and Pawl.Engine.Cast.castSpellWith folds it into the candidates
-- before CR 601.2a's move (raiseCandidate).
permissionIncrease :: PlayerId -> ObjectId -> GameState -> Natural
permissionIncrease pid oid gs = case Game.lookupObject oid gs of
  Just obj
    | Object.zone obj == Zone.Exile,
      Just permission <- Object.playableFromExile obj,
      Expiry.permissionOpen pid permission gs ->
        ExilePlayPermission.increase permission
  _ -> 0

-- CR 601.2f: permissionIncrease added to one candidate's mana part,
-- Commander.taxCandidates' shape and reason. A cost with no mana part stays
-- unpayable.
raiseCandidate :: PlayerId -> ObjectId -> GameState -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
raiseCandidate pid oid gs cost =
  let owed = permissionIncrease pid oid gs
      add (ManaCost.MkManaCost symbols) = ManaCost.MkManaCost (symbols <> [ManaSymbol.Generic owed])
   in if owed == 0 then cost else cost {Cost.mana = fmap add (Cost.mana cost)}

-- CR 601.2f / 113.6d: the reductions a spell's OWN text applies to its
-- own cost -- the sentence Thrasta, Tempest's Roar prints out, and the one CR
-- 702.41a's affinity and CR 702.125a's undaunted state as a keyword
-- (Keyword.selfCostReductionsOf) -- each Quantity evaluated and its amount
-- REPEATED that many times rather than multiplied, so a typed amount falls out
-- with no arithmetic. A NEGATIVE or UNDETERMINABLE Quantity contributes nothing.
-- Each comes back with its direction, a "costs more" sentence being the same
-- shape (Dragon's Prey).
--
-- CR 601.2c's announced `targets` answer a sentence's whichTargets: ANY object
-- target matching, against its own view with the spell as the source and `pid`
-- as the perspective. Pawl.CostSpec's "a cost that reads the spell's targets"
-- group proves it.
--
-- The PRINTED reductions come straight off the object rather than through a
-- projection: it is the half Cast.asProposed already stamped (CR 709.3b), with
-- its copy stamp's costs laid over it (Game.castingFaceOf, CR 707.2). They are
-- abilities (CR 113.6d), so a layer-6 wipe in force on the spell takes them
-- (CR 613.1f) -- Pawl.CostSpec's "CR 601.2f a Thrasta that perpetually lost all
-- abilities is not reduced" proves it. The GRANTED ones (CR 613.1f) are read
-- off the projection, which is where layer 6 records them -- Pawl.CostSpec's
-- Richlau, Headmaster group proves it. The KEYWORDS are the projection's too,
-- printed and granted alike (CR 702.41a, CR 702.125a), so a spell given
-- affinity is reduced by it -- Pawl.CostSpec's Mycosynth Golem group proves
-- it.
selfReductions :: Set.Set ObjectId -> PlayerId -> ObjectId -> GameState -> [(CostDirection.CostDirection, ManaCost.ManaCost)]
selfReductions targets pid oid gs =
  let -- CR 109.5: the perspective is the would-be controller, `pid` -- not
      -- Projection.controllerOf, which answers Nothing for a card in a hand.
      -- The source is the spell itself, the reduction being printed on it.
      context = Filter.contextFor (Game.teams gs) (Just pid) (Just oid)
      -- CR 601.2f: a conditional reduction is asked here, as the total is
      -- determined, against the same perspective as its count.
      applies reduction =
        all (Condition.holds (Projection.fullView gs) context gs oid) (CostReduction.condition reduction)
          && all (\wanted -> any (\target -> Filter.matches context (Projection.viewOfObject target gs) wanted) (Set.toList targets)) (CostReduction.whichTargets reduction)
      scaled reduction =
        let copies = Quantity.evaluate (Projection.fullView gs) context gs oid (CostReduction.perEach reduction)
            -- Saturating rather than partial: an Int cannot hold every Integer.
            -- A negative saturates to 0, the floor the header states.
            times n = concat (replicate (max 0 (Integer.toIntSaturating n)) (ManaCost.unwrap (CostReduction.amount reduction)))
         in fmap ((,) (CostReduction.direction reduction) . ManaCost.MkManaCost . times) copies
   in Maybe.mapMaybe scaled (filter applies (selfSentences pid oid gs))

-- The sentences selfReductions reads, printed, granted and keyword alike. The
-- granted and keyword ones are `asSpellProjected`'s, so the keywords are
-- `spellKeywords`' reading.
selfSentences :: PlayerId -> ObjectId -> GameState -> [CostReduction.CostReduction]
selfSentences pid oid gs = case (Game.lookupObject oid gs, Game.cardOf oid gs, Game.faceOf oid gs) of
  (Just obj, Just card, Just printedFace) ->
    let face = Game.castingFaceOf obj card printedFace
        projected = asSpellProjected pid oid gs
        printed = if PC.lostAllAbilities projected then [] else Face.costReductions face
     in printed <> PC.grantedCostReductions projected <> Keyword.selfCostReductionsOf (PC.keywords projected)
  _ -> []

-- CR 601.2a / 613.1f: the keywords of the spell `pid` is casting, printed and
-- granted alike, with how many instances of each -- the one reader for what the
-- spell has once it is on the stack: its cost reductions (affinity, undaunted),
-- its additional costs (kicker, casualty, buyback, entwine, escalate), its mana
-- substitutes (convoke, delve, improvise) and assist. Pawl.CostSpec's Mycosynth
-- Golem and Chief Engineer groups and Pawl.KeywordTriggerSpec's "CR 702.153a a
-- granted casualty copies Lightning Bolt" prove a grant reaching it.
--
-- What the CARD has where it lies, before the move -- the keywords that permit
-- the cast from that zone (Cast.projectedKeywords) -- is a different question,
-- asked of the proposal board. candidateCostsGiven asks both, each alternative
-- cost of the one its keyword's rule names.
spellKeywords :: PlayerId -> ObjectId -> GameState -> Map.Map Keyword.Type.Keyword Natural
spellKeywords pid oid gs = PC.keywords (asSpellProjected pid oid gs)

-- The spell's projection off `asSpell`'s board.
asSpellProjected :: PlayerId -> ObjectId -> GameState -> PC.ProjectedCharacteristics
asSpellProjected pid oid gs = Projection.project oid (asSpell pid oid gs)

-- CR 601.2a / 601.2f: a card whose cast is being proposed (Game.beingCast) is
-- still in its old zone on a gate's board, but its total is determined with it
-- on the stack under its caster, so its abilities are projected there: an
-- effect confined to the old zone no longer reaches it. Pawl.CostSpec's "CR
-- 601.2f undaunted the graveyard card lost applies to the spell" (Yixlid
-- Jailer) proves it. Every other board, the payment's among them, is returned
-- unchanged.
asSpell :: PlayerId -> ObjectId -> GameState -> GameState
asSpell pid oid gs
  | Game.beingCast gs oid =
      let moved = Game.withoutBeingCast gs
       in moved {GameState.objects = Map.adjust (\o -> o {Object.zone = Zone.Stack, Object.enteredUnder = Just pid}) oid (GameState.objects moved)}
  | otherwise = gs

-- Whether the spell's CR 601.2f adjustments read CR 601.2c's targets -- one of
-- its own cost sentences (Bury in Books), or another object's per-target change
-- (PlayerEffect.spellCostReadsTargets) -- so a gate measuring the cost before
-- them has to search the aimings (Pawl.Engine.Cast.payableCostAt).
readsTargets :: PlayerId -> ObjectId -> GameState -> Bool
readsTargets pid oid gs =
  any (Maybe.isJust . CostReduction.whichTargets) (selfSentences pid oid gs)
    || PlayerEffect.spellCostReadsTargets pid oid gs

-- CR 601.2f's adjustments for an ACTIVATION cost, which CR 602.2b routes
-- through rule 601.2b-i like a spell's. No commander tax: CR 903.8 taxes
-- CASTING a commander, and an activation is not a cast.
--
-- Reached by a MANA ability's cost too, through manaActivationAdjustments: CR
-- 605.3b gives one no stack window, so it gathers these for itself rather than
-- through Pawl.Engine.Activate.
--
-- The Keyword is the ability's PROVENANCE -- the stamp
-- Pawl.Types.ActivatedAbility.keyword carries, threaded from the caller that has
-- the ability in hand; see activationCostAdjustments for what it narrows.
--
-- The ObjectIds beside it are CR 601.2c's ANNOUNCED TARGETS, threaded the same
-- way and read by the same gather -- empty for every caller standing before CR
-- 601.2c, which is where a reducer naming a target (Dwarven Mauler) simply does
-- not apply.
activationAdjustments :: Set.Set ObjectId -> Maybe Keyword.Type.Keyword -> AbilityKind.AbilityKind -> LoyaltyKind.LoyaltyKind -> PlayerId -> ObjectId -> GameState -> CostAdjustments.CostAdjustments
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
          else concatMap (\r -> fmap (r :) (orders (List.delete r rs))) (ListUtils.nubOrd rs)
      withTotal order =
        let reordered = adjustments {CostAdjustments.reductions = order}
         in (reordered, applyAdjustments reordered manaCost)
      manaValue (_, ManaCost.MkManaCost symbols) = sum (fmap Quantity.symbolValue symbols)
      candidates = List.sortOn manaValue (fmap withTotal (orders (CostAdjustments.reductions adjustments)))
   in case ListUtils.nubOrdOn snd candidates of
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
-- Quantity.symbolColors, so CR 107.4e's and CR 107.4f's count too, on THIS
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
        CostScale.PerColoredSymbol color -> length (filter (elem color . Quantity.symbolColors) symbols)
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

-- CR 702.51b / 702.66b / 702.126b: the ways a keyword on this object lets part of
-- an already TOTALLED mana cost be paid by spending something else -- tapping a
-- permanent, or exiling a card from a graveyard -- rather than with mana. One
-- entry per distinct set of symbols the substitutes could cover, each a residual
-- mana cost paired with the substitutes spent and how many of each; the FIRST
-- entry substitutes nothing, which is rule 702.51a's "you may" said of every
-- symbol at once.
--
-- A COSTCOMPONENT and not a cost reduction, which is CR 702.51b read twice: the
-- substitution is no part of CR 601.2f's total, so a convoked or delved spell's
-- mana value is unchanged, and the tap or the exile is a payment the payer makes
-- rather than arithmetic. Carrying it as a component is also what puts it on the
-- ClaimAxis beside every other claim on those objects (`claimOf`), so one
-- creature cannot both convoke and be tapped for mana and one card cannot be
-- delved away twice, and what makes `payComponent`'s existing arms ask WHICH
-- objects (`substituteComponent`).
--
-- ONE component per symbol KIND and per substitute, not one per object spent:
-- rule 702.51a's eligibility is a function of the symbol, so the green symbols'
-- taps and the generic ones' are two pools Hall's condition has to weigh against
-- each other (Pawl.Engine.Claim.satisfiable) -- a creature that is green serves
-- either, and a creature that is not serves only the generic one.
--
-- The count offered is CAPPED by how many objects the substitute's criterion
-- admits. Not a correctness condition -- `jointlyPayable` is what refuses an
-- unsatisfiable set -- but an enumeration bound: a {8} cost beside two creatures
-- offers three entries rather than nine.
--
-- The keywords are `spellKeywords`', so a granted convoke (Chief Engineer)
-- substitutes as a printed one does.
--
-- The OFFERS come in two provenances and this function holds only the keywords'.
-- CR 701.67a's waterbend is the other, and it rides the COST (CostComponent.Waterbend)
-- rather than the object, which is what lets rule 701.67b scope it to one
-- component of the total: `waterbendOffers` caps it at the waterbend cost's own
-- generic amount where a keyword's offer is capped only by the symbol.
manaSubstitutions :: [CostComponent.CostComponent Keyword.Type.Keyword] -> Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> GameState -> ManaCost.ManaCost -> [(ManaCost.ManaCost, [(Keyword.Substitute, Natural)])]
manaSubstitutions components slots pid oid gs =
  let keywords = Map.keysSet (spellKeywords pid oid gs)
   in substitutionsOffering (\symbol -> fmap (\substitute -> (substitute, Nothing)) (Keyword.manaSubstitutesFor symbol keywords) <> waterbendOffers components symbol) slots pid oid gs

-- CR 701.67a's half of the offer alone, which is what every payment but a
-- cast gets -- an activation, and CR 118.12's resolution-time payment of a ward
-- or an unless cost: CR 702.51a, CR 702.66a and CR 702.126a all function while
-- a SPELL is on the stack, so no keyword of the source's reaches those costs,
-- where a waterbend cost is one component of the cost being paid and says so
-- itself.
--
-- The answer for a cost stating no waterbend is exactly one entry substituting
-- nothing -- `offers` is empty, so the product is the empty vector -- which is
-- the answer every such payment had before rule 701.67a arrived.
waterbendSubstitutions :: [CostComponent.CostComponent Keyword.Type.Keyword] -> Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> GameState -> ManaCost.ManaCost -> [(ManaCost.ManaCost, [(Keyword.Substitute, Natural)])]
waterbendSubstitutions components = substitutionsOffering (waterbendOffers components)

-- CR 701.67a as an offer, and CR 701.67b as the CEILING on it: "for each generic
-- mana in that cost", where `that cost` is the waterbend cost and not the total
-- cost it is part of. Nothing where the cost states no waterbend.
--
-- SUMMED over the components, since two waterbend costs on one total cost would
-- each license their own generic. Arithmetic rather than proven behaviour: no
-- card in Scryfall `oracle:waterbend`, 2026-09-19, states two, and Waterbender
-- Ascension would refute it by stating a second.
waterbendOffers :: [CostComponent.CostComponent Keyword.Type.Keyword] -> ManaSymbol.ManaSymbol -> [(Keyword.Substitute, Maybe Natural)]
waterbendOffers components symbol =
  let allowance = sum [n | CostComponent.Waterbend n <- components] + sum [n | CostComponent.WaterbendInstead n <- components]
   in case symbol of
        ManaSymbol.Generic _ | allowance > 0 -> [(Keyword.TapUntapped waterbendCriterion, Just allowance)]
        _ -> []

-- Rule 701.67a's "an untapped artifact or creature you control", MINTED here for
-- Pawl.Engine.Keyword.manaSubstitutesFor's reason: the rule's own words are what
-- say what is eligible, so CR 612.2 has no word of a card's to swap in it and
-- Pawl.CardSpec's filter traversals never see it.
waterbendCriterion :: Filter.Type.Filter Keyword.Type.Keyword
waterbendCriterion =
  Filter.Type.And
    [ Filter.Type.Or [Filter.Type.HasCardType CardType.Artifact, Filter.Type.HasCardType CardType.Creature],
      Filter.Type.Not Filter.Type.IsTapped,
      Filter.Type.ControlledBy PlayerRelation.You
    ]

-- The body both offers share: one entry per set of symbols the substitutes could
-- cover. Each offer carries its own CEILING beside the symbol's own count -- rule
-- 701.67b's, or Nothing where the rule stating the substitute bounds it by the
-- symbol alone.
substitutionsOffering :: (ManaSymbol.ManaSymbol -> [(Keyword.Substitute, Maybe Natural)]) -> Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> GameState -> ManaCost.ManaCost -> [(ManaCost.ManaCost, [(Keyword.Substitute, Natural)])]
substitutionsOffering offersFor slots pid oid gs manaCost =
  let -- The cost's symbols as one entry per KIND, a Generic counting for its own
      -- amount (CR 107.4b) where every other symbol is one mana.
      sizeOf symbol = case symbol of
        ManaSymbol.Generic n -> (ManaSymbol.Generic 1, n)
        other -> (other, 1)
      kinds = Map.toAscList (Map.fromListWith (+) (fmap sizeOf (ManaCost.unwrap manaCost)))
      offered (symbol, n) =
        [ (symbol, n, substitute, minimum (n : Natural.length (substituteCandidates slots pid oid substitute gs) : Maybe.maybeToList ceiling_))
        | (substitute, ceiling_) <- offersFor symbol
        ]
      offers = concatMap offered kinds
      -- The cartesian product over how many of each offer is substituted for,
      -- ascending, which is what puts the substitute-nothing entry first.
      vectors = filter withinCost (traverse (\(symbol, n, substitute, cap) -> fmap (\k -> (symbol, n, substitute, k)) [0 .. cap]) offers)
      -- One symbol kind can carry TWO offers -- convoke and delve both substitute
      -- for generic mana -- and rule 702.51a's "for each generic mana" bounds the
      -- two together rather than apiece. A FENCE: the only printing that states
      -- both is Hogaak, Arisen Necropolis, which cannot be transcribed (see
      -- Pawl.Engine.Keyword.manaSubstitutesFor), so no card in `data/cards/`
      -- reaches a vector this drops. A spell's waterbend cost beside one of
      -- those keywords is the other pair, bounded here the same way.
      withinCost vector = all (\(symbol, n, _, _) -> sum [k | (s, _, _, k) <- vector, s == symbol] <= n) vector
      entry vector =
        ( List.foldl' (\acc (symbol, _, _, k) -> withoutMana symbol k acc) manaCost vector,
          [(substitute, k) | (_, _, substitute, k) <- vector, k > 0]
        )
   in fmap entry vectors

-- The objects a Keyword.Substitute may be paid with, per its arm: CR 702.51a's
-- and CR 702.126a's out of the battlefield, CR 702.66a's out of the payer's own
-- graveyard. Read for its SIZE by `substitutionsOffering` above, which is how
-- many of a symbol kind the offer can reach.
substituteCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Keyword.Substitute -> GameState -> [ObjectId]
substituteCandidates slots pid oid substitute gs = case substitute of
  Keyword.TapUntapped criterion -> tapCandidates slots pid oid criterion gs
  Keyword.TapToConvoke criterion -> tapCandidates slots pid oid criterion gs
  Keyword.ExileFromGraveyard criterion -> exileCandidates slots pid oid criterion gs

-- The component that SPENDS this many of a Keyword.Substitute's objects. Carrying
-- the substitution as a component is what puts it on the same ClaimAxis as every
-- other claim on those objects (`claimOf`) and what makes `payComponent`'s
-- existing arm ask WHICH ones -- Prompt.ChooseTaps for the one, and
-- Prompt.ChooseExilesFromGraveyard for the other.
substituteComponent :: Keyword.Substitute -> Natural -> CostComponent.CostComponent Keyword.Type.Keyword
substituteComponent substitute n = case substitute of
  Keyword.TapUntapped criterion -> CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion False)
  Keyword.TapToConvoke criterion -> CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion False)
  Keyword.ExileFromGraveyard criterion -> CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion)

-- The components an offer's substitutes are paid as, `substituteComponent` over
-- each.
substituteComponents :: [(Keyword.Substitute, Natural)] -> [CostComponent.CostComponent Keyword.Type.Keyword]
substituteComponents = fmap (uncurry substituteComponent)

-- CR 702.51c: does spending this substitute convoke the spell?
convokes :: Keyword.Substitute -> Bool
convokes substitute = case substitute of
  Keyword.TapToConvoke _ -> True
  Keyword.TapUntapped _ -> False
  Keyword.ExileFromGraveyard _ -> False

-- A SUBSTITUTION's mana halves folded into a TOTALLING, which is the shape
-- Mana.announce's `total` parameter takes. The offer arrives as a parameter
-- because the two carriers state different ones -- `manaSubstitutions` for a
-- cast and `waterbendSubstitutions` for everything else. That offer decides whether to ask
-- which half of a hybrid symbol is announced, and it asks only where two halves
-- are payable -- so a totalling blind to CR 702.51b would find NO half payable on
-- a Merrow Skyswimmer ({3}{W/U}{W/U}, convoke) cast off nothing but creatures,
-- and rule 601.2b's choice would be made by the fallback instead of by the payer.
--
-- The CLAIMS are dropped, unlike the gate's, and the direction is deliberate:
-- this decides what to ASK. A route offered whose taps cannot all be satisfied
-- fails a payment CR 601.2h would have failed anyway, where a route withheld is a
-- choice taken away from the payer.
--
-- PROVEN, not a fence: Pawl.CostSpec's "CR 601.2b the payer announces a convoked
-- spell's hybrid halves, both blue" reddens on a bare `total_`, Merrow Skyswimmer
-- being the transcribable printing that states convoke beside a hybrid symbol.
substitutedManas :: (ManaCost.ManaCost -> [(ManaCost.ManaCost, [(Keyword.Substitute, Natural)])]) -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> ManaCost.ManaCost -> [ManaCost.ManaCost]
substitutedManas substitute total_ manaCost = concatMap (fmap fst . substitute) (total_ manaCost)

-- This much of ONE kind of mana taken out of a cost: a generic amount comes off
-- the generic symbols in printed order, and any other kind drops that many
-- occurrences of itself. A TOTALLED cost is canonical -- `applyAdjustments`
-- leaves the generic component as one leading symbol -- so the generic walk sees
-- at most one, and it is written for the general case anyway because `plus`
-- concatenates two mana parts without canonicalising.
withoutMana :: ManaSymbol.ManaSymbol -> Natural -> ManaCost.ManaCost -> ManaCost.ManaCost
withoutMana kind n (ManaCost.MkManaCost symbols) =
  let generic left rest = case rest of
        [] -> []
        ManaSymbol.Generic m : more
          | left == 0 -> rest
          | m > left -> ManaSymbol.Generic (m - left) : more
          | otherwise -> generic (left - m) more
        other : more -> other : generic left more
      typed left rest = case rest of
        [] -> []
        symbol : more
          | left > 0 && symbol == kind -> typed (left - 1) more
          | otherwise -> symbol : typed left more
   in ManaCost.MkManaCost
        ( case kind of
            ManaSymbol.Generic _ -> generic n symbols
            _ -> typed n symbols
        )

-- CR 118.3 asked of the non-mana half alone: every component payable on its own,
-- and all of them payable TOGETHER out of the objects they draw on. The two
-- questions the mana-side gates pair with `Mana.canPayCommittingGiven`, shared so
-- that a gate and an offer cannot ask different ones of a set of components one
-- of them assembled.
componentsPayable :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> GameState -> Bool
componentsPayable slots pid oid components gs =
  all (\component -> canPayComponent slots pid oid component gs) components
    && jointlyPayable slots pid oid components gs

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

-- CR 101.4's "Then the actions happen simultaneously" for one player's several
-- CR 118.12 payments: `plus` over them, with every fixed life payment summed
-- into ONE PayLife, so the life is paid as one event rather than one per offer
-- -- Killing Wave's ruling, "each player pays life and sacrifices creatures at
-- the same time".
together :: NonEmpty.NonEmpty (Cost Keyword.Type.Keyword) -> Cost Keyword.Type.Keyword
together costs =
  let summed = foldr1 plus costs
      life = sum [n | CostComponent.PayLife n <- Cost.components summed]
      others = filter (\component -> case component of CostComponent.PayLife _ -> False; _ -> True) (Cost.components summed)
   in summed {Cost.components = others <> [CostComponent.PayLife life | life > 0]}

-- CR 702.24a's "[cost] for each age counter on it": N whole copies of this cost,
-- as ONE cost. `plus` folded over itself, so the mana parts concatenate and the
-- components are appended in order -- which is also rule 702.24a's "each choice
-- is made separately for each age counter", since a replicated component is
-- chosen for again when the payment reaches it.
--
-- THE FOLD'S UNIT IS TAKEN FROM `cost`, which is the whole of #2875: a cost with
-- no mana part is CR 118.6's UNPAYABLE one, and this function must not be the
-- thing that makes it payable. A constant `Just (MkManaCost [])` unit answered
-- {0} at n == 0 whatever it was handed, and Cost.canPay admits {0}; `plus`
-- already carried Nothing through for every n >= 1, so only the empty fold ever
-- lost it.
--
-- CR 118.6a is the direction and not a ruling on the zero case, which no
-- printing states: that rule keeps an unpayable cost unpayable when an effect
-- increases it or adds to it, and its one exception is an ALTERNATIVE cost,
-- which a multiplier is not. Nothing else in rule 118 turns an unpayable cost
-- payable, so unpayable in, unpayable out, at every count.
--
-- N of zero over a PAYABLE cost is {0} with no components, which CR 118.5 makes
-- real and payable and is what "for each" of nothing means. Rule 702.24a reaches
-- neither zero case -- the age counter is put on before the offer -- but
-- Pawl.Types.PayGate.perEach is stated over every count.
repeated :: Natural -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
repeated n cost = foldr plus (Cost.MkCost (fmap (const (ManaCost.MkManaCost [])) (Cost.mana cost)) []) (List.genericReplicate n cost)

-- CR 601.2b: substitute the chosen value of X everywhere in this cost -- the mana
-- part's ManaSymbol.Variable, and the components' CostComponent.PayLifeX and its
-- siblings. BOTH
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
  CostComponent.PayEnergyX -> CostComponent.PayEnergy x
  CostComponent.PayLife _ -> component
  CostComponent.PayHalfLife _ -> component
  CostComponent.TapThis -> component
  CostComponent.UntapThis -> component
  CostComponent.SacrificeThis -> component
  CostComponent.ReturnThis -> component
  CostComponent.Sacrifice {} -> component
  CostComponent.TapForTotalPower {} -> component
  CostComponent.TapPermanents {} -> component
  CostComponent.ReturnPermanents {} -> component
  CostComponent.ExilePermanents {} -> component
  CostComponent.DiscardCards {} -> component
  CostComponent.DiscardThis _ -> component
  CostComponent.PutCardFromHandOntoBattlefield _ -> component
  CostComponent.PayEnergy _ -> component
  CostComponent.AddLoyaltyToThis _ -> component
  CostComponent.RemoveLoyaltyFromThis _ -> component
  CostComponent.RemoveLoyaltyFromThisX -> CostComponent.RemoveLoyaltyFromThis x
  CostComponent.RemoveCountersFromThis _ -> component
  CostComponent.RemoveCounters {} -> component
  CostComponent.RemovePlusOneCountersX criterion -> CostComponent.RemoveCounters (CountersFromPermanents.MkCountersFromPermanents x (WhichCounters.OfKind CounterKind.PlusOnePlusOne) criterion CounterSpread.FromAmong)
  CostComponent.SacrificeX criterion -> CostComponent.Sacrifice (Sacrifice.MkSacrifice x criterion)
  CostComponent.PutPlusOneCountersOnThis _ -> component
  CostComponent.Blight _ -> component
  CostComponent.Forage -> component
  CostComponent.FlipCoin -> component
  -- CR 702.174a's cost names no X.
  CostComponent.ChooseOpponent -> component
  -- BlightX's rewrite one keyword action over: the announcement fixes CR
  -- 701.67b's ceiling exactly as it fixes the {X} that licence scopes.
  CostComponent.WaterbendX -> CostComponent.Waterbend x
  -- The amount is already fixed: a waterbend cost written with X is
  -- WaterbendX above until the announcement rewrites it to this arm.
  CostComponent.Waterbend _ -> component
  CostComponent.WaterbendInstead _ -> component
  -- PayLifeX's rewrite one keyword action over: CR 107.3a gives ONE announced
  -- value to the whole cost, so Soul Immolation's "blight X" takes the same X a
  -- mana cost's {X} would have taken.
  CostComponent.BlightX -> CostComponent.Blight x
  CostComponent.ExileThisFromGraveyard -> component
  CostComponent.ExileThis -> component
  CostComponent.ExileCardsFromGraveyard {} -> component
  CostComponent.ExileMaterials {} -> component
  CostComponent.ExileTopFromGraveyard _ -> component
  CostComponent.CollectEvidence _ -> component
  -- CR 601.2f's computed amount, not CR 601.2b's X: fixComputed reads it once the
  -- targets exist.
  CostComponent.CollectEvidenceOfTargets -> component
  CostComponent.ExileCardFromHand _ -> component
  CostComponent.RevealCardFromHand _ -> component
  CostComponent.Behold _ -> component
  CostComponent.BeholdAndExile _ -> component
  CostComponent.MillCards _ -> component

-- Does this cost contain an X (CR 107.3)? What decides whether the caster is
-- asked for a value at CR 601.2b. BOTH HALVES: CR 601.2b names the mana cost as
-- an EXAMPLE and CR 107.3a lists the additional cost beside it, so Hatred, whose
-- only X is in "pay X life", is asked exactly as Blaze is.
hasVariable :: Cost Keyword.Type.Keyword -> Bool
hasVariable cost = manaHasVariable cost || any componentHasVariable (Cost.components cost)

-- CR 107.3b, asked of ONE of CR 601.2b's candidates: whether announcing this
-- cost leaves X the caster's to name or fixes it at 0. The same predicate
-- Cast.castProposed asks before raising Prompt.ChooseX, so a gate that reads
-- this and the announcement itself cannot disagree about which cost has an X.
variableChoice :: Cost Keyword.Type.Keyword -> VariableChoice.VariableChoice
variableChoice cost = if hasVariable cost then VariableChoice.Announced else VariableChoice.FixedAtZero

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
  CostComponent.PayEnergyX -> True
  CostComponent.PayLife _ -> False
  CostComponent.PayHalfLife _ -> False
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.SacrificeThis -> False
  CostComponent.ReturnThis -> False
  CostComponent.Sacrifice {} -> False
  CostComponent.TapForTotalPower {} -> False
  CostComponent.TapPermanents {} -> False
  CostComponent.ReturnPermanents {} -> False
  CostComponent.ExilePermanents {} -> False
  CostComponent.DiscardCards {} -> False
  CostComponent.DiscardThis _ -> False
  CostComponent.PutCardFromHandOntoBattlefield _ -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.AddLoyaltyToThis _ -> False
  CostComponent.RemoveLoyaltyFromThis _ -> False
  CostComponent.RemoveLoyaltyFromThisX -> True
  CostComponent.RemoveCountersFromThis _ -> False
  CostComponent.RemoveCounters {} -> False
  CostComponent.RemovePlusOneCountersX _ -> True
  CostComponent.SacrificeX _ -> True
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.Blight _ -> False
  CostComponent.BlightX -> True
  -- Nullary: CR 701.61a states no number at all, so there is nothing for CR
  -- 601.2b to announce.
  CostComponent.Forage -> False
  CostComponent.FlipCoin -> False
  CostComponent.ChooseOpponent -> False
  CostComponent.WaterbendX -> True
  CostComponent.Waterbend _ -> False
  CostComponent.WaterbendInstead _ -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileThis -> False
  CostComponent.ExileCardsFromGraveyard {} -> False
  CostComponent.ExileMaterials {} -> False
  CostComponent.ExileTopFromGraveyard _ -> False
  CostComponent.CollectEvidence _ -> False
  CostComponent.CollectEvidenceOfTargets -> False
  CostComponent.ExileCardFromHand _ -> False
  CostComponent.RevealCardFromHand _ -> False
  CostComponent.Behold _ -> False
  CostComponent.BeholdAndExile _ -> False
  CostComponent.MillCards _ -> False

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
  -- CR 118.3 measures the announced amount against the energy counters the
  -- player has, so a big enough X refuses -- PayLifeX's arm above and for its
  -- reason. Pawl.CardSpec's CR 101.1 sweep reads this answer now that it covers
  -- activation costs: Sphinx of the Revelation's carries this component and
  -- states no ceiling, so False here would make that printing an offender.
  CostComponent.PayEnergyX -> True
  -- FALSE, and that is CR 701.68b rather than an omission: the rule refuses a
  -- blight only where the player controls no creature, and names no number of
  -- counters that is too many. So a Soul Immolation announcement is refused by
  -- CR 101.1's sentence alone.
  CostComponent.BlightX -> False
  CostComponent.PayLife _ -> False
  CostComponent.PayHalfLife _ -> False
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.SacrificeThis -> False
  CostComponent.ReturnThis -> False
  CostComponent.Sacrifice {} -> False
  CostComponent.TapForTotalPower {} -> False
  CostComponent.TapPermanents {} -> False
  CostComponent.ReturnPermanents {} -> False
  CostComponent.ExilePermanents {} -> False
  CostComponent.DiscardCards {} -> False
  CostComponent.DiscardThis _ -> False
  CostComponent.PutCardFromHandOntoBattlefield _ -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.AddLoyaltyToThis _ -> False
  CostComponent.RemoveLoyaltyFromThis _ -> False
  -- CR 606.6 measures the announced X against the loyalty counters present, so
  -- a big enough X refuses.
  CostComponent.RemoveLoyaltyFromThisX -> True
  CostComponent.RemoveCountersFromThis _ -> False
  CostComponent.RemoveCounters {} -> False
  -- CR 118.3 measures the announced count against the +1\/+1 counters the
  -- criterion admits between them, so a big enough X refuses.
  CostComponent.RemovePlusOneCountersX _ -> True
  -- CR 701.21a: one permanent per sacrifice, so an X past the matching
  -- permanents the payer controls refuses.
  CostComponent.SacrificeX _ -> True
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.Blight _ -> False
  CostComponent.Forage -> False
  CostComponent.FlipCoin -> False
  CostComponent.ChooseOpponent -> False
  -- False, Waterbend's answer below: the licence itself demands nothing,
  -- and the mana the announcement grows is the cost's own mana part,
  -- which `manaHasVariable` answers for.
  CostComponent.WaterbendX -> False
  CostComponent.Waterbend _ -> False
  CostComponent.WaterbendInstead _ -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileThis -> False
  CostComponent.ExileCardsFromGraveyard {} -> False
  CostComponent.ExileMaterials {} -> False
  CostComponent.ExileTopFromGraveyard _ -> False
  CostComponent.CollectEvidence _ -> False
  CostComponent.CollectEvidenceOfTargets -> False
  CostComponent.ExileCardFromHand _ -> False
  CostComponent.RevealCardFromHand _ -> False
  CostComponent.Behold _ -> False
  CostComponent.BeholdAndExile _ -> False
  CostComponent.MillCards _ -> False

-- CR 101.1: the ceiling this face's own words put on CR 601.2b's announced X --
-- Soul Immolation's "X can't be greater than the greatest toughness among
-- creatures you control". Nothing where the face states none, which is every
-- other printing in `data/cards/`.
--
-- The LEAST of them where the face states more than one, which CR 101.2 settles
-- rather than this function: each sentence is a "can't" and beats the permission
-- on its own, so a value both must satisfy is bounded by the smaller. The face
-- that carries two is CR 709.4c's combined view of a split card whose halves
-- each print one, priced as CR 702.102b's fused split spell.
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
maximumX pid oid face = ceilingOf pid oid (Face.maximumX face)

-- The same reduction over ceilings a caller has already gathered, shared with
-- Pawl.Engine.Activate: CR 602.2b routes an activation through rule 601.2b, so an
-- ability's own ceilings (Pawl.Types.ActivatedAbility.maximumX) are read exactly
-- as a face's are, off the ability's source and its activator.
ceilingOf :: PlayerId -> ObjectId -> [Quantity.Type.Quantity] -> GameState -> Maybe Natural
ceilingOf pid oid quantities gs =
  let context = Filter.contextFor (Game.teams gs) (Just pid) (Just oid)
      evaluated quantity = Integer.toNaturalSaturating (Maybe.fromMaybe 0 (Quantity.evaluate (Projection.fullView gs) context gs oid quantity))
   in case fmap evaluated quantities of
        [] -> Nothing
        ceilings -> Just (minimum ceilings)

-- CR 118.13: a mana symbol payable in multiple ways has its payment chosen by
-- the payer -- CR 107.4f's Phyrexian symbol and both of CR 107.4e's hybrids.
-- WHEN is the caller's, this function being the seam all of rule 118.13's
-- moments share: rule 118.13a's as the spell or ability is proposed (CR 601.2b),
-- one step before CR 601.2f's total, rule 118.13b's immediately before a cost
-- paid during a resolution is paid (Pawl.Engine.Resolve.Effect.payGatePaidBy), and rule
-- 118.13c's immediately before a special action's cost is
-- (Pawl.Engine.FaceDown.turnFaceUp and its five siblings). `announceToll` below
-- is the same choice at a moment rule 118.13 states none for.
--
-- The life the announcement committed becomes a CostComponent.PayLife, making
-- the returned cost CR 601.2b's "nonhybrid equivalent cost" in full (CR 107.4f,
-- CR 119.4). The Natural returned beside it is how many of CR 107.4f's symbols
-- took that route -- CR 400.7d's cost record, which rule 702.150a's compleated
-- reads off the permanent (Pawl.Types.Object.phyrexianLifePaid). Only
-- Pawl.Engine.Cast stores it: rule 702.150a asks about "the player who CAST it",
-- so an activation's and a resolution-time payment's answers are discarded by
-- their callers. `lifeOwedBy`'s sum also goes IN, as the life this cost owes OUTSIDE
-- its mana part -- without it a route the player cannot afford gets offered. A
-- PayHalfLife is fixed to its number first (`fixHalfLife`).
--
-- `total` is CR 601.2f's totalling, the CALLER's to supply, and it must be the
-- SAME cost the caller's own gate measured: against the printed cost a reduction
-- could hide a route and this function elide the prompt. It answers a LIST
-- because CR 118.7e's choice of half is not made until CR 601.2f. `spending` is
-- CR 118.14's permission, here for the same reason.
announce :: PaymentSubject.PaymentSubject -> ManaSpending.ManaSpending -> PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> Game (Cost Keyword.Type.Keyword, Natural)
announce subject spending pid oid total_ printed = do
  gs <- State.get
  let cost = fixHalfLife pid gs printed
  case Cost.mana cost of
    -- CR 118.6: an object with no mana cost has no mana symbols to announce.
    Nothing -> pure (cost, 0)
    Just manaCost -> do
      -- The claims are read here rather than inside Mana.announce, which cannot
      -- reach claimOf -- this module imports that one, not the other way about.
      (announced, life, paidWithLife) <- Mana.announce subject (midPayment (manaActivationsGiven (PlayerEffect.applying pid gs))) spending pid oid total_ (lifeOwedBy pid gs (Cost.components cost)) (energyOwedBy (Cost.components cost)) (claimsOf Map.empty pid oid (Cost.components cost) gs) manaCost
      pure
        ( cost
            { Cost.mana = Just announced,
              Cost.components =
                Cost.components cost <> (if life > 0 then [CostComponent.PayLife life] else [])
            },
          paidWithLife
        )

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
-- two things separate them, a floored reduction beside an unfloored one
-- (Heartstone and Blossoming Tortoise on an animated Mishra's Foundry) and a
-- reduction confined to coloured mana beside one that is not (Edgewalker beside
-- a typed reducer that prints no such sentence). The cheapest is offered first,
-- so it is also the default a short transcript replays.
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
-- Generic. Nothing for every symbol with one way to reduce, and DEDUPLICATED so
-- the degenerate `Hybrid t t` offers one half, not two.
--
-- CR 107.4f's Phyrexian symbol is NOT here: CR 118.7f gives such a reduction one
-- mana of the symbol's colour with no choice, which reducingManaTypeOf reads
-- directly. The HYBRID Phyrexian symbol is here, as its two colours: CR 107.4f
-- makes it both, so CR 118.7f's one mana of its colour needs one chosen, and CR
-- 118.7e gives that choice to the payer. Its 2 life is no half, since neither
-- rule reduces a cost by life.
reductionHalvesOf :: ManaSymbol.ManaSymbol -> Maybe [ManaSymbol.ManaSymbol]
reductionHalvesOf symbol = case symbol of
  ManaSymbol.Generic _ -> Nothing
  ManaSymbol.OfType _ -> Nothing
  ManaSymbol.Hybrid (Hybrid.MkHybrid a b) -> Just (ListUtils.nubOrd [ManaSymbol.OfType a, ManaSymbol.OfType b])
  ManaSymbol.MonocoloredHybrid manaType ->
    Just [ManaSymbol.OfType manaType, ManaSymbol.Generic Mana.monocoloredHybridGeneric]
  ManaSymbol.Phyrexian _ -> Nothing
  ManaSymbol.HybridPhyrexian (HybridPhyrexian.MkHybridPhyrexian a b) ->
    Just (ListUtils.nubOrd [ManaSymbol.OfType (ManaType.Colored a), ManaSymbol.OfType (ManaType.Colored b)])
  ManaSymbol.Snow -> Nothing
  -- {X} offers no halves to choose between, CR 107.3 making it a value a player
  -- announces rather than a way of paying one symbol. Reached with the symbol
  -- still unsubstituted wherever a PRINTED cost is read ahead of any
  -- announcement, which grantedForetellCost does; a cost being TOTALLED never
  -- carries one, CR 601.2b preceding CR 601.2f.
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

-- The same question in the shape a cost adjustment asks it
-- (Pawl.Types.AddActivationCost.whichLoyalty, Pawl.Types.LoyaltyKind).
--
-- Asked of the PRINTED cost, never of the total: Carth the Lion's own addition
-- is a loyalty component, so a reading taken after CR 601.2f folded the
-- adjustments in would make every ability it touched a loyalty ability and tax
-- itself into applying. Pawl.Engine.Activatable.loyaltyOk reads CR 606.3 off the
-- printed cost for that reason too.
loyaltyKindOf :: Cost Keyword.Type.Keyword -> LoyaltyKind.LoyaltyKind
loyaltyKindOf cost = if isLoyaltyCost cost then LoyaltyKind.LoyaltyAbility else LoyaltyKind.NonLoyaltyAbility

-- CR 606.2 reads the symbol, not its number, so an unannounced [-X] is one too.
isLoyaltyComponent :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
isLoyaltyComponent component = component == CostComponent.RemoveLoyaltyFromThisX || Maybe.isJust (loyaltyAmountOf component)

-- The SIGNED amount of loyalty a component moves, positive for CR 606.4's adding
-- half and negative for the removing one. `isLoyaltyComponent` above is this
-- without the number, plus the unannounced [-X], so the two cannot drift
-- apart. An Integer and not a Natural: CR 606.5's combining sums the two halves
-- against each other.
--
-- EXHAUSTIVE with no wildcard, `orderSensitive`'s posture and for its reason.
loyaltyAmountOf :: CostComponent.CostComponent Keyword.Type.Keyword -> Maybe Integer
loyaltyAmountOf component = case component of
  CostComponent.AddLoyaltyToThis n -> Just (toInteger n)
  CostComponent.RemoveLoyaltyFromThis n -> Just (negate (toInteger n))
  -- Nothing until CR 601.2b substitutes it; `isLoyaltyComponent` classifies it
  -- all the same.
  CostComponent.RemoveLoyaltyFromThisX -> Nothing
  CostComponent.TapThis -> Nothing
  CostComponent.UntapThis -> Nothing
  CostComponent.SacrificeThis -> Nothing
  CostComponent.ReturnThis -> Nothing
  CostComponent.PayLife _ -> Nothing
  CostComponent.PayHalfLife _ -> Nothing
  CostComponent.PayLifeX -> Nothing
  CostComponent.PayEnergyX -> Nothing
  CostComponent.Sacrifice {} -> Nothing
  CostComponent.TapForTotalPower {} -> Nothing
  CostComponent.TapPermanents {} -> Nothing
  CostComponent.ReturnPermanents {} -> Nothing
  CostComponent.ExilePermanents {} -> Nothing
  CostComponent.DiscardCards {} -> Nothing
  CostComponent.DiscardThis _ -> Nothing
  CostComponent.PutCardFromHandOntoBattlefield _ -> Nothing
  CostComponent.PayEnergy _ -> Nothing
  -- Nothing, and this is the LOAD-BEARING arm of the component: CR 606.4's
  -- loyalty symbol is what CR 606.2 reads to call an ability a loyalty ability,
  -- and a Just here would ration Barkhide Troll's ability to once a turn (CR
  -- 606.3), gate it on the sorcery window and feed it to `combineLoyalty` (CR
  -- 606.5). Removing a +1\/+1 counter is none of that. Proven, not a fence:
  -- Pawl.CostSpec's Barkhide Troll cases both redden on a Just here.
  CostComponent.RemoveCountersFromThis _ -> Nothing
  -- Nothing, the arm above's reason unchanged by the counters coming off
  -- another permanent: CR 606.4's loyalty symbol is what makes a loyalty
  -- ability, and this is a +1\/+1 counter.
  CostComponent.RemoveCounters {} -> Nothing
  CostComponent.RemovePlusOneCountersX _ -> Nothing
  CostComponent.SacrificeX _ -> Nothing
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing
  CostComponent.Blight _ -> Nothing
  CostComponent.BlightX -> Nothing
  CostComponent.Forage -> Nothing
  CostComponent.FlipCoin -> Nothing
  CostComponent.ChooseOpponent -> Nothing
  CostComponent.WaterbendX -> Nothing
  CostComponent.Waterbend _ -> Nothing
  CostComponent.WaterbendInstead _ -> Nothing
  CostComponent.ExileThisFromGraveyard -> Nothing
  CostComponent.ExileThis -> Nothing
  CostComponent.ExileCardsFromGraveyard {} -> Nothing
  CostComponent.ExileMaterials {} -> Nothing
  CostComponent.ExileTopFromGraveyard _ -> Nothing
  CostComponent.CollectEvidence _ -> Nothing
  CostComponent.CollectEvidenceOfTargets -> Nothing
  CostComponent.ExileCardFromHand _ -> Nothing
  CostComponent.RevealCardFromHand _ -> Nothing
  CostComponent.Behold _ -> Nothing
  CostComponent.BeholdAndExile _ -> Nothing
  CostComponent.MillCards _ -> Nothing

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
--
-- Only NUMBERED components combine: `plusComponents` runs after `substituteX`,
-- so an unannounced [-X] never reaches here, and one that did is left alone for
-- canPayComponent to refuse rather than silently summed as 0.
combineLoyalty :: [CostComponent.CostComponent Keyword.Type.Keyword] -> [CostComponent.CostComponent Keyword.Type.Keyword]
combineLoyalty components = case break numbered components of
  (_, []) -> components
  (before, _ : after) ->
    let net = sum (Maybe.mapMaybe loyaltyAmountOf components)
        combined =
          if net < 0
            then CostComponent.RemoveLoyaltyFromThis (Integer.toNaturalSaturating (negate net))
            else CostComponent.AddLoyaltyToThis (Integer.toNaturalSaturating net)
     in before <> (combined : filter (not . numbered) after)
  where
    numbered = Maybe.isJust . loyaltyAmountOf

-- CR 113.6m's COST half: an ability whose cost moves the object it's on out of a
-- particular zone functions only in that zone. The "or effect" half is
-- Pawl.Engine.EffectZone, and Activatable.zoneFunctionedFrom joins them.
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
  -- CR 113.6m again, and Nothing for SacrificeThis' reason above: it moves the
  -- object off the BATTLEFIELD, where CR 113.6's default already had it, so
  -- naming the zone would be redundant rather than wrong. A FENCE and not proven
  -- behaviour, for exactly that reason -- answering Just Zone.Battlefield leaves
  -- the suite green, the two readings agreeing wherever CR 113.6's default holds.
  CostComponent.ReturnThis -> Nothing
  CostComponent.SacrificeThis -> Nothing
  CostComponent.ExileThis -> Nothing
  CostComponent.PayLife _ -> Nothing
  CostComponent.PayHalfLife _ -> Nothing
  CostComponent.PayLifeX -> Nothing
  CostComponent.PayEnergyX -> Nothing
  CostComponent.Sacrifice {} -> Nothing
  -- These tap permanents that stay on the battlefield, so nothing moves out of
  -- any zone and CR 113.6's default stands.
  CostComponent.TapForTotalPower {} -> Nothing
  CostComponent.TapPermanents {} -> Nothing
  -- Nothing, and NOT Just Zone.Battlefield: CR 113.6m is about an ability that
  -- moves THE OBJECT IT'S ON, and this moves OTHER permanents.
  CostComponent.ReturnPermanents {} -> Nothing
  CostComponent.ExilePermanents {} -> Nothing
  -- Nothing, and NOT Just Zone.Graveyard: rule 113.6m again, and these move
  -- OTHER cards.
  CostComponent.ExileCardsFromGraveyard {} -> Nothing
  CostComponent.ExileTopFromGraveyard _ -> Nothing
  CostComponent.CollectEvidence _ -> Nothing
  CostComponent.CollectEvidenceOfTargets -> Nothing
  -- Nothing for the arms above's reason and one more: CR 702.167a's component
  -- moves objects out of TWO zones, so there is no single zone to name even if
  -- rule 113.6m asked about them.
  CostComponent.ExileMaterials {} -> Nothing
  CostComponent.DiscardCards {} -> Nothing
  -- Nothing, and NOT Just Zone.Hand as DiscardThis above answers, for the same
  -- reason the arms above give: CR 113.6m asks about an ability that moves THE
  -- OBJECT IT'S ON, and these move another card out of the payer's hand.
  CostComponent.PutCardFromHandOntoBattlefield _ -> Nothing
  CostComponent.ExileCardFromHand _ -> Nothing
  -- Nothing for a further reason than the arms above: CR 701.20b moves no card
  -- at all, so there is no zone for CR 113.6m to be told about.
  CostComponent.RevealCardFromHand _ -> Nothing
  -- Nothing, the arm above's reason: CR 701.4a moves no object at all, whichever
  -- of its two halves the payer takes, so there is no zone for CR 113.6m to be
  -- told about.
  CostComponent.Behold _ -> Nothing
  -- Nothing, and NOT Just Zone.Hand or Zone.Battlefield: CR 113.6m asks about an
  -- ability that moves THE OBJECT IT'S ON, and this exiles another object.
  CostComponent.BeholdAndExile _ -> Nothing
  -- Nothing, and NOT Just Zone.Library, for the arms above's reason: CR 701.17a
  -- mills the cards on top of the paying player's library, which are OTHER cards
  -- than the object the cost is on -- a Millikin on the battlefield is not in the
  -- library it mills.
  CostComponent.MillCards _ -> Nothing
  CostComponent.PayEnergy _ -> Nothing
  CostComponent.AddLoyaltyToThis _ -> Nothing
  CostComponent.RemoveLoyaltyFromThis _ -> Nothing
  CostComponent.RemoveLoyaltyFromThisX -> Nothing
  -- CR 122.1's counter is a marker and not an object, and CR 122.2 has counters
  -- "simply cease to exist" rather than travel, so removing one moves nothing out
  -- of any zone and CR 113.6's battlefield default stands. A FENCE and not proven
  -- behaviour, as ReturnThis' answer above is: Barkhide Troll's ability functions
  -- on the battlefield under either answer, so nothing separates them.
  CostComponent.RemoveCountersFromThis _ -> Nothing
  -- Nothing for the arm above's reason and one more: the counters come off
  -- ANOTHER permanent, which CR 113.6m does not ask about either way.
  CostComponent.RemoveCounters {} -> Nothing
  CostComponent.RemovePlusOneCountersX _ -> Nothing
  CostComponent.SacrificeX _ -> Nothing
  -- CR 122.6 puts counters on a permanent already where it is, so nothing moves
  -- out of any zone.
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing
  CostComponent.Blight _ -> Nothing
  CostComponent.BlightX -> Nothing
  -- Nothing, and NOT Just Zone.Graveyard: rule 113.6m asks about an ability that
  -- moves THE OBJECT IT'S ON, and CR 701.61a moves OTHER cards -- the
  -- ExileCardsFromGraveyard arm above's answer, for its reason, and the Food half
  -- is Sacrifice's.
  CostComponent.Forage -> Nothing
  CostComponent.FlipCoin -> Nothing
  CostComponent.ChooseOpponent -> Nothing
  CostComponent.WaterbendX -> Nothing
  CostComponent.Waterbend _ -> Nothing
  CostComponent.WaterbendInstead _ -> Nothing

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
  -- One of the six True-capable arms: CR 701.9a discards from the HAND, CR
  -- 400.2's hidden zone, and the criterion is the rule's stated quality --
  -- Magmatic Insight's "discard a land card" states one, Cathartic Reunion's
  -- "discard two cards" does not.
  CostComponent.DiscardCards d -> Filter.statesAQuality (DiscardCards.whichCards d)
  -- The second: CR 406.2's exile reads the same hidden hand, so the criterion
  -- decides -- Jhoira of the Ghitu's "a nonland card" states a quality,
  -- Cadaverous Bloom's "a card" does not.
  CostComponent.ExileCardFromHand criterion -> Filter.statesAQuality criterion
  -- The third: CR 701.20a reveals out of the same hidden hand, and the criterion
  -- decides -- Living Destiny's "a creature card" states a quality. CR 701.20b
  -- leaving the card in the hand does not matter here: rule 118.8c asks what the
  -- cost's cards are described BY, not where they end up.
  CostComponent.RevealCardFromHand criterion -> Filter.statesAQuality criterion
  -- The fourth: CR 118.12's hand-to-battlefield cost reads the same hidden zone,
  -- and every printing of it names a quality -- Hakbal of the Surging Soul's "a
  -- land card". The DESTINATION is not what rule 118.8c asks about; the zone the
  -- cards are described IN is, and that is the hand.
  CostComponent.PutCardFromHandOntoBattlefield criterion -> Filter.statesAQuality criterion
  -- The fifth, and with the sixth below the only ones whose action reaches a
  -- PUBLIC zone as well: CR 701.4a's hand half is RevealCardFromHand's action
  -- exactly, and rule 118.8c asks whether the cost INCLUDES such an action rather
  -- than whether it forces one, so the battlefield half it offers instead does
  -- not take this arm out.
  CostComponent.Behold behold -> Filter.statesAQuality (Behold.whichObjects behold)
  -- The sixth, Behold's arm above: the exile after the behold changes nothing
  -- about which cards the cost is described by.
  CostComponent.BeholdAndExile criterion -> Filter.statesAQuality criterion
  -- The hidden zone WITHOUT a quality: CR 702.29a names the object the cost is
  -- on, so no card is described and the player has none to fail to find.
  CostComponent.DiscardThis _ -> False
  -- Cards, but in a PUBLIC zone (CR 400.2), so the first conjunct fails however
  -- specific the filter is: the battlefield, then the graveyard.
  CostComponent.Sacrifice {} -> False
  CostComponent.TapForTotalPower {} -> False
  CostComponent.TapPermanents {} -> False
  CostComponent.ReturnPermanents {} -> False
  CostComponent.ExilePermanents {} -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileCardsFromGraveyard {} -> False
  CostComponent.ExileMaterials {} -> False
  CostComponent.ExileTopFromGraveyard _ -> False
  CostComponent.CollectEvidence _ -> False
  CostComponent.CollectEvidenceOfTargets -> False
  -- No cards at all, so there is no "action involving cards" to classify.
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.SacrificeThis -> False
  CostComponent.ExileThis -> False
  -- CR 118.8c: the hand is a hidden zone, but this names the object the cost is
  -- ON rather than describing a card, so nothing is stated for a player to fail
  -- to find -- DiscardThis' answer above and for its reason.
  CostComponent.ReturnThis -> False
  CostComponent.PayLife _ -> False
  CostComponent.PayHalfLife _ -> False
  CostComponent.PayLifeX -> False
  CostComponent.PayEnergyX -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.AddLoyaltyToThis _ -> False
  CostComponent.RemoveLoyaltyFromThis _ -> False
  CostComponent.RemoveLoyaltyFromThisX -> False
  CostComponent.RemoveCountersFromThis _ -> False
  CostComponent.RemoveCounters {} -> False
  CostComponent.RemovePlusOneCountersX _ -> False
  CostComponent.SacrificeX _ -> False
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.Blight _ -> False
  CostComponent.BlightX -> False
  -- Cards, but in PUBLIC zones (CR 400.2) on both halves of rule 701.61a, so the
  -- first conjunct fails -- the Sacrifice and ExileCardsFromGraveyard arms above,
  -- for their reason. Rule 701.61a states no quality either way.
  CostComponent.Forage -> False
  CostComponent.FlipCoin -> False
  CostComponent.ChooseOpponent -> False
  CostComponent.WaterbendX -> False
  CostComponent.Waterbend _ -> False
  CostComponent.WaterbendInstead _ -> False
  -- The other hidden zone (CR 400.2), and the FIRST conjunct is satisfied where
  -- no other arm's is -- but the second is not: CR 701.17a takes the cards off
  -- the top, so "mill a card" describes no quality for a player to fail to find.
  -- The whole of CR 118.8c's phrase is "cards with a stated quality in a hidden
  -- zone", and this component can never state one, having no Filter at all.
  CostComponent.MillCards _ -> False

-- CR 306.5c: a planeswalker's loyalty is the number of loyalty counters on it.
-- Zero for an object with none, which CR 704.5i reads as loyalty 0 -- so this is
-- only ever asked of something already known to be a planeswalker.
loyaltyCountersOn :: ObjectId -> GameState -> Natural
loyaltyCountersOn = countersOn CounterKind.Loyalty

-- CR 122.1: how many counters of one kind an object has, zero for an object with
-- none and zero for one that is not there. The general form of
-- loyaltyCountersOn above, which the +1\/+1 removal cost reads too.
countersOn :: CounterKind.CounterKind Keyword.Type.Keyword -> ObjectId -> GameState -> Natural
countersOn kind oid gs =
  maybe 0 (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)

-- The cards this player may discard to pay a cost on `oid`: their hand, in its
-- own order, narrowed by the criterion and minus `oid` itself -- see
-- canPayComponent's DiscardCards arm for why that exclusion is CR 601.2a.
--
-- And minus the card being CAST, which is the same rule reaching an object this
-- cost is not on: a mana source paying for a spell may not spend the spell
-- (Game.beingCast).
--
-- `slots` is what the announcement bound (announcedSlots), which every pool
-- below takes for the same reason: CR 601.2c chooses the targets before CR
-- 601.2h pays, so a criterion may name one -- "a creature other than the
-- target" -- and Filter.IsBound reads this map. Synthetic Spiteful Rite is the
-- cast that proves it and Synthetic Spiteful Altar the activation
-- (Pawl.CostSpec's "Synthetic Spiteful Rite" group).
--
-- Matched through the card's own CR 613 projection: rule 613.1 names no zone, so
-- a card in a hand is folded exactly as a permanent is, and Putrid Raptor's
-- "discard a Zombie card" morph cost is payable with a creature card printed as
-- something else under Maskwood Nexus (Pawl.CostSpec's Putrid Raptor pair).
discardCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
discardCandidates slots pid oid criterion gs =
  let context = SourceContext.withChoicesOf oid gs (Filter.contextWithSlots (Game.teams gs) (Just pid) Nothing slots)
      viewOf = Projection.viewsOf gs
      matches candidate = Filter.matches context (viewOf candidate) criterion
   in filter (\candidate -> candidate /= oid && not (Game.beingCast gs candidate) && matches candidate) (Game.zoneMembers Zone.Hand pid gs)

-- The cards this player may put onto the battlefield to pay a CR 118.12
-- PutCardFromHandOntoBattlefield component on `oid`: discardCandidates' pool,
-- narrowed by the same criterion through the same CR 613 projection and read out
-- of the same hidden zone (CR 402.3 keeps a hand its owner's).
--
-- `oid` is excluded for discardCandidates' reason, CR 601.2a: a card being cast
-- is on the stack and is not in the hand this reads. That exclusion is inert for
-- every printing of this cost -- all of them pay it at RESOLUTION (CR 118.12),
-- where the object the cost is on is a trigger or a spell already on the stack
-- and so not in a hand at all -- and it is kept anyway so the two hand-reading
-- pools cannot disagree about what a hand holds.
putOntoBattlefieldCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
putOntoBattlefieldCandidates = discardCandidates

-- The cards this player may exile to pay an ExileCardFromHand component on
-- `oid`: the same pool again, read out of the same hidden zone (CR 402.3) and
-- narrowed by the same criterion through the same CR 613 projection. The
-- DESTINATION is what separates this component from the two above, and CR 406.2
-- says nothing about which cards may go there, so nothing separates the pools.
--
-- `oid` is excluded, discardCandidates' CR 601.2a exclusion: a card being cast is
-- on the stack and not in the hand this reads.
exileFromHandCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
exileFromHandCandidates = discardCandidates

-- The cards this player may reveal to pay a RevealCardFromHand component on
-- `oid`: the same pool once more, out of the same hidden zone (CR 402.3) and
-- narrowed by the same criterion through the same CR 613 projection. CR 701.20a
-- says nothing about which cards may be shown, so nothing separates the pools.
--
-- `oid` is excluded, discardCandidates' CR 601.2a exclusion: Living Destiny is on
-- the stack by the time its own additional cost is paid, and a spell cannot
-- reveal itself out of a hand it has left.
revealFromHandCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
revealFromHandCandidates = discardCandidates

-- The objects this player may behold to pay a Behold component on `oid`: CR
-- 701.4a's two pools as one list, `revealFromHandCandidates`' hand ahead of the
-- battlefield permanents this player CONTROLS that the criterion admits.
--
-- NARROWED to what the payer controls, unlike `tapCandidates` and
-- `returnCandidates`: rule 701.4a says "a [quality] permanent you control", so
-- the restriction is the RULE's and not the card's -- Caustic Exhale's criterion
-- states "a Dragon" and nothing else, and narrowing anywhere but here would put
-- the rule's words on every card that prints one.
--
-- Matched through the same CR 613 projection on both sides, `discardCandidates`'
-- reading, and against a context built the same way on both, so the hand half and
-- the battlefield half cannot read the criterion differently.
beholdCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
beholdCandidates slots pid oid criterion gs =
  let context = SourceContext.withChoicesOf oid gs (Filter.contextWithSlots (Game.teams gs) (Just pid) Nothing slots)
      viewOf = Projection.viewsOf gs
      matches candidate = Filter.matches context (viewOf candidate) criterion
   in revealFromHandCandidates slots pid oid criterion gs
        <> filter matches (List.sort (Projection.controls pid gs))

-- CR 701.4a `n` times over `beholdCandidates`' one pool: the objects this player
-- beholds to pay a Behold or BeholdAndExile component on `oid`, or Nothing where
-- the pool holds fewer than `n`. The pool is read HERE so an earlier component of
-- the same cost that emptied it leaves the component Unpaid.
--
-- DISTINCT: each pick is withheld from the asks after it, so no object is beheld
-- twice -- Pawl.CostSpec's "CR 701.4a three Elementals are three objects" is the
-- proof. One Prompt.ChooseBehold per object, raised only while the pool left
-- holds more than the objects still owed; at exactly that many every one is
-- beheld and there is nothing to choose. FILTERED and not trusted (#222).
--
-- Each is revealed where it is in the hand, conditioned on the ZONE rather than
-- on which half the answer came from, so the two halves cannot disagree.
beholdObjects :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Natural -> Filter.Type.Filter Keyword.Type.Keyword -> Game (Maybe [ObjectId])
beholdObjects slots pid oid n criterion = do
  gs <- State.get
  let pool = beholdCandidates slots pid oid criterion gs
      decider = Decide.deciderFor pid gs
      pick chosen owed
        | owed == 0 = pure (Just (reverse chosen))
        | otherwise =
            let left = filter (`notElem` chosen) pool
             in case left of
                  first : second : more
                    | Natural.length left > owed -> do
                        answer <- Game.choose (Prompt.ChooseBehold decider pid oid (first NonEmpty.:| (second : more)))
                        pick ((if List.elem answer left then answer else first) : chosen) (owed - 1)
                  first : _
                    | Natural.length left >= owed -> pick (first : chosen) (owed - 1)
                  _ -> pure Nothing
  beheld <- pick [] n
  Monad.forM_ (Maybe.fromMaybe [] beheld) $ \chosen ->
    Monad.when (fmap Object.zone (Game.lookupObject chosen gs) == Just Zone.Hand) (Event.reveal RevealCause.Ordinary pid chosen)
  pure beheld

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
-- No `oid` exclusion, unlike discardCandidates above: CR 602.2a leaves an ability
-- activated FROM a graveyard with its source still there, a legal candidate for
-- its own cost. What is excluded instead is the card being CAST (Game.beingCast),
-- which is the exclusion the callers asking BEFORE CR 601.2a's move need and the
-- only one CR 601.2a states -- Loathsome Chimera is not among the cards its own
-- escape cost can exile.
exileCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
exileCandidates slots pid oid criterion gs =
  let context = SourceContext.withChoicesOf oid gs (Filter.contextWithSlots (Game.teams gs) (Just pid) Nothing slots)
      viewOf = Projection.viewsOf gs
      matches candidate = Filter.matches context (viewOf candidate) criterion
   in filter (\candidate -> not (Game.beingCast gs candidate) && matches candidate) (Game.zoneMembers Zone.Graveyard pid gs)

-- The objects this player may exile to pay an ExileMaterials component on `oid`:
-- CR 702.167a's "from among permanents you control and\/or cards in your
-- graveyard" as one list, `beholdCandidates`' union with the graveyard where that
-- one reads the hand, and `exileCandidates`' graveyard half verbatim.
--
-- ONE criterion read over both halves, through the same CR 613 projection and a
-- context built the same way on both, which is CR 702.167b's exception to rule
-- 109.2: "creature" admits a creature on the battlefield and a creature card in
-- the graveyard, and the two halves cannot read the criterion differently.
--
-- `oid` IS EXCLUDED from the battlefield half, unlike `exileCandidates`' graveyard
-- pool: rule 702.167a exiles this permanent as part of the same cost, so it is
-- never among its own [materials] -- a payment that took it as a material would
-- leave the ExileThis component nothing to exile.
materialCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
materialCandidates slots pid oid criterion gs =
  let context = SourceContext.withChoicesOf oid gs (Filter.contextWithSlots (Game.teams gs) (Just pid) Nothing slots)
      viewOf = Projection.viewsOf gs
      matches candidate = candidate /= oid && Filter.matches context (viewOf candidate) criterion
   in filter matches (List.sort (Projection.controls pid gs))
        <> exileCandidates slots pid oid criterion gs

-- The one card an ExileTopFromGraveyard component takes: the TOP matching card
-- of this player's graveyard, or Nothing where it holds none.
--
-- The LAST of exileCandidates' answer is the top: CR 404.1 puts an arrival on
-- top and Game.insertIntoZone appends, the opposite end from a library. No
-- prompt, and that is CR 404.2 rather than an elision: a graveyard's order is not
-- the player's to change, so "the top creature card" names exactly one.
topExileCandidate :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> Maybe ObjectId
topExileCandidate slots pid oid criterion gs =
  Maybe.listToMaybe (reverse (exileCandidates slots pid oid criterion gs))

-- The cards this player may exile to collect evidence: their WHOLE graveyard,
-- `exileCandidates` under rule 701.59a's absent criterion -- "any number of
-- cards from your graveyard" names no quality, so the trivial predicate is the
-- criterion rather than a stand-in for one. Game.beingCast's exclusion rides along
-- from there, and is CR 601.2a for the offer paths that ask before the card
-- moves.
evidenceCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> GameState -> [ObjectId]
evidenceCandidates slots pid oid = exileCandidates slots pid oid (Filter.Type.And [])

-- CR 202.3's mana value of one card, read off its CR 613 projection --
-- `tapPower`'s posture one zone over, and through the same projection
-- `exileCandidates` matches the criterion against, so the pool and the total
-- describe each card the same way. 0 where there is no value to read, CR 202.3a's
-- own answer for an object with no mana cost.
evidenceValue :: ObjectId -> GameState -> Integer
evidenceValue candidate gs = Maybe.fromMaybe 0 (Filter.manaValue (Projection.viewsOf gs candidate))

-- The permanents this player may tap to pay a TapForTotalPower or TapPermanents
-- component on `oid`: every battlefield object matching the criterion, ascending.
--
-- NOT Replacement.sacrificeCandidates, and the difference is the CONTEXT: that
-- one pre-narrows to `Projection.controls pid` and applies CR 101.2's sacrifice
-- restrictions, where this one carries CR 702.122d's prohibition and the source's
-- colours (CR 702.78a). Both read the PAYER as "you" and the permanent
-- whose ability is being paid for as the source -- without it a Vehicle that has
-- already become a creature could crew itself.
tapCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
tapCandidates slots pid oid criterion gs =
  let -- CR 702.122d's prohibition rides the CONTEXT rather than narrowing the
      -- pool here, and that is what confines rule 702.122d to a CREW cost: the
      -- atom that reads it is written into rule 702.122a's criterion by
      -- Pawl.Engine.Keyword's `crew` and by nothing else, so a TapForTotalPower
      -- printed outside a crew ability
      -- (data/cards/synthetic-crewed-battery.json) never asks and the set is
      -- never forced.
      -- CR 105.2, the half rule 702.78a's criterion reads off the SOURCE: the
      -- spell being cast, whose colours the atom intersects each candidate's
      -- against. Read with last known information for
      -- Pawl.Engine.Resolve.Slots.effectContext's reason -- the same road it fills
      -- sourceManaValue by -- and empty where the object is gone, which the atom
      -- already answers False for.
      --
      -- CR 607.2d, the source's entry choices (SourceContext.withChoicesOf), for
      -- a criterion naming "the chosen type" -- matchesPermanent's reading.
      context =
        SourceContext.withChoicesOf
          oid
          gs
          (Filter.contextCrewing (Game.teams gs) (Just pid) (Just oid) slots (CrewRestriction.cantCrew (Set.toList (GameState.battlefield gs)) gs))
            { Filter.sourceColors = maybe Set.empty Filter.colors (Projection.viewWithLastKnownAnywhere gs oid)
            }
      viewOf = Projection.viewsOf gs
      matches candidate =
        Filter.matches context (viewOf candidate) criterion
   in List.sort (filter matches (Set.toList (GameState.battlefield gs)))

-- The permanents this player may return to hand to pay a ReturnPermanents
-- component on `oid`: `tapCandidates`' pool exactly, which is what an alias
-- rather than a second walk says -- both ask which battlefield objects the
-- criterion admits, from the payer's perspective and against `oid` as the
-- source. The NAMES are what differ, so each cost's arm reads the pool under
-- the action it takes.
--
-- NOT narrowed to what the payer controls, `tapCandidates`' reading, and
-- deliberately unlike Replacement.sacrificeCandidates' pool even though
-- `claimOf` puts both on ClaimAxis.Removal Zone.Battlefield: that one narrows
-- because CR 701.21a forbids sacrificing a permanent you don't control, and no
-- rule says the same of returning one -- CR 118.1 asks only that the payer carry
-- out the instruction. Narrowing here would enforce a restriction the CR does
-- not state, so the criterion carries it where a card wants it (Meloku the
-- Clouded Mirror's prints `ControlledBy You`). Scryfall
-- `o:/[Rr]eturn (a|an|two|three|another) [^.:]+ to (its|their) owner.s hand:/
-- -o:"you control"`, 2026-09-02, no hit, and the same query over "as an
-- additional cost" none either: every printed return cost states it today, so
-- the two pools coincide for every printing, `data/cards/` included.
returnCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
returnCandidates = tapCandidates

-- The permanents this player may take `n` counters of the kinds `which` reaches
-- off to pay a RemoveCounters component spread FromOne on `oid`:
-- `tapCandidates`' pool narrowed to the ones CR 118.3 leaves payable, since a
-- permanent carrying fewer than `n` counters cannot have `n` removed. The narrowing is HERE rather than at the
-- prompt so that the gate and the payment ask one question, canPayComponent's
-- posture for every other choosing component.
--
-- NOT narrowed to what the payer controls, `returnCandidates`' reading and for
-- its reason: CR 118.1 asks only that the payer carry out the instruction, so a
-- card that wants the restriction prints it (Zameck Guildmage's criterion carries
-- `ControlledBy You`).
counterRemovalCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> Natural -> WhichCounters.WhichCounters Keyword.Type.Keyword -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
counterRemovalCandidates slots pid oid n which criterion gs =
  filter (\candidate -> sum (removableCounters which candidate gs) >= n) (tapCandidates slots pid oid criterion gs)

-- `counterRemovalCandidates` for a removal spread FROM AMONG the permanents
-- (CounterSpread.FromAmong): every admitted permanent carrying at least one
-- counter `which` reaches, with how many it carries, since CR 118.3 now asks of
-- the total rather than of any one permanent.
spreadRemovalCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> WhichCounters.WhichCounters Keyword.Type.Keyword -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> Map.Map ObjectId Natural
spreadRemovalCandidates slots pid oid which criterion gs =
  fmap sum (mixedRemovalCandidates slots pid oid which criterion gs)

-- `spreadRemovalCandidates` kept BY KIND, for a cost naming none (CR 122.1:
-- counters of different names are not interchangeable, so which kind comes off
-- is the payer's choice as well as which permanent).
mixedRemovalCandidates :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> WhichCounters.WhichCounters Keyword.Type.Keyword -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> Map.Map ObjectId (Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural)
mixedRemovalCandidates slots pid oid which criterion gs =
  Map.filter (not . Map.null) (Map.fromList [(candidate, removableCounters which candidate gs) | candidate <- tapCandidates slots pid oid criterion gs])

-- The counters of the kinds `which` reaches on one object, by kind.
removableCounters :: WhichCounters.WhichCounters Keyword.Type.Keyword -> ObjectId -> GameState -> Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural
removableCounters which candidate gs = Map.filter (> 0) $ case which of
  WhichCounters.OfKind kind -> Map.singleton kind (countersOn kind candidate gs)
  WhichCounters.OfAnyKind -> maybe Map.empty Object.counters (Game.lookupObject candidate gs)

-- The division of `owed` counters that takes them off the candidates in
-- ascending order, emptying each before the next. It is the ONLY division where
-- there is one candidate or the candidates carry exactly `owed` between them,
-- which is where payComponent elides the prompt, and Pawl.Engine.Replay's
-- default answer otherwise.
fillInOrder :: (Ord key) => Natural -> Map.Map key Natural -> Map.Map key Natural
fillInOrder owed offered = Map.fromList (go owed (Map.toAscList offered))
  where
    go left candidates = case candidates of
      [] -> []
      (candidate, carried) : rest
        | left == 0 -> []
        | otherwise -> (candidate, min left carried) : go (left - min left carried) rest

-- Is this division of `owed` counters one the offer allows? Every permanent it
-- names offered and carrying at least what it gives, and the whole adding up to
-- `owed` exactly -- CR 601.2h's "partial payments are not allowed".
dividesRemoval :: Natural -> Map.Map ObjectId Natural -> Map.Map ObjectId Natural -> Bool
dividesRemoval owed offered division = sum division == owed && withinOffer offered division

-- `dividesRemoval` for a count the payer chooses (CounterSpread.FromAmongAtLeast):
-- the whole reaching `least` rather than equalling it.
dividesRemovalAtLeast :: Natural -> Map.Map ObjectId Natural -> Map.Map ObjectId Natural -> Bool
dividesRemovalAtLeast least offered division = sum division >= least && withinOffer offered division

-- Every permanent a division names is offered and carries at least what the
-- division takes off it.
withinOffer :: (Ord key) => Map.Map key Natural -> Map.Map key Natural -> Bool
withinOffer offered = and . Map.mapWithKey (\candidate taken -> taken <= Map.findWithDefault 0 candidate offered)

-- A division by permanent AND kind, flattened to one key per pair so that
-- `fillInOrder` and `withinOffer` read it as they read a one-kind division.
flattenMixed :: Map.Map ObjectId (Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural) -> Map.Map (ObjectId, CounterKind.CounterKind Keyword.Type.Keyword) Natural
flattenMixed division = Map.fromList [((candidate, kind), taken) | (candidate, kinds) <- Map.toList division, (kind, taken) <- Map.toList kinds, taken > 0]

-- `flattenMixed`'s inverse.
unflattenMixed :: Map.Map (ObjectId, CounterKind.CounterKind Keyword.Type.Keyword) Natural -> Map.Map ObjectId (Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural)
unflattenMixed flat = Map.fromListWith Map.union [(candidate, Map.singleton kind taken) | ((candidate, kind), taken) <- Map.toList flat, taken > 0]

-- The division of a mixed removal the rules leave no choice in, where there is
-- one: every counter offered where they number exactly `owed`, or `owed` of the
-- one kind on the one permanent offered (under a spread FromOne, the one
-- permanent alone). Pawl.Engine.Replay's default answer is `fillMixedInOrder`.
onlyMixedDivision :: CounterSpread.CounterSpread -> Natural -> Map.Map ObjectId (Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural) -> Maybe (Map.Map ObjectId (Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural))
onlyMixedDivision spread owed offered = case spread of
  CounterSpread.FromAmongAtLeast
    | sum flat == owed -> Just offered
    | otherwise -> Nothing
  CounterSpread.FromAmong -> exact
  CounterSpread.FromOne -> exact
  where
    flat = flattenMixed offered
    exact
      | sum flat == owed = Just offered
      | Map.size flat <= 1 = Just (fillMixedInOrder spread owed offered)
      | otherwise = Nothing

-- `fillInOrder` for a division by permanent and kind; under a spread FromOne,
-- off the first permanent offered alone.
fillMixedInOrder :: CounterSpread.CounterSpread -> Natural -> Map.Map ObjectId (Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural) -> Map.Map ObjectId (Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural)
fillMixedInOrder spread owed offered = unflattenMixed (fillInOrder owed (flattenMixed from))
  where
    from = case spread of
      CounterSpread.FromOne -> Map.take 1 offered
      CounterSpread.FromAmong -> offered
      CounterSpread.FromAmongAtLeast -> offered

-- Is this division by permanent and kind one the offer allows? Every pair it
-- names offered and carrying what it takes, the whole `owed` exactly (at least
-- `owed`, under FromAmongAtLeast), and under FromOne off a single permanent.
dividesMixedRemoval :: CounterSpread.CounterSpread -> Natural -> Map.Map ObjectId (Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural) -> Map.Map ObjectId (Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural) -> Bool
dividesMixedRemoval spread owed offered division =
  withinOffer (flattenMixed offered) flat && case spread of
    CounterSpread.FromOne -> sum flat == owed && Set.size (Set.map fst (Map.keysSet flat)) <= 1
    CounterSpread.FromAmong -> sum flat == owed
    CounterSpread.FromAmongAtLeast -> sum flat >= owed
  where
    flat = flattenMixed division

-- The power a candidate contributes to CR 702.122a's total. Zero for a permanent
-- with no power at all, which after CR 208.3 is every noncreature one.
tapPower :: ObjectId -> GameState -> Integer
tapPower candidate gs = Maybe.fromMaybe 0 (Projection.powerOf candidate gs)

-- CR 205.3m: a tap candidate's creature types, off its projection -- so a
-- changeling holds every one (CR 702.73a) and a land type none.
tapCreatureTypes :: ObjectId -> GameState -> Set.Set Subtype.Type.Subtype
tapCreatureTypes candidate gs = Set.filter Subtype.isCreatureType (Filter.subtypes (Projection.viewsOf gs candidate))

-- CR 205.3m: do these permanents hold one creature type in common? False for
-- none at all, since no type is held.
shareACreatureType :: [ObjectId] -> GameState -> Bool
shareACreatureType chosen gs = case fmap (`tapCreatureTypes` gs) chosen of
  [] -> False
  first : rest -> not (Set.null (List.foldl' Set.intersection first rest))

-- CR 205.3m: the most of these permanents that hold one creature type in
-- common, Pawl.Engine.Count's MostSharingACreatureType over tap candidates.
largestSharingGroup :: [ObjectId] -> GameState -> Natural
largestSharingGroup candidates gs =
  Foldable.foldl' max 0 (Map.fromListWith (+) [(subtype, 1) | candidate <- candidates, subtype <- Set.toList (tapCreatureTypes candidate gs)])

-- CR 701.26a: tap one permanent, through Pawl.Engine.Event's funnel so that
-- paying a tap cost is a becomes-tapped event like any other route. Shared by
-- every component that taps -- see payComponent's TapThis arm -- which is what
-- gave all of them the event for one call.
tapObject :: ObjectId -> Game ()
tapObject = Event.tap

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
-- A `*This` arm whose component `canPayComponent` refuses answers an EMPTY pool
-- rather than Nothing.
--
-- Every pool is read against `slots`, canPayComponent's reading and for its
-- reason: CR 601.2c's bindings where the caller has them, and the empty map
-- where no announcement is behind the payment at all.
--
-- EXHAUSTIVE with no wildcard, this module's posture, and -Werror makes it.
claimOf :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> GameState -> Maybe Claim
claimOf slots pid oid component gs =
  let claim a p n = Just (Claim.Type.MkClaim {Claim.Type.axis = a, Claim.Type.pool = p, Claim.Type.count = n, Claim.Type.threshold = Nothing})
      -- One selection of `candidates` whose amounts reach `needed`.
      reaching a needed amountOf candidates =
        Just
          ( Claim.Type.MkClaim
              { Claim.Type.axis = a,
                Claim.Type.pool = Set.fromList candidates,
                Claim.Type.count = 1,
                Claim.Type.threshold = Just (Threshold.MkThreshold {Threshold.total = needed, Threshold.amounts = Map.fromList (fmap (\candidate -> (candidate, amountOf candidate)) candidates)})
              }
          )
      -- A `*This` arm's pool: the object itself where `canPayComponent` pays the
      -- component, so the two agree by construction.
      itself = if canPayComponent slots pid oid component gs then Set.singleton oid else Set.empty
   in case component of
        -- CR 701.21a: the permanents this player controls that match the criterion.
        CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) ->
          claim (ClaimAxis.Removal Zone.Battlefield) (Set.fromList (Replacement.sacrificeCandidates (Just pid) slots pid (Just oid) criterion gs)) n
        CostComponent.SacrificeThis -> claim (ClaimAxis.Removal Zone.Battlefield) itself 1
        -- The same battlefield pool SacrificeThis draws on -- a permanent returned to
        -- hand is as gone from the battlefield as one sacrificed.
        --
        -- A FENCE and not proven behaviour: Grinning Ignus is the one card printing
        -- this component, its cost states one component and a non-empty mana part, so
        -- `repeatsOf` settles at 1 before any axis matters. Keying it ClaimAxis.Tapping
        -- instead leaves the suite green.
        CostComponent.ReturnThis -> claim (ClaimAxis.Removal Zone.Battlefield) itself 1
        -- The same battlefield pool the two arms above claim, and on the same axis: a
        -- permanent exiled is as gone from the battlefield as one sacrificed.
        --
        -- A FENCE and not proven behaviour, ReturnThis' note above: every printing of
        -- this component in `data/cards/` -- Brittle Effigy, Hanged Executioner --
        -- states a non-empty mana part, so `repeatsOf` settles at 1 before any axis
        -- matters.
        CostComponent.ExileThis -> claim (ClaimAxis.Removal Zone.Battlefield) itself 1
        CostComponent.DiscardCards (DiscardCards.MkDiscardCards n criterion) ->
          claim (ClaimAxis.Removal Zone.Hand) (Set.fromList (discardCandidates slots pid oid criterion gs)) n
        CostComponent.DiscardThis _ -> claim (ClaimAxis.Removal Zone.Hand) itself 1
        -- The same hand pool the two arms above claim, and on the same axis: what the
        -- payment spends is a card leaving the hand, and the battlefield end adds a
        -- permanent rather than competing for one. A FENCE and not proven behaviour --
        -- every printing of this cost is a CR 118.12 offer, which `repeatsOf` never
        -- measures, so no board separates this from any other axis.
        CostComponent.PutCardFromHandOntoBattlefield criterion ->
          claim (ClaimAxis.Removal Zone.Hand) (Set.fromList (putOntoBattlefieldCandidates slots pid oid criterion gs)) 1
        -- The same hand pool and the same axis: CR 406.2's exile spends a card leaving
        -- the hand exactly as a discard does. LOAD-BEARING and not a fence -- Cadaverous
        -- Bloom's cost has no mana part, so `repeatsOf` reads this claim to decide how
        -- many times the ability can be activated, which is the hand's size.
        --
        -- The pool excludes the card being CAST as well as the object the cost is on
        -- (Game.beingCast), so a spell being offered is not fuel for the source that would
        -- pay for it: CR 601.2a has it on the stack by the time CR 601.2h pays.
        CostComponent.ExileCardFromHand criterion ->
          claim (ClaimAxis.Removal Zone.Hand) (Set.fromList (exileFromHandCandidates slots pid oid criterion gs)) 1
        CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) ->
          claim (ClaimAxis.Removal Zone.Graveyard) (Set.fromList (exileCandidates slots pid oid criterion gs)) n
        -- The same graveyard pool and the same axis, as ONE selection reaching the
        -- threshold rather than the component's number of cards: rule 701.59a's
        -- number is a THRESHOLD on total mana value, so which cards a payment
        -- exiles is not settled until the payer picks them. TapForTotalPower's arm
        -- below and for its reason. A threshold of 0 is paid by the empty set and
        -- claims nothing.
        CostComponent.CollectEvidence n
          | n > 0 -> reaching (ClaimAxis.Removal Zone.Graveyard) (toInteger n) (`evidenceValue` gs) (evidenceCandidates slots pid oid gs)
          | otherwise -> Nothing
        -- The arm above at the amount these slots fix (fixComputed).
        CostComponent.CollectEvidenceOfTargets -> claimOf slots pid oid (fixComputed slots gs component) gs
        -- A pool of at most ONE, CR 404.2's order having picked it.
        CostComponent.ExileTopFromGraveyard criterion ->
          claim (ClaimAxis.Removal Zone.Graveyard) (Set.fromList (Maybe.maybeToList (topExileCandidate slots pid oid criterion gs))) 1
        CostComponent.ExileThisFromGraveyard -> claim (ClaimAxis.Removal Zone.Graveyard) itself 1
        -- CR 701.17a spends cards out of the paying player's own library, so the pool
        -- is that library and the count is how many the mill takes -- the ZONE keying a
        -- Removal soundly for the header's reason. What it buys is two mills of one
        -- cost needing two cards rather than one, which Hall's condition then asks; a
        -- FENCE, no card in `data/cards/` milling twice in one cost.
        CostComponent.MillCards n ->
          claim (ClaimAxis.Removal Zone.Library) (Set.fromList (Game.zoneMembers Zone.Library pid gs)) n
        -- CR 107.5: {T} spends exactly the untapped-ness the TapPermanents arm below
        -- claims, so it is the same axis, on a pool of one.
        CostComponent.TapThis -> claim ClaimAxis.Tapping itself 1
        -- Nothing: CR 107.6's {Q} spends TAPPED-ness, a third axis, and names the
        -- object the cost is on, so two such claims come from one cost carrying {Q}
        -- twice or from two mana abilities of one permanent that a board takes
        -- together (Mana.sourceOptions). data/cards/ prints neither.
        CostComponent.UntapThis -> Nothing
        -- ONE selection of candidates whose powers reach the threshold, and
        -- deliberately not the Natural as a count: that number is a THRESHOLD on an
        -- aggregate rather than a count of objects, so which permanents a payment
        -- taps is not settled until the payer picks them, and only some of them add
        -- up (Pawl.Engine.Claim.assignable); a threshold of 0 is paid by the empty
        -- set, taps nothing and claims nothing. Pawl.ManaSpec's Synthetic Muster
        -- Dynamo beside a Heritage Druid is the board a count of objects got wrong.
        -- Dividing the pool is still the wrong direction for `repeatsOf`, which is
        -- why `uncountedCeiling` counts this component's repeats itself.
        --
        -- The pool is tapCandidates', TapPermanents' below: tapped candidates included,
        -- the same permissive reading and for its reason. CR 702.122a's own criterion
        -- excludes them (Pawl.Engine.Keyword's crew), so a crew cost's pool is the
        -- untapped creatures exactly.
        CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower threshold criterion)
          | threshold > 0 -> reaching ClaimAxis.Tapping (toInteger threshold) (`tapPower` gs) (tapCandidates slots pid oid criterion gs)
          | otherwise -> Nothing
        -- CR 601.2f's "tapping permanents", on the TAPPING axis rather than a zone's,
        -- for the header's reason. ManaSpec's "a creature tapped for mana can still be
        -- sacrificed" is the case that proves the axes stay apart.
        --
        -- The pool is every candidate the criterion admits, tapped ones included --
        -- the PERMISSIVE reading where a criterion omits "untapped". `repeatsOf`
        -- divides this pool, so such a criterion UNDERSTATES how often the cost can
        -- be paid rather than overstating it: an already-tapped candidate spends no
        -- untapped-ness and so is payable again, where dividing counts it once.
        -- Every criterion in the tree writes "untapped" anyway, printed
        -- (Heritage Druid, Springleaf Drum) and minted alike
        -- (Pawl.Engine.Keyword's conspire and station), so the division is exact
        -- for all of them.
        --
        -- A SUPERSET where the component asks for a shared creature type: the
        -- claim pools every candidate, and `payable`'s arm is what narrows to a
        -- group holding one type (Weight of Conscience states no other claim).
        CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion _) ->
          claim ClaimAxis.Tapping (Set.fromList (tapCandidates slots pid oid criterion gs)) n
        -- The battlefield pool SacrificeThis and ReturnThis draw on, on their axis
        -- and not the tapping one: a permanent returned to hand is as gone from the
        -- battlefield as one sacrificed, so a cost that returns and a cost that
        -- sacrifices compete for the same objects.
        --
        -- A FENCE and not proven behaviour, ReturnThis' above and for its reason:
        -- Meloku the Clouded Mirror's cost states one object-claiming component, so
        -- Hall's condition groups a single claim and keying it ClaimAxis.Tapping
        -- instead leaves the suite green.
        CostComponent.ReturnPermanents (ReturnPermanents.MkReturnPermanents n criterion) ->
          claim (ClaimAxis.Removal Zone.Battlefield) (Set.fromList (returnCandidates slots pid oid criterion gs)) n
        -- CR 406.2's exile out of the same pool, ReturnPermanents' reading.
        CostComponent.ExilePermanents (ExilePermanents.MkExilePermanents n criterion) ->
          claim (ClaimAxis.Removal Zone.Battlefield) (Set.fromList (returnCandidates slots pid oid criterion gs)) n
        CostComponent.PayLife _ -> Nothing
        CostComponent.PayHalfLife _ -> Nothing
        CostComponent.PayLifeX -> Nothing
        CostComponent.PayEnergyX -> Nothing
        CostComponent.PayEnergy _ -> Nothing
        CostComponent.AddLoyaltyToThis _ -> Nothing
        CostComponent.RemoveLoyaltyFromThis _ -> Nothing
        CostComponent.RemoveLoyaltyFromThisX -> Nothing
        -- Nothing: CR 122.1's counter is a marker rather than an object, so no object
        -- leaves any pool -- the two arms either side of this one, for their reason. A
        -- FENCE, `repeatsOf` settling before any axis matters for a cost with one
        -- component and a mana part.
        CostComponent.RemoveCountersFromThis _ -> Nothing
        -- Nothing, the arm above's rule 122.1 reason, though this one DOES pick
        -- an object out of a pool: what it spends is the counters on that
        -- object, which are markers rather than objects, so no pool shrinks --
        -- the Blight arm below's shape.
        CostComponent.RemoveCounters {} -> Nothing
        CostComponent.RemovePlusOneCountersX _ -> Nothing
        CostComponent.SacrificeX _ -> Nothing
        CostComponent.PutPlusOneCountersOnThis _ -> Nothing
        -- Nothing, though this one DOES pick an object out of a pool: CR 701.68a takes
        -- nothing out of a zone. Two blights in one cost may choose the same creature,
        -- which is right -- CR 122.6 stacks counters.
        CostComponent.Blight _ -> Nothing
        CostComponent.BlightX -> Nothing
        -- Nothing, though this one DOES take objects out of a pool: CR 701.61a's two
        -- halves spend out of DIFFERENT pools on different axes -- three cards off a
        -- graveyard, or one Food off the battlefield -- and a Claim names one axis, so
        -- no single claim describes the component. A FENCE, no card in `data/cards/`
        -- printing two forages in one cost.
        CostComponent.Forage -> Nothing
        CostComponent.FlipCoin -> Nothing
        -- CR 702.174a's choice spends nothing, FlipCoin's answer just above.
        CostComponent.ChooseOpponent -> Nothing
        CostComponent.WaterbendX -> Nothing
        -- No claim: rule 701.67a's taps are a component of their own once the
        -- payer takes the offer (`manaSubstitutions`), and that one claims them.
        CostComponent.Waterbend _ -> Nothing
        CostComponent.WaterbendInstead _ -> Nothing
        -- Nothing, Blight's arm above and for its reason one rule over: CR 701.20b
        -- leaves the revealed card in the hand, so nothing leaves any pool. CR
        -- 701.20c is what makes the shared-choice half right here too -- a card
        -- already revealed may be revealed again.
        CostComponent.RevealCardFromHand _ -> Nothing
        -- Nothing, RevealCardFromHand's arm just above and for a reason that covers
        -- both of CR 701.4a's halves: neither revealing a card nor choosing a permanent
        -- takes anything out of any pool.
        CostComponent.Behold _ -> Nothing
        -- Nothing, ExileMaterials' arm below and for its reason: CR 701.4a's pool
        -- spans the hand and the battlefield, two DIFFERENT axes, and a Claim names
        -- one. A FENCE: Champion of the Weird's cost states no second component
        -- that draws on either zone.
        CostComponent.BeholdAndExile _ -> Nothing
        -- Nothing, Forage's arm above and for its reason: CR 702.167a's pool spans the
        -- battlefield and the payer's graveyard, two DIFFERENT axes, and a Claim names
        -- one. A FENCE, no cost in `data/cards/` printing this component beside a
        -- second one that draws on either zone.
        CostComponent.ExileMaterials {} -> Nothing

-- CR 118.3's "fully", asked of a cost's components TOGETHER rather than one at a
-- time: CR 601.2h pays them in any order, so the question is whether SOME
-- assignment of distinct objects pays every one in full. Jarad, Golgari Lich
-- Lord's "Sacrifice a Swamp and a Forest" beside one Bayou tells the two readings
-- apart. Pawl.Engine.Claim.satisfiable carries the per-axis grouping and Hall's
-- condition; this module's part is which components claim, and on which axis.
jointlyPayable :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> GameState -> Bool
jointlyPayable slots pid oid components gs = Claim.satisfiable (claimsOf slots pid oid components gs)

-- Everything these components will spend out of a pool of objects, on whichever
-- axis each spends it (Pawl.Types.ClaimAxis) -- what `jointlyPayable` asks
-- Hall's condition of, and what the MANA side is handed to ask it of these
-- claims and its sources' together.
claimsOf :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> GameState -> [Claim]
claimsOf slots pid oid components gs = Maybe.mapMaybe (\component -> claimOf slots pid oid component gs) components

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
--
-- SLOTLESS, and exactly so rather than as a shortcut: every caller is a special
-- action or CR 118.12's resolution-time payment, and no announcement stands
-- behind either, so there is no binding a criterion could read (Pawl.CardSpec's
-- SlotlessCostFramed). The gates that DO sit in front of an announcement take
-- their slots as an argument (canPaySomeCompletion below).
--
-- The SUBJECT is the caller's, `pay`'s reason: this gate and the payment behind
-- it have to ask CR 106.6 the same question, or an action Overgrown Zealot's
-- mana can pay for is never offered. Most callers are ForNeither; the two
-- special actions a printed rider names -- CR 116.2m's unlock
-- (Pawl.Engine.Room.canUnlock) and CR 116.2b's turn-up
-- (Pawl.Engine.FaceDown.canTurnFaceUp) -- pass their own permanent.
canPay :: PaymentSubject.PaymentSubject -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPay = canPayReading Map.empty

-- `canPay` with a slot map its components' criteria read, which is CR 118.12's
-- resolution-time payment: the resolving object's slots, so Calim, Djinn
-- Emperor's "two OTHER cards named Calim" can exclude the card its own discard
-- cost moved (Binding.discardedCard). `payReading` is the payment it measures.
canPayReading :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PaymentSubject.PaymentSubject -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPayReading slots subject pid oid cost gs = case Cost.mana cost of
  Nothing -> False
  Just manaCost ->
    -- CR 701.67a's taps, weighed against the cost's own components as
    -- canPaySomeCompletionGiven weighs them: `any` entry of the offer, each
    -- entry's residual mana beside the union of the components.
    let payableWith (residual, extra) =
          let components = Cost.components cost <> substituteComponents extra
           in -- CR 118.14's permission is a CAST's, and no caller of this one is
              -- casting -- what reaches here is a special action's cost and CR
              -- 118.12's resolution-time payment -- so the mana is spent as it
              -- is. Which CR 106.6-restricted mana is a supply is the subject's
              -- question.
              --
              -- The energy committed is a fence: no special action or CR 118.12
              -- cost in data/cards/ pays energy.
              Mana.canPayCommitting subject (midPayment (manaActivationsGiven (PlayerEffect.applying pid gs))) ManaSpending.AsProduced pid (lifeOwedBy pid gs components) (energyOwedBy components) (claimsOf slots pid oid components gs) residual gs
                && all (\component -> canPayComponent slots pid oid component gs) components
                && jointlyPayable slots pid oid components gs
     in any payableWith (waterbendSubstitutions (Cost.components cost) slots pid oid gs manaCost)

-- How many times may this player activate this mana ability, right now, and what
-- does one activation spend? CR 605.3b keeps a mana ability off the stack, so
-- nothing here comes from Activatable.activatable and every restriction that window
-- applies has to be applied here instead.
--
-- Two of them are read off the ability's OWN activation cost (CR 602.2b):
-- CR 118.3's payability and CR 302.6's settle. NEITHER is a fact about the
-- permanent alone -- CR 107.5 bars a tapped permanent from paying {T} and says
-- nothing about a cost without one, and CR 302.6 gates only a cost holding {T}
-- or {Q} -- so both are asked per ROUTE rather than of the source.
--
-- `canPay` above, with the mana half asked through Mana.supplyCapacity rather
-- than through this function: asking back through the plain capacity would not
-- terminate -- Transmogrant Altar's "{B}, {T}, Sacrifice a creature" would ask
-- whether its own {B} is payable by a walk that asks the same of the Altar --
-- and Mana.ForSupply cuts that question, the mana being carried as a DEMAND one
-- level up instead.
--
-- CR 118.3 wants the Mana.ForOffer read, the same sentence Mana.manaSourcesGiven
-- applies to a Phyrexian Tower with no creature to give: an activation whose mana
-- part nothing on the board pays is not an offer, and offering it costs a player
-- a priority action that changes nothing. `payManaExcept`'s in-payment window is
-- judged against this same list, so the gate and that window agree about which
-- routes a mana ability's own payment may take (CR 605.3a).
--
-- The CLAIMS and the LIFE ride along with the count, which alone is a fact about
-- this source in isolation: two sources whose costs both sacrifice a creature
-- each answer 1 beside one creature. Both are ONE activation's, unscaled.
--
-- CR 601.2f's TOTAL and not the printed route, which CR 602.2b makes an
-- activation cost's rule as much as a spell's: Heartstone's "activated abilities
-- of creatures cost {1} less" reaches Coal Golem's "{3}, Sacrifice this
-- creature: Add {R}{R}{R}", and a gate reading {3} refuses an activation the
-- rules charge {2} for. `manaActivationAdjustments` is the gather, and
-- Cost.tapForManaWith charges the same total off the same one.
manaActivations :: Mana.Capacity
manaActivations measure pcs pid oid cost restrictions ability gs = manaActivationsGiven (PlayerEffect.applying pid gs) measure pcs pid oid cost restrictions ability gs

-- The same capacity given the player effects the CALLER has already gathered.
-- `manaActivationAdjustments` is a walk of everything in play asking each what
-- player abilities it prints, and it does not depend on which route is being
-- measured -- so a caller that asks per ROUTE of per PERMANENT took an identical
-- one every time, which is one more per-permanent O(N) walk inside
-- Action.legalActions' own loop; see #1073, which an allocation guard caught.
-- Not implemented: nothing asserts it stays hoisted (gap #578).
--
-- PARTIALLY APPLIED, which is the whole of the hoist: one closure carrying one
-- thunk, so every route the sweep under it measures shares the one walk.
--
-- IT MUST BE `PlayerEffect.applying pid gs`'s OWN ANSWER for the pid and the
-- board this is then called with, exactly as `pcs` must be that board's
-- projection; the wrapper above is what a caller with no list of its own uses.
--
-- Each row carries the SOURCE that printed it, which the gather under this needs
-- and no reader here reads: see Pawl.Engine.PlayerEffect.matchesObjectFrom.
manaActivationsGiven :: [(RowSource, PlayerEffect.Type.PlayerEffect)] -> Mana.Capacity
manaActivationsGiven effects measure pcs pid oid printedCost restrictions ability gs =
  let adjustments = manaActivationAdjustmentsGiven effects pid (ActivatedAbility.keyword =<< ability) oid gs
      -- The COMPONENT half of CR 601.2f, applied here so every conjunct below
      -- measures the components an effect added as well as the printed ones. The
      -- MANA half is not folded in, because CR 118.7e and CR 601.2f leave two
      -- choices inside it that nobody has made yet: `manaPartPayable` asks
      -- whether SOME total is payable, `totalManas`' shape and
      -- canPaySomeCompletionGiven's posture.
      cost = plusComponents adjustments printedCost
      -- CR 118.3 asked of the route's OWN mana part, which is the one conjunct
      -- the two readers differ on. Mana.ForSupply models that mana as a DEMAND
      -- instead (Mana.payableResolutionsGiven), so asking it here would both
      -- double-charge the route and recurse forever -- an Altar asking whether
      -- its own {B} is payable by a walk that asks the same of the Altar. CR
      -- 118.6's Nothing is still unpayable under both.
      payable = case measure of
        Mana.ForOffer -> manaPartPayable effects adjustments pid oid cost gs
        Mana.ForSupply -> Maybe.isJust (Cost.mana cost)
   in if payable
        && all (\component -> canPayComponent Map.empty pid oid component gs) (Cost.components cost)
        && jointlyPayable Map.empty pid oid (Cost.components cost) gs
        && sicknessOkGiven pcs pid oid cost gs
        -- CR 701.35a's "its activated abilities can't be activated", which reaches a
        -- mana ability too. Here rather than in Mana.manaSourcesGiven, because this
        -- is what BOTH of CR 605.3a's windows consult -- sickness's position above.
        && not (Detain.detained oid gs)
        -- CR 101.2 over CR 605.3a's permission: an effect aimed at this
        -- permanent saying its activated abilities can't be activated -- printed
        -- (Arrest) or stored by a resolution (Deadlock Trap). Here for detain's
        -- reason -- this is what BOTH of CR 605.3a's windows consult -- and
        -- asked as ManaAbility, which is the only kind
        -- reaching this function. A row naming NonManaAbility is therefore no
        -- answer here, which is exactly what Realmbreaker's Grasp's "unless
        -- they're mana abilities" says.
        && not (ActivationProhibition.prohibited AbilityKind.ManaAbility oid gs)
        -- CR 602.5's player-axis prohibition (Sen Triplets), read here for
        -- detain's reason: the sentence carves no mana ability out where CR
        -- 702.61b does, so both of CR 605.3a's windows owe it. The stamp is what
        -- a row naming a keyword (Kang the Conqueror's power-up) compares; no
        -- mana ability in data/cards/ is under power-up, so on this road that
        -- is a regression fence rather than a proof.
        && not (PlayerEffect.prohibitsActivatingGiven (ActivatedAbility.keyword =<< ability) effects)
        -- CR 602.5's printed "activate only ..." rider, which CR 605.1's own sentence
        -- keeps on a mana ability -- a timing restriction does not stop an ability
        -- being one. Here for sickness's and detain's reason, and it is the reason
        -- this argument is threaded from the ABILITY all the way down (CR 605.3a):
        -- Pawl.Engine.Activate refuses a mana ability one conjunct before it reads
        -- the rider, so a rider read only there is a rider nothing reads.
        --
        -- CR 109.4a and CR 113.8 make `pid` the rider's "you": a mana ability's
        -- controller is the player activating it, which Mana Cache's "during
        -- their turn" reads (Pawl.ManaSpec's Mana Cache group). CR 602.1b's
        -- other instruction, who may activate at all, is Mana.permitsRoute.
        && ActivationRestriction.restrictionsOk pid oid ability restrictions gs
        then
          Activations.MkActivations
            { -- The PRINTED mana part, which is what `repeatsOf` reads: a
              -- reduction that emptied it would make the route repeatable where
              -- every activation still has to find that mana again. Both readers
              -- hand this function the same cost, so both get the same count.
              Activations.times = repeatsOf pid oid cost gs,
              Activations.claims = claimsOf Map.empty pid oid (Cost.components cost) gs,
              Activations.life = lifeOwedBy pid gs (Cost.components cost),
              Activations.energy = energyOwedBy (Cost.components cost)
            }
        else Activations.MkActivations {Activations.times = 0, Activations.claims = [], Activations.life = 0, Activations.energy = 0}

-- CR 601.2f's adjustments for a MANA ability's activation cost, gathered where
-- CR 605.3b leaves no stack window for Pawl.Engine.Activate to gather them in.
--
-- NO TARGETS either, and CR 605.1a is why there can be none: a mana ability
-- "doesn't target", so the set a reducer would be asked about is empty by the
-- rule rather than by this caller's position.
--
-- The rule-702 stamp IS threaded, off the route's own ability
-- (Mana.manaRoutesOfGiven), because CR 702.177a's exhaust is printed on the
-- ability rather than minting one and a mana ability can carry it -- Loot, the
-- Pathfinder's "Exhaust -- {G}, {T}: Add three mana of any one color" is the
-- pool's, and Boom Scholar's "exhaust abilities of other permanents you control"
-- is what would name it. A family-minted ability could not reach here (CR
-- 605.1a: one adds mana, targets nothing and is no loyalty ability, which
-- cycling's card movement rules out), so this argument was exact while exhaust
-- did not exist.
--
-- LoyaltyKind.NonLoyaltyAbility is exact rather than elided for the same reason
-- and off the same rule: CR 605.1a's third criterion is "it's not a loyalty
-- ability", so nothing reaching this function can be one, and Carth the Lion's
-- addition spares every activation gathered here -- including a planeswalker's
-- granted mana ability (A Realm Reborn).
--
-- AbilityKind.ManaAbility is the THIRD criterion, and the Nothing above could
-- never have stood in for it: no rule-702 provenance is equally true of every
-- ordinary activated ability arriving through Pawl.Engine.Activate, while
-- everything reaching this function is a mana ability by CR 605.1a -- which is
-- what makes Suppression Field's "unless they're mana abilities" and Zirda, the
-- Dawnwaker's "that aren't mana abilities" spare every activation gathered here,
-- the increase side and the reduction side of one rider.
manaActivationAdjustments :: Maybe Keyword.Type.Keyword -> PlayerId -> ObjectId -> GameState -> CostAdjustments.CostAdjustments
manaActivationAdjustments stamp pid oid gs = manaActivationAdjustmentsGiven (PlayerEffect.applying pid gs) pid stamp oid gs

-- The same gather off a hoisted effect list; see manaActivationsGiven.
manaActivationAdjustmentsGiven :: [(RowSource, PlayerEffect.Type.PlayerEffect)] -> PlayerId -> Maybe Keyword.Type.Keyword -> ObjectId -> GameState -> CostAdjustments.CostAdjustments
manaActivationAdjustmentsGiven effects pid stamp = PlayerEffect.activationCostAdjustmentsGiven effects pid Set.empty stamp AbilityKind.ManaAbility LoyaltyKind.NonLoyaltyAbility

-- CR 118.3 asked of a mana ability's own MANA part, and the one read
-- manaActivations makes that could ask itself. Nothing is CR 118.6's unpayable
-- cost -- an object with no mana cost -- and is `canPay`'s own first arm; an
-- EMPTY mana part is payable with no walk at all, which is most of `data/cards/`
-- and is what keeps a whole-board walk off the tap-for-mana path.
--
-- The life and the claims are the components', because CR 118.3's "fully"
-- reaches across the two halves of one cost -- the same handoff `canPay` makes.
--
-- ANY total CR 601.2f could reach, canPaySomeCompletionGiven's posture: the
-- reduction's hybrid halves (CR 118.7e) and the order of several reductions are
-- the payer's, unmade at this moment, so a cost this refuses has to be one NO
-- resolution could have paid.
manaPartPayable :: [(RowSource, PlayerEffect.Type.PlayerEffect)] -> CostAdjustments.CostAdjustments -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Bool
manaPartPayable effects adjustments pid oid cost gs = case Cost.mana cost of
  Nothing -> False
  Just (ManaCost.MkManaCost []) -> True
  Just manaCost ->
    any
      ( \totalled ->
          Mana.canPayCommitting
            (PaymentSubject.Activating oid Nothing Nothing)
            (manaActivationsGiven effects)
            ManaSpending.AsProduced
            pid
            (lifeOwedBy pid gs (Cost.components cost))
            (energyOwedBy (Cost.components cost))
            (claimsOf Map.empty pid oid (Cost.components cost) gs)
            totalled
            gs
      )
      (totalManas adjustments manaCost)

-- How many times IN A ROW a cost already known to be payable once could be paid
-- -- what makes Ashnod's Altar beside two creatures two mana activations, and
-- Treasonous Ogre ("Pay 3 life: Add {R}") at 20 life six.
--
-- The SMALLEST ceiling the cost's resources impose (CR 118.3's "fully"). Four
-- are counted, each totalled over the WHOLE cost: OBJECTS, through
-- Pawl.Engine.Claim.repeats, so two components drawing on one pool do not each
-- get it -- untapped-ness is one such pool (ClaimAxis.Tapping), which is what
-- makes Heritage Druid beside nine untapped Elves three activations and nine
-- mana; LIFE, through CR 119.4, so the ceiling is the life total divided by
-- `lifeOwedBy`; ENERGY, through CR 107.14, the same division by `energyOwedBy`
-- -- Synthetic Dynamo Conduit at three energy is three activations; and the
-- COUNTERS on the source, CR 122.1's marker, so each kind's ceiling is what it
-- carries divided by `countersOwedBy` -- Workhorse's four +1\/+1 counters are
-- four activations and four mana.
--
-- Each totalled and then DIVIDED for one reason: two components spending the same
-- resource would each get the whole of it if they were asked separately, which
-- would OVERSTATE, and this function's whole direction is the other way.
--
-- Anything else is `uncountedCeiling`'s, one component at a time: a THRESHOLD
-- on an aggregate there counts the disjoint selections reaching it, and most
-- of the rest cap the answer at 1. A cost imposing no ceiling at all -- a CR
-- 701.68a blight alone, which a player controlling a creature can pay any
-- number of times -- answers 1 too, an understatement and the safe direction
-- below. So does a MANA
-- part, repeating which would spend mana this function has not measured. The
-- offer paths (Mana.manaSourcesGiven, tapForMana) read only whether the count
-- exceeds 0, so that arm decides nothing for them; the SUPPLY walk reads the
-- count itself, and a mana-eating route counted twice would owe its own cost
-- twice over while Mana.payableResolutionsGiven charged it once -- which is where
-- the sentence is load-bearing.
-- Understating is the safe direction, and why the uncounted components cap
-- rather than divide: a supply too large offers a cast that then cannot be paid,
-- and an offer that changes nothing is offered again forever.
--
-- Both ceilings are this source asked ALONE against the untouched board; where
-- something else IS spending they are loose, and Pawl.Engine.Mana's joint
-- question across sources tightens them.
repeatsOf :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Natural
repeatsOf pid oid cost gs =
  let components = Cost.components cost
      claims = claimsOf Map.empty pid oid components gs
      objectCeiling = if null claims then [] else [Claim.repeats claims]
      lifeCeiling = case lifeOwedBy pid gs components of
        0 -> []
        owed -> [div (lifeTotalOf pid gs) owed]
      energyCeiling = case energyOwedBy components of
        0 -> []
        owed -> [div (Game.energyOf pid gs) owed]
      counterCeiling = [div (countersOn kind oid gs) owed | (kind, owed) <- Map.toList (countersOwedBy components), owed > 0]
      ceilings = objectCeiling <> lifeCeiling <> energyCeiling <> counterCeiling <> Maybe.mapMaybe (uncountedCeiling pid oid claims gs) components
   in case Cost.mana cost of
        Just (ManaCost.MkManaCost []) -> case ceilings of
          [] -> 1
          limits -> minimum limits
        _ -> 1

-- The ceiling ONE component imposes that `repeatsOf`'s four totals do not
-- already carry, or Nothing where one of them does or where it spends nothing
-- that runs out. 1 for every resource this module cannot count, and for two it
-- need not. EXACT for CR 107.5's {T}, CR 107.6's {Q} and CR 606.4's loyalty (CR
-- 606.3 allows one loyalty ability per turn whatever the counters allow).
--
-- `claims` are the whole cost's, which a threshold arm reads to learn whether
-- it is the only component drawing on its pool.
--
-- EXHAUSTIVE with no wildcard, this module's posture, and -Werror makes it.
uncountedCeiling :: PlayerId -> ObjectId -> [Claim] -> GameState -> CostComponent.CostComponent Keyword.Type.Keyword -> Maybe Natural
uncountedCeiling pid oid claims gs component = case component of
  -- Counted by `objectCeiling`.
  CostComponent.Sacrifice {} -> Nothing
  CostComponent.SacrificeThis -> Nothing
  CostComponent.ReturnThis -> Nothing
  CostComponent.ReturnPermanents {} -> Nothing
  CostComponent.ExilePermanents {} -> Nothing
  CostComponent.DiscardCards {} -> Nothing
  CostComponent.DiscardThis _ -> Nothing
  CostComponent.PutCardFromHandOntoBattlefield _ -> Nothing
  CostComponent.ExileCardFromHand _ -> Nothing
  CostComponent.ExileCardsFromGraveyard {} -> Nothing
  -- 1, and counted by none of the four totals: `claimOf` states no claim for
  -- this component either, Forage's answer below and for its reason. An
  -- UNDERSTATEMENT and the header's safe direction.
  CostComponent.ExileMaterials {} -> Just 1
  CostComponent.ExileTopFromGraveyard _ -> Nothing
  CostComponent.ExileThisFromGraveyard -> Nothing
  CostComponent.ExileThis -> Nothing
  -- Counted by `objectCeiling` too, the library being this claim's pool.
  CostComponent.MillCards _ -> Nothing
  -- Counted by `lifeCeiling`, CR 119.4.
  CostComponent.PayLife _ -> Nothing
  CostComponent.PayHalfLife _ -> Nothing
  -- Zero, not the 1 the uncounted components take: an unannounced X cannot be
  -- paid even once (`canPayComponent`). Unreachable, since `manaActivations`
  -- asks canPayComponent of every component before reaching `repeatsOf`.
  CostComponent.PayLifeX -> Just 0
  -- Zero, PayLifeX's answer above and for its reason.
  CostComponent.PayEnergyX -> Just 0
  CostComponent.TapThis -> Just 1
  CostComponent.UntapThis -> Just 1
  -- A THRESHOLD on an aggregate, so NOT `objectCeiling`'s division: four 1/1s
  -- pay a total power of 3 once, not four times. `thresholdRepeats` over the
  -- candidates' powers, `canPayComponent`'s pool and reading -- six 1/1s are two
  -- activations of Synthetic Muster Dynamo (Pawl.ManaSpec). A threshold of 0
  -- taps nothing and imposes nothing.
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower threshold criterion)
    | threshold > 0 ->
        Just
          ( alone
              ClaimAxis.Tapping
              threshold
              (fmap (max 0 . (`tapPower` gs)) (tapCandidates Map.empty pid oid criterion gs))
          )
    | otherwise -> Nothing
  -- TapForTotalPower's arm above one zone over: CR 701.59a's number is a
  -- threshold on total mana value, so three one-drops collect evidence 3 once,
  -- not three times. A FENCE: Cryptex, the one printed mana ability collecting
  -- evidence, also charges {T}.
  CostComponent.CollectEvidence threshold
    | threshold > 0 ->
        Just
          ( alone
              (ClaimAxis.Removal Zone.Graveyard)
              threshold
              (fmap (`evidenceValue` gs) (evidenceCandidates Map.empty pid oid gs))
          )
    | otherwise -> Nothing
  -- The arm above at the amount no targets fix: CR 605.3b's mana ability has no
  -- CR 601.2c step (fixComputed).
  CostComponent.CollectEvidenceOfTargets -> uncountedCeiling pid oid claims gs (fixComputed Map.empty gs component)
  -- Counted by `objectCeiling`, on ClaimAxis.Tapping: the count is exact, so the
  -- pool of untapped candidates divided by it is how many times in a row the
  -- component can be paid. Heritage Druid's nine Elves are three activations
  -- (Pawl.ManaSpec).
  CostComponent.TapPermanents {} -> Nothing
  -- Counted by `energyCeiling`, CR 107.14.
  CostComponent.PayEnergy _ -> Nothing
  CostComponent.AddLoyaltyToThis _ -> Just 1
  CostComponent.RemoveLoyaltyFromThis _ -> Just 1
  -- Counted by `counterCeiling`, CR 122.1.
  CostComponent.RemoveCountersFromThis _ -> Nothing
  -- 1, and NOT folded into `counterCeiling`: that ceiling divides the counters
  -- on `oid`, and this component takes them off ANOTHER permanent, so the
  -- division would measure the wrong pile. An UNDERSTATEMENT -- a creature with
  -- nine counters pays Zameck Guildmage's cost nine times -- and the header's
  -- safe direction. MTGJSON 2026-08-23, a cost removing counters from anything
  -- but "this" followed by ": Add": no printing.
  CostComponent.RemoveCounters {} -> Just 1
  -- Zero, PayLifeX's answer above and for its reason.
  CostComponent.RemovePlusOneCountersX _ -> Just 0
  CostComponent.SacrificeX _ -> Just 0
  -- Nothing: this component PUTS counters on, so it spends nothing that runs
  -- out, and repeating it is bounded by whatever else the cost spends. Blight's
  -- arm below and for its reason; a FENCE, no mana ability in `data/cards/`
  -- pairing it with a counted resource.
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing
  -- Nothing, the arm above's answer: CR 701.20c lets a revealed card be
  -- revealed again, so CR 701.20b spends nothing. A FENCE, the arm above's.
  CostComponent.RevealCardFromHand _ -> Nothing
  -- Nothing, the arm above's answer: neither of CR 701.4a's halves spends
  -- anything. A FENCE, the arm above's.
  CostComponent.Behold _ -> Nothing
  -- 1, ExileMaterials' answer above and for its reason: `claimOf` states no
  -- claim for this component. An understatement, the header's safe direction.
  CostComponent.BeholdAndExile _ -> Just 1
  -- Nothing: CR 701.68a puts counters on a creature and takes nothing out of
  -- any pool, and CR 704.3 checks no state-based action inside CR 601.2g's
  -- mana window, so the creature blighted stays to be blighted again. The rest
  -- of the cost is the bound -- Synthetic Withering Font's life (Pawl.ManaSpec).
  CostComponent.Blight _ -> Nothing
  -- Zero, PayLifeX's answer above and for its reason: an unannounced X cannot be
  -- paid even once.
  CostComponent.BlightX -> Just 0
  -- Zero, BlightX's answer above and for its reason.
  CostComponent.RemoveLoyaltyFromThisX -> Just 0
  -- 1, and counted by none of the four totals: `claimOf` states no claim for
  -- this component, so `objectCeiling` has no pool to divide. An UNDERSTATEMENT
  -- -- a graveyard of nine pays three forages -- and the header's safe direction.
  CostComponent.Forage -> Just 1
  -- Nothing, Blight's answer above: nothing in rule 705 bounds how many coins a
  -- player may flip, so the rest of the cost is the bound. A FENCE,
  -- PutPlusOneCountersOnThis' above.
  CostComponent.FlipCoin -> Nothing
  -- 1: `claimOf` states no claim, so there is no pool to divide, and rule
  -- 702.174a's cost is offered once per gift ability
  -- (Pawl.Engine.Keyword.optionalCost).
  CostComponent.ChooseOpponent -> Just 1
  -- Zero, BlightX's answer above and for its reason: an unannounced X
  -- cannot be paid even once.
  CostComponent.WaterbendX -> Just 0
  -- 1, and counted by none of the four totals: rule 701.67a's licence spends
  -- nothing, so `objectCeiling` has no pool to divide. Unreachable -- a
  -- waterbend cost carries the mana it licenses, so `repeatsOf` answers 1
  -- before it reads this.
  CostComponent.Waterbend _ -> Just 1
  CostComponent.WaterbendInstead _ -> Just 1
  where
    -- `thresholdRepeats` where this is the ONLY claim on its axis, and 1 where
    -- another component of the cost draws on it too: the selections would then
    -- have to leave that component its share, which this count cannot see.
    alone axis threshold amounts =
      if Natural.length (filter ((== axis) . Claim.Type.axis) claims) == 1
        then thresholdRepeats (toInteger threshold) amounts
        else 1

-- How many DISJOINT selections of these amounts each reach `threshold`, every
-- one holding the fewest amounts any selection can (Claim.fewestReaching) --
-- the repeats of a threshold cost. An UNDERSTATEMENT where uneven amounts would
-- allow more selections of mixed sizes -- {3, 1, 1, 1} reach 3 twice, and this
-- counts once -- which is the header's safe direction.
--
-- EXACT otherwise, by search rather than greedily. Some best answer puts the
-- largest amount in a selection, since it can replace any member of one, so
-- the search takes it and tries every minimal completion.
thresholdRepeats :: Integer -> [Integer] -> Natural
thresholdRepeats threshold amounts =
  let size = Claim.fewestReaching threshold amounts
      grouped = Map.toDescList (Map.fromListWith (+) [(amount, 1 :: Natural) | amount <- amounts, amount > 0])
   in if threshold <= 0 then 0 else selectionsOf threshold size grouped

-- `thresholdRepeats`' search over amounts grouped by value, largest first.
selectionsOf :: Integer -> Natural -> [(Integer, Natural)] -> Natural
selectionsOf threshold size groups = case groups of
  [] -> 0
  (largest, count) : rest ->
    let remaining = if count > 1 then (largest, count - 1) : rest else rest
        bound = min (div (sum (fmap (\(amount, n) -> amount * toInteger n) groups)) threshold) (toInteger (div (sum (fmap snd groups)) (max 1 size)))
        tries = [1 + selectionsOf threshold size (without chosen remaining) | chosen <- reachingCompletions (size - 1) (threshold - largest) remaining]
     in if bound <= 0 then 0 else upTo (Integer.toNaturalSaturating bound) tries
  where
    -- The best of these, stopping at the first to reach the bound.
    upTo bound = List.foldl' (\best try -> if best >= bound then best else max best try) 0
    without chosen = Maybe.mapMaybe (\(amount, n) -> let taken = Maybe.fromMaybe 0 (lookup amount chosen) in if n > taken then Just (amount, n - taken) else Nothing)

-- Every way to take at most `budget` more of these grouped amounts so that
-- together they reach `need`, each MINIMAL -- the last amount taken is the one
-- that reaches it -- as a count per value.
reachingCompletions :: Natural -> Integer -> [(Integer, Natural)] -> [[(Integer, Natural)]]
reachingCompletions budget need groups
  | need <= 0 = [[]]
  | otherwise = case groups of
      [] -> []
      (amount, count) : rest
        | toInteger budget * amount < need -> []
        | otherwise ->
            let enough = Integer.toNaturalSaturating (div (need + amount - 1) amount)
                finishing = [[(amount, enough)] | enough <= min count budget]
                partial =
                  [ [(amount, taken) | taken > 0] <> more
                  | taken <- [0 .. minimum [count, budget, enough - 1]],
                    more <- reachingCompletions (budget - taken) (need - toInteger taken * amount) rest
                  ]
             in finishing <> partial

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
--
-- `substitute` is CR 702.51b's, CR 702.66b's, CR 702.126b's and CR 701.67a's
-- offer, applied AFTER `total_` because every one of those rules places it after
-- the total cost is determined: it answers one residual cost per set of symbols a
-- substitute could pay for, each with the components that spends
-- (`manaSubstitutions`), and this asks `any` of those too. A CAST passes
-- `manaSubstitutions` and an ACTIVATION `waterbendSubstitutions`, which is
-- the waterbend half alone; `canPayReading` asks the latter's the same way.
canPaySomeCompletion :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PaymentSubject.PaymentSubject -> ManaSpending.ManaSpending -> PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> (ManaCost.ManaCost -> [(ManaCost.ManaCost, [(Keyword.Substitute, Natural)])]) -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPaySomeCompletion slots subject spending pid oid total_ substitute cost gs =
  let pcs = Projection.projectAll gs
   in canPaySomeCompletionGiven slots subject spending (supplyManaSourcesGiven (Projection.controlGrants gs) pcs pid gs) pcs pid oid total_ substitute cost gs

-- The mana sources CR 605.3a OFFERS: every permanent this player could tap for
-- mana right now, whatever that tap itself costs. ONE function pairing
-- `manaActivations` with the sweep taken under it, so a hoisted list cannot be
-- built under a capacity the reader does not read.
--
-- Action.legalActions' ActivateManaAbility offers are this list and nothing
-- else. It is NOT the list a payability GATE is judged against; that one is
-- supplyManaSourcesGiven below, and NEITHER contains the other -- see there.
activationManaSourcesGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
activationManaSourcesGiven grants pcs pid gs = Mana.manaSourcesGiven Set.empty (manaActivationsGiven (PlayerEffect.applying pid gs)) grants pcs pid gs

-- The mana sources a payability GATE is judged against -- the same sweep under
-- Mana.supplyCapacity, which is the invariant Mana.payableResolutionsGiven
-- states and its type cannot. NEITHER list contains the other, and the two
-- wrappers are why:
--
--   * Mana.supplyCapacity is the same capacity asked Mana.ForSupply, which drops
--     the CR 118.3 read of the route's own mana part and so only ADDS sources --
--     Transmogrant Altar beside no Swamp is a supply source that is no offer.
--     Nothing is overstated by it: such a permanent's option carries the mana
--     part as a DEMAND the board must then serve, and taking it zero times is
--     always among its options.
--
--   * `stackedManaActivations` only REMOVES them, and it is the whole of what
--     both gates below are: every caller is gating an action CR 601.2a or CR
--     602.2a puts on the stack BEFORE its cost is paid, which closes CR 307.5's
--     window. Grinning Ignus in that window is an offer that is no supply.
supplyManaSourcesGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
supplyManaSourcesGiven grants pcs pid gs = Mana.manaSourcesGiven Set.empty (Mana.supplyCapacity (stackedManaActivations (PlayerEffect.applying pid gs))) grants pcs pid gs

-- `manaActivationsGiven` narrowed to the routes a payment made AFTER its object
-- reached the stack may take -- which is every cast (CR 601.2a) and every
-- activation (CR 602.2a), both rules putting the object on the stack a step
-- before CR 601.2f-h totals and pays the cost.
--
-- CR 307.5's empty-stack conjunct is the only window that move closes, and
-- ActivationRestriction.needsEmptyStack is what reports it. Asked of the RIDER
-- and not of the board: this runs one step ahead of the move, so the live
-- GameState.stack is the wrong stack to read -- an empty one here says nothing
-- about the payment, and reading it counted Grinning Ignus's "{R}, Return this
-- creature to its owner's hand: Add {C}{C}{R}. Activate only as a sorcery" as a
-- supply for a cast whose own proposal had already closed its window, so the cast
-- was offered and then rewound under CR 601.2 (#2005).
--
-- NOT A LOSS OF THE PLAY: CR 605.3a's priority window is untouched
-- (activationManaSourcesGiven above), so the mana is floated first with the stack
-- still empty and then spent out of the pool, which is what the rules leave the
-- player and what the Ignus is printed to do.
--
-- The IN-PAYMENT window needs no arm of its own: Cost.payMana and payManaExcept
-- run with the object already on the stack, so ActivationRestriction.restrictionsOk
-- reads the real stack there and refuses the same routes. The gate and the
-- payment agree because this function states exactly the move between them.
stackedManaActivations :: [(RowSource, PlayerEffect.Type.PlayerEffect)] -> Mana.Capacity
stackedManaActivations effects = midPayment (stackedAt effects)
  where
    stackedAt given measure pcs pid oid cost restrictions ability gs =
      if any ActivationRestriction.needsEmptyStack restrictions
        then Mana.noActivations
        else manaActivationsGiven given measure pcs pid oid cost restrictions ability gs

-- CR 602.5e inside CR 605.3a's payment windows: a route whose rider keeps it to
-- the priority window (ActivationRestriction.refusedMidPayment) is no route
-- while something is being paid for. Rhystic Cave's "Activate only as an
-- instant", which its ruling reads as forbidding exactly this, is what keeps
-- its "unless any player pays {1}" from leaving a payment short: the mana a
-- paid gate withholds was never counted as supply and never offered mid-payment.
midPayment :: Mana.Capacity -> Mana.Capacity
midPayment capacity measure pcs pid oid cost restrictions ability gs =
  if any ActivationRestriction.refusedMidPayment restrictions
    then Mana.noActivations
    else capacity measure pcs pid oid cost restrictions ability gs

-- The same question given a board the CALLER has already walked; handing the
-- board in changes no answer. Build `sources` with supplyManaSourcesGiven
-- above and nothing else. ONLY the mana half gets the pre-walked board -- the
-- COMPONENTS are still asked through canPayComponent, whose Sacrifice,
-- TapForTotalPower and CollectEvidence arms make per-object walks of their own
-- (#1448).
canPaySomeCompletionGiven :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PaymentSubject.PaymentSubject -> ManaSpending.ManaSpending -> [ObjectId] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> (ManaCost.ManaCost -> [(ManaCost.ManaCost, [(Keyword.Substitute, Natural)])]) -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPaySomeCompletionGiven slots subject spending sources pcs pid oid total_ substitute cost gs = case Cost.mana cost of
  Nothing -> False
  Just (ManaCost.MkManaCost symbols) ->
    let outside = lifeOwedBy pid gs (Cost.components cost)
        outsideEnergy = energyOwedBy (Cost.components cost)
        claimed = claimsOf slots pid oid (Cost.components cost) gs
        -- CR 702.51b's and CR 702.66b's substitutes come with components of
        -- their own (`manaSubstitutions`), and CR 118.3 weighs those against the
        -- cost's own
        -- rather than beside them: a creature tapped for convoke is one the
        -- printed cost cannot also tap. So the claims and the component gates are
        -- asked of the UNION, once per entry -- and the no-substitute entry every
        -- other caller offers is answered out of the hoisted pair, which is the
        -- whole of what this costs a cast with neither keyword.
        componentsWith extra = Cost.components cost <> substituteComponents extra
        claimsWith extra = if null extra then claimed else claimsOf slots pid oid (componentsWith extra) gs
        ownComponentsOk = componentsPayable slots pid oid (Cost.components cost) gs
        componentsOkWith extra = if null extra then ownComponentsOk else componentsPayable slots pid oid (componentsWith extra) gs
        -- One player-effect gather for the whole question, shared by every
        -- source Mana.manaSuppliesGiven measures under it.
        --
        -- `stackedManaActivations` and NOT `manaActivationsGiven`: both callers
        -- gate an action CR 601.2a or CR 602.2a puts on the stack before its cost
        -- is paid. It must also MATCH what `sources` was built under
        -- (supplyManaSourcesGiven) -- a source listed there whose every route this
        -- capacity refuses offers nothing, and Mana.payableResolutionsGiven's
        -- product turns one of those into no board at all.
        --
        -- A FENCE on this half and not proven behaviour: the source list is
        -- narrowed the same way, so the one permanent this refuses is already off
        -- it, and unstacking THIS capacity alone leaves the suite green. What
        -- would observe it is a permanent with TWO mana routes, only one carrying
        -- the rider -- a source on the free route, which this must still refuse
        -- the ridden one of -- and the rider has to be one
        -- ActivationRestriction.needsEmptyStack answers True for. In `data/cards/`
        -- those two sets do not meet: Grinning Ignus, Lavinia and Synthetic Ember
        -- Spring carry the pool's three refusable riders on a mana ability and each
        -- has one route, while Gemstone Caverns, Muraganda Raceway and Phyrexian
        -- Tower have two routes and no rider. Nimbus Maze is both and still not
        -- it: two of its three routes carry CR 602.5's board condition, which
        -- needsEmptyStack admits.
        hoisted = stackedManaActivations (PlayerEffect.applying pid gs)
        payable (completed, life) =
          any
            ( any
                ( \(residual, extra) ->
                    componentsOkWith extra
                      && Mana.canPayCommittingGiven subject hoisted spending sources pcs pid (outside + life) outsideEnergy (claimsWith extra) residual gs
                )
                . substitute
            )
            (total_ (ManaCost.MkManaCost completed))
     in any payable (Mana.completions symbols)

-- CR 107.1a / 107.1b: half the payer's life total, rounded as printed, and 0
-- from a total at or below 0 -- Lurking Evil's ruling, "if you have zero or
-- negative life, half your life is zero".
halfLifeOf :: Rounding.Rounding -> PlayerId -> GameState -> Natural
halfLifeOf rounding pid gs =
  maybe 0 (Integer.toNaturalSaturating . Quantity.halve rounding . Player.life) (Map.lookup pid (GameState.players gs))

-- CR 601.2f: a half-life cost is fixed to a number once, against the life total
-- the payer has as the total cost is determined, so a CR 601.2g mana ability
-- that pays life cannot move it. Applied by `announce`, the seam a cast, an
-- activation and a CR 118.12 payment each pass before paying; Pawl.CostSpec's
-- Murderous Betrayal case is the proof.
fixHalfLife :: PlayerId -> GameState -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
fixHalfLife pid gs cost =
  let fixed component = case component of
        CostComponent.PayHalfLife rounding -> CostComponent.PayLife (halfLifeOf rounding pid gs)
        _ -> component
   in cost {Cost.components = fmap fixed (Cost.components cost)}

-- CR 601.2f: a component whose amount the targets compute, fixed to the amount
-- `slots` give it -- CR 601.2c's targets, or the aiming a gate measures. Urgent
-- Necropsy's "the total mana value of the permanents this spell targets": each
-- DISTINCT object once, so an artifact creature named by two of its slots counts
-- once, and only those still on the battlefield, which is what a permanent is
-- (CR 110.1). A player target has no mana value and is not in `slots`.
fixComputed :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> GameState -> CostComponent.CostComponent Keyword.Type.Keyword -> CostComponent.CostComponent Keyword.Type.Keyword
fixComputed slots gs component
  | targetComputed component =
      let permanents = Set.filter (\candidate -> Game.zoneOf candidate gs == Just Zone.Battlefield) (Set.unions (Map.elems slots))
       in CostComponent.CollectEvidence (Integer.toNaturalSaturating (sum (fmap (`evidenceValue` gs) (Set.toList permanents))))
  | otherwise = component

-- fixComputed over a whole cost. Urgent Necropsy's ruling locks its X in once
-- the targets are chosen and before any of the cost is paid, which is where
-- Pawl.Engine.Cast.castProposed and Pawl.Engine.Activate.activateAbility call
-- this.
fixComputedIn :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> GameState -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
fixComputedIn slots gs cost = cost {Cost.components = fmap (fixComputed slots gs) (Cost.components cost)}

-- Is this component's amount computed from the targets? EXHAUSTIVE with no
-- wildcard: a new such component owes an answer here, or neither readsBoundSlot
-- nor fixComputed sees it.
targetComputed :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
targetComputed component = case component of
  CostComponent.CollectEvidenceOfTargets -> True
  CostComponent.CollectEvidence _ -> False
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.SacrificeThis -> False
  CostComponent.ReturnThis -> False
  CostComponent.PayLife _ -> False
  CostComponent.PayLifeX -> False
  CostComponent.PayHalfLife _ -> False
  CostComponent.Sacrifice _ -> False
  CostComponent.SacrificeX _ -> False
  CostComponent.TapForTotalPower _ -> False
  CostComponent.TapPermanents _ -> False
  CostComponent.ReturnPermanents _ -> False
  CostComponent.ExilePermanents _ -> False
  CostComponent.DiscardCards _ -> False
  CostComponent.DiscardThis _ -> False
  CostComponent.PutCardFromHandOntoBattlefield _ -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.PayEnergyX -> False
  CostComponent.AddLoyaltyToThis _ -> False
  CostComponent.RemoveLoyaltyFromThis _ -> False
  CostComponent.RemoveLoyaltyFromThisX -> False
  CostComponent.RemoveCountersFromThis _ -> False
  CostComponent.RemoveCounters _ -> False
  CostComponent.RemovePlusOneCountersX _ -> False
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.Blight _ -> False
  CostComponent.Forage -> False
  CostComponent.FlipCoin -> False
  CostComponent.BlightX -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileThis -> False
  CostComponent.ExileCardsFromGraveyard _ -> False
  CostComponent.ExileMaterials _ -> False
  CostComponent.ExileTopFromGraveyard _ -> False
  CostComponent.ExileCardFromHand _ -> False
  CostComponent.RevealCardFromHand _ -> False
  CostComponent.Behold _ -> False
  CostComponent.BeholdAndExile _ -> False
  CostComponent.MillCards _ -> False
  CostComponent.ChooseOpponent -> False
  CostComponent.Waterbend _ -> False
  CostComponent.WaterbendInstead _ -> False
  CostComponent.WaterbendX -> False

-- CR 119.4's payments a cost owes OUTSIDE its mana part, added up -- what CR
-- 118.3 makes the mana part's own life share a total with. Total, so a new
-- life-spending component cannot be added without answering here. The payer and
-- the board are PayHalfLife's, whose amount is measured against them.
lifeOwedBy :: PlayerId -> GameState -> [CostComponent.CostComponent Keyword.Type.Keyword] -> Natural
lifeOwedBy pid gs = sum . fmap (lifeOwedByComponent pid gs)

lifeOwedByComponent :: PlayerId -> GameState -> CostComponent.CostComponent Keyword.Type.Keyword -> Natural
lifeOwedByComponent pid gs component = case component of
  CostComponent.PayLife n -> n
  -- CR 118.3: shares a total with the life a mana ability spends. Pawl.CostSpec's
  -- "CR 118.3 Murderous Betrayal's half and Mana Confluence's life are weighed
  -- together" is the proof.
  CostComponent.PayHalfLife rounding -> halfLifeOf rounding pid gs
  -- 0, an unannounced X naming no amount to owe. Not a claim that this component
  -- is free: `canPayComponent` refuses it outright.
  CostComponent.PayLifeX -> 0
  CostComponent.PayEnergyX -> 0
  CostComponent.TapThis -> 0
  CostComponent.UntapThis -> 0
  CostComponent.SacrificeThis -> 0
  CostComponent.ReturnThis -> 0
  CostComponent.Sacrifice {} -> 0
  CostComponent.TapForTotalPower {} -> 0
  CostComponent.TapPermanents {} -> 0
  CostComponent.ReturnPermanents {} -> 0
  CostComponent.ExilePermanents {} -> 0
  CostComponent.DiscardCards {} -> 0
  CostComponent.DiscardThis _ -> 0
  CostComponent.PutCardFromHandOntoBattlefield _ -> 0
  CostComponent.PayEnergy _ -> 0
  CostComponent.AddLoyaltyToThis _ -> 0
  CostComponent.RemoveLoyaltyFromThis _ -> 0
  CostComponent.RemoveCountersFromThis _ -> 0
  CostComponent.RemoveCounters {} -> 0
  CostComponent.RemovePlusOneCountersX _ -> 0
  CostComponent.SacrificeX _ -> 0
  CostComponent.PutPlusOneCountersOnThis _ -> 0
  CostComponent.Blight _ -> 0
  CostComponent.BlightX -> 0
  CostComponent.RemoveLoyaltyFromThisX -> 0
  CostComponent.Forage -> 0
  CostComponent.FlipCoin -> 0
  CostComponent.ChooseOpponent -> 0
  CostComponent.WaterbendX -> 0
  CostComponent.Waterbend _ -> 0
  CostComponent.WaterbendInstead _ -> 0
  CostComponent.ExileThisFromGraveyard -> 0
  CostComponent.ExileThis -> 0
  CostComponent.ExileCardsFromGraveyard {} -> 0
  CostComponent.ExileMaterials {} -> 0
  CostComponent.ExileTopFromGraveyard _ -> 0
  CostComponent.CollectEvidence _ -> 0
  CostComponent.CollectEvidenceOfTargets -> 0
  CostComponent.ExileCardFromHand _ -> 0
  CostComponent.RevealCardFromHand _ -> 0
  CostComponent.Behold _ -> 0
  CostComponent.BeholdAndExile _ -> 0
  CostComponent.MillCards _ -> 0

-- CR 107.14's payments a cost owes outside its mana part, added up --
-- `lifeOwedBy`'s energy sibling, and what `repeatsOf`'s energyCeiling divides
-- by. Total for lifeOwedBy's reason.
energyOwedBy :: [CostComponent.CostComponent Keyword.Type.Keyword] -> Natural
energyOwedBy = sum . fmap energyOwedByComponent

energyOwedByComponent :: CostComponent.CostComponent Keyword.Type.Keyword -> Natural
energyOwedByComponent component = case component of
  CostComponent.PayEnergy n -> n
  -- 0, PayLifeX's answer in lifeOwedByComponent and for its reason.
  CostComponent.PayEnergyX -> 0
  CostComponent.PayLife _ -> 0
  CostComponent.PayHalfLife _ -> 0
  CostComponent.PayLifeX -> 0
  CostComponent.TapThis -> 0
  CostComponent.UntapThis -> 0
  CostComponent.SacrificeThis -> 0
  CostComponent.ReturnThis -> 0
  CostComponent.Sacrifice {} -> 0
  CostComponent.TapForTotalPower {} -> 0
  CostComponent.TapPermanents {} -> 0
  CostComponent.ReturnPermanents {} -> 0
  CostComponent.ExilePermanents {} -> 0
  CostComponent.DiscardCards {} -> 0
  CostComponent.DiscardThis _ -> 0
  CostComponent.PutCardFromHandOntoBattlefield _ -> 0
  CostComponent.AddLoyaltyToThis _ -> 0
  CostComponent.RemoveLoyaltyFromThis _ -> 0
  CostComponent.RemoveCountersFromThis _ -> 0
  CostComponent.RemoveCounters {} -> 0
  CostComponent.RemovePlusOneCountersX _ -> 0
  CostComponent.SacrificeX _ -> 0
  CostComponent.PutPlusOneCountersOnThis _ -> 0
  CostComponent.Blight _ -> 0
  CostComponent.BlightX -> 0
  CostComponent.RemoveLoyaltyFromThisX -> 0
  CostComponent.Forage -> 0
  CostComponent.FlipCoin -> 0
  CostComponent.ChooseOpponent -> 0
  CostComponent.WaterbendX -> 0
  CostComponent.Waterbend _ -> 0
  CostComponent.WaterbendInstead _ -> 0
  CostComponent.ExileThisFromGraveyard -> 0
  CostComponent.ExileThis -> 0
  CostComponent.ExileCardsFromGraveyard {} -> 0
  CostComponent.ExileMaterials {} -> 0
  CostComponent.ExileTopFromGraveyard _ -> 0
  CostComponent.CollectEvidence _ -> 0
  CostComponent.CollectEvidenceOfTargets -> 0
  CostComponent.ExileCardFromHand _ -> 0
  CostComponent.RevealCardFromHand _ -> 0
  CostComponent.Behold _ -> 0
  CostComponent.BeholdAndExile _ -> 0
  CostComponent.MillCards _ -> 0

-- The counters a cost takes OFF the object it is on, added up per kind --
-- `lifeOwedBy`'s counter sibling, and `repeatsOf`'s counterCeiling is what
-- divides by it. Total for lifeOwedBy's reason: a new component spending these
-- counters cannot be added without answering here.
--
-- Only the REMOVING component counts. CostComponent.PutPlusOneCountersOnThis
-- moves the resource the other way, so folding it in would net two components
-- that CR 122.1 never nets -- and CR 601.2h leaves the payment's ORDER to the
-- payer, so a cost holding both has no fixed number of counters to divide.
countersOwedBy :: [CostComponent.CostComponent Keyword.Type.Keyword] -> Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural
countersOwedBy components = Map.fromListWith (+) [(kind, owed) | component <- components, (kind, owed) <- countersOwedByComponent component]

countersOwedByComponent :: CostComponent.CostComponent Keyword.Type.Keyword -> [(CounterKind.CounterKind Keyword.Type.Keyword, Natural)]
countersOwedByComponent component = case component of
  CostComponent.RemoveCountersFromThis removal -> [(CountersFromThis.kind removal, CountersFromThis.count removal)]
  -- Zero, and that is the header's "off the object it is on": this component
  -- takes its counters off ANOTHER permanent, so `counterCeiling`'s division of
  -- `oid`'s counters has nothing to learn from it.
  CostComponent.RemoveCounters {} -> []
  CostComponent.RemovePlusOneCountersX _ -> []
  CostComponent.SacrificeX _ -> []
  CostComponent.PutPlusOneCountersOnThis _ -> []
  CostComponent.PayLife _ -> []
  CostComponent.PayHalfLife _ -> []
  CostComponent.PayLifeX -> []
  CostComponent.PayEnergyX -> []
  CostComponent.TapThis -> []
  CostComponent.UntapThis -> []
  CostComponent.SacrificeThis -> []
  CostComponent.ReturnThis -> []
  CostComponent.Sacrifice {} -> []
  CostComponent.TapForTotalPower {} -> []
  CostComponent.TapPermanents {} -> []
  CostComponent.ReturnPermanents {} -> []
  CostComponent.ExilePermanents {} -> []
  CostComponent.DiscardCards {} -> []
  CostComponent.DiscardThis _ -> []
  CostComponent.PutCardFromHandOntoBattlefield _ -> []
  CostComponent.PayEnergy _ -> []
  CostComponent.AddLoyaltyToThis _ -> []
  CostComponent.RemoveLoyaltyFromThis _ -> []
  CostComponent.Blight _ -> []
  CostComponent.BlightX -> []
  CostComponent.RemoveLoyaltyFromThisX -> []
  CostComponent.Forage -> []
  CostComponent.FlipCoin -> []
  CostComponent.ChooseOpponent -> []
  CostComponent.WaterbendX -> []
  CostComponent.Waterbend _ -> []
  CostComponent.WaterbendInstead _ -> []
  CostComponent.ExileThisFromGraveyard -> []
  CostComponent.ExileThis -> []
  CostComponent.ExileCardsFromGraveyard {} -> []
  CostComponent.ExileMaterials {} -> []
  CostComponent.ExileTopFromGraveyard _ -> []
  CostComponent.CollectEvidence _ -> []
  CostComponent.CollectEvidenceOfTargets -> []
  CostComponent.ExileCardFromHand _ -> []
  CostComponent.RevealCardFromHand _ -> []
  CostComponent.Behold _ -> []
  CostComponent.BeholdAndExile _ -> []
  CostComponent.MillCards _ -> []

-- CR 118.3 for ONE component. `slots` is what CR 601.2c has bound, or would bind
-- under the announcement the caller is measuring; every criterion below is read
-- against it, so a gate and CR 601.2h's payment answer one question.
canPayComponent :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> GameState -> Bool
canPayComponent slots pid oid component gs = case component of
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
  -- activation for nothing (CR 118.3).
  CostComponent.SacrificeThis ->
    Set.member oid (GameState.battlefield gs)
      && Projection.controllerOf oid gs == Just pid
      && not (SacrificeRestriction.prohibited oid gs)
  -- CR 118.1 as a cost: only a permanent, and only one this player controls --
  -- SacrificeThis' two conjuncts above, read here rather than left to the funnel
  -- for that arm's reason.
  --
  -- WITHOUT CR 101.2's prohibition, deliberately: an effect saying a permanent
  -- can't be sacrificed says nothing about returning it to its owner's hand, and
  -- reading that guard here would refuse a cost the rules allow. UNPROVEN --
  -- data/cards/garland-royal-kidnapper.json is the pool's one producer of a
  -- sacrifice restriction and it reaches only creatures its controller does not
  -- own, so no board here can hold a prohibited Grinning Ignus. A declared
  -- reading, not a tested one.
  CostComponent.ReturnThis ->
    Set.member oid (GameState.battlefield gs)
      && Projection.controllerOf oid gs == Just pid
  -- CR 406.2 as a cost: only a permanent, and only one this player controls --
  -- ReturnThis' two conjuncts above, and WITHOUT CR 101.2's prohibition for that
  -- arm's reason, an effect forbidding a sacrifice saying nothing about an exile.
  CostComponent.ExileThis ->
    Set.member oid (GameState.battlefield gs)
      && Projection.controllerOf oid gs == Just pid
  -- CR 119.4: payable only if the life total is at least the amount. This
  -- component ALONE, which is not CR 118.3's question -- canPay hands
  -- `lifeOwedBy`'s sum to the mana side, and this can only be the weaker check.
  CostComponent.PayLife n -> Event.canPayLife pid n gs
  -- CR 119.4 over the amount `halfLifeOf` measures, which never exceeds the
  -- total it halves -- so only CR 119.8's prohibition refuses it.
  CostComponent.PayHalfLife rounding -> Event.canPayLife pid (halfLifeOf rounding pid gs) gs
  -- CR 601.2b: this is the component BEFORE X is announced, so there is no
  -- amount to measure against CR 119.4 -- CR 601.2 reverses a casting a player
  -- cannot comply with rather than choosing a value for them. Unreachable from
  -- either cast path, both of which substitute before they measure or pay; a
  -- fence, with Pawl.CostSpec's "an unannounced X is unpayable" as the test.
  CostComponent.PayLifeX -> False
  -- CR 601.2b again: unpayable until the activating player announces a value,
  -- PayLifeX's arm above and for its reason. Unreachable from the activation
  -- path, which substitutes before it measures or pays.
  CostComponent.PayEnergyX -> False
  -- CR 601.2b again, PayEnergyX's arm above and for its reason.
  CostComponent.RemovePlusOneCountersX _ -> False
  CostComponent.SacrificeX _ -> False
  -- CR 701.21a: this player must control at least `n` matching permanents. This
  -- component ALONE, PayLife's caveat -- two Sacrifice components of one cost can
  -- each find the same permanent here, and `jointlyPayable` asks them together.
  CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) ->
    Natural.length (Replacement.sacrificeCandidates (Just pid) slots pid (Just oid) criterion gs) >= n
  -- CR 702.122a: payable iff SOME subset of the candidates reaches the
  -- threshold, decided without enumerating one -- the greatest total any subset
  -- can reach is the sum of the candidates' POSITIVE powers, since adding one of
  -- power 0 or less cannot raise it and the player is never obliged to. Exact
  -- rather than a bound, and `>=` because CR 702.122a says "or greater". A
  -- threshold of 0 is payable by the empty set, with no special case.
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n criterion) ->
    sum (fmap (max 0 . (`tapPower` gs)) (tapCandidates slots pid oid criterion gs)) >= toInteger n
  -- Sacrifice's arm read over tapping: the count is HOW MANY, so the question is
  -- a size and not a sum. "Untapped" is not asked here and is not missing -- CR
  -- 107.5's exclusion is not this component's, so a card that wants it prints it
  -- (Springleaf Drum's criterion carries `Not IsTapped`). This component ALONE,
  -- Sacrifice's caveat; ManaSpec's "one creature cannot pay for both Drums" is
  -- the test that `jointlyPayable` asks them together.
  --
  -- CR 205.3m where the permanents must share a creature type: payable iff n
  -- candidates hold ONE type in common, the largest such group.
  CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion sharing) ->
    let candidates = tapCandidates slots pid oid criterion gs
     in if sharing then largestSharingGroup candidates gs >= n else Natural.length candidates >= n
  -- CR 118.3: this player must have at least `n` permanents the criterion
  -- admits to return. TapPermanents' arm above over a different action, and
  -- WITHOUT CR 101.2's sacrifice prohibition for the reason ReturnThis' arm
  -- gives. `claimOf` must agree.
  --
  -- REDUNDANT with that claim on every board this pool can build, which is why
  -- Pawl.CostSpec asks this function directly: relaxing this arm alone leaves
  -- the gate green, `jointlyPayable`'s empty pool refusing the same activation.
  CostComponent.ReturnPermanents (ReturnPermanents.MkReturnPermanents n criterion) ->
    Natural.length (returnCandidates slots pid oid criterion gs) >= n
  -- CR 118.3 for the exile, the arm above's reading over the same pool.
  CostComponent.ExilePermanents (ExilePermanents.MkExilePermanents n criterion) ->
    Natural.length (returnCandidates slots pid oid criterion gs) >= n
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
    Natural.length (discardCandidates slots pid oid criterion gs) >= n
  -- CR 118.3: payable only if the hand holds a card the criterion admits. This
  -- is what keeps Hakbal of the Surging Soul's landless controller from being
  -- offered a cost they cannot pay -- CR 118.12's offer is gated on
  -- affordability by Resolve.payGatePaidBy, so an empty pool takes the "doesn't"
  -- branch with no prompt at all.
  CostComponent.PutCardFromHandOntoBattlefield criterion ->
    not (null (putOntoBattlefieldCandidates slots pid oid criterion gs))
  -- CR 118.3: payable only if the hand holds a card the criterion admits, the
  -- arm above's reading. `claimOf` must agree.
  --
  -- REDUNDANT with that claim on every board Cadaverous Bloom can build, the
  -- ReturnPermanents arm's note above: relaxing this arm to True leaves
  -- Pawl.CostSpec's Cadaverous Bloom cases green, the empty claim pool refusing
  -- the same activation. A FENCE, not proven behaviour.
  CostComponent.ExileCardFromHand criterion ->
    not (null (exileFromHandCandidates slots pid oid criterion gs))
  -- CR 118.3: payable only if the hand holds a card the criterion admits, the arm
  -- above's reading. What it decides is the OFFER: `payComponent` below answers
  -- Unpaid on an empty pool anyway, so a caster with no creature card is refused
  -- either way -- Pawl.CostSpec's "CR 118.3 the cast is not offered at all" is the
  -- assertion this arm alone reddens, and the two beneath it hold without it.
  CostComponent.RevealCardFromHand criterion ->
    not (null (revealFromHandCandidates slots pid oid criterion gs))
  -- CR 118.3: payable only if the payer's hand and battlefield together hold at
  -- least `n` objects the criterion admits, RevealCardFromHand's arm above over
  -- CR 701.4a's two zones at once. What it decides is the OFFER;
  -- `payComponent` answers Unpaid on too small a pool either way. Pawl.CostSpec's
  -- "CR 118.3 two Elementals cannot behold three" is the proof.
  CostComponent.Behold (Behold.MkBehold n criterion) ->
    Natural.length (beholdCandidates slots pid oid criterion gs) >= n
  -- The arm above at a count of one: CR 118.3 asks the same pool, the exile
  -- spending what it beheld.
  CostComponent.BeholdAndExile criterion ->
    not (null (beholdCandidates slots pid oid criterion gs))
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
    Natural.length (exileCandidates slots pid oid criterion gs) >= n
  -- CR 118.3: the arm above over CR 702.167a's two pools at once, so that a craft
  -- ability is not OFFERED where the battlefield and the graveyard together hold
  -- too few materials. ">=" answers both readings of the count: an exact one is
  -- payable at exactly that many and a minimum at that many or more, so rule
  -- 702.167a's "one or more" moves the CEILING and never the floor.
  CostComponent.ExileMaterials (ExileMaterials.MkExileMaterials n _ criterion) ->
    Natural.length (materialCandidates slots pid oid criterion gs) >= n
  -- CR 701.59b: a player who cannot reach the total can't collect evidence at all.
  -- The arm above read as a SUM rather than a size, TapForTotalPower's question one
  -- zone over -- and EXACT rather than a bound, no card having a negative mana value
  -- (CR 202.3), so the whole graveyard is the largest total any subset reaches.
  -- ">=" because rule 701.59a says "or greater", and a threshold of 0 is paid by the
  -- empty set with no special case.
  CostComponent.CollectEvidence n ->
    sum (fmap (`evidenceValue` gs) (evidenceCandidates slots pid oid gs)) >= toInteger n
  -- The arm above at the amount these slots fix (fixComputed).
  CostComponent.CollectEvidenceOfTargets -> canPayComponent slots pid oid (fixComputed slots gs component) gs
  -- CR 118.3 again: payable only if the graveyard holds a matching card at all,
  -- since the top one is then determined.
  CostComponent.ExileTopFromGraveyard criterion ->
    Maybe.isJust (topExileCandidate slots pid oid criterion gs)
  -- CR 107.14 / CR 118.3: payable only if the player has at least that many
  -- energy counters.
  CostComponent.PayEnergy n -> Game.energyOf pid gs >= n
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
  -- CR 118.3: payable only while the permanent still carries at least that many
  -- counters of the kind -- "a player can't pay a cost without having the necessary
  -- resources to pay it fully". CR 606.6 is the loyalty-only analogue and is NOT
  -- the rule here. ">=" for that arm's reason: a removal of exactly what is there
  -- is payable and leaves none.
  --
  -- Deliberately NOT gated on control, unlike the loyalty arms above and like
  -- PutPlusOneCountersOnThis below: CR 122 qualifies a counter by the object it
  -- sits on and by nothing else, and CR 602.1a already fixes the payer as the
  -- player activating the ability. Mana Cache pairs this component with an
  -- Activator.AnyPlayer clause, and Pawl.ManaSpec's Mana Cache group is what
  -- proves an opponent's activation spends its counter.
  CostComponent.RemoveCountersFromThis removal ->
    Set.member oid (GameState.battlefield gs)
      && countersOn (CountersFromThis.kind removal) oid gs >= CountersFromThis.count removal
  -- CR 118.3 again, asked of the payer's BOARD rather than of `oid`: payable
  -- only while some permanent the criterion admits carries at least that many
  -- +1\/+1 counters. What it decides is the OFFER -- an activated ability whose
  -- cost includes one is never offered where no such permanent exists (CR
  -- 601.2h's "unpayable costs can't be paid", reaching an activation through CR
  -- 602.2b).
  --
  -- The count and the criterion are read TOGETHER, in
  -- `counterRemovalCandidates`: a board of creatures each carrying one counter
  -- cannot pay a cost that removes two, and asking the two questions separately
  -- would offer it.
  --
  -- Spread FROM AMONG several permanents, the count is read against the
  -- counters the admitted permanents carry BETWEEN them: two creatures with one
  -- counter each pay Novijen Sages' two.
  --
  -- This component ALONE, Sacrifice's caveat -- but `claimOf` states no claim
  -- for it, CR 122.1's counter being a marker, so `jointlyPayable` has nothing
  -- to add and two such components of one cost can each see the same permanent.
  CostComponent.RemoveCounters (CountersFromPermanents.MkCountersFromPermanents n which criterion spread) -> case spread of
    CounterSpread.FromOne -> not (null (counterRemovalCandidates slots pid oid n which criterion gs))
    CounterSpread.FromAmong -> sum (spreadRemovalCandidates slots pid oid which criterion gs) >= n
    CounterSpread.FromAmongAtLeast -> sum (spreadRemovalCandidates slots pid oid which criterion gs) >= n
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
  -- CR 608.2d: a player who can neither exile three cards from their graveyard
  -- nor sacrifice a Food can't choose either half, which is how a forage COST is
  -- unpayable rather than a no-op -- CR 601.2h's "unpayable costs can't be paid",
  -- reaching an activation through CR 602.2b, so the ability is never offered.
  -- Nothing about `oid`: rule 701.61a's candidates are qualified by the FORAGER's
  -- own graveyard and control, the Blight arm above's shape.
  CostComponent.Forage -> Forage.canForage pid gs
  -- Always payable: CR 705.1's coin is not one of CR 118.3's resources, so there
  -- is no board on which a player lacks it. The OUTCOME does not enter into it --
  -- a flip the payer goes on to lose pays the cost exactly as a won one does.
  CostComponent.FlipCoin -> True
  -- CR 102.2 / CR 104.2a: rule 702.174a's cost names an opponent, so a payer
  -- with none left, or none in range (CR 801.5a), cannot pay it -- the payment
  -- arm's own offer. Nothing about `oid`: the choice is about the table, not
  -- about the object the cost is on.
  CostComponent.ChooseOpponent -> not (null (Game.opponentsInReach pid gs))
  -- CR 601.2b: the component BEFORE X is announced, so there is no
  -- ceiling for rule 701.67b to scope -- BlightX's arm below, verbatim.
  -- Unreachable from the activation path, which substitutes before it
  -- measures or pays; a fence, with Pawl.CostSpec's "an unannounced
  -- waterbend X is unpayable" as the test.
  CostComponent.WaterbendX -> False
  -- Always payable: rule 701.67a's licence spends nothing of its own, and the
  -- mana it scopes is the cost's own mana part, which the mana half gates.
  CostComponent.Waterbend _ -> True
  CostComponent.WaterbendInstead _ -> True
  -- CR 701.17b's last sentence, stated of costs in as many words: "the player
  -- can't pay a cost that includes milling a number of cards greater than the
  -- number of cards in their library". Not the general "as many as possible" of
  -- rule 701.17b's second sentence, which is about an INSTRUCTION to mill.
  --
  -- This component ALONE, Sacrifice's caveat: two mills in one cost can each see
  -- the same cards here, and `jointlyPayable` asks them together.
  CostComponent.MillCards n -> Natural.length (Game.zoneMembers Zone.Library pid gs) >= n
  -- CR 601.2b: the component BEFORE X is announced, so there is no number of
  -- counters to measure rule 701.68b against -- PayLifeX's arm above, verbatim.
  -- Unreachable from either cast path, both of which substitute before they
  -- measure or pay; a fence, with Pawl.CostSpec's "an unannounced blight X is
  -- unpayable" as the test.
  CostComponent.BlightX -> False
  -- Unpayable until announced, BlightX's answer above and for its reason; a
  -- fence with no test, the activation road substituting it first.
  CostComponent.RemoveLoyaltyFromThisX -> False

-- CR 601.2c / 602.2b: what the announcement had bound by the time CR 601.2h pays
-- -- the targets, and CR 601.2b's X -- read off the stack object `pay` is handed
-- as `announced`, the one holder both Pawl.Engine.Cast and Pawl.Engine.Activate
-- stamp before paying. Nothing for a payment with no announcement behind it.
--
-- Pawl.Engine.Binding.slotObjects rather than the raw bindings, so a batch slot
-- is visible whole -- the same map every resolution-time Context carries.
announcedSlots :: Maybe ObjectId -> GameState -> Map.Map SlotName.SlotName (Set.Set ObjectId)
announcedSlots announced gs = case announced >>= \a -> Game.lookupObject a gs of
  Nothing -> Map.empty
  Just obj -> Binding.slotObjects (Object.bindings obj)

-- Does this cost's payability DEPEND on what CR 601.2c binds? A criterion that
-- names no slot answers the same against every slot map, so a gate measuring
-- such a cost may read the empty one and be exact; one that names a slot may
-- not, and its gate owes the lookahead over the announcements still open
-- (Pawl.Engine.Cast.payableCostAt, Pawl.Engine.Activatable.aimingSomewhere).
--
-- The classification is a Filter's, never a component's identity: every
-- criterion a component carries goes through Filter.boundSlots.
--
-- So does an amount the targets compute (targetComputed), priced per aiming. A
-- REGRESSION FENCE: Urgent Necropsy's slots all take "up to one", so the empty
-- aiming is always its cheapest and measuring with nothing bound answers alike.
readsBoundSlot :: Cost Keyword.Type.Keyword -> Bool
readsBoundSlot = any (\component -> targetComputed component || not (Set.null (Set.unions (fmap Filter.boundSlots (criteriaOf component))))) . Cost.components

-- CR 601.2c's targets as a payability gate can tell them apart: two targets with
-- one signature are interchangeable to every reader of the aiming, so
-- Pawl.Engine.Target.aimingsBy may try one of them in place of each. Without
-- this the lookahead is exponential in a plural slot's legal set (Clever
-- Concealment under Hinata, Dawn-Crowned).
--
-- The readers, each answered here: a cost change's per-target questions
-- (PlayerEffect.targetQuestions), the spell's own whichTargets sentences
-- (selfReductions), every claim the cost's components make with nothing bound
-- -- membership, and what the object adds towards a threshold -- and, beside a
-- targetComputed component, whether a permanent is there and its mana value
-- (fixComputed).
--
-- And what a target is to the MANA half, which CR 601.2g pays before 601.2h and
-- whose sources' own costs claim objects jointly with the components
-- (Mana.canPayCommittingGiven): a target's membership in every claim a mana
-- source makes on an axis the components also claim, and, for a target that is
-- itself such a source, its routes with itself written as "self". Blood Pet and
-- Grizzly Bears are alike to Synthetic Spiteful Rite's sacrifice but not to the
-- {B} it needs -- Pawl.CostSpec's "CR 601.2g a target the components cannot
-- tell apart may still be the mana source". Two Treasures stay alike. Only the
-- components' axes, because a target leaves no other claim's pool.
--
-- Claims read with nothing bound are what a target LEAVES when its criterion
-- says "isn't a target". A criterion reading a slot any other way could tell
-- interchangeable-looking targets apart, so the answer is Nothing and the caller
-- falls back to every subset. No cost in data/cards/ reads a slot otherwise
-- (grep of IsBound under additional and activation costs, 2026-10-08).
aimingSignature :: PlayerId -> ObjectId -> GameState -> Cost Keyword.Type.Keyword -> Maybe (Recipient.Recipient -> ([Integer], [(Activations.Activations, [(Bool, Maybe (Maybe Integer), Claim)], Mana.Type.Mana, ManaCost.ManaCost)]))
aimingSignature pid oid gs cost
  | not (all onlyExcludesTargets criteria) = Nothing
  -- One excluded slot across the whole cost: aimingsBy fixes each class's
  -- union over the slots and each slot's draw, which settles how many targets
  -- one slot excludes, but not the overlap of two excluded slots beside a third.
  | Set.size (Set.fromList (concatMap excludedSlots criteria)) > 1 || any ((> 1) . length . excludedSlots) criteria = Nothing
  | otherwise =
      let context = Filter.contextFor (Game.teams gs) (Just pid) (Just oid)
          wanted = Maybe.mapMaybe CostReduction.whichTargets (selfSentences pid oid gs)
          claims = claimsOf Map.empty pid oid (Cost.components cost) gs
          computed = any targetComputed (Cost.components cost)
          ofObject r f = maybe 0 f (Recipient.objectOf r)
          self r = fmap (\w -> ofObject r (\o -> toInteger (fromEnum (Filter.matches context (Projection.viewOfObject o gs) w)))) wanted
          claimed r = concatMap (\c -> [ofObject r (toInteger . fromEnum . (`Set.member` Claim.Type.pool c)), maybe 0 (\t -> ofObject r (\o -> Map.findWithDefault 0 o (Threshold.amounts t))) (Claim.Type.threshold c)]) claims
          evidence r = if computed then [ofObject r (\o -> toInteger (fromEnum (Game.zoneOf o gs == Just Zone.Battlefield))), ofObject r (`evidenceValue` gs)] else []
          axes = Set.fromList (fmap Claim.Type.axis claims)
          capacity = Mana.supplyCapacity (midPayment (manaActivationsGiven (PlayerEffect.applying pid gs)))
          pcs = Projection.projectAll gs
          supplies =
            if Set.null axes
              then []
              else [(source, supply) | source <- Mana.manaSourcesGiven Set.empty capacity (Projection.controlGrants gs) pcs pid gs, supply <- Mana.manaSuppliesGiven capacity pcs pid source gs]
          -- The yield's OWN claims, narrowed per sacrifice candidate where the
          -- supply prices one (Mana.manaSuppliesGiven's fourth element).
          relevant (_, _, _, own) = filter ((`Set.member` axes) . Claim.Type.axis) (Activations.claims own)
          -- Is the target in each source's claim pool, one entry per claim for
          -- every target alike, so the lists line up by source. A pool of the
          -- source alone is left out: only that source is in it, and its routes
          -- say so, so two Treasures stay alike. A REGRESSION FENCE: no test
          -- puts a source between two self-only sources' ObjectIds.
          pooled r = [ofObject r (\o -> toInteger (fromEnum (Set.member o (Claim.Type.pool c)))) | (source, supply) <- supplies, c <- relevant supply, Claim.Type.pool c /= Set.singleton source]
          -- The target's own routes, itself written out of every pool.
          selfless o c =
            ( Set.member o (Claim.Type.pool c),
              fmap (Map.lookup o . Threshold.amounts) (Claim.Type.threshold c),
              c {Claim.Type.pool = Set.delete o (Claim.Type.pool c), Claim.Type.threshold = fmap (\t -> t {Threshold.amounts = Map.delete o (Threshold.amounts t)}) (Claim.Type.threshold c)}
            )
          routes r = case Recipient.objectOf r of
            Nothing -> []
            Just o -> [(activations {Activations.claims = []}, fmap (selfless o) (relevant supply), mana, cost') | (source, supply@(_, mana, cost', activations)) <- supplies, source == o, not (null (relevant supply))]
       in Just (\r -> (fmap (toInteger . fromEnum) (PlayerEffect.targetQuestions pid gs r) <> self r <> claimed r <> evidence r <> pooled r, routes r))
  where
    criteria = concatMap criteriaOf (Cost.components cost)

-- aimingSignature as the key Pawl.Engine.Target.aimingsBy classes targets by:
-- the target itself where no signature can be given, so the search falls back
-- to every subset.
aimingKey :: PlayerId -> ObjectId -> GameState -> Cost Keyword.Type.Keyword -> Recipient.Recipient -> Either Recipient.Recipient ([Integer], [(Activations.Activations, [(Bool, Maybe (Maybe Integer), Claim)], Mana.Type.Mana, ManaCost.ManaCost)])
aimingKey pid oid gs cost = maybe Left (Right .) (aimingSignature pid oid gs cost)

-- What a recipient IS, for aimingsBy: a creature, a planeswalker and a permanent
-- named generically are one object, so one object answering two slots under two
-- tags counts once, as PlayerEffect.perTargetCount counts it.
aimedReferent :: Recipient.Recipient -> Recipient.Recipient
aimedReferent r = maybe r Recipient.ToObject (Recipient.objectOf r)

-- The slots a criterion excludes with a top-level Not (IsBound _), or every
-- target at once with Not IsTarget -- Nothing, which aimingsBy's per-class
-- union over the slots settles exactly.
excludedSlots :: Filter.Type.Filter Keyword.Type.Keyword -> [Maybe SlotName.SlotName]
excludedSlots criterion =
  let excluded f = case f of
        Filter.Type.Not (Filter.Type.IsBound name) -> [Just name]
        Filter.Type.Not Filter.Type.IsTarget -> [Nothing]
        _ -> []
   in case criterion of
        Filter.Type.And conjuncts -> concatMap excluded conjuncts
        _ -> excluded criterion

-- Does this criterion read a slot only as "isn't a target" -- a top-level
-- Not (IsBound _) or Not IsTarget, alone or as a conjunct beside slotless ones? aimingSignature's
-- condition.
onlyExcludesTargets :: Filter.Type.Filter Keyword.Type.Keyword -> Bool
onlyExcludesTargets criterion =
  let excludes f = case f of
        Filter.Type.Not (Filter.Type.IsBound _) -> True
        Filter.Type.Not Filter.Type.IsTarget -> True
        _ -> False
      rest = case criterion of
        Filter.Type.And conjuncts -> filter (not . excludes) conjuncts
        _ -> [criterion | not (excludes criterion)]
   in all (Set.null . Filter.boundSlots) rest

-- Every criterion a cost component carries, as the Filters a slot name could
-- hide in. EXHAUSTIVE with no wildcard, claimOf's posture: a new component with
-- a criterion has to answer here or the gate above stops seeing it.
criteriaOf :: CostComponent.CostComponent Keyword.Type.Keyword -> [Filter.Type.Filter Keyword.Type.Keyword]
criteriaOf component = case component of
  CostComponent.Sacrifice sacrifice -> [Sacrifice.whichPermanents sacrifice]
  CostComponent.TapForTotalPower tap -> [TapForTotalPower.whichPermanents tap]
  CostComponent.TapPermanents tap -> [TapPermanents.whichPermanents tap]
  CostComponent.ReturnPermanents ret -> [ReturnPermanents.whichPermanents ret]
  CostComponent.ExilePermanents exiled -> [ExilePermanents.whichPermanents exiled]
  CostComponent.DiscardCards discard -> [DiscardCards.whichCards discard]
  CostComponent.PutCardFromHandOntoBattlefield criterion -> [criterion]
  CostComponent.ExileCardFromHand criterion -> [criterion]
  CostComponent.RevealCardFromHand criterion -> [criterion]
  CostComponent.Behold behold -> [Behold.whichObjects behold]
  CostComponent.BeholdAndExile criterion -> [criterion]
  CostComponent.ExileCardsFromGraveyard exile -> [ExileCardsFromGraveyard.whichCards exile]
  CostComponent.ExileMaterials materials -> [ExileMaterials.whichObjects materials]
  CostComponent.ExileTopFromGraveyard criterion -> [criterion]
  CostComponent.RemoveCounters remove -> [CountersFromPermanents.whichPermanent remove]
  CostComponent.RemovePlusOneCountersX criterion -> [criterion]
  CostComponent.SacrificeX criterion -> [criterion]
  -- No criterion: rule 701.59a describes the cards by a TOTAL and by nothing else,
  -- so this belongs with the amount-carrying arms below.
  CostComponent.CollectEvidence _ -> []
  CostComponent.CollectEvidenceOfTargets -> []
  -- The rest carry no criterion at all: each names either the source object or
  -- a bare amount.
  CostComponent.TapThis -> []
  CostComponent.UntapThis -> []
  CostComponent.SacrificeThis -> []
  CostComponent.ReturnThis -> []
  CostComponent.PayLife _ -> []
  CostComponent.PayHalfLife _ -> []
  CostComponent.PayLifeX -> []
  CostComponent.PayEnergyX -> []
  CostComponent.DiscardThis _ -> []
  CostComponent.PayEnergy _ -> []
  CostComponent.AddLoyaltyToThis _ -> []
  CostComponent.RemoveLoyaltyFromThis _ -> []
  CostComponent.RemoveCountersFromThis _ -> []
  CostComponent.PutPlusOneCountersOnThis _ -> []
  CostComponent.Blight _ -> []
  CostComponent.BlightX -> []
  CostComponent.RemoveLoyaltyFromThisX -> []
  -- Rule 701.61a's two candidate sets are the rulebook's own and carry no card
  -- Filter, so there is no criterion for the lint to sweep.
  CostComponent.Forage -> []
  CostComponent.FlipCoin -> []
  CostComponent.ChooseOpponent -> []
  CostComponent.WaterbendX -> []
  -- The criterion the licence leads to is MINTED by `waterbendSubstitute` from
  -- rule 701.67a's own words rather than printed, `manaSubstitutesFor`'s posture:
  -- no word of a card's is in it, so CR 612.2 has nothing to swap.
  CostComponent.Waterbend _ -> []
  CostComponent.WaterbendInstead _ -> []
  CostComponent.ExileThisFromGraveyard -> []
  CostComponent.ExileThis -> []
  CostComponent.MillCards _ -> []

-- CR 733.1: put back what an action the player could not legally complete did.
--
-- `before` is where the failed ACTION began, which is the caller's to name: a
-- cast's is ahead of CR 601.2a's move, an activation's ahead of CR 602.2a's
-- reveal, a combat declaration's ahead of CR 508.1a's or 509.1a's record. No
-- caller unwinds a wider snapshot afterwards, so nothing discards what a payer
-- kept. Everything the rule reverses unconditionally -- the announcement, and
-- every payment made -- is undone by going back to it.
--
-- `windows` are the CR 605.3a mana windows the action opened, oldest first,
-- each holding the states it opened and closed on, its activations -- nested
-- ones included -- and the stretches each of them wrote. A window's `closed`
-- holds its activations with nothing paid out of them yet, and it is what its
-- payer gets by keeping every one: the sources stay tapped, the mana they made
-- stays in the pool (CR 106.4), and CR 405.6c's other effects -- Ancient
-- Tomb's 2 damage -- stay done.
--
-- The CHOICE is each payer's because rule 733.1 says "each player MAY also
-- reverse ANY" of exactly these, where the rest of the sentence is flat: one
-- question per activation. Most actions open one window; a cast with CR
-- 702.132a's assist opens the chosen player's ahead of the caster's
-- (`offerAssist`), so two players can each be asked. They answer in APNAP
-- order (CR 101.4), each against the state the cancellation and the earlier
-- answers leave, with everything not yet answered still standing -- so a later
-- answer sees what an earlier one chose (CR 101.4b).
--
-- One payer's activations are asked NEWEST FIRST, in the order they finished,
-- which is what makes the rule's "unless" clause a question about answers
-- already given: mana is only ever spent on an activation that finished later,
-- so whether reversing one leaves its mana spent on another that was kept is
-- settled once every later one is answered. Such an activation is not asked,
-- since the rule leaves nothing to ask. `spendable` is that check: replayed in
-- the order they finished, each kept activation must still be able to take
-- what it spent out of the pool the window opened on and what the kept ones
-- before it added. A unit has no identity beyond its fields
-- (Pawl.Types.ManaUnit), so where two equal units could have paid, either is
-- taken to have. CostSpec's Skyshroud Elf and Mystic Gate cases are the proof.
--
-- Where each kept stretch OPENED is what tells the sides apart: everything
-- between one kept stretch's close (or `before`) and the next one's opening is
-- reversed, and everything a kept stretch wrote stays. `composeReversal` builds
-- the state for a set of answers out of
-- Pawl.Engine.Reversal.withoutAnnouncement, which answers Nothing where two
-- sides wrote one leaf irreconcilably. An activation is offered only where
-- reversing it composes, with every one not yet answered kept, so the final
-- state is always one already composed: the one the last "reverse" answer was
-- checked against.
--
-- Not implemented: asking where a set of answers does not compose. With
-- everything kept the whole action goes back unasked, a combat toll's mill
-- (`unreversibleStretch`) with it; otherwise the activation stands unasked
-- (#4860).
--
-- The special actions and CR 118.12's payment announce nothing that writes:
-- their two states differ only in GameState.lastChoice and, at
-- Pawl.Engine.Companion.take, in GameState.nextObjectId, both of which ride at
-- `closed`'s value by rule, so the composed state IS `closed` there.
--
-- CostSpec's "Reversal" group proves it at CR 118.12's payment,
-- Pawl.FaceDownSpec's "Reversal at a special action" group at a special
-- action's, CostSpec's "Reversal after an announcement" group at a cast, an
-- activation and a mana ability's own cost, CombatCostSpec's "Reversal at a
-- combat toll" group at CR 508.1's and CR 509.1's declarations, CostSpec's
-- "Charging Binox" group at an assisted cast's two windows, and CostSpec's
-- "Reversing some mana abilities" group at a subset, a nested activation and
-- the "unless" clause.
--
-- A nested window's kept activations come back to the enclosing window when
-- the activation they paid for failed, each re-based onto the states keeping
-- only it and the ones before it, so the enclosing reversal offers each again
-- on its own; the scenario
-- cost/cr-733-1-what-a-nested-window-kept-is-offered-again-one-activation-at-a-time
-- is the proof.
--
-- Not implemented: telling them apart where such a state does not compose;
-- they come back as one (#4860).
--
-- The CANCELLATION HAPPENS FIRST, before the question: rule 733.1 reverses the
-- action and cancels the payments flat, and only its last-but-one sentence
-- offers the mana abilities back. So the payers are asked against the state
-- that cancellation leaves -- the announcement undone and nothing of the cost
-- paid -- rather than against the half-paid state the refusal left, which is
-- what Pawl.Engine.Game.ask hands the answerer. It also keeps CR 104.4b's
-- stamp: `Game.choose` writes GameState.lastChoice, and each state an answer
-- leaves is put with the live stamp rather than the one it was composed with,
-- and with the live GameState.nextTimestamp the stamp was drawn from.
--
-- Every restore goes through `keepingLibraryActions` rather than a bare
-- State.put, CR 733.1's last sentence's reason: a shuffle or a reveal one of the
-- reversed abilities performed stands even though the rest of it goes back. The
-- no-activation arm reads the LIVE state rather than a window's `closed`, which
-- is what the callers it took this restore over from did.
--
-- The answer is the activations that STAND, empty unless a payer kept them: a
-- nested window's caller (`tapForManaWith`) hands them to the window it is
-- nested in, whose own reversal then offers them too.
reverseIllegal :: [ManaWindow.ManaWindow] -> GameState -> Game ([ManaActivation.ManaActivation], [ManaSegment.ManaSegment])
reverseIllegal windows before =
  let indexed = [((w, i), window, activation) | (w, window) <- zip [0 :: Int ..] windows, (i, activation) <- zip [0 :: Int ..] (ManaWindow.activated window)]
      segmentsOf = Map.fromListWith (flip (<>)) [((w, ManaSegment.activation segment), [segment]) | (w, window) <- zip [0 :: Int ..] windows, segment <- ManaWindow.segments window]
      -- CR 733.1: "players may not reverse actions that moved cards to a
      -- library [or] from a library to any zone other than the stack". An
      -- activation that did -- a CR 605.1b triggered mana ability that draws
      -- (Synthetic Wellspring Growth) -- stands unasked, and so does a combat
      -- toll's mill (`unreversibleStretch`).
      movedLibraryCard key = any (\segment -> libraryMembershipChanged (ManaSegment.opened segment) (ManaSegment.closed segment)) (Map.findWithDefault [] key segmentsOf)
      askers = [entry | entry@(key, _, _) <- indexed, not (movedLibraryCard key)]
      compose reversed = composeReversal before windows (\w i -> Set.notMember (w, i) reversed)
      -- CR 733.1's "unless", per payer, replayed from the pool their first
      -- window opened on.
      spendable reversed =
        all
          ( \window ->
              let pid = ManaWindow.payer window
                  kept = [activation | (key, owner, activation) <- indexed, ManaWindow.payer owner == pid, Set.notMember key reversed]
                  step held activation = fmap (<> ManaActivation.added activation) (Monad.foldM takeUnit held (ManaActivation.spent activation))
               in Maybe.isJust (Monad.foldM step (Mana.Type.unwrap (Game.poolOf pid (ManaWindow.opened window))) kept)
          )
          (ListUtils.nubOrdOn ManaWindow.payer windows)
      takeUnit held unit = case break (== unit) held of
        (above, _ : below) -> Just (above <> below)
        (_, []) -> Nothing
   in case (indexed, compose Set.empty) of
        ([], _) -> noActivations <$ restoreKeepingLibraryActions before
        (_, Nothing) -> noActivations <$ restoreKeepingLibraryActions before
        (_, Just _) -> do
          -- The state the answers given so far leave, everything not yet
          -- answered still standing, put with the live CR 104.4b stamp so that
          -- no answer's is discarded. The stamp was drawn from the live
          -- GameState.nextTimestamp, so that supply comes across with it: a
          -- state that goes back past the announcement would otherwise hold a
          -- stamp ahead of its own supply, which
          -- Pawl.Engine.Engine.checkMandatoryLoop's gap underflows on. The
          -- scenario cost/cr-733-1-an-underpaid-curtain-of-light-reverses-its-plains
          -- is the proof.
          let settle reversed =
                Monad.forM_
                  (compose reversed)
                  ( \composedState ->
                      State.modify'
                        ( \live ->
                            composedState
                              { GameState.lastChoice = GameState.lastChoice live,
                                GameState.nextTimestamp = max (GameState.nextTimestamp composedState) (GameState.nextTimestamp live)
                              }
                        )
                  )
          settle Set.empty
          cancelled <- State.get
          let rank (_, window, _) = List.elemIndex (ManaWindow.payer window) (Game.apnapOrder cancelled)
          reversed <-
            Monad.foldM
              ( \given (key, window, activation) ->
                  let trying = Set.insert key given
                   in if not (spendable trying) || Maybe.isNothing (compose trying)
                        then pure given
                        else do
                          -- CR 101.4b: the board already shows the earlier answers.
                          settle given
                          current <- State.get
                          answer <- Game.choose (Prompt.ReverseManaAbilities (Decide.deciderFor (ManaWindow.payer window) current) (ManaWindow.payer window) (ManaActivation.sources activation))
                          pure $ case answer of
                            OptionalDecision.Exercises -> trying
                            OptionalDecision.Declines -> given
              )
              Set.empty
              (List.sortOn rank (reverse askers))
          settle reversed
          live <- State.get
          -- What stands, each re-based onto the states keeping only it and
          -- the ones that finished before it, so that the window this one is
          -- nested in can offer each back on its own; the last closes on the
          -- live state, the enclosing window's next activation opening on it.
          let standing = [(key, activation) | (key, _, activation) <- indexed, Set.notMember key reversed]
              keepingFirst k = compose (Set.union reversed (Set.fromList (fmap fst (drop k standing))))
              rebased i activation opened closed = ([activation], [ManaSegment.MkManaSegment {ManaSegment.activation = i, ManaSegment.opened = opened, ManaSegment.closed = closed}])
          pure $ case NonEmpty.nonEmpty standing of
            Nothing -> noActivations
            Just stood -> case traverse keepingFirst [0 .. length standing - 1] of
              Just states -> mconcat (List.zipWith4 rebased [0 ..] (fmap snd standing) states (drop 1 states <> [live]))
              Nothing ->
                let activations = fmap snd stood
                 in rebased
                      0
                      ManaActivation.MkManaActivation
                        { ManaActivation.sources = Semigroup.sconcat (fmap ManaActivation.sources activations),
                          ManaActivation.spent = foldMap ManaActivation.spent activations,
                          ManaActivation.added = foldMap ManaActivation.added activations
                        }
                      (Maybe.fromMaybe before (keepingFirst 0))
                      live

-- No activations, and so no stretches.
noActivations :: ([ManaActivation.ManaActivation], [ManaSegment.ManaSegment])
noActivations = ([], [])

-- `later`'s activations after `earlier`'s, its stretches renumbered to match.
spliceActivations :: ([ManaActivation.ManaActivation], [ManaSegment.ManaSegment]) -> ([ManaActivation.ManaActivation], [ManaSegment.ManaSegment]) -> ([ManaActivation.ManaActivation], [ManaSegment.ManaSegment])
spliceActivations (earlier, earlierSegments) (later, laterSegments) =
  let shift segment = segment {ManaSegment.activation = ManaSegment.activation segment + length earlier}
   in (earlier <> later, earlierSegments <> fmap shift laterSegments)

-- The activation of `oid` that ran from `start` to `end` and paid, after the
-- nested activations that paid for it: it owns the stretches either side of
-- theirs, and what it ADDED to `pid`'s pool is what those stretches changed
-- there plus what its cost took out.
withParent :: PlayerId -> ObjectId -> GameState -> GameState -> ([ManaActivation.ManaActivation], [ManaSegment.ManaSegment]) -> [ManaUnit.ManaUnit] -> ([ManaActivation.ManaActivation], [ManaSegment.ManaSegment])
withParent pid oid start end (children, childSegments) spentUnits =
  let parent = length children
      own = zipWith (\opened closed -> ManaSegment.MkManaSegment {ManaSegment.activation = parent, ManaSegment.opened = opened, ManaSegment.closed = closed}) (start : fmap ManaSegment.closed childSegments) (fmap ManaSegment.opened childSegments <> [end])
      counted units = Map.fromListWith (+) [(unit, 1 :: Int) | unit <- units]
      pooled gs = counted (Mana.Type.unwrap (Game.poolOf pid gs))
      change segment = Map.unionWith (+) (pooled (ManaSegment.closed segment)) (fmap negate (pooled (ManaSegment.opened segment)))
      added = concat [replicate n unit | (unit, n) <- Map.toList (Map.unionsWith (+) (counted spentUnits : fmap change own)), n > 0]
      activation = ManaActivation.MkManaActivation {ManaActivation.sources = oid NonEmpty.:| [], ManaActivation.spent = spentUnits, ManaActivation.added = added}
   in (children <> [activation], concat (zipWith (\mine theirs -> [mine, theirs]) own childSegments) <> drop (length childSegments) own)

-- One set of CR 733.1's answers composed into the state it leaves: `windows`
-- oldest first, `keeps w i` whether the payer keeps window `w`'s activation
-- `i`.
--
-- A window's kept stretches are taken in maximal runs, each from its first
-- stretch's opening to its last one's close -- from the window's own opening
-- or to its own close where the run reaches that end, so a window kept whole
-- is the one stretch it always was. The stretches that go back are the gaps
-- between one kept run's close (or `before`) and the next one's opening, which
-- hold the announcement and everything reversed in between. Each is undone
-- with Pawl.Engine.Reversal.withoutAnnouncement, NEWEST FIRST: that function
-- reads the log of its first state as a prefix of its third's, which holds
-- only while nothing earlier has been cut out. A stretch past the newest kept
-- one is dropped by going back to that one's close, keeping the library
-- actions the rest performed (`keepingLibraryActions`).
composeReversal :: GameState -> [ManaWindow.ManaWindow] -> (Int -> Int -> Bool) -> Maybe GameState
composeReversal before windows keeps = case NonEmpty.nonEmpty windows of
  Nothing -> Just before
  Just ws ->
    let final = ManaWindow.closed (NonEmpty.last ws)
        newestWindow = length windows - 1
        -- Each run as its opening, its close, and whether it is the newest
        -- window's and reaches that window's close.
        stretches w window =
          let segments = zip [0 :: Int ..] (ManaWindow.segments window)
              lastIndex = length segments - 1
              kept = keeps w . ManaSegment.activation . snd
              runs = filter (kept . NonEmpty.head) (NonEmpty.groupWith kept segments)
              stretch run =
                let (first, firstSegment) = NonEmpty.head run
                    (end, endSegment) = NonEmpty.last run
                 in ( if first == 0 then ManaWindow.opened window else ManaSegment.opened firstSegment,
                      if end == lastIndex then ManaWindow.closed window else ManaSegment.closed endSegment,
                      w == newestWindow && end == lastIndex
                    )
           in fmap stretch runs
        keptRuns = concat (zipWith stretches [0 :: Int ..] windows)
     in case reverse keptRuns of
          [] -> Just (keepingLibraryActions final before)
          (_, newest, reachesFinal) : _ ->
            let tailState = if reachesFinal then newest else keepingLibraryActions final newest
                starts = before : fmap (\(_, closed, _) -> closed) keptRuns
             in foldr (\(start, (opened, _, _)) rest -> rest >>= Reversal.withoutAnnouncement start opened) (Just tailState) (zip starts keptRuns)

-- CR 733.1's last sentence: shuffling a library or revealing cards from one is
-- never reversed, even where the mana ability that did it is. Restoring
-- `snapshot` with those two things taken from `since` (the state as the mana
-- window closed, still carrying them) keeps every OTHER write reversed while
-- these two stand.
--
-- A library takes `since`'s ORDER, with whatever `snapshot` held that `since`
-- lacks put back at the index it held (Pawl.Engine.Reversal.restoredOrder). The
-- card that goes back is one the reversed action took, and CR 733.1 reverses a
-- move from a library to the stack: a spell cast from a library (Panglacial
-- Wurm) returns, and a shuffle a mana ability performed beside it stands.
-- Pawl.CastSpec's "a refused library cast puts Panglacial back" is the proof
-- that the card comes back; ReversalSpec's `restoredOrder` case that the shuffle
-- stands. A MillCards cost never reaches this: CR 601.2h pays it in a second pass
-- that nothing can refuse after (`pay`), and a combat toll whose LATER tag
-- refuses keeps it by composition instead (`unreversibleStretch`).

-- `events` keeps only the Revealed entries `since` gained past `snapshot`'s own
-- length whose card is in a library on BOTH sides, not the whole suffix: a tap
-- or a mana addition the window performed is exactly what the surrounding
-- reversal DOES undo, and a card revealed from a hand (CR 602.2a's activation
-- reveal, logged inside Pawl.Engine.Activate's announcement) is not what the
-- rule's "revealed from a library" protects. `nextEventGroup` rides at
-- `since`'s value, since a carried-over entry may already hold a group
-- `snapshot`'s own counter would otherwise repeat -- GameState.events' groups
-- are non-decreasing along the log.
keepingLibraryActions :: GameState -> GameState -> GameState
keepingLibraryActions since snapshot =
  let keep pid held = case Map.lookup pid (GameState.library since) of
        Just reordered -> Seq.fromList (Reversal.restoredOrder (Foldable.toList held) (Foldable.toList reordered))
        Nothing -> held
      inLibraryOf gs oid = any (Foldable.elem oid) (GameState.library gs)
      revealedFromLibrary logged = case LoggedEvent.event logged of
        GameEvent.Revealed revealed -> inLibraryOf snapshot (Revealed.card revealed) && inLibraryOf since (Revealed.card revealed)
        _ -> False
   in snapshot
        { GameState.library = Map.mapWithKey keep (GameState.library snapshot),
          GameState.events = GameState.events snapshot <> Seq.filter revealedFromLibrary (Seq.drop (Seq.length (GameState.events snapshot)) (GameState.events since)),
          GameState.nextEventGroup = GameState.nextEventGroup since
        }

-- The restore every caller that reverts a failed payment to its own snapshot
-- performs: `before` with what CR 733.1's last sentence keeps standing, read off
-- the live state. One body so a new caller cannot regress to a bare State.put.
restoreKeepingLibraryActions :: GameState -> Game ()
restoreKeepingLibraryActions before = do
  gs <- State.get
  State.put (keepingLibraryActions gs before)

-- CR 702.51b / 702.66b / 702.126b / 701.67a: the payer picks which of
-- `substituting`'s entries this cast or activation takes -- after CR 601.2f
-- locked the total cost in, which is where every one of those rules says the
-- substitution applies, and before CR 601.2h's payment. WHICH offer is the
-- caller's: a cast passes `manaSubstitutions` and every other payment
-- `waterbendSubstitutions`.
--
-- FILTERED, NOT TRUSTED. CR 118.3 is the filter: an entry whose taps or exiles
-- the board cannot satisfy together is never offered. An unrecognised answer reads as the
-- FIRST entry, which both offers make the substitute-nothing one -- a
-- fallback must not tap a creature the payer did not offer up.
--
-- ELIDED where one entry is left, which is every cast by a spell with none of
-- the keywords, every activation of a cost stating no waterbend, and every board
-- with no eligible object: rule 702.51a's and rule 701.67a's "you may" then have
-- nothing to ask.
--
-- RUN INSIDE THE PAYMENT, once CR 601.2g's window has closed and before a symbol
-- is spent -- the order the reminder text describes ("each artifact you tap
-- after you're done activating mana abilities"). Pawl.Engine.Cost.paySubstituting
-- is what holds it there; Pawl.CostSpec's "CR 601.2g the mana window opens
-- before the payer says how much of a Siege Wurm's cost is convoked" is the
-- proof.
--
-- The answer is the residual cost and the SUBSTITUTES APART, rather than one
-- cost with their components folded in: CR 702.51c's record is of the
-- creatures tapped THIS way, and Binding.tappedPermanent names every permanent
-- any tap component of the cost took (`paySubstituting`).
announceSubstitutions :: ([CostComponent.CostComponent Keyword.Type.Keyword] -> Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> GameState -> ManaCost.ManaCost -> [(ManaCost.ManaCost, [(Keyword.Substitute, Natural)])]) -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game (Cost Keyword.Type.Keyword, [(Keyword.Substitute, Natural)])
announceSubstitutions substituting pid oid cost = do
  slots <- State.gets (announcedSlots (Just oid))
  announceSubstitutionsReading slots substituting pid oid cost

-- `announceSubstitutions` with the slot map its component criteria read handed
-- in (`payReading`).
announceSubstitutionsReading :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> ([CostComponent.CostComponent Keyword.Type.Keyword] -> Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> GameState -> ManaCost.ManaCost -> [(ManaCost.ManaCost, [(Keyword.Substitute, Natural)])]) -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game (Cost Keyword.Type.Keyword, [(Keyword.Substitute, Natural)])
announceSubstitutionsReading slots substituting pid oid cost = case Cost.mana cost of
  -- CR 118.6: an unpayable cost states no symbol to substitute for.
  Nothing -> pure (cost, [])
  Just manaCost -> do
    gs <- State.get
    let variant (residual, extra) = (cost {Cost.mana = Just residual}, extra)
        whole (candidate, extra) = candidate {Cost.components = Cost.components candidate <> substituteComponents extra}
        offered = filter (\candidate -> componentsPayable slots pid oid (Cost.components (whole candidate)) gs) (fmap variant (substituting (Cost.components cost) slots pid oid gs manaCost))
    case offered of
      -- REACHABLE, and not by the gate's measurement: this runs after CR 601.2g's
      -- window, so a mana ability may have tapped the very permanent the cost's
      -- own component needed and left even the substitute-nothing entry
      -- unpayable. Answered with the cost unchanged rather than with an error,
      -- because the offer is not the place that reports an unpayable cost -- CR
      -- 601.2h's payment is, and it reverses the cast.
      [] -> pure (cost, [])
      [only] -> pure only
      first : rest -> do
        chosen <- Game.choose (Prompt.ChooseCost (Decide.deciderFor pid gs) pid oid (fmap whole (first : rest)))
        pure (Maybe.fromMaybe first (List.find ((== chosen) . whole) offered))

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
-- All or nothing (CR 601.2h). An Unpaid result unwinds the whole ACTION even
-- though paying is monadic, and the caller does nothing more. WHAT goes back is
-- rule 733.1's partition and not the whole state: the announcement and the
-- payments go unasked, and the mana abilities activated in the CR 605.3a window
-- go back only if the payer says so (`reverseIllegal` above).
--
-- Rule 733.1's MOVE limb -- an action that moved cards to or from a library may
-- NOT be reversed -- reaches the window through CR 605.1b alone: CR 605.1a
-- disqualifies an ACTIVATED ability whose cost or effect moves a card to or from
-- a library (Pawl.Engine.ManaAbility.costMovesLibraryCard is the cost half), but
-- a triggered mana ability may draw, and `reverseIllegal` leaves a window that
-- moved one standing. The components can -- MillCards is the one that
-- does -- but CR 601.2h pays those in a SECOND pass (paidInSecondPass below),
-- and `payComponent` refuses a MillCards only short of cards, which the gate
-- measured and the window cannot change, so no failure can follow one within
-- this payment.
--
-- Rule 733.1's SHUFFLE and REVEAL clauses, which CR 605.1a does NOT exclude --
-- Pawl.Engine.ManaAbility.movesLibraryCard answers False of Effect.Shuffle and
-- Effect.Reveal, so a mana ability may carry one -- are why `reverseIllegal`
-- above restores through `keepingLibraryActions` rather than a bare State.put:
-- CostSpec's "Reversal" group proves the library stands at CR 118.12's moment,
-- and its "Synthetic Reversal Rig" group at an activation.
--
-- `began` is WHERE the failed action began -- the caller's snapshot ahead of its
-- own announcement, CR 601.2a's move for a cast and CR 602.2a's reveal for an
-- activation. Everything between it and the window is what rule 733.1 reverses
-- unasked (Pawl.Engine.Reversal).
--
-- `moment` is which of CR 601.2h and CR 118.12 this payment is (see
-- Pawl.Types.PaymentMoment). Taken from the CALLER and never derived, since the
-- cost itself does not say -- `counterCause` below is what reads it, and the
-- parameter is what makes a new caller state its moment rather than inherit a
-- default.
--
-- `subject` is WHAT this payment is for (Pawl.Types.PaymentSubject): CR 601.2h's
-- spell at Pawl.Engine.Cast, CR 602.2b's ability source at Pawl.Engine.Activate,
-- the permanent being unlocked at Pawl.Engine.Room or turned face up at
-- Pawl.Engine.FaceDown, and none of those at a combat toll and CR 118.12's
-- resolution-time payment. It is not `oid` under another name -- `oid` is
-- whatever object the cost belongs to -- and CR 106.6's restrictions name one of
-- the subjects, so the two questions are different ones. Mana.spendableFor is
-- what reads it.
--
-- `announced` is the object the announcement put on the stack -- CR 601.2a's
-- spell, or CR 602.2a's ability object rather than the source `subject` carries
-- -- and nothing for every other payment. A third question again, read twice:
-- it is where CR 400.7d's record of the mana spent goes (`recordSpent` in
-- payManaExcept), and its Object.bindings are the targets CR 601.2c chose,
-- which CR 601.2h's payment comes after -- so a component's criterion can name
-- them (announcedSlots below). Both callers stamp the bindings before calling
-- this: Pawl.Engine.Cast.castProposed and Pawl.Engine.Activate.activateAbility.
--
-- `perform` is CR 405.6c's executor, carried down to the mana window for a mana
-- ability that has an effect beyond its mana (Pawl.Types.ManaAbilityPerformer).
--
-- CR 701.67a's taps are offered (`waterbendSubstitutions`), `canPay`'s offer, so
-- the gate and the payment agree about a waterbend cost.
pay :: ManaAbilityPerformer.ManaAbilityPerformer -> GameState -> PaymentMoment.PaymentMoment -> PaymentSubject.PaymentSubject -> Maybe ObjectId -> ManaSpending.ManaSpending -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game Payment.Payment
pay perform began moment subject announced spending pid oid cost = fmap fst (paySubstituting perform began [] moment subject announced spending pid oid (announceSubstitutions waterbendSubstitutions pid oid) cost)

-- `pay` with no announcement and the component criteria reading `slots`
-- instead, `canPayReading`'s payment. No announcement, so CR 400.7d's record of
-- the mana spent goes nowhere, as it does for every CR 118.12 payment.
--
-- CR 701.67a's taps are offered against the same `slots`: The Unagi of Kyoshi
-- Island's ward and Waterbending Lesson's unless cost are waterbend costs paid
-- here (Pawl.CostSpec's groups of those names).
payReading :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> ManaAbilityPerformer.ManaAbilityPerformer -> GameState -> PaymentMoment.PaymentMoment -> PaymentSubject.PaymentSubject -> ManaSpending.ManaSpending -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game Payment.Payment
payReading slots perform began moment subject spending pid oid cost = fmap fst (paySubstitutingReading slots perform began [] moment subject Nothing spending pid oid (announceSubstitutionsReading slots waterbendSubstitutions pid oid) cost)

-- `pay` with CR 702.51a's, CR 702.66a's and CR 702.126a's substitution offered
-- INSIDE the mana window rather than ahead of it, and the components it adds
-- kept APART from the cost's own.
--
-- WHERE THE OFFER SITS is CR 601.2g and the reminder text: the window comes
-- after the total cost is determined and before the cost is paid, and convoke's
-- and improvise's reminders put the taps after it ("each artifact you tap after
-- you're done activating mana abilities pays for {1}"). So `announce` is run by
-- `payManaWindow` once the window has CLOSED -- a payer holding Birds of
-- Paradise sees what colour it made before saying how many creatures convoke the
-- spell, and a creature tapped for mana in the window is no longer an untapped
-- creature the offer can spend.
--
-- KEPT APART is the other half, and CR 702.51c's: the record is of the creatures
-- tapped THIS way, where Binding.tappedPermanent names every permanent any tap
-- component of the cost took. The second answer is the slots the CONVOKING
-- substitutes bound alone (`convokes`), and Pawl.Engine.Cast reads it.
--
-- `earlier` are the mana windows the announcement already ran for OTHER
-- players, oldest first -- CR 702.132a's assisting player's, and nothing
-- otherwise. A refused payment reverses them with its own, so each of those
-- players is asked too (`reverseIllegal`).
--
-- The cost's own components and the substitutes (split again by `convokes`) go
-- through payComponents SEPARATELY, so each group makes CR 601.2h's
-- two passes of its own. That is not the rule: CR 601.2h wants ONE first pass
-- over everything that moves no card out of a library, then one second pass over
-- the rest -- so a cost carrying a MillCards component of its own would have it
-- paid before the substitutes' first pass, which inverts the rule's order.
-- Nothing in `data/cards/` observes it, which is why no issue is filed: no
-- printing there that states convoke, delve or improvise carries a cost
-- component at all (checked 2026-09-13), and one that did would refute this.
paySubstituting :: ManaAbilityPerformer.ManaAbilityPerformer -> GameState -> [ManaWindow.ManaWindow] -> PaymentMoment.PaymentMoment -> PaymentSubject.PaymentSubject -> Maybe ObjectId -> ManaSpending.ManaSpending -> PlayerId -> ObjectId -> (Cost Keyword.Type.Keyword -> Game (Cost Keyword.Type.Keyword, [(Keyword.Substitute, Natural)])) -> Cost Keyword.Type.Keyword -> Game (Payment.Payment, Map.Map SlotName.SlotName Binding.Type.Binding)
paySubstituting perform began earlier moment subject announced spending pid oid substituting cost = do
  slots <- State.gets (announcedSlots announced)
  paySubstitutingReading slots perform began earlier moment subject announced spending pid oid substituting cost

-- `paySubstituting` with the slot map its component criteria read handed in
-- rather than read off `announced` (`payReading`).
paySubstitutingReading :: Map.Map SlotName.SlotName (Set.Set ObjectId) -> ManaAbilityPerformer.ManaAbilityPerformer -> GameState -> [ManaWindow.ManaWindow] -> PaymentMoment.PaymentMoment -> PaymentSubject.PaymentSubject -> Maybe ObjectId -> ManaSpending.ManaSpending -> PlayerId -> ObjectId -> (Cost Keyword.Type.Keyword -> Game (Cost Keyword.Type.Keyword, [(Keyword.Substitute, Natural)])) -> Cost Keyword.Type.Keyword -> Game (Payment.Payment, Map.Map SlotName.SlotName Binding.Type.Binding)
paySubstitutingReading slots perform began earlier moment subject announced spending pid oid substituting cost =
  case Cost.mana cost of
    -- CR 118.6: attempting to pay an unpayable cost is an illegal action, and
    -- CR 733.1 reverses it with no window to ask about.
    Nothing -> do
      Monad.void (reverseIllegal earlier began)
      pure (Payment.Unpaid, Map.empty)
    -- CR 601.2g: the window PROMPTS for which sources to activate, so it is
    -- monadic, and it hands back how to reverse itself rather than reversing
    -- itself -- one question has to cover the components below too.
    Just manaCost -> do
      let announceMana mc = do
            (chosen, extra) <- substituting cost {Cost.mana = Just mc}
            pure (Maybe.fromMaybe mc (Cost.mana chosen), extra)
      (paidMana, substitutes, own) <- payManaWindow perform Set.empty announced subject spending pid announceMana manaCost
      paidOwn <-
        if paidMana
          then payComponents moment slots pid oid (Cost.components cost)
          else pure Payment.Unpaid
      -- The convoking taps are paid as a group of their own so that their
      -- bindings stay apart (CR 702.51c), after the other substitutes. The
      -- payer still picks every permanent in each group, so the order between
      -- the groups decides nothing.
      let (convoking, others) = List.partition (convokes . fst) substitutes
      (outcome, substituted) <- case paidOwn of
        Payment.Unpaid -> pure (Payment.Unpaid, Map.empty)
        Payment.Paid bound -> do
          paidOthers <- payComponents moment slots pid oid (substituteComponents others)
          case paidOthers of
            Payment.Unpaid -> pure (Payment.Unpaid, Map.empty)
            Payment.Paid _ -> do
              paidConvoking <- payComponents moment slots pid oid (substituteComponents convoking)
              pure $ case paidConvoking of
                Payment.Unpaid -> (Payment.Unpaid, Map.empty)
                Payment.Paid convoked -> (mergeBound bound (mergeBound convoked paidOthers), convoked)
      case outcome of
        -- The components' bound slots ride out unchanged: the mana window
        -- above binds none, and a caller that has a binding environment to
        -- write them into is the only thing between here and CR 608.2h.
        Payment.Paid _ -> pure (outcome, substituted)
        -- CR 733.1: the action is reversed back to `began`, and each window is
        -- its payer's to keep.
        Payment.Unpaid -> do
          Monad.void (reverseIllegal (earlier <> [own]) began)
          pure (Payment.Unpaid, Map.empty)

-- CR 508.1h-508.1j and CR 509.1d-509.1f: pay a COMBAT TOLL -- the costs to attack
-- or to block that one declaration incurred, each tagged with the permanent that
-- printed it (Pawl.Engine.AttackCost.totalCost).
--
-- The mana halves are ADDED and the non-mana halves are not, which is what "the
-- total cost" means once CR 508.1h's list is read past its first item: two
-- Ghostly Prisons owe {4} in one payment, and two Exalted Dragons owe two
-- sacrifices that each keep the permanent that demanded them. So CR 508.1i and CR
-- 509.1e open ONE mana window, here, and the components follow it.
--
-- Skipped at {0}, so a toll of components alone never opens a window: both rules
-- say "if any of the costs require mana".
--
-- ALL OR NOTHING (CR 508.1j, CR 509.1f: "partial payments are not allowed"). A
-- payer who sacrifices the first land and then cannot find a second ends up
-- having sacrificed nothing. CR 508.1's and CR 509.1's preambles send the
-- illegal declaration to rule 733, so a Nothing here has reversed the whole
-- declaration back to `began`, the caller's snapshot ahead of CR 508.1a's or
-- 509.1a's record, and the mana abilities the window activated go back only if
-- the payer says so (`reverseIllegal` above) -- `pay`'s posture for CR 601.2h.
-- A part that moved a card into or out of a library stands (`payTagged`).
-- The declaration's own writes include CR 508.1f's tap and CR 508.1g's exert on
-- a creature the window may tap for mana too, which is why
-- Pawl.Engine.Reversal descends inside an Object.
--
-- `earlier` is what the players ahead of this one in the same declaration left
-- standing -- their mana windows and their unreversible stretches, oldest first
-- -- and a Just hands it back with this payer's appended, so that a later
-- payer's refusal reverses the WHOLE declaration with each of them asked
-- (Pawl.Engine.Combat.payTolls). The scenario
-- team/cr-733-1-a-card-one-teammate-milled-to-attack-stays-milled-when-the-other-s-toll-fails
-- is the proof.
--
-- The bound slots ride out unread. A component of a combat toll binds what
-- payComponent binds it (Sacrifice, TapPermanents, TapForTotalPower, ExileThis
-- and ExileThisFromGraveyard each reserve a name), and
-- there is no resolving ability holding a binding environment to write them
-- into -- CR 508.1j and CR 509.1f name a payment and no effect, where CR
-- 601.2f's components are paid for a spell that goes on to resolve.
--
-- The ORDER ACROSS TAGS is the payer's, which is what "in any order" says about
-- a toll two permanents taxed: `tollOrderObservable` below decides whether the
-- payer can tell one order from another, and Prompt.OrderCombatTolls asks. Each
-- tag's OWN components are ordered by payTagged after that, so the two
-- prompts nest rather than compete. CombatEffectSpec's "CR 508.1j the payer
-- orders the two taxing permanents: Hollow Warrior before Exalted Dragon" is the
-- proof.
--
-- The pooled MANA is paid before the order is asked, which is CR 508.1i and CR
-- 509.1e sitting ahead of the payment rule rather than a choice pawl made, and
-- `pay` above takes the same posture for CR 601.2g.
payToll :: ManaAbilityPerformer.ManaAbilityPerformer -> GameState -> [ManaWindow.ManaWindow] -> PlayerId -> [(ObjectId, Cost Keyword.Type.Keyword)] -> Game (Maybe [ManaWindow.ManaWindow])
payToll perform began earlier pid charges =
  -- CR 118.6: a toll one of whose parts is unpayable is unpayable whole.
  --
  -- `earlier` rides along here as it does below, a FENCE rather than proven
  -- behaviour: nothing in the suite reaches this arm behind another payer.
  case traverse (Cost.mana . snd) charges of
    Nothing -> refused earlier
    Just _ -> do
      announced <- announceToll pid charges
      let pooled = ManaCost.MkManaCost (concatMap (foldMap ManaCost.unwrap . Cost.mana . snd) announced)
      (paidMana, windows) <-
        if null (ManaCost.unwrap pooled)
          then pure (True, [])
          -- No subject (ForNeither), so mana a CR 106.6 permission restricts
          -- cannot pay a combat toll and mana a prohibition restricts can
          -- (Pawl.Engine.Mana.admitsUnder). Exact: every printed restriction
          -- names a cast, an activation or a special action, and CR 508.1j's
          -- toll is none of those.
          else do
            (paid, _, window) <- payManaWindow perform Set.empty Nothing PaymentSubject.ForNeither ManaSpending.AsProduced pid (\mc -> pure (mc, [])) pooled
            pure (paid, [window])
      if not paidMana
        then refused (earlier <> windows)
        else do
          let tagged = fmap (fmap Cost.components) announced
          ordered <-
            if tollOrderObservable tagged
              then do
                gs <- State.get
                answer <- Game.choose (Prompt.OrderCombatTolls (Decide.deciderFor pid gs) pid (fmap fst tagged))
                -- FILTERED, NOT TRUSTED: Game.permute keeps the gathered order
                -- for an answer that is not a permutation of the offered indices,
                -- payPass's posture below.
                pure (Game.permute tagged answer)
              else pure tagged
          (outcome, stretches) <- payTagged pid ordered
          case outcome of
            Payment.Paid _ -> pure (Just (earlier <> windows <> stretches))
            Payment.Unpaid -> refused (earlier <> windows <> stretches)
  where
    refused windows = Nothing <$ reverseIllegal windows began

-- Which way each of the toll's symbols payable in more than one way will be
-- paid, chosen by the PAYER immediately before CR 508.1j's and CR 509.1f's
-- payment -- so after CR 508.1h's and CR 509.1d's lock-in, and before CR 508.1i's
-- and CR 509.1e's mana window, which `payToll` above opens.
--
-- NOT one of rule 118.13's moments, and that rule states no fourth: 118.13a names
-- a spell's or an activated ability's cost, 118.13b a cost paid during a
-- resolution and 118.13c a special action's, and a cost to attack or block is
-- none of the three. What puts the choice with the payer is that it IS a choice
-- -- docs/design.md's second invariant -- and the placement is the one both
-- rules that do state a moment for a cost already settled use, "immediately
-- before they pay that cost". Norn's Annex is the only printing that reaches
-- this: Scryfall `o:/unless .* pays? \{/`, 2026-09-01, 294 printings and its
-- {W/P} the only one payable in more than one way.
--
-- ONE ANNOUNCEMENT PER CHARGE, tagged with the permanent that printed it, so the
-- prompt names an object even though CR 508.1h's total is on none
-- (Pawl.Engine.AttackCost.costsOn). Payability is still measured across the WHOLE
-- toll: `total_` hands Mana.announce the charges already announced beside the
-- ones still to come, and `committed` carries the life earlier charges took, so a
-- route offered here is one the rest of the toll survives. Norn's Annex taxing
-- two attackers is the board that needs it -- at 3 life and no white source
-- neither {W/P} may take the life route, where a per-charge measure would offer
-- both and strand the payment.
--
-- The life a charge's announcement commits becomes a CostComponent.PayLife on
-- THAT charge, which payTagged pays against that same permanent -- `announce`
-- above, one charge at a time. Its Natural is discarded for Activate's reason:
-- rule 702.150a asks about the player who CAST an object.
announceToll :: PlayerId -> [(ObjectId, Cost Keyword.Type.Keyword)] -> Game [(ObjectId, Cost Keyword.Type.Keyword)]
announceToll pid charges = do
  start <- State.get
  let symbolsOf = foldMap ManaCost.unwrap . Cost.mana . snd
      -- CR 118.3 makes the whole toll one demand on one life total, so every
      -- charge's own CR 119.4 payments ride on every route offered for any of them.
      outside = sum (fmap (lifeOwedBy pid start . Cost.components . snd) charges)
      outsideEnergy = sum (fmap (energyOwedBy . Cost.components . snd) charges)
      go done committed remaining = case remaining of
        [] -> pure (reverse done)
        (tag, cost) : rest -> case Cost.mana cost of
          -- CR 118.6: nothing to announce, and unreachable besides -- payToll
          -- refuses a toll holding an unpayable charge before calling this.
          Nothing -> go ((tag, cost) : done) committed rest
          Just manaCost -> do
            gs <- State.get
            -- Order within the probe carries nothing -- payability is a question
            -- about a multiset of symbols -- so `done` rides in reversed.
            --
            -- `others` and `committed` below are REDUNDANT with each other on
            -- every board `data/cards/` can build, the pool's only hybrid toll
            -- being Norn's Annex's {W/P}: dropping either alone leaves
            -- CombatEffectSpec's "CR 508.1h two taxed attackers at 3 life"
            -- green, and dropping both puts alice at -1. Both are kept because
            -- they answer different halves of CR 118.3 -- what the rest of the
            -- toll still needs, and what earlier answers have already spent.
            let others = concatMap symbolsOf done <> concatMap symbolsOf rest
                total_ mana = [ManaCost.MkManaCost (ManaCost.unwrap mana <> others)]
                claimed = concatMap (\(t, c) -> claimsOf Map.empty pid t (Cost.components c) gs) charges
            (settled, life, _) <-
              Mana.announce
                PaymentSubject.ForNeither
                (midPayment (manaActivationsGiven (PlayerEffect.applying pid gs)))
                ManaSpending.AsProduced
                pid
                tag
                total_
                (outside + committed)
                outsideEnergy
                claimed
                manaCost
            let paid =
                  cost
                    { Cost.mana = Just settled,
                      Cost.components = Cost.components cost <> (if life > 0 then [CostComponent.PayLife life] else [])
                    }
            go ((tag, paid) : done) (committed + life) rest
  go [] 0 charges

-- CR 508.1j / 509.1f: can the payer tell one order of a toll's CHARGES from
-- another? `orderObservable` below, one level up, and its two conditions read
-- over whole charges rather than parts.
--
-- TWO OR MORE charges holding a part that touches an object (`orderSensitive`):
-- a charge that is mana alone was spent from the pool payToll already paid, so
-- nothing about it competes with anything.
--
-- And NOT ALL EQUAL by their PARTS, the tags being distinct permanents by
-- construction and so never equal. Two Exalted Dragons owe the same "sacrifice a
-- land" twice, and the payer who orders those two picks the same land from the
-- same offer either way. Sound only while no toll in `data/cards/` prints a part
-- naming the permanent it is on -- Pawl.Types.CostComponent's SacrificeThis and
-- TapThis, which would make two equal lists name two different permanents; the
-- pool's tolls are Exalted Dragon's Sacrifice, Hollow Warrior's TapPermanents,
-- Synthetic Tithe of Memory's MillCards and Sphere of Safety's counted mana, and
-- none of them does.
--
-- BOTH conjuncts are FENCES rather than proven behaviour, `orderObservable`'s
-- admission below: the boards that would tell them apart print two identical
-- tolls or a mana-only toll beside another, and answering True for those raises
-- a prompt whose answer changes nothing, so dropping either leaves the suite
-- green.
tollOrderObservable :: [(ObjectId, [CostComponent.CostComponent Keyword.Type.Keyword])] -> Bool
tollOrderObservable charges = case filter (any orderSensitive . snd) charges of
  first : rest@(_ : _) -> not (all (\charge -> snd charge == snd first) rest)
  _ -> False

-- payToll's fold: each tag's components against the permanent that printed them,
-- stopping at the first refusal. payInOrder's shape one level up, and the merge is
-- that function's for its reason.
--
-- A part that moved a card into or out of a library comes back as a stretch CR
-- 733.1 forbids reversing (`unreversibleStretch`): CR 508.1j's "in any order"
-- lets a mill paid for one tag finish before a LATER tag refuses, and the
-- reversal that follows must keep it. The scenario
-- combat-cost/cr-733-1-a-card-milled-to-attack-stays-milled-when-a-later-toll-fails
-- is the proof.
--
-- Not implemented: the payer's order WITHIN a tag. CR 508.1j and CR 509.1f say
-- only "in any order", but each tag is split by payComponents' two passes
-- (`paidInSecondPass`), so a mill is always paid after the tag's other parts.
-- Not implemented either: keeping a reveal from a library standing when its part
-- moved no card. Such a part falls in a reversed gap, so its Revealed events go
-- back (#4862).
payTagged :: PlayerId -> [(ObjectId, [CostComponent.CostComponent Keyword.Type.Keyword])] -> Game (Payment.Payment, [ManaWindow.ManaWindow])
payTagged pid charges = case charges of
  [] -> pure (bindsNothing, [])
  (oid, components) : rest -> do
    -- CR 508.1j / 509.1f: a toll is paid during the declaration, which is a
    -- turn-based action and not a resolution (PaymentMoment's own reason).
    --
    -- No slots: a declaration announces no targets (CR 508.1h, CR 509.1d), so
    -- there is nothing for a toll's criterion to be bound to.
    let (second, first) = List.partition paidInSecondPass components
        payKeeping stretches parts = case parts of
          [] -> pure (bindsNothing, reverse stretches)
          component : others -> do
            opened <- State.get
            outcome <- payComponent PaymentMoment.OutsideResolution Map.empty pid oid component
            closed <- State.get
            let kept = [unreversibleStretch pid oid opened closed | libraryMembershipChanged opened closed] <> stretches
            case outcome of
              Payment.Unpaid -> pure (Payment.Unpaid, reverse kept)
              Payment.Paid bound -> do
                (rested, standing) <- payKeeping kept others
                pure (mergeBound bound rested, standing)
    firstOutcome <- payPass PaymentMoment.OutsideResolution Map.empty pid oid first
    case firstOutcome of
      Payment.Unpaid -> pure (Payment.Unpaid, [])
      Payment.Paid firstBound -> do
        (secondOutcome, stretches) <- payKeeping [] =<< orderPass pid oid second
        case secondOutcome of
          Payment.Unpaid -> pure (Payment.Unpaid, stretches)
          Payment.Paid secondBound -> do
            (outcome, later) <- payTagged pid rest
            pure (mergeBound firstBound (mergeBound secondBound outcome), stretches <> later)

-- A stretch of a payment that moved a card into or out of a library, which CR
-- 733.1 forbids reversing, shaped as a window holding one activation that
-- `reverseIllegal` keeps standing unasked -- the posture it takes towards a mana
-- ability that drew. It is NOT a CR 605.3a window: `oid` is the permanent whose
-- charge it paid, and nothing offers it back.
unreversibleStretch :: PlayerId -> ObjectId -> GameState -> GameState -> ManaWindow.ManaWindow
unreversibleStretch pid oid opened closed =
  ManaWindow.MkManaWindow
    { ManaWindow.payer = pid,
      ManaWindow.activated = [ManaActivation.MkManaActivation {ManaActivation.sources = oid NonEmpty.:| [], ManaActivation.spent = [], ManaActivation.added = []}],
      ManaWindow.segments = [ManaSegment.MkManaSegment {ManaSegment.activation = 0, ManaSegment.opened = opened, ManaSegment.closed = closed}],
      ManaWindow.spent = [],
      ManaWindow.opened = opened,
      ManaWindow.closed = closed
    }

-- CR 733.1: did a card go into or out of a library between the two states? Its
-- "moved cards to a library [or] from a library to any zone other than the
-- stack", read off membership: nothing that reaches here moves a card from a
-- library to the stack.
libraryMembershipChanged :: GameState -> GameState -> Bool
libraryMembershipChanged opened closed =
  let held gs = Set.fromList (foldMap Foldable.toList (GameState.library gs))
   in held opened /= held closed

-- CR 601.2h: the parts are paid "in any order", and the ORDER IS THE PAYER'S.
-- Observable: Jarad, Golgari Lich Lord's "Sacrifice a Swamp and a Forest" beside
-- one Bayou and one plain Swamp is payable, and a payer who spends the Bayou on
-- the Swamp half loses the cost.
--
-- TWO PASSES, which is CR 601.2h's own structure: everything that moves no card
-- out of a library into a public zone first, in any order, then everything left
-- (`paidInSecondPass` below). Each pass is ordered on its own, so a cost holding
-- one part of each kind asks NOTHING -- Millikin's "{T}, Mill a card" is that
-- cost -- where one list of both would have offered orders the rules forbid.
--
-- Asked ONCE PER PASS; each component's own prompts are still issued as it is
-- paid.
--
-- FILTERED, NOT TRUSTED: Game.permute keeps the printed order for an answer that
-- is not a permutation of the offered indices.
payComponents :: PaymentMoment.PaymentMoment -> Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> Game Payment.Payment
payComponents moment slots pid oid components = do
  let (second, first) = List.partition paidInSecondPass components
  outcome <- payPass moment slots pid oid first
  case outcome of
    Payment.Unpaid -> pure Payment.Unpaid
    Payment.Paid bound -> fmap (mergeBound bound) (payPass moment slots pid oid second)

-- ONE of CR 601.2h's two passes: the payer orders it where the order is
-- observable, then it is paid in that order.
payPass :: PaymentMoment.PaymentMoment -> Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> Game Payment.Payment
payPass moment slots pid oid components = payInOrder moment slots pid oid =<< orderPass pid oid components

-- The payer's order for one pass, asked only where it is observable.
orderPass :: PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> Game [CostComponent.CostComponent Keyword.Type.Keyword]
orderPass pid oid components =
  if orderObservable components
    then do
      gs <- State.get
      answer <- Game.choose (Prompt.OrderCostComponents (Decide.deciderFor pid gs) pid oid components)
      pure (Game.permute components answer)
    else pure components

-- CR 601.2h: is this part paid in the SECOND pass -- "all costs that don't
-- involve random elements or moving objects from the library to a public zone"
-- being the first, and this the rest?
--
-- CostComponent.FlipCoin is the RANDOM half's only arm; nothing in
-- Pawl.Types.CostComponent rolls a die, so every other answer below is decided by
-- the library half alone. One predicate for both, because rule 601.2h's two
-- criteria select one pass between them.
--
-- EXHAUSTIVE with no wildcard, `orderSensitive`'s posture and for its reason.
paidInSecondPass :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
paidInSecondPass component = case component of
  -- CR 701.17a moves cards from a library to a graveyard, and CR 400.2 makes the
  -- one hidden and the other public. The one True arm in the vocabulary.
  CostComponent.MillCards _ -> True
  -- A HAND is not a library (CR 400.2 makes both hidden, which is not what rule
  -- 601.2h asks), and a graveyard, a battlefield and a stack are not either, so
  -- every other component that moves an object stays in the first pass.
  CostComponent.DiscardCards {} -> False
  CostComponent.DiscardThis _ -> False
  CostComponent.PutCardFromHandOntoBattlefield _ -> False
  CostComponent.SacrificeThis -> False
  CostComponent.Sacrifice {} -> False
  CostComponent.ReturnThis -> False
  CostComponent.ReturnPermanents {} -> False
  CostComponent.ExilePermanents {} -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileThis -> False
  CostComponent.ExileCardsFromGraveyard {} -> False
  CostComponent.ExileMaterials {} -> False
  CostComponent.ExileTopFromGraveyard _ -> False
  CostComponent.CollectEvidence _ -> False
  CostComponent.CollectEvidenceOfTargets -> False
  CostComponent.ExileCardFromHand _ -> False
  -- These move no object at all.
  CostComponent.TapThis -> False
  CostComponent.UntapThis -> False
  CostComponent.TapForTotalPower {} -> False
  CostComponent.TapPermanents {} -> False
  CostComponent.PayLife _ -> False
  CostComponent.PayHalfLife _ -> False
  CostComponent.PayLifeX -> False
  CostComponent.PayEnergyX -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.AddLoyaltyToThis _ -> False
  CostComponent.RemoveLoyaltyFromThis _ -> False
  CostComponent.RemoveCountersFromThis _ -> False
  CostComponent.RemoveCounters {} -> False
  CostComponent.RemovePlusOneCountersX _ -> False
  CostComponent.SacrificeX _ -> False
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.Blight _ -> False
  CostComponent.BlightX -> False
  CostComponent.RemoveLoyaltyFromThisX -> False
  -- CR 701.61a moves cards from a graveyard to exile, or a Food from the
  -- battlefield to a graveyard, and neither is a library, so the first pass holds
  -- it -- the header's reading.
  CostComponent.Forage -> False
  -- CR 601.2h's RANDOM half, and the one part of this type that reaches it: a
  -- coin flip is the rule's own example of a cost involving a random element, so
  -- it is paid after every part that does not.
  CostComponent.FlipCoin -> True
  -- CR 702.174a's choice moves no object and involves no random element, so
  -- neither half of rule 601.2h's first criterion reaches it.
  CostComponent.ChooseOpponent -> False
  CostComponent.WaterbendX -> False
  CostComponent.Waterbend _ -> False
  CostComponent.WaterbendInstead _ -> False
  -- CR 701.20b moves nothing out of any zone, so rule 601.2h's library half has
  -- nothing to ask of it.
  CostComponent.RevealCardFromHand _ -> False
  -- CR 701.4a moves nothing out of any zone, RevealCardFromHand's arm above.
  CostComponent.Behold _ -> False
  -- The exile moves a card out of a hand or off the battlefield, neither a
  -- library, so the header's first pass holds it.
  CostComponent.BeholdAndExile _ -> False

payInOrder :: PaymentMoment.PaymentMoment -> Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> [CostComponent.CostComponent Keyword.Type.Keyword] -> Game Payment.Payment
payInOrder moment slots pid oid components = case components of
  [] -> pure bindsNothing
  component : rest -> do
    outcome <- payComponent moment slots pid oid component
    case outcome of
      Payment.Unpaid -> pure Payment.Unpaid
      Payment.Paid bound -> fmap (mergeBound bound) (payInOrder moment slots pid oid rest)

-- The slots two components of one cost bound, in one map: see
-- Pawl.Engine.Binding.mergePaid.
mergeBound :: Map.Map SlotName.SlotName Binding.Type.Binding -> Payment.Payment -> Payment.Payment
mergeBound bound outcome = case outcome of
  Payment.Unpaid -> Payment.Unpaid
  Payment.Paid rest -> Payment.Paid (Binding.mergePaid bound rest)

-- A component that bound no slot.
bindsNothing :: Payment.Payment
bindsNothing = Payment.Paid Map.empty

-- The payment an ExileThis or ExileThisFromGraveyard component makes: CR 601.2h
-- pays it, and Binding.exiledCard names what CR 400.7 put into exile.
--
-- An empty arrival binds NOTHING rather than an empty set, a replacement effect
-- having sent the object somewhere other than exile: the two spellings read
-- differently where a slot's PRESENCE is the question, and
-- Pawl.Engine.Resolve.clauseIsInert asks exactly that -- a present key naming
-- nobody would make the clause look answerable and raise its CR 603.5 offer.
bindExiled :: Seq.Seq ObjectId -> Payment.Payment
bindExiled arrived = case Foldable.toList arrived of
  [] -> bindsNothing
  ids -> Payment.Paid (Binding.paidObjects Binding.exiledCard (Set.fromList (fmap Recipient.ToObject ids)))

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
-- EXHAUSTIVE with no wildcard, `claimOf`'s posture. Asked WITHIN one of CR
-- 601.2h's passes rather than across the whole cost -- `paidInSecondPass` above
-- is what splits them -- so a True here only ever competes with the other parts
-- of its own pass.
orderSensitive :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
orderSensitive component = case component of
  CostComponent.Sacrifice {} -> True
  CostComponent.SacrificeThis -> True
  -- A FENCE rather than proven behaviour: Grinning Ignus is the one card that
  -- prints this component and its cost has no second order-sensitive part, so
  -- `orderObservable` is False whichever way this answers.
  CostComponent.ReturnThis -> True
  -- A FENCE rather than proven behaviour, ReturnThis' above and for its reason:
  -- Meloku the Clouded Mirror is the one card that PRINTS this component, and its
  -- cost has no second order-sensitive part. Nor has any cost that MINTS one --
  -- CR 702.49a's ninjutsu, CR 702.188a's web-slinging and CR 702.190a's sneak,
  -- which each append exactly this component to a mana cost and nothing else.
  CostComponent.ReturnPermanents {} -> True
  CostComponent.ExilePermanents {} -> True
  CostComponent.DiscardCards {} -> True
  CostComponent.DiscardThis _ -> True
  CostComponent.PutCardFromHandOntoBattlefield _ -> True
  CostComponent.ExileCardsFromGraveyard {} -> True
  CostComponent.ExileMaterials {} -> True
  CostComponent.ExileTopFromGraveyard _ -> True
  CostComponent.CollectEvidence _ -> True
  CostComponent.CollectEvidenceOfTargets -> True
  CostComponent.ExileCardFromHand _ -> True
  -- FALSE, one of the two object-choosing components that answer so: CR 701.20b
  -- leaves the card where it was, so paying this changes no other part's pool.
  CostComponent.RevealCardFromHand _ -> False
  -- FALSE, RevealCardFromHand's arm above and for its reason: CR 701.4a leaves the
  -- card in the hand and the permanent on the battlefield, so paying this changes
  -- no other part's pool.
  CostComponent.Behold _ -> False
  -- TRUE, unlike Behold above: the exile takes the object out of a pool another
  -- part could draw on.
  CostComponent.BeholdAndExile _ -> True
  CostComponent.ExileThisFromGraveyard -> True
  CostComponent.ExileThis -> True
  CostComponent.TapThis -> True
  CostComponent.UntapThis -> True
  CostComponent.TapForTotalPower {} -> True
  CostComponent.TapPermanents {} -> True
  CostComponent.AddLoyaltyToThis _ -> True
  -- True: the counters come off the permanent the cost is on, an object. A FENCE
  -- rather than proven behaviour: Barkhide Troll's cost has no second
  -- order-sensitive part, and Hickory Woodlot's {T} beside it makes the order a
  -- question no board in the suite reads.
  CostComponent.RemoveCountersFromThis _ -> True
  -- True, the arm above's reading over another permanent: paying this takes the
  -- counters a second part of the same cost could have taken. A FENCE rather
  -- than proven behaviour -- Zameck Guildmage's cost's other part is mana, which
  -- is not a component at all, so `orderObservable` is False either way.
  CostComponent.RemoveCounters {} -> True
  -- True, the substituted component's answer; unreachable before the
  -- announcement substitutes it.
  CostComponent.RemovePlusOneCountersX _ -> True
  CostComponent.SacrificeX _ -> True
  CostComponent.RemoveLoyaltyFromThis _ -> True
  CostComponent.PutPlusOneCountersOnThis _ -> True
  CostComponent.Blight _ -> True
  -- The arm above's classification, which is what CR 601.2b turns this into.
  -- Unreachable unsubstituted: `pay` runs on the announced cost.
  CostComponent.BlightX -> True
  -- The substituted component's answer, BlightX's posture.
  CostComponent.RemoveLoyaltyFromThisX -> True
  -- True: CR 701.61a moves three cards out of a graveyard or a Food off the
  -- battlefield, either of which another part of the same cost could have spent.
  -- A FENCE rather than proven behaviour -- Thornvault Forager is the one card in
  -- `data/cards/` printing this component, and its cost's other part is a {T},
  -- which answers True too, so `orderObservable` is False either way.
  CostComponent.Forage -> True
  -- False: CR 705.1's flip spends nothing another part of the same cost could
  -- have spent, the per-player scalars' answer. Alone in CR 601.2h's second pass
  -- on this pool -- `paidInSecondPass` above puts it there and nothing else --
  -- so `orderObservable` is False either way.
  CostComponent.FlipCoin -> False
  -- False: rule 702.174a's choice spends nothing another part of the same cost
  -- could have spent, FlipCoin's answer just above.
  CostComponent.ChooseOpponent -> False
  CostComponent.WaterbendX -> False
  -- False: rule 701.67a's licence spends nothing another part of the same cost
  -- could have spent, ChooseOpponent's answer just above.
  CostComponent.Waterbend _ -> False
  CostComponent.WaterbendInstead _ -> False
  -- CR 701.17a puts a card into a graveyard, which a graveyard-reading part of
  -- the same cost could then spend (Circling Vultures' "the top creature card of
  -- your graveyard"). Alone in CR 601.2h's second pass on this pool, so nothing
  -- it could compete with is ever in the same pass -- a FENCE, not proven
  -- behaviour.
  CostComponent.MillCards _ -> True
  CostComponent.PayLife _ -> False
  CostComponent.PayHalfLife _ -> False
  CostComponent.PayLifeX -> False
  CostComponent.PayEnergyX -> False
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
-- Keyed by ROUTE, the (permanent, ability) pair Mana.InFlight uses, since CR
-- 605.3a goes on offering a permanent's other mana abilities: declining Skyshroud
-- Elf's {1} leaves its {T} on the window (data/scenarios'
-- skyshroud-elf-declined-route-keeps-tap is the proof).
--
-- The life budget only ever binds a cost NOTHING ANNOUNCED for, and every road
-- into this function now runs `announce` first: a cast, an activation, a CR
-- 118.12 pay gate, a special action, a combat toll, and a mana ability's own
-- activation cost. So no gameplay path reaches it with a Phyrexian symbol still
-- in the cost, and it is a regression fence rather than live behaviour --
-- Pawl.ManaSpec drives it by calling `payMana` directly. Recomputed on EVERY pass
-- all the same, a tap being able to change it -- a Birds of Paradise tapped for
-- blue takes the mana way to an unannounced {G/P} off the board, leaving CR
-- 107.4f's 2 life.
--
-- The window in full. `inFlight` is Pawl.Engine.Mana.InFlight, the ABILITIES
-- mid-activation, which is the one thing that has to bound the recursion CR
-- 602.2b creates; `subject` is payMana's own first argument, what this payment is
-- for. `inFlight` is non-empty only where `subject` is not a Casting: an
-- activation is not a cast.
--
-- CR 605.3c is what the set says -- an ability being activated cannot be
-- activated again until it has resolved -- and it TERMINATES the recursion,
-- since every nested activation adds one (permanent, ability) pair to a set the
-- battlefield and the abilities printed on it bound.
--
-- The rule narrows the ABILITY and not the permanent, so a permanent's OTHER
-- mana ability stays on the window its first one opened: Skyshroud Elf's
-- "{1}: Add {R} or {W}" is paid by tapping the same Elf for its "{T}: Add {G}"
-- (Pawl.ManaSpec's Skyshroud Elf group is what proves it).
payManaExcept :: ManaAbilityPerformer.ManaAbilityPerformer -> Mana.InFlight -> Maybe ObjectId -> PaymentSubject.PaymentSubject -> ManaSpending.ManaSpending -> PlayerId -> ManaCost.ManaCost -> Game Bool
payManaExcept perform inFlight record subject spending pid cost = do
  before <- State.get
  -- No substitution: CR 702.51a, CR 702.66a and CR 702.126a all function while a
  -- SPELL is on the stack, and every road into this one is something else.
  (paid, _, window) <- payManaWindow perform inFlight record subject spending pid (\mc -> pure (mc, [])) cost
  -- CR 733.1, this payment being the whole of what failed: the payer may keep
  -- what the window activated.
  Monad.unless paid (Monad.void (reverseIllegal [window] before))
  pure paid

-- payManaExcept's window, handing back what CR 733.1 needs to reverse it: the
-- outcome, and the window itself (`reverseIllegal`), whose reversal also takes
-- the state the payment began in -- which is the CALLER's, since a payment goes
-- on past the window and one question has to cover the whole reversal. `pay`,
-- `payToll` and `payActivation` hold it until the components have settled.
--
-- `substituting` is CR 702.51b's, CR 702.66b's and CR 702.126b's offer, run once the
-- window has CLOSED and before a symbol is spent: it takes the total cost's mana
-- and answers what is left of it after the substitutes the payer chose, beside
-- the components that spending is written as. `settle` plans against that
-- residual, and the caller pays those components (`paySubstituting`).
--
-- The window LOOP still reads the unsubstituted cost, which is what `covered`
-- below asks about: nothing has been substituted yet while the window is open,
-- so CR 118.3c's question is put against the whole of it.
payManaWindow :: ManaAbilityPerformer.ManaAbilityPerformer -> Mana.InFlight -> Maybe ObjectId -> PaymentSubject.PaymentSubject -> ManaSpending.ManaSpending -> PlayerId -> (ManaCost.ManaCost -> Game (ManaCost.ManaCost, [(Keyword.Substitute, Natural)])) -> ManaCost.ManaCost -> Game (Bool, [(Keyword.Substitute, Natural)], ManaWindow.ManaWindow)
payManaWindow perform inFlight record subject spending pid substituting cost = do
  -- Where the window OPENED, which is the far end of the announcement's diff:
  -- everything between the caller's own snapshot and this is what the
  -- announcement wrote, and everything after it is the window's. Taken here
  -- because this is the one place that can name the boundary (`reverseIllegal`).
  entry <- State.get
  let -- What the pool would leave if the cost were paid out of it right now.
      --
      -- CR 609.4b's clauses are resolved from the board on EVERY pass rather than
      -- captured at entry: they are a CR 613.11 continuous effect and not a
      -- permission the cast carried in, so a Celestial Dawn that leaves
      -- mid-payment stops applying (CR 604.2) -- the opposite of `spending`, which
      -- rule 118.14 fixes when the cast was permitted.
      settlement gs = Mana.spend (PlayerEffect.spendManaAsThoughFor pid subject gs) spending (Maybe.fromMaybe 0 (Mana.lifeNeeded subject (midPayment (manaActivationsGiven (PlayerEffect.applying pid gs))) spending pid cost gs)) cost (Mana.Type.MkMana (fst (Mana.spendableFor subject pid gs)))
      -- `activated` is the mana abilities this window has run, in the order
      -- they finished, with what each wrote -- CR 733.1's "any legal mana
      -- abilities that player activated", gathered because that rule offers
      -- each of them back.
      window refused activated = do
        gs <- State.get
        let covered = Maybe.isJust (settlement gs)
            -- One projection per pass, shared by the enumeration and the
            -- interchangeability test rather than computed twice: Mana.manaSources
            -- is this same call.
            pcs = Projection.projectAll gs
            -- CR 605.3a offers every source, and this window narrows it by CR
            -- 605.3c alone: the ABILITY mid-activation is off its own window and
            -- off every window nested inside it, while the permanent's other mana
            -- abilities stay on. Applied inside Mana.manaSourcesGiven, beside the
            -- CR 118.3 gate it cannot be asked apart from: a permanent is still a
            -- source when some route of it is both payable and not in flight.
            --
            -- The capacity is taken on the board of the PASS rather than once for
            -- the payment: a tap changes the board, and
            -- Pawl.Engine.PlayerEffect.applying is a function of it. `pid` is the
            -- payer, and CR 113.8 makes the payer the controller of every mana
            -- ability activated here, so the capacity's own `pid` is this one.
            windowCapacity = midPayment (manaActivationsGiven (PlayerEffect.applying pid gs))
            --
            -- The routes this window saw REFUSED join the in-flight ones for the
            -- offer alone: a permanent stays on while some other route of it is
            -- open, and `refused` is not handed down to a nested window.
            offered = Mana.manaSourcesGiven (Set.union inFlight refused) windowCapacity (Projection.controlGrants gs) pcs pid gs
        case offered of
          [] -> settle activated
          candidate : rest -> do
            answer <- chooseSource covered pid (Interchangeable.representatives pcs gs (candidate NonEmpty.:| rest)) gs
            case answer of
              Nothing -> settle activated
              Just oid -> do
                start <- State.get
                (produced, nested, nestedSpent, failed) <- tapForManaWith perform midPayment inFlight refused pid oid
                end <- State.get
                -- An activation that FAILED reversed itself already (payActivation
                -- below), so it is not one of rule 733.1's to offer back -- but
                -- the activations its own nested window ran and the payer KEPT
                -- are, since they stand in this window. One that PAID is offered
                -- back beside the nested activations that paid for it: CR 733.1
                -- offers "any legal mana abilities", and a nested one is one.
                window (Set.union failed refused) (spliceActivations activated (if produced then withParent pid oid start end nested nestedSpent else nested))
      -- CR 601.2h: the window is closed, so the cost is paid out of what is there
      -- -- and simply is not paid when the player floated too little.
      --
      -- WHICH mana goes is the payer's (Mana.spendChosen), so this asks rather
      -- than reading `settlement`'s assignment: that one answers only whether the
      -- pool pays.
      settle :: ([ManaActivation.ManaActivation], [ManaSegment.ManaSegment]) -> Game (Bool, [(Keyword.Substitute, Natural)], ManaWindow.ManaWindow)
      settle (activated, segments) = do
        -- The state the window CLOSED on, which is the one a payer who declines
        -- to reverse their mana abilities goes back to: this is after every
        -- activation and before a symbol of the cost has been paid out of the
        -- pool. Taken AHEAD of `substituting`, since CR 702.132a's assisting
        -- player pays there and CR 733.1 cancels that payment too.
        closed <- State.get
        -- CR 601.2g then the reminder text: the substitutes are offered HERE,
        -- with the window shut and nothing spent, so the payer answers knowing
        -- what their mana abilities produced.
        (residual, extra) <- substituting cost
        gs <- State.get
        -- The window's own facts, which CR 733.1's reversal (`reverseIllegal`)
        -- takes beside the state the caller's action began in. Built here and
        -- not in the caller because this is the one place that holds them.
        --
        -- CR 106.6: the payment sees only the mana it may spend, and the rest of
        -- the pool goes back beside what it leaves (CR 106.4 -- unspent mana stays
        -- unspent, it does not vanish because one cost could not use it).
        let shut = ManaWindow.MkManaWindow {ManaWindow.payer = pid, ManaWindow.activated = activated, ManaWindow.segments = segments, ManaWindow.spent = [], ManaWindow.opened = entry, ManaWindow.closed = closed}
            (available, withheld) = Mana.spendableFor subject pid gs
        case Mana.plan (PlayerEffect.spendManaAsThoughFor pid subject gs) spending (Maybe.fromMaybe 0 (Mana.lifeNeeded subject (midPayment (manaActivationsGiven (PlayerEffect.applying pid gs))) spending pid residual gs)) residual (Mana.Type.MkMana available) of
          Nothing -> pure (False, extra, shut)
          Just (steps, life) -> do
            (Mana.Type.MkMana left, spent) <- Mana.spendChosen pid (PlayerEffect.spendManaAsThoughFor pid subject gs) steps (Mana.Type.MkMana available)
            -- Three writes in the order the one composed `State.modify'` they
            -- replace applied them in: the pool goes back, then the life is paid,
            -- then CR 400.7d's record of what was spent. Event.payLife is monadic
            -- because CR 119.4's loss goes through the replacement funnel.
            State.modify' (Mana.setPool pid (Mana.Type.MkMana (withheld <> left)))
            Event.payLife pid life
            State.modify' (recordSpent spent)
            pure (True, extra, shut {ManaWindow.spent = Mana.Type.unwrap spent})
      -- CR 400.7d's cost record for the MANA, kept where CR 107.4h's third
      -- sentence can be asked about it afterwards -- "the {S} symbol can also be
      -- used to refer to mana of any type produced by a snow source spent to pay a
      -- cost". Berg Strider and Forsworn Paladin are the readers.
      --
      -- `record` is the object it goes on, and it is the CALLER's rather than the
      -- subject's: CR 601.2h's payer names the spell (Pawl.Engine.Cast), and CR
      -- 602.2b's names the CR 602.2a ability object on the stack rather than the
      -- source permanent PaymentSubject.Activating carries (Pawl.Engine.Activate).
      -- Writing an activation's units onto the source would clobber the record of
      -- the mana that cast it, which is the one CR 400.7d is about.
      --
      -- Nothing for the payments with no object to name: a special action's cost, a
      -- combat toll, CR 118.12's resolution-time payment, and a mana ability's own
      -- cost, which CR 605.3b keeps off the stack entirely.
      --
      -- Written HERE rather than by the caller because this is where the caster's
      -- units are known (an assisting player's are `payAssist`'s). An unpaid cost
      -- writes nothing: `payMana` restores the state it entered with, and this
      -- line is only reached once the payment has settled.
      recordSpent spent gs = maybe gs (\sid -> recordPayment sid spent gs) record
  window Set.empty noActivations

-- CR 400.7d's record of mana spent on `sid`, ADDED to what is there: CR
-- 702.132a's assisting player pays ahead of the caster (`payAssist`), and both
-- payments are "what mana was spent to pay those costs". The record starts
-- empty, since CR 400.7 makes the spell a new object.
--
-- CR 106.6a's eagerly created effects go up beside the record and on the same
-- state, since this is where the units spent are known:
-- ManaRider.granted mints one continuous effect per unit whose rider the
-- paid-for object matches (Generator Servant), and armSpendTriggers arms each
-- unit's spend trigger (Pyromancer's Goggles). AFTER the record, so that the
-- condition is matched on the board CR 400.7d has already described -- a rider
-- clause reading the payment would otherwise see none.
recordPayment :: ObjectId -> Mana.Type.Mana -> GameState -> GameState
recordPayment sid spent gs =
  let add o = o {Object.manaSpent = Mana.Type.MkMana (Mana.Type.unwrap (Object.manaSpent o) <> Mana.Type.unwrap spent)}
      recorded = gs {GameState.objects = Map.adjust add sid (GameState.objects gs)}
   in armSpendTriggers sid spent (ManaRider.granted sid spent recorded)

-- CR 106.6 / 603.7a: each spent unit's delayed ability triggers when the unit
-- pays for casting a spell its filter matches -- one per unit (CR 106.6a), with
-- "that spell" bound as Binding.castSpell. Armed as a reflexive entry, whose
-- existence is the trigger (Pawl.Engine.Event.Trigger.isReflexive), so it goes
-- on the stack the next time a player would receive priority (CR 603.3).
--
-- The entry's creation moment is minted here rather than at production. Only
-- CR 701.27f reads it, and no spend trigger in data/cards/ transforms anything.
--
-- Game.isSpell is CR 601.2h's "to cast": an activation's payment names an
-- ability object, and nothing else is paid for with an object at all.
armSpendTriggers :: ObjectId -> Mana.Type.Mana -> GameState -> GameState
armSpendTriggers sid spent gs =
  let fires trigger = Game.isSpell sid gs && ManaRider.spentOnMatches sid gs (SpendTrigger.casts trigger)
      arm g trigger =
        Event.armDelayed
          (SpendTrigger.ability trigger)
          (SpendTrigger.source trigger)
          (SpendTrigger.controller trigger)
          (Map.singleton Binding.castSpell (Binding.toObject sid))
          Onset.Immediately
          Nothing
          g
   in List.foldl' arm gs (filter fires (Maybe.mapMaybe ManaUnit.spendTrigger (Mana.Type.unwrap spent)))

payMana :: ManaAbilityPerformer.ManaAbilityPerformer -> PaymentSubject.PaymentSubject -> ManaSpending.ManaSpending -> PlayerId -> ManaCost.ManaCost -> Game Bool
payMana perform = payManaExcept perform Set.empty Nothing

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

-- CR 107.4b: how much GENERIC mana a cost states -- the amount CR 702.132a's
-- chosen player may pay any part of. Only the generic symbol: CR 107.4e's {2/B}
-- is one symbol two mana pay rather than a generic component, and rule 702.132a
-- names the component.
genericMana :: Cost Keyword.Type.Keyword -> Natural
genericMana cost = sum (fmap genericOf (foldMap ManaCost.unwrap (Cost.mana cost)))

-- genericMana's per-symbol half, enumerated rather than defaulted so that a new
-- CR 107.4 symbol has to say whether it is generic.
genericOf :: ManaSymbol.ManaSymbol -> Natural
genericOf symbol = case symbol of
  ManaSymbol.Generic n -> n
  ManaSymbol.OfType _ -> 0
  ManaSymbol.Hybrid _ -> 0
  ManaSymbol.MonocoloredHybrid _ -> 0
  ManaSymbol.Phyrexian _ -> 0
  ManaSymbol.HybridPhyrexian _ -> 0
  -- CR 107.4h's {S} demands mana from a snow source, which is a fact about
  -- where the mana came from rather than a generic component.
  ManaSymbol.Snow -> 0
  -- CR 107.3: {X} is already the announced value by the time a total cost is
  -- read (`substituteX`), so this arm is the UNannounced symbol and counts for
  -- nothing rather than guessing one.
  ManaSymbol.Variable -> 0

-- CR 702.132a's first three sentences: before the caster activates mana
-- abilities they may choose another player, and that player then has a chance to
-- activate mana abilities of their own. Answers WHO was chosen; `payAssist`
-- below is the rule's last sentence.
--
-- Asked only where the total cost holds generic mana, which is rule 702.132a's
-- own condition -- a cost with none leaves the chosen player nothing to pay and
-- the rule offers no choice.
--
-- The candidates are every OTHER player still in the game (CR 104.2a), not the
-- caster's opponents: rule 702.132a says "another player", and a teammate is one
-- (CR 102.3).
--
-- NEVER ELIDED for a lone candidate, `chooseSource`'s posture: declining is an
-- answer on every board, and a player who is asked gets a mana window the
-- caster's board can see. FILTERED, NOT TRUSTED for the same reason -- an
-- unrecognised answer reads as choosing nobody, since the fallback must not
-- draft a player into the cast.
--
-- The chosen player's window is `payManaWindow` over an EMPTY cost: rule 702.132a
-- gives them a chance to activate mana abilities and nothing yet to pay, so the
-- loop is the ordinary CR 605.3a one and its settlement spends no symbol. It runs
-- HERE, ahead of the caster's, which is the order rule 702.132a states. The
-- window comes back beside the player, since CR 733.1 lets the helper keep
-- what they activated in it if the cast is reversed (`reverseIllegal`).
offerAssist :: ManaAbilityPerformer.ManaAbilityPerformer -> PaymentSubject.PaymentSubject -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game (Maybe (PlayerId, ManaWindow.ManaWindow))
offerAssist perform subject pid sid cost = do
  gs <- State.get
  if genericMana cost == 0 || not (Map.member Keyword.Type.Assist (spellKeywords pid sid gs))
    then pure Nothing
    else case NonEmpty.nonEmpty (filter (/= pid) (Game.stillPlaying gs)) of
      Nothing -> pure Nothing
      Just candidates -> do
        answer <- Game.choose (Prompt.ChooseAssistant (Decide.deciderFor pid gs) pid sid candidates)
        let chosen = case answer of
              Just helper | List.elem helper (NonEmpty.toList candidates) -> Just helper
              _ -> Nothing
        Monad.forM
          chosen
          ( \helper -> do
              (_, _, window) <- payManaWindow perform Set.empty Nothing subject ManaSpending.AsProduced helper (\mc -> pure (mc, [])) (ManaCost.MkManaCost [])
              pure (helper, window)
          )

-- CR 702.132a at the castability gate: a totalled mana cost less the generic
-- mana the best-placed other player could pay of it. CR 601.2 lets a player
-- propose a cast whatever they can pay, so a gate on payability has to count
-- what the chosen player COULD add; whether they do is theirs to answer at
-- `payAssist`, and a cast they decline to help fails CR 601.2h and unwinds
-- (CR 733.1).
--
-- The HELPER's supply is measured apart from the caster's, which is exact: each
-- pays from their own pool and the mana abilities of what they control, so
-- neither payment can spend what the other's needs, and paying more of the
-- generic only makes the caster's residual easier.
assistable :: PaymentSubject.PaymentSubject -> PlayerId -> ObjectId -> GameState -> ManaCost.ManaCost -> ManaCost.ManaCost
assistable subject pid oid gs manaCost
  | not (Map.member Keyword.Type.Assist (spellKeywords pid oid gs)) = manaCost
  | otherwise =
      let generic = sum (fmap genericOf (ManaCost.unwrap manaCost))
          pays helper n = canPaySomeCompletion Map.empty subject ManaSpending.AsProduced helper oid pure (\mc -> [(mc, [])]) (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []) gs
          capacity helper = Natural.length (takeWhile (pays helper) [1 .. generic])
          best = maximum (0 : fmap capacity (filter (/= pid) (Game.stillPlaying gs)))
       in withoutMana (ManaSymbol.Generic 0) best manaCost

-- CR 702.132a's last sentence: before the caster begins to pay the total cost,
-- the player they chose may pay for any amount of the generic mana in it.
-- Answers the cost that is LEFT for the caster.
--
-- Run at the seam rule 702.132a names and CR 601.2g fixes -- both windows shut,
-- no symbol of the cost spent -- which is where `paySubstituting` already runs CR
-- 702.51b's substitution offer, so this rides that hook
-- (Pawl.Engine.Cast.castProposed) rather than opening a seam of its own.
--
-- The BOUND offered is what their pool actually pays, walked up from one rather
-- than named as a ceiling: CR 118.3 governs what a player may pay, and a generic
-- demand any unit serves makes the payable amounts a prefix.
--
-- CLAMPED, not rejected -- Prompt.ChoosePaidEnergy's posture: rule 702.132a
-- states an amount a player may pay rather than an announcement the cast rests
-- on, so an answer past the bound is enforced down to it.
--
-- CR 118.14's permission is the CASTER's and is not extended here: rule 118.14
-- scopes it to "mana that player spends to cast spells that way", and the
-- assisting player is not casting. CR 609.4b's per-player half IS theirs, so
-- `spendManaAsThough` is read off them.
--
-- The units spent go on CR 400.7d's record of the spell (`recordPayment`), so
-- CR 106.6a's rider on the helper's mana matches the spell it paid for --
-- Pawl.CostSpec's "CR 106.6a the Binox bob's Generator Servant helped pay for
-- attacks the turn it arrives" is the proof.
payAssist :: PlayerId -> PaymentSubject.PaymentSubject -> ObjectId -> Cost Keyword.Type.Keyword -> Game (Cost Keyword.Type.Keyword)
payAssist helper subject sid cost = case Cost.mana cost of
  Nothing -> pure cost
  Just manaCost -> do
    gs <- State.get
    let asThough = PlayerEffect.spendManaAsThough helper gs
        (available, withheld) = Mana.spendableFor subject helper gs
        generic amount = ManaCost.MkManaCost [ManaSymbol.Generic amount]
        planFor amount = Mana.plan asThough ManaSpending.AsProduced 0 (generic amount) (Mana.Type.MkMana available)
        bound = Natural.length (takeWhile (Maybe.isJust . planFor) [1 .. genericMana cost])
    if bound == 0
      then pure cost
      else do
        answer <- Game.choose (Prompt.ChooseAssistAmount (Decide.deciderFor helper gs) helper sid bound)
        let amount = min answer bound
        case planFor amount of
          Nothing -> pure cost
          Just (steps, _) -> do
            (Mana.Type.MkMana left, spent) <- Mana.spendChosen helper asThough steps (Mana.Type.MkMana available)
            State.modify' (Mana.setPool helper (Mana.Type.MkMana (withheld <> left)))
            State.modify' (recordPayment sid spent)
            pure cost {Cost.mana = Just (withoutMana (ManaSymbol.Generic 0) amount manaCost)}

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
-- The ability's NON-MANA clauses run too (CR 405.6c), through the injected
-- Pawl.Types.ManaAbilityPerformer rather than a call into Pawl.Engine.Resolve,
-- which sits above this module.
--
-- `pid` is the player activating it (CR 602.1a), who need not control the
-- permanent where the route says any player may (CR 602.1b).
tapForMana :: ManaAbilityPerformer.ManaAbilityPerformer -> PlayerId -> ObjectId -> Game Bool
tapForMana perform pid oid = fmap (\(produced, _, _, _) -> produced) (tapForManaWith perform id Set.empty Set.empty pid oid)

-- The same activation carrying the abilities already mid-activation (CR
-- 605.3c), which is payManaExcept's one narrowing: the route CHOSEN here joins
-- the set before its cost opens a window of its own, so the recursion cannot
-- revisit that ability -- and the options are filtered by the set too, since a
-- nested window reaching this permanent must not re-offer it. Added here rather
-- than in `payActivation`, which is the only caller and cannot see which route
-- was chosen. CR 605.3a's priority window starts from the empty set, nothing
-- being in flight there.
--
-- `window` narrows the capacity to the window the activation is made in:
-- `midPayment` inside a payment, the identity at priority (`tapForMana`).
--
-- `refused` is payManaWindow's routes already declined on this window, kept off
-- the choice here as on the offer there; the answer's last part is the route
-- this activation adds to them, empty when it paid. Its second and third are
-- what CR 733.1 reads of the activation's own cost: the nested activations
-- that stand (`payActivation`) and the mana it took from the pool.
tapForManaWith :: ManaAbilityPerformer.ManaAbilityPerformer -> (Mana.Capacity -> Mana.Capacity) -> Mana.InFlight -> Mana.InFlight -> PlayerId -> ObjectId -> Game (Bool, ([ManaActivation.ManaActivation], [ManaSegment.ManaSegment]), [ManaUnit.ManaUnit], Mana.InFlight)
tapForManaWith perform window inFlight refused activator oid = do
  gs <- State.get
  -- Every route of this permanent, off the offer's own list
  -- (Mana.manaRoutesOfGiven), which is what a source with no route left to
  -- choose answers as refused. A FENCE: the offer and the filter below agree,
  -- so this only keeps payManaWindow's loop finite should they ever not.
  let everyRoute = Set.fromList (fmap (\(_, _, ability, _) -> (oid, ability)) (Mana.manaRoutesOfGiven Map.empty oid gs))
  case Game.lookupObject oid gs of
    Nothing -> pure (False, noActivations, [], everyRoute)
    Just _ -> do
      -- CR 109.4a/113.8: the mana ability's controller is the player
      -- activating it -- the permanent's controller, or anyone at all for a
      -- route printing "any player may activate this ability" (CR 602.1b,
      -- Mana.permitsRoute). That player makes the colour choice and pays the
      -- cost (CR 602.1a).
      --
      -- Not whose POOL the mana lands in, which each AddMana's own reference
      -- names (CR 106.4) and CR 109.5 makes this player only by default:
      -- Yurlok of Scorch Thrash's "Each player adds {B}{R}{G}" fills the whole
      -- table's. See the Payment.Paid branch below.
      let controller = activator
          permitted = Mana.permitsRoute (Projection.controllerOf oid gs == Just activator) . ManaOption.ability
          -- ONE gather of the player's effects for every option this permanent
          -- offers, rather than one per option: `manaActivations` would take its
          -- own, and that walk is the shape #1073 was about.
          capacity = window (manaActivationsGiven (PlayerEffect.applying controller gs))
      case filter (\option -> permitted option && not (Mana.inFlightRoute (Set.union inFlight refused) oid (ManaOption.ability option)) && Activations.times (capacity Mana.ForOffer Map.empty controller oid (ManaOption.cost option) (ManaOption.restrictions option) (ManaOption.ability option) gs) > 0) (Mana.manaOptionsOf oid gs) of
        [] -> pure (False, noActivations, [], everyRoute)
        first : rest -> do
          -- CR 106.3 / 608.2d: the activator picks the route and the mana for
          -- their OWN pool (@Relative You@). A share under any other reference
          -- is its recipient's to pick as it is added (`pickShare` below), so routes differing only
          -- there are one choice here -- Spectral Searchlight's five colours
          -- are one route until "that player" is known.
          let ownPart option = option {ManaOption.yield = Map.filterWithKey (\ref _ -> ref == you) (ManaOption.yield option), ManaOption.steps = fmap (fmap (Map.filterWithKey (\ref _ -> ref == you))) (ManaOption.steps option)}
              you = PlayerRef.Relative PlayerRelation.You
          chosen <- case ListUtils.nubOrdOn ownPart (first : rest) of
            route : routes -> chooseManaYield controller oid (route NonEmpty.:| routes) gs
            [] -> pure first
          let alike = filter (\option -> ownPart option == ownPart chosen) (first : rest)
          -- CR 601.2f, reached by CR 602.2b: what is paid is the TOTAL, and the
          -- gate above measured that same total through `capacity`. Off ONE
          -- gather (`manaActivationAdjustments`), so the offer and the payment
          -- cannot read different reducers -- Cast.castSpell's and
          -- Activate.activateAbility's arrangement, on the path CR 605.3b leaves
          -- them no stack window in.
          --
          -- `gs` is still current: chooseManaYield only prompts.
          let gathered = manaActivationAdjustments (ActivatedAbility.keyword =<< ManaOption.ability chosen) controller oid gs
              withComponents = plusComponents gathered (ManaOption.cost chosen)
          -- CR 118.13a: a mana ability is an activated ability (CR 605.1a) and
          -- CR 602.2b sends its activation cost through CR 601.2b, so a symbol
          -- payable in several ways is announced as the ability is activated
          -- rather than left to the payment. BEFORE announceReductions, which is
          -- CR 601.2f -- Cast.castSpell's and Activate.activateAbility's order,
          -- and rule 601.2b's own. Mystic Gate's "{W/U}, {T}" is what observes it
          -- (Pawl.ManaSpec's Mystic Gate group).
          --
          -- The Phyrexian life record is DISCARDED, Activate's reason: rule
          -- 702.150a reads what the player who CAST a spell announced.
          (announcedCost, _) <- announce (PaymentSubject.Activating oid Nothing Nothing) ManaSpending.AsProduced controller oid (totalManas gathered) withComponents
          -- CR 118.7e's half of each hybrid symbol in a reduction, and CR
          -- 601.2f's order of several reductions -- both the payer's, and both
          -- elided by announceReductions wherever the answers cannot differ,
          -- which is every board `data/cards/` can build today.
          announced <- announceReductions controller oid gs announcedCost gathered
          (outcome, nested, nestedSpent) <- payActivation perform (Set.insert (oid, ManaOption.ability chosen) inFlight) controller oid (totalWith announced announcedCost)
          case outcome of
            Payment.Unpaid -> pure (False, nested, [], Set.singleton (oid, ManaOption.ability chosen))
            Payment.Paid paid -> do
              -- CR 601.2h / 608.2h: the yield is priced again with the slots
              -- the payment bound, since an offer cannot know which artifact
              -- Priest of Yawgmoth's cost will sacrifice (Pawl.ManaSpec's Priest
              -- of Yawgmoth group). Every pair whose offer the activator picked
              -- among is a candidate, so a colour choice the offer collapsed is
              -- asked now -- Food Chain's colour (Pawl.ManaSpec's Food Chain
              -- group) -- and a yield the slots leave unchanged asks nothing.
              --
              -- Not implemented: the slots reaching CR 405.6c's other effects,
              -- which run with none of them -- see `perform` below (#4850).
              gsPaid <- State.get
              let pricedAlike = [priced | (offered, priced) <- Mana.manaRepricingsGiven paid Map.empty oid gs, List.elem offered alike]
              pricedChosen <- case ListUtils.nubOrdOn ownPart pricedAlike of
                route : routes -> chooseManaYield controller oid (route NonEmpty.:| routes) gsPaid
                [] -> pure chosen
              let pricedShares = filter (\option -> ownPart option == ownPart pricedChosen) pricedAlike
              -- CR 608.2c: the clauses in printed order, each decided as it is
              -- reached, on the board the cost and the earlier clauses left --
              -- Hickory Woodlot's "if there are no depletion counters" reads the
              -- counter its own cost removed (Pawl.ManaSpec's Hickory Woodlot
              -- group). The source stands in for the ability object CR 605.3b
              -- leaves uncreated, and CR 608.2h's last-known information answers
              -- for a source the cost sacrificed.
              --
              -- A clause that happens adds its share of the yield, then runs the
              -- other effects printed after it -- CR 405.6c's "the mana is
              -- produced and the other effect happens immediately", HERE, inside
              -- the window this activation was made in. Ancient Tomb's 2 damage
              -- is charged before the rest of the payment can spend the mana it
              -- just made (Pawl.ManaSpec's Ancient Tomb group). The performer runs them;
              -- Pawl.Engine.Resolve.Effect.performManaAbility is where the source
              -- stands in for the ability object.
              --
              -- CR 106.4: each share goes to the players its own reference
              -- names, resolved through Mana.recipientsOf -- the same function
              -- Mana.manaSuppliesGiven keeps the payer's share by, so the offer
              -- and the payment cannot disagree about whose pool a route fills.
              -- Yurlok of Scorch Thrash is the printing that observes it, and
              -- Pawl.ManaSpec's Yurlok group is what proves it. ORDER across
              -- recipients is unobservable: a pool is a multiset
              -- (Pawl.Types.Mana) and CR 101.4's ordering rule is about CHOICES,
              -- of which the addition itself makes none.
              --
              -- A clause offers mana only where its "if" held at the OFFER, so one
              -- whose "if" only the cost makes true adds none. MTGJSON's dump of
              -- 2026-08-23 prints no such clause (mana-ability lines matching
              -- "Add ... . If ... add", every hit an "instead" whose "if" no cost
              -- of its own touches); a land whose cost removes the counter its
              -- "instead" reads would refute this.
              --
              -- CR 118.12 after the "if", Pawl.Types.Clause's printed order:
              -- Rhystic Cave's "unless any player pays {1}" is offered to the
              -- table by the performer, the source standing in for the ability
              -- object here too, and a clause whose gate says no adds no mana.
              --
              -- CR 608.2c / 608.2d inside a clause: the effects printed BEFORE its
              -- first addition run first, and the slots they bind are what the
              -- addition's recipient reads -- Valleymaker's "Choose a player.
              -- That player adds {G}{G}{G}" (Pawl.ManaSpec's Valleymaker group).
              -- The slots are threaded through the performer, CR 605.3b leaving
              -- no ability object to hold them. A share under any reference but
              -- @Relative You@ is then picked by each RECIPIENT among the routes
              -- alike in the activator's own part: Spectral Searchlight's "any
              -- color they choose" (Pawl.ManaSpec's Spectral Searchlight group).
              let shareAt i ref option = Map.lookup ref . snd =<< Maybe.listToMaybe (drop i (ManaOption.steps option))
                  pickShare i ref recipient mana = case ListUtils.nubOrdOn (shareAt i ref) pricedShares of
                    representative : more@(_ : _) | ref /= you -> do
                      gsNow <- State.get
                      picked <- chooseManaYield recipient oid (representative NonEmpty.:| more) gsNow
                      pure (maybe (Mana.unitsOf mana) Mana.unitsOf (shareAt i ref picked))
                    _ -> pure (Mana.unitsOf mana)
                  happens clause = do
                    gsNow <- State.get
                    let context = Filter.contextFor (Game.teams gsNow) (Just controller) (Just oid)
                    pure (maybe True (Condition.holds (Projection.viewWithLastKnownAnywhere gsNow) context gsNow oid) (Clause.condition clause))
                  gated cIdx answers clause = case Clause.payGate clause of
                    Nothing -> pure (True, answers)
                    Just gate -> ManaAbilityPerformer.payGate perform oid controller cIdx gate answers
                  step (answers, bound, filled, made) (cIdx, (i, (clause, share))) = do
                    holds <- happens clause
                    (admitted, answers2) <- if holds then gated cIdx answers clause else pure (False, answers)
                    if not admitted
                      then pure (answers2, bound, filled, made)
                      else do
                        let (leading, trailing) = List.break (Maybe.isJust . ManaAbility.manaProduced) (Foldable.toList (Clause.effects clause))
                        bound1 <- ManaAbilityPerformer.effects perform oid controller bound leading
                        gsNow <- State.get
                        let chosenPlayers = Map.filter (not . Set.null) (fmap (Set.fromList . Maybe.mapMaybe Recipient.playerOf . Set.toList) bound1)
                        parts <- traverse (\(ref, mana) -> traverse (\recipient -> fmap ((,) recipient) (pickShare i ref recipient mana)) (Mana.recipientsOf controller chosenPlayers gsNow ref)) (Map.toList share)
                        let shares = concat parts
                            -- CR 106.12a's "produced", once per addition whoever's
                            -- pool it reached: the first recipient's pick, or the
                            -- offered share where it reached nobody.
                            produced = concat (zipWith (\(_, mana) part -> maybe (Mana.unitsOf mana) snd (Maybe.listToMaybe part)) (Map.toList share) parts)
                        -- CR 603.7e: a spend trigger is the ACTIVATOR's, which
                        -- Mana.manaOptionsOfGiven could only guess as the
                        -- permanent's controller. A fence: no "any player may
                        -- activate" route in data/cards/ prints one.
                        let activated = fmap (fmap (fmap (\u -> u {ManaUnit.spendTrigger = fmap (\t -> t {SpendTrigger.controller = controller}) (ManaUnit.spendTrigger u)}))) shares
                        State.modify' (\g -> List.foldl' (\acc (recipient, units) -> Mana.addMana recipient units acc) g activated)
                        bound2 <- ManaAbilityPerformer.effects perform oid controller bound1 (filter (Maybe.isNothing . ManaAbility.manaProduced) trailing)
                        pure (answers2, bound2, filled <> shares, made <> produced)
              (_, _, shares, producedUnits) <- Monad.foldM step (Map.empty, Map.empty, [], []) (zip (fmap ClauseIndex.MkClauseIndex [0 ..]) (zip [0 :: Int ..] (ManaOption.steps pricedChosen)))
              -- CR 605.1b's "mana being added to a player's mana pool", one event
              -- per player whose pool this activation filled, and CR 106.12a's
              -- "produced": what the clauses that happened added, whoever's pool.
              let added = Map.fromListWith Set.union [(recipient, Set.fromList (fmap ManaUnit.manaType units)) | (recipient, units) <- shares, not (null units)]
              -- CR 602.5b: record that THIS ability of this permanent has now
              -- been activated, which is the whole of both counted riders'
              -- storage (`capacity` above is where they are read on this path).
              -- CR 605.3b leaves a mana ability no stack object, so this is the
              -- activation and Activate.activateAbility never sees it -- and
              -- ActivationRestriction.recordActivation is the writer the two
              -- roads share, so they cannot disagree about what was spent.
              --
              -- Only after Payment.Paid, so a route refused mid-payment leaves no
              -- record. CR 305.6's intrinsic route has no ability and prints no
              -- rider, so it can never reach this.
              case ManaOption.ability chosen of
                Just spent -> State.modify' (ActivationRestriction.recordActivation oid spent)
                Nothing -> pure ()
              -- CR 602.2's history, off `gs`, the board the activation began on:
              -- a Treasure its own cost sacrificed is still logged as what it was.
              -- The intrinsic route is logged too; CR 305.6 makes it an activated
              -- ability all the same.
              State.modify' (ActivationRestriction.logActivation gs controller oid (ActivatedAbility.keyword =<< ManaOption.ability chosen) AbilityKind.ManaAbility Set.empty)
              -- CR 106.12: this activation "tapped [the permanent] for mana"
              -- exactly when {T} was in its cost and it produced mana, which is
              -- what CR 106.12a's condition watches. Recorded LAST, after CR
              -- 405.6c's other effects, because CR 605.4a puts what it triggers
              -- immediately after the whole mana ability.
              --
              -- The events, not the board: a permanent tapped by {T} is also
              -- tapped by Icy Manipulator, and Pawl.Engine.Event.tap has already
              -- written GameEvent.BecameTapped for both.
              -- Every unit produced and not the payer's share: CR 106.12a asks
              -- whether the activation PRODUCED mana, which it did whoever's
              -- pool it went to.
              let tappedForMana =
                    [ GameEvent.TappedForMana (TappedForMana.MkTappedForMana {TappedForMana.permanent = oid, TappedForMana.mana = Set.fromList (fmap ManaUnit.manaType producedUnits)})
                    | List.elem CostComponent.TapThis (Cost.components (ManaOption.cost chosen)) && not (null producedUnits)
                    ]
                  -- CR 605.1b's other event, whether or not {T} was paid, and
                  -- recorded at the same moment for the same CR 605.4a reason.
                  manaAdded = fmap (\(recipient, types) -> GameEvent.ManaAdded (ManaAdded.MkManaAdded {ManaAdded.player = recipient, ManaAdded.source = oid, ManaAdded.mana = types, ManaAdded.cause = ManaAddedCause.ManaAbility})) (Map.toList added)
                  -- CR 605.3b's own moment: the ability "resolves immediately
                  -- after it is activated", and this line is where that has just
                  -- finished happening. UNCONDITIONAL where the tap event is
                  -- gated on {T} and on a yield -- CR 605.2 keeps an ability that
                  -- produced nothing a mana ability, and it resolved either way.
                  --
                  -- Every unit produced and not the payer's share, TappedForMana's
                  -- reason: "the amount of mana this creature produced" (Tyvar
                  -- the Bellicose) asks what the permanent made, whoever's pool
                  -- CR 106.4 sent it to.
                  manaAbilityResolved = GameEvent.ManaAbilityResolved (ManaAbilityResolved.MkManaAbilityResolved {ManaAbilityResolved.permanent = oid, ManaAbilityResolved.amount = Natural.length producedUnits})
              applyManaTriggers perform (tappedForMana <> manaAdded <> [manaAbilityResolved])
              pure (True, nested, nestedSpent, Set.empty)

-- CR 605.4a: record the events one activated mana ability wrote -- CR 106.12a's
-- tap for mana, CR 605.1b's mana being added and CR 605.3b's own resolution --
-- and apply, where they stand,
-- the triggered mana abilities they fired: "a triggered mana ability doesn't go
-- on the stack ... it resolves immediately after the mana ability that
-- triggered it, without waiting for priority".
--
-- Each event carries the mana TYPES it is about, which the "of a specified
-- type" narrowings read -- Gauntlet of Power's "for mana of the chosen color",
-- Caged Sun's "one or more mana of the chosen color". Recorded on the event
-- because nothing else can answer for it afterwards: the mana is a
-- Pawl.Types.ManaUnit in a pool carrying no reference to its source.
--
-- The events are recorded whatever they fire, since an ordinary triggered ability
-- watching the same moment is CR 603.3's business and reaches the stack through
-- Pawl.Engine.Engine.placePendingTriggers like any other. What this consumes is
-- only the CR 605.1b subset, and that same predicate is what
-- placePendingTriggers uses to refuse them a stack object -- one classifier, two
-- readers, so an ability cannot both resolve here and be placed there.
--
-- Gathered against the events just recorded rather than everything unscanned:
-- a payment taps several permanents in turn (payManaWindow), and the earlier
-- taps' triggers have already been applied here. The watermark is deliberately
-- NOT moved -- it belongs to the CR 117.5 scan, and moving it would swallow the
-- ordinary triggers these same events owe that scan.
--
-- CR 603.4's intervening "if" is applied, Event.reactionTriggers doing it.
--
-- Not implemented: an ordering choice where ONE activation fires several
-- triggered mana abilities -- two Wild Growths enchanting one Forest, or a Caged
-- Sun beside a Gauntlet of Power. `fired` is applied in gather order,
-- engine-chosen. CR 605.4a keeps them off the stack, so CR 603.3b's process does
-- not literally run, but it is the rule that gives their controller the order,
-- and no prompt is raised. Sound only while every such ability's effect is
-- order-independent, which every AddMana into a pool is (#3724).
--
-- Not implemented: the printed "triggers only once" riders
-- (Pawl.Types.TriggerLimit), which Event.withinTriggerLimit spends for a trigger
-- that reaches the stack (#3724).
applyManaTriggers :: ManaAbilityPerformer.ManaAbilityPerformer -> [GameEvent.GameEvent] -> Game ()
applyManaTriggers perform events = do
  Monad.mapM_ (State.modify' . Event.recordEvent) events
  gs <- State.get
  let recorded = Foldable.toList (Seq.drop (Seq.length (GameState.events gs) - length events) (GameState.events gs))
      fired = filter (\p -> ManaAbility.isTriggeredManaAbility (PendingTrigger.firedBy p) (PendingTrigger.ability p)) (Event.reactionTriggers recorded gs)
  Monad.mapM_ (ManaAbilityPerformer.triggered perform) fired

-- CR 602.2b sends an activation cost through CR 601.2b-i, so a mana ability pays
-- its whole cost. All or nothing, `pay`'s posture and for CR 601.2h's reason,
-- and CR 733.1's question with it: the mana abilities a NESTED window activated
-- are the payer's to keep, and the outer window goes on with them. Nothing is
-- written between `before` and the nested window -- the announcement ahead of
-- this only prompts -- so no announcement is undone here.
--
-- MANA FIRST (CR 601.2g), then the components (CR 601.2h), which is what lets
-- Transmogrant Altar's "{B}, {T}, Sacrifice a creature" tap the one creature for
-- its {B} and then sacrifice that same creature. Paid the other way round the
-- sacrifice takes the only black source off the board before the window opens,
-- and the activation fails on a board the rules say it succeeds on.
--
-- The EMPTY mana part still short-circuits, and CR 601.2g is why as well as
-- speed: the window is conditioned on the total cost including a mana payment,
-- so a cost with none opens none. That is most of `data/cards/`, and this is on
-- the path of every tap for mana.
--
-- The recursion CR 602.2b makes of that window is bounded by the in-flight set
-- `tapForManaWith` added this route to (CR 605.3c), not by the order.
payActivation :: ManaAbilityPerformer.ManaAbilityPerformer -> Mana.InFlight -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game (Payment.Payment, ([ManaActivation.ManaActivation], [ManaSegment.ManaSegment]), [ManaUnit.ManaUnit])
payActivation perform inFlight pid oid cost = do
  before <- State.get
  (paid, windows) <- case Cost.mana cost of
    Just (ManaCost.MkManaCost []) -> pure (True, [])
    -- CR 118.14's permission is granted to CAST a spell and never to activate an
    -- ability, so an activation cost is paid with the mana it is. CR 106.6 is a
    -- different sentence and answers differently: this is CR 602.2b's payment,
    -- so mana restricted to activations may pay it -- Omen Hawker's {C} into
    -- Chromatic Star's {1} -- and mana restricted to casts may not. `oid` is the
    -- ability's SOURCE, which is what "abilities of artifacts" reads.
    Just manaCost -> do
      (windowPaid, _, window) <- payManaWindow perform inFlight Nothing (PaymentSubject.Activating oid Nothing Nothing) ManaSpending.AsProduced pid (\mc -> pure (mc, [])) manaCost
      pure (windowPaid, [window])
    -- CR 118.6: attempting to pay an unpayable cost is an illegal action.
    Nothing -> pure (False, [])
  -- CR 602.2b sends this through CR 601.2h, so the payment is made while the
  -- ability is being ACTIVATED and no resolution is behind it.
  --
  -- No slots, by CR 605.1a rather than by this caller's position: a mana ability
  -- "doesn't require a target", so its announcement binds nothing a criterion
  -- could read.
  outcome <- if paid then payComponents PaymentMoment.OutsideResolution Map.empty pid oid (Cost.components cost) else pure Payment.Unpaid
  let settled = case outcome of
        Payment.Paid _ -> paid
        Payment.Unpaid -> False
  standing <- if settled then pure (foldr (spliceActivations . \window -> (ManaWindow.activated window, ManaWindow.segments window)) noActivations windows) else reverseIllegal windows before
  -- `outcome` and not a fresh Paid: the components' bound slots are what the
  -- payment bound, the mana half binding none of its own. `standing` is the
  -- nested activations that stand -- every one of a paid window's, and what
  -- the payer chose to keep of a refused one's -- and the mana is what the
  -- payment took from the pool, which CR 733.1's "unless" clause reads.
  pure (if settled then outcome else Payment.Unpaid, standing, if settled then foldMap ManaWindow.spent windows else [])

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
-- already collapsed routes alike in cost, yield and ability.
--
-- FILTERED, NOT TRUSTED: honouring an option the source does not offer would
-- mint mana out of nothing, or charge the wrong cost for it.
--
-- `pid` is whoever picks: the activator for the route and their own share, and
-- the recipient for a share under any other reference (tapForManaWith's
-- `pickShare`).
chooseManaYield :: PlayerId -> ObjectId -> NonEmpty.NonEmpty ManaOption.ManaOption -> GameState -> Game ManaOption.ManaOption
chooseManaYield pid oid candidates gs = case candidates of
  only NonEmpty.:| [] -> pure only
  _ -> do
    answer <- Game.choose (Prompt.ChooseManaYield (Decide.deciderFor pid gs) pid oid candidates)
    pure $
      if List.elem answer (NonEmpty.toList candidates)
        then answer
        else NonEmpty.head candidates

-- CR 614.1c's record written from the other provenance: the player the payer
-- chose as a COST, rather than the one an as-enters replacement asks for. One
-- field for both because the rules make it one fact -- "the chosen player" is
-- what CR 702.174b's effect names -- and no card carries both writers.
stampChosenPlayer :: ObjectId -> PlayerId -> Game ()
stampChosenPlayer oid pid =
  State.modify' $ \gs ->
    let stamp object = object {Object.chosenPlayer = Just pid}
     in gs {GameState.objects = Map.adjust stamp oid (GameState.objects gs)}

-- CR 118.3 asked AGAIN as each component is paid, and not only at the gate: CR
-- 601.2g's window activates mana abilities before any part is paid and CR 601.2h
-- lets the payer order the parts, so either can spend what a later part needs --
-- Brittle Effigy's ExileThis ahead of its own {T}, Cadaverous Bloom exiling the
-- Trumpeting Carnosaur whose "discard this card" is still owed, an Aether Hub
-- spending the energy "Pay X {E}" counts. That makes the ORDER
-- unpayable rather than the cost, so `canPay` was right to allow it, and CR
-- 601.2h refuses the payment whole: Unpaid rather than a funnel's silent no-op
-- or floor, which `pay` turns into CR 733.1's reversal of the entire action.
-- Refusing an order is not choosing one for the player. Nothing narrows the
-- window's offer either: CR 605.3a lets a player activate any mana ability at a
-- payment, and refusing the payment afterwards is the rules' own answer.
--
-- ONE guard ahead of every arm, so the gate and the payment ask one question.
-- Pawl.CostSpec's Hanweir Battlements, Ashnod's Altar, Brittle Effigy and
-- Trumpeting Carnosaur groups and Pawl.ActivateSpec's "CR 601.2h energy an
-- Aether Hub spends mid-payment is not there for Pay X {E}" are the proofs.
payComponent :: PaymentMoment.PaymentMoment -> Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> Game Payment.Payment
payComponent moment slots pid oid component = do
  gs <- State.get
  if canPayComponent slots pid oid component gs
    then payPayable moment slots pid oid component
    else pure Payment.Unpaid

-- `payComponent`'s arms, each reached only once CR 118.3 holds.
payPayable :: PaymentMoment.PaymentMoment -> Map.Map SlotName.SlotName (Set.Set ObjectId) -> PlayerId -> ObjectId -> CostComponent.CostComponent Keyword.Type.Keyword -> Game Payment.Payment
payPayable moment slots pid oid component = case component of
  -- CR 701.26a through tapObject; CR 107.5's "already tapped" is `payComponent`'s
  -- guard.
  CostComponent.TapThis -> do
    tapObject oid
    pure bindsNothing
  -- Through Pawl.Engine.Event.untap, the CR 701.26b funnel, exactly as TapThis
  -- above goes through Event.tap -- and rule 701.26b draws no such distinction
  -- between the two. A permanent with a stun counter therefore stays tapped and
  -- sheds a counter (CR 122.1d) when this cost is paid.
  --
  -- The payment is still MADE when CR 122.1d takes the untap, which is the rule
  -- rather than a shortcut: CR 614.6 replaces the EVENT the paying produces, and
  -- CR 601.2h's "partial payments are not allowed" is about what the player
  -- performs, not about what the event turns into. That is a replaced event and
  -- not an unpayable part, so it is `payComponent`'s guard that has to be able
  -- to tell them apart -- CR 122.1d leaves the permanent on the battlefield and tapped,
  -- which `canPayComponent` calls payable.
  CostComponent.UntapThis -> do
    Event.untap oid
    pure bindsNothing
  -- Through Event.sacrifice, the CR 701.21 funnel, and never a direct zone poke:
  -- a cost payment is a game event, so dies-triggers, replacement effects and the
  -- turn history all see it. `pid` is the player paying, who for "sacrifice
  -- this" is its controller.
  CostComponent.SacrificeThis -> do
    Event.sacrifice pid oid
    pure bindsNothing
  -- Through Event.changeZone, the CR 400.7 funnel, and never a direct zone poke,
  -- SacrificeThis' call above and for its reason. CR 400.3 is what makes the bare
  -- Zone.Hand right: an object that would go to a hand other than its owner's
  -- goes to its owner's, so the funnel already spells the printed "its owner's".
  CostComponent.ReturnThis -> do
    Event.changeZone oid Zone.Hand
    pure bindsNothing
  -- CR 119.4: the payment is subtracted from the life total. Not every
  -- component reached the gate: one a cost effect reads off the spell's targets
  -- (Filter.TargetsSource) joins at CR 601.2f, after the castability gate ran
  -- without it, so `payComponent`'s guard is the only CR 119.4 check it gets.
  -- Pawl.CastSpec's Terror of the Peaks group is the proof.
  CostComponent.PayLife n -> do
    Event.payLife pid n
    pure bindsNothing
  -- PayLife's arm over the live half: `announce` fixes the component first, so
  -- only one joining after it (PayLife's caveat above) is measured here. A FENCE:
  -- no card in `data/cards/` adds a half-life cost that way.
  CostComponent.PayHalfLife rounding -> do
    gs <- State.get
    payPayable moment slots pid oid (CostComponent.PayLife (halfLifeOf rounding pid gs))
  -- Unpayable, `canPayComponent`'s answer and for its reason. Unpaid rather than
  -- a guessed 0, which CR 601.2h turns into the reversal of the whole casting.
  CostComponent.PayLifeX -> pure Payment.Unpaid
  -- Unpayable, `canPayComponent`'s answer and for its reason -- PayLifeX's arm
  -- above, verbatim.
  CostComponent.PayEnergyX -> pure Payment.Unpaid
  -- Unpayable, PayEnergyX's arm above and for its reason.
  CostComponent.RemovePlusOneCountersX _ -> pure Payment.Unpaid
  CostComponent.SacrificeX _ -> pure Payment.Unpaid
  -- CR 701.17a: the top `n` cards of the PAYING player's own library (CR 400.3),
  -- moved to their graveyard. `canPayComponent` above has already refused a cost
  -- milling more than the library holds (CR 701.17b); a MillCountR row resizing
  -- the instruction can still take it past that, which is why the funnel clamps.
  --
  -- Through Event.millFrom, which is CR 614.1a's road as well as CR 400.7's: rule
  -- 701.17a makes this a mill wherever it happens, so Bruvac the Grandiloquent
  -- sees a cost's mill exactly as it sees a resolution's. Then recorded as the
  -- mill it was -- ONE entry for the batch, Resolve's Effect.Mill arm's reading of
  -- rule 701.17a, and none at all where every move was cancelled.
  --
  -- Binds NO slot: CR 701.17c's look-back is for a later clause of the same
  -- resolution, and a cost has none -- see Pawl.Types.CostComponent's arm.
  CostComponent.MillCards n -> do
    arrived <- Event.millFrom pid n
    Monad.unless (null arrived) (State.modify' (Event.recordEvent (GameEvent.Milled (Milled.MkMilled pid (Seq.fromList arrived)))))
    pure bindsNothing
  -- CR 701.21a: the player chooses which of their permanents dies, so this is a
  -- prompt. Elided only when forced -- exactly as many candidates as the count,
  -- or a count of 0, which a SacrificeX announced at 0 leaves.
  -- Three payable Mountains and a count of two IS asked: they differ in tap
  -- state, counters and attached auras.
  --
  -- Reject-not-repair: an answer that is not a size-`n` subset of the offered
  -- candidates makes the whole payment Unpaid, which pay's restore turns into a
  -- no-op.
  --
  -- The sacrifices are ONE event on ONE board (Event.sacrificeAll),
  -- ExileCardsFromGraveyard's reason below. Pawl.CostSpec's "CR 601.2h Phyrexian
  -- Tribute's two sacrifices grow Vengeful Townsfolk once" proves the event, and
  -- its Anafenza case the board.
  CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) -> do
    gs <- State.get
    let candidates = Replacement.sacrificeCandidates (Just pid) slots pid (Just oid) criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if n == 0 || Natural.length candidates <= n
        then pure (Set.fromList (List.genericTake n candidates))
        else Game.choose (Prompt.ChooseSacrifices decider pid oid candidates n Seq.empty)
    if Set.isSubsetOf chosen (Set.fromList candidates) && Natural.length chosen == n
      then do
        Event.sacrificeAll (fmap ((,) pid) (Set.toAscList chosen))
        -- CR 608.2h: the permanents are gone by the time anything this cost paid
        -- for resolves, so an effect that reads one ("the sacrificed creature's
        -- power") needs a name for it. One of the components that bind a slot
        -- -- TapPermanents and TapForTotalPower below, and the two exiling
        -- components through `bindExiled`; every other returns bindsNothing.
        --
        -- Bound under the id it had on the battlefield, which is the id
        -- Event.changeZone files its last known information under -- the graveyard
        -- incarnation is a different object (CR 400.7) and carries none of it.
        pure (Payment.Paid (Binding.paidObjects Binding.sacrificedPermanent (Set.map Recipient.ToObject chosen)))
      else pure Payment.Unpaid
  -- CR 702.122a: the payer chooses WHICH permanents to tap and HOW MANY, so this
  -- is a prompt, and unlike Sacrifice above it is NEVER elided -- whether the
  -- answer is forced is a question about subsets rather than a count, and getting
  -- it wrong decides for the player.
  --
  -- Reject-not-repair, Sacrifice's posture. The total is summed over the answer
  -- as given, INCLUDING any negative power: CR 702.122a measures the creatures
  -- that were tapped, not a best case. The tap goes through tapObject, TapThis'
  -- route, so each one is a becomes-tapped event (CR 701.26a).
  --
  -- Binds Binding.tappedForTotalPower, which is CR 702.122b's "crews a Vehicle"
  -- relation: Pawl.Engine.Activate reads the slot off this payment and records
  -- GameEvent.Crewed, so Gearshift Ace's crewer-side trigger can find itself
  -- among them. Its own slot and NOT the sibling arm's Binding.tappedPermanent:
  -- see that slot's comment for why one name for both questions would go quiet
  -- on a cost carrying both components.
  --
  -- This component is not crew's alone
  -- (data/cards/synthetic-crewed-battery.json), which is why the binding is
  -- unconditional here and the crew reading is made where the keyword is known
  -- -- CR 702.122d's prohibition is made there too, as the criterion atom
  -- `tapCandidates` above supplies the set for.
  --
  -- The taps are ONE event group, TapPermanents' reason below.
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n criterion) -> do
    gs <- State.get
    let candidates = tapCandidates slots pid oid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <- Game.choose (Prompt.ChooseTapsForTotalPower decider pid oid candidates n)
    let totalPower = sum (fmap (`tapPower` gs) (Set.toAscList chosen))
    if Set.isSubsetOf chosen (Set.fromList candidates) && totalPower >= toInteger n
      then do
        Event.simultaneously (Monad.mapM_ tapObject (Set.toAscList chosen))
        pure (Payment.Paid (Binding.paidObjects Binding.tappedForTotalPower (Set.map Recipient.ToObject chosen)))
      else pure Payment.Unpaid
  -- The payer chooses WHICH permanents to tap, so this is a prompt. Sacrifice's
  -- posture rather than TapForTotalPower's: the count is exact, so as many
  -- candidates as the count leaves one legal answer and the prompt is elided,
  -- where a THRESHOLD would have left a choice among subsets.
  --
  -- Reject-not-repair, Sacrifice's posture again; the tap goes through tapObject,
  -- TapThis' route.
  --
  -- Binds Binding.tappedPermanent (CR 601.2h): Unerring Sling's "damage equal
  -- to the tapped creature's power" needs a name for what its own cost tapped. Unlike Sacrifice's arm the object
  -- is still on the battlefield, so the read is CR 608.2h's CURRENT information
  -- rather than last known.
  --
  -- The taps are ONE event group, Sacrifice's reason; Pawl.CostSpec's "CR 601.2h
  -- Adaptive Gemguard's two tapped Merfolk make one Deeproot Pilgrimage token"
  -- proves it.
  --
  -- A shared creature type (CR 205.3m) is asked of the CHOSEN set, reject-not-
  -- repair: a set holding no type in common is unpaid.
  CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion sharing) -> do
    gs <- State.get
    let candidates = tapCandidates slots pid oid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length candidates <= n
        then pure (Set.fromList candidates)
        else Game.choose (Prompt.ChooseTaps decider pid oid candidates n)
    if Set.isSubsetOf chosen (Set.fromList candidates) && Natural.length chosen == n && (not sharing || shareACreatureType (Set.toList chosen) gs)
      then do
        Event.simultaneously (Monad.mapM_ tapObject (Set.toAscList chosen))
        pure (Payment.Paid (Binding.paidObjects Binding.tappedPermanent (Set.map Recipient.ToObject chosen)))
      else pure Payment.Unpaid
  -- CR 118.1 as a cost: the payer chooses WHICH permanents go back, so this is a
  -- prompt. TapPermanents' posture above -- the count is exact, so as many
  -- candidates as the count leaves one legal answer and the prompt is elided --
  -- and reject-not-repair, Sacrifice's.
  --
  -- Through Event.changeZonesTogether, the CR 400.7 funnel's batch door, and
  -- never a direct zone poke: ReturnThis' reason, with CR 400.3 making the bare
  -- Zone.Hand the printed "its owner's". The returns are ONE event on ONE board,
  -- Sacrifice's reason; Pawl.CostSpec's "CR 601.2h Gush's two returned Islands draw once for
  -- Synthetic Return Ledger" proves it.
  --
  -- Binds Binding.returnedPermanent, the ids as they were BEFORE the move: CR
  -- 702.49c's ninja reads what the returned creature was attacking, which CR
  -- 608.2h's last known information answers off the old id (CR 400.7).
  CostComponent.ReturnPermanents (ReturnPermanents.MkReturnPermanents n criterion) -> do
    gs <- State.get
    let candidates = returnCandidates slots pid oid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length candidates <= n
        then pure (Set.fromList candidates)
        else Game.choose (Prompt.ChooseReturns decider pid oid candidates n)
    if Set.isSubsetOf chosen (Set.fromList candidates) && Natural.length chosen == n
      then do
        Monad.void (Event.changeZonesTogether (fmap (\returned -> (returned, Zone.Hand)) (Set.toAscList chosen)))
        pure (Payment.Paid (Binding.paidObjects Binding.returnedPermanent (Set.map Recipient.ToObject chosen)))
      else pure Payment.Unpaid
  -- CR 406.2 as a cost: Food Chain's "Exile a creature you control". The
  -- payer chooses (CR 118.1), ReturnPermanents' prompt posture, and the exiles
  -- are ONE event, its reason. Binds Binding.exiledPermanent, the ids as they
  -- were BEFORE the move: "the exiled creature's mana value" is CR 608.2h's
  -- last known information of the permanent.
  CostComponent.ExilePermanents (ExilePermanents.MkExilePermanents n criterion) -> do
    gs <- State.get
    let candidates = returnCandidates slots pid oid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length candidates <= n
        then pure (Set.fromList candidates)
        else Game.choose (Prompt.ChooseExiles decider pid oid candidates n)
    if Set.isSubsetOf chosen (Set.fromList candidates) && Natural.length chosen == n
      then do
        Monad.void (Event.changeZonesTogether (fmap (\exiled -> (exiled, Zone.Exile)) (Set.toAscList chosen)))
        pure (Payment.Paid (Binding.paidObjects Binding.exiledPermanent (Set.map Recipient.ToObject chosen)))
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
  -- `ListUtils.nubOrd` is what the Set-answered Sacrifice arm already accepts rather than
  -- the repair it looks like.
  --
  -- CR 701.9a's move goes through Event.discard, so the card gets a CR 400.7
  -- incarnation, Rest in Peace's redirect composes, and the discard is recorded
  -- for a rule 701.9a trigger to read.
  --
  -- The discards are ONE event group, Sacrifice's reason; Pawl.CostSpec's "CR
  -- 601.2h Cathartic Reunion's two discards fire Magmakin Artillerist once, for 2"
  -- proves it.
  CostComponent.DiscardCards (DiscardCards.MkDiscardCards n criterion) -> do
    gs <- State.get
    let held = discardCandidates slots pid oid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length held <= n
        then pure held
        else Game.choose (Prompt.ChooseDiscard decider pid held n)
    let distinct = ListUtils.nubOrd chosen
    if all (\c -> List.elem c held) distinct && Natural.length distinct == n
      then do
        Event.simultaneously (Monad.mapM_ (Event.discard DiscardCause.Ordinary pid) distinct)
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
  --
  -- Binds Binding.discardedCard off what ARRIVED, bindExiled's posture, and only
  -- where CR 400.7j lets the ability find it: in a PUBLIC zone.
  CostComponent.DiscardThis cause -> do
    arrived <- Event.discardReturning cause pid oid
    gs <- State.get
    let findable = Seq.filter (\a -> maybe False (not . Game.isHiddenZone . Object.zone) (Game.lookupObject a gs)) arrived
    pure $ case Foldable.toList findable of
      [] -> bindsNothing
      ids -> Payment.Paid (Binding.paidObjects Binding.discardedCard (Set.fromList (fmap Recipient.ToObject ids)))
  -- CR 118.12's hand-to-battlefield cost. The candidates are re-read HERE rather
  -- than carried from `canPayComponent`'s check: CR 118.12 pays as the ability
  -- resolves, and an earlier component of the same cost may have emptied the
  -- hand, so an empty pool at this moment is Unpaid rather than a partial
  -- payment.
  --
  -- Asked only at TWO or more, which is Prompt.ChooseCardInHand's own posture
  -- where Pawl.Engine.Resolve gathers the same ref: one candidate leaves one
  -- legal answer and nothing to put to anybody. FILTERED and not trusted (#222),
  -- discardCandidates' reading: an answer naming a card that was never offered
  -- falls back to the first rather than moving it.
  --
  -- Through Event.changeZone, CR 400.7's funnel, so the arrival is an entry like
  -- any other -- CR 614.1c's as-enters replacements run and a CR 603.6a enters
  -- trigger fires. NO controller rider is handed to it, and none is owed: CR
  -- 110.2a gives the permanent to the player who put it there, and this
  -- component reads only the PAYER's own hand (CR 402.3), so the payer is the
  -- owner the default already names.
  CostComponent.PutCardFromHandOntoBattlefield criterion -> do
    gs <- State.get
    let held = putOntoBattlefieldCandidates slots pid oid criterion gs
        decider = Decide.deciderFor pid gs
    case held of
      [] -> pure Payment.Unpaid
      first : rest -> do
        chosen <- case rest of
          [] -> pure first
          second : more -> do
            answer <- Game.choose (Prompt.ChooseCardInHand decider pid oid (first NonEmpty.:| (second : more)))
            pure (if List.elem answer held then answer else first)
        Event.changeZone chosen Zone.Battlefield
        pure bindsNothing
  -- CR 406.2's move out of the hidden hand, PutCardFromHandOntoBattlefield's arm
  -- above with the other destination: the candidates are re-read HERE so an
  -- earlier component of the same cost that emptied the hand leaves this Unpaid,
  -- and the prompt is raised only at two or more, one candidate leaving nothing
  -- to put to anybody. FILTERED and not trusted (#222) -- an answer naming a card
  -- that was never offered falls back to the first.
  --
  -- Through Event.changeZone, CR 400.7's funnel, so the exile is a zone change
  -- like any other and a CR 603.6a leaves-the-hand watcher sees it.
  CostComponent.ExileCardFromHand criterion -> do
    gs <- State.get
    let held = exileFromHandCandidates slots pid oid criterion gs
        decider = Decide.deciderFor pid gs
    case held of
      [] -> pure Payment.Unpaid
      first : rest -> do
        chosen <- case rest of
          [] -> pure first
          second : more -> do
            answer <- Game.choose (Prompt.ChooseCardInHand decider pid oid (first NonEmpty.:| (second : more)))
            pure (if List.elem answer held then answer else first)
        Event.changeZone chosen Zone.Exile
        pure bindsNothing
  -- CR 701.20a's reveal as a cost, the arm above's pool and prompt with no zone
  -- change at the end of it (CR 701.20b): the candidates are re-read HERE so an
  -- earlier component of the same cost that emptied the hand leaves this Unpaid,
  -- and the prompt is raised only at two or more. FILTERED and not trusted (#222).
  --
  -- Binds Binding.revealedCard, so "the revealed card's mana value" has a name to
  -- read at resolution (Living Destiny); Pawl.Engine.Cast folds the payment's slots
  -- onto the spell.
  --
  -- Not implemented: CR 701.20a's duration -- the card stays revealed until the
  -- spell leaves the stack, and pawl's reveal is a one-off log entry (#1408).
  -- Nothing observes that entry today either: neutralizing the Event.reveal call
  -- leaves Pawl.CostSpec's Living Destiny cases green, since no printing in the
  -- pool triggers on a reveal and every answerer already sees every hand. The
  -- call stays because CR 701.20a is what makes this a reveal rather than a look
  -- (CR 701.20e), and it is a fence rather than proven behaviour.
  CostComponent.RevealCardFromHand criterion -> do
    gs <- State.get
    let held = revealFromHandCandidates slots pid oid criterion gs
        decider = Decide.deciderFor pid gs
    case held of
      [] -> pure Payment.Unpaid
      first : rest -> do
        chosen <- case rest of
          [] -> pure first
          second : more -> do
            answer <- Game.choose (Prompt.ChooseCardInHand decider pid oid (first NonEmpty.:| (second : more)))
            pure (if List.elem answer held then answer else first)
        Event.reveal RevealCause.Ordinary pid chosen
        pure (Payment.Paid (Binding.paidObjects Binding.revealedCard (Set.singleton (Recipient.ToObject chosen))))
  -- CR 701.4a's two-zone choice through `beholdObjects`: this many distinct
  -- objects out of the payer's hand and the permanents they control taken
  -- together, each revealed where it came out of the hand.
  --
  -- BINDS the objects under Binding.beheldObject, which is what CR 701.4b's "if a
  -- [quality] was beheld" is read off -- through Quantity.WasBound, which asks
  -- whether the slot is bound and never goes back to the board for the quality.
  -- Osseous Exhale is the card, and Pawl.CostSpec's "CR 701.4b the quality is
  -- the one the object had when it was beheld" is the proof.
  CostComponent.Behold (Behold.MkBehold n criterion) -> do
    beheld <- beholdObjects slots pid oid n criterion
    pure (maybe Payment.Unpaid (Payment.Paid . Binding.paidObjects Binding.beheldObject . Set.fromList . fmap Recipient.ToObject) beheld)
  -- CR 701.4a's behold of ONE object, `beholdObjects` as the arm above, then CR
  -- 406.2's exile of what was beheld through Event.changeZoneReturning.
  --
  -- LINKS the card CR 400.7 put into exile to `oid`, the spell, which is CR
  -- 607.2q's "cards exiled to pay the cost of the spell"; Event.carryOver hands
  -- the link to the permanent the spell becomes. Pawl.CostSpec's Champion of the
  -- Weird group is the proof.
  CostComponent.BeholdAndExile criterion -> do
    beheld <- beholdObjects slots pid oid 1 criterion
    case beheld of
      Just [chosen] -> do
        arrived <- Event.changeZoneReturning chosen Zone.Exile
        let link = ExileLink.MkExileLink {ExileLink.source = oid, ExileLink.ability = Nothing}
        State.modify' (\g -> g {GameState.exiledWith = foldr (`Map.insert` link) (GameState.exiledWith g) arrived})
        pure bindsNothing
      _ -> pure Payment.Unpaid
  -- CR 107.14: paying energy removes that many energy counters from the player.
  -- Natural subtraction is PARTIAL, so `left` is guarded; `payComponent`'s guard
  -- has already refused `have < n`, and this keeps the arm total anyway.
  CostComponent.PayEnergy n -> do
    spendEnergy pid n
    pure bindsNothing
  -- CR 606.4's placement, through Event.putCounters -- CR 122.6's funnel, the
  -- road PutPlusOneCountersOnThis below takes and for its reason: WHICH
  -- replacements see the counters is settled by the CAUSE, which `counterCause`
  -- reads off the moment, rather than by which door the placement came in at.
  --
  -- CR 602.2b pays an activation cost as part of ACTIVATING (CR 601.2h), which
  -- CR 609.1 gives no resolution to hang an effect on, so a loyalty symbol is
  -- paid at OutsideResolution and arrives as CounterCause.ByPayment. That is
  -- the whole of the split Replacement.matchesPutter draws: CR 614.16's
  -- effect-grain row (Doubling Season, "if an EFFECT would put") refuses a
  -- ByPayment placement, and CR 614.1's player-grain row (Vorinclex, Monstrous
  -- Raider, "if YOU would put") takes it, the payer being a player whatever
  -- moment they pay at. Pawl.PlaneswalkerSpec pins the pair on one board --
  -- "CR 614.16 Doubling Season does not double a loyalty ability's own cost"
  -- and "CR 614.1 Vorinclex doubles a loyalty ability's own cost" -- against
  -- CR 306.5b's entry counters, which both cards do double.
  --
  -- What the funnel adds over the direct write this used to be: the
  -- GameEvent.CountersPut record a counter-watching trigger reads (the trade
  -- RemoveLoyaltyFromThis below already made for CountersRemoved) and CR
  -- 613.7c's timestamp on the loyalty counters, which the direct write left
  -- stale.
  CostComponent.AddLoyaltyToThis n -> do
    Monad.void (Event.putCounters (counterCause moment pid) oid CounterKind.Loyalty n)
    pure bindsNothing
  -- CR 606.4's other half, through Event.removeCounters -- CR 122's removal
  -- funnel, the sibling of the placement funnel above -- so the removal is
  -- recorded as a GameEvent.CountersRemoved a trigger can see (Chandra, Fire
  -- Artisan's "whenever one or more loyalty counters are removed from Chandra",
  -- which her own -7 fires).
  --
  -- This half owes none of the CR 614.16-versus-CR 614.1 argument above: rule
  -- 614.16 is about REPLACING a placement, and Event.removeCounters runs no
  -- replacement loop at all -- its own Haddock gives the reason, that no
  -- ReplacementEffect class in Pawl.Types.ReplacementEffect pairs with a removal.
  -- So the cause the placement above must carry has no analogue here; what this
  -- funnel adds is the record alone.
  --
  -- The funnel saturates, which is the floor a direct write would have had to
  -- apply itself: CR 606.6 has already refused an activation the permanent
  -- cannot pay for, so a saturating removal is unreachable through this door
  -- anyway.
  CostComponent.RemoveLoyaltyFromThis n -> do
    Event.removeCounters oid CounterKind.Loyalty n
    pure bindsNothing
  -- CR 118.1's removal as a cost, through Event.removeCounters -- CR 122's
  -- removal funnel, RemoveLoyaltyFromThis' road above and for its reason: what
  -- the funnel adds is the GameEvent.CountersRemoved record a counter-watching
  -- trigger reads. CR 614.16 does not arise, being about PLACING a counter, so
  -- this arm owes none of AddLoyaltyToThis' argument above.
  --
  -- The funnel saturates, which canPayComponent has already made unreachable
  -- through this door: CR 118.3 refuses an activation the permanent cannot pay
  -- for.
  CostComponent.RemoveCountersFromThis removal -> do
    Event.removeCounters oid (CountersFromThis.kind removal) (CountersFromThis.count removal)
    pure bindsNothing
  -- The same removal aimed at ANOTHER permanent, so the payer chooses which one
  -- and this is a prompt -- elided at exactly one candidate, where the rules
  -- leave nothing to ask. Sacrifice's posture, read over one object rather than
  -- a subset: the count is counters and not permanents, so what forcedness
  -- measures is how many permanents the criterion and CR 118.3 leave.
  --
  -- Reject-not-repair, Sacrifice's posture again: an answer naming something
  -- never offered makes the whole payment Unpaid, which `pay`'s restore turns
  -- into a no-op.
  --
  -- Through Event.removeCounters, CR 122's removal funnel -- the arm above's
  -- road and for its reason, so a counter-watching trigger reads the same
  -- GameEvent.CountersRemoved whichever component spent it. Saturation is
  -- unreachable through this door, `counterRemovalCandidates` having refused
  -- every permanent short of `n`.
  --
  -- Binds HOW MANY it removed under Binding.removedCounters, and not what it
  -- took them off: no printing of this cost goes on to name the permanents.
  --
  -- Spread FROM AMONG several permanents, the payer divides the count instead,
  -- and the prompt is elided where `fillInOrder`'s division is the only one: a
  -- single candidate, or candidates carrying exactly the count between them.
  -- Pawl.CostSpec's Novijen Sages group proves the division.
  --
  -- With the count a FLOOR (FromAmongAtLeast), the payer also settles how many,
  -- so the prompt stands even over one candidate and is elided only where the
  -- candidates carry exactly the floor. Pawl.CostSpec's Ooze Flux group proves it.
  CostComponent.RemoveCounters (CountersFromPermanents.MkCountersFromPermanents n which criterion spread) -> do
    gs <- State.get
    let decider = Decide.deciderFor pid gs
        removed taken = Payment.Paid (Map.singleton Binding.removedCounters (Binding.toAmount taken))
    case which of
      -- CR 122.1: with no kind named, the payer divides by kind as well, and a
      -- spread FromOne is that division off a single permanent. Pawl.CostSpec's
      -- Tayam, Luminous Enigma and Soul Diviner groups prove it.
      WhichCounters.OfAnyKind -> do
        let offered = case spread of
              CounterSpread.FromOne -> Map.filter (\kinds -> sum kinds >= n) (mixedRemovalCandidates slots pid oid which criterion gs)
              CounterSpread.FromAmong -> mixedRemovalCandidates slots pid oid which criterion gs
              CounterSpread.FromAmongAtLeast -> mixedRemovalCandidates slots pid oid which criterion gs
        division <- case onlyMixedDivision spread n offered of
          Just only -> pure only
          Nothing -> Game.choose (Prompt.ChooseMixedCounterRemoval decider pid oid spread n offered)
        if dividesMixedRemoval spread n offered division
          then do
            let flat = flattenMixed division
            Monad.forM_ (Map.toList flat) (\((chosen, kind), taken) -> Event.removeCounters chosen kind taken)
            pure (removed (sum flat))
          else pure Payment.Unpaid
      WhichCounters.OfKind kind -> do
        let removeDivision division = do
              Monad.forM_ (Map.toList (Map.filter (> 0) division)) (\(chosen, taken) -> Event.removeCounters chosen kind taken)
              pure (removed (sum division))
        case spread of
          CounterSpread.FromOne -> case counterRemovalCandidates slots pid oid n which criterion gs of
            -- CR 118.3's board, which canPayComponent has already refused, so reaching
            -- it means the counters left between the check and the payment.
            [] -> pure Payment.Unpaid
            candidates@(first : rest) -> do
              chosen <- case rest of
                [] -> pure first
                second : more -> Game.choose (Prompt.ChooseCounterRemoval decider pid oid (first NonEmpty.:| (second : more)))
              if List.elem chosen candidates
                then do
                  Event.removeCounters chosen kind n
                  pure (removed n)
                else pure Payment.Unpaid
          CounterSpread.FromAmong -> do
            let offered = spreadRemovalCandidates slots pid oid which criterion gs
                carried = sum offered
            division <-
              if Map.size offered <= 1 || carried == n
                then pure (fillInOrder n offered)
                else Game.choose (Prompt.ChooseCounterRemovalAmong decider pid oid n offered)
            -- CR 118.3's board again where `carried` falls short, which the division
            -- then cannot add up to.
            if dividesRemoval n offered division
              then removeDivision division
              else pure Payment.Unpaid
          CounterSpread.FromAmongAtLeast -> do
            let offered = spreadRemovalCandidates slots pid oid which criterion gs
            division <-
              if sum offered == n
                then pure offered
                else Game.choose (Prompt.ChooseCounterRemovalAtLeast decider pid oid n offered)
            -- CR 118.3's board where the candidates fall short of the floor, which
            -- no division then reaches.
            if dividesRemovalAtLeast n offered division
              then removeDivision division
              else pure Payment.Unpaid
  -- CR 122.6's placement, through the Event.putCounters funnel -- the same call
  -- AddLoyaltyToThis above makes, and the difference is WHEN the cost is paid,
  -- which `counterCause` below reads off the moment rather than assuming.
  -- Every printing of this component is CR 118.12's endure, paid as the spell or
  -- ability RESOLVES, which is what CR 609.1 calls an effect, so even CR 614.16's
  -- narrower subject reaches it: Doubling Season doubles endure's counter, and
  -- still not a planeswalker's +1, which is the split Pawl.PlaneswalkerSpec and
  -- Pawl.ReplacementSpec's blight pair pin. CR 614.1's passive subject --
  -- Hardened Scales -- reaches the placement at EITHER moment, so it says nothing
  -- about which cause this arm hands the funnel. Paid whatever the funnel then
  -- places.
  CostComponent.PutPlusOneCountersOnThis n -> do
    Monad.void (Event.putCounters (counterCause moment pid) oid CounterKind.PlusOnePlusOne n)
    pure bindsNothing
  -- CR 701.68a's whole procedure, which Pawl.Engine.Blight owns. Unpaid on rule
  -- 701.68b's board, which canPayComponent has already refused, so reaching it
  -- means the creature left between the check and the payment.
  --
  -- The one component paid at BOTH moments -- Soul Immolation's additional cost
  -- under CR 601.2h, Boggart Mischief's "unless you blight 1" under CR 118.12 --
  -- so the cause is `counterCause`'s and not a constant. Doubling Season doubles
  -- the second and not the first; Vorinclex, Monstrous Raider doubles both.
  CostComponent.Blight n -> do
    blighted <- Blight.blight (counterCause moment pid) oid n
    -- The creature chosen is DISCARDED here, where the effect arm binds it: CR
    -- 701.68c's "blighted creature" is read by a later clause of the same
    -- resolution, and a cost is paid before there is one.
    pure (if Maybe.isJust blighted then bindsNothing else Payment.Unpaid)
  -- Unpayable, `canPayComponent`'s answer and for its reason -- PayLifeX's arm
  -- above, verbatim.
  CostComponent.BlightX -> pure Payment.Unpaid
  -- Unpayable, BlightX's arm above and for its reason.
  CostComponent.RemoveLoyaltyFromThisX -> pure Payment.Unpaid
  -- CR 701.61a's whole procedure, which Pawl.Engine.Forage owns -- the Blight arm
  -- above's shape. Unpaid on the board CR 608.2d refuses, which canPayComponent
  -- has already checked, so reaching it means the graveyard or the Food went away
  -- between the check and the payment.
  --
  -- The forage BINDS NOTHING: rule 701.61a names neither the cards exiled nor the
  -- Food sacrificed afterwards, so no printing has anything to read.
  CostComponent.Forage -> do
    did <- Forage.forage pid oid
    pure (if did then bindsNothing else Payment.Unpaid)
  -- CR 705.1's flip as a payment, through Event.flipWinLoseCoin so a cost's coin
  -- is the same CR 705.1 event an effect's coin is -- Karplusan Minotaur's own
  -- two triggers watch the flip its cumulative upkeep pays.
  --
  -- CR 705.2's win/lose kind rather than the face-only kind: the cost names no
  -- face and prints no consequence of its own, so the flip has a caller and the
  -- outcome is the one rule 705.2's second sentence describes.
  --
  -- ALWAYS PAID. The outcome is not a payment condition -- `canPayComponent`
  -- above says why -- and the flip BINDS NOTHING: rule 705.1 names nothing the
  -- rest of the cost or a later clause could read.
  CostComponent.FlipCoin -> do
    statements <- Coin.statementsFor (Just pid)
    _ <- Event.flipWinLoseCoin pid statements
    pure bindsNothing
  -- CR 702.174a's first ability, paid by naming the opponent the gift is
  -- promised to. The answer is FILTERED rather than trusted and falls back to the
  -- head, Pawl.Engine.Event's as-enters chooser's posture.
  --
  -- Written to Object.chosenPlayer rather than bound as a slot: CR 702.174b's
  -- payoff is a triggered ability of the PERMANENT the spell becomes, and CR
  -- 400.7 clears a spell's bindings on that move where
  -- Pawl.Engine.Event.changeZoneAttaching carries this field across it under CR
  -- 400.7d. Pawl.Types.PlayerRef's ChosenPlayer is what reads it back.
  --
  -- ALWAYS PAID where the gate allowed the offer at all: the payer has an
  -- opponent, and naming one spends nothing that could run out.
  CostComponent.ChooseOpponent -> do
    gs <- State.get
    case Game.opponentsInReach pid gs of
      -- Unreachable behind `payComponent`'s guard, which reads the same offer;
      -- defensive, as CR 118.3 would have it.
      [] -> pure Payment.Unpaid
      -- CR 102.2: a two-player game leaves exactly one opponent, and one option
      -- is not a choice.
      [sole] -> do
        stampChosenPlayer oid sole
        pure bindsNothing
      first : second : rest -> do
        let offered = first NonEmpty.:| (second : rest)
        answer <- Game.choose (Prompt.ChooseOpponent (Decide.deciderFor pid gs) pid oid offered)
        stampChosenPlayer oid (if List.elem answer (NonEmpty.toList offered) then answer else first)
        pure bindsNothing
  -- Unpayable, `canPayComponent`'s answer and for its reason -- BlightX's
  -- arm above, verbatim.
  CostComponent.WaterbendX -> pure Payment.Unpaid
  -- NOTHING TO PAY. Rule 701.67a's waterbend cost is the mana this component
  -- scopes, and that mana is in the cost's own mana part, paid by `payMana`
  -- like every other symbol; what the component states is the licence to
  -- substitute taps for it, which `announceSubstitutions` has already
  -- cashed into a TapPermanents component of its own by the time any component
  -- is paid.
  --
  -- CR 701.67c's trigger is written from here all the same, "regardless of how
  -- they paid that cost": this arm is the one place a waterbend cost is reached
  -- whether the generic mana went out as mana or as rule 701.67a's taps, since
  -- the substitution `announceSubstitutions` cashed is a TapPermanents component
  -- that says nothing about which licence bought it.
  --
  -- Binds the amount under Binding.waterbendCost for the same reason: it is
  -- what "if this spell's additional cost was paid" reads (Quantity.WasBound).
  CostComponent.Waterbend n -> do
    State.modify' (Event.recordEvent (GameEvent.Waterbent pid))
    pure (Payment.Paid (Map.singleton Binding.waterbendCost (Binding.toAmount n)))
  -- CR 701.67c's event, Waterbend's reason, and NO record: this waterbend is
  -- CR 118.9's alternative cost, so "if this spell's additional cost was paid"
  -- must not read it. Pawl.CostSpec's "CR 118.8b Spirit Water Revival cast
  -- through Hama without its {6} draws two" is the proof.
  CostComponent.WaterbendInstead _ -> do
    State.modify' (Event.recordEvent (GameEvent.Waterbent pid))
    pure bindsNothing
  -- CR 406.2's move, through the Event.changeZone funnel, so the card gets a CR
  -- 400.7 incarnation and anything watching a graveyard-to-exile move sees it.
  -- No prompt: the cost names this card.
  --
  -- The card is in EXILE by the time the ability resolves, which is what makes CR
  -- 113.7a load-bearing here -- Loxodon Surveyor's draw resolves off a source
  -- that has already left the graveyard the cost read.
  --
  -- Binds Binding.exiledCard off what ARRIVED, ExileThis' binding below and for
  -- its reason.
  CostComponent.ExileThisFromGraveyard -> do
    arrived <- Event.changeZoneReturning oid Zone.Exile
    pure (bindExiled arrived)
  -- CR 406.2's move off the BATTLEFIELD, through the same funnel and with no
  -- prompt for the same reason: the cost names this permanent. CR 118.3 asked
  -- again, SacrificeThis' reason above; Pawl.CostSpec's "CR 118.3 the Altar eats
  -- the Executioner before its own exile is paid" is the proof.
  --
  -- Binds Binding.exiledCard, so that a later clause of the same ability can name
  -- the card this payment put into exile -- CR 702.167a's "Return this card to
  -- the battlefield" after craft's "Exile this permanent". The card CR 400.7
  -- made, not the permanent that left, which is why the binding is taken off what
  -- arrived; see that slot.
  CostComponent.ExileThis -> do
    arrived <- Event.changeZoneReturning oid Zone.Exile
    pure (bindExiled arrived)
  -- CR 406.2's move again, for CHOSEN cards: the payer picks which, so this is a
  -- prompt. Elided only when forced, Sacrifice's elision.
  --
  -- Reject-not-repair, Sacrifice's posture verbatim. The candidates are read
  -- ONCE, before the prompt, so the answer is checked against the same list the
  -- player was offered.
  --
  -- The exiles are ONE event on ONE board (Event.changeZonesTogether): paying
  -- CR 601.2h's cost exiles them as a single action (Rakshasa Vizier's ruling,
  -- 2014-09-20), so a "one or more cards" trigger fires once for the lot.
  -- Pawl.CostSpec's "CR 601.2h delving three cards fires Rakshasa Vizier once,
  -- for three counters" proves it; ExileMaterials and CollectEvidence below
  -- follow.
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) -> do
    gs <- State.get
    let candidates = exileCandidates slots pid oid criterion gs
        decider = Decide.deciderFor pid gs
    chosen <-
      if Natural.length candidates <= n
        then pure (Set.fromList candidates)
        else Game.choose (Prompt.ChooseExilesFromGraveyard decider pid oid candidates n)
    if Set.isSubsetOf chosen (Set.fromList candidates) && Natural.length chosen == n
      then do
        Monad.void (Event.changeZonesTogether (fmap (\c -> (c, Zone.Exile)) (Set.toAscList chosen)))
        pure bindsNothing
      else pure Payment.Unpaid
  -- CR 702.167a's [materials]: the arm above over the battlefield and the
  -- graveyard at once, with `materialCandidates` for the pool and
  -- Prompt.ChooseMaterials for the ask. Elided only when forced, and
  -- reject-not-repair, both that arm's posture verbatim.
  --
  -- `orMore` is rule 702.167a's "one or more", which makes the count a MINIMUM:
  -- the size the answer is checked against becomes a floor rather than an
  -- equality, and nothing else moves. The ELISION is the same test under both
  -- readings -- with no more candidates than the count there is exactly one legal
  -- answer either way, the whole pool, so eliding decides nothing for the payer;
  -- with fewer the check below leaves the component Unpaid.
  --
  -- BINDS the materials' exile incarnations under Binding.craftMaterials, which
  -- is CR 702.167c's "the exiled cards used to craft it": the craft ability's
  -- return links them to the permanent it puts onto the battlefield.
  CostComponent.ExileMaterials (ExileMaterials.MkExileMaterials n orMore criterion) -> do
    gs <- State.get
    let candidates = materialCandidates slots pid oid criterion gs
        decider = Decide.deciderFor pid gs
        enough chosen = if orMore then Natural.length chosen >= n else Natural.length chosen == n
    chosen <-
      if Natural.length candidates <= n
        then pure (Set.fromList candidates)
        else Game.choose (Prompt.ChooseMaterials decider pid oid candidates n orMore)
    if Set.isSubsetOf chosen (Set.fromList candidates) && enough chosen
      then do
        arrived <- Event.changeZonesTogether (fmap (\c -> (c, Zone.Exile)) (Set.toAscList chosen))
        pure (Payment.Paid (Binding.paidObjects Binding.craftMaterials (Set.fromList (fmap Recipient.ToObject (foldMap Foldable.toList arrived)))))
      else pure Payment.Unpaid
  -- CR 701.59a: the payer chooses WHICH cards and HOW MANY, so this is a prompt,
  -- and it is NEVER elided -- TapForTotalPower's posture, the number being a
  -- threshold on an aggregate rather than a count, so whether the answer is forced
  -- is a question about subsets and settling it here would decide for the player.
  --
  -- Reject-not-repair, Sacrifice's posture. The total is summed over the answer as
  -- given, and the candidates are read HERE so an earlier component of the same
  -- cost that emptied the graveyard leaves this Unpaid. The exile goes through
  -- Event.changeZonesTogether, ExileCardsFromGraveyard's route above.
  --
  -- BINDS the exiled cards under Binding.collectedEvidence, which is what CR
  -- 701.59c's linked "if evidence was collected" is read off, through
  -- Quantity.WasBound -- Behold's route above. Pawl.CostSpec's Vitu-Ghazi
  -- Inspector group is the proof.
  CostComponent.CollectEvidence n -> do
    gs <- State.get
    let candidates = evidenceCandidates slots pid oid gs
        decider = Decide.deciderFor pid gs
    chosen <- Game.choose (Prompt.ChooseCollectEvidence decider pid oid candidates n)
    let collected = sum (fmap (`evidenceValue` gs) (Set.toAscList chosen))
    if Set.isSubsetOf chosen (Set.fromList candidates) && collected >= toInteger n
      then do
        Monad.void (Event.changeZonesTogether (fmap (\c -> (c, Zone.Exile)) (Set.toAscList chosen)))
        -- CR 701.59a's "whenever you collect evidence" reads this.
        State.modify' (Event.recordEvent (GameEvent.CollectedEvidence pid))
        pure (Payment.Paid (Binding.paidObjects Binding.collectedEvidence (Set.map Recipient.ToObject chosen)))
      else pure Payment.Unpaid
  -- The arm above at the amount these slots fix (fixComputed). A REGRESSION
  -- FENCE: Cast.castProposed fixes the amount before CR 601.2h pays, per Urgent
  -- Necropsy's ruling, so a payment reaching here unfixed has no caller.
  CostComponent.CollectEvidenceOfTargets -> do
    gs <- State.get
    payPayable moment slots pid oid (fixComputed slots gs component)
  -- CR 406.2 with no prompt: CR 404.2's order determines the card. Unpaid where
  -- the graveyard holds no matching card, agreeing with canPayComponent above.
  CostComponent.ExileTopFromGraveyard criterion -> do
    gs <- State.get
    case topExileCandidate slots pid oid criterion gs of
      Nothing -> pure Payment.Unpaid
      Just candidate -> do
        Event.changeZone candidate Zone.Exile
        pure bindsNothing

-- CR 614.16's question, asked of a payment: does the placement this cost makes
-- have a resolving spell or ability's effect behind it?
--
-- CR 609.1 says it does exactly when the cost is paid under CR 118.12, as the
-- object resolves; CR 601.2h's payment and CR 508.1j's toll are made while a
-- spell is cast, an ability activated or a declaration is being made, and none of
-- those is a resolution. The PLAYER is the payer either way: rule 118.12 names
-- the player who takes the action, and rule 601.2h's payer is the one casting or
-- activating.
--
-- What this buys is Soul Immolation beside Doubling Season: the blight is the
-- payer's, and no effect's, so rule 614.16's row does not apply and X counters go
-- on rather than 2X; see #1647. Vorinclex, Monstrous Raider's row names a player
-- instead and still applies, which is the pair that says the answer is about the
-- CAUSE and not about counters-during-costs.
--
-- Not implemented: the reading Event.resolvingDiscardCause takes for a discard,
-- that a CR 118.12 cost paid on resolution is no effect; this arm and
-- Effect.Blight's cause call it one, so Doubling Season doubles Grub, Notorious
-- Auntie's blight (#4544).
counterCause :: PaymentMoment.PaymentMoment -> PlayerId -> CounterCause.CounterCause
counterCause moment pid = case moment of
  PaymentMoment.OutsideResolution -> CounterCause.ByPayment pid
  PaymentMoment.DuringResolution -> CounterCause.ByEffect pid

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
        -- CR 601.2b has NOT named -- Flame Javelin's own ruling. Every road into
        -- this function now totals a cost CR 601.2b has already completed, so the
        -- arm is a total function's and not a live case.
        ManaSymbol.MonocoloredHybrid _ -> 0
        -- CR 107.4f makes this a COLOURED symbol whose other half is life.
        ManaSymbol.Phyrexian _ -> 0
        -- CR 107.4f's hybrid Phyrexian symbol is coloured twice over and generic
        -- not at all: neither of its three ways is generic mana.
        ManaSymbol.HybridPhyrexian _ -> 0
        -- CR 107.4h: generic reductions don't affect {S} costs, which is why it
        -- is not spelled Generic 1. The one arm where this function and
        -- reducingGenericOf part company; the Adjustments case "CR 107.4h a
        -- generic reduction does not affect an {S} in the cost" proves this side.
        ManaSymbol.Snow -> 0
        -- {X} is no part of the generic component: CR 107.3 makes it a value a
        -- player announces, and CR 601.2b's announcement precedes rule 601.2f, so
        -- a cost being TOTALLED never carries one. LIVE rather than unreachable
        -- all the same -- grantedForetellCost applies rule 702.143d's reduction
        -- to a PRINTED mana cost ahead of any announcement, so an {X} card an
        -- effect foretold (Blaze) carries its symbol through here untouched and
        -- announces it at the cast.
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
        -- Neither of its halves is generic mana (reductionHalvesOf).
        ManaSymbol.HybridPhyrexian _ -> 0
        -- CR 118.7g: a snow-symbol reduction is that much GENERIC mana. THE arm
        -- this side exists for; CR 107.4h is about the other side.
        ManaSymbol.Snow -> 1
        -- {X} again, costGenericOf's Variable arm and for its reason.
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
        ManaSymbol.HybridPhyrexian _ -> True
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
        -- it does. Every road into this function now totals a cost CR 601.2b has
        -- already completed, so the arm is a total function's and not a live case.
        ManaSymbol.Hybrid {} -> Nothing
        ManaSymbol.MonocoloredHybrid _ -> Nothing
        -- EXACT rather than an elision: the symbol is necessarily UNANNOUNCED, CR
        -- 601.2b's announcement leaving behind either an OfType or a payment of
        -- life, so no caller reaching this arm has established that there is a
        -- green mana here to cancel. Edgewalker's ruling says so outright.
        ManaSymbol.Phyrexian _ -> Nothing
        -- Nothing, the arm above's reason twice over: unannounced, and naming
        -- two colours rather than one even once it is.
        ManaSymbol.HybridPhyrexian _ -> Nothing
        -- CR 107.4h: {S} is paid with one mana of ANY type, so it names none, and
        -- a reduction of one white mana cannot single it out.
        ManaSymbol.Snow -> Nothing
        -- {X} again, costGenericOf's Variable arm; {X} names no mana type.
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
        -- The hybrid arms' reason: its two colours are halves the payer chooses
        -- between (reductionHalvesOf), so one still spelled {G/U/P} is unasked.
        ManaSymbol.HybridPhyrexian _ -> Nothing
        -- CR 118.7g makes an {S} reduction GENERIC mana, so reducingGenericOf's
        -- Snow arm is where it lands.
        ManaSymbol.Snow -> Nothing
        -- {X} again, costGenericOf's Variable arm and for its reason.
        ManaSymbol.Variable -> Nothing
      -- The canonical form point 5's header describes: the generic component as
      -- one leading symbol, then the typed symbols left.
      canonical generic typed = ManaCost.MkManaCost ((if generic == 0 then [] else [ManaSymbol.Generic generic]) <> typed)
      -- Point 1: every increase, onto the generic component, before any reduction.
      raise (ManaCost.MkManaCost symbols) =
        canonical (sum (fmap costGenericOf symbols) + sum increases) (filter isTyped symbols)
      -- ONE reduction, with the floor and the coloured-mana confinement its own
      -- effect states.
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

-- CR 107.14: pay this much {E} -- remove that many energy counters from the
-- player. Natural subtraction is PARTIAL, so the floor is explicit; every caller
-- has already measured against Game.energyOf, and the guard keeps this total anyway.
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
