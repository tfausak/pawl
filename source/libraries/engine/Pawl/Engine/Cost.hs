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
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Blight as Blight
import qualified Pawl.Engine.Claim as Claim
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Detain as Detain
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
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
import qualified Pawl.Types.CandidateCost as CandidateCost
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Claim (Claim)
import qualified Pawl.Types.Claim as Claim.Type
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostAdjustments as CostAdjustments
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostReduction as CostReduction
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
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TapPermanents as TapPermanents
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

-- A candidate no keyword ability offered: the card's own printed cost, one of
-- its printed alternatives, or a cost an effect applied (CR 118.9). See
-- candidateCostsFor for why the graveyard arm is the only one that tags.
untagged :: Cost Keyword.Type.Keyword -> CandidateCost.CandidateCost
untagged = CandidateCost.MkCandidateCost Nothing

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
-- printed one first, then each alternative, and last any standing CR 118.9
-- grant a player effect applies. Empty for anything that is not a
-- card: a token is created onto the battlefield and never cast, and an ability
-- on the stack is not a spell.
--
-- A LAND yields one candidate whose mana part is Nothing (CR 202.1's "a card's
-- mana cost", absent), which CR 118.6 makes unpayable, so canPay says False and
-- Cast.castable never offers it -- the rule rather than a special case.
--
-- The candidates depend on the ZONE the object is being cast from, because CR
-- 702.34a's permission and its cost are one sentence. From a graveyard the
-- keyword costs are the only ones a card can carry on its own, so a card with no
-- flashback, aftermath or jump-start yields no candidate at all -- CR 601.3's
-- default prohibition arriving through Cast.castable's affordability gate as
-- well as through its permission gate. A PLAYER-scoped permission (Yawgmoth's
-- Will) is the exception, and it is one because the permission and the cost are
-- NOT one sentence there: the effect says nothing about what the spell costs, so
-- the printed cost is offered exactly as a hand would offer it.
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
--
-- The COSTS ALONE, for every caller that only prices the cast.
-- candidateCostsFor below is the same list with each candidate's offering
-- keyword still attached, and this is defined in terms of that one so the two
-- cannot drift: a cast that has to know WHICH cost it paid (CR 702.34a) reads
-- the tagged list, and everything else reads this.
--
-- WHICH candidates carry a keyword is argued at candidateCostsFor.
costsFor :: CardName.CardName -> ObjectId -> GameState -> [Cost Keyword.Type.Keyword]
costsFor name oid gs = fmap CandidateCost.cost (candidateCostsFor name oid gs)

-- costsFor's list with CR 601.2b's other half recorded: WHICH ability offered
-- each candidate, which is the fact CR 702.34a's "if the flashback cost was
-- paid" and CR 702.133a's "if this spell was cast using its jump-start
-- ability" are conditioned on.
--
-- THE GRAVEYARD ARM is the only one that tags, and the rules are what confine
-- it there: those two abilities are the pool's only ones that ask which of a
-- spell's candidate costs was the one paid, and both are graveyard costs. CR
-- 702.127a's aftermath asks a different question -- "if this spell was cast
-- from a graveyard" -- which is answered by the zone rather than by the cost,
-- so its candidate is tagged for the record and no reader consults the tag.
-- Rule 702.37a's morph cost and rule 702.143a's foretell cost have no such
-- clause behind them, so tagging them would be a field nothing reads.
candidateCostsFor :: CardName.CardName -> ObjectId -> GameState -> [CandidateCost.CandidateCost]
candidateCostsFor name oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj | Facing.isFaceDown (Object.facing obj) -> [untagged faceDownCost]
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
          -- cast a card from another player's HAND or GRAVEYARD -- CR 400.1 files
          -- both by player and Cast.zoneCandidates hands out only the caster's own
          -- -- so the owner and the caster are the same player wherever this is
          -- asked. That holds for the CR 601.3 permission read in the graveyard arm
          -- below as well, which is the other reader of the owner here. Exile is
          -- the one zone whose candidates are not filed by owner, and no permission
          -- in the pool aims a player at a card there that somebody else owns.
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
            --
            -- CR 601.3 / Yawgmoth's Will is the fourth shape, and the one that
            -- is not a keyword: an EFFECT that permits the cast supplies no cost
            -- with it, so what the card asks for is what it asks for anywhere --
            -- the printed cost and the card's own alternatives, exactly the
            -- hand's list. Offered BESIDE the three above rather than instead of
            -- them, which is the rules answer for a flashback card in a
            -- graveyard under such an effect: both costs are available and CR
            -- 601.2b picks one.
            Zone.Graveyard ->
              let -- CR 613.1: the keywords the card HAS in the graveyard, not
                  -- the ones it prints. Rule 702.34a's cost is stated by the
                  -- ability, and an ability granted to a card in a graveyard
                  -- (Viral Spawning's own, CR 113.6f) states it as much as a
                  -- printed one does -- so a projected read is what makes the
                  -- granted cost reachable at all (#1385).
                  --
                  -- Off the OBJECT rather than the face, so the caller's CR
                  -- 709.3a half is the one measured: every caller stamps the
                  -- proposal through Pawl.Engine.Cast.asProposed first, and the
                  -- projection resolves that same stamp.
                  keywords = Map.keysSet (Projection.keywordsOf oid gs)
                  -- The flashback keyword AS IT WAS READ, rebuilt from the
                  -- cost the projected set carried: rule 702.34a's ability and
                  -- its cost are one sentence, so the cost is the whole of what
                  -- distinguishes one flashback instance from another.
                  flashback cost = CandidateCost.MkCandidateCost (Just (Keyword.Type.Flashback cost)) (withAdditional cost)
               in fmap flashback (Maybe.maybeToList (Keyword.flashbackCost keywords))
                    <> [CandidateCost.MkCandidateCost (Just Keyword.Type.Aftermath) printed | Keyword.hasAftermath keywords]
                    <> [ CandidateCost.MkCandidateCost
                           (Just Keyword.Type.JumpStart)
                           -- CR 702.133a's cost names no quality -- "discard a
                           -- card" -- so the criterion admits everything.
                           printed {Cost.components = Cost.components printed <> [CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))]}
                       | Keyword.hasJumpStart keywords
                       ]
                    -- UNTAGGED, and that is the whole of this issue's fix: an
                    -- effect's permission states no cost, so the cost paid
                    -- under it is the card's own and neither rule 702.34a's
                    -- clause nor rule 702.133a's is satisfied by paying it.
                    <> (if PlayerEffect.mayCastFromGraveyard (Object.owner obj) oid gs then fmap untagged (printed : alternatives) else [])
            -- CR 702.170d: a PLOTTED card is cast "without paying its mana
            -- cost", which is CR 118.9's alternative cost and so
            -- withoutPayingManaCost above -- the card's own additional costs
            -- ride along with it, since that function carries them.
            --
            -- INSTEAD of the printed cost and not beside it, which is the whole
            -- difference from the graveyard arm's Yawgmoth's Will case: rule
            -- 702.170d is the only thing permitting this cast (nothing else in
            -- the pool casts a card from exile for its mana cost), so offering
            -- `printed` here would price a cast no rule allows.
            --
            -- The OTHER permission this zone can carry -- CR 715.3d's Adventure
            -- exile, and Effect.GrantPlayFromExile -- states no cost, so what the
            -- card asks for is what it asks for anywhere. That is the `_` arm's
            -- list, and this arm falls back to it for an exiled card that is not
            -- plotted.
            Zone.Exile
              | Maybe.isJust (Object.plotted obj) -> [untagged (withoutPayingManaCost face)]
            -- CR 702.143a: a FORETOLD card is cast "by paying any foretell cost
            -- it has rather than paying that spell's mana cost", which is CR
            -- 118.9's alternative cost -- so the keyword's payload is wrapped by
            -- withAdditional exactly as flashback's is, and the card's own
            -- additional costs ride along.
            --
            -- INSTEAD of the printed cost, the plotted arm's argument above:
            -- rule 702.143a is the only thing permitting this cast.
            --
            -- A card foretold with NO foretell cost yields no candidate at all,
            -- so CR 601.3's default prohibition arrives through the affordability
            -- gate. That is unreachable from this module's own writer -- CR
            -- 116.2h exiles only a card WITH foretell -- and is CR 702.143d's
            -- shape, which pawl cannot state (#1486).
            Zone.Exile
              | Maybe.isJust (Object.foretold obj) ->
                  fmap (untagged . withAdditional) (Maybe.maybeToList (Keyword.foretellCost (Face.keywords face)))
            -- CR 118.9's OTHER half -- "or applied to it from another effect" --
            -- as a STANDING grant: Omniscience's "you may cast spells from your
            -- hand without paying their mana costs" is a player-scoped
            -- alternative cost that no per-card list can hold, because the
            -- effect never names the cards it will apply to. The one-shot half
            -- is Effect.OfferCast, which hands CastOffer.withoutPayingManaCost
            -- to a card its own resolution already chose.
            --
            -- APPENDED to the hand's ordinary list rather than replacing it,
            -- which is the graveyard arm's Yawgmoth's Will argument in the other
            -- direction: CR 118.9a lets the controller announce which single
            -- alternative cost they pay, so a Fireblast under this grant still
            -- gets to sacrifice two Mountains and a flashback card cast free
            -- still has flashback in the graveyard afterwards. Last, so that
            -- `firstOffered` and Replay's "the printed cost comes first" both
            -- read what they read today.
            --
            -- UNTAGGED, for the reason `untagged` states: no keyword ability
            -- offered it, so paying it satisfies neither CR 702.34a's clause nor
            -- CR 702.133a's.
            --
            -- CR 107.3b's "the only legal choice for X is 0" falls out rather
            -- than being enforced: withoutPayingManaCost carries an empty
            -- ManaCost, which has no variable, so Pawl.Engine.Cast.castProposed
            -- never raises Prompt.ChooseX for this candidate.
            --
            -- The OWNER is the caster here, exactly as the graveyard arm argues
            -- above: CR 400.1 files a hand by player and Cast.zoneCandidates
            -- hands out only the caster's own.
            Zone.Hand ->
              fmap untagged (printed : alternatives)
                <> [untagged (withoutPayingManaCost face) | PlayerEffect.mayCastFromHandWithoutPayingManaCost (Object.owner obj) oid gs]
            _ -> fmap untagged (printed : alternatives)
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
-- each one lands on is applyAdjustments's business.
--
-- CR 601.2f's LOCK-IN belongs to the caller and not to this function, which is
-- only the totalling as a function of state. Pawl.Engine.Cast.castProposed and
-- Pawl.Engine.Activate.activateAbility each total the cost ONCE per announcement
-- -- through totalWith, over the adjustments CR 118.7e's prompt resolved -- and
-- hand the resulting VALUE to `pay` below, which never re-reads the state for it
-- -- so an effect that would change the total after that point does nothing.
-- Pawl.CostSpec's Altar's Reap group is what proves it: the creature paying the
-- additional cost is the cost reducer, so a total re-read after CR 601.2h's
-- sacrifice costs a mana more.
--
-- CR 118.6a's first sentence needs no special case: fmap over the Maybe leaves
-- Nothing as Nothing.
total :: PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Cost Keyword.Type.Keyword
total pid oid cost gs = totalWith (spellAdjustments pid oid gs) cost

-- CR 601.2f's increases and reductions for a SPELL being cast: the ones CARDS
-- generate (Pawl.Engine.PlayerEffect, plus the spell's own text through
-- selfReductions below) plus the one the RULES do, CR 903.8's commander tax.
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
--
-- The spell's OWN reductions are APPENDED to the ones the battlefield generates
-- rather than being applied first or last. Rule 601.2f lets the player apply
-- multiple reductions in any order, and applyAdjustments' descending-floor sort
-- is what fixes the order pawl actually takes (#88); every reduction gathered
-- here states no floor, so where they land in the list changes no total.
spellAdjustments :: PlayerId -> ObjectId -> GameState -> CostAdjustments.CostAdjustments
spellAdjustments pid oid gs =
  let adjustments = PlayerEffect.spellCostAdjustments pid oid gs
      withSelf =
        adjustments
          { CostAdjustments.reductions =
              CostAdjustments.reductions adjustments <> fmap (\amount -> (amount, 0)) (selfReductions pid oid gs)
          }
      commanderTax = Commander.tax pid oid gs
   in if commanderTax == 0
        then withSelf
        else withSelf {CostAdjustments.increases = commanderTax : CostAdjustments.increases withSelf}

-- CR 601.2f / 113.6d: the reductions a spell's OWN printed text applies to its
-- own cost -- Thrasta, Tempest's Roar's "This spell costs {3} less to cast for
-- each other spell cast this turn" -- with each one's Quantity evaluated and
-- its amount repeated that many times.
--
-- REPEATED rather than multiplied, which is what makes a typed amount fall out
-- with no arithmetic of its own: applyAdjustments reads a reduction's generic
-- symbols as a sum and cancels its typed ones one for one, so three copies of
-- {3} is {9} of generic reduction and three copies of a {W} would cancel three
-- white symbols. Zero copies is the empty list, which is {0} and reduces
-- nothing; a Quantity that comes out NEGATIVE takes the same floor, since a
-- reduction of a negative amount is not an increase (CR 601.2f orders the two
-- and does not let one become the other).
--
-- An UNDETERMINABLE Quantity (Nothing) contributes nothing at all, which is the
-- direction that leaves the spell dearer rather than cheaper -- the same answer
-- Pawl.Engine.Quantity gives every other reader that cannot resolve a
-- reference.
--
-- The face comes straight off the object rather than through a projection, for
-- Pawl.Types.CostReduction's reason (#160). It is the face the CAST has already
-- singled out: Cast.asProposed stamps the chosen half before any of this
-- module's gates run, so a split card is priced from the half being cast (CR
-- 709.3b) and a face-down proposal reads Card.faceDownFace, which prints none of
-- these (CR 708.2a).
--
-- Read LIVE, at the moment CR 601.2f determines the total, and never from a
-- snapshot taken earlier: this is a pure function of the `gs` its caller hands
-- it, and Cast.castProposed hands it the post-CR-601.2a state. The lock-in the
-- rule then imposes is the CALLER's -- see `total` above -- so the value is
-- computed once from live state and then frozen, rather than being frozen
-- before it was ever computed.
selfReductions :: PlayerId -> ObjectId -> GameState -> [ManaCost.ManaCost]
selfReductions pid oid gs =
  let -- The evaluation's perspective is the CASTER's (CR 109.5: "its would-be
      -- controller, if a player is attempting to cast it"), which for a spell
      -- being cast is `pid` -- and not Projection.controllerOf, which answers
      -- Nothing for a card still in a hand. The source is the spell itself,
      -- since the reduction is printed on it: a Filter.IsSource written inside
      -- one of these counts names the spell being cast.
      context = Filter.contextFor (Just pid) (Just oid)
      scaled reduction =
        let copies = Quantity.evaluate (Projection.fullView gs) context gs oid (CostReduction.perEach reduction)
            -- Saturating rather than partial: an Int cannot hold every Integer,
            -- and a count that overflowed one would be a reduction no cost could
            -- survive anyway. A negative saturates to 0, which is the floor the
            -- header states.
            times n = concat (replicate (max 0 (Integer.toIntSaturating n)) (ManaCost.unwrap (CostReduction.amount reduction)))
         in fmap (ManaCost.MkManaCost . times) copies
   in case Game.faceOf oid gs of
        Nothing -> []
        Just face -> Maybe.mapMaybe scaled (Face.costReductions face)

-- CR 601.2f's adjustments for an ACTIVATION cost, which CR 602.2b routes through
-- rule 601.2b-i like a spell's. Heartstone's floored reduction and Blossoming
-- Tortoise's unfloored one are what reach it (#90); no commander tax, since CR
-- 903.8 taxes CASTING a commander and an activation is not a cast.
--
-- Not reached by a MANA ability's cost, which pays through manaActivations rather
-- than through Pawl.Engine.Activate -- and unobservably so, since every mana
-- ability in the pool has an empty mana part for a reduction to take from (#1120).
--
-- `srcId` is the ability's SOURCE PERMANENT, which is what
-- PlayerEffect.activationCostAdjustments matches the criterion against -- the
-- same argument position a spell's own object takes above, and deliberately: what
-- differs between the two moments is the constructor gathered, never the call
-- site (#90).
activationAdjustments :: PlayerId -> ObjectId -> GameState -> CostAdjustments.CostAdjustments
activationAdjustments = PlayerEffect.activationCostAdjustments

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
adjustmentResolutions :: CostAdjustments.CostAdjustments -> [CostAdjustments.CostAdjustments]
adjustmentResolutions adjustments =
  let resolveOne symbol = case reductionHalvesOf symbol of
        Nothing -> [symbol]
        Just [] -> [symbol]
        Just halves -> halves
      resolveAll (ManaCost.MkManaCost symbols, floor_) = fmap (\xs -> (ManaCost.MkManaCost xs, floor_)) (traverse resolveOne symbols)
   in fmap
        (\reductions -> adjustments {CostAdjustments.reductions = reductions})
        (traverse resolveAll (CostAdjustments.reductions adjustments))

-- The same totalling over adjustments the CALLER already has, which is what CR
-- 118.7e's prompt needs: `announceReductions` asks the payer which half of each
-- hybrid symbol in a reduction it takes, and the answers have to reach
-- applyAdjustments rather than being read out of the game state a second time.
-- `total` above is this over the adjustments as they stand unannounced, which no
-- caller in the engine asks for; `totalManas` below is it over every resolution.
totalWith :: CostAdjustments.CostAdjustments -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
totalWith adjustments cost = cost {Cost.mana = fmap (applyAdjustments adjustments) (Cost.mana cost)}

-- CR 601.2f's "plus all additional costs", the half that is not mana: the
-- components an effect ADDS to a cost, appended to the ones the cost prints
-- (Brutal Suppression's "Sacrifice a land" onto a Rebel's activation cost, by CR
-- 602.2b).
--
-- SEPARATE from `totalWith` above rather than folded into it, and the separation
-- is what keeps the components from being added twice: the gate measures a cost
-- before CR 601.2b's completion (`canPaySomeCompletion` takes the mana totalling
-- as a FUNCTION and never a whole cost), while `totalWith` runs on the announced
-- cost afterwards. Both moments need the components, so this is applied at the
-- earlier one and `totalWith` leaves them alone -- see Pawl.Engine.Activate,
-- which is the only caller of either that has adjustments carrying any.
--
-- APPENDED, so a printed component is paid before an added one absent a payer's
-- reordering -- and CR 601.2h makes the order the payer's anyway
-- (`payComponents` prompts for it whenever it is observable). The LOYALTY
-- components are the exception, merged rather than appended by `combineLoyalty`
-- below: CR 606.5 makes them one cost, so there is no order between them for CR
-- 601.2h to offer.
--
-- A no-op for every SPELL cost, whose adjustments carry no components at all
-- (Pawl.Engine.PlayerEffect.spellCostAdjustments).
--
-- Applied AFTER `substituteX`, which is why an ADDED component may not carry CR
-- 601.2b's X: a CostComponent.PayLifeX arriving this way would never be
-- substituted, and `canPayComponent` would refuse the whole cost. No effect in
-- the pool adds one -- CR 601.2b announces the variables of the cost as printed,
-- and an increase that named an unannounced X would be an amount nobody chose --
-- so this is a bound on the open half rather than an elision.
--
-- CR 606.5's combining runs here, once the additions are on: this is the single
-- funnel both moments go through -- the gate (Pawl.Engine.Activate.payableCostAt)
-- and the cost `Cost.pay` will charge (Pawl.Engine.Activate.activateAbility
-- announces through it) -- so the combined cost is the one measured AND the one
-- paid. `combineLoyalty` is the identity on every cost carrying at most one
-- loyalty component, which is every printed one.
plusComponents :: CostAdjustments.CostAdjustments -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
plusComponents adjustments cost = cost {Cost.components = combineLoyalty (Cost.components cost <> CostAdjustments.components adjustments)}

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
--
-- Takes the ADJUSTMENTS rather than gathering them, so the two moments CR 601.2f
-- reaches share one totalling: Pawl.Engine.Cast hands it `spellAdjustments` and
-- Pawl.Engine.Activate hands it `activationAdjustments` (#90). Nothing about the
-- arithmetic below knows which it was given.
totalManas :: CostAdjustments.CostAdjustments -> ManaCost.ManaCost -> [ManaCost.ManaCost]
totalManas adjustments =
  let resolutions = adjustmentResolutions adjustments
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

-- CR 601.2b: substitute the chosen value of X everywhere in this cost -- the mana
-- part's ManaSymbol.Variable, and the components' CostComponent.PayLifeX.
-- Identity on a cost that declares no X, and on an unpayable one.
--
-- BOTH halves, because CR 107.3a gives one announced value to "a mana cost,
-- alternative cost, additional cost, and/or activation cost with an {X}, [-X], or
-- X in it" -- Hatred's X is the same X whichever half of the cost it sits in, and
-- CR 107.3i keeps the two equal.
substituteX :: Natural -> Cost Keyword.Type.Keyword -> Cost Keyword.Type.Keyword
substituteX x cost =
  cost
    { Cost.mana = fmap (Mana.substituteX x) (Cost.mana cost),
      Cost.components = fmap (substituteXInComponent x) (Cost.components cost)
    }

-- EXHAUSTIVE with no wildcard, this module's posture for every CostComponent
-- match: a new component that could carry CR 601.2b's X owes an answer here, and
-- -Werror is what makes it.
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
  CostComponent.DiscardThis -> component
  CostComponent.PayEnergy _ -> component
  CostComponent.AddLoyaltyToThis _ -> component
  CostComponent.RemoveLoyaltyFromThis _ -> component
  CostComponent.PutPlusOneCountersOnThis _ -> component
  -- Not this arm's X: Soul Immolation's "blight X" is announced under a bound
  -- rule 701.68a does not state, so it has no spelling here at all (gap #1646).
  CostComponent.Blight _ -> component
  CostComponent.ExileThisFromGraveyard -> component
  CostComponent.ExileCardsFromGraveyard {} -> component
  CostComponent.ExileTopFromGraveyard _ -> component

-- Does this cost contain an X (CR 107.3)? What decides whether the caster is
-- asked for a value at CR 601.2b -- a spell with no X is not asked, and CR 602.2b
-- sends an activation cost through the same question.
--
-- BOTH HALVES, mana part and components. CR 601.2b's "a variable cost that will
-- be paid as it's being cast (such as an {X} in its mana cost)" names the mana
-- cost as an EXAMPLE rather than as the rule, and CR 107.3a lists the additional
-- cost beside the mana cost -- so Hatred, whose only X is in "pay X life", is
-- asked exactly as Blaze is.
hasVariable :: Cost Keyword.Type.Keyword -> Bool
hasVariable cost = manaHasVariable || any componentHasVariable (Cost.components cost)
  where
    manaHasVariable = case Cost.mana cost of
      Nothing -> False
      Just (ManaCost.MkManaCost symbols) -> elem ManaSymbol.Variable symbols

-- substituteXInComponent's predicate half, and exhaustive for its reason: the
-- two must agree, since a component this answers False for is one no announcement
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
  CostComponent.DiscardThis -> False
  CostComponent.PayEnergy _ -> False
  CostComponent.AddLoyaltyToThis _ -> False
  CostComponent.RemoveLoyaltyFromThis _ -> False
  CostComponent.PutPlusOneCountersOnThis _ -> False
  CostComponent.Blight _ -> False
  CostComponent.ExileThisFromGraveyard -> False
  CostComponent.ExileCardsFromGraveyard {} -> False
  CostComponent.ExileTopFromGraveyard _ -> False

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
-- `substituteX` is what makes the demand grow, on BOTH halves of the cost: the
-- generic mana Mana.substituteX writes, and the CostComponent.PayLife this
-- module's substituteXInComponent writes for a CostComponent.PayLifeX. A half
-- that took the value and charged nothing for it would leave the climb without a
-- bound -- Pawl.CostSpec's "Hatred is asked for X, bounded by the life its cost
-- can pay" is the case that stops running at all if the life half ever does.
--
-- Answers 0 for a cost with no X in it, a totality guard rather than a rule:
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
-- callers total against different adjustments: Pawl.Engine.Cast passes
-- `totalManas (spellAdjustments …)` and Pawl.Engine.Activate passes
-- `totalManas (activationAdjustments …)`. Either way it is the SAME cost the
-- caller's own payability gate measured -- against the printed cost instead, a
-- reduction could hide a route and this function elide the prompt, which is the
-- failure #416 named for spells and #90 kept off the activation path only by
-- there being no reduction to reach it.
--
-- It answers a LIST because CR 118.7e's choice of half is not made until CR
-- 601.2f, one step after this: a route is offered where SOME resolution of the
-- reductions pays it, which is exactly what the gate asks.
--
-- The COST that arrives already carries CR 601.2b's announced value of X, since
-- that rule puts the value of the variable before this announcement. Named
-- `total_` only because this module's own `total` is in scope.
--
-- `spending` is CR 118.14's permission, and it belongs here for the reason
-- `total` does: this offer measures payability, so a payer who may spend mana of
-- any type must be offered the routes that permission opens -- otherwise the
-- gate and the announcement disagree again, one resource over.
announce :: ManaSpending.ManaSpending -> PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> Game (Cost Keyword.Type.Keyword)
announce spending pid oid total_ cost = case Cost.mana cost of
  -- CR 118.6: an object with no mana cost has no mana symbols to announce.
  Nothing -> pure cost
  Just manaCost -> do
    -- The components' claims are read here rather than inside Mana.announce,
    -- which cannot reach removalClaim: this module imports that one, not the
    -- other way about. Nothing announcing changes the board, so reading them once
    -- is the same answer every offer would have got.
    gs <- State.get
    (announced, life) <- Mana.announce manaActivations spending pid oid total_ (lifeOwedBy (Cost.components cost)) (claimsOf pid oid (Cost.components cost) gs) manaCost
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
-- The INCREASES and the FLOOR ride through untouched. CR 118.7e is a rule about
-- which half of a reduction's symbol applies: pawl's increases are amounts of
-- generic mana with no symbol to choose halves of, and a floor is a limit on the
-- result rather than an amount of mana at all.
--
-- Takes the ADJUSTMENTS the caller gathered rather than gathering its own, which
-- is what makes the announced reduction the one that will be applied: the caller
-- hands the same record to `totalWith` afterwards, and the two moments CR 601.2f
-- reaches can hand in different ones (#90). A no-op for the SPELL path it used to
-- gather for itself -- CR 903.8's tax is folded into the candidate before CR
-- 601.2a's move (Cast.castProposed) and Commander.tax answers 0 for a spell on the
-- stack, so `spellAdjustments` and the bare player-effect gathering agree here.
announceReductions :: PlayerId -> ObjectId -> GameState -> CostAdjustments.CostAdjustments -> Game CostAdjustments.CostAdjustments
announceReductions pid oid gs adjustments =
  let chooseOne symbol = case reductionHalvesOf symbol of
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
      chooseAll (ManaCost.MkManaCost symbols, floor_) = fmap (\xs -> (ManaCost.MkManaCost xs, floor_)) (traverse chooseOne symbols)
   in fmap
        (\reductions -> adjustments {CostAdjustments.reductions = reductions})
        (traverse chooseAll (CostAdjustments.reductions adjustments))

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
  ManaSymbol.Hybrid (Hybrid.MkHybrid a b) -> Just (List.nub [ManaSymbol.OfType a, ManaSymbol.OfType b])
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
-- TapForTotalPower and TapPermanents are deliberately NOT here, and the omission
-- is the rule rather than an oversight: those components tap OTHER permanents by
-- written instruction, and rule 302.6 gates a creature's own activated ability
-- with the tap SYMBOL in it. A Vehicle that arrived this turn may be crewed and a
-- summoning-sick creature may be tapped for Springleaf Drum; rule 302.6's second
-- sentence still stops either attacking.
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
isLoyaltyComponent = Maybe.isJust . loyaltyAmountOf

-- The SIGNED amount of loyalty a component moves, positive for the adding half
-- of CR 606.4 and negative for the removing half, or Nothing for a component
-- that is not a loyalty cost at all. `isLoyaltyComponent` above is this asked
-- without the number, so the two answers cannot drift apart.
--
-- An Integer and not a Natural: CR 606.5's combining sums the two halves against
-- each other, and the intermediate is signed even though each component's own
-- payload is not.
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
  CostComponent.DiscardThis -> Nothing
  CostComponent.PayEnergy _ -> Nothing
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing
  CostComponent.Blight _ -> Nothing
  CostComponent.ExileThisFromGraveyard -> Nothing
  CostComponent.ExileCardsFromGraveyard {} -> Nothing
  CostComponent.ExileTopFromGraveyard _ -> Nothing

-- CR 606.5: "If the total cost to activate a loyalty ability contains multiple
-- costs to add or remove loyalty counters, those costs are combined into a single
-- cost to add or remove loyalty counters, as appropriate." Carth the Lion's added
-- [+1] on Jace Beleren's printed [-10] is one cost of -9, which 9 loyalty pays --
-- where the pair asked separately is refused, since canPayComponent's CR 606.6
-- arm measures each against the counters present before any of the cost is paid.
-- The divergence it fixes runs pawl STRICTER than the rules.
--
-- ONE component whenever the cost had any, even at a net of zero, rather than
-- dropping the pair. Three readers turn on it: rule 606.5 itself says "combined
-- into a single cost" and not "into none"; CR 606.4 puts the counters on "that
-- permanent", whose battlefield-and-control floor is canPayComponent's loyalty
-- arms and would go unasked if nothing were emitted; and `isLoyaltyCost` stays
-- true of the totalled cost. Paying AddLoyaltyToThis 0 adds no counters, so the
-- choice is unobservable on the board -- it is the reading, not an outcome.
--
-- The combined component takes the FIRST loyalty component's position, so a cost
-- printing one loyalty symbol beside other parts comes back in its printed order
-- and CR 601.2h's prompt sees the same list it saw before.
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
  -- LOAD-BEARING since CR 702.29b and CR 702.77b put the minted cycling and
  -- reinforce abilities into the projection for every zone: this is the answer
  -- Activate.functionsIn reads to keep a Rustic Clachan on the battlefield from
  -- offering its reinforce ability.
  CostComponent.DiscardThis -> Just Zone.Hand
  CostComponent.ExileThisFromGraveyard -> Just Zone.Graveyard
  CostComponent.TapThis -> Nothing
  CostComponent.UntapThis -> Nothing
  CostComponent.SacrificeThis -> Nothing
  CostComponent.PayLife _ -> Nothing
  CostComponent.PayLifeX -> Nothing
  CostComponent.Sacrifice {} -> Nothing
  -- CR 702.122a taps permanents on the battlefield and moves nothing out of any
  -- zone, so CR 113.6m says nothing and CR 113.6's default stands.
  CostComponent.TapForTotalPower {} -> Nothing
  -- TapForTotalPower's answer for its reason: this taps permanents that stay on
  -- the battlefield, so CR 113.6m says nothing about it either.
  CostComponent.TapPermanents {} -> Nothing
  -- Nothing, and NOT Just Zone.Graveyard -- the one place this component parts
  -- from ExileThisFromGraveyard above. CR 113.6m is about an ability that "moves
  -- THE OBJECT IT'S ON out of a particular zone"; this one moves OTHER cards,
  -- chosen from the payer's graveyard, and leaves the object carrying the cost
  -- exactly where it was. So CR 113.6m does not reach it and CR 113.6's default
  -- stands -- the same reading TapForTotalPower gets just above. Answering the
  -- graveyard here would make an activated ability with this cost unactivatable
  -- from the battlefield, which no rule asks for.
  CostComponent.ExileCardsFromGraveyard {} -> Nothing
  -- ExileCardsFromGraveyard's answer for its reason: this too moves cards other
  -- than the object the cost is on, so CR 113.6m does not reach it.
  CostComponent.ExileTopFromGraveyard _ -> Nothing
  CostComponent.DiscardCards {} -> Nothing
  CostComponent.PayEnergy _ -> Nothing
  CostComponent.AddLoyaltyToThis _ -> Nothing
  CostComponent.RemoveLoyaltyFromThis _ -> Nothing
  -- CR 122.6 puts counters on a permanent already where it is, so nothing moves
  -- out of any zone and CR 113.6m does not reach this either.
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing
  -- PutPlusOneCountersOnThis's answer for its reason: CR 122.6 puts the counters
  -- on a creature already on the battlefield, so nothing moves out of any zone.
  CostComponent.Blight _ -> Nothing

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
-- own order, narrowed by the criterion and minus `oid` itself. See
-- canPayComponent's DiscardCards arm for why the exclusion is CR 601.2a and not
-- a convenience. Hand order rather than sorted, unlike
-- Replacement.sacrificeCandidates: Game.zoneMembers already returns a hand in a
-- fixed order, which Prompt.ChooseDiscard offers it in.
--
-- Matched against the PRINTED card and never a projection, exileCandidates'
-- reading below and its context -- the payer as perspective, no source -- for
-- its reasons.
--
-- Not implemented: a card in a hand has a projection too, so a continuous effect
-- that changed the axis this criterion reads is missed here (#160).
discardCandidates :: PlayerId -> ObjectId -> Filter.Type.Filter Keyword.Type.Keyword -> GameState -> [ObjectId]
discardCandidates pid oid criterion gs =
  let context = Filter.contextFor (Just pid) Nothing
      matches candidate = case Game.faceOf candidate gs of
        Nothing -> False
        Just face -> Filter.matches context (Projection.viewOfCardIn gs candidate face) criterion
   in filter (\candidate -> candidate /= oid && matches candidate) (Game.zoneMembers Zone.Hand pid gs)

-- The cards this player may exile to pay an ExileCardsFromGraveyard component:
-- their OWN graveyard, in its own order, narrowed by the criterion.
--
-- Per-owner, and that is CR 400.3 with CR 108.4: a graveyard is not a shared
-- zone, and a card in one has no controller for a control-shaped gate to read,
-- so Game.zoneMembers Zone.Graveyard pid is the whole of "your graveyard".
--
-- Matched against the PRINTED card and never a projection: Projection.viewOfCardIn
-- is the view -- printed on every axis but CR 208.2a's characteristic-defining
-- power, which functions in a graveyard too -- and a candidate whose card cannot
-- be found matches nothing.
--
-- Not implemented: a graveyard card HAS a projection, so a continuous effect
-- that changed the axis this criterion reads is missed here (#160).
-- The context carries the
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

-- The permanents this player may tap to pay a TapForTotalPower or TapPermanents
-- component on `oid`: every battlefield object matching the criterion, ascending,
-- the order Replacement.sacrificeCandidates offers its own in.
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
-- payComponent's TapThis arm for why -- shared by the components that tap,
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
  CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) ->
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
  CostComponent.DiscardCards (DiscardCards.MkDiscardCards n criterion) ->
    claim Zone.Hand (Set.fromList (discardCandidates pid oid criterion gs)) n
  CostComponent.DiscardThis -> claim Zone.Hand (itself (isOwnedIn Zone.Hand)) 1
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) ->
    claim Zone.Graveyard (Set.fromList (exileCandidates pid criterion gs)) n
  -- A pool of at most ONE, and the claim is on that one card rather than on a
  -- choice among several: CR 404.2's order picks it. An empty pool is how this
  -- arm spells the refusal canPayComponent gives below.
  CostComponent.ExileTopFromGraveyard criterion ->
    claim Zone.Graveyard (Set.fromList (Maybe.maybeToList (topExileCandidate pid criterion gs))) 1
  CostComponent.ExileThisFromGraveyard -> claim Zone.Graveyard (itself (isOwnedIn Zone.Graveyard)) 1
  CostComponent.TapThis -> Nothing
  CostComponent.UntapThis -> Nothing
  CostComponent.TapForTotalPower {} -> Nothing
  -- Nothing, TapForTotalPower's answer and for the reason stated above: a
  -- tapped permanent stays on the battlefield, so this component takes nothing
  -- out of any pool. Not implemented: two such components -- two Springleaf
  -- Drums, or one cost carrying two -- therefore each count the SAME untapped
  -- creature, so a board holding fewer untapped creatures than the tapping
  -- costs that want them reads as payable, and the second payment goes Unpaid
  -- at CR 601.2h (#1718).
  CostComponent.TapPermanents {} -> Nothing
  CostComponent.PayLife _ -> Nothing
  CostComponent.PayLifeX -> Nothing
  CostComponent.PayEnergy _ -> Nothing
  CostComponent.AddLoyaltyToThis _ -> Nothing
  CostComponent.RemoveLoyaltyFromThis _ -> Nothing
  CostComponent.PutPlusOneCountersOnThis _ -> Nothing
  -- Nothing, though this one DOES pick an object out of a pool: a claim is what a
  -- component takes OUT of a zone, and CR 701.68a takes nothing -- the chosen
  -- creature stays where it is. Two blights in one cost may therefore choose the
  -- same creature, which is right: CR 122.6 stacks counters.
  CostComponent.Blight _ -> Nothing
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
    -- CR 118.14's permission is a CAST's (rule 118.14 scopes it to the spells an
    -- effect permitted), and no caller of this one is casting: what is left here
    -- is a special action's cost and CR 118.12's resolution-time payment, so the
    -- mana is spent as it is.
    Mana.canPayCommitting manaActivations ManaSpending.AsProduced pid (lifeOwedBy (Cost.components cost)) (claimsOf pid oid (Cost.components cost) gs) manaCost gs
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
-- And WHAT ONE ACTIVATION SPENDS, alongside the count, because the count alone is
-- a fact about this source in isolation: two sources whose costs both sacrifice a
-- creature each answer 1 beside one creature, and two whose costs each pay 3 life
-- both answer 2 at 6 life. Only the claims say the first pair cannot both have the
-- creature (#1126), and only the life says the second pair cannot both have the
-- life (Mana.payableResolutionsGiven). Both are one activation's, unscaled -- the
-- reader multiplies by however many it takes.
--
-- `pcs` is the pre-projected board CR 302.6's two reads want; Map.empty asks for
-- a fresh projection, which is what a caller with no sweep in hand passes (#200).
manaActivations :: Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> GameState -> Activations.Activations
manaActivations pcs pid oid cost gs =
  if Maybe.isJust (Cost.mana cost)
    && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)
    && jointlyPayable pid oid (Cost.components cost) gs
    && sicknessOkGiven pcs pid oid cost gs
    -- CR 701.35a's third clause, on the half of it CR 702.61b's split second
    -- exempts and rule 701.35a does not: "its activated abilities can't be
    -- activated" reaches a mana ability too. Here rather than in
    -- Mana.manaSourcesGiven, because this is what BOTH of CR 605.3a's windows
    -- consult -- the offer sweeps sources through it, and Cost.tapForMana asks it
    -- again per option at the payment -- which is sickness's position one line up.
    && not (Detain.detained oid gs)
    then
      Activations.MkActivations
        { Activations.times = repeatsOf pid oid cost gs,
          Activations.claims = claimsOf pid oid (Cost.components cost) gs,
          Activations.life = lifeOwedBy (Cost.components cost)
        }
    else Activations.MkActivations {Activations.times = 0, Activations.claims = [], Activations.life = 0}

-- How many times IN A ROW a cost already known to be payable once could be paid,
-- which is what makes Ashnod's Altar beside two creatures two mana activations
-- rather than one (#1128), and Treasonous Ogre ("Pay 3 life: Add {R}") at 20 life
-- six.
--
-- The SMALLEST ceiling the cost's resources impose, since CR 118.3's "fully" is
-- what limits a repetition and every resource limits it at once. Two of them are
-- counted, each totalled over the WHOLE cost rather than per component:
--
--   1. OBJECTS, through Pawl.Engine.Claim.repeats: a cost whose components take
--      objects out of zones can be paid as many times as those zones hold objects
--      for it, jointly, so two components drawing on one pool do not each get it.
--
--   2. LIFE, through CR 119.4. k repetitions of a cost that pays n life are one
--      demand for k*n on one life total: each single payment wants the total at n
--      or more, and a total covering k*n covers all k in turn whatever the order,
--      so the ceiling is the total divided by n. `lifeOwedBy` is the n, which is
--      why two PayLife components repeat together and not each.
--
-- Anything else caps the answer at 1 (`uncountedCeiling`), and so does a MANA
-- part, since repeating it would spend mana this walk has not measured (#1120).
-- Exact for CR 107.5's {T} -- a tapped permanent cannot pay it again -- and an
-- understatement for the rest.
--
-- Understating is the safe direction and the reason the uncounted components cap
-- rather than divide: a supply short by one refuses a cost the payment loop could
-- have paid, while a supply too large offers a cast that then cannot be paid --
-- and an offer that changes nothing is offered again forever. It is also why NO
-- ceiling at all answers 1: a cost that spends nothing this function can see is
-- limited by something it cannot.
--
-- Both ceilings are this source asked ALONE against the untouched board, and
-- Pawl.Engine.Mana re-asks the joint question across sources afterwards. They
-- cannot disagree with it, because each is the largest k that question could
-- admit for one source with nothing else spending: Pawl.Engine.Claim.repeats is
-- the largest k Hall's condition admits, and k times the life is what
-- payableResolutionsGiven's CR 119.4 clause totals. Where something else IS
-- spending -- a second life-paying source, or life the cost itself owes -- the
-- ceiling here is loose and that clause is what tightens it.
--
-- The pool is read ONCE, off the untouched board, which is exact for every
-- criterion in the pool: taking one creature out of it leaves the rest
-- creatures.
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
-- carry, or Nothing where one of them does.
--
-- 1 for every resource this module cannot count, and for two it need not. EXACT
-- for CR 107.5's {T} and CR 107.6's {Q} -- a permanent already tapped cannot be
-- tapped again to pay one, nor an untapped one untapped -- and for CR 606.4's
-- loyalty, since CR 606.3 lets a player activate one loyalty ability of a
-- permanent per turn whatever the counters allow. An UNDERSTATEMENT for CR
-- 107.14's energy, for a counter put on the source, and for the two components
-- that tap OTHER permanents, each of which a player with enough could pay several times
-- over; `repeatsOf` above argues for understating (#1280).
--
-- EXHAUSTIVE with no wildcard, this module's posture for every CostComponent
-- match: a new component owes an answer about how often it can be paid, and
-- -Werror is what makes it.
uncountedCeiling :: CostComponent.CostComponent Keyword.Type.Keyword -> Maybe Natural
uncountedCeiling component = case component of
  -- Counted by `objectCeiling`: every one of these has a removalClaim.
  CostComponent.Sacrifice {} -> Nothing
  CostComponent.SacrificeThis -> Nothing
  CostComponent.DiscardCards {} -> Nothing
  CostComponent.DiscardThis -> Nothing
  CostComponent.ExileCardsFromGraveyard {} -> Nothing
  CostComponent.ExileTopFromGraveyard _ -> Nothing
  CostComponent.ExileThisFromGraveyard -> Nothing
  -- Counted by `lifeCeiling`, CR 119.4.
  CostComponent.PayLife _ -> Nothing
  -- Zero, and not the 1 the uncounted components take: an unannounced X cannot be
  -- paid even once (`canPayComponent`). Unreachable in fact -- `manaActivations`
  -- asks canPayComponent of every component before it reaches `repeatsOf` -- so
  -- this is the answer that stays true if that guard ever moves.
  CostComponent.PayLifeX -> Just 0
  CostComponent.TapThis -> Just 1
  CostComponent.UntapThis -> Just 1
  CostComponent.TapForTotalPower {} -> Just 1
  CostComponent.TapPermanents {} -> Just 1
  CostComponent.PayEnergy _ -> Just 1
  CostComponent.AddLoyaltyToThis _ -> Just 1
  CostComponent.RemoveLoyaltyFromThis _ -> Just 1
  CostComponent.PutPlusOneCountersOnThis _ -> Just 1
  -- An UNDERSTATEMENT, PutPlusOneCountersOnThis's: a player controlling a creature
  -- can blight as often as they can pay the rest of the cost.
  CostComponent.Blight _ -> Just 1

-- This player's life total as an amount that could be PAID (CR 119.4), which is
-- to say floored at zero: a player at or below 0 life can pay nothing but CR
-- 119.4b's zero. A player the state does not hold reads as 0, which is
-- Event.canPayLife's own refusal.
lifeTotalOf :: PlayerId -> GameState -> Natural
lifeTotalOf pid gs = case Map.lookup pid (GameState.players gs) of
  Nothing -> 0
  Just player -> Integer.toNaturalSaturating (Player.life player)

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
-- reason Pawl.Engine.Cost.announce's is: Pawl.Engine.Cast passes `totalManas`
-- over the SPELL adjustments and Pawl.Engine.Activate passes it over the
-- ACTIVATION ones (#90). CR 601.2f's non-mana additions are not in this
-- parameter at all -- they are on the `cost` that arrives
-- (Pawl.Engine.Cost.plusComponents), since a component is not a mana cost to
-- rewrite.
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
canPaySomeCompletion :: ManaSpending.ManaSpending -> PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPaySomeCompletion spending pid oid total_ cost gs =
  let pcs = Projection.projectAll gs
   in canPaySomeCompletionGiven spending (activationManaSourcesGiven (Projection.controlGrants gs) pcs pid gs) pcs pid oid total_ cost gs

-- The mana sources an ACTIVATION payment is judged against, which is what every
-- gate below hands Mana.payableResolutionsGiven. ONE function pairing
-- `manaActivations` with the sweep taken under it, so a hoisted list cannot be
-- built under a capacity the gate does not read -- the invariant
-- Mana.payableResolutionsGiven states and its type cannot.
activationManaSourcesGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
activationManaSourcesGiven = Mana.manaSourcesGiven manaActivations

-- The same question given a board the CALLER has already walked. The wrapper
-- above reaches Mana.canPayCommitting, which takes one control-grant walk and
-- one whole-board projection per call; Action.legalActions' activation gate asks
-- it once per permanent, so that is a whole-board sweep per permanent (#716).
-- Handing the board in changes no answer -- see Mana.payableResolutionsGiven and
-- the snapshot argument at Projection.projectGiven.
--
-- `sources` rather than the control grants, because the grants were only ever
-- forwarded to Mana.manaSourcesGiven and that sweep is the same for every
-- permanent in one enumeration (#1073). Build it with
-- activationManaSourcesGiven above and nothing else.
--
-- ONLY the mana half gets the pre-walked board. The COMPONENTS are still
-- asked through canPayComponent, whose Sacrifice and TapForTotalPower arms make
-- per-object walks of their own (#1448); no activation cost in the pool carries
-- one. Their CLAIMS do reach the mana side, for `canPay`'s reason (#1134).
canPaySomeCompletionGiven :: ManaSpending.ManaSpending -> [ObjectId] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> (ManaCost.ManaCost -> [ManaCost.ManaCost]) -> Cost Keyword.Type.Keyword -> GameState -> Bool
canPaySomeCompletionGiven spending sources pcs pid oid total_ cost gs = case Cost.mana cost of
  Nothing -> False
  Just (ManaCost.MkManaCost symbols) ->
    let outside = lifeOwedBy (Cost.components cost)
        claimed = claimsOf pid oid (Cost.components cost) gs
        payable (completed, life) =
          any
            (\totalled -> Mana.canPayCommittingGiven manaActivations spending sources pcs pid (outside + life) claimed totalled gs)
            (total_ (ManaCost.MkManaCost completed))
     in any payable (Mana.completions symbols)
          && all (\component -> canPayComponent pid oid component gs) (Cost.components cost)
          && jointlyPayable pid oid (Cost.components cost) gs

-- CR 119.4's payments a cost owes OUTSIDE its mana part, added up -- what CR
-- 118.3 makes the mana part's own life share a total with. A cost with no PayLife
-- component owes 0, which is what leaves every such cost's answer exactly as it
-- was.
--
-- PayLife is the only component that spends a KNOWN amount of life, and
-- PayLifeX the only one that will spend an announced amount -- which is why the
-- latter owes 0 here rather than a guess. This module is the one place that
-- matches a CostComponent constructor, and the match is total so a new
-- life-spending component cannot be added without answering here.
lifeOwedBy :: [CostComponent.CostComponent Keyword.Type.Keyword] -> Natural
lifeOwedBy = sum . fmap lifeOwedByComponent

lifeOwedByComponent :: CostComponent.CostComponent Keyword.Type.Keyword -> Natural
lifeOwedByComponent component = case component of
  CostComponent.PayLife n -> n
  -- 0, because an unannounced X names no amount to owe. Not a claim that this
  -- component is free: `canPayComponent` refuses it outright, so no cost carrying
  -- one is payable for this sum to understate.
  CostComponent.PayLifeX -> 0
  CostComponent.TapThis -> 0
  CostComponent.UntapThis -> 0
  CostComponent.SacrificeThis -> 0
  CostComponent.Sacrifice {} -> 0
  CostComponent.TapForTotalPower {} -> 0
  CostComponent.TapPermanents {} -> 0
  CostComponent.DiscardCards {} -> 0
  CostComponent.DiscardThis -> 0
  CostComponent.PayEnergy _ -> 0
  CostComponent.AddLoyaltyToThis _ -> 0
  CostComponent.RemoveLoyaltyFromThis _ -> 0
  CostComponent.PutPlusOneCountersOnThis _ -> 0
  CostComponent.Blight _ -> 0
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
  -- CR 601.2b: the value of X is announced as the spell is cast, and this is the
  -- component BEFORE that announcement. There is no amount to measure against CR
  -- 119.4, so the answer is no -- CR 601.2 reverses a casting a player cannot
  -- comply with rather than choosing a value on their behalf.
  --
  -- Unreachable from either cast path: `hasVariable` reads the components, so a
  -- cost carrying this is announced, and both paths substitute (`substituteX`)
  -- before they measure or pay. What it is is the fence under that reasoning --
  -- Pawl.CostSpec's "an unannounced X is unpayable" case is the test.
  CostComponent.PayLifeX -> False
  -- CR 701.21a: this player must control at least `n` matching permanents.
  --
  -- This component ALONE, which is not the whole of CR 118.3's question, exactly
  -- as the PayLife arm above is not: two Sacrifice components of one cost can
  -- each find the same permanent here, and `jointlyPayable` is what asks them
  -- together. Kept because a component is asked about on its own terms here, and
  -- it can only ever be the weaker of the two -- it is the singleton subset of
  -- Hall's condition, spelled out.
  CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) ->
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
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n criterion) ->
    sum (fmap (max 0 . (`tapPower` gs)) (tapCandidates pid oid criterion gs)) >= toInteger n
  -- Payable iff the criterion admits at least `count` permanents, which is
  -- Sacrifice's arm read over tapping rather than over sacrificing: the count is
  -- HOW MANY, so the question is a size and not a sum. "Untapped" is not asked
  -- here and is not missing -- CR 107.5's exclusion is not this component's, so
  -- a card that wants it prints it, and Springleaf Drum's criterion carries
  -- `Not IsTapped` itself.
  --
  -- This component ALONE, Sacrifice's caveat and for its reason -- except that
  -- `jointlyPayable` cannot second it, since tapping states no claim
  -- (`removalClaim`).
  CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion) ->
    Natural.length (tapCandidates pid oid criterion gs) >= n
  -- CR 601.2f: payable only if the hand holds at least that many cards the
  -- criterion admits -- Magmatic Insight is uncastable out of a landless hand
  -- however many cards it holds.
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
  CostComponent.DiscardCards (DiscardCards.MkDiscardCards n criterion) ->
    Natural.length (discardCandidates pid oid criterion gs) >= n
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
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) ->
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
  --
  -- Rule 606.6's "taking into account any additional costs" is already answered
  -- by the time this is asked: `plusComponents` combined the printed symbol with
  -- every added one into the single component per CR 606.5, so `n` here is the
  -- total and not one half of it.
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
  -- CR 701.68b: "if a player is given the choice to blight but is unable to put N
  -- -1/-1 counters on a creature they control (usually because they control no
  -- creatures), they can't choose to blight." Read here rather than at the
  -- payment, so the refusal arrives where the rules put it -- CR 601.2h's
  -- "unpayable costs can't be paid", which CR 602.2b hands to an activation. An
  -- activated ability with this cost is never OFFERED
  -- (Pawl.Engine.Activate.activatableGiven has canPay as a conjunct), a spell with
  -- it as an additional cost is uncastable, and CR 118.12's resolution offer is
  -- never raised.
  --
  -- Nothing about `oid` and nothing about N. Rule 701.68a's candidate is qualified
  -- by CONTROL alone -- not by being the object the cost is on, which is the whole
  -- difference from PutPlusOneCountersOnThis above -- and CR 122.6 puts any number
  -- of counters on any creature, so no N is too large for a candidate that exists.
  CostComponent.Blight _ -> Blight.canBlight pid gs

-- CR 601.2g then 601.2h: the mana window first, then the payment. The order the
-- components are paid in is the PAYER'S, and payComponents below is where it is
-- asked for.
--
-- The cost is the one the CALLER determined, taken as a value and never re-read
-- from the game state -- which is CR 601.2f's "locked in" (see `total` above).
-- Paying one part of it therefore cannot change what another part costs. CR
-- 118.14's `spending` arrives the same way and for a sharper reason: the object
-- that granted the payer that permission is not the object being paid for, since
-- CR 601.2a moved the card to the stack before any of this
-- (Pawl.Engine.Cast.spendingFor).
--
-- A payment can still go Unpaid where `canPay` called the cost payable, and that
-- is the player's own doing rather than the engine's: paying one object-removing
-- component takes an object the next one could have used, so an order -- or an
-- answer to the first component's own prompt -- that spends the wrong object
-- loses the payment. The board never goes illegal for it; the restore below
-- makes an Unpaid payment a complete no-op, so what is lost is the activation
-- rather than the game state.
--
-- All or nothing (CR 601.2h: partial payments are not allowed). The entry state
-- is captured and restored on any rejection, so an Unpaid result is a complete
-- no-op even though paying is monadic and a component may prompt. That no-op is
-- what CR 118.12's resolution-time caller rests on too, where an Unpaid result
-- lands on the same branch as a refusal (Pawl.Engine.Resolve.paid).
pay :: ManaSpending.ManaSpending -> PlayerId -> ObjectId -> Cost Keyword.Type.Keyword -> Game Payment.Payment
pay spending pid oid cost = do
  before <- State.get
  case Cost.mana cost of
    -- CR 118.6: attempting to pay an unpayable cost is an illegal action.
    Nothing -> pure Payment.Unpaid
    -- CR 601.2g: payMana PROMPTS for which sources to activate, so it is monadic
    -- and restores the pre-payment state itself when it cannot be paid.
    Just manaCost -> do
      paidMana <- payMana spending pid manaCost
      if not paidMana
        then pure Payment.Unpaid
        else do
          outcome <- payComponents pid oid (Cost.components cost)
          case outcome of
            Payment.Paid -> pure Payment.Paid
            Payment.Unpaid -> do
              State.put before
              pure Payment.Unpaid

-- CR 601.2h: the parts are paid "in any order", and the ORDER IS THE PAYER'S --
-- so this asks for it rather than fixing it. Which order is chosen is
-- observable: Jarad, Golgari Lich Lord's "Sacrifice a Swamp and a Forest" beside
-- one Bayou and one plain Swamp is payable, and a payer who spends the Bayou on
-- the Swamp half loses it -- which paying the Forest half first denies them.
--
-- Asked ONCE for the whole cost rather than once per part. Nothing is lost:
-- each component's own prompts are still issued as that component is paid, so a
-- payer still chooses which Swamp to sacrifice knowing what the earlier parts
-- took.
--
-- ONE pass, where CR 601.2h states two. The second pass takes the parts that
-- involve a random element or move an object from a library to a public zone,
-- and this vocabulary has none -- see `orderSensitive` below, whose exhaustive
-- case is where a component that did would land.
--
-- FILTERED, NOT TRUSTED, payComponent's posture: Game.permute keeps the printed
-- order for an answer that is not a permutation of the offered indices.
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
  [] -> pure Payment.Paid
  component : rest -> do
    outcome <- payComponent pid oid component
    case outcome of
      Payment.Unpaid -> pure Payment.Unpaid
      Payment.Paid -> payInOrder pid oid rest

-- CR 601.2h: can this cost's payer tell one order from another? Two conditions,
-- and the prompt above is asked only when both hold.
--
-- TWO OR MORE parts that touch objects, `orderSensitive` below. A part that
-- spends only a per-player scalar is inert with respect to every other part:
-- nothing in this vocabulary reads a life total or an energy count, and CR
-- 118.3's check totals what the whole cost spends of each (canPay's `lifeOwedBy`
-- above), so no ordering of them pays where another fails.
--
-- And NOT ALL EQUAL, the elision Prompt.OrderTriggers' `interchangeable` makes
-- for its own batch: equal parts draw on one pool for one count each, and each
-- is asked its own choices when its turn comes, so every permutation of them
-- offers the payer the same decisions. A FENCE rather than proven behaviour --
-- no card in the pool prints two identical order-sensitive parts, so dropping
-- this conjunct leaves the suite green.
orderObservable :: [CostComponent.CostComponent Keyword.Type.Keyword] -> Bool
orderObservable components = case filter orderSensitive components of
  first : rest@(_ : _) -> not (all (== first) rest)
  _ -> False

-- Can paying this part change what another part of the same cost can pay with?
-- True for every part that moves an object out of a zone, taps or untaps one, or
-- changes the counters on one -- a criterion reads tap state and counters (a
-- Filter), and TapForTotalPower totals the power counters change.
--
-- False for the per-player scalars, for `orderObservable`'s stated reason.
--
-- EXHAUSTIVE with no wildcard, `removalClaim`'s posture and for its reason: a new
-- component has to answer here, and -Werror is what makes it. A component that
-- involved a random element, or moved an object from a library to a public zone,
-- would want CR 601.2h's second pass as well as an answer here.
orderSensitive :: CostComponent.CostComponent Keyword.Type.Keyword -> Bool
orderSensitive component = case component of
  CostComponent.Sacrifice {} -> True
  CostComponent.SacrificeThis -> True
  CostComponent.DiscardCards {} -> True
  CostComponent.DiscardThis -> True
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
  CostComponent.PayLife _ -> False
  CostComponent.PayLifeX -> False
  CostComponent.PayEnergy _ -> False

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
payMana :: ManaSpending.ManaSpending -> PlayerId -> ManaCost.ManaCost -> Game Bool
payMana spending pid cost = do
  before <- State.get
  paid <- window Set.empty
  Monad.unless paid (State.put before)
  pure paid
  where
    -- What the pool would leave if the cost were paid out of it right now.
    settlement gs = Mana.spend spending (Maybe.fromMaybe 0 (Mana.lifeNeeded manaActivations spending pid cost gs)) cost (Game.poolOf pid gs)
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
      --
      -- Not implemented: the recipient an AddMana payload may NAME. CR 106.4
      -- lets a card fill in whose pool, and this path adds the whole yield here
      -- instead; a resolving ability reads the reference (Resolve's arm). No
      -- mana ability in the pool names anybody (#1673).
      let controller = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
      case filter (\option -> Activations.times (manaActivations Map.empty controller oid (ManaOption.cost option) gs) > 0) (Mana.manaOptionsOf oid gs) of
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
    -- CR 118.14's permission is granted to CAST a spell and never to activate an
    -- ability, so an activation cost is paid with the mana it is (rule 118.14's
    -- last sentence: "to cast spells that way").
    (Payment.Paid, Just manaCost) -> payMana ManaSpending.AsProduced pid manaCost
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
  -- Unpayable, `canPayComponent`'s answer and for its reason: CR 601.2b's value
  -- has not been announced, so there is nothing to subtract. Unpaid rather than a
  -- guessed 0, which CR 601.2h turns into the reversal of the whole casting.
  CostComponent.PayLifeX -> pure Payment.Unpaid
  -- CR 701.21a: the player chooses which of their permanents dies, so this is a
  -- prompt. Elided only when forced -- exactly as many candidates as the count.
  -- Three payable Mountains and a count of two IS asked: they differ in tap
  -- state, counters and attached auras, so "they are all the same" is not a claim
  -- this engine may make.
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
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n criterion) -> do
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
  -- The payer chooses WHICH permanents to tap, so this is a prompt. Sacrifice's
  -- posture rather than TapForTotalPower's: the count is exact, so exactly as
  -- many candidates as the count leaves one legal answer and the prompt is
  -- elided -- where a THRESHOLD would have left a choice among subsets. Two
  -- untapped creatures and a count of one IS asked; they differ in power,
  -- counters and attached auras, and the engine may not decide for the player.
  --
  -- Reject-not-repair, Sacrifice's posture again: an answer that is not a
  -- size-`n` subset of the offered candidates makes the whole payment Unpaid,
  -- which `pay`'s restore turns into a no-op. An already-tapped answer is
  -- refused by the same clause wherever the criterion says `Not IsTapped`,
  -- since `tapCandidates` never offered it.
  --
  -- The tap is a direct edit, TapThis' route and for TapThis' stated reason.
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
        pure Payment.Paid
      else pure Payment.Unpaid
  -- CR 701.9b: the discarding player chooses which cards, so this is a prompt.
  -- Elided only when forced -- exactly as many MATCHING cards in hand as the
  -- count (the same elision the Discard EFFECT makes, #63). The criterion is what
  -- decides that, not the hand's size: Magmatic Insight beside one land and three
  -- other cards asks nothing, since only the land can pay.
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
  -- CR 701.68a's whole procedure, which Pawl.Engine.Blight owns -- this arm knows
  -- only that a cost asked for it. Unpaid on rule 701.68b's board, which
  -- canPayComponent has already refused, so reaching it means the creature left
  -- between the check and the payment.
  --
  -- ByEffect, and the one place this module's CR 614.16 story is not exact.
  -- CR 118.12 pays a blight during a resolution, where the cause is right --
  -- PutPlusOneCountersOnThis's argument just above, unchanged. CR 601.2h pays one
  -- while a spell is being cast or an ability activated, where CR 609.1 has no
  -- resolution for the placement to be the effect of, so CR 614.16's rows should
  -- not reach it and here they do. Unobservable in the pool: rule 614.16's
  -- effect-grain patterns in `data/cards` (Doubling Season, Hardened Scales) all
  -- name +1/+1 counters, and Vorinclex, Monstrous Raider's name a PLAYER, which
  -- CR 614.16 does not gate and which applies to a cost payment either way
  -- (gap #1647).
  CostComponent.Blight n -> do
    blighted <- Blight.blight pid oid n
    pure (if blighted then Payment.Paid else Payment.Unpaid)
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
--    synthetics CR 118.7e-g needed and Thrasta's self-reduction, and no test aims
--    a TYPED one at a cost the spill would reach: Thrasta's {3} and the {S} and
--    {2/B} reductions name no type for the spill to strand, and the {G/P} and
--    {W/B} ones are aimed at costs with no generic component for CR 118.7b-c to
--    move the stranded mana onto.
-- 4. CR 601.2f's floor at {0} needs no special case: ManaCost is a list of
--    symbols and the empty list IS {0}.
-- 5. A REDUCING EFFECT'S OWN FLOOR is applied as that reduction lands, never as a
--    clamp on the pooled result: Heartstone's "This effect can't reduce the mana
--    in that cost to less than one mana" is card text CR 101.1 lets override the
--    rules, and it says THIS EFFECT. A floored reduction beside an unfloored one
--    (Heartstone and Blossoming Tortoise on an animated Mutavault's {1}) is what
--    tells the two readings apart: the floor stops Heartstone taking the last
--    mana and has nothing to say about what the Tortoise then takes, so the cost
--    is {0} and a pooled clamp would leave {1}. The shortfall a floor makes up is
--    GENERIC mana, which is the only mana a floored reduction in the pool takes
--    (every printing of the sentence reduces by generic only, CR 118.7a).
--
--    NEVER RAISES a cost that was already below the floor -- Heartstone's own
--    ruling, "It will not add a {1} to abilities with no generic mana in their
--    activation cost" -- so the requirement is the floor or the mana that
--    reduction found, whichever is smaller.
--
-- Reductions are FOLDED one at a time, in DESCENDING order of floor. CR 601.2f
-- lets the player apply multiple reductions in any order, and once the floors
-- differ the order is observable -- a floored reduction applied first takes its
-- full amount and leaves the unfloored one to finish the cost off, where the same
-- pair the other way round strands a mana. Descending floor is the CHEAPEST order
-- (the more constrained reduction bites while there is still room for it), so
-- pawl takes it and does not ask; the player's choice of a costlier order is the
-- elision (#88). Reductions that state the SAME floor commute, and the sort is
-- stable, so they keep the order they were gathered in.
--
-- The sort itself is a FENCE and not a proved behaviour: no cost the pool can
-- build observes it, since the one board with mixed floors (Pawl.ActivateSpec's
-- UnflooredActivationCostReduction) has a printed {1} that either order empties,
-- and reversing the sort leaves the suite green. What the suite does prove is that
-- each floor binds its own reduction and no other.
--
-- That every reduction applies at all, rather than one of them, is Edgewalker's
-- own ruling.
--
-- Every step's result is CANONICAL: one leading Generic symbol carrying the whole
-- generic component (omitted entirely when it is zero), then the SURVIVING printed
-- typed symbols in their original order. Presentation, not semantics -- Mana.spend
-- sums every generic symbol and matches typed symbols first -- but it is what makes
-- a total cost comparable, and it is what the next reduction in the fold reads.
applyAdjustments :: CostAdjustments.CostAdjustments -> ManaCost.ManaCost -> ManaCost.ManaCost
applyAdjustments adjustments cost =
  let increases = CostAdjustments.increases adjustments
      reductions = CostAdjustments.reductions adjustments
      costGenericOf symbol = case symbol of
        ManaSymbol.Generic n -> n
        ManaSymbol.OfType _ -> 0
        -- CR 107.4e: a colour/colour hybrid is paid with one mana of a stated
        -- type, so it is no part of the generic component CR 118.7a reductions
        -- come off.
        ManaSymbol.Hybrid {} -> 0
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
        ManaSymbol.Hybrid {} -> 0
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
        ManaSymbol.Hybrid {} -> True
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
        ManaSymbol.Hybrid {} -> Nothing
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
        ManaSymbol.Hybrid {} -> Nothing
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
      -- The canonical form point 5's header describes: the generic component as
      -- one leading symbol, then the typed symbols left.
      canonical generic typed = ManaCost.MkManaCost ((if generic == 0 then [] else [ManaSymbol.Generic generic]) <> typed)
      -- Point 1: every increase, onto the generic component, before any reduction.
      raise (ManaCost.MkManaCost symbols) =
        canonical (sum (fmap costGenericOf symbols) + sum increases) (filter isTyped symbols)
      -- ONE reduction, with the floor its own effect states.
      reduce (ManaCost.MkManaCost symbols) (ManaCost.MkManaCost reducingSymbols, floor_) =
        let generic = sum (fmap costGenericOf symbols)
            typed = filter isTyped symbols
            taken = sum (fmap reducingGenericOf reducingSymbols)
            -- Natural subtraction is PARTIAL (it throws on underflow), so the CR
            -- 601.2f floor is also what keeps this total.
            lowered = if generic >= taken then generic - taken else 0
            -- Point 5 above. `survivors` is the typed part the cancellation
            -- leaves, and every typed symbol is at least one mana (CR
            -- 107.4e/107.4f/107.4h), so the mana left in the cost is `lowered`
            -- plus how many of them there are.
            survivors = cancel (Maybe.mapMaybe reducingManaTypeOf reducingSymbols) typed
            typedCount = Natural.length survivors
            required = min floor_ (generic + Natural.length typed)
            floored = if lowered + typedCount >= required then lowered else required - typedCount
         in canonical floored survivors
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
   in List.foldl' reduce (raise cost) (List.sortOn (Ord.Down . snd) reductions)
