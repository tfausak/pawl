module Pawl.Engine.Cast where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Commander as Commander
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.SplitSecond as SplitSecond
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.CastingRestriction as CastingRestriction
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.Modal as Modal.Type
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- CR 117.1a's second sentence: the window a NONINSTANT spell is cast in -- a
-- main phase of its controller's own turn, with an empty stack. Every noninstant
-- card type restates it (CR 301.1, 302.1, 303.1, 306.1, 307.1), and this one
-- predicate is all five. The priority requirement is implicit: the engine only
-- offers actions to the player who holds priority.
--
-- The window the CARD TYPE gets, which is not always the window the card gets:
-- CR 702.8a's flash lifts a card out of it (instantSpeed below).
--
-- Shared with the CR 307.5 window an ability can carry (Activate.restrictionsOk) --
-- see Turn.sorcerySpeedWindow for why there is one copy.
sorcerySpeed :: PlayerId -> GameState -> Bool
sorcerySpeed = Turn.sorcerySpeedWindow

-- CR 117.1a is the default this implements: the first sentence for an instant
-- (CR 304.1), the second for everything else (CR 302.1 / 307.1). Priority is
-- implicit -- the engine only offers actions to the priority holder.
--
-- Flash is that second sentence being OVERRIDDEN rather than restated, so
-- instantSpeed's disjunction is CR 101.1 resolved, not CR 117.1a read generously.
--
-- THREE disjuncts, not two, because the widening arrives on two different axes.
-- instantSpeed is what the CARD is (CR 304.1) or what it says about itself (CR
-- 702.8a); PlayerEffect.mayCastAsThoughItHadFlash is what an effect says about
-- the PLAYER (CR 601.3b, Vedalken Orrery). They are read BESIDE each other and
-- neither is folded into the other -- see instantSpeed below for what folding
-- would cost.
--
-- The window the RULES give a spell, and not the whole of when it may be cast: a
-- card may narrow this further with a printed restriction (CR 601.3), which
-- `castable` conjoins separately through printedRestrictionsOk.
timingOk :: PlayerId -> ObjectId -> CardName.CardName -> GameState -> Bool
timingOk pid oid name gs = case proposedFace oid name gs of
  Nothing -> False
  Just face ->
    instantSpeed face
      || PlayerEffect.mayCastAsThoughItHadFlash pid oid gs
      || sorcerySpeed pid gs

-- The half whose cast is being proposed (CR 709.3a: "Only the chosen half is
-- evaluated to see if it can be cast"). Nothing when the id is unknown or no
-- card stands behind it.
--
-- NOT Game.faceOf, and the difference is the point: the card is still in a hand
-- or a graveyard when every gate below is asked, where Object.face is unset and
-- CR 709.4 would hand back the two halves COMBINED -- one action per card,
-- priced at the sum of both costs. The name comes from the proposal instead, so
-- each half is gated and priced on its own.
--
-- Resolved through the same Game.resolveFace seam Object.face reads through, so
-- what a gate measures before the move and what the stack incarnation shows
-- after it cannot drift.
--
-- CR 708.4 is the one thing that overrules the name, and it arrives through the
-- STATE rather than as an argument: a cast proposed face down is one `asProposed`
-- has stamped Facing.FaceDown onto, and the rule says the object is turned face
-- down BEFORE it is put onto the stack, so every gate below must already be
-- measuring CR 708.2a's characteristics. Read off `Object.facing` for the same
-- reason `asProposed` exists at all -- one stamped state, so a gate and the
-- announcement it authorises cannot disagree.
proposedFace :: ObjectId -> CardName.CardName -> GameState -> Maybe (Face.Face Card.Type.Card)
proposedFace oid name gs = case fmap Object.facing (Game.lookupObject oid gs) of
  Nothing -> Nothing
  Just Facing.FaceDown -> Just Card.faceDownFace
  Just Facing.FaceUp -> fmap (Game.resolveFace (Just name)) (Game.cardOf oid gs)

-- CR 304.1 / 702.8a: is this card one the rules let its controller cast whenever
-- they have priority, rather than only in the sorcery-speed window? Two ways in,
-- and they are two because one is a CARD TYPE and the other is a KEYWORD.
--
-- CR 702.8a's widening is this predicate's second disjunct, and it is lifted HERE
-- and emphatically not inside Turn.sorcerySpeedWindow: that window
-- has one copy because CR 307.1 and CR 307.5 are the same three conjuncts, and
-- CR 702.8a is about neither. Flash is a permission a CARD carries about casting
-- ITSELF, so widening the shared window would make an equip ability on the same
-- board instant-speed, which no rule says.
--
-- Read off the PRINTED keywords rather than through the CR 613 projection (CR
-- 113.6e for the zones, #160 for why printed and projected agree in them), and
-- wherever the cast is being proposed from -- CR 702.8a's "functions in any zone
-- from which you could play the card it's on".
--
-- The PLAYER-scoped sibling is NOT this and is deliberately not folded in: an
-- effect that lets a player cast OTHER spells as though they had flash (CR
-- 601.3b, Vedalken Orrery) is read in timingOk above, beside this predicate,
-- through PlayerEffect.mayCastAsThoughItHadFlash. Widening this one instead would
-- say the Orrery gave every card in every zone the flash keyword, which is not
-- what CR 702.8a's "the card it's on" means.
instantSpeed :: Face.Face Card.Type.Card -> Bool
instantSpeed face = Card.isInstant face || Keyword.hasFlash (Face.keywords face)

-- CR 601.2c / 700.2a: castable when the fillable modes admit some selection at
-- all (Modal.selectionPossible) -- ordinarily at least as many fillable modes as
-- the selection demands, under a range as many as its floor demands, and under CR
-- 700.2d's exception a single fillable mode, which may then be chosen as many
-- times as the count asks. For a non-modal card (one mode, count 1) this is
-- identical to "every slot fillable".
--
-- CR 109.5 / 601.2a: the perspective a "target creature an opponent controls"
-- slot is measured against is the player CASTING the spell. Taken as a parameter
-- rather than read off the card, which is in a hand and has no controller at all.
--
-- MEASURED BEFORE THE MOVE, which castSpell no longer is, and that is structural
-- rather than an oversight: this is an OFFER, computed by Action.legalActions
-- while every card is still where it was. What survives of the difference is the
-- object's ZONE, and three capabilities pawl LACKS are what keep that from being
-- observable -- not any claim about Magic:
--
--   * no target pool names a hidden zone at all. Pawl.Types.Pool has Permanents,
--     Spells, CardsInGraveyard and CardsInExile, and no hand or library arm
--     (#559), so no target set can be measured differently either side of it.
--   * nothing counts a hand. No Pawl.Types.Count arm reaches a hand zone, so
--     hand size cannot enter a cost or a filter.
--   * a cost adjustment carries a LITERAL amount. PlayerEffect.IncreaseSpellCost
--     and ReduceSpellCost hold a Natural or a ManaCost, never a Quantity, so no
--     adjustment can count anything -- which is the only route a zone read could
--     take into Cost.total.
--
-- The one object whose stack membership the move does change is the spell itself,
-- and CR 115.5 takes that one back out of every stack pool
-- (Target.legalRecipients). What the CHOSEN HALF changes is no longer part of
-- that argument: the specs come from `proposedFace`, and `castable` hands this a
-- state with that same half stamped onto the OBJECT (asProposed), so a filter
-- that reads the spell's own characteristics reads the half being cast too.
targetable :: PlayerId -> ObjectId -> CardName.CardName -> GameState -> Bool
targetable pid oid name gs = case proposedFace oid name gs of
  Nothing -> False
  Just face ->
    let modal = Face.spell face
     in Modal.selectionPossible (Target.fillableModes (Just pid) oid (Card.enchantSpecs face) modal gs) (Modal.Type.selection modal)

-- CR 601.2b's X=0 floor measured at CR 601.2f's total: a candidate cost is
-- affordable when it is payable with X=0 (the caster may always choose 0)
-- against the TOTAL cost, not the printed one. Taxing castability without taxing
-- payment lets the player underpay; taxing payment without taxing castability
-- offers a cast that cannot be afforded, and nothing REPAIRS a cast partway:
-- the announcement unwinds whole or not at all (castSpell's haddock, proven by
-- Pawl.CastSpec's mis-coloured-mana pair).
--
-- CR 118.13a's announcement is measured against the same total, and castSpell
-- hands Cost.totalMana in for exactly that reason: a gate and an offer that
-- disagree about what a cost is are two ways of getting the same question wrong.
-- castSpell asks this same predicate again once the announced X exists (#417).
--
-- The HALF being cast reaches CR 601.2f's adjustments through the state, not
-- through an argument: `cost` comes from the chosen face and Cost.total reads
-- the object's characteristics through Game.faceOf, so every caller hands in a
-- state with that face stamped on (asProposed) and the two cannot name different
-- halves. AdventureSpec's "Thalia taxes the Adventure half and not the creature
-- half" is what holds it.
payableCost :: PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCost = payableCostAt 0

-- The same question asked at some OTHER value of X. `payableCost` is this at CR
-- 601.2b's floor, and `affordableX` is this climbed; one predicate, so what the
-- gate measures and what the bound reports cannot drift apart.
--
-- CR 601.2b's COMPLETION comes before CR 601.2f's totalling, which is why this is
-- Cost.canPaySomeCompletion and not Cost.canPay over Cost.total: a {2/R} totalled
-- while still spelled {2/R} hides the generic reduction the announcement would
-- expose. Cost.totalManas is the totalling, and it is the same function castSpell
-- hands Cost.announce, so the gate and the offer read the adjustments through one
-- function.
--
-- CR 118.7e leaves the same choice inside a REDUCTION written with a hybrid
-- symbol, which is why that totalling answers one cost per resolution and this
-- gate asks whether some resolution pays: the choice is the payer's, made at CR
-- 601.2f, and a gate that priced the symbol at nothing refused casts the payer
-- was entitled to.
payableCostAt :: Natural -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCostAt x pid oid gs cost =
  Cost.canPaySomeCompletion pid oid (Cost.totalManas (Cost.spellAdjustments pid oid gs)) (Cost.substituteX x cost) gs

-- CR 601.2b: the greatest value of X this player could actually pay for, which is
-- what Prompt.ChooseX carries -- measured on the cost the cast is measuring, with
-- the same predicate castability was gated on.
--
-- Advisory, and nothing here clamps: see Prompt.ChooseX for why announcing past
-- this is legal (CR 601.2b) and what it costs the player (#741).
--
-- The SEARCH is Cost.greatestPayableX, shared with Activate.affordableX; the
-- PREDICATE is not, since an activation cost totals against its own adjustments
-- (Cost.activationAdjustments).
-- This haddock discharges that search's monotonicity requirement for the spell's
-- predicate.
--
-- FOUND BY ASCENDING SEARCH from 0, which is sound and terminating only because
-- payability is MONOTONE in X, and that holds structurally rather than by
-- inspection of the pool: X reaches a cost only as generic mana
-- (Mana.substituteX), CR 601.2f's adjustments never read the cost so they add and
-- forgive the same amounts at every X, and only Mana.canPay's leftover comparison
-- reads the generic count -- on the demanding side of a >= whose supply side X
-- cannot move, and which the finite supplies must eventually fail.
--
-- The two degenerate costs -- one with no {X} in it, which would climb forever,
-- and one unpayable even at X=0 -- both answer 0, and Cost.greatestPayableX says
-- why. Neither is reachable from castSpell, which asks only about a candidate
-- that already passed payableCost and only when Cost.hasVariable holds.
affordableX :: PlayerId -> ObjectId -> GameState -> Cost Keyword -> Natural
affordableX pid oid gs cost = Cost.greatestPayableX (\x -> payableCostAt x pid oid gs cost) cost

-- CR 702.42a: the ADDITIONAL cost this player may pay right now to choose all of
-- this modal spell's modes, or Nothing when entwining is not on offer at all.
--
-- Three conditions, and each is a different rule:
--
--   1. The card HAS entwine. CR 702.42a is a static ability of the spell itself,
--      so it is read off the card's printed keywords and not through the CR 613
--      projection of the stack object CR 601.2a has already made. Game.faceOf,
--      because the half being cast is stamped on that object before this is
--      asked, so CR 709.3b's "only the characteristics of the half being cast"
--      already narrows the keywords read here.
--   2. Every printed mode is LEGAL (CR 700.2a), so choosing ALL modes is not open
--      when one of them cannot be chosen. Unobservable for Dream's Grip and
--      written anyway: without it an entwined cast would announce fewer modes
--      than CR 702.42a says it chose, and castSpell's own size check would turn
--      the whole cast into a silent no-op.
--   3. Some candidate cost plus this one is payable -- CR 601.2f's "plus all
--      additional costs", at CR 601.2b's X=0 floor and with the same payableCost
--      predicate castability was gated on. An option the player cannot take is
--      not offered.
--
-- None of the three is a choice being made for the player: an option the card
-- does not have, that CR 700.2a closes, or that CR 118.3 says cannot be paid, is
-- not an option. WHICH candidate will carry the cost is not decided here --
-- castSpell narrows the candidates once the answer is in.
--
-- `candidates` is handed in rather than read from `Cost.costsFor`: by the
-- time castSpell asks, CR 601.2a has already moved the card to the stack, and
-- pawl offers a candidate cost BY ZONE (flashback's only from a graveyard), so
-- the list has to come from the proposal. See castSpell.
entwineOffer :: PlayerId -> ObjectId -> [Cost Keyword] -> GameState -> Maybe (Cost Keyword)
entwineOffer pid oid candidates gs = case Game.faceOf oid gs of
  Nothing -> Nothing
  Just face -> do
    cost <- Keyword.entwineCost (Face.keywords face)
    let modal = Face.spell face
        legal = Target.fillableModes (Just pid) oid (Card.enchantSpecs face) modal gs
    Monad.guard (Natural.length legal == Modal.modeCount modal)
    Monad.guard (any (\candidate -> payableCost pid oid gs (Cost.plus candidate cost)) candidates)
    pure cost

-- CR 702.33a: the ADDITIONAL cost this player may pay as they cast this spell, or
-- Nothing when kicking is not on offer at all.
--
-- Two conditions where entwineOffer above has three, and each is entwineOffer's:
--
--   1. The card HAS kicker, read off the printed keywords of the half being cast
--      for that function's reason -- rule 702.33a is a static ability of the spell
--      itself (CR 702.33a: "functions while the spell with kicker is on the
--      stack").
--   2. Some candidate cost plus this one is payable -- CR 601.2f's "plus all
--      additional costs", at CR 601.2b's X=0 floor and with the same payableCost
--      predicate castability was gated on. An option the player cannot take is not
--      offered.
--
-- What it does NOT test is rule 700.2a's mode legality, which is the third of
-- entwine's: kicker widens no mode choice, so a spell with a kicker cost is
-- offered it whatever its modes are.
--
-- Neither condition is a choice being made for the player: an option the card does
-- not have, or that CR 118.3 says cannot be paid, is not an option. WHICH candidate
-- will carry the cost is not decided here -- castProposed narrows the candidates
-- once the answer is in -- and `candidates` is handed in for entwineOffer's reason.
kickerOffer :: PlayerId -> ObjectId -> [Cost Keyword] -> GameState -> Maybe (Cost Keyword)
kickerOffer pid oid candidates gs = case Game.faceOf oid gs of
  Nothing -> Nothing
  Just face -> do
    cost <- Keyword.kickerCost (Face.keywords face)
    Monad.guard (any (\candidate -> payableCost pid oid gs (Cost.plus candidate cost)) candidates)
    pure cost

-- CR 702.33d's designation, written onto the spell's own stack incarnation: "that
-- spell has been kicked". Read back by Quantity.WasKicked through the CR 613
-- projection, which is how the card's own CR 702.33e ability sees it.
--
-- An idempotent write of a field no layer computes, and one direction only: rule
-- 702.33d gives no way to unkick a spell, and CR 400.7 ends the designation with
-- the incarnation (Object.newIncarnation), so nothing has to clear it.
stampKicked :: ObjectId -> GameState -> GameState
stampKicked sid gs =
  gs
    { GameState.objects =
        Map.adjust (\o -> o {Object.kicked = True}) sid (GameState.objects gs)
    }

-- CR 601.3: the zones a spell can be cast from at all, in the engine's
-- canonical order -- what castableSpells scans, and the list castableZones
-- filters, so the two can never disagree about where to look.
--
-- The library is deliberately absent: Panglacial Wurm's permission is scoped to
-- a search in progress (castableWhileSearching) rather than to the whole game,
-- so it is not a zone a player may simply cast from.
castZones :: [Zone.Zone]
castZones = [Zone.Hand, Zone.Graveyard, Zone.Exile, Zone.Command]

-- Which of those zones THIS player may cast THIS half of THIS object from --
-- where a permission turns into a zone.
--
-- Takes the object and the board, where a card-carried permission would need
-- only the face: CR 715.3d's permission is state on ONE exiled incarnation and
-- names ONE player, so no function of a Face could answer for it.
castableZones :: PlayerId -> ObjectId -> Face.Face Card.Type.Card -> GameState -> [Zone.Zone]
castableZones pid oid face gs =
  let permitted zone = case zone of
        -- CR 304.1 / 307.1: the rules' own allowance is worded "from their
        -- hand", so the hand needs no permission of its own -- unless a rule takes
        -- it away, which CR 702.127a's second static ability does: "this half of
        -- this split card can't be cast from any zone other than a graveyard".
        --
        -- A PROHIBITION and so read here rather than as a CastingPermission: a
        -- permission list can only add zones, and this one removes the zone every
        -- card gets for free. It reads the FACE, which is how the rule's "this
        -- half" scoping comes out right -- Dusk is castable from a hand and Dawn
        -- is not, off the same card.
        Zone.Hand -> not (Keyword.hasAftermath (Face.keywords face))
        Zone.Graveyard -> permitsCastFromGraveyard face
        Zone.Exile -> permitsCastFromExile pid oid face gs
        -- CR 903.8: "a commander's owner may cast it from the command zone". A
        -- FORMAT's permission, not a card's, which is why it is asked of
        -- Pawl.Engine.Commander rather than read off the face -- see that module.
        -- The owner test is inside isCommander's caller: rule 903.8 lets only the
        -- commander's owner cast it from there.
        Zone.Command -> Commander.canCastFromCommandZone pid oid gs
        -- No other zone is in castZones.
        _ -> False
   in filter permitted castZones

-- CR 601.3: may this player cast this half of this exiled card? Two conjuncts,
-- from two rules, and CR 715.3d's are why the Adventure half of an exiled
-- adventurer card is not offered while the same card in a hand offers both:
--
--   * Object.playableFromExile names a player -- which is what keeps a
--     permission from being an offer to the table. Written either by CR 715.3d's
--     own "for as long as that card remains exiled, that player may play it" or
--     by an Effect.GrantPlayFromExile a card states.
--   * CR 715.3d's "it can't be cast as an Adventure this way" -- so the proposed
--     face must not be the Adventure one.
--
-- Not implemented: the rule's own last clause, "although other effects that
-- allow a player to cast it may allow a player to cast it as an Adventure".
-- The Adventure exclusion is applied to every permission this field can hold,
-- where CR 715.3d scopes it to the one CR 715.3d itself grants -- so a card
-- permitted to be played by some other effect is refused its Adventure half
-- (#669). STRICTER than printed, which is the admissible direction: the engine
-- offers fewer casts than the rules allow, never more.
permitsCastFromExile :: PlayerId -> ObjectId -> Face.Face Card.Type.Card -> GameState -> Bool
permitsCastFromExile pid oid face gs =
  fmap ExilePlayPermission.player (Game.lookupObject oid gs >>= Object.playableFromExile) == Just pid
    && not (Card.isAdventure face)

-- Is this object somewhere this player may cast it from?
inCastableZone :: PlayerId -> ObjectId -> CardName.CardName -> GameState -> Bool
inCastableZone pid oid name gs =
  case proposedFace oid name gs of
    Nothing -> False
    Just face -> any (\zone -> elem oid (Game.zoneMembers zone pid gs)) (castableZones pid oid face gs)

-- CR 205.4e: a legendary instant or sorcery can't be cast unless its caster
-- controls a legendary creature or a legendary planeswalker.
--
-- A RULE, not a card-carried permission. CR 205.4e restricts the PLAYER from the
-- rulebook, so it is checked here beside the CR 601.3 checks and is emphatically
-- not a CastingPermission -- a card-carried version would be the rules core
-- learning from data a restriction it already knows. Reading a supertype and a
-- card type is the same closed-half act as CR 704.5j's legend rule.
--
-- The SPELL's own type line is read PRINTED (Card.isLegendary): it is in a hand
-- or a graveyard when this is asked, where no projection exists, and CR 205.4a
-- makes supertypes a printed-type-line read regardless.
legendaryRestrictionOk :: PlayerId -> ObjectId -> CardName.CardName -> GameState -> Bool
legendaryRestrictionOk pid oid name gs = case proposedFace oid name gs of
  Nothing -> False
  Just face ->
    not (Card.isLegendary face && (Card.isInstant face || Card.isSorcery face))
      || controlsLegendaryCreatureOrPlaneswalker pid gs

-- CR 205.4e's condition. Read through the PROJECTION rather than off the printed
-- card, because "controls a legendary creature" is a question about the object as
-- it exists on the battlefield: a Clone copying Thalia is a legendary creature
-- (CR 707.2 lists supertype and card type among the copiable values) though its
-- own card carries no supertype at all. BOTH disjuncts are read that way, so a
-- permanent animated into a planeswalker satisfies it and a planeswalker turned
-- into something else does not.
controlsLegendaryCreatureOrPlaneswalker :: PlayerId -> GameState -> Bool
controlsLegendaryCreatureOrPlaneswalker pid gs =
  let qualifies oid =
        Projection.controllerOf oid gs == Just pid
          && Set.member Supertype.Legendary (Projection.supertypesOf oid gs)
          && ( Projection.isCreatureOf oid gs
                 || Set.member CardType.Planeswalker (Projection.cardTypesOf oid gs)
             )
   in any qualifies (GameState.battlefield gs)

-- CR 601.3's PROHIBITION half as printed on the card about ITSELF -- Rally the
-- Troops' "only during the declare attackers step and only if you've been
-- attacked this step".
--
-- The exact counterweight to permissionsOf above, and read the same way: off the
-- card, never through the projection (CR 113.6e, which for this pool means a
-- hand, which pawl's projection does not reach -- #160). ALL of them must hold,
-- which is what CR 601.3's "no ... prohibits" means; one permission, by contrast,
-- suffices.
--
-- Casing on the arms is a classification, not an effect's identity:
-- Pawl.Engine.Cast is the sole reader of Pawl.Types.CastingRestriction exactly as
-- it is of CastingPermission.
printedRestrictionsOk :: PlayerId -> ObjectId -> CardName.CardName -> GameState -> Bool
printedRestrictionsOk pid oid name gs = case proposedFace oid name gs of
  Nothing -> False
  Just face -> all (restrictionMet pid gs) (Face.castingRestrictions face)

-- Does the game state satisfy this one printed clause?
restrictionMet :: PlayerId -> GameState -> CastingRestriction.CastingRestriction -> Bool
restrictionMet pid gs restriction = case restriction of
  -- CR 500.1: a step or a stepless phase, compared against the one the game is
  -- in.
  CastingRestriction.DuringPhase phase -> GameState.phase gs == phase
  -- CR 508.3b's question, and it lives in Pawl.Engine.Combat because the ability
  -- side's clause of the same name asks exactly it: one question about the combat
  -- record, two gates that differ in what ELSE they may read (CR 307.5).
  CastingRestriction.AttackedThisStep -> Combat.attackedThisStep pid gs

-- CR 709.3a / 715.3a: the half being cast, RECORDED ON THE OBJECT, so that every
-- characteristic read of it resolves through Game.resolveFace to that half alone
-- (CR 709.3b / 715.3b) rather than to the unnamed fallback -- CR 709.4's combined
-- view for a split card, CR 715.4's normal half for an adventurer card.
--
-- The GATE's writer, and only the gate's: `castable` and
-- `castableWhileSearching` stamp a state they only READ, since the card has not
-- moved and nothing here moves it. The CR 400.7 incarnation on the stack is
-- stamped by the CR 601.2a move itself (Event.changeZoneShowing), from the same
-- `name` the gate was asked about -- so what a gate measures and what the
-- incarnation shows cannot name different halves.
--
-- NOT a simulation of CR 601.2a's move, and it does not need to be: both rules
-- say outright that castability is evaluated against the chosen half -- CR 709.3a
-- "only the chosen half is evaluated to see if it can be cast", CR 715.3a "only
-- the alternative characteristics are evaluated to see if it can be cast" -- and
-- both put that choice before the move rather than inside the announcement: CR
-- 709.3 "a player chooses which half of a split card they are casting BEFORE
-- putting it onto the stack", CR 715.3 "as a player plays an adventurer card,
-- the player chooses whether they play the card normally or as an Adventure". So no
-- object is minted (CR 400.7), no CR 616.1 replacement loop runs, and nothing
-- prompts.
--
-- What the offer's state and the real stack incarnation still differ in is the
-- object's ZONE, and no cost adjustment reads one: Pawl.Engine.Filter's View
-- carries no zone axis, and Projection.viewOfObject applies no zone gate --
-- projectGiven falls through to the full layer fold off the battlefield (see
-- Pawl.Types.Affected's MatchingAnywhere).
--
-- CR 708.4 rides the same stamp: a cast proposed face down is measured against
-- the face-down characteristics, and the rule puts that turning-over BEFORE the
-- object is put onto the stack -- so `facing` is written here alongside the
-- half, and every gate below reads it through `proposedFace` and `Game.faceOf`
-- without knowing morph exists. FaceUp is CR 110.5b's default and what every
-- ordinary proposal passes.
asProposed :: ObjectId -> CardName.CardName -> Facing.Facing -> GameState -> GameState
asProposed oid name facing gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.face = Just name, Object.facing = facing}) oid (GameState.objects gs)}

-- Affordable and correctly timed, actually in a zone this player may cast it
-- from, fillable, and prohibited by nothing. CR 601.2b: affordable means at least
-- ONE candidate cost is payable.
--
-- THREE prohibitions, not one, because they are carried by three different
-- things: a continuous effect on the player (Rule of Law, Silence), the card's
-- own printed text, and CR 205.4e itself.
--
-- Asked of ONE HALF, named: CR 709.3a and CR 715.3a evaluate only the chosen half
-- to see if it can be cast, so a multi-face card is asked this question once per
-- half. That name reaches the conjuncts TWICE over, belt and braces rather than
-- necessity -- once stamped, the two resolve the same face -- and each carries a
-- job the other cannot: as an ARGUMENT, which is how the ones that read the CARD
-- -- the timing window, the printed restrictions, the candidate costs, the target
-- specs -- resolve their face, and where the name is used AS A NAME (CR 601.3a's
-- prohibitions, Null Chamber; Cost.costsFor); and as `asProposed`'s STAMP, which
-- is how the ones that go on to read the
-- OBJECT resolve theirs: CR 601.2f's adjustments through Cost.total, and any
-- filter measuring the spell's own characteristics. Thalia's "noncreature spells
-- cost {1} more to cast" is the observable: it taxes the Sorcery half of an
-- adventurer card and not the Creature half, off one card in one hand.
--
-- And of one FACING, for the reason CR 708.4 gives: a morph cast and an ordinary
-- cast of the same card are two different proposals, gated separately, because
-- "any effects or prohibitions that would apply to casting an object with these
-- characteristics (and not the face-up object's characteristics) are applied to
-- casting this object". Every conjunct below reads the face-down face once the
-- stamp is on, so none of them names morph.
castable :: PlayerId -> ObjectId -> CardName.CardName -> Facing.Facing -> GameState -> Bool
castable pid oid name facing gs =
  let proposed = asProposed oid name facing gs
      -- CR 708.2a's "no name", where the name is used AS A NAME. Taken off the
      -- proposed face rather than from the argument, so a face-down proposal
      -- carries the empty name CR 708.4 gives it and a face-up one carries the
      -- half's own -- the two coincide for every face-up cast, since
      -- `proposedFace` resolved that half by this very name.
      proposedName = maybe name Face.name (proposedFace oid name proposed)
   in timingOk pid oid name proposed
        && inCastableZone pid oid name proposed
        -- CR 601.3: gated HERE, upstream of Action.legalActions, because the
        -- engine never offers an illegal action and then rejects it. The half's
        -- own name goes with it, since CR 601.3a's prohibitions name a quality of
        -- the spell (Null Chamber) and CR 709.3a evaluates only the chosen half --
        -- and the OBJECT with it, since a quality can also be a Filter over the
        -- spell's characteristics (Damping Engine), which is read off the
        -- `proposed` stamp this call already carries.
        && not (PlayerEffect.prohibitsCasting pid oid proposedName proposed)
        -- CR 601.3's prohibit half again, from a different CARRIER: a spell on
        -- the stack (CR 702.61a) rather than a continuous effect on a player. It
        -- names neither a player nor a quality of the spell, so it takes no
        -- argument beyond the board.
        && not (SplitSecond.inForce proposed)
        && printedRestrictionsOk pid oid name proposed
        && legendaryRestrictionOk pid oid name proposed
        && any (payableCost pid oid proposed) (Cost.costsFor name oid proposed)
        && targetable pid oid name proposed

-- Every cast this player may propose right now, in castZones' order, as the
-- (object, half, facing) triples Action.Cast is built from. `castable` re-checks
-- the permission per card, so a graveyard with no flashback card in it
-- contributes nothing.
--
-- ONE ENTRY PER CASTABLE FACE, which is CR 709.3's choice being offered rather
-- than made: a split card in hand with both halves affordable appears twice,
-- and the player picks by picking an action. A card whose halves are separately
-- gated appears only as often as CR 709.3a lets it.
--
-- AND ONE MORE PER FACE WITH MORPH, which is CR 702.37d's permission being
-- offered the same way: "you can't normally cast a card face down; a morph
-- ability allows you to do so". Casting face up and casting face down are two
-- different casts of one card at two different costs for two different objects,
-- so they are two actions and the player picks -- the engine never decides which
-- (docs/design.md's second invariant). Both are then gated independently, since
-- CR 708.4 measures each against its own characteristics: a card whose face-up
-- cast is prohibited or unaffordable may still be castable face down, and the
-- other way round.
--
-- Read off the card's PRINTED keywords, which is rule 702.37a's own scope ("a
-- static ability that functions in any zone from which you could play the
-- card"), and off the face being proposed, so a multi-face card offers the
-- morph cast only for the half that prints one. No printing has one.
castableSpells :: PlayerId -> GameState -> [(ObjectId, CardName.CardName, Facing.Facing)]
castableSpells pid gs =
  let facings face =
        Facing.FaceUp
          : [Facing.FaceDown | Maybe.isJust (Keyword.morphCost (Face.keywords face))]
      proposals oid =
        [ (oid, Face.name face, facing)
        | face <- foldMap Card.castableFaces (Game.cardOf oid gs),
          facing <- facings face
        ]
      offered oid = filter (\(_, name, facing) -> castable pid oid name facing gs) (proposals oid)
      inZone zone = concatMap offered (Game.zoneMembers zone pid gs)
   in concatMap inZone castZones

-- CR 601.3 (Panglacial): may this card be cast from the library while its
-- controller searches their own library? A membership test on the card's casting
-- permissions -- a classification, never card identity.
permitsCastWhileSearching :: Face.Face Card.Type.Card -> Bool
permitsCastWhileSearching face =
  elem CastingPermission.CastFromLibraryWhileSearching (permissionsOf face)

-- CR 601.3 / 702.34a: may this card be cast from its owner's graveyard? The
-- flashback half of the same membership test.
permitsCastFromGraveyard :: Face.Face Card.Type.Card -> Bool
permitsCastFromGraveyard face =
  elem CastingPermission.CastFromGraveyard (permissionsOf face)

-- Every casting permission a card has: the ones it PRINTS (Panglacial Wurm) plus
-- the ones rule 702 gives it for a keyword it holds (flashback). Read off the
-- card and never through the projection: these permissions function in the
-- library and the graveyard (CR 113.6), which pawl's projection does not reach
-- (#160).
--
-- The face's own type line is what answers rule 702.34a's "if the resulting
-- spell is an instant or sorcery spell", and it is the PROPOSED face's because
-- the caller has already narrowed to one (proposedFace). CR 601.3e's Melek
-- example is that same reading one zone over: under "you may cast instant and
-- sorcery spells from the top of your library", an adventurer card offers its
-- instant Adventure half and not its creature half.
permissionsOf :: Face.Face Card.Type.Card -> [CastingPermission.CastingPermission]
permissionsOf face =
  Face.castingPermissions face
    <> Keyword.castingPermissionsOf (TypeLine.types (Face.typeLine face)) (Face.keywords face)

-- The library cards this player may cast while searching their own library:
-- permitted, not prohibited, affordable, and with a fillable target set.
-- Deliberately omits timingOk -- the permission IS the CR 601.3 timing exception
-- (the ruling: "follows all normal rules ... except for timing").
--
-- The prohibition is NOT omitted, and that is the point: CR 601.3 is one sentence
-- with two halves, and the Panglacial permission excepts only the timing one, so
-- a Rule of Law still stops a cast from the library, and so does a Null Chamber
-- that named the Wurm. CR 205.4e's restriction rides along for the same reason,
-- and THAT one is unobservable in this pool -- every card holding the permission
-- is a creature and none is a legendary sorcery -- and is written anyway,
-- because the alternative is a cast the rules forbid.
--
-- Those three conjuncts, and the affordability and target-fillability beside
-- them, are `castableWhenOffered` below -- shared with CR 608.2g's other
-- producer, so the two offers cannot come to disagree about what is still asked.
--
-- printedRestrictionsOk rides along too, and it is the closest call of the three:
-- a "Cast this spell only during the declare attackers step" IS about timing, so
-- the ruling's "except for timing" could be read to lift it. pawl takes the
-- narrower reading -- the ruling excepts the RULES' own timing window (CR 302.1 /
-- 307.1), not a prohibition the card prints on itself. Unobservable: no card
-- holding the permission prints a restriction alongside it.
--
-- ONE ENTRY PER CASTABLE HALF, exactly as castableSpells offers a hand's split
-- card twice: CR 709.3's "A player chooses which half of a split card they are
-- casting" is a choice CR 601.3 does not take away, so a split card printing
-- the permission on both halves reaches the prompt as two options and the
-- player picks. Each half is gated on its own, which is CR 709.3a.
castableWhileSearching :: PlayerId -> GameState -> [(ObjectId, CardName.CardName)]
castableWhileSearching pid gs =
  let allowed oid face =
        let name = Face.name face
            -- The same stamped state `castable` gates on, for the same rule:
            -- CR 601.3's exception is about TIMING, so the half a library cast
            -- is evaluated against is still CR 709.3a's chosen one, face up:
            -- CR 702.37a's morph ability functions from a library, but CR 601.3
            -- is what would have to permit the cast from there and the
            -- Panglacial permission is printed text the face-down object does
            -- not have (CR 708.2a). Unreachable either way -- no card holds both.
            proposed = asProposed oid name Facing.FaceUp gs
         in permitsCastWhileSearching face
              && castableWhenOffered pid oid name (Cost.costsFor name oid proposed) proposed
      proposals oid = fmap (\face -> (oid, Face.name face)) (filter (allowed oid) (foldMap Card.castableFaces (Game.cardOf oid gs)))
   in concatMap proposals (Game.zoneMembers Zone.Library pid gs)

-- CR 608.2g: everything a cast an EFFECT offers must still satisfy, given the
-- candidate costs that offer supplies. `castable`'s conjuncts minus the two the
-- offer itself answers:
--
--   * TIMING. CR 608.2g's cast happens inside a resolution, where CR 117.1a's
--     window is closed and no player has priority. The Panglacial ruling says the
--     same of that rule's other producer -- "follows all normal rules ... except
--     for timing".
--   * CR 601.3's PERMISSION and the zone it turns into. The effect instructing
--     the cast IS the permission, and it names the object rather than a zone, so
--     inCastableZone has nothing to ask. Each caller keeps its own gate for the
--     half of rule 601.3 that is about the card -- Panglacial's printed
--     permission above, and CR 310.11b's "it" being where the exile left it.
--
-- Everything else stays, and CR 601.3's own second half is why: that rule is one
-- sentence with two limbs, and neither producer excepts the PROHIBIT one. A Rule
-- of Law still stops the cast, a Null Chamber that named the card still stops it,
-- CR 205.4e still applies, an unpayable cost is still no offer, and CR 601.2c
-- still needs a fillable target set.
--
-- The candidates arrive as an ARGUMENT rather than being read from the object,
-- because that is exactly what CR 118.9's "applied to it from another effect"
-- changes: an offer carrying that alternative hands in the one cost the rule
-- allows (CR 118.9a), where an offer carrying none hands in Cost.costsFor's own
-- list.
--
-- `gs` must already be `asProposed`-stamped for the half being offered, as
-- `castable`'s conjuncts require.
castableWhenOffered :: PlayerId -> ObjectId -> CardName.CardName -> [Cost Keyword] -> GameState -> Bool
castableWhenOffered pid oid name candidates proposed =
  -- CR 601.3's prohibit half, asked with the half's own name: a quality-bearing
  -- prohibition stops one card without stopping any other candidate.
  not (PlayerEffect.prohibitsCasting pid oid name proposed)
    -- CR 702.61a stays too, for CR 601.3's own reason above: an offered cast is
    -- still a cast. Reachable because CR 702.61b keeps triggered abilities going
    -- on the stack, so one can resolve ABOVE the split-second spell and offer a
    -- cast while it is still there.
    && not (SplitSecond.inForce proposed)
    && any (payableCost pid oid proposed) candidates
    && printedRestrictionsOk pid oid name proposed
    && legendaryRestrictionOk pid oid name proposed
    && targetable pid oid name proposed

-- CR 601.3 (Panglacial): while a player searches their own library, offer them
-- the chance to cast a castable-while-searching card from it, before any card is
-- found (per the ruling). Loops so multiple copies may be cast; each cast removes
-- a card from the library, so castableWhileSearching shrinks and the loop
-- terminates. castSpell is the re-entrant call -- casting mid-resolution, the
-- whole point.
castWhileSearching :: PlayerId -> Game ()
castWhileSearching pid = do
  gs <- State.get
  case castableWhileSearching pid gs of
    [] -> pure ()
    options -> do
      let decider = Decide.deciderFor pid gs
      choice <- Game.choose (Prompt.CastWhileSearching decider pid options)
      case choice of
        Nothing -> pure ()
        Just (oid, name) ->
          -- Reject-not-repair: an option not in the offered set is a no-op that
          -- ends the loop, never a repair. The PAIR is what is checked, so a
          -- half the offer did not include is rejected even when the card's
          -- other half was offered (CR 709.3a).
          Monad.when (elem (oid, name) options) $ do
            -- Face up: castableWhileSearching offers no face-down cast, for the
            -- reason its `proposed` note gives.
            castSpell pid oid name Facing.FaceUp
            castWhileSearching pid

-- CR 601.2's own order, walked in it: 601.2a moves the card to the stack FIRST,
-- then 601.2b chooses the modes and the cost and announces X and the Phyrexian
-- symbols, then 601.2c chooses the targets, then 601.2f-h totals the cost and
-- pays it, and 601.2i records that the spell has been cast. The spell is a stack
-- object for the whole of its own announcement, which is what makes every read
-- below see the CR 400.7 incarnation rather than the card still sitting in a
-- hand. CR 115.5 is what keeps that from being a new bug rather than a fix: the
-- spell now appears in its OWN Pool.Spells, and a spell on the stack being an
-- illegal target for itself takes it back out (Target.legalRecipients).
--
-- TWO things are read from `before`, one step ahead of the move, because CR
-- 400.7 mints an incarnation with no memory of where it came from: the zone the
-- cast was proposed FROM, which armCastFromGraveyard needs, and the CANDIDATE
-- COSTS, which pawl offers by zone -- unless an effect applied an alternative
-- cost, in which case CR 118.9's is the only one (castSpellWith). CR 601.2b
-- determines those at the proposal, so locking them in there is the rule's own
-- reading rather than a workaround.
--
-- REJECT-NOT-REPAIR, as a genuine rewind: an illegal answer at any step restores
-- `before`, which is what undoes the CR 601.2a move -- CR 601.2's own remedy, and
-- the posture Activate.activateAbility already takes. Pawl.CastSpec's pair "CR
-- 601.2 a mis-coloured mana answer unwinds the whole cast" and "the same cast
-- with the right colour succeeds" prove it end to end: one colour apart, and the
-- rejected one leaves the card in hand with its payer untapped. What the restore
-- does NOT undo is a prompt already issued (#741).
--
-- Every prompt below is answerable: legalActions only offers affordable,
-- fully-fillable casts. A legal answer CAN still fail after the prompt, and one
-- class of it is deliberate: castability asks whether SOME sequence of choices
-- pays the cost, and a player who then taps their only Birds of Paradise for the
-- wrong colour cannot pay (Cost.payMana argues why the engine must let them).
-- What must NOT happen is pawl offering a route it can already see the total cost
-- cannot pay, which is why Cost.announce is handed CR 601.2f's totalling below.
--
-- A spell with no slots (in its chosen modes) asks nothing.
--
-- `name` is CR 709.3's half-choice, ALREADY MADE: the rule puts it before the
-- card is put onto the stack, so it arrives with the proposal rather than being
-- prompted for here. CR 601.2b's last sentence is what that buys -- a
-- previously made choice may restrict the ones announced below.
--
-- `facing` is CR 702.37c's other already-made choice, and it rides the action
-- for CR 709.3's reason one rule over: CR 708.4 puts the turning-over BEFORE the
-- object is put onto the stack, so it cannot be a prompt inside the
-- announcement.
castSpell :: PlayerId -> ObjectId -> CardName.CardName -> Facing.Facing -> Game ()
castSpell = castSpellWith Nothing

-- castSpell with CR 118.9's other source of an alternative cost: one "applied to
-- it from another effect" rather than listed in the spell's own text. Just c
-- REPLACES the candidate list with that one cost; Nothing is CR 601.2b's own
-- candidates, which is every cast the rules themselves offer.
--
-- REPLACES rather than joins, and CR 118.9b is why: "an effect that allows you to
-- cast a spell may require a certain alternative cost to be paid". CR 310.11b's
-- offer is one of those -- a player casting a defeated Siege does not get to pay
-- {2}{W} instead -- and CR 118.9a's "only one alternative cost can be applied to
-- any one spell" is what keeps the printed alternatives from joining it.
--
-- Nothing about the offer is re-checked here: `castSpellWith` casts, and whether
-- the cast may be offered at all is `castableWhenOffered`'s question, asked by
-- the caller that made the offer.
castSpellWith :: Maybe (Cost Keyword) -> PlayerId -> ObjectId -> CardName.CardName -> Facing.Facing -> Game ()
castSpellWith applied pid oid name facing = do
  before <- State.get
  -- The state the GATE measured, which is `before` with CR 709.3's half and CR
  -- 708.4's facing stamped on. Read from rather than written to the game: the
  -- card has not moved, and the move below is what makes the stamp real.
  let proposed = asProposed oid name facing before
  case proposedFace oid name proposed of
    Nothing -> pure ()
    Just face -> do
      let castFrom = fmap Object.zone (Game.lookupObject oid before)
          -- Read off the PROPOSED state, so a face-down cast is priced at CR
          -- 702.37a's {3} rather than at the card's own mana cost -- the same
          -- candidate list `castable` gated the offer on.
          -- CR 903.8's commander tax is added HERE, before CR 601.2a's move,
          -- and not left to Cost.total further down. The move mints a fresh CR
          -- 400.7 incarnation on the stack, and the tax is a question about the
          -- command zone -- so by the time castProposed prices `sid`, the object
          -- the rule is about is gone. `oid` is still in the command zone at this
          -- line, which is the only point where the question has an answer.
          --
          -- The castability GATE (castable, below) prices the same spell through
          -- Cost.total on this same pre-move id, so it sees the tax too, and the
          -- two agree. Nothing is double-counted: Cost.total adds the tax only
          -- for an object currently in the command zone, and `sid` never is.
          --
          -- Added to the candidate rather than kept as a separate increase
          -- because CR 601.2f applies every increase before any reduction, so
          -- {2} folded in here and {2} added as an increase reach the same total
          -- -- and a cost reduction still applies to it, which is what rule
          -- 601.2f's order says and what Commander decks expect.
          taxed = Commander.taxCandidates pid oid before
          candidates = fmap taxed (maybe (Cost.costsFor name oid proposed) pure applied)
      -- CR 601.2a, carrying CR 709.3a's "only that half is considered to be put
      -- onto the stack": the chosen half is part of the move rather than a
      -- stamp applied once it has landed, so the CR 400.7 incarnation never
      -- exists without it and every read of it -- inside the move as much as in
      -- CR 601.2b's announcements below -- sees that half alone (CR 709.3b)
      -- rather than CR 709.4's combined view. Event.changeZoneShowing says what
      -- it costs and what can observe it.
      --
      -- `name` is the same name castable's gate stamped through asProposed, so
      -- the offer and the announcement cannot name different halves.
      --
      -- Nothing means the id was unknown or the CR 616.1 replacement loop
      -- cancelled the move, and a proposal whose first step did not happen is
      -- one the game returns from (CR 601.2).
      --
      -- CR 708.4 is the same claim about the other status: the object is turned
      -- face down BEFORE it is put onto the stack, so the move carries the
      -- facing too and no reader inside it sees a face-up incarnation.
      moved <- case facing of
        Facing.FaceUp -> Event.changeZoneShowing oid Zone.Stack (Just name)
        Facing.FaceDown -> Event.changeZoneFaceDown oid Zone.Stack (Just name)
      case moved of
        Nothing -> State.put before
        Just sid -> castProposed pid sid face castFrom candidates before

-- CR 601.2b-i for a spell already on the stack -- castSpell's body once its CR
-- 601.2a move has happened. `sid` is the stack incarnation (CR 400.7), the object
-- every step below announces for, targets relative to, is projected from and
-- stamps its choices onto; `before` is the state to return to. Split out so the
-- whole announcement reads one state and one id.
castProposed :: PlayerId -> ObjectId -> Face.Face Card.Type.Card -> Maybe Zone.Zone -> [Cost Keyword] -> GameState -> Game ()
castProposed pid sid face castFrom candidates before = do
  gs <- State.get
  let decider = Decide.deciderFor pid gs
      modal = Face.spell face
      legal = Target.fillableModes (Just pid) sid (Card.enchantSpecs face) modal gs
      -- CR 601.2e: an illegal proposal returns the game to the moment before the
      -- casting was proposed, which is the state before CR 601.2a's move. CR
      -- 601.6 says the same for a permission lost after the proposal completes.
      reject :: Game ()
      reject = State.put before
  -- CR 702.42a: entwine, asked FIRST -- before the mode choice CR 601.2b lists
  -- first -- because that rule states the widened selection and the extra payment
  -- as ONE decision. A player who entwines has thereby announced their mode
  -- choice, so there is nothing left for ChooseModes to ask; a player who
  -- declines is asked the ordinary question one line below, in 601.2b's order.
  --
  -- The choice is never made for them. entwineOffer answers Nothing only where
  -- there is no option to offer -- no entwine, an illegal mode (CR 700.2a), or
  -- no payable route -- and where there IS one, both answers go to the player.
  --
  -- Carried as the additional Cost itself rather than as a flag, so the two
  -- things it changes -- the mode count just below and the candidate costs
  -- further down -- read the same value.
  entwined <- case entwineOffer pid sid candidates gs of
    Nothing -> pure Nothing
    Just extra -> do
      decision <- Game.choose (Prompt.ChooseEntwine decider pid sid extra)
      pure $ case decision of
        EntwineDecision.Entwines -> Just extra
        EntwineDecision.Declines -> Nothing
  -- CR 700.2 normally, CR 702.42a's "all modes" when the entwine cost is being
  -- paid. The printed ModeSelection is untouched either way: entwine overrides
  -- the selection for this ONE cast, it does not reprint the card. It overrides
  -- CR 700.2d's exception along with the count, and that is the rule rather than a
  -- simplification: "all modes" names each printed mode once, so no mode entwined
  -- onto a spell can repeat -- and no entwine card prints the exception anyway.
  let selection = case entwined of
        Just _ -> ModeSelection.ChooseExactly (Modal.modeCount modal)
        Nothing -> Modal.Type.selection modal
  -- CR 601.2b: modes are chosen BEFORE X and targets. Forced and unprompted
  -- exactly when there is nothing to choose, which Modal.forcedSelection decides
  -- -- ordinarily as many legal modes as the selection demands or fewer, so every
  -- legal mode must be taken and the options are indistinguishable. An entwined
  -- cast is always in that case: entwineOffer has already established that every
  -- mode is legal, so `legal` has exactly as many members as the selection demands
  -- and CR 702.42a's "all modes" is the only answer.
  --
  -- Two cards hold this branch and its complement in place, both "Choose two --"
  -- of four (ModalSpec): Cryptic Command, whose last two modes take no targets
  -- and whose "target permanent" mode is fillable in every state that can pay for
  -- it, so the prompt is always asked and is answered only when it really offers
  -- all four; and Ojutai's Command, whose two targeting modes look at a graveyard
  -- and the stack, which a cast can leave empty -- exactly two choosable modes,
  -- and a spec that fails if a prompt is issued.
  --
  -- Sorted on the way in, so the stored selection is in printed order (CR 608.2c)
  -- with a repeated mode's instances adjacent (CR 700.2d). The answerer's order
  -- carries no information: the player chooses WHICH modes, never in what order
  -- they resolve.
  chosenModes <- case Modal.forcedSelection legal selection of
    Just forced -> pure forced
    Nothing -> fmap Seq.sort (Game.choose (Prompt.ChooseModes decider pid sid legal selection))
  -- Reject-not-repair: an answer that does not satisfy the printed instruction --
  -- wrong size, an illegal mode, or a repeat the instruction does not permit (CR
  -- 700.2d) -- rewinds the whole cast, guarding every step below.
  if not (Modal.selectionSatisfiedBy legal selection chosenModes)
    then reject
    else do
      -- CR 601.2b: the cost to be paid is announced after the modes and before X
      -- and targets. Only PAYABLE candidates are offered (CR 118.9b makes an
      -- alternative optional, so a player who can afford both is really
      -- choosing); one payable candidate is forced and unprompted.
      -- Reject-not-repair: an answer outside the offered set rewinds the cast.
      --
      -- CR 601.2f: an announced entwine -- and the kicker announced below it -- is
      -- added to every candidate BEFORE the payability filter, so the routes
      -- offered are the ones that can actually pay it -- and CR 118.9d is what
      -- makes it apply to an alternative cost as readily as to the printed one.
      let withEntwine candidate = maybe candidate (Cost.plus candidate) entwined
          entwinedCandidates = fmap withEntwine candidates
      -- CR 702.33a: kicker, asked HERE -- after the modes and before the cost, the
      -- variable and the targets -- because that is where CR 601.2b puts the
      -- announcement of an additional cost, and rule 702.33a bundles nothing else
      -- into the question the way rule 702.42a bundles a mode choice.
      --
      -- The choice is never made for them: kickerOffer answers Nothing only where
      -- there is no option to offer -- no kicker, or no payable route -- and where
      -- there IS one, both answers go to the player.
      --
      -- Offered against the ENTWINED candidates, so a player who has already
      -- announced one additional cost is asked about this one only if the two
      -- together are payable (CR 601.2f's one total). No card carries both, and the
      -- composition is the rule rather than a guess about the pool.
      --
      -- Carried as the additional Cost itself rather than as a flag, for entwine's
      -- reason: the candidate costs below and the CR 702.33d stamp read one value.
      kicked <- case kickerOffer pid sid entwinedCandidates gs of
        Nothing -> pure Nothing
        Just extra -> do
          decision <- Game.choose (Prompt.ChooseKicker decider pid sid extra)
          pure $ case decision of
            KickerDecision.Kicks -> Just extra
            KickerDecision.Declines -> Nothing
      -- CR 702.33d: "if a spell's controller declares the intention to pay any of
      -- that spell's kicker costs, that spell has been kicked" -- the DECLARATION
      -- is what designates it, so the stamp lands here and not at CR 601.2h's
      -- payment. A cast that fails after this point rewinds to `before`, which
      -- takes the stamp with it along with the spell.
      Monad.when (Maybe.isJust kicked) (State.modify' (stampKicked sid))
      let withKicker candidate = maybe candidate (Cost.plus candidate) kicked
          payable = filter (payableCost pid sid gs) (fmap withKicker entwinedCandidates)
      if null payable
        then reject
        else do
          chosenCost <- case payable of
            [only] -> pure only
            _ -> Game.choose (Prompt.ChooseCost decider pid sid payable)
          if notElem chosenCost payable
            then reject
            else do
              let specs = Card.modesTargetSpecs chosenModes face
                  sets = Target.legalSets (Just pid) sid specs gs
              -- CR 601.2b's announcement is free -- any Natural -- but the player
              -- making it is told what the board can pay. The bound rides the
              -- CHOSEN cost, and nothing filters the answer against it: an
              -- unaffordable announcement still reverses the whole cast (#417).
              mAmount <-
                if Cost.hasVariable chosenCost
                  then fmap Just (Game.choose (Prompt.ChooseX decider pid sid (affordableX pid sid gs chosenCost)))
                  else pure Nothing
              -- CR 601.2: a step the player cannot comply with makes the casting
              -- illegal and returns the game to before it was proposed. The X just
              -- named is where that can first become true, since every candidate
              -- offered above passed payableCost at CR 601.2b's X=0 FLOOR -- the
              -- only value castability can measure before the announcement exists.
              --
              -- Asked with the same predicate the floor was asked with, so a gate
              -- and an announcement cannot disagree about what a cost is. That
              -- matters beyond tidiness: CR 118.13a's announcement below runs on
              -- this cost, and on a {X}{G/P} (Corrosive Gale) a large enough X
              -- leaves NEITHER of CR 107.4f's two routes payable -- whereupon
              -- Mana.announce would have to invent an offer.
              --
              -- Reject-not-repair: the announcement is NOT clamped to affordableX
              -- (CR 601.2b lets the player name the value freely), it is honoured
              -- and then loses the spell. Reversing here rather than at CR 601.2h's
              -- failed payment costs the player nothing, since everything between
              -- is undone by the same reversal (#56 is the prompts it still owes).
              --
              -- Asked unconditionally rather than only when there is an {X}: for a
              -- cost with none, `announcedAtX` IS the chosen candidate, which buys
              -- one predicate over one cost instead of two spellings of when the
              -- gate applies.
              let announcedAtX = maybe chosenCost (\x -> Cost.substituteX x chosenCost) mAmount
              if not (payableCost pid sid gs announcedAtX)
                then reject
                else do
                  -- CR 601.2b's own order puts the hybrid and Phyrexian
                  -- announcements AFTER the value of X and before CR 601.2c's
                  -- targets; CR 118.13a is what forbids deferring them to payment
                  -- time.
                  --
                  -- Cost.totalManas is handed in so that the routes offered are the
                  -- ones CR 601.2f's total can pay -- the same adjusted cost
                  -- payableCost gated this cast on, read from the same `gs` the
                  -- total below is.
                  announcedCost <- Cost.announce pid sid (Cost.totalManas (Cost.spellAdjustments pid sid gs)) announcedAtX
                  -- CR 601.2c, and the spell is on the stack for it: `sets` above
                  -- was computed from the same post-move `gs`, so a "target spell"
                  -- slot draws from the pool CR 601.2a built -- with this spell in
                  -- it, and CR 115.5 taking it back out.
                  chosen <- Target.chooseTargets decider pid sid specs sets
                  if not (Target.selectionLegal specs sets chosen)
                    then reject
                    else do
                      -- CR 601.2b then 601.2f: X substituted and the Phyrexian
                      -- symbols announced above, then the total cost. A criterion
                      -- is read against the spell's STACK incarnation, the
                      -- projection CR 601.2f's total is owed. Not the same
                      -- projection castable measured -- that one read `oid` in a
                      -- hand, this reads `sid` on the stack -- but provably the
                      -- same HALF, since asProposed stamped both.
                      --
                      -- CR 118.7e: a reduction written with a hybrid mana
                      -- symbol has its half chosen "at the time the cost
                      -- reduction is applied", which is this step and not CR
                      -- 601.2b's announcement above. The answers arrive as the
                      -- adjustments themselves, so `totalWith` applies exactly
                      -- what was chosen; `payableCost` gated the cast on some
                      -- resolution paying, so the answer given here can be a
                      -- worse one -- CR 118.7e attaches no condition to the
                      -- choice, and CR 601.2h's failed payment is what `reject`
                      -- below answers with.
                      adjustments <- Cost.announceReductions pid sid gs (Cost.spellAdjustments pid sid gs)
                      let paidCost = Cost.totalWith adjustments announcedCost
                      payment <- Cost.pay pid sid paidCost
                      case payment of
                        -- CR 601.2h: the payment failed, so the cast is illegal
                        -- and CR 601.2 returns the game to before it was proposed
                        -- -- which is what takes the spell back off the stack.
                        Payment.Unpaid -> reject
                        -- WHICH of the candidate costs was paid is not recorded
                        -- past this point (#101). CR 702.33d's kicker designation
                        -- is not that record: it says an additional cost was
                        -- announced, never which candidate carried it.
                        Payment.Paid -> do
                          -- CR 601.2i: the spell has been cast. Emitted AFTER the
                          -- last step that can fail, so a rejected announcement
                          -- records nothing.
                          --
                          -- `sid` is the spell -- the incarnation CR 601.2a put
                          -- on the stack, which is what a "whenever you cast a
                          -- [type] spell" trigger reads its characteristics off
                          -- (TriggerCondition.SpellCast).
                          --
                          -- The snapshot is projected off the SAME state the
                          -- event is appended to, and taken here rather than
                          -- left to a reader for GameEvent.Moved's reason: by
                          -- the time a look-back count folds the log, `sid` has
                          -- resolved or been countered and the projection would
                          -- have nothing to read. CR 601.2i has already applied
                          -- the effects that modify the spell as it is cast, so
                          -- this records what became cast.
                          State.modify' (\g -> Event.recordEvent (GameEvent.SpellCast pid sid (Projection.project sid g)) g)
                          -- Stamped on `sid` itself, the incarnation CR 601.2a
                          -- put on the stack, rather than on whatever is on top
                          -- of it now.
                          --
                          -- CR 109.5: "The words 'you' and 'your' on an object
                          -- refer to the object's controller, its would-be
                          -- controller (if a player is attempting to play, cast,
                          -- or activate it)". `pid` is both here -- the caster is
                          -- the spell's controller -- so the slot is stamped
                          -- alongside the chosen targets, as
                          -- Activate.activateAbility does for CR 109.5's
                          -- activated-ability sentence and Engine.placeBorne for
                          -- its triggered-ability one. Char's "and 2 damage to
                          -- you" is what reads it (Pawl.CastSpec's Char case).
                          State.modify'
                            ( \g ->
                                g
                                  { GameState.objects =
                                      Map.adjust
                                        (\o -> o {Object.bindings = Binding.setYou pid (Binding.fromChoices chosen mAmount chosenModes)})
                                        sid
                                        (GameState.objects g)
                                  }
                            )
                          Monad.when (castFrom == Just Zone.Graveyard) (armCastFromGraveyard pid face sid)
                          -- CR 903.8: the cast is now announced, so this is a
                          -- "previous time they cast it from the command zone"
                          -- for every later one. Here rather than at CR 601.2a's
                          -- move, because everything above this line can still
                          -- reject the cast and rewind to `before` -- a proposal
                          -- the game returned from was never a cast (CR 601.2),
                          -- and must not make the next one dearer.
                          --
                          -- Counted for the CASTER, who by
                          -- Commander.canCastFromCommandZone is also the owner:
                          -- rule 903.8 lets nobody else cast it from there.
                          Monad.when (castFrom == Just Zone.Command) (State.modify' (Commander.recordCast pid))

-- CR 702.34a's SECOND static ability -- exile this card instead of putting it
-- anywhere else any time it would leave the stack -- installed onto the spell's
-- new stack incarnation as a floating replacement (CR 614.3). The effects
-- themselves come from Pawl.Engine.Keyword, so this installs a replacement it
-- never inspects.
--
-- ARMED HERE rather than re-derived from the card while the spell sits on the
-- stack because CR 702.34a conditions the ability on "if the flashback cost was
-- paid", and nothing downstream records which cost was paid (#101). This is the
-- one point in the engine that knows: castSpell read the proposing zone one step
-- ahead of CR 601.2a's move, and Pawl.Engine.Cost.costsFor offers no candidate but
-- the flashback cost from a graveyard, so for every card in this pool the two
-- facts coincide. NOT the general rule -- a card cast from a graveyard under some
-- other permission would be exiled here when CR 702.34a says it should not be,
-- which is #101's gap.
--
-- CR 614.3's `uses` is Once and its expiry is Never: a spell leaves the stack
-- exactly once, and the ability has no duration.
armCastFromGraveyard :: PlayerId -> Face.Face Card.Type.Card -> ObjectId -> Game ()
armCastFromGraveyard caster face spellId =
  let arm re = State.modify' $ \gs ->
        let (ts, gs1) = Game.freshTimestamp gs
            active =
              ActiveReplacement.MkActiveReplacement
                { ActiveReplacement.effect = re,
                  -- CR 113.7: the source is the spell itself, which is what the
                  -- pattern's Filter.IsSource is compared against.
                  ActiveReplacement.source = spellId,
                  -- CR 109.5: the caster. Nothing in CR 702.34a's exile reads it
                  -- -- the pattern is Filter.IsSource under
                  -- ControllerRelation.Anyones, and neither consults the
                  -- perspective the row supplies -- but the row carries it as
                  -- every other row does, and a redirect that named a
                  -- Filter.ControlledBy would.
                  ActiveReplacement.controller = caster,
                  ActiveReplacement.timestamp = ts,
                  ActiveReplacement.expiry = Expiry.Never,
                  ActiveReplacement.uses = Uses.Once,
                  ActiveReplacement.origin = ReplacementOrigin.Other,
                  ActiveReplacement.rider = Nothing
                }
         in gs1 {GameState.replacements = active : GameState.replacements gs1}
   in Monad.mapM_ arm (Keyword.castFromGraveyardReplacementsOf (Face.keywords face))
