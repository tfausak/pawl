module Pawl.Engine.Cast where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.CastingRestriction as CastingRestriction
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Supertype as Supertype
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
-- Shared with the CR 307.5 window an ability can carry (Activate.timingOk) --
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
-- The window the RULES give a spell, and not the whole of when it may be cast: a
-- card may narrow this further with a printed restriction (CR 601.3), which
-- `castable` conjoins separately through printedRestrictionsOk.
timingOk :: PlayerId -> ObjectId -> CardName.CardName -> GameState -> Bool
timingOk pid oid name gs = case proposedFace oid name gs of
  Nothing -> False
  Just face -> instantSpeed face || sorcerySpeed pid gs

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
proposedFace :: ObjectId -> CardName.CardName -> GameState -> Maybe (Face.Face Card.Type.Card)
proposedFace oid name gs = fmap (Game.resolveFace (Just name)) (Game.cardOf oid gs)

-- The one half a card offers to cast, where it offers exactly one. Nothing for
-- a card with several -- CR 709.3's choice is the player's, and this makes it
-- for nobody.
--
-- Only castWhileSearching needs it, because Prompt.CastWhileSearching carries a
-- bare object id with no room for a face; the offer a MULTI-face card would
-- make from a library is therefore not made at all (#655).
soleCastableFace :: ObjectId -> GameState -> Maybe (Face.Face Card.Type.Card)
soleCastableFace oid gs = case fmap Card.castableFaces (Game.cardOf oid gs) of
  Just [face] -> Just face
  _ -> Nothing

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
-- The PLAYER-scoped sibling is not this and is not built: an effect that lets a
-- player cast OTHER spells as though they had flash (CR 601.3b, Vedalken Orrery)
-- would be read here beside this predicate, never folded into it (#565).
instantSpeed :: Face.Face Card.Type.Card -> Bool
instantSpeed face = Card.isInstant face || Keyword.hasFlash (Face.keywords face)

-- CR 601.2c / 700.2a: castable when at least as many modes are fillable as the
-- selection demands. For a non-modal card (one mode, count 1) this is identical
-- to "every slot fillable".
--
-- CR 109.5 / 601.2a: the perspective a "target creature an opponent controls"
-- slot is measured against is the player CASTING the spell. Taken as a parameter
-- rather than read off the card, which is in a hand and has no controller at all.
--
-- MEASURED BEFORE THE MOVE, which castSpell no longer is, and that is structural
-- rather than an oversight: this is an OFFER, computed by Action.legalActions
-- while every card is still where it was. The two agree for every card in this
-- pool, because the only object whose stack membership the move changes is the
-- spell itself and CR 115.5 takes that one back out of every stack pool
-- (Target.legalRecipients). What the CHOSEN HALF changes is no longer part of
-- that argument: the specs come from `proposedFace`, and `castable` hands this a
-- state with that same half stamped onto the OBJECT (asProposed), so a filter
-- that reads the spell's own characteristics reads the half being cast too.
targetable :: PlayerId -> ObjectId -> CardName.CardName -> GameState -> Bool
targetable pid oid name gs = case proposedFace oid name gs of
  Nothing -> False
  Just face ->
    let count = Modal.selectionCount (Face.spell face)
     in Natural.length (Target.fillableModes (Just pid) oid (Card.enchantSpecs face) (Face.spell face) gs) >= count

-- CR 601.2b's X=0 floor measured at CR 601.2f's total: a candidate cost is
-- affordable when it is payable with X=0 (the caster may always choose 0)
-- against the TOTAL cost, not the printed one. Taxing castability without taxing
-- payment lets the player underpay; taxing payment without taxing castability
-- offers a cast that cannot be afforded, and there is no mid-announcement
-- rewind (#56).
--
-- CR 118.13a's announcement is measured against the same total, and castSpell
-- hands Cost.totalMana in for exactly that reason: a gate and an offer that
-- disagree about what a cost is are two ways of getting the same question wrong.
-- castSpell asks this same predicate again once the announced X exists (#417).
--
-- The HALF being cast reaches CR 601.2f's adjustments through the state, not
-- through an argument: `cost` comes from the chosen face and Cost.total reads
-- the object's characteristics through Game.faceOf, so both callers hand in a
-- state with that face stamped on (asProposed) and the two cannot name different
-- halves. AdventureSpec's "Thalia taxes the Adventure half and not the creature
-- half" is what holds it.
payableCost :: PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCost = payableCostAt 0

-- The same question asked at some OTHER value of X. `payableCost` is this at CR
-- 601.2b's floor, and `affordableX` is this climbed; one predicate, so what the
-- gate measures and what the bound reports cannot drift apart.
payableCostAt :: Natural -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCostAt x pid oid gs cost = Cost.canPay pid oid (Cost.total pid oid (Cost.substituteX x cost) gs) gs

-- CR 601.2b: the greatest value of X this player could actually pay for, which is
-- what Prompt.ChooseX carries -- measured on the cost the cast is measuring, with
-- the same predicate castability was gated on.
--
-- Advisory, and nothing here clamps: see Prompt.ChooseX for why announcing past
-- this is legal (CR 601.2b) and what it costs the player (#56).
--
-- The SEARCH is Cost.greatestPayableX, shared with Activate.affordableX; the
-- PREDICATE is not, since an activation cost skips CR 601.2f's totalling (#90).
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

-- CR 601.3: the zones a spell can be cast from at all, in the engine's
-- canonical order -- what castableSpells scans, and the list castableZones
-- filters, so the two can never disagree about where to look.
--
-- The library is deliberately absent: Panglacial Wurm's permission is scoped to
-- a search in progress (castableWhileSearching) rather than to the whole game,
-- so it is not a zone a player may simply cast from.
castZones :: [Zone.Zone]
castZones = [Zone.Hand, Zone.Graveyard, Zone.Exile]

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
        -- hand", so the hand needs no permission of its own.
        Zone.Hand -> True
        Zone.Graveyard -> permitsCastFromGraveyard face
        Zone.Exile -> permitsCastFromExile pid oid face gs
        -- No other zone is in castZones.
        _ -> False
   in filter permitted castZones

-- CR 715.3d: may this player cast this half of this exiled card? Both halves of
-- the rule, and the second is why the Adventure half of an exiled adventurer
-- card is not offered while the same card in a hand offers both:
--
--   * "For as long as that card remains exiled, that player may play it" -- the
--     permission the resolution wrote (Object.playableFromExileBy), which naming
--     a player is what keeps it from being an offer to everyone.
--   * "It can't be cast as an Adventure this way" -- so the proposed face must
--     not be the Adventure one.
--
-- The rule's own last clause, "although other effects that allow a player to
-- cast it may allow a player to cast it as an Adventure", is about a DIFFERENT
-- permission granting the Adventure half from exile. No card in this pool grants
-- one, and this function is only ever asked about the CR 715.3d permission
-- (#669).
permitsCastFromExile :: PlayerId -> ObjectId -> Face.Face Card.Type.Card -> GameState -> Bool
permitsCastFromExile pid oid face gs =
  (Game.lookupObject oid gs >>= Object.playableFromExileBy) == Just pid
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
  CastingRestriction.AttackedThisStep -> attackedThisStep pid gs

-- "only if you've been attacked this step", asked of the CASTING player.
--
-- CR 506.2 makes the nonactive player the defending player, so the question is
-- whether any creature was DECLARED attacking THIS PLAYER rather than a
-- planeswalker of theirs -- exactly membership in Combat.declaredAttacked.
-- DECLARED, and not "or put onto the battlefield attacking": CR 508.4 says such
-- creatures never "attacked", and CR 508.3b spells out the player side. So this
-- reads Combat.declaredAttacked and NOT Combat.attacked, CR 508.8's wider set;
-- the two fields exist because the two rules disagree (see Pawl.Types.Combat).
-- Eightfold Maze's ruling pins the reading: a creature needs to have attacked
-- YOU, which is why this cannot be emptiness of the record, and CR 306.6 is what
-- made it observable.
--
-- Membership in the HISTORICAL set rather than a search of Combat.attackers,
-- because CR 506.4 removing the lone attacker from combat does not un-attack
-- anybody. No separate Combat.defender test either: only a defending player can
-- be attacked, so an OfPlayer entry naming this player IS that conjunct.
--
-- "THIS STEP" is read off the combat record, which CR 511.3 scopes to the whole
-- combat PHASE. The two spans coincide for every card in the pool because this
-- set is written ONLY by Pawl.Engine.Combat.declareAttackers, CR 508.1's
-- turn-based action. That is a fact about the pool rather than a rule (#447):
-- what remains open is a second declaration inside one phase.
attackedThisStep :: PlayerId -> GameState -> Bool
attackedThisStep pid gs =
  Set.member (AttackTarget.OfPlayer pid) (Combat.declaredAttacked (GameState.combat gs))

-- CR 709.3a / 715.3a: the half being cast, RECORDED ON THE OBJECT, so that every
-- characteristic read of it resolves through Game.resolveFace to that half alone
-- (CR 709.3b / 715.3b) rather than to the unnamed fallback -- CR 709.4's combined
-- view for a split card, CR 715.4's normal half for an adventurer card.
--
-- ONE writer, two callers, which is the point. castSpell stamps the CR 400.7
-- incarnation CR 601.2a has just put on the stack and keeps the result; castable
-- stamps a state it only READS, since the card has not moved and nothing here
-- moves it. What a gate measures and what the incarnation shows therefore cannot
-- name different halves.
--
-- NOT a simulation of CR 601.2a's move, and it does not need to be: both rules
-- say outright that castability is evaluated against the chosen half -- CR 709.3a
-- "only the chosen half is evaluated to see if it can be cast", CR 715.3a "only
-- the alternative characteristics are evaluated to see if it can be cast" -- and
-- CR 601.2b's last sentence puts that choice before the announcement. So no
-- object is minted (CR 400.7), no CR 616.1 replacement loop runs, and nothing
-- prompts.
--
-- What the offer's state and the real stack incarnation still differ in is the
-- object's ZONE, and no cost adjustment reads one: Pawl.Engine.Filter's View
-- carries no zone axis, and Projection.viewOfObject applies no zone gate --
-- projectGiven falls through to the full layer fold off the battlefield (see
-- Pawl.Types.Affected's MatchingAnywhere).
asProposed :: ObjectId -> CardName.CardName -> GameState -> GameState
asProposed oid name gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.face = Just name}) oid (GameState.objects gs)}

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
-- half. That name reaches the conjuncts TWICE over, and both are needed: as an
-- argument, which is how the ones that read the card resolve their face, and as
-- `asProposed`'s stamp, which is how the ones that read the OBJECT -- CR 601.2f's
-- adjustments through Cost.total, and the target specs -- resolve theirs.
-- Thalia's "noncreature spells cost {1} more to cast" is the observable: it taxes
-- the Sorcery half of an adventurer card and not the Creature half, off one card
-- in one hand.
castable :: PlayerId -> ObjectId -> CardName.CardName -> GameState -> Bool
castable pid oid name gs =
  let proposed = asProposed oid name gs
   in timingOk pid oid name proposed
        && inCastableZone pid oid name proposed
        -- CR 601.3: gated HERE, upstream of Action.legalActions, because the
        -- engine never offers an illegal action and then rejects it. The half's
        -- own name goes with it, since CR 601.3a's prohibitions name a quality of
        -- the spell (Null Chamber) and CR 709.3a evaluates only the chosen half.
        && not (PlayerEffect.prohibitsCasting pid name proposed)
        && printedRestrictionsOk pid oid name proposed
        && legendaryRestrictionOk pid oid name proposed
        && any (payableCost pid oid proposed) (Cost.costsFor name oid proposed)
        && targetable pid oid name proposed

-- Every cast this player may propose right now, in castZones' order, as the
-- (object, half) pairs Action.Cast is built from. `castable` re-checks the
-- permission per card, so a graveyard with no flashback card in it contributes
-- nothing.
--
-- ONE ENTRY PER CASTABLE FACE, which is CR 709.3's choice being offered rather
-- than made: a split card in hand with both halves affordable appears twice,
-- and the player picks by picking an action. A card whose halves are separately
-- gated appears only as often as CR 709.3a lets it.
castableSpells :: PlayerId -> GameState -> [(ObjectId, CardName.CardName)]
castableSpells pid gs =
  let proposals oid = fmap (\face -> (oid, Face.name face)) (foldMap Card.castableFaces (Game.cardOf oid gs))
      offered oid = filter (\(_, name) -> castable pid oid name gs) (proposals oid)
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
permissionsOf :: Face.Face Card.Type.Card -> [CastingPermission.CastingPermission]
permissionsOf face =
  Face.castingPermissions face <> Keyword.castingPermissionsOf (Face.keywords face)

-- The library cards this player may cast while searching their own library:
-- permitted, not prohibited, affordable, and with a fillable target set.
-- Deliberately omits timingOk -- the permission IS the CR 601.3 timing exception
-- (the ruling: "follows all normal rules ... except for timing").
--
-- The prohibition is NOT omitted, and that is the point: CR 601.3 is one sentence
-- with two halves, and the Panglacial permission excepts only the timing one, so
-- a Rule of Law still stops a cast from the library, and so does a Null Chamber
-- that named the Wurm. CR 205.4e's restriction rides along for the same reason,
-- and THAT one is unobservable in this pool -- Panglacial Wurm is a creature and
-- never a legendary sorcery -- and is written anyway, because the alternative is
-- a cast the rules forbid.
--
-- printedRestrictionsOk rides along too, and it is the closest call of the three:
-- a "Cast this spell only during the declare attackers step" IS about timing, so
-- the ruling's "except for timing" could be read to lift it. pawl takes the
-- narrower reading -- the ruling excepts the RULES' own timing window (CR 302.1 /
-- 307.1), not a prohibition the card prints on itself. Unobservable: Panglacial
-- Wurm is the only card with the permission and it prints no restriction.
--
-- A card with several castable halves is not offered here at all, and that is
-- soleCastableFace's restriction rather than a rule: Prompt.CastWhileSearching
-- carries a bare object id, so two halves of one card would arrive at the
-- prompt indistinguishable (#655). No card in the pool holds the permission on
-- a multi-face card.
castableWhileSearching :: PlayerId -> GameState -> [ObjectId]
castableWhileSearching pid gs =
  let allowed oid = case soleCastableFace oid gs of
        Nothing -> False
        Just face ->
          let name = Face.name face
              -- The same stamped state `castable` gates on, for the same rule:
              -- CR 601.3's exception is about TIMING, so the half a library cast
              -- is evaluated against is still CR 709.3a's chosen one.
              proposed = asProposed oid name gs
           in permitsCastWhileSearching face
                -- CR 601.3's prohibit half, moved inside `allowed` when it grew
                -- a name: a quality-bearing prohibition stops one card without
                -- stopping the search's other candidates.
                && not (PlayerEffect.prohibitsCasting pid name proposed)
                && any (payableCost pid oid proposed) (Cost.costsFor name oid proposed)
                && printedRestrictionsOk pid oid name proposed
                && legendaryRestrictionOk pid oid name proposed
                && targetable pid oid name proposed
   in filter allowed (Game.zoneMembers Zone.Library pid gs)

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
      choice <- Trans.lift (Program.prompt (Prompt.CastWhileSearching decider pid options))
      case choice of
        Nothing -> pure ()
        Just oid ->
          -- Reject-not-repair: an option not in the offered set is a no-op that
          -- ends the loop, never a repair.
          Monad.when (elem oid options) $ case soleCastableFace oid gs of
            -- Unreachable: castableWhileSearching offered only ids that have
            -- one, and nothing has moved since. Total rather than partial.
            Nothing -> pure ()
            Just face -> do
              castSpell pid oid (Face.name face)
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
-- COSTS, which pawl offers by zone. CR 601.2b determines those at the proposal,
-- so locking them in there is the rule's own reading rather than a workaround.
--
-- REJECT-NOT-REPAIR, as a genuine rewind: an illegal answer at any step restores
-- `before`, which is what undoes the CR 601.2a move -- CR 601.2's own remedy, and
-- the posture Activate.activateAbility already takes. What the restore does NOT
-- undo is a prompt already issued (#56).
--
-- Every prompt below is answerable: legalActions only offers affordable,
-- fully-fillable casts. A legal answer CAN still fail after the prompt, and one
-- class of it is deliberate: castability asks whether SOME sequence of choices
-- pays the cost, and a player who then taps their only Birds of Paradise for the
-- wrong colour cannot pay (Mana.payCost argues why the engine must let them).
-- What must NOT happen is pawl offering a route it can already see the total cost
-- cannot pay, which is why Cost.announce is handed CR 601.2f's totalling below.
--
-- A spell with no slots (in its chosen modes) asks nothing.
--
-- `name` is CR 709.3's half-choice, ALREADY MADE: the rule puts it before the
-- card is put onto the stack, so it arrives with the proposal rather than being
-- prompted for here. CR 601.2b's last sentence is what that buys -- a
-- previously made choice may restrict the ones announced below.
castSpell :: PlayerId -> ObjectId -> CardName.CardName -> Game ()
castSpell pid oid name = do
  before <- State.get
  case proposedFace oid name before of
    Nothing -> pure ()
    Just face -> do
      let castFrom = fmap Object.zone (Game.lookupObject oid before)
          candidates = Cost.costsFor name oid before
      -- CR 601.2a. Nothing means the id was unknown or the CR 616.1 replacement
      -- loop cancelled the move, and a proposal whose first step did not happen
      -- is one the game returns from (CR 601.2).
      moved <- Event.changeZoneReturning oid Zone.Stack
      case moved of
        Nothing -> State.put before
        Just sid -> do
          -- CR 709.3a: "Only that half is considered to be put onto the stack."
          -- Stamped the instant the CR 601.2a move lands and before any of CR
          -- 601.2b's announcements, so every read of the stack incarnation from
          -- here on sees the chosen half alone (CR 709.3b) rather than CR
          -- 709.4's combined view -- entwineOffer's keywords and the CR 613
          -- projection included. Event.changeZone cleared the field on the way
          -- in (CR 400.7), so this is a write onto a fresh object.
          --
          -- Through asProposed, the same write castable's gate reads through, so
          -- the offer and the announcement cannot name different halves.
          --
          -- JUST AFTER the move, not during it: anything that runs inside CR
          -- 601.2a -- a CR 616.1 entry replacement, or a trigger the move fires
          -- -- reads the stack object before this stamp lands, and so sees CR
          -- 709.4's combined view instead of the chosen half. Not implemented;
          -- no card in the pool replaces or triggers on a cast card's move to
          -- the stack (#659).
          State.modify' (asProposed sid name)
          castProposed pid sid face castFrom candidates before

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
      decision <- Trans.lift (Program.prompt (Prompt.ChooseEntwine decider pid sid extra))
      pure $ case decision of
        EntwineDecision.Entwines -> Just extra
        EntwineDecision.Declines -> Nothing
  -- CR 700.2 normally, CR 702.42a's "all modes" when the entwine cost is being
  -- paid. The printed ModeSelection is untouched either way: entwine overrides
  -- the count for this ONE cast, it does not reprint the card.
  let count = case entwined of
        Just _ -> Modal.modeCount modal
        Nothing -> Modal.selectionCount modal
  -- CR 601.2b: modes are chosen BEFORE X and targets. Forced and unprompted
  -- exactly when there is nothing to choose -- as many legal modes as the
  -- selection demands or fewer, so every legal mode must be taken and the
  -- options are indistinguishable. An entwined cast is always in that case:
  -- entwineOffer has already established that every mode is legal, so `legal`
  -- has exactly `count` members and CR 702.42a's "all modes" is the only answer.
  --
  -- Two cards hold this branch and its complement in place, both "Choose two --"
  -- of four (ModalSpec): Cryptic Command, whose last two modes take no targets
  -- and whose "target permanent" mode is fillable in every state that can pay for
  -- it, so the prompt is always asked and is answered only when it really offers
  -- all four; and Ojutai's Command, whose two targeting modes look at a graveyard
  -- and the stack, which a cast can leave empty -- exactly two choosable modes,
  -- and a spec that fails if a prompt is issued.
  chosenModes <-
    if Natural.length legal <= count
      then pure legal
      else Trans.lift (Program.prompt (Prompt.ChooseModes decider pid sid legal count))
  -- Reject-not-repair: an answer that is not a size-`count` subset of the legal
  -- modes rewinds the whole cast, guarding every step below.
  if not (Set.isSubsetOf chosenModes legal && Natural.length chosenModes == count)
    then reject
    else do
      -- CR 601.2b: the cost to be paid is announced after the modes and before X
      -- and targets. Only PAYABLE candidates are offered (CR 118.9b makes an
      -- alternative optional, so a player who can afford both is really
      -- choosing); one payable candidate is forced and unprompted.
      -- Reject-not-repair: an answer outside the offered set rewinds the cast.
      --
      -- CR 601.2f: an announced entwine is added to every candidate BEFORE the
      -- payability filter, so the routes offered are the ones that can actually
      -- pay it -- and CR 118.9d is what makes it apply to an alternative cost as
      -- readily as to the printed one.
      let withEntwine candidate = maybe candidate (Cost.plus candidate) entwined
          payable = filter (payableCost pid sid gs) (fmap withEntwine candidates)
      if null payable
        then reject
        else do
          chosenCost <- case payable of
            [only] -> pure only
            _ -> Trans.lift (Program.prompt (Prompt.ChooseCost decider pid sid payable))
          if notElem chosenCost payable
            then reject
            else do
              let sets = Target.legalSets (Just pid) sid (Card.modesTargetSpecs chosenModes face) gs
              -- CR 601.2b's announcement is free -- any Natural -- but the player
              -- making it is told what the board can pay. The bound rides the
              -- CHOSEN cost, and nothing filters the answer against it: an
              -- unaffordable announcement still reverses the whole cast (#417).
              mAmount <-
                if Cost.hasVariable chosenCost
                  then fmap Just (Trans.lift (Program.prompt (Prompt.ChooseX decider pid sid (affordableX pid sid gs chosenCost))))
                  else pure Nothing
              -- CR 601.2: a step the player cannot comply with makes the casting
              -- illegal and returns the game to before it was proposed. The X just
              -- named is where that can first become true, since every candidate
              -- offered above passed payableCost at CR 601.2b's X=0 FLOOR -- the
              -- only value castability can measure before the announcement exists.
              --
              -- Asked with the same predicate the floor was asked with, so a gate
              -- and an announcement cannot disagree about what a cost is. That
              -- matters beyond tidiness: CR 118.13a's Phyrexian announcement below
              -- runs on this cost, and on a {X}{G/P} (Corrosive Gale) a large
              -- enough X leaves NEITHER of CR 107.4f's two routes payable --
              -- whereupon Mana.announcePhyrexian would have to invent an offer.
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
                  -- CR 601.2b's own order puts the Phyrexian announcement AFTER
                  -- the value of X and before CR 601.2c's targets; CR 118.13a is
                  -- what forbids deferring it to payment time.
                  --
                  -- Cost.totalMana is handed in so that the routes offered are the
                  -- ones CR 601.2f's total can pay -- the same adjusted cost
                  -- payableCost gated this cast on, read from the same `gs` the
                  -- total below is.
                  announcedCost <- Cost.announce pid sid (Cost.totalMana pid sid gs) announcedAtX
                  -- CR 601.2c, and the spell is on the stack for it: `sets` above
                  -- was computed from the same post-move `gs`, so a "target spell"
                  -- slot draws from the pool CR 601.2a built -- with this spell in
                  -- it, and CR 115.5 taking it back out.
                  chosen <-
                    if Map.null sets
                      then pure Map.empty
                      else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid sid sets))
                  let keysAgree = Map.keysSet chosen == Map.keysSet sets
                      eachLegal = and (Map.intersectionWith Set.member chosen sets)
                  if not (keysAgree && eachLegal)
                    then reject
                    else do
                      -- CR 601.2b then 601.2f: X substituted and the Phyrexian
                      -- symbols announced above, then the total cost. A criterion
                      -- is read against the spell's STACK incarnation, the
                      -- projection CR 601.2f's total is owed -- the same one
                      -- castable's own gate read through asProposed.
                      let paidCost = Cost.total pid sid announcedCost gs
                      payment <- Cost.pay pid sid paidCost
                      case payment of
                        -- CR 601.2h: the payment failed, so the cast is illegal
                        -- and CR 601.2 returns the game to before it was proposed
                        -- -- which is what takes the spell back off the stack.
                        Payment.Unpaid -> reject
                        -- Which of the candidate costs was paid is not recorded
                        -- past this point (#101).
                        Payment.Paid -> do
                          -- CR 601.2i: the spell has been cast. Emitted AFTER the
                          -- last step that can fail, so a rejected announcement
                          -- records nothing.
                          State.modify' (Event.recordEvent (GameEvent.SpellCast pid))
                          -- Stamped on `sid` itself, the incarnation CR 601.2a
                          -- put on the stack, rather than on whatever is on top
                          -- of it now.
                          State.modify'
                            ( \g ->
                                g
                                  { GameState.objects =
                                      Map.adjust
                                        (\o -> o {Object.bindings = Binding.fromChoices chosen mAmount chosenModes})
                                        sid
                                        (GameState.objects g)
                                  }
                            )
                          Monad.when (castFrom == Just Zone.Graveyard) (armCastFromGraveyard pid face sid)

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
                  -- CR 113.7: the source is the spell itself, which is also what
                  -- the pattern's TheSource subject is compared against.
                  ActiveReplacement.source = spellId,
                  -- CR 109.5: the caster. Nothing reads it -- CR 702.34a's exile
                  -- is a ZoneChangeR whose subject is TheSource, an identity test
                  -- with no relation to resolve -- but the row carries it as
                  -- every other row does.
                  ActiveReplacement.controller = caster,
                  ActiveReplacement.timestamp = ts,
                  ActiveReplacement.expiry = Expiry.Never,
                  ActiveReplacement.uses = Uses.Once,
                  ActiveReplacement.origin = ReplacementOrigin.Other
                }
         in gs1 {GameState.replacements = active : GameState.replacements gs1}
   in Monad.mapM_ arm (Keyword.castFromGraveyardReplacementsOf (Face.keywords face))
