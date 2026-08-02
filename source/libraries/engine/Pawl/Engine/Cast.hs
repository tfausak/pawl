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
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.CastingRestriction as CastingRestriction
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.Expiry as Expiry
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
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- CR 117.1a's second sentence: the window a NONINSTANT spell is cast in -- a
-- main phase of its controller's own turn, with an empty stack. Every noninstant
-- card type restates it in the same words -- CR 301.1 artifact, CR 302.1
-- creature, CR 303.1 enchantment, CR 306.1 planeswalker, CR 307.1 sorcery -- and
-- this one predicate is all five of them. "Sorcery speed" is the colloquial name
-- for the window and appears nowhere in the rules. (The priority requirement is
-- implicit: the engine only offers actions to the player who holds priority.)
--
-- The window the CARD TYPE gets, which is not always the window the card gets:
-- rule 702.8a's flash lifts a card out of it (instantSpeed below). This function
-- is only the window itself.
--
-- CR 307.1's window, shared with the CR 307.5 one an ability can carry
-- (Activate.timingOk) -- see Turn.sorcerySpeedWindow for why there is one copy.
sorcerySpeed :: PlayerId -> GameState -> Bool
sorcerySpeed = Turn.sorcerySpeedWindow

-- CR 117.1a is the default this implements: "A player may cast an instant spell
-- any time they have priority. A player may cast a noninstant spell during their
-- main phase any time they have priority and the stack is empty." Priority is
-- implicit -- the engine only offers actions to the priority holder -- so what
-- is left is the first sentence for an instant (CR 304.1) and the second for
-- everything else (CR 302.1 / 307.1).
--
-- Flash is that second sentence being OVERRIDDEN rather than restated, which is
-- CR 101.1: "Whenever a card's text directly contradicts these rules, the card
-- takes precedence. The card overrides only the rule that applies to that
-- specific situation." Rule 702.8a is the card's text and it contradicts rule
-- 117.1a's second sentence flatly, so instantSpeed's disjunction is CR 101.1
-- resolved, not CR 117.1a read generously.
--
-- The window the RULES give a spell, and not the whole of when it may be cast: a
-- card may narrow this further with a printed restriction (CR 601.3), which
-- `castable` conjoins separately through printedRestrictionsOk.
timingOk :: PlayerId -> ObjectId -> GameState -> Bool
timingOk pid oid gs = case Game.cardOf oid gs of
  Nothing -> False
  Just card -> instantSpeed card || sorcerySpeed pid gs

-- CR 304.1 / 702.8a: is this card one the rules let its controller cast whenever
-- they have priority, rather than only in the sorcery-speed window? Two ways in,
-- and they are two because one is a CARD TYPE and the other is a KEYWORD.
--
-- Rule 702.8a: "'Flash' means 'You may play this card any time you could cast an
-- instant.'" -- this predicate's second disjunct in the rule's own words, and
-- the first thing in the engine to WIDEN the rules' own window from the card.
-- Narrowing it from the card is older: printedRestrictionsOk has read CR 601.3
-- clauses like Rally the Troops' "only during the declare attackers step" since
-- before this, which is why timingOk's haddock says the window here is not the
-- whole of when a spell may be cast.
--
-- Lifted HERE, in Cast's disjunction, and emphatically not inside
-- Turn.sorcerySpeedWindow: that window has one copy because CR 307.1 (a spell)
-- and CR 307.5 (an "activate only as a sorcery" ability, Activate.timingOk) are
-- the same three conjuncts, and rule 702.8a is about neither. Flash is a
-- permission a CARD carries about casting ITSELF, so widening the shared window
-- would make an equip ability on the same board instant-speed, which no rule
-- says. Pawl.ActivateSpec's "flash does not make an activated ability
-- instant-speed" is what proves it did not.
--
-- Read off the PRINTED keywords rather than through the CR 613 projection --
-- Keyword.hasFlash carries that argument, which is CR 113.6e for the zones and
-- #160 for why printed and projected agree in them -- and read wherever the cast
-- is being proposed from, since Game.cardOf is zone-agnostic and `castable` asks
-- this of every zone in castZones. Rule 702.8a's "functions in any zone from
-- which you could play the card it's on" is that.
--
-- The PLAYER-scoped sibling is not this and is not built: an effect that lets a
-- player cast OTHER spells as though they had flash (CR 601.3b, Vedalken Orrery)
-- would be read here beside this predicate, never folded into it (#565).
instantSpeed :: Card.Type.Card -> Bool
instantSpeed card = Card.isInstant card || Keyword.hasFlash (Card.Type.keywords card)

-- CR 601.2c / 700.2a: castable when at least as many modes are fillable as the
-- selection demands ("choose one" / ChooseExactly 1, the only selection so
-- far). Unobservable for Bolt (AnyTarget always holds a living player); first
-- falsified for a single-mode card by Giant Growth at M3b, and for a modal
-- card by Chaos Charm at M4g (castable via its damage/haste mode with no Wall
-- in play). For a non-modal card (one mode, count 1) this is identical to
-- "every slot fillable": the single mode fillable = all its slots fillable =
-- the whole card's slots.
-- CR 109.5 / 601.2a: the perspective a "target creature an opponent controls"
-- slot is measured against is the player CASTING the spell, who becomes its
-- controller the moment it is put on the stack. Taken as a parameter rather than
-- read off the card, which is in a hand and has no controller at all.
targetable :: PlayerId -> ObjectId -> GameState -> Bool
targetable pid oid gs = case Game.cardOf oid gs of
  Nothing -> False
  Just card ->
    let count = Modal.selectionCount (Card.Type.spell card)
     in Natural.length (Target.fillableModes (Just pid) oid (Card.enchantSpecs card) (Card.Type.spell card) gs) >= count

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
-- The X=0 floor USED to be one place they still could, since the announcement
-- runs on the value the player named; castSpell now asks this same predicate
-- again once that value exists, which is what closed it (#417).
payableCost :: PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCost = payableCostAt 0

-- The same question asked at some OTHER value of X. `payableCost` is this at CR
-- 601.2b's floor, and `affordableX` is this climbed; one predicate, so what the
-- gate measures and what the bound reports cannot drift apart.
payableCostAt :: Natural -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCostAt x pid oid gs cost = Cost.canPay pid oid (Cost.total pid oid (Cost.substituteX x cost) gs) gs

-- CR 601.2b: the greatest value of X this player could actually pay for, which is
-- what Prompt.ChooseX carries. Measured on the cost the cast is measuring -- the
-- candidate chosen at CR 601.2b, totalled at CR 601.2f -- and with the same
-- predicate castability was gated on, never a freshly derived one.
--
-- Advisory, and nothing here clamps: see Prompt.ChooseX for why announcing past
-- this is legal (CR 601.2b) and what it costs the player (CR 601.2, #56).
--
-- The SEARCH is Cost.greatestPayableX, shared with Activate.affordableX so that a
-- spell and an activation cannot climb differently; what is not shared is the
-- predicate, since an activation cost skips CR 601.2f's totalling (#90). This
-- haddock is what discharges that search's monotonicity requirement for the
-- spell's predicate.
--
-- FOUND BY ASCENDING SEARCH from 0, which is only sound because payability is
-- MONOTONE in X -- unpayable at n means unpayable at every value above n. It is,
-- and for reasons that hold structurally rather than by inspection of the pool:
--
--   1. X reaches a cost ONLY as generic mana. Mana.substituteX rewrites each
--      Variable symbol to Generic x and touches nothing else, and Cost.substituteX
--      rewrites the mana part alone -- so raising X raises the generic demand (by
--      one per {X} symbol) while every typed symbol, and every CostComponent, is
--      exactly what it was. Cost.canPay's component half therefore answers the
--      same at every X.
--   2. CR 601.2f's totalling cannot undo that. Cost.total's increases and
--      reductions come from PlayerEffect.costAdjustments, which reads the player,
--      the object and the state and never the cost, so the amount added and the
--      amount taken off are the same at every X. applyAdjustments pools them into
--      the one generic component, which is monotone in the cost's own generic sum
--      and floored at 0 by CR 601.2f ("It can't be reduced to less than {0}"); the
--      typed cancellation it performs does not read the generic part at all.
--   3. More generic mana is never easier to pay. Mana.canPay asks of each of CR
--      601.2b's nonhybrid resolutions that CR 119.4 admit the life, that Hall's
--      condition hold for the typed demands, and that the supplies left over cover
--      the generic part. Only that last one reads the generic count, and it reads
--      it on the demanding side of a >= whose supply side X cannot move.
--
-- The same three facts are why the climb TERMINATES. Each step raises the cost's
-- own generic sum by at least one (one per {X} symbol) while the amount the
-- reductions take off is a constant, so the TOTALLED generic demand grows without
-- bound -- the CR 601.2f floor delays that but cannot stop it -- and the supplies
-- are finite, so the generic comparison in (3) fails at some X no greater than
-- the mana available plus whatever the reductions forgive.
--
-- The two degenerate costs -- one with no {X} in it, which would climb forever,
-- and one unpayable even at X=0 -- both answer 0, and Cost.greatestPayableX says
-- why. Neither is reachable from castSpell, which asks only about a candidate
-- that already passed payableCost and only when Cost.hasVariable holds.
affordableX :: PlayerId -> ObjectId -> GameState -> Cost Keyword -> Natural
affordableX pid oid gs cost = Cost.greatestPayableX (\x -> payableCostAt x pid oid gs cost) cost

-- CR 702.42a: the ADDITIONAL cost this player may pay right now to choose all of
-- this modal spell's modes -- "You may choose all modes of this spell instead of
-- just the number specified. If you do, you pay an additional [cost]" -- or
-- Nothing when entwining is not on offer at all.
--
-- Three conditions, and each is a different rule:
--
--   1. The card HAS entwine. Rule 702.42a is a static ability of the spell
--      itself, so it is read off the card's printed keywords (Keyword.entwineCost)
--      one step before CR 601.2a moves it to the stack.
--   2. Every printed mode is LEGAL. CR 700.2a: "If one of the modes would be
--      illegal (due to an inability to choose legal targets, for example), that
--      mode can't be chosen." Choosing ALL modes is therefore not open when one
--      of them cannot be chosen. Unobservable for Dream's Grip, whose two modes
--      draw from the same unfiltered pool and so are fillable together or not at
--      all, and written anyway: without it an entwined cast would announce fewer
--      modes than rule 702.42a says it chose, and castSpell's own size check
--      would turn the whole cast into a silent no-op.
--   3. Some candidate cost plus this one is payable -- CR 601.2f's "plus all
--      additional costs", measured with the same payableCost predicate
--      castability was gated on, at CR 601.2b's X=0 floor. An option the player
--      cannot take is not offered, which is the same posture ChooseCost takes
--      towards an unaffordable alternative.
--
-- None of the three is a choice being made for the player: an option the card
-- does not have, that CR 700.2a closes, or that CR 118.3 says cannot be paid, is
-- not an option.
--
-- WHICH candidate will carry the cost is not decided here: this answers only
-- whether SOME route pays it, and castSpell narrows the candidates to the routes
-- that really do once the answer is in.
entwineOffer :: PlayerId -> ObjectId -> GameState -> Maybe (Cost Keyword)
entwineOffer pid oid gs = case Game.cardOf oid gs of
  Nothing -> Nothing
  Just card -> do
    cost <- Keyword.entwineCost (Card.Type.keywords card)
    let modal = Card.Type.spell card
        legal = Target.fillableModes (Just pid) oid (Card.enchantSpecs card) modal gs
    Monad.guard (Natural.length legal == Modal.modeCount modal)
    Monad.guard (any (\candidate -> payableCost pid oid gs (Cost.plus candidate cost)) (Cost.costsFor oid gs))
    pure cost

-- CR 601.3: the zones a spell can be cast from at all, in the engine's
-- canonical order -- what castableSpells scans, and the list castableZones
-- filters, so the two can never disagree about where to look.
--
-- The library is deliberately absent: Panglacial Wurm's permission is scoped to
-- a search in progress (castableWhileSearching) rather than to the whole game,
-- so it is not a zone a player may simply cast from.
castZones :: [Zone.Zone]
castZones = [Zone.Hand, Zone.Graveyard]

-- Which of those zones THIS card may be cast from -- where a permission turns
-- into a zone.
castableZones :: Card.Type.Card -> [Zone.Zone]
castableZones card =
  let permitted zone = case zone of
        -- CR 304.1 / 307.1: the rules' own allowance is worded "from their
        -- hand", so the hand needs no permission of its own.
        Zone.Hand -> True
        Zone.Graveyard -> permitsCastFromGraveyard card
        -- No other zone is in castZones.
        _ -> False
   in filter permitted castZones

-- Is this object somewhere this player may cast it from?
inCastableZone :: PlayerId -> ObjectId -> GameState -> Bool
inCastableZone pid oid gs =
  case Game.cardOf oid gs of
    Nothing -> False
    Just card -> any (\zone -> elem oid (Game.zoneMembers zone pid gs)) (castableZones card)

-- CR 205.4e: "Any instant or sorcery spell with the supertype 'legendary' is
-- subject to a casting restriction. A player can't cast a legendary instant or
-- sorcery spell unless that player controls a legendary creature or a legendary
-- planeswalker."
--
-- A RULE, not a card-carried permission. Rule 205.4e restricts the PLAYER from
-- the rulebook, so it is checked here beside the CR 601.3 checks and is
-- emphatically not a CastingPermission -- a card-carried version would be the
-- rules core learning from data a restriction it already knows. Reading a
-- supertype and a card type is the same closed-half act as CR 704.5j's legend
-- rule; nothing here touches an effect's identity.
--
-- The SPELL's own type line is read PRINTED (Card.isLegendary): it is in a hand
-- or a graveyard when this is asked, where no projection exists, and CR 205.4a
-- makes supertypes a printed-type-line read regardless
-- (Projection.printedSupertypes says so).
legendaryRestrictionOk :: PlayerId -> ObjectId -> GameState -> Bool
legendaryRestrictionOk pid oid gs = case Game.cardOf oid gs of
  Nothing -> False
  Just card ->
    not (Card.isLegendary card && (Card.isInstant card || Card.isSorcery card))
      || controlsLegendaryCreatureOrPlaneswalker pid gs

-- CR 205.4e's condition. Read through the PROJECTION rather than off the
-- printed card, because "controls a legendary creature" is a question about the
-- object as it exists on the battlefield: a Clone copying Thalia is a legendary
-- creature (CR 707.2 lists supertype and card type among the copiable values)
-- though its own card carries no supertype at all, which is the same reading
-- Pawl.Engine.Sba.legendGroups takes for the legend rule.
--
-- BOTH of rule 205.4e's disjuncts, "a legendary creature or a legendary
-- planeswalker", read through the same projection for the same reason: a
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

-- CR 601.3's PROHIBITION half -- "no rule or effect prohibits that player from
-- casting it" -- as printed on the card about ITSELF. Rally the Troops' "Cast
-- this spell only during the declare attackers step and only if you've been
-- attacked this step."
--
-- The exact counterweight to permissionsOf above, and read the same way: off the
-- card, never through the projection. CR 113.6e is the rule -- "an object's
-- ability that restricts or modifies how that particular object can be played or
-- cast functions in any zone from which it could be played or cast and also on
-- the stack" -- which for this pool means a hand, where the CR 613 layer system
-- does not reach. ALL of them must hold, which is what CR 601.3's "no ...
-- prohibits" means; one permission, by contrast, suffices.
--
-- Casing on the arms is a classification, not an effect's identity: Pawl.Engine.Cast is
-- the sole reader of Pawl.Types.CastingRestriction exactly as it is of
-- CastingPermission.
printedRestrictionsOk :: PlayerId -> ObjectId -> GameState -> Bool
printedRestrictionsOk pid oid gs = case Game.cardOf oid gs of
  Nothing -> False
  Just card -> all (restrictionMet pid gs) (Card.Type.castingRestrictions card)

-- Does the game state satisfy this one printed clause?
restrictionMet :: PlayerId -> GameState -> CastingRestriction.CastingRestriction -> Bool
restrictionMet pid gs restriction = case restriction of
  -- CR 500.1: a step or a stepless phase, compared against the one the game is
  -- in. Reading GameState.phase is the same closed-half act as CR 307.1's
  -- main-phase test in Turn.sorcerySpeedWindow.
  CastingRestriction.DuringPhase phase -> GameState.phase gs == phase
  CastingRestriction.AttackedThisStep -> attackedThisStep pid gs

-- "only if you've been attacked this step", asked of the CASTING player.
--
-- CR 506.2: "During the combat phase of a two-player game, the nonactive player
-- is the defending player; that player, planeswalkers they control, and battles
-- they protect may be attacked." So the question is whether any creature was
-- declared attacking -- or put onto the battlefield attacking (CR 508.8) -- THIS
-- PLAYER, rather than a planeswalker of theirs, which is exactly membership in
-- Combat.attacked.
--
-- Eightfold Maze's ruling is the reading pinned here, both sentences of it: "If
-- all the attacking creatures attack your planeswalkers, you can't cast Eightfold
-- Maze. To cast it, a creature needs to have attacked _you_." That second
-- sentence is why this cannot be emptiness of the record, and CR 306.6 is what
-- made it observable: a creature really can attack a planeswalker instead.
--
-- Membership in the HISTORICAL set rather than a search of Combat.attackers,
-- because CR 506.4 removing the lone attacker from combat does not un-attack
-- anybody -- see Pawl.Types.Combat's `attacked` field. No separate
-- Combat.defender test either: only a defending player can be attacked, so an
-- OfPlayer entry naming this player IS that conjunct.
--
-- "THIS STEP" is read off the combat record, which CR 511.3 scopes to the whole
-- combat PHASE. The two spans coincide for every card in the pool -- the set is
-- written only by Pawl.Engine.Combat.declareAttackers (CR 508.1, the declare attackers
-- step's turn-based action) and by putOntoBattlefieldAttacking, whose every
-- producer resolves in a declare attackers step (a CR 508.3a attack trigger,
-- Hanweir Garrison's; a CR 603.7 delayed ability keyed to the beginning of that
-- step, Meandering Towershell's) -- but that is a fact about the pool rather
-- than a rule (#447). CR 508.4d's later-step entries are the ones that would
-- part them, and none is built (#368).
attackedThisStep :: PlayerId -> GameState -> Bool
attackedThisStep pid gs =
  Set.member (AttackTarget.OfPlayer pid) (Combat.attacked (GameState.combat gs))

-- Affordable and correctly timed, actually in a zone this player may cast it
-- from, fillable, and prohibited by nothing. CR 601.2b: affordable means at least
-- ONE candidate cost is payable -- a spell may have alternative costs, and only
-- one need be.
--
-- THREE prohibitions, not one, and they are three because they are carried by
-- three different things: a continuous effect on the player
-- (PlayerEffect.prohibitsCasting -- Rule of Law, Silence), the card's own printed
-- text (printedRestrictionsOk), and rule 205.4e itself
-- (legendaryRestrictionOk).
castable :: PlayerId -> ObjectId -> GameState -> Bool
castable pid oid gs =
  timingOk pid oid gs
    && inCastableZone pid oid gs
    -- CR 601.3: no rule or effect prohibits this player from casting a spell
    -- (Rule of Law, Silence). Gated HERE, upstream of Action.legalActions,
    -- because the engine never offers an illegal action and then rejects it.
    && not (PlayerEffect.prohibitsCasting pid gs)
    && printedRestrictionsOk pid oid gs
    && legendaryRestrictionOk pid oid gs
    && any (payableCost pid oid gs) (Cost.costsFor oid gs)
    && targetable pid oid gs

-- Every card this player may cast right now, in castZones' order -- so
-- Action.legalActions offers a flashback card in the graveyard exactly as it
-- offers one in hand. `castable` re-checks the permission per card, so a
-- graveyard with no flashback card in it contributes nothing.
castableSpells :: PlayerId -> GameState -> [ObjectId]
castableSpells pid gs =
  let inZone zone = filter (\oid -> castable pid oid gs) (Game.zoneMembers zone pid gs)
   in concatMap inZone castZones

-- CR 601.3 (Panglacial): may this card be cast from the library while its
-- controller searches their own library? A membership test on the card's casting
-- permissions -- Cast is the sole reader of CastingPermission, and this is a
-- classification, never card identity.
permitsCastWhileSearching :: Card.Type.Card -> Bool
permitsCastWhileSearching card =
  elem CastingPermission.CastFromLibraryWhileSearching (permissionsOf card)

-- CR 601.3 / 702.34a: may this card be cast from its owner's graveyard? The
-- flashback half of the same membership test.
permitsCastFromGraveyard :: Card.Type.Card -> Bool
permitsCastFromGraveyard card =
  elem CastingPermission.CastFromGraveyard (permissionsOf card)

-- Every casting permission a card has: the ones it PRINTS (Panglacial Wurm) plus
-- the ones rule 702 gives it for a keyword it holds (flashback). Read off the
-- card and never through the projection, which is what Card.castingPermissions'
-- own comment already argues: these permissions function in the library and the
-- graveyard (CR 113.6), which pawl's projection does not reach (#160).
permissionsOf :: Card.Type.Card -> [CastingPermission.CastingPermission]
permissionsOf card =
  Card.Type.castingPermissions card <> Keyword.castingPermissionsOf (Card.Type.keywords card)

-- The library cards this player may cast while searching their own library:
-- permitted, not prohibited, affordable (Mana.canPay), and with a fillable
-- target set (Cast.targetable). Deliberately omits timingOk -- the permission IS
-- the CR 601.3 timing exception (the ruling: "follows all normal rules ...
-- except for timing").
--
-- The prohibition is NOT omitted, and that is the point: CR 601.3 is one
-- sentence with two halves, and the Panglacial permission excepts only the
-- timing one. A Rule of Law still stops a cast from the library. CR 205.4e's
-- restriction rides along for the same reason -- it is not a timing rule, so the
-- permission does not reach it either. Unobservable in this pool (no card both
-- searches this way and is a legendary instant or sorcery) and written anyway,
-- because the alternative is a cast the rules forbid.
--
-- printedRestrictionsOk rides along too, and it is the closest call of the three:
-- a "Cast this spell only during the declare attackers step" IS about timing, so
-- the ruling's "except for timing" could be read to lift it. pawl takes the
-- narrower reading -- the ruling excepts the RULES' own timing window (CR 302.1 /
-- 307.1), not a prohibition the card prints on itself, which is what CR 601.3's
-- second half covers. Unobservable in this pool for the same reason as CR 205.4e:
-- Panglacial Wurm is the only card with the permission and it prints no
-- restriction.
castableWhileSearching :: PlayerId -> GameState -> [ObjectId]
castableWhileSearching pid gs =
  let permitted oid = maybe False permitsCastWhileSearching (Game.cardOf oid gs)
      affordable oid = any (payableCost pid oid gs) (Cost.costsFor oid gs)
      allowed oid =
        permitted oid
          && affordable oid
          && printedRestrictionsOk pid oid gs
          && legendaryRestrictionOk pid oid gs
          && targetable pid oid gs
   in if PlayerEffect.prohibitsCasting pid gs
        then []
        else filter allowed (Game.zoneMembers Zone.Library pid gs)

-- CR 601.3 (Panglacial): while a player searches their own library, offer them
-- the chance to cast a castable-while-searching card from it, before any card is
-- found (per the ruling). Loops so multiple copies may be cast (also per the
-- ruling); each cast removes a card from the library, so castableWhileSearching
-- shrinks and the loop terminates. castSpell is the re-entrant call -- casting
-- mid-resolution, the whole point.
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
          Monad.when (elem oid options) $ do
            castSpell pid oid
            castWhileSearching pid

-- CR 601.2b then 601.2c: choose modes, THEN targets (601.2c), pay (601.2f-h),
-- move to the stack (601.2a), stamp the choices on the NEW stack incarnation
-- (CR 400.7). Prompting before payment is 601.2's own order; there is no rewind
-- for mid-announcement failure (#56), and legalActions only offers affordable,
-- fully-fillable casts -- so every prompt below is answerable.
--
-- A legal answer CAN still fail after the prompt, and one class of it is
-- deliberate: castability asks whether SOME sequence of choices pays the cost,
-- and the mana window then asks the player to make them. A player who taps their
-- only Birds of Paradise for the wrong colour cannot pay, and Mana.payCost's own
-- haddock argues at length that the engine must let them. What must NOT happen is
-- pawl offering a route it can already see the total cost cannot pay, which is why
-- Cost.announce is handed CR 601.2f's totalling below.
--
-- An illegal answer at ANY step makes the
-- whole cast a no-op: reject-not-repair, the AssignCombatDamage posture. A
-- spell with no slots (in its chosen modes) asks nothing.
castSpell :: PlayerId -> ObjectId -> Game ()
castSpell pid oid = do
  gs <- State.get
  case Game.cardOf oid gs of
    Nothing -> pure ()
    Just card -> do
      let decider = Decide.deciderFor pid gs
          modal = Card.Type.spell card
          legal = Target.fillableModes (Just pid) oid (Card.enchantSpecs card) modal gs
      -- CR 702.42a: entwine, asked FIRST -- before the mode choice CR 601.2b
      -- lists first -- because rule 702.42a states the widened selection and the
      -- extra payment as ONE decision ("You may choose all modes of this spell
      -- instead of just the number specified. If you do, you pay an additional
      -- [cost]"). A player who entwines has thereby announced their mode choice,
      -- so there is nothing left for ChooseModes to ask; a player who declines
      -- is asked the ordinary question one line below, in 601.2b's own order.
      --
      -- The choice is never made for them. entwineOffer answers Nothing only
      -- where there is no option to offer -- no entwine, an illegal mode (CR
      -- 700.2a), or no payable route -- and where there IS one, both answers go
      -- to the player and the engine takes neither.
      --
      -- The answer is carried as the additional Cost itself rather than as a
      -- flag, so the two things it changes -- the mode count just below and the
      -- candidate costs further down -- read the same value.
      entwined <- case entwineOffer pid oid gs of
        Nothing -> pure Nothing
        Just extra -> do
          decision <- Trans.lift (Program.prompt (Prompt.ChooseEntwine decider pid oid extra))
          pure $ case decision of
            EntwineDecision.Entwines -> Just extra
            EntwineDecision.Declines -> Nothing
      -- CR 700.2 normally, CR 702.42a's "all modes" when the entwine cost is
      -- being paid. The printed ModeSelection is untouched either way: entwine
      -- overrides the count for this ONE cast, it does not reprint the card.
      let count = case entwined of
            Just _ -> Modal.modeCount modal
            Nothing -> Modal.selectionCount modal
      -- CR 601.2b: modes are chosen BEFORE X and targets. Elided (forced,
      -- unprompted) exactly when there is nothing to choose -- as many legal
      -- modes as the selection demands or fewer (a non-modal card's one mode,
      -- or a modal card whose only-just-fillable modes leave no real
      -- choice), #50. An entwined cast is always in that case: entwineOffer has
      -- already established that every mode is legal, so `legal` has exactly
      -- `count` members and CR 702.42a's "all modes" is the only answer.
      chosenModes <-
        if Natural.length legal <= count
          then pure legal
          else Trans.lift (Program.prompt (Prompt.ChooseModes decider pid oid legal count))
      -- Reject-not-repair: an answer that is not a size-`count` subset of the
      -- legal modes makes the whole cast a no-op, guarding every step below.
      Monad.when (Set.isSubsetOf chosenModes legal && Natural.length chosenModes == count) $ do
        -- CR 601.2b: the cost to be paid is announced after the modes and
        -- before X and targets. Only PAYABLE candidates are offered (CR
        -- 118.9b makes an alternative optional, so a player who can afford both
        -- is really choosing); one payable candidate is forced and unprompted.
        -- Reject-not-repair: an answer outside the offered set makes the whole
        -- cast a no-op.
        --
        -- CR 601.2f: "The total cost is the mana cost or alternative cost (as
        -- determined in rule 601.2b), plus all additional costs and cost
        -- increases". An announced entwine is added to every candidate BEFORE
        -- the payability filter, so the routes offered are the ones that can
        -- actually pay it -- and CR 118.9d is what makes it apply to an
        -- alternative cost as readily as to the printed one. What the caster
        -- then chooses between, and what is finally paid, already carries it.
        let withEntwine candidate = maybe candidate (Cost.plus candidate) entwined
            payable = filter (payableCost pid oid gs) (fmap withEntwine (Cost.costsFor oid gs))
        Monad.unless (null payable) $ do
          chosenCost <- case payable of
            [only] -> pure only
            _ -> Trans.lift (Program.prompt (Prompt.ChooseCost decider pid oid payable))
          Monad.when (elem chosenCost payable) $ do
            let sets = Target.legalSets (Just pid) oid (Card.modesTargetSpecs chosenModes card) gs
            -- CR 601.2b's announcement is free -- any Natural -- but the player
            -- making it is told what the board can pay. The bound rides the
            -- CHOSEN cost, so an alternative cost or a CR 601.2f adjustment moves
            -- it, and nothing filters the answer against it: an unaffordable
            -- announcement still reverses the whole cast (#417).
            mAmount <-
              if Cost.hasVariable chosenCost
                then fmap Just (Trans.lift (Program.prompt (Prompt.ChooseX decider pid oid (affordableX pid oid gs chosenCost))))
                else pure Nothing
            -- CR 601.2: "If a player is unable to comply with the requirements of
            -- a step listed below while performing that step, the casting of the
            -- spell is illegal; the game returns to the moment before the casting
            -- of that spell was proposed." The X the player just named is where
            -- that can first become true, and this is the step it becomes true in:
            -- every candidate offered above passed payableCost at CR 601.2b's X=0
            -- FLOOR, which is the only value castability can measure before the
            -- announcement exists.
            --
            -- Asked with the same predicate the floor was asked with, on the cost
            -- carrying the announced value, so a gate and an announcement cannot
            -- disagree about what a cost is. That matters beyond tidiness: CR
            -- 118.13a's Phyrexian announcement below runs on this cost, and on a
            -- {X}{G/P} (Corrosive Gale) a large enough X leaves NEITHER of CR
            -- 107.4f's two routes payable -- whereupon Mana.announcePhyrexian has
            -- no offer to make and would have to invent one. This gate is what
            -- keeps that arm out of reach, and its haddock says so.
            --
            -- Reject-not-repair, the posture every other step here takes: the
            -- announcement is NOT clamped to affordableX (CR 601.2b lets the
            -- player announce the value of the variable freely), it is honoured
            -- and then loses the spell. Reversing here rather than at CR 601.2h's
            -- failed payment costs the player nothing, since everything between
            -- is undone by the same reversal -- and it is the posture castability
            -- itself already takes, measuring payability at CR 601.2b instead of
            -- waiting for the payment to fail (#56 is the prompts that reversal
            -- still owes).
            --
            -- Asked unconditionally rather than only when there is an {X}: for a
            -- cost with none, `announcedAtX` IS the chosen candidate and this
            -- re-asks a question already answered above, which costs a payability
            -- check and buys one predicate over one cost instead of two spellings
            -- of when the gate applies.
            let announcedAtX = maybe chosenCost (\x -> Cost.substituteX x chosenCost) mAmount
            Monad.when (payableCost pid oid gs announcedAtX) $ do
              -- CR 601.2b's own order puts the Phyrexian announcement AFTER the
              -- value of X and before CR 601.2c's targets: "If a cost that will be
              -- paid as the spell is being cast includes Phyrexian mana symbols,
              -- the player announces whether they intend to pay 2 life or a
              -- corresponding colored mana cost for each of those symbols." CR
              -- 118.13a is what forbids deferring it to payment time.
              --
              -- Cost.totalMana is handed in so that the routes offered are the ones
              -- CR 601.2f's total can pay -- the same adjusted cost payableCost
              -- gated this cast on, and read from the same `gs` the total below is
              -- (ManaSpec's Mana.TotalCost group).
              announcedCost <- Cost.announce pid oid (Cost.totalMana pid oid gs) announcedAtX
              chosen <-
                if Map.null sets
                  then pure Map.empty
                  else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid oid sets))
              let keysAgree = Map.keysSet chosen == Map.keysSet sets
                  eachLegal = and (Map.intersectionWith Set.member chosen sets)
              Monad.when (keysAgree && eachLegal) $ do
                -- CR 612 binding: choose the basic land types for each
                -- text-change slot. Always answerable (the five basics), so no
                -- castability gate.
                let textSlots = Resolve.textChangeSlots card
                    ask slot = do
                      pair <- Trans.lift (Program.prompt (Prompt.ChooseBasicLandTypes decider pid oid slot))
                      pure (slot, pair)
                bound <- fmap Map.fromList (traverse ask textSlots)
                -- CR 601.2b then 601.2f: substitute X and announce the Phyrexian
                -- symbols (both above), then compute the total cost. The object is
                -- still in HAND here, one step before 601.2a moves it to the
                -- stack, so a criterion is read against its hand projection (#89).
                let paidCost = Cost.total pid oid announcedCost gs
                payment <- Cost.pay pid oid paidCost
                case payment of
                  Payment.Unpaid -> pure ()
                  -- Which of the candidate costs was paid is not recorded past
                  -- this point (#101); the chosen Cost is discarded once paid.
                  Payment.Paid -> do
                    -- Read BEFORE the move, which is the last moment the zone the
                    -- spell was cast from is knowable: CR 400.7 mints a new
                    -- incarnation on the stack with no memory of where it came
                    -- from. Read from the LIVE state rather than the `gs` this
                    -- cast opened with, so nothing a cost payment did is missed.
                    castFrom <- State.gets (fmap Object.zone . Game.lookupObject oid)
                    Event.changeZone oid Zone.Stack
                    -- CR 601.2i: the spell has been cast. Emitted here, AFTER
                    -- the last step that can fail, so a rejected announcement
                    -- records nothing.
                    State.modify' (Event.recordEvent (GameEvent.SpellCast pid))
                    moved <- State.get
                    case GameState.stack moved of
                      [] -> pure ()
                      top : _ -> do
                        State.put
                          moved
                            { GameState.objects =
                                Map.adjust
                                  (\o -> o {Object.bindings = Binding.fromChoices chosen bound mAmount chosenModes})
                                  top
                                  (GameState.objects moved)
                            }
                        Monad.when (castFrom == Just Zone.Graveyard) (armCastFromGraveyard card top)

-- CR 702.34a's SECOND static ability -- "exile this card instead of putting it
-- anywhere else any time it would leave the stack" -- installed onto the spell's
-- new stack incarnation as a floating replacement (CR 614.3). The effects
-- themselves come from Pawl.Engine.Keyword, so this function installs a replacement it
-- never inspects.
--
-- WHY IT IS ARMED HERE rather than re-derived from the card while the spell sits
-- on the stack: rule 702.34a conditions the ability on "if the flashback cost
-- was paid", and nothing downstream records which cost was paid (#101). This is
-- the one point in the engine that knows -- the object was in a graveyard one
-- line ago, and Pawl.Engine.Cost.costsFor offers no candidate but the flashback cost
-- from there, so for every card in this pool "was cast from the graveyard" and
-- "the flashback cost was paid" are the same fact. NOT the general rule: a card
-- cast from a graveyard under some other permission would be exiled here when
-- rule 702.34a says it should not be, which is precisely #101's gap and not a
-- claim this function makes.
--
-- CR 614.3's `uses` is Once and its expiry is Never: a spell leaves the stack
-- exactly once, and the ability has no duration -- it stops mattering because its
-- subject is gone, which is what the store spells Uses.Once.
armCastFromGraveyard :: Card.Type.Card -> ObjectId -> Game ()
armCastFromGraveyard card top =
  let arm re = State.modify' $ \gs ->
        let (ts, gs1) = Game.freshTimestamp gs
            active =
              ActiveReplacement.MkActiveReplacement
                { ActiveReplacement.effect = re,
                  -- CR 113.7: the source is the spell itself, which is also what
                  -- the pattern's TheSource subject is compared against.
                  ActiveReplacement.source = top,
                  ActiveReplacement.timestamp = ts,
                  ActiveReplacement.expiry = Expiry.Never,
                  ActiveReplacement.uses = Uses.Once
                }
         in gs1 {GameState.replacements = active : GameState.replacements gs1}
   in Monad.mapM_ arm (Keyword.castFromGraveyardReplacementsOf (Card.Type.keywords card))
