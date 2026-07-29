module Pawl.Cast where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Cost as Cost
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Game as Game
import qualified Pawl.Keyword as Keyword
import qualified Pawl.Modal as Modal
import qualified Pawl.PlayerEffect as PlayerEffect
import qualified Pawl.Projection as Projection
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Target as Target
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CastingPermission as CastingPermission
import Pawl.Type.Cost (Cost)
import qualified Pawl.Type.Expiry as Expiry
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Payment as Payment
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.Uses as Uses
import qualified Pawl.Type.Zone as Zone

-- CR 302.1 / 307.1: a creature spell may be cast only when its controller could
-- cast a sorcery -- a main phase of their own turn, with an empty stack. Both
-- rules spell that window out in the same words; "sorcery speed" is the
-- colloquial name for it and appears nowhere in the rules. (The
-- priority requirement is implicit: the engine only offers actions to the player
-- who holds priority.)
--
-- CR 307.1's window, shared with the CR 307.5 one an ability can carry
-- (Activate.timingOk) -- see Turn.sorcerySpeedWindow for why there is one copy.
sorcerySpeed :: PlayerId -> GameState -> Bool
sorcerySpeed = Turn.sorcerySpeedWindow

-- CR 117.1a / 304.1: an instant is castable whenever its controller has
-- priority; anything else needs sorcery speed (CR 302.1 / 307.1). Priority is
-- implicit: the engine only offers actions to the priority holder.
timingOk :: PlayerId -> ObjectId -> GameState -> Bool
timingOk pid oid gs = case Game.cardOf oid gs of
  Nothing -> False
  Just card -> Card.isInstant card || sorcerySpeed pid gs

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
payableCost :: PlayerId -> ObjectId -> GameState -> Cost -> Bool
payableCost pid oid gs cost = Cost.canPay pid oid (Cost.total pid oid (Cost.substituteX 0 cost) gs) gs

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
      || controlsLegendaryCreature pid gs

-- CR 205.4e's condition. Read through the PROJECTION rather than off the
-- printed card, because "controls a legendary creature" is a question about the
-- object as it exists on the battlefield: a Clone copying Thalia is a legendary
-- creature (CR 707.2 lists supertype and card type among the copiable values)
-- though its own card carries no supertype at all, which is the same reading
-- Pawl.Sba.legendGroups takes for the legend rule.
--
-- Rule 205.4e's second disjunct, "or a legendary planeswalker", is not
-- implemented: Pawl.Type.CardType has no Planeswalker constructor to test for
-- (#301).
controlsLegendaryCreature :: PlayerId -> GameState -> Bool
controlsLegendaryCreature pid gs =
  let qualifies oid =
        Projection.controllerOf oid gs == Just pid
          && Set.member Supertype.Legendary (Projection.supertypesOf oid gs)
          && Projection.isCreatureOf oid gs
   in any qualifies (GameState.battlefield gs)

-- Affordable and correctly timed, actually in a zone this player may cast it
-- from, fillable, and not prohibited. CR 601.2b: affordable means at least ONE
-- candidate cost is payable -- a spell may have alternative costs, and only one
-- need be.
castable :: PlayerId -> ObjectId -> GameState -> Bool
castable pid oid gs =
  timingOk pid oid gs
    && inCastableZone pid oid gs
    -- CR 601.3: no rule or effect prohibits this player from casting a spell
    -- (Rule of Law, Silence). Gated HERE, upstream of Action.legalActions,
    -- because the engine never offers an illegal action and then rejects it.
    && not (PlayerEffect.prohibitsCasting pid gs)
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
-- graveyard (CR 113.6), where the CR 613 layer system does not reach.
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
castableWhileSearching :: PlayerId -> GameState -> [ObjectId]
castableWhileSearching pid gs =
  let permitted oid = maybe False permitsCastWhileSearching (Game.cardOf oid gs)
      affordable oid = any (payableCost pid oid gs) (Cost.costsFor oid gs)
      allowed oid = permitted oid && affordable oid && legendaryRestrictionOk pid oid gs && targetable pid oid gs
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
-- for mid-announcement failure (#56) -- legalActions only offers affordable,
-- fully-fillable casts, so a legal answer cannot fail after the prompt.
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
          legal = Target.fillableModes (Just pid) oid (Card.enchantSpecs card) (Card.Type.spell card) gs
          count = Modal.selectionCount (Card.Type.spell card)
      -- CR 601.2b: modes are chosen BEFORE X and targets. Elided (forced,
      -- unprompted) exactly when there is nothing to choose -- as many legal
      -- modes as the selection demands or fewer (a non-modal card's one mode,
      -- or a modal card whose only-just-fillable modes leave no real
      -- choice), #50.
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
        let payable = filter (payableCost pid oid gs) (Cost.costsFor oid gs)
        Monad.unless (null payable) $ do
          chosenCost <- case payable of
            [only] -> pure only
            _ -> Trans.lift (Program.prompt (Prompt.ChooseCost decider pid oid payable))
          Monad.when (elem chosenCost payable) $ do
            let sets = Target.legalSets (Just pid) oid (Card.modesTargetSpecs chosenModes card) gs
            mAmount <-
              if Cost.hasVariable chosenCost
                then fmap Just (Trans.lift (Program.prompt (Prompt.ChooseX decider pid oid)))
                else pure Nothing
            -- CR 601.2b's own order puts the Phyrexian announcement AFTER the
            -- value of X and before CR 601.2c's targets: "If a cost that will be
            -- paid as the spell is being cast includes Phyrexian mana symbols,
            -- the player announces whether they intend to pay 2 life or a
            -- corresponding colored mana cost for each of those symbols." CR
            -- 118.13a is what forbids deferring it to payment time.
            announcedCost <- Cost.announce pid oid (maybe chosenCost (\x -> Cost.substituteX x chosenCost) mAmount)
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
-- themselves come from Pawl.Keyword, so this function installs a replacement it
-- never inspects.
--
-- WHY IT IS ARMED HERE rather than re-derived from the card while the spell sits
-- on the stack: rule 702.34a conditions the ability on "if the flashback cost
-- was paid", and nothing downstream records which cost was paid (#101). This is
-- the one point in the engine that knows -- the object was in a graveyard one
-- line ago, and Pawl.Cost.costsFor offers no candidate but the flashback cost
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
