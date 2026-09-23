module Pawl.Engine.Activate where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.ActivationRestriction as ActivationRestriction
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Crewing as Crewing
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.Modal as Modal.Type
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StackObjectKind as StackObjectKind
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 602.2a: an ability activated from a hidden zone reveals the card that has
-- it (CR 701.20a). Note what the rule does NOT say: there is no qualifier about
-- the cost. The "cost that can't be paid while the object is on the battlefield"
-- clause people remember is CR 113.6j, which is about where an ability FUNCTIONS.
--
-- The revealer is the activating player. CR 602.2a is worded passively and names
-- nobody, but the hidden zone being read is theirs -- CR 400.3 puts every card in
-- a hand in its owner's, and Activatable.activatorOf gives a card in a hand to that
-- owner precisely because CR 108.4 leaves it with no controller.
--
-- Reaches exactly the hand today: abilitiesFor serves the battlefield, the hand,
-- the graveyard and the command zone and answers [] elsewhere -- and of those
-- four only a hand is hidden (CR 400.2 names the graveyard and the command zone
-- among the public zones), so the library offers
-- nothing to activate, and mana abilities never reach this function (CR 605.3b
-- keeps them off the stack).
--
-- This reveal is also what CR 702.49a's "Reveal this card from your hand" comes
-- to. Rule 602.2a fires on the same activation and rule 701.20a gives the two one
-- duration, so Pawl.Engine.Keyword.ninjutsu mints no reveal component of its own
-- and nothing in Pawl.Types.CostComponent reveals anything.
--
-- Not implemented: CR 701.20a's duration, revealed from announcement until the
-- ability leaves the stack. Nothing stores it, so a player deciding a response
-- cannot see the revealed card (#1408). Vacuous for cycling and reinforce, whose
-- minted costs discard the card (CR 702.29a, CR 702.77a) as Faerie Macabre's
-- authored one does (CR 113.6j), leaving it in a public graveyard a moment later.
-- NOT vacuous for ninjutsu, whose card stays in its owner's hand for the whole
-- window; Ninja of the Deep Hours is the first card in `data/cards/` this bites.
revealIfHidden :: PlayerId -> ObjectId -> Game ()
revealIfHidden pid srcId = do
  gs <- State.get
  case fmap Object.zone (Game.lookupObject srcId gs) of
    Just zone | Game.isHiddenZone zone -> Event.reveal RevealCause.Ordinary pid srcId
    _ -> pure ()

-- CR 602.2: announce the activation, revealing the card if it is coming from a
-- hidden zone (602.2a), put the ability on the stack (a fresh OfAbility object),
-- then walk CR 601.2b-i as CR 602.2b sends it -- choose modes, announce the value
-- of X, announce the hybrid and Phyrexian symbols (CR 118.13a), stamp targets,
-- pay -- and keep priority (117.3c). Reject-not-repair on an illegal mode or
-- target answer.
--
-- An announced X can lose the activation all by itself: enumeration measures the
-- cost at the least X the ability permits, and the value the player names is theirs to
-- name freely. See the gate below.
--
-- `before` is the pre-announcement state and is the ONLY thing the rejection
-- paths restore to, which is what puts CR 602.2a's reveal inside the rollback:
-- an activation the engine refused revealed nothing, and must leave no reveal
-- in the log claiming otherwise. Everything else reads `gs`, the state as of the
-- announcement.
activateAbility :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> Game ()
activateAbility pid srcId ability = do
  before <- State.get
  -- CR 602.2a's own order: the reveal is part of announcing, and so precedes
  -- the ability becoming an object on the stack (the rest of that same rule).
  revealIfHidden pid srcId
  gs <- State.get
  let (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source =
              Source.OfAbility
                ActivatedAbilitySource.MkActivatedAbilitySource
                  { ActivatedAbilitySource.source = srcId,
                    ActivatedAbilitySource.ability = ability
                  },
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.flipped = False,
            Object.exiledFaceDown = False,
            Object.exileLookers = Set.empty,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.controlClock = Map.empty,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.foretellCostReduction = Nothing,
            Object.warped = Nothing,
            Object.preparedCopyOf = Nothing,
            Object.ringBearerFor = Nothing,
            Object.duplicate = Nothing,
            Object.paired = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.designationValues = Map.empty,
            Object.paidCosts = Map.empty,
            Object.tributePaid = False,
            Object.bestowed = False,
            Object.mutating = False,
            Object.prototyped = False,
            Object.boughtBack = False,
            Object.spliced = Seq.empty,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.castFrom = Nothing,
            Object.castUsing = Nothing,
            Object.castGrant = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty,
            Object.activatedOnce = Set.empty
          }
      onStack =
        gs2
          { GameState.objects = Map.insert abilId obj (GameState.objects gs2),
            GameState.stack = abilId : GameState.stack gs2
          }
      decider = Decide.deciderFor pid gs
      -- CR 602.2b/700.2a, mirroring Cast.castSpell's mode block: a selection with
      -- one answer is FORCED, unprompted -- every single-mode ability is exactly
      -- {ModeIndex 0}. A real choice issues ChooseModes. Modal.forcedSelection is
      -- what tells the two apart, CR 700.2d's exception included.
      legal = Target.fillableModes (Just pid) Map.empty srcId Map.empty (ActivatedAbility.modal ability) gs
      selection = Modal.Type.selection (ActivatedAbility.modal ability)
      -- CR 601.2f's reductions can name the KIND of ability (Fluctuator's
      -- "cycling abilities"), so every totalling below is asked with this
      -- ability's provenance -- the two gates and the payment alike, or a
      -- reduction the gate withheld could still be applied when the cost is
      -- paid. Read off the ability's own stamp, so the answer does not depend on
      -- the zone its source is in when this is asked.
      stamp = ActivatedAbility.keyword ability
  State.put onStack
  -- Sorted on the way in, for the reason Cast.castProposed gives: printed order
  -- (CR 608.2c), with a repeated mode's instances adjacent (CR 700.2d).
  chosenModes <- case Modal.forcedSelection legal selection of
    Just forced -> pure forced
    Nothing -> fmap Seq.sort (Game.choose (Prompt.ChooseModes decider pid abilId legal selection))
  -- Reject-not-repair: an answer that does not satisfy the printed instruction
  -- makes the whole activation a no-op, guarding every step below.
  if not (Modal.selectionSatisfiedBy legal selection chosenModes)
    then State.put before
    else do
      -- CR 602.2b routes the rest of the activation through CR 601.2b-i, so CR
      -- 601.2b's announcement of a variable cost (CR 107.3) governs an activation
      -- cost's {X} too, and it is asked HERE -- after the modes, before CR
      -- 118.13a's announcement and before CR 601.2c's targets, which is 601.2b's
      -- own order.
      --
      -- Cinder Elemental exercises it. Not asking was not a missing question but
      -- a free {X}: a ManaSymbol.Variable that survives to payment demands
      -- nothing (Mana.waysOf), so the engine was announcing 0 on the player's
      -- behalf (#544).
      --
      -- The bound rides the PRINTED cost, which is what `activatable` gated on:
      -- both go through payableCostAt, so CR 601.2f's totalling is applied by the
      -- predicate rather than baked into the cost handed to it. Nothing filters
      -- the answer against the bound (see Prompt.ChooseX).
      let printedCost = ActivatedAbility.cost ability
          -- CR 601.2c's slots and their legal recipients, taken HERE rather than
          -- below where they are answered: the gate one step down has to measure
          -- a cost the targets can still change, so it is handed what they could
          -- be (aimingSomewhere). Read off `gs`, the same pre-stack board
          -- Target.chooseTargets is offered from, so one binding serves both.
          slots = Modal.modesTargetSlots chosenModes (ActivatedAbility.modal ability)
          -- The PRE-X map, and it says so: `unannounced` is True, so a slot's CR
          -- 202.3 computed bound reading the X states no bound rather than an
          -- unmeetable one, and the lookahead measures the cost against every
          -- recipient the announcement could still reach. candidateSlotsGiven's
          -- call is the same one, for the reason its own note gives.
          --
          -- NO GAMEPLAY OBSERVER today, and the flag is kept anyway because CR
          -- 601.2b has not happened yet where it is read: this map is consulted
          -- only by a cost that READS A BOUND SLOT (aimingSomewhere's slotReading),
          -- and no ability in `data/cards/` has both such a cost and a slot with a
          -- computed bound -- the whole suite stays green with this True flipped to
          -- False.
          --
          -- The map CR 601.2c is ANSWERED from is the second one below, taken once
          -- the X exists.
          unannouncedSets = Target.legalSets (Just pid) True Map.empty srcId slots gs
          aimableUnannounced = [fmap Activatable.recipientObjects unannouncedSets]
          -- CR 101.1: the ceiling this ABILITY's own words put on the value about
          -- to be announced -- Blighted Nightmare's "X can't be greater than the
          -- greatest toughness among creatures you control". Read HERE and once,
          -- the timing CR 601.2b fixes through 602.2b, so a creature that leaves in
          -- response cannot shrink it.
          --
          -- Off the SOURCE permanent (`srcId`) rather than the ability object: CR
          -- 113.7 makes that permanent the ability's source, and it is the object
          -- every other quantity on this road is evaluated against. It is still in
          -- its zone here, the activation cost being unpaid until below.
          mCeiling = Cost.ceilingOf pid srcId (ActivatedAbility.maximumX ability) gs
          -- CR 101.1: the floor the same words put on it -- Katara, Water Tribe's
          -- Hope's "X can't be 0". Printed, so there is nothing to evaluate.
          floorX = ActivatedAbility.minimumX ability
      mAmount <-
        if Cost.hasVariable printedCost
          then fmap Just (Game.choose (Prompt.ChooseX decider pid abilId floorX (Activatable.affordableX mCeiling aimableUnannounced stamp pid srcId gs printedCost)))
          else pure Nothing
      let announcedAtX = maybe printedCost (\x -> Cost.substituteX x printedCost) mAmount
          -- CR 101.1, and CR 101.2 for its direction, exactly as Cast.castProposed
          -- reads a face's: the ability's printed sentence overrides the rule that
          -- would otherwise leave X free, and a "can't" beats the permission.
          -- REJECT rather than clamp, which is this whole step's posture.
          overCeiling = case (mCeiling, mAmount) of
            (Just c, Just x) -> x > c
            _ -> False
          -- The floor's side of the same sentence, rejected the same way.
          underFloor = maybe False (< floorX) mAmount
          -- CR 601.2c's slots and their legal recipients as the announcement
          -- ACTUALLY made can reach them, seeded with CR 601.2b's X: a slot's own
          -- CR 202.3 computed bound reads it (Blighted Nightmare's "creature card
          -- with mana value X or less"), and `unannounced` is False because the
          -- value exists by now. Cast.castProposed's shape exactly, and
          -- Binding.fromChoices is the same writer CR 601.2i stamps the ability
          -- object with below, so the offer and the record cannot spell one X two
          -- ways.
          --
          -- The SAME seed reaches Target.selectionLegal's joint check below, so a
          -- slot offered against the announced X is not re-judged against no X.
          --
          -- `unannounced` is False because the announcement is MADE; it changes
          -- nothing while the seed carries the value, which is what makes flipping
          -- it here leave the suite green (Filter.boundUnannounced answers only
          -- where the seed cannot supply the number).
          seed = Binding.fromChoices Map.empty mAmount Seq.empty
          sets = Target.legalSets (Just pid) False seed srcId slots gs
          aimable = [fmap Activatable.recipientObjects sets]
      -- CR 602.2: an activation a player cannot comply with is illegal, and the
      -- game returns to the moment before it started. The X just named is where
      -- that can first become true: `activatable` measured the cost at the
      -- least X the ability permits, the only value it can know before an
      -- announcement exists. Both gates look ahead to CR 601.2c's candidate targets the same
      -- way (aimingSomewhere); what the player actually aims at is charged
      -- below, and a choice that reduces nothing loses the ability at the
      -- payment rather than here.
      --
      -- Measured against the POST-X map, which the earlier gates could not be:
      -- once the value is named the candidate set is exact, so a cost reading a
      -- bound slot is priced against what CR 601.2c will actually offer rather
      -- than against the wider pre-announcement lookahead.
      --
      -- Asked with the same predicate that floor was asked with, so a gate and an
      -- announcement cannot disagree about what a cost is. That matters beyond
      -- tidiness: CR 118.13a's announcement below runs on this cost, and an X
      -- large enough to leave neither of CR 107.4f's routes payable would leave
      -- Mana.announce with no offer to make. This gate is what keeps that arm out
      -- of reach from here (#417).
      --
      -- Reject-not-repair, the posture every other step here takes: the
      -- announcement is NOT clamped to affordableX -- CR 601.2b lets the player
      -- announce the value freely -- it is honoured and then loses the ability.
      -- Asked unconditionally rather than only when there is an {X}, which buys
      -- one predicate over one cost instead of two spellings of when the gate
      -- applies.
      if overCeiling || underFloor || not (Activatable.payableCost aimable stamp pid srcId gs announcedAtX)
        then State.put before -- reject: the whole activation is a no-op
        else do
          -- CR 118.13a's announcement, which names an activated ability's
          -- activation cost, happens here at 601.2b's position and not when the
          -- cost is paid. Moltensteel Dragon exercises it; rule 118.13b's cost
          -- paid during a resolution announces at its own site
          -- (Pawl.Engine.Resolve.payGatePaidBy), and rule 118.13c's special
          -- action at each of its own (Pawl.Engine.FaceDown.turnFaceUp and
          -- its five siblings).
          --
          -- Measured through the SAME totalling payableCost gated on, off the
          -- same adjustments -- against the printed cost instead, a reduction
          -- could hide a route and Mana.announce would elide the prompt and pay
          -- life on the player's behalf (#416, for the spell that named it).
          --
          -- Run on the cost carrying the ANNOUNCED value, which is CR 601.2b's own
          -- order (the value of X precedes the hybrid and Phyrexian
          -- announcements).
          --
          -- CR 601.2f's additional components are on the cost by this point
          -- (Cost.plusComponents), which is what payableCost measured and what
          -- Cost.pay will charge. It matters to the announcement itself: the
          -- offers are filtered against the claims a component makes on a zone,
          -- so a Phyrexian symbol offered without the added "Sacrifice a land"
          -- in view would be offered against a board that has one land too many.
          -- TARGET-BLIND, unlike the gate above, and deliberately: that gate
          -- asks whether SOME aiming pays and may look ahead, while this is a
          -- CHOICE the player makes against one definite cost, and CR 601.2c's
          -- targets are not announced until below. What that costs is nothing
          -- here -- CR 118.13a's announcement is a choice of halves, every
          -- activation-cost reducer in the pool reduces by GENERIC mana, and a
          -- target-aware reduction therefore cannot change which nonhybrid
          -- equivalent or Phyrexian half a player would announce. The reductions
          -- themselves are gathered again below, once the targets exist.
          let gathered = Cost.activationAdjustments Set.empty stamp AbilityKind.NonManaAbility (Cost.loyaltyKindOf (ActivatedAbility.cost ability)) pid srcId gs
          -- The Phyrexian life record is DISCARDED here: CR 702.150a reads what
          -- the player who CAST a spell announced, and no rule asks the same of
          -- an activation cost.
          -- CR 701.67a's taps counted into the totalling the announcement
          -- measures through, Cast.castSpellWith's posture and for its reason
          -- (Cost.substitutedManas): a half this offer makes payable is a half
          -- CR 601.2b leaves to the payer rather than to the fallback.
          let totalledCost = Cost.plusComponents gathered announcedAtX
          (announcedCost, _) <- Cost.announce (PaymentSubject.Activating srcId) ManaSpending.AsProduced pid srcId (Cost.substitutedManas (Cost.activationManaSubstitutions (Cost.Type.components totalledCost) Map.empty pid srcId gs) (Cost.totalManas gathered)) totalledCost
          chosen <- Target.chooseTargets pid abilId srcId (Maybe.fromMaybe 0 mAmount) slots sets
          if not (Target.selectionLegal (Just pid) seed srcId (Maybe.fromMaybe 0 mAmount) slots sets chosen gs)
            then State.put before -- reject: the whole activation is a no-op
            else do
              -- CR 113.7: bind the source permanent under the reserved self slot, so
              -- an activated ability that refers to "this creature" (Longtusk Cub)
              -- resolves the reference as a slot read -- exactly as
              -- Engine.placeBorne does for a TRIGGERED ability's source.
              --
              -- CR 109.5 binds the controller under the reserved you slot in the
              -- same breath: "The words 'you' and 'your' on an object refer to the
              -- object's controller ... For an activated ability, this is the
              -- player who activated the ability." That player is `pid`, the one
              -- CR 602.2 lets activate this ability at all -- so Brothers of Fire's
              -- "and 1 damage to you" reaches a player, exactly as
              -- Engine.placeBorne does for a TRIGGERED ability's controller.
              --
              -- CR 601.2b's announced X is stamped alongside, onto the ABILITY object
              -- and not the source permanent -- Cinder Elemental sacrifices that
              -- permanent to pay, so the ability is the only holder still there to
              -- read at resolution (Quantity.evaluateFor). CR 113.7a is why the
              -- ability keeps all three once its source is gone: "Once activated or
              -- triggered, an ability exists on the stack independently of its
              -- source."
              State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setThisAbility abilId (Binding.setYou pid (Binding.setTriggerSource srcId (Binding.fromChoices chosen mAmount chosenModes)))}) abilId (GameState.objects g)})
              -- CR 601.2c's board, before CR 601.2g/h's window and payment can
              -- change it: what Event.becameTarget samples below.
              announced <- State.get
              -- CR 601.2f, at the position CR 602.2b gives it and in Cast.castSpell's
              -- own order: the reductions that apply to this activation are announced
              -- (CR 118.7e's choice of half) and then applied to the announced cost.
              -- The record announced is the record applied, so the cost paid is the
              -- cost the gates measured -- one reduction cannot be gathered twice
              -- from two states.
              --
              -- CR 118.7e asks nothing today: every activation-cost reducer in the
              -- pool reduces by generic mana (Heartstone's {1}), which has no halves
              -- to choose between. The seam is here rather than skipped so that the
              -- one that does cannot arrive at a path that never asks.
              --
              -- CR 601.2f's ORDER is asked at the same seam and does reach a board:
              -- Heartstone's floor beside Blossoming Tortoise's absence of one on an
              -- animated Mishra's Foundry is two orders at two prices. The ANNOUNCED
              -- cost goes in because the order is chosen against the cost it will be
              -- applied to.
              -- CR 601.2c's announced targets, gathered a SECOND time now that
              -- they exist: Dwarven Mauler's "equip abilities you activate that
              -- target this creature" is a reduction the pre-target gather above
              -- cannot see, and CR 601.2f's position after 601.2c is what makes
              -- reading them here the rule's own order rather than a shortcut.
              -- Only the REDUCTIONS can differ between the two records --
              -- ReduceActivationCost is the one arm carrying a target criterion
              -- -- so the increases and the CR 601.2f components the announcement
              -- above measured are the same ones charged below.
              let aimedAt = Set.unions (fmap Activatable.recipientObjects (Map.elems chosen))
                  targeted = Cost.activationAdjustments aimedAt stamp AbilityKind.NonManaAbility (Cost.loyaltyKindOf (ActivatedAbility.cost ability)) pid srcId gs
              adjustments <- Cost.announceReductions pid srcId gs announcedCost targeted
              let paidCost = Cost.totalWith adjustments announcedCost
              -- CR 601.2g/h via Pawl.Engine.Cost.pay: the mana window, then the
              -- components. The gates above prove SOME sequence of choices pays for
              -- this ability -- but Unpaid is reachable all the same, because the
              -- mana window then asks the player to make those choices and a
              -- mis-tapped colour is a choice the engine must honour (Cost.payMana).
              -- Reject-not-repair restores the whole activation, including the
              -- ability object this function put on the stack.
              -- CR 701.67a's offer is handed to the PAYMENT rather than made
              -- here, Cast.castSpellWith's posture and for its reason: CR
              -- 601.2g's mana window opens first, and the payer says how much of
              -- the waterbend cost they tap for once it closes
              -- (Cost.paySubstituting). The bindings the substitution makes are
              -- dropped -- no printing reads back which permanents a waterbend
              -- cost tapped, where CR 702.51c's convoke does.
              (payment, _) <- Cost.paySubstituting Resolve.performManaAbility Nothing PaymentMoment.OutsideResolution (PaymentSubject.Activating srcId) (Just abilId) ManaSpending.AsProduced pid srcId (Cost.announceSubstitutions Cost.activationManaSubstitutions pid srcId) paidCost
              case payment of
                -- CR 606.3: record that a loyalty ability of THIS PERMANENT was
                -- activated, which is the whole of the once-per-turn limit's storage
                -- (see loyaltyOk above). Every path that rejects the activation
                -- restores `before`, and the log lives in that state, so no rejected
                -- activation can leave a record behind.
                Payment.Paid bound -> do
                  -- CR 608.2h: the slots the PAYMENT bound -- the permanent a
                  -- Sacrifice component put in a graveyard -- folded onto the
                  -- ability object, so "the sacrificed creature's power" has a
                  -- name to read at resolution (Jarad, Golgari Lich Lord). After
                  -- the payment because that is when the payment knows them, and
                  -- onto the ABILITY for the reason CR 113.7a gives above: the
                  -- source may itself be what was sacrificed.
                  State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setPaid bound (Object.bindings o)}) abilId (GameState.objects g)})
                  Monad.when
                    (Cost.isLoyaltyCost (ActivatedAbility.cost ability))
                    (State.modify' (Event.recordEvent (GameEvent.LoyaltyAbilityActivated srcId)))
                  -- CR 602.5b: record that THIS ability of this permanent has now
                  -- been activated, which is the whole of both counted riders'
                  -- storage -- see ActivationRestriction.recordActivation, the
                  -- one writer this and CR 605.3b's mana road share. Beside the
                  -- loyalty record above and for its reason: every rejecting path
                  -- restores `before`, so no refused activation leaves one behind.
                  State.modify' (ActivationRestriction.recordActivation srcId ability)
                  -- CR 702.122b: the creatures this crew cost tapped crewed the
                  -- Vehicle as they were tapped, so the relation is recorded at
                  -- the payment and a crew ability that never resolves still
                  -- leaves it (CrewSpec's "a countered crew ability"). A case on
                  -- the rule-702 keyword stamp, never on an effect.
                  Monad.when
                    ((Keyword.familyOf =<< stamp) == Just KeywordFamily.Crew)
                    ( State.modify'
                        ( Event.recordEvent
                            ( GameEvent.Crewed
                                Crewing.MkCrewing
                                  { Crewing.vehicle = srcId,
                                    Crewing.crewedBy = Set.fromList (Maybe.mapMaybe Recipient.objectOf (foldMap Set.toList (Map.lookup Binding.tappedForTotalPower bound)))
                                  }
                            )
                        )
                    )
                  -- CR 601.2c through CR 602.2b: each chosen object became a
                  -- target of this ability, which is what CR 702.21a's ward
                  -- watches -- and an activated ability is the half
                  -- GameEvent.SpellCast could never carry.
                  --
                  -- The ABILITY object and not its source permanent: rule 702.21a
                  -- counters "that spell or ability", and CR 113.7a makes the
                  -- ability the thing on the stack. Its controller is `pid` (CR
                  -- 113.8), the player rule 702.21a offers the cost to.
                  --
                  -- After the payment for Cast.castSpell's reason: everything
                  -- above can still restore `before` and unwind the activation.
                  Event.becameTarget announced abilId StackObjectKind.ActivatedAbility pid chosen
                -- CR 733.1's last sentence, Cost.keepingLibraryActions' reason:
                -- a mana ability tapped in the window this payment opened may
                -- have shuffled or revealed, and this reject-not-repair
                -- restore must not undo that too.
                Payment.Unpaid -> Cost.restoreKeepingLibraryActions before
