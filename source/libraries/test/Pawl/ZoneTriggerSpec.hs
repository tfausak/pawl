{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Trigger over zone changes: the graveyard, exile and command
-- zones, entering and leaving the battlefield, and CR 603.10a's look-back at
-- the game state the leaving object left behind. The machinery is
-- Pawl.TriggerSpec.
module Pawl.ZoneTriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Event.Binding as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BecameAttached as BecameAttached
import qualified Pawl.Types.BecameAttacked as BecameAttacked
import qualified Pawl.Types.BecameBlocking as BecameBlocking
import qualified Pawl.Types.BecameDesignated as BecameDesignated
import qualified Pawl.Types.BecameTarget as BecameTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardLeavesGraveyard as CardLeavesGraveyard
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.ClassLevelChange as ClassLevelChange
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.CoinFlipped as CoinFlipped
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
-- Aliased Condition.Type, not Condition, per the project-wide convention
-- (CardSpec's note): the evaluator module Pawl.Engine.Condition may later be imported
-- and must not collide.
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ControlChanged as ControlChanged
import qualified Pawl.Types.ControllerBecomesTarget as ControllerBecomesTarget
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPlacement as CounterPlacement
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.CreatureBecomesBlockedByAtLeast as CreatureBecomesBlockedByAtLeast
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Discarded as Discarded
import qualified Pawl.Types.Drew as Drew
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.FloatingCandidate as FloatingCandidate
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.HalfUnlocked as HalfUnlocked
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Mentored as Mentored
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.PermanentWasSacrificed as PermanentWasSacrificed
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerAttacksPlayer as PlayerAttacksPlayer
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.StackObjectKind as StackObjectKind
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Transformed as Transformed
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.VentureMarkerEntered as VentureMarkerEntered
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Pawl.Engine.Event.gatherTriggers runs in Game since CR 603.7b's second
-- sentence became a question for the controller of a delayed entry that matched
-- two SIMULTANEOUS occurrences. No board in this module arms a delayed entry at
-- all, so nothing here is ever asked; Pawl.EventTriggerSpec's Synthetic Singular
-- Cure is where the question is.
gathered :: GameState.GameState -> [PendingTrigger.PendingTrigger]
gathered gs = fst (fst (S.runPureWith S.identityAnswer gs (Event.gatherTriggers (Event.unscannedGrouped gs) gs)))

-- CR 702.29c: "'When you cycle this card' means 'When you discard this card to
-- pay an activation cost of a cycling ability.' These abilities trigger from
-- whatever zone the card winds up in after it's cycled."
--
-- Windcaller Aven is the card: a {4}{U}{U} 4/3 with flying, Cycling {U}, and
-- "When you cycle this card, target creature gains flying until end of turn".
-- The trigger is mandatory and its effect is Serpent's Gift's exact shape, so
-- the only new thing any test below can be passing on is the trigger itself.
cyclingTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cyclingTriggerSpec s registry =
  Spec.describe s "CyclingTrigger" $ do
    -- The whole card: cycle the Aven for {U}, its trigger targets the Piker as
    -- it is placed (CR 603.3d), and the Piker is flying once it resolves.
    Spec.it s "CR 702.29c whole card: cycling Windcaller Aven grants flying" $ do
      aven <- S.printingOf s registry "Windcaller Aven"
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      let (creature, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
          (g1, avenId) = S.handOne aven g0
          gs = g1 {GameState.priority = Just S.alice}
      Spec.assertBool s (not (Projection.hasKeyword Keyword.Type.Flying creature gs)) "the Piker does not start with flying"
      case Activate.abilitiesFor avenId gs of
        [ability] -> do
          let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice avenId ability)
              -- The settle PLACES the trigger and stamps its target (CR
              -- 603.3d); resolving it is the next thing to happen, and it is on
              -- top of the draw it was triggered alongside.
              placed = S.runPure S.identityAnswer cycled Engine.settleForPriority
              after = S.runPure S.identityAnswer placed Stack.resolveTop
          Spec.assertEqWith s "the Aven is in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice cycled)) 1
          Spec.assertBool s (length (GameState.stack placed) == 2) "the trigger is on the stack, above the draw"
          Spec.assertBool s (Projection.hasKeyword Keyword.Type.Flying creature after) "and the Piker has flying once it resolves"
        abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
    -- "These abilities trigger from whatever zone the card winds up in": the
    -- trigger's source is the graveyard incarnation, which CR 400.7 makes a
    -- DIFFERENT object from the card that was in hand. The scan finds it in
    -- neither of the two places it looked before this rule -- the battlefield,
    -- and a permanent that just left it.
    Spec.it s "CR 702.29c the trigger fires from the graveyard, off a new incarnation" $ do
      aven <- S.printingOf s registry "Windcaller Aven"
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
          (g1, avenId) = S.handOne aven g0
          gs = g1 {GameState.priority = Just S.alice}
      case Activate.abilitiesFor avenId gs of
        [ability] -> do
          let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice avenId ability)
              placed = S.runPure S.identityAnswer cycled Engine.placePendingTriggers
          Spec.assertEqWith s "the id that was in hand is gone" (Game.lookupObject avenId placed) Nothing
          Spec.assertEqWith s "the draw and the trigger are both on the stack" (length (GameState.stack placed)) 2
        abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
    -- The discriminating twin, and the reason CR 702.29c needs an event of its
    -- own rather than matching the zone change the discard already records: an
    -- ORDINARY discard of the same card, through the same CR 400.7 funnel, is
    -- not cycling and fires nothing.
    Spec.it s "CR 702.29c discarding the Aven without cycling fires nothing" $ do
      aven <- S.printingOf s registry "Windcaller Aven"
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      let (creature, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
          (g1, _) = S.handOne aven g0
          gs = g1 {GameState.priority = Just S.alice}
          -- The same card, the same graveyard, one component over: a cost that
          -- discards a card of the player's choice rather than this one.
          discarded = S.runPure S.identityAnswer gs (Cost.payComponent PaymentMoment.OutsideResolution Map.empty S.alice S.noSource (CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))))
          after = S.runPure S.identityAnswer discarded Engine.settleForPriority
      Spec.assertEqWith s "the Aven really did reach the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice discarded)) 1
      Spec.assertEqWith s "nothing was put on the stack" (GameState.stack after) []
      Spec.assertBool s (not (Projection.hasKeyword Keyword.Type.Flying creature after)) "and the Piker never gained flying"
    -- The other control: cycling a card that has no cycling trigger fires
    -- nothing, so the trigger is the Aven's and not the act of cycling.
    Spec.it s "CR 702.29c cycling a card with no such trigger fires nothing" $ do
      mauler <- S.printingOf s registry "Barkhide Mauler"
      forest <- S.printingOf s registry "Forest"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, g0) = S.addCreature piker S.alice (S.landsInPlay forest 2)
          (g1, maulerId) = S.handOne mauler g0
          gs = g1 {GameState.priority = Just S.alice}
      case Activate.abilitiesFor maulerId gs of
        [ability] -> do
          let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice maulerId ability)
              placed = S.runPure S.identityAnswer cycled Engine.placePendingTriggers
          Spec.assertEqWith s "only the draw is on the stack" (length (GameState.stack placed)) 1
        abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))

-- CR 113.6k: "A trigger condition that can't trigger from the battlefield
-- functions in all zones it can trigger from." Narcomoeba's "When this card is
-- put into your graveyard from your library" is such a condition -- the bearer
-- is in a graveyard when it fires, never on the battlefield -- so the scan has
-- to look somewhere other than the battlefield to find it.
--
-- Two proving pairs, both with Narcomoeba as the bearer. Tome Scour ("target
-- player mills five cards") leaves it in the graveyard, so the live boundary scan
-- finds it; Soul Warden rides along in the same graveyard as the control, because
-- its CR 603.6a trigger functions ONLY on the battlefield and so must stay silent
-- even when a creature enters right in front of it.
--
-- Corpse Churn mills it and takes it back out in ONE resolution, so the boundary
-- scan cannot see it at all and CR 603.10's first sentence has to be answered
-- from CR 608.2h last known information -- Event.eventTriggers'
-- `arrivedInGraveyard`.
--
-- The last case turns that source around and proves its FILTER: Come Back Wrong
-- buries a Meren of Clan Nel Toth and returns her in the same resolution, and CR
-- 113.6k is what keeps her battlefield-only dies trigger from seeing the very
-- death that buried her.
graveyardTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graveyardTriggerSpec s registry =
  let -- alice: one Island in play (Tome Scour's {U}), Tome Scour in hand, and a
      -- three-card library of Narcomoeba, Soul Warden and a Goblin Piker. Five
      -- mills a three-card library empty (CR 701.17b), so every one of them
      -- lands in the graveyard in one event batch and the scan has to pick the
      -- one card whose ability functions there.
      milledBoard = do
        island <- S.printingOf s registry "Island"
        tomeScour <- S.printingOf s registry "Tome Scour"
        narcomoeba <- S.printingOf s registry "Narcomoeba"
        soulWarden <- S.printingOf s registry "Soul Warden"
        piker <- S.printingOf s registry "Goblin Piker"
        let base = S.landsInPlay island 1
            (_, g1) = S.addLibraryCard narcomoeba S.alice base
            (_, g2) = S.addLibraryCard soulWarden S.alice g1
            (_, g3) = S.addLibraryCard piker S.alice g2
            (g4, spellId) = S.handOne tomeScour g3
        pure (g4 {GameState.priority = Just S.alice}, spellId)
      -- Takes every "may". There is exactly one in this scenario -- Narcomoeba's
      -- -- so this is not a blanket yes standing in for a specific answer.
      takeOptional :: Prompt.Prompt r -> r
      takeOptional p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- Cast Tome Scour at alice herself (S.identityAnswer's ChooseTargets picks
      -- the least id in each set, and alice is player 0), resolve it, settle so
      -- any trigger reaches the stack, then resolve that trigger.
      millSelf :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      millSelf answer (gs, spellId) =
        let cast = S.runPure answer gs (S.cast S.alice spellId)
            milled = S.runPure answer cast Stack.resolveTop
            placed = S.runPure answer milled Engine.settleForPriority
         in (placed, S.runPure answer placed Stack.resolveTop)
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      narcomoebaName = CardName.MkCardName $ Text.pack "Narcomoeba"
      merenName = CardName.MkCardName $ Text.pack "Meren of Clan Nel Toth"
      experienceOf = S.playerCounterOf PlayerCounterKind.Experience
      -- Corpse Churn {1}{B} Instant, "Mill three cards, then you may return a
      -- creature card from your graveyard to your hand." (name, cost, type line
      -- and oracle text checked against Scryfall.)
      --
      -- alice: two Swamps in play for the cost, Corpse Churn in hand, and a
      -- ONE-CARD library holding Narcomoeba. CR 701.17b's "as many as possible"
      -- mills exactly that card, so the milled count (1) differs from the printed
      -- three and a reading that milled three would leave a different board.
      corpseChurnBoard = do
        swamp <- S.printingOf s registry "Swamp"
        churn <- S.printingOf s registry "Corpse Churn"
        narcomoeba <- S.printingOf s registry "Narcomoeba"
        let base = S.landsInPlay swamp 2
            (_, g1) = S.addLibraryCard narcomoeba S.alice base
            (g2, spellId) = S.handOne churn g1
        pure (g2 {GameState.priority = Just S.alice}, spellId)
      -- Exercises Corpse Churn's OPTIONAL clause, pinned by clause index rather
      -- than answered blanket-yes: clause 1 is the "you may return", clause 0 the
      -- mandatory mill, and Narcomoeba's own printed "may" is a ChooseOptional too
      -- -- a blanket yes would conflate the two.
      returnsIt :: Prompt.Prompt r -> r
      returnsIt p = case p of
        Prompt.ChooseOptional _ _ _ _ clause
          | clause == ClauseIndex.MkClauseIndex 1 -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
   in Spec.describe s "GraveyardTrigger" $ do
        -- The gameplay-level proof, cast to resolution.
        Spec.it s "CR 113.6k whole card: Tome Scour mills Narcomoeba and its trigger puts it onto the battlefield" $ do
          board <- milledBoard
          let (placed, after) = millSelf takeOptional board
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Battlefield S.alice after)) "Narcomoeba is on the battlefield"
          Spec.assertBool s (not (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice after))) "and no longer in the graveyard"
          -- The control, in the same graveyard: Soul Warden's "whenever
          -- another creature enters" functions only on the battlefield (CR
          -- 113.6's default), and a creature entered right in front of it.
          Spec.assertEqWith s "the Soul Warden in the graveyard gained nothing" (S.lifeOf S.alice after) (Just 20)
        -- CR 603.5: the "may" is a real choice, and declining is the other
        -- half of it. The trigger still went on the stack and still resolved.
        Spec.it s "CR 603.5 declining the may leaves Narcomoeba in the graveyard" $ do
          board <- milledBoard
          let (placed, after) = millSelf S.identityAnswer board
          Spec.assertEqWith s "the trigger reached the stack anyway" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice after)) "Narcomoeba is still in the graveyard"
          Spec.assertBool s (not (Set.member narcomoebaName (namesIn Zone.Battlefield S.alice after))) "and not on the battlefield"
          Spec.assertEqWith s "and the ability left the stack -- a declined may is not a fizzle" (length (GameState.stack after)) 0
        -- "from your library" doing real work, half one: the same card moved
        -- out of a HAND reaches the same graveyard and must not trigger.
        Spec.it s "CR 113.6k Narcomoeba put into the graveyard from the HAND does not trigger" $ do
          narcomoeba <- S.printingOf s registry "Narcomoeba"
          let (handCard, gs) = S.addHandCard narcomoeba S.alice (Setup.emptyGame S.bothPlayers)
              buried = S.runPure S.identityAnswer gs (Event.changeZone handCard Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (gathered buried)) []
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice buried)) "it is in the graveyard"
        -- "from your library" doing real work, half two: dying is a move to
        -- the same graveyard from the battlefield, and is not this trigger.
        Spec.it s "CR 113.6k Narcomoeba dying from the BATTLEFIELD does not trigger" $ do
          narcomoeba <- S.printingOf s registry "Narcomoeba"
          let (creature, gs) = S.addCreature narcomoeba S.alice (Setup.emptyGame S.bothPlayers)
              died = S.runPure S.identityAnswer gs (Event.changeZone creature Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (gathered died)) []
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice died)) "it is in the graveyard"
        -- The zone half, isolated from the mill: a graveyard card whose only
        -- trigger functions on the battlefield (CR 113.6's default) is not
        -- scanned into firing by an event it would have seen from play.
        Spec.it s "CR 113.6 a battlefield-only trigger in a graveyard is not scanned" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          piker <- S.printingOf s registry "Goblin Piker"
          let (wardenCard, gs0) = S.addLibraryCard soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              buried = S.runPure S.identityAnswer gs0 (Event.changeZone wardenCard Zone.Graveyard)
              (pikerCard, gs1) = S.addHandCard piker S.alice buried
              entered = S.runPure S.identityAnswer gs1 (Event.changeZone pikerCard Zone.Battlefield)
          Spec.assertBool s (Set.member (CardName.MkCardName $ Text.pack "Soul Warden") (namesIn Zone.Graveyard S.alice entered)) "the Warden is in the graveyard"
          Spec.assertEqWith s "and a creature entering fires nothing" (fmap PendingTrigger.source (gathered entered)) []
        -- CR 603.10's first sentence against a card that is NOT in the graveyard
        -- at the CR 117.5 boundary: Corpse Churn mills Narcomoeba and returns it
        -- to the hand in one resolution, with no boundary in between.
        --
        -- The discriminator is the graveyard's contents, asserted as an equality:
        -- at the boundary it holds Corpse Churn alone (CR 404.1's finished
        -- instant), which is an Instant with no triggered ability, so
        -- `inGraveyards` contributes NOTHING and the live reading of the board
        -- gives the opposite answer. The trigger can only have come from CR 608.2h
        -- last known information.
        Spec.it s "CR 603.10 Corpse Churn mills Narcomoeba and returns it in one resolution; the trigger still fires" $ do
          (gs, spellId) <- corpseChurnBoard
          let cast = S.runPure returnsIt gs (S.cast S.alice spellId)
              resolved = S.runPure returnsIt cast Stack.resolveTop
              -- The NARROW path: the scan itself, no priority loop and no settle,
              -- which cannot tell "never triggered" from "triggered and swept".
              scanned = gathered resolved
          Spec.assertEqWith s "the graveyard holds only Corpse Churn at the boundary" (namesIn Zone.Graveyard S.alice resolved) (Set.singleton (CardName.MkCardName $ Text.pack "Corpse Churn"))
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Hand S.alice resolved)) "Narcomoeba is in alice's hand instead"
          Spec.assertEqWith s "exactly one trigger, from the departed graveyard card" (length scanned) 1
          Spec.assertEqWith s "and it is Narcomoeba's condition" (fmap (TriggeredAbility.condition . PendingTrigger.ability) scanned) [TriggerCondition.SelfPutIntoGraveyardFromLibrary]
          -- Gameplay level, through the real boundary.
          let placed = S.runPure returnsIt resolved Engine.settleForPriority
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          -- CR 400.7 / CR 608.2h: the ability's source is the graveyard
          -- incarnation, which no longer exists, so "put this onto the
          -- battlefield" finds nothing and the card stays in the hand. The trigger
          -- resolves and does nothing -- it is not a fizzle.
          let after = S.runPure returnsIt placed Stack.resolveTop
          Spec.assertBool s (not (Set.member narcomoebaName (namesIn Zone.Battlefield S.alice after))) "and nothing came back onto the battlefield"
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Hand S.alice after)) "Narcomoeba is still in the hand"
          Spec.assertEqWith s "and the ability left the stack" (length (GameState.stack after)) 0
        -- The negative, and the pair for the case above: same spell, same mana,
        -- same answerer, same Narcomoeba-ends-in-hand outcome. The ONE difference
        -- is how Narcomoeba got into the graveyard -- here it was already there, so
        -- there is no arrival event in this batch to name it, and the mill takes a
        -- Swamp instead. "From your library" still has to do its work.
        Spec.it s "CR 603.10 a card already in the graveyard that LEAVES it is not a candidate" $ do
          swamp <- S.printingOf s registry "Swamp"
          churn <- S.printingOf s registry "Corpse Churn"
          narcomoeba <- S.printingOf s registry "Narcomoeba"
          let base = S.landsInPlay swamp 2
              (_, g1) = S.addGraveyardCard narcomoeba S.alice base
              (_, g2) = S.addLibraryCard swamp S.alice g1
              (g3, spellId) = S.handOne churn g2
              gs = g3 {GameState.priority = Just S.alice}
              cast = S.runPure returnsIt gs (S.cast S.alice spellId)
              resolved = S.runPure returnsIt cast Stack.resolveTop
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Hand S.alice resolved)) "Narcomoeba left the graveyard for the hand"
          Spec.assertEqWith s "and nothing triggered -- it never arrived from a library in this batch" (fmap PendingTrigger.source (gathered resolved)) []
        -- The FILTER on that source, which is CR 113.6k itself: a departed
        -- graveyard card is offered only the abilities that function in a
        -- graveyard. Come Back Wrong ("Destroy target creature. If a creature
        -- card is put into a graveyard this way, return it to the battlefield
        -- under your control.") aimed at Meren of Clan Nel Toth is the board that
        -- observes it -- one resolution in which a permanent DIES and then LEAVES
        -- its graveyard, with no CR 117.5 boundary between the two, so the
        -- graveyard incarnation is reachable by nothing but `arrivedInGraveyard`.
        --
        -- Meren's "whenever ANOTHER creature you control dies" functions only on
        -- the battlefield (CR 113.6's default), and the death it would see is the
        -- very move that buried her: CR 400.7 minted a fresh id for the graveyard
        -- incarnation, so the printed "another" compares two different ids and
        -- passes. Without the filter she takes an experience counter for her own
        -- death. permanentDiesSpec below is where that same exclusion is proved
        -- from the battlefield, where the two ids DO coincide.
        Spec.it s "CR 113.6k a battlefield-only trigger on a card that arrived in a graveyard and left it is not offered" $ do
          swamp <- S.printingOf s registry "Swamp"
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          comeBackWrong <- S.printingOf s registry "Come Back Wrong"
          let (merenId, board) = S.addCreature meren S.alice (S.landsInPlay swamp 3)
              (gs, spellId) = S.handOne comeBackWrong board
              -- Pinned by FILTERING the offered set rather than building a
              -- Recipient. Meren is the only creature, so this is the identity on
              -- a set of one, and it cannot smuggle in a recipient CR 608.2b's
              -- re-read would drop.
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature merenId) . snd) sets
                _ -> S.identityAnswer p
              cast = S.runPure answer gs (S.cast S.alice spellId)
              resolved = S.runPure answer cast Stack.resolveTop
              settled = S.runPure answer resolved Engine.settleForPriority
              after = S.runPure answer settled Stack.resolveTop
          Spec.assertEqWith s "CR 113.6k alice takes no experience counter: her Meren did not see her own death from the graveyard" (experienceOf S.alice after) 0
          Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack settled)) 0
          Spec.assertEqWith s "and the narrow scan of that batch offered nothing" (fmap PendingTrigger.source (gathered resolved)) []
          -- The board really is the one the case needs: she died, and she is not
          -- in the graveyard the CR 117.5 boundary would have scanned.
          Spec.assertEqWith s "CR 400.7 the permanent that died is gone" (Game.lookupObject merenId resolved) Nothing
          Spec.assertBool s (Set.member merenName (namesIn Zone.Battlefield S.alice resolved)) "a fresh Meren stands on the battlefield: she really did leave the graveyard"
          Spec.assertBool s (not (Set.member merenName (namesIn Zone.Graveyard S.alice resolved))) "with nothing of hers left in it"

-- Gaea's Blessing {1}{G} Sorcery, "Target player shuffles up to three target
-- cards from their graveyard into their library. Draw a card. When this card is
-- put into your graveyard from your library, shuffle your graveyard into your
-- library." (name, cost, type line and oracle text checked against Scryfall.)
--
-- Narcomoeba's CR 113.6k condition carrying rule 701.24's OTHER shape: a set
-- rather than named objects (Effect.ShuffleIntoLibrary over
-- ObjectRef.EachCardInGraveyard), which is CR 701.24d -- "if an effect would
-- cause a player to shuffle a set of objects into a library, that library is
-- shuffled even if there are no objects in that set". The set is empty when the
-- graveyard is emptied in response, and the library named by the effect is then
-- the only thing left saying which library to shuffle (#558).
gaeasBlessingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gaeasBlessingSpec s registry =
  let namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      -- CR 701.24a leaves a shuffle observable only through the order it
      -- produces, so the interpreter REVERSES it (Pawl.TargetSpec's Riftsweeper cases,
      -- for the same reason). What order a shuffle leaves is not asserted
      -- anywhere -- a real one has none.
      reversing :: Prompt.Prompt r -> r
      reversing p = case p of
        Prompt.Shuffle ids -> reverse ids
        _ -> S.identityAnswer p
   in Spec.describe s "GaeasBlessing" $ do
        -- The whole card through a real mill: Tome Scour empties alice's
        -- three-card library, so every card lands in her graveyard at once and
        -- the trigger scan has to find the ability on a SORCERY card there. Tome
        -- Scour itself joins them on the way out (CR 608.2n), which is why the
        -- library the trigger refills is four cards and not three.
        Spec.it s "CR 113.6k whole card: milled, Gaea's Blessing shuffles the whole graveyard back into the library" $ do
          island <- S.printingOf s registry "Island"
          tomeScour <- S.printingOf s registry "Tome Scour"
          blessing <- S.printingOf s registry "Gaea's Blessing"
          piker <- S.printingOf s registry "Goblin Piker"
          bolt <- S.printingOf s registry "Lightning Bolt"
          let base = S.landsInPlay island 1
              (_, g1) = S.addLibraryCard blessing S.alice base
              (_, g2) = S.addLibraryCard piker S.alice g1
              (_, g3) = S.addLibraryCard bolt S.alice g2
              (g4, spellId) = S.handOne tomeScour g3
              board = g4 {GameState.priority = Just S.alice}
              cast = S.runPure reversing board (S.cast S.alice spellId)
              milled = S.runPure reversing cast Stack.resolveTop
              placed = S.runPure reversing milled Engine.settleForPriority
              after = S.runPure reversing placed Stack.resolveTop
          Spec.assertEqWith s "the mill emptied the library" (length (Game.zoneMembers Zone.Library S.alice milled)) 0
          Spec.assertEqWith s "and its trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertEqWith s "which empties the graveyard" (Game.zoneMembers Zone.Graveyard S.alice after) []
          Spec.assertEqWith s "into a library of four -- the three milled cards and Tome Scour" (length (Game.zoneMembers Zone.Library S.alice after)) 4
          Spec.assertEqWith
            s
            "each of them by name"
            (namesIn Zone.Library S.alice after)
            (Set.fromList (fmap (CardName.MkCardName . Text.pack) ["Gaea's Blessing", "Goblin Piker", "Lightning Bolt", "Tome Scour"]))
        -- CR 701.24d, off a PAIR of boards differing in exactly one thing:
        -- whether the graveyard was emptied between the trigger going on the
        -- stack and its resolution. With the set empty there is no object left to
        -- read an owner off, so only the library the effect NAMES can be shuffled
        -- -- and the reversal is what shows it was.
        --
        -- THREE SEATS: bob's library is seeded and untouched, so "your library"
        -- is told apart from every library. carol holds the third seat so that
        -- bob is not simply "the other player".
        Spec.it s "CR 701.24d the library is shuffled even when the graveyard has been emptied in response" $ do
          blessing <- S.printingOf s registry "Gaea's Blessing"
          piker <- S.printingOf s registry "Goblin Piker"
          bolt <- S.printingOf s registry "Lightning Bolt"
          -- S.addLibraryCard puts each card ON TOP, so the second of each pair
          -- heads the library and the first sits under it.
          let (blessingId, g1) = S.addLibraryCard blessing S.alice S.threePlayerGame
              (herDeeperId, g2) = S.addLibraryCard piker S.alice g1
              (herTopId, g3) = S.addLibraryCard bolt S.alice g2
              (hisDeeperId, g4) = S.addLibraryCard bolt S.bob g3
              (hisTopId, board) = S.addLibraryCard piker S.bob g4
              buried = S.runPure S.identityAnswer board (Event.changeZone blessingId Zone.Graveyard)
              placed = S.runPure S.identityAnswer buried Engine.settleForPriority
              emptied = case Game.zoneMembers Zone.Graveyard S.alice placed of
                -- CR 400.7 minted a fresh id when the card arrived, so the
                -- graveyard's own member is the one to exile.
                [buriedId] -> S.runPure S.identityAnswer placed (Event.changeZone buriedId Zone.Exile)
                _ -> placed
              withCard = S.runPure reversing placed Stack.resolveTop
              withoutCard = S.runPure reversing emptied Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertEqWith s "the control: it shuffles the graveyard's one card in, leaving the graveyard empty" (Game.zoneMembers Zone.Graveyard S.alice withCard) []
          Spec.assertEqWith s "and her library three" (length (Game.zoneMembers Zone.Library S.alice withCard)) 3
          Spec.assertEqWith s "exiled in response, the graveyard is empty before the trigger resolves" (Game.zoneMembers Zone.Graveyard S.alice emptied) []
          Spec.assertEqWith
            s
            "CR 701.24d: her library is shuffled all the same, and gains nothing -- the reversal shows through"
            (Game.zoneMembers Zone.Library S.alice withoutCard)
            [herDeeperId, herTopId]
          Spec.assertEqWith s "with bob's library neither shuffled nor added to" (Game.zoneMembers Zone.Library S.bob withoutCard) [hisTopId, hisDeeperId]
          Spec.assertEqWith s "and the trigger off the stack" (length (GameState.stack withoutCard)) 0

-- CR 113.6m for a TRIGGERED ability: "an ability whose cost or effect specifies
-- that it moves the object it's on out of a particular zone functions only in
-- that zone". The rule says "an ability", not "an activated ability", and
-- Pawl.ActivateSpec's Reassembling Skeleton is the same sentence read off an
-- activated one.
--
-- Squee, Goblin Nabob {2}{R} Legendary Creature -- Goblin 1/1, "At the beginning
-- of your upkeep, you may return this card from your graveyard to your hand."
-- (name, cost, type line, P/T and oracle text checked against Scryfall.) The
-- printing that makes the rule bite, because CR 113.6k cannot reach it: "at the
-- beginning of your upkeep" is a condition that triggers perfectly well from the
-- battlefield, so the only thing that says "graveyard" is the effect's own
-- words.
--
-- The controls are built to leave the zone derivation as the sole difference:
--
--   * Bitterblossom sits in the SAME graveyard, with the SAME condition, and its
--     effect names no zone. CR 113.6's default keeps it on the battlefield, so
--     the upkeep that fires Squee must pass it over. Without it, a scan that
--     simply offered every graveyard card's every ability would pass.
--   * Squee ON the battlefield at the same upkeep fires nothing, which is the
--     "functions ONLY in that zone" half.
-- Drought's FIRST sentence, "At the beginning of your upkeep, sacrifice this
-- enchantment unless you pay {W}{W}" (Oracle text checked against Scryfall) --
-- CR 118.12a's gate over a MANA cost, where Circling Vultures' is over a
-- component. The other two sentences are Pawl.CastSpec's droughtSpec and
-- Pawl.ActivateSpec's droughtActivationSpec.
--
-- THREE boards, and the third is what makes the gate a real choice: an
-- implementation that sacrificed unconditionally passes the first, one that
-- never sacrificed passes the second, and only declining a payment alice could
-- have made tells "she was asked" from "she was charged".
droughtUpkeepSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
droughtUpkeepSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      -- Drought under alice with `plains` Plains beside it, her upkeep begun and
      -- the trigger settled onto the stack.
      board plains n = do
        drought <- S.printingOf s registry "Drought"
        let (droughtId, g1) = S.addCreature drought S.alice (S.landsFor plains S.alice n (Setup.emptyGame S.bothPlayers))
        pure (droughtId, S.runPure S.identityAnswer (beginUpkeep g1) Engine.settleForPriority)
   in Spec.describe s "DroughtUpkeep" $ do
        -- CR 118.3: no white mana, so the payment is not on offer at all and the
        -- "unless" branch runs.
        Spec.it s "CR 118.12a with no white mana Drought sacrifices itself" $ do
          plains <- S.printingOf s registry "Plains"
          (droughtId, onStack) <- board plains 0
          let after = S.runPure (paysFor S.alice) onStack Stack.resolveTop
          Spec.assertBool s (not (S.onBattlefield droughtId after)) "Drought was sacrificed"
          Spec.assertEqWith s "CR 701.21a into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
          Spec.assertBool s (not (null (GameState.stack onStack))) "and the upkeep trigger really reached the stack"
        Spec.it s "CR 118.12a paying {W}{W} keeps it on the battlefield" $ do
          plains <- S.printingOf s registry "Plains"
          (droughtId, onStack) <- board plains 2
          let after = S.runPure (paysFor S.alice) onStack Stack.resolveTop
          Spec.assertBool s (S.onBattlefield droughtId after) "Drought survived"
          Spec.assertEqWith s "both Plains paid for it" (S.tappedCount S.alice after) 2
          Spec.assertEqWith s "and nothing reached her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
        -- The same board, the other answer: the payment is a "may", so declining
        -- one alice could have made is what runs the branch.
        Spec.it s "CR 118.12a declining the payment sacrifices it even off two Plains" $ do
          plains <- S.printingOf s registry "Plains"
          (droughtId, onStack) <- board plains 2
          let after = S.runPure S.identityAnswer onStack Stack.resolveTop
          Spec.assertBool s (not (S.onBattlefield droughtId after)) "Drought was sacrificed"
          Spec.assertEqWith s "with both Plains still untapped" (S.tappedCount S.alice after) 0

graveyardEffectZoneTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graveyardEffectZoneTriggerSpec s registry =
  let squeeName = CardName.MkCardName (Text.pack "Squee, Goblin Nabob")
      upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      settle answer gs = S.runPure answer gs Engine.settleForPriority
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      -- Takes every "may". Squee's is the only one `buriedBoard` can raise --
      -- Bitterblossom's mode is mandatory -- so this is not a blanket yes
      -- standing in for a specific answer.
      takeOptional :: Prompt.Prompt r -> r
      takeOptional p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- alice's graveyard: Squee, and Bitterblossom as the control. Returns
      -- Squee's graveyard id and the board with alice's upkeep begun.
      buriedBoard = do
        squee <- S.printingOf s registry "Squee, Goblin Nabob"
        bitterblossom <- S.printingOf s registry "Bitterblossom"
        let (_, g1) = S.addGraveyardCard bitterblossom S.alice (Setup.emptyGame S.bothPlayers)
            (squeeId, g2) = S.addGraveyardCard squee S.alice g1
        pure (squeeId, beginUpkeep g2)
   in Spec.describe s "GraveyardEffectZoneTrigger" $ do
        -- The gathering itself: one trigger, and it is Squee's. The count is what
        -- the Bitterblossom control turns on -- two would mean the scan read the
        -- graveyard indiscriminately.
        Spec.it s "CR 113.6m Squee's upkeep trigger is gathered from the graveyard, on its effect's word alone" $ do
          (squeeId, gs) <- buriedBoard
          Spec.assertEqWith
            s
            "exactly one trigger, from Squee"
            (fmap PendingTrigger.source (gathered gs))
            [TriggerSource.OfObject squeeId]
        -- End to end through the real engine: the trigger is placed, resolves,
        -- and CR 400.7's funnel moves the card to alice's hand.
        Spec.it s "CR 113.6m whole card: it resolves and Squee returns to its owner's hand" $ do
          (_, gs) <- buriedBoard
          let placed = settle takeOptional gs
              after = S.runPure takeOptional placed Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member squeeName (namesIn Zone.Hand S.alice after)) "Squee is in hand"
          Spec.assertBool s (not (Set.member squeeName (namesIn Zone.Graveyard S.alice after))) "and no longer in the graveyard"
          -- The control, in the same graveyard and under the same condition:
          -- Bitterblossom's effect names no zone, so CR 113.6's default leaves it
          -- on the battlefield and its "you lose 1 life" never runs.
          Spec.assertEqWith s "the Bitterblossom in the graveyard cost alice nothing" (S.lifeOf S.alice after) (Just 20)
        -- CR 603.5: the "may" is a real choice. The trigger is placed either way,
        -- so declining tells the zone gate apart from the mode gate.
        Spec.it s "CR 603.5 declining the may leaves Squee in the graveyard" $ do
          (_, gs) <- buriedBoard
          let placed = settle S.identityAnswer gs
              after = S.runPure S.identityAnswer placed Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack anyway" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member squeeName (namesIn Zone.Graveyard S.alice after)) "Squee is still in the graveyard"
          Spec.assertBool s (not (Set.member squeeName (namesIn Zone.Hand S.alice after))) "and not in hand"
        -- "Functions ONLY in that zone", the other direction: the same card, the
        -- same upkeep, one zone away. Nothing but CR 113.6m can withhold it --
        -- the condition matches a battlefield permanent perfectly well, which is
        -- exactly what Bitterblossom does from there.
        Spec.it s "CR 113.6m the same card on the battlefield triggers for nobody" $ do
          squee <- S.printingOf s registry "Squee, Goblin Nabob"
          let (_, gs) = S.addCreature squee S.alice (Setup.emptyGame S.bothPlayers)
              begun = beginUpkeep gs
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (gathered begun)) []

-- CR 114.4: "abilities of emblems function in the command zone" -- the third zone
-- Pawl.Engine.Event.eventTriggers scans, and the only one whose membership is
-- decided by the OBJECT rather than by the trigger condition (CR 113.6p).
--
-- Ajani, Adversary of Tyrants {2}{W}{W} Legendary Planeswalker -- Ajani, loyalty
-- 4. "-7: You get an emblem with \"At the beginning of your end step, create three
-- 1/1 white Cat creature tokens with lifelink.\"" (Name, cost, type line, loyalty
-- and oracle text checked against Scryfall.) The pool's first emblem with a
-- triggered ability, so before this the ability was authorable and inert.
--
-- The board holds the vacuity traps down:
--
--   * The emblem is the ONLY bearer, and CR 114.1 keeps it in the command zone
--     for its whole existence -- there is no battlefield reading of this ability
--     for the assertion to be passing on instead.
--   * Three seats, and the two boards differ in exactly one thing: WHOSE end step
--     began. "At the beginning of YOUR end step" is CR 114.2's controller, and a
--     scan that took the active player, or the owner of some other object, would
--     fire on bob's.
commandZoneTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
commandZoneTriggerSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      -- One seat's end step, everything else held equal.
      beginEndStepOf pid gs =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan endStep pid))
          gs {GameState.phase = endStep, GameState.activePlayer = pid}
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      catName = CardName.MkCardName (Text.pack "Cat Token")
      cats gs = length (filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just catName) (Set.toList (GameState.battlefield gs)))
      ultimate = 2 :: Int
      -- alice's Ajani at seven loyalty, its ultimate activated and resolved, at
      -- three seats. The loyalty is a fixture rather than seven turns of +1: CR
      -- 306.5b's counters are what the ability's cost pays, and how they got there
      -- is no part of what this asks.
      emblemBoard = do
        ajani <- S.printingOf s registry "Ajani, Adversary of Tyrants"
        let (ajaniId, g1) = S.addCreature ajani S.alice S.threePlayerGame
            armed = S.addCounter CounterKind.Loyalty 7 ajaniId g1
            used = case drop ultimate (Face.activatedAbilities (S.combinedFace ajani)) of
              ability : _ -> S.runPure S.identityAnswer armed (do Activate.activateAbility S.alice ajaniId ability; Stack.resolveTop)
              [] -> armed
        pure (Set.toList (GameState.command used), used)
   in Spec.describe s "CommandZoneTrigger" $ do
        -- The premise, asserted rather than assumed: CR 114.2 put one emblem in
        -- the command zone, and it is on nobody's battlefield.
        Spec.it s "CR 114.2 the ultimate puts one emblem in the command zone" $ do
          (emblems, gs) <- emblemBoard
          Spec.assertEqWith s "one emblem" (length emblems) 1
          Spec.assertEqWith s "no Cats yet" (cats gs) 0
        -- The gathering itself, at the narrowest path: one trigger, borne by the
        -- emblem, from a zone no other source reads.
        Spec.it s "CR 114.4 the emblem's trigger is gathered from the command zone" $ do
          (emblems, gs) <- emblemBoard
          let atEnd = beginEndStepOf S.alice gs
          Spec.assertEqWith
            s
            "exactly one trigger, borne by the emblem"
            (fmap PendingTrigger.source (gathered atEnd))
            (fmap TriggerSource.OfObject emblems)
        -- End to end through the real engine: placed, resolved, three Cats.
        Spec.it s "CR 114.4 whole card: three Cat tokens arrive at its controller's end step" $ do
          (_, gs) <- emblemBoard
          let placed = settle (beginEndStepOf S.alice gs)
              after = resolveAll placed
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertEqWith s "three Cats" (cats after) 3
        -- The negative, on the same board with one thing changed: CR 114.2 makes
        -- the emblem alice's, and bob's end step is not hers.
        Spec.it s "CR 114.2 another seat's end step fires nothing" $ do
          (_, gs) <- emblemBoard
          let atBobs = beginEndStepOf S.bob gs
              after = resolveAll (settle atBobs)
          Spec.assertEqWith s "no trigger gathered" (length (gathered atBobs)) 0
          Spec.assertEqWith s "no Cats" (cats after) 0

-- Serra Avatar ({4}{W}{W}{W} Creature -- Avatar, printed */*), second line: "When
-- Serra Avatar is put into a graveyard from anywhere, shuffle it into its
-- owner's library." Oracle text verified against Scryfall. Its first line, the
-- CR 604.3 life-total P/T, is Pawl.PowerToughnessSpec's half.
--
-- CR 603.6c's LAST sentence is the whole reason this is a condition of its own:
-- "An ability that triggers when a card is put into a certain zone 'from
-- anywhere' is never treated as a leaves-the-battlefield ability, even if an
-- object is put into that zone from the battlefield." Two things follow that
-- SelfDies gets the other way round, and the tests below are built to tell them
-- apart:
--
--   * a NON-battlefield origin fires it. A discarded or milled Serra Avatar
--     never left the battlefield, and encoding this trigger as SelfDies would
--     leave it in the graveyard.
--   * no CR 603.10a look-back. Not being a leaves-the-battlefield ability, this
--     condition is absent from that rule's list of exceptions, so CR 603.10's
--     normal reading applies and the bearer is the CR 400.7 incarnation that
--     ARRIVED in the graveyard -- which is also the card the shuffle has to move,
--     so "self" names it directly and no `became` slot is needed.
--
-- CR 113.6k puts the ability in the graveyard, exactly as it does Narcomoeba's
-- above, and for a nearer reason: this condition can never trigger with its
-- bearer on the battlefield, however the card got to the graveyard.
serraAvatarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
serraAvatarSpec s registry =
  let avatarName = CardName.MkCardName (Text.pack "Serra Avatar")
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      -- Settle so any trigger reaches the stack, then resolve it.
      fireTrigger gs =
        let placed = S.runPure S.identityAnswer gs Engine.settleForPriority
         in (placed, S.runPure S.identityAnswer placed Stack.resolveTop)
      -- One Serra Avatar in a zone of alice's, with priority so
      -- Engine.settleForPriority has somewhere to give it.
      cardIn place = do
        avatar <- S.printingOf s registry "Serra Avatar"
        let (oid, gs) = place avatar S.alice (Setup.emptyGame S.bothPlayers)
        pure (oid, gs {GameState.priority = Just S.alice})
   in Spec.describe s "Serra Avatar" $ do
        -- The gameplay-level proof, cast to resolution: alice Murders her own
        -- Avatar. S.identityAnswer targets the least Recipient and
        -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
        -- creature on the board is the target without a bespoke interpreter.
        Spec.it s "CR 603.6 whole card: a Murdered Serra Avatar shuffles itself into its owner's library" $ do
          swamp <- S.printingOf s registry "Swamp"
          murder <- S.printingOf s registry "Murder"
          avatar <- S.printingOf s registry "Serra Avatar"
          let (gs0, spellId) = S.handOne murder (S.landsInPlay swamp 3)
              (_, board) = S.addCreature avatar S.alice gs0
              cast = S.runPure S.identityAnswer board (S.cast S.alice spellId)
              died = S.runPure S.identityAnswer cast Stack.resolveTop
              (placed, after) = fireTrigger died
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Graveyard S.alice died)) "it died into the graveyard"
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Library S.alice after)) "and CR 701.24 put it into its owner's library"
          Spec.assertBool s (not (Set.member avatarName (namesIn Zone.Graveyard S.alice after))) "leaving the graveyard"
        -- "FROM ANYWHERE" doing real work, half one, and the falsifier for
        -- encoding this trigger as SelfDies: a discarded Serra Avatar never
        -- touched the battlefield, and CR 700.4's "dies" would have nothing to
        -- say about it.
        Spec.it s "CR 603.6 a Serra Avatar put into the graveyard from the HAND triggers" $ do
          (handCard, gs) <- cardIn S.addHandCard
          let discarded = S.runPure S.identityAnswer gs (Event.changeZone handCard Zone.Graveyard)
              (placed, after) = fireTrigger discarded
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Library S.alice after)) "it is in its owner's library"
          Spec.assertBool s (not (Set.member avatarName (namesIn Zone.Graveyard S.alice after))) "and no longer in the graveyard"
        -- Half two, and the falsifier for the other direction -- collapsing this
        -- condition into Narcomoeba's SelfPutIntoGraveyardFromLibrary. A mill
        -- fires BOTH conditions; the hand case above is what only this one sees.
        Spec.it s "CR 603.6 a Serra Avatar milled from the LIBRARY triggers" $ do
          (libraryCard, gs) <- cardIn S.addLibraryCard
          let milled = S.runPure S.identityAnswer gs (Event.changeZone libraryCard Zone.Graveyard)
              (placed, after) = fireTrigger milled
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Library S.alice after)) "it is back in its owner's library"
        -- The DESTINATION is the whole condition, so it has to be load-bearing:
        -- an Avatar exiled off the battlefield left the battlefield just as
        -- surely and reached no graveyard, so this must stay silent where
        -- SelfLeavesTheBattlefield would fire.
        --
        -- Asserted against Event.matchesTrigger DIRECTLY, and deliberately not
        -- through a gathered scan: an exiled card is not among the graveyard
        -- candidates eventTriggers offers, so a scan-level assertion would pass
        -- whether or not this condition reads the destination at all. Both
        -- destinations are asserted from the one event shape, so the True side is
        -- what makes the False side mean something.
        Spec.it s "CR 603.6 only a GRAVEYARD destination matches: exile does not" $ do
          (creature, gs) <- cardIn S.addCreature
          let moveTo to = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange creature creature Zone.Battlefield to) S.emptyCharacteristics)
              matches = Event.matchesTrigger gs creature S.alice TriggerCondition.SelfPutIntoGraveyardFromAnywhere
          Spec.assertBool s (matches (moveTo Zone.Graveyard)) "a graveyard-bound move matches"
          Spec.assertBool s (not (matches (moveTo Zone.Exile))) "an exile-bound move does not"
        -- The gameplay-level companion to the pair above: an Avatar exiled off
        -- the battlefield really does leave nothing on the stack.
        Spec.it s "CR 603.6 a Serra Avatar EXILED from the battlefield triggers nothing" $ do
          (creature, gs) <- cardIn S.addCreature
          let exiled = S.runPure S.identityAnswer gs (Event.changeZone creature Zone.Exile)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (gathered exiled)) []
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Exile S.alice exiled)) "it is in exile"

-- Planar Void ({B} Enchantment): "Whenever another card is put into a graveyard
-- from anywhere, exile that card." Oracle text verified against Scryfall
-- 2026-09-02. The bystander reading of the condition Serra Avatar prints
-- self-scoped above, and the pool's producer for CR 712.21's Example, whose meld
-- board is Pawl.MeldSpec's.
--
-- Three things separate it from the conditions on either side of it, one test
-- each below:
--
--   * the ARRIVING card is what the payload acts on -- "exile that card" reads
--     CR 400.7e's `became` slot, and the departed id CR 400.7 deleted would move
--     nothing.
--   * a NON-battlefield origin fires it, CR 603.6c's last sentence again, so
--     PermanentDies would leave the discarded card in the graveyard.
--   * "another" excludes the Void's own arrival, and by CR 603.10's first
--     sentence rather than by the Filter: a permanent put into a graveyard is
--     not among the objects that exist immediately after the event that put it
--     there.
planarVoidSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
planarVoidSpec s registry =
  let namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      voidName = CardName.MkCardName (Text.pack "Planar Void")
      -- Planar Void on alice's battlefield, with priority so
      -- Engine.settleForPriority has somewhere to give a trigger.
      withVoid = do
        void <- S.printingOf s registry "Planar Void"
        let (voidId, gs) = S.addCreature void S.alice (Setup.emptyGame S.bothPlayers)
        pure (voidId, gs {GameState.priority = Just S.alice})
      fireTrigger gs =
        let placed = S.runPure S.identityAnswer gs Engine.settleForPriority
         in (placed, S.runPure S.identityAnswer placed Stack.resolveTop)
   in Spec.describe s "Planar Void" $ do
        -- The gameplay-level proof: a creature dies with the Void out, and the
        -- card it became is exiled out of the graveyard rather than left there.
        Spec.it s "CR 603.6 whole card: a destroyed creature's card is exiled out of the graveyard" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          (_, board) <- withVoid
          let (pikerId, gs) = S.addCreature piker S.alice board
              died = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [pikerId])
              (placed, after) = fireTrigger died
          Spec.assertBool s (Set.member pikerName (namesIn Zone.Exile S.alice after)) "CR 603.6 the arriving card was exiled"
          Spec.assertBool s (not (Set.member pikerName (namesIn Zone.Graveyard S.alice after))) "and left the graveyard"
          -- The proxies, kept after the behaviour: the card really did reach the
          -- graveyard first, and exactly one trigger did the exiling.
          Spec.assertBool s (Set.member pikerName (namesIn Zone.Graveyard S.alice died)) "setup: it died into the graveyard"
          Spec.assertEqWith s "one trigger reached the stack" (length (GameState.stack placed)) 1
        -- "FROM ANYWHERE" doing real work: a discarded card never left the
        -- battlefield, so PermanentDies would say nothing about it.
        Spec.it s "CR 603.6 a card put into the graveyard from the HAND is exiled too" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          (_, board) <- withVoid
          let (handCard, gs) = S.addHandCard piker S.alice board
              discarded = S.runPure S.identityAnswer gs (Event.changeZone handCard Zone.Graveyard)
              (placed, after) = fireTrigger discarded
          Spec.assertBool s (Set.member pikerName (namesIn Zone.Exile S.alice after)) "CR 603.6 the discarded card was exiled"
          Spec.assertEqWith s "one trigger reached the stack" (length (GameState.stack placed)) 1
        -- The printed "another", and the negative half of the pair above: the
        -- SAME board minus the second permanent. CR 603.10's first sentence is
        -- what declines it -- the Void is not on the battlefield immediately
        -- after the event that put it into the graveyard -- so nothing is
        -- gathered at all and its own card stays where it landed.
        Spec.it s "CR 603.10 the Void's OWN arrival in the graveyard triggers nothing" $ do
          (voidId, board) <- withVoid
          let died = S.runPure S.identityAnswer board (Event.destroy Regenerability.Regenerable [voidId])
              settled = S.runPure S.identityAnswer died Engine.settleForPriority
              -- Resolved as well as settled, so the exile assertion below is
              -- about a trigger that DIDN'T FIRE rather than one that merely sat
              -- on the stack unresolved. Stack.resolveTop over the empty stack
              -- this board leaves is a no-op.
              after = S.runPure S.identityAnswer settled Stack.resolveTop
          Spec.assertBool s (not (Set.member voidName (namesIn Zone.Exile S.alice after))) "CR 603.10 it does not exile its own card"
          Spec.assertBool s (Set.member voidName (namesIn Zone.Graveyard S.alice after)) "CR 603.10 which stays in the graveyard"
          -- The proxies, kept after the behaviour: nothing was gathered and
          -- nothing reached the stack, so the card above is unexiled because the
          -- condition declined rather than because a trigger went unresolved.
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (gathered died)) []
          Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack settled)) 0

-- CR 603.6c: leaves-the-battlefield abilities "trigger when a permanent moves
-- from the battlefield to another zone ... written as, but aren't limited to,
-- 'When [this object] leaves the battlefield, . . .' or 'Whenever [something]
-- is put into a graveyard from the battlefield, . . . .'" Doomed Traveler
-- prints the second of those in its abbreviated form: CR 700.4 says "the term
-- dies means 'is put into a graveyard from the battlefield.'"
--
-- CR 603.10a is what makes it more than a tenth condition: "Some zone-change
-- triggers look back in time. These are leaves-the-battlefield abilities ...",
-- so the match is against the game as it was IMMEDIATELY BEFORE the event. By
-- the time the scan runs, the Traveler is a card in a graveyard with a fresh id
-- (CR 400.7) and nothing is on the battlefield to find -- which is what makes
-- the token appearing at all the discriminating assertion here.
diesTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
diesTriggerSpec s registry =
  let -- alice: one Mountain (Lightning Bolt's {R}), a Doomed Traveler in play,
      -- and the Bolt in hand. S.identityAnswer targets the least Recipient, and
      -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
      -- creature on the board is the target without a bespoke interpreter.
      boltBoard = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        doomedTraveler <- S.printingOf s registry "Doomed Traveler"
        let (_, withTraveler) = S.addCreature doomedTraveler S.alice (S.landsInPlay mountain 1)
        pure (S.handOne lightningBolt withTraveler)
      -- Cast the Bolt, resolve it (3 damage marked on a 1/1), settle -- CR
      -- 704.5g's state-based action destroys it and the CR 117.5 settle's OWN
      -- trigger scan must see that death -- then resolve the trigger.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      spiritsOf pid gs =
        filter
          -- CR 111.4: Doomed Traveler does not specify the token's name, so the
          -- name is its subtype plus the word "Token".
          (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Spirit Token"))
          (Game.zoneMembers Zone.Battlefield pid gs)
      travelerName = CardName.MkCardName $ Text.pack "Doomed Traveler"
   in Spec.describe s "DiesTrigger" $ do
        -- The gameplay-level proof, cast to resolution, through a real
        -- removal spell and the state-based action it sets up.
        Spec.it s "CR 603.6c whole card: Lightning Bolt kills Doomed Traveler and its dies trigger makes a flying Spirit" $ do
          board <- boltBoard
          let (settled, after) = boltIt board
          -- The trigger was gathered in the SAME settle that ran the SBA
          -- (Engine.settleForPriority: performStateBasedActions, then
          -- placePendingTriggers, then loop).
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          -- And it did so with the Traveler already gone: an implementation
          -- matching against the live battlefield would find nothing here.
          Spec.assertBool s (Set.member travelerName (namesIn Zone.Graveyard S.alice settled)) "the Traveler is in the graveyard by then"
          Spec.assertBool s (not (Set.member travelerName (namesIn Zone.Battlefield S.alice settled))) "and not on the battlefield"
          case spiritsOf S.alice after of
            [spirit] -> do
              Spec.assertEqWith s "power" (Projection.powerOf spirit after) (Just 1)
              Spec.assertEqWith s "toughness" (Projection.toughnessOf spirit after) (Just 1)
              Spec.assertEqWith s "white" (Projection.colorsOf spirit after) (Set.singleton Color.White)
              Spec.assertEqWith s "Spirit" (Projection.subtypesOf spirit after) (Set.singleton Subtype.Spirit)
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Flying spirit after) "with flying"
            other -> Spec.assertFailure s ("expected exactly one Spirit token, got " <> show (length other))
        -- CR 700.4 doing real work: "dies" is NARROWER than CR 603.6c's
        -- leaves-the-battlefield. The same permanent moved from the
        -- battlefield to EXILE has left the battlefield and has not died.
        Spec.it s "CR 700.4 a Traveler exiled from the battlefield does not trigger" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          let (traveler, gs) = S.addCreature doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
              exiled = S.runPure S.identityAnswer gs (Event.changeZone traveler Zone.Exile)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (gathered exiled)) []
          Spec.assertBool s (Set.member travelerName (namesIn Zone.Exile S.alice exiled)) "it is in exile"
        -- The other half of "from the battlefield": the same card discarded
        -- reaches the same graveyard and has not died (CR 700.4).
        Spec.it s "CR 700.4 a Traveler discarded from the HAND does not trigger" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          let (traveler, gs) = S.addHandCard doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
              discarded = S.runPure S.identityAnswer gs (Event.changeZone traveler Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (gathered discarded)) []
        -- Self-scoped: SOME OTHER creature dying is not this Traveler's
        -- death, even though the Traveler is right there to see it.
        Spec.it s "CR 603.6c another creature dying does not fire the Traveler's trigger" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withTraveler) = S.addCreature doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, gs) = S.addCreature piker S.alice withTraveler
              died = S.runPure S.identityAnswer gs (Event.changeZone pikerId Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (gathered died)) []
        -- CR 603.3a through CR 603.10a's look-back: "the player who controlled
        -- the ability's source at the time it triggered" is read from the game
        -- as it was immediately BEFORE the death, so a Traveler bob owns but
        -- alice has stolen with Control Magic hands ALICE the Spirit. Reading
        -- the graveyard card's owner instead would answer bob.
        Spec.it s "CR 603.3a the trigger is controlled by whoever controlled the Traveler as it died" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          controlMagic <- S.printingOf s registry "Control Magic"
          let (traveler, withTraveler) = S.addCreature doomedTraveler S.bob (Setup.emptyGame S.bothPlayers)
              (aura, withAura) = S.addCreature controlMagic S.alice withTraveler
              stolen = S.attach aura traveler withAura
              died = S.runPure S.identityAnswer stolen (Event.changeZone traveler Zone.Graveyard)
          Spec.assertEqWith s "alice controlled it as it died" (Projection.controllerOf traveler stolen) (Just S.alice)
          Spec.assertEqWith s "so the trigger is hers, not its owner's" (fmap PendingTrigger.controller (gathered died)) [S.alice]

-- CR 603.6c's SECOND written form with a BYSTANDER bearer -- "Whenever
-- [something] is put into a graveyard from the battlefield", narrowed by CR
-- 700.4's "dies" -- and Meren of Clan Nel Toth {2}{B}{G} Legendary Creature --
-- Human Shaman 3/4, "Whenever another creature you control dies, you get an
-- experience counter", the card that proves it.
--
-- diesTriggerSpec above is the SELF-scoped half of the same rule: there the
-- bearer IS the permanent that died. Here the bearer watches, so the three
-- printed words that narrow the watching are three arms of the condition's own
-- Filter, exactly as Soul Warden's "another" is (#163) -- "creature" is
-- HasCardType, "you control" is ControlledBy You read against CR 109.5's you
-- (the ability's controller, CR 603.3a), and "another" is Not IsSource. Each
-- gets a falsifier below, because a condition that ignored any one of them
-- would still pass the whole-card case.
--
-- The dying permanent is read from CR 608.2h last known information, which is
-- not an implementation convenience but CR 603.10a ("some zone-change triggers
-- look back in time. These are leaves-the-battlefield abilities ...") read
-- through CR 603.10's own definition of looking back: "using the existence of
-- those abilities and the appearance of objects immediately prior to the
-- event." By the CR 117.5 boundary the creature is a card in a graveyard,
-- CR 108.4 says "a card doesn't have a controller unless that card represents a
-- permanent or spell", and CR 108.4a would hand a matcher reading the graveyard
-- card its OWNER instead -- so a creature its controller had stolen would be
-- credited back to the player who no longer had it. The Control Magic case at
-- the end is that falsifier.
--
-- Meren's SECOND ability -- the end-step reanimation -- is `merenEndStepSpec`
-- below; the last assertion in this group is what keeps the two from drifting
-- onto different cards.
--
-- Two cases here kill the bearer and another creature in one batch, and they are
-- a PAIR: CR 704.3 makes Day of Judgment's deaths one event, so Meren sees the
-- Piker that died alongside her, while a Salt Road Skirmish that destroys her and
-- then buries two tokens later in the same batch is a sequence, and she sees
-- neither. Both are needed -- admitting the whole batch passes the first and
-- answers the second 2 -- and the first is asserted for BOTH object-id orders,
-- since that is what makes it a test of the rule rather than of the id minting.
permanentDiesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentDiesSpec s registry =
  let anotherCreatureYouControl =
        Filter.Type.And
          [ Filter.Type.HasCardType CardType.Creature,
            Filter.Type.ControlledBy PlayerRelation.You,
            Filter.Type.Not Filter.Type.IsSource
          ]
      sourcesOf gs = fmap PendingTrigger.source (gathered gs)
      experienceOf = S.playerCounterOf PlayerCounterKind.Experience
      -- alice's Meren beside one creature of `victim`'s printing, controlled by
      -- `owner`.
      merenBeside victim owner gs0 = do
        meren <- S.printingOf s registry "Meren of Clan Nel Toth"
        printing <- S.printingOf s registry victim
        let (merenId, withMeren) = S.addCreature meren S.alice gs0
            (victimId, gs) = S.addCreature printing owner withMeren
        pure (merenId, victimId, gs)
   in Spec.describe s "PermanentDies" $ do
        -- The gameplay-level proof, cast to resolution: alice's Lightning Bolt
        -- kills her own Goblin Piker, CR 704.5g's state-based action moves it
        -- to the graveyard, and the CR 117.5 settle's trigger scan sees the
        -- death. One experience counter, from a card that started with none.
        Spec.it s "CR 700.4 whole cards: alice's Piker dies and her Meren gets an experience counter" $ do
          mountain <- S.printingOf s registry "Mountain"
          lightningBolt <- S.printingOf s registry "Lightning Bolt"
          (_, pikerId, board) <- merenBeside "Goblin Piker" S.alice (S.landsInPlay mountain 1)
          let (gs, spellId) = S.handOne lightningBolt board
              -- Bolt the Piker by id rather than by S.identityAnswer's least
              -- Recipient, which would aim at whichever creature sorts first.
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature pikerId))) sets
                _ -> S.identityAnswer p
              cast = S.runPure answer gs (S.cast S.alice spellId)
              damaged = S.runPure answer cast Stack.resolveTop
              settled = S.runPure answer damaged Engine.settleForPriority
              after = S.runPure answer settled Stack.resolveTop
          Spec.assertEqWith s "alice starts with no experience" (experienceOf S.alice gs) 0
          Spec.assertEqWith s "the Piker is gone" (Game.lookupObject pikerId settled) Nothing
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and alice has exactly one experience counter" (experienceOf S.alice after) 1
          Spec.assertEqWith s "bob has none" (experienceOf S.bob after) 0
        -- CR 603.10a's own Example, played out: "Two creatures are on the
        -- battlefield along with an artifact that has the ability 'Whenever a
        -- creature dies, you gain 1 life.' Someone casts a spell that destroys
        -- all artifacts, creatures, and enchantments. The artifact's ability
        -- triggers twice, EVEN THOUGH THE ARTIFACT GOES TO ITS OWNER'S
        -- GRAVEYARD AT THE SAME TIME AS THE CREATURES." Meren is that artifact
        -- and the Piker is one of those creatures: CR 704.3 / CR 608.2f make
        -- Day of Judgment's two deaths ONE event, so the look-back reads a board
        -- on which Meren and the Piker were both still there.
        --
        -- BOTH ID ORDERS, which is the whole point rather than belt and braces:
        -- the two boards differ in nothing a rule can see, so an engine that
        -- answered by the order the ids were minted in would answer them
        -- differently. The Meren-first board is the one that used to answer 0.
        --
        -- Exactly ONE counter, not two: Meren's own death is excluded by the
        -- printed "another" (the falsifier below), so the count discriminates
        -- between seeing her group-mate and seeing her whole group.
        Spec.it s "CR 603.10a Meren sees the Piker that died alongside her, in either id order" $ do
          plains <- S.printingOf s registry "Plains"
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          piker <- S.printingOf s registry "Goblin Piker"
          dayOfJudgment <- S.printingOf s registry "Day of Judgment"
          let -- The two boards, differing only in which of the two creatures was
              -- minted first and so which one Event.destroyIn reaches first.
              board merenFirst =
                let base = Setup.emptyGame S.bothPlayers
                 in if merenFirst
                      then snd (S.addCreature piker S.alice (snd (S.addCreature meren S.alice base)))
                      else snd (S.addCreature meren S.alice (snd (S.addCreature piker S.alice base)))
              run merenFirst =
                let withLands = List.foldl' (\gs _ -> snd (S.addCreature plains S.alice gs)) (board merenFirst) [1 :: Int .. 4]
                    (withSpell, spell) = S.handOne dayOfJudgment withLands
                    afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
                    swept = S.runPure S.identityAnswer afterCast Stack.resolveTop
                    settled = S.runPure S.identityAnswer swept Engine.settleForPriority
                 in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
              (merenFirstSettled, merenFirstAfter) = run True
              (pikerFirstSettled, pikerFirstAfter) = run False
              creaturesLeft gs = Set.size (Set.filter (`Projection.isCreatureOf` gs) (GameState.battlefield gs))
          Spec.assertEqWith s "the sweep left no creatures either way" (fmap creaturesLeft [merenFirstSettled, pikerFirstSettled]) [0, 0]
          Spec.assertEqWith s "one trigger reached the stack with Meren minted first" (length (GameState.stack merenFirstSettled)) 1
          Spec.assertEqWith s "and one with the Piker minted first" (length (GameState.stack pikerFirstSettled)) 1
          Spec.assertEqWith s "alice has exactly one experience counter with Meren minted first" (experienceOf S.alice merenFirstAfter) 1
          Spec.assertEqWith s "and exactly one with the Piker minted first" (experienceOf S.alice pikerFirstAfter) 1
          Spec.assertEqWith s "bob has none either way" (fmap (experienceOf S.bob) [merenFirstAfter, pikerFirstAfter]) [0, 0]
        -- The control that keeps the case above from being answered by simply
        -- admitting everything in the batch. Three groups, ONE batch: alice's
        -- Salt Road Skirmish destroys her own Meren (CR 701.8), then creates two
        -- 1/1 Warrior tokens later in that same resolution, and the CR 117.5
        -- settle's first state-based-action pass buries both as 0/0 under Night
        -- of Souls' Betrayal (CR 704.5f). GameState.scannedThrough is not bumped
        -- until the trigger scan, so all three share a batch -- and none of them
        -- shares Meren's event.
        --
        -- ZERO counters is the rules answer: two creatures alice controls die
        -- STRICTLY AFTER Meren left, so CR 603.10a's "immediately prior to the
        -- event" reads a board she is not on. A look-back that took the whole
        -- batch rather than the event's own group would answer 2.
        --
        -- The target is pinned by ObjectId rather than left to
        -- S.identityAnswer's least Recipient, the way the Bolt above is.
        Spec.it s "CR 603.10a a Meren who died earlier in the batch sees neither token buried later in it" $ do
          swamp <- S.printingOf s registry "Swamp"
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          night <- S.printingOf s registry "Night of Souls' Betrayal"
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          let (merenId, withMeren) = S.addCreature meren S.alice (Setup.emptyGame S.bothPlayers)
              (_, withNight) = S.addCreature night S.alice withMeren
              withLands = List.foldl' (\gs _ -> snd (S.addCreature swamp S.alice gs)) withNight [1 :: Int .. 4]
              (withSpell, spell) = S.handOne skirmish withLands
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature merenId))) sets
                _ -> S.identityAnswer p
              afterCast = S.runPure answer withSpell (S.cast S.alice spell)
              resolved = S.runPure answer afterCast Stack.resolveTop
              settled = S.runPure answer resolved Engine.settleForPriority
          Spec.assertEqWith s "Meren was destroyed by her controller's own spell" (Game.lookupObject merenId settled) Nothing
          Spec.assertEqWith s "the two tokens entered and were buried as 0/0" (length (filter (\zc -> ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard) (S.zoneChangesOf settled))) 3
          Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack settled)) 0
          Spec.assertEqWith s "and alice has no experience counters at all" (experienceOf S.alice settled) 0
        -- "ANOTHER", the Filter's Not IsSource arm. Meren's own death IS a
        -- creature alice controls dying, so the silence has to come from the
        -- exclusion rather than from the condition failing to see the death at
        -- all -- and the second pair of assertions is what tells those apart:
        -- the same bearer, the same event, and a Filter differing only in the
        -- exclusion fires. That also exercises the CR 608.2h read on a bearer
        -- and a candidate that are one departed object.
        Spec.it s "CR 603.6c another: Meren's own death does not give her controller a counter" $ do
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          let anyCreatureYouControl =
                Filter.Type.And
                  [ Filter.Type.HasCardType CardType.Creature,
                    Filter.Type.ControlledBy PlayerRelation.You
                  ]
              (merenId, gs) = S.addCreature meren S.alice (Setup.emptyGame S.bothPlayers)
              died = S.runPure S.identityAnswer gs (Event.changeZone merenId Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (sourcesOf died) []
          case Event.unscannedEvents died of
            [death] -> do
              Spec.assertBool s (Event.matchesTrigger died merenId S.alice (TriggerCondition.PermanentDies anyCreatureYouControl) death) "a Filter without the exclusion admits Meren's own death"
              Spec.assertBool s (not (Event.matchesTrigger died merenId S.alice (TriggerCondition.PermanentDies anotherCreatureYouControl) death)) "so the printed \"another\" is the only thing declining it"
            other -> Spec.assertFailure s ("expected exactly one event, got " <> show (length other))
        -- "YOU CONTROL", the ControlledBy arm, read through CR 109.5 against
        -- the ability's controller (CR 603.3a). bob's creature dying in front
        -- of alice's Meren is not it.
        Spec.it s "CR 109.5 you control: an opponent's creature dying fires nothing" $ do
          (_, pikerId, gs) <- merenBeside "Goblin Piker" S.bob (Setup.emptyGame S.bothPlayers)
          let died = S.runPure S.identityAnswer gs (Event.changeZone pikerId Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (sourcesOf died) []
        -- "CREATURE", the HasCardType arm. A land alice controls reaching the
        -- same graveyard from the same battlefield is not a creature dying.
        Spec.it s "CR 205.2a creature: a land of alice's dying fires nothing" $ do
          (_, landId, gs) <- merenBeside "Mountain" S.alice (Setup.emptyGame S.bothPlayers)
          let died = S.runPure S.identityAnswer gs (Event.changeZone landId Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (sourcesOf died) []
        -- CR 700.4 doing the same work it does for SelfDies: "dies" is
        -- narrower than CR 603.6c's leaves-the-battlefield. alice's own
        -- creature EXILED off the battlefield has left it without dying.
        Spec.it s "CR 700.4 a creature of alice's exiled from the battlefield fires nothing" $ do
          (_, pikerId, gs) <- merenBeside "Goblin Piker" S.alice (Setup.emptyGame S.bothPlayers)
          let exiled = S.runPure S.identityAnswer gs (Event.changeZone pikerId Zone.Exile)
          Spec.assertEqWith s "nothing triggered" (sourcesOf exiled) []
        -- The same card discarded from a HAND reaches the same graveyard and
        -- has not died: the `from` half of CR 700.4.
        Spec.it s "CR 700.4 a creature card discarded from alice's hand fires nothing" $ do
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withMeren) = S.addCreature meren S.alice (Setup.emptyGame S.bothPlayers)
              (handCard, gs) = S.addHandCard piker S.alice withMeren
              discarded = S.runPure S.identityAnswer gs (Event.changeZone handCard Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (sourcesOf discarded) []
        -- CR 603.10a / CR 608.2h: the dying creature is read as it was
        -- IMMEDIATELY PRIOR to the event. bob owns the Piker, alice has stolen
        -- it with Control Magic, and it dies -- so "a creature YOU control"
        -- holds for alice. A matcher reading the card that landed in the
        -- graveyard would take CR 108.4a's substitute for CR 108.4's missing
        -- controller -- its owner, bob -- and answer no.
        Spec.it s "CR 608.2h a stolen creature dying is read with the controller it had as it left" $ do
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          piker <- S.printingOf s registry "Goblin Piker"
          controlMagic <- S.printingOf s registry "Control Magic"
          let (merenId, withMeren) = S.addCreature meren S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, withPiker) = S.addCreature piker S.bob withMeren
              (aura, withAura) = S.addCreature controlMagic S.alice withPiker
              stolen = S.attach aura pikerId withAura
              died = S.runPure S.identityAnswer stolen (Event.changeZone pikerId Zone.Graveyard)
          Spec.assertEqWith s "alice controlled it as it died" (Projection.controllerOf pikerId stolen) (Just S.alice)
          Spec.assertEqWith s "so her Meren triggered" (sourcesOf died) [TriggerSource.OfObject merenId]
        -- The condition is the card's, not this spec's: the printed Filter is
        -- what the matcher is asked about everywhere above. The second entry is
        -- the end-step ability merenEndStepSpec below plays out, and it is
        -- asserted here so the two groups cannot drift onto different cards.
        Spec.it s "Meren's printed condition is PermanentDies over another creature you control" $ do
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          Spec.assertEqWith
            s
            "two triggered abilities, the first with that condition"
            (fmap TriggeredAbility.condition (Face.triggeredAbilities (S.combinedFace meren)))
            [ TriggerCondition.PermanentDies anotherCreatureYouControl,
              TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Ending EndingStep.EndStep) TurnScope.ControllersTurn)
            ]

-- CR 603.2c's two sentences, and the fork they force on the written form
-- permanentDiesSpec above proves the other side of. Vengeful Townsfolk's
-- "whenever ONE OR MORE other creatures you control die" names the whole CR 704.3
-- batch as its trigger event, so a state-based-action pass that buries three
-- creatures contains ONE occurrence of it -- where Meren's "whenever another
-- creature you control dies" names each death, and the rule's own Example fires
-- that once per member.
--
-- Three boards prove the arity, and all three are load-bearing:
--
--   * one batch of three, answering ONE counter. TWO of alice's rather than one,
--     because a lone death makes the two readings agree and the board could not
--     discriminate at all; bob's third, because without a creature dying under
--     another controller in the same batch the Filter's "you control" half cannot
--     be observed false.
--   * a SECOND batch later, answering a second counter. Without it an
--     implementation that fired once per TURN passes the first board.
--   * two death GROUPS inside ONE CR 117.5 scan, answering two counters.
--     GameState.scannedThrough is not bumped until the scan, so several groups
--     share one; without this board an implementation that deduped per scan
--     rather than per group passes the other two.
--
-- Two more prove the same arity over the OTHER road into a graveyard, a swept
-- Effect.MoveToZone rather than CR 704.3's pass. Synthetic Twofold Interment is
-- both of them: a two-clause sorcery, "put all Goblins into their owners'
-- graveyards" and then the same over Zombies. Four Goblins and no Zombie answer
-- ONE counter, where a Goblin and a Zombie answer two -- one instruction is one
-- event however many permanents it takes, and two instructions are two however
-- few. The second board is what separates "one event" from "one resolution",
-- and neither can be got from the three above, none of which moves a permanent
-- through Pawl.Engine.Resolve's opcode at all.
--
-- WHY A SYNTHETIC SWEEPER. Scryfall o:"into their owners' graveyards", 2026-08-27,
-- matches eight printings and not one of them takes a permanent off the
-- battlefield; oracle:/put (all|each|target) .*(creature|permanent)s? .*into (its
-- owner's|their owners') graveyard/ -o:"from your graveyard", same date, matches
-- Bronzebeak Foragers and Gelatinous Cube, both of which move a card out of
-- exile. Every printed mass move from the battlefield to a graveyard is worded
-- as a destruction (CR 701.8) or a sacrifice (CR 701.21), and pawl routes those
-- through Event.destroyIn and Event.sacrifice rather than through this opcode. A
-- printing whose own words put several permanents into graveyards would refute
-- the synthetic; nothing in the CR forbids one, CR 400.7 being the same funnel
-- either way.
--
-- Lethal damage settled through Engine.settleForPriority rather than a sweeper on
-- the first two boards: a sweeper kills the bearer too, and this card's payload
-- acts on itself, so there would be nothing left to read the counter off. The
-- deaths are one event either way -- Sba.performStateBasedActions is one
-- Event.simultaneously bracket.
--
-- Three more boards follow, on the Filter and on CR 603.10a: the arity boards
-- cannot see either half of "other creatures YOU CONTROL", since once the batch is
-- deduped to one trigger, admitting an extra dying creature changes no counter.
-- Each is the first board with exactly one thing changed.
permanentsDieSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentsDieSpec s registry =
  let -- The distinct EventGroups the log's battlefield-to-graveyard moves carry.
      -- The precondition every case below rests on: were the deaths not one group,
      -- "once for the batch" and "once per turn" would be the same claim, and the
      -- gameplay assertion would prove the weaker one.
      deathGroups gs =
        List.nub
          ( Maybe.mapMaybe
              ( \logged -> case LoggedEvent.event logged of
                  GameEvent.Moved (Moved.MkMoved zc _ _)
                    | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard -> Just (LoggedEvent.group logged)
                  _ -> Nothing
              )
              (Foldable.toList (GameState.events gs))
          )
      -- alice's Vengeful Townsfolk, three Goblin Pikers of hers and one of bob's.
      -- The Townsfolk takes no damage on any board here, so the bearer is standing
      -- when the counter is read -- Pawl.Support's builders settle what they place,
      -- so nothing else about it needs arranging.
      townsfolkBoard = do
        townsfolk <- S.printingOf s registry "Vengeful Townsfolk"
        piker <- S.printingOf s registry "Goblin Piker"
        let (bearer, withBearer) = S.addCreature townsfolk S.alice (Setup.emptyGame S.bothPlayers)
            (aliceFirst, withFirst) = S.addCreature piker S.alice withBearer
            (aliceSecond, withSecond) = S.addCreature piker S.alice withFirst
            (aliceThird, withThird) = S.addCreature piker S.alice withSecond
            (bobs, gs) = S.addCreature piker S.bob withThird
        pure (bearer, aliceFirst, aliceSecond, aliceThird, bobs, gs)
      -- Every trigger the settle put on the stack, resolved. Resolving only the TOP
      -- would leave the power/toughness assertions below vacuous: a broken engine's
      -- extra triggers would still be sitting on the stack, unread, and the bearer
      -- would answer the correct number either way. The stack strictly shrinks --
      -- nothing here is a settle, and a +1/+1 counter puts nothing back on.
      resolveWholeStack gs =
        if null (GameState.stack gs)
          then gs
          else resolveWholeStack (S.runPure S.identityAnswer gs Stack.resolveTop)
      -- A Goblin Piker is a 2/1, so one marked damage is CR 704.5g lethal.
      lethal = 1 :: Natural
   in Spec.describe s "PermanentsDie" $ do
        -- The proving case. Three creatures die in one CR 704.3 pass, two of them
        -- alice's, and her Townsfolk grows by exactly one.
        --
        -- 4/4 is the whole discrimination: 5/5 is the per-object reading (one
        -- counter per creature of hers that died) and 3/3 is silence.
        Spec.it s "CR 603.2c three creatures dying in one batch give the Townsfolk one counter" $ do
          (bearer, aliceFirst, aliceSecond, _, bobs, board) <- townsfolkBoard
          let damaged = S.markDamage aliceFirst lethal (S.markDamage aliceSecond lethal (S.markDamage bobs lethal board))
              settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
              after = resolveWholeStack settled
          Spec.assertEqWith s "the Townsfolk is a 4/4: one +1/+1 counter for the whole batch" (S.powerToughnessOf bearer after) (Just (4, 4))
          Spec.assertEqWith s "it was a 3/3 before the batch" (S.powerToughnessOf bearer board) (Just (3, 3))
          Spec.assertEqWith s "all three creatures died" (fmap (\oid -> Game.lookupObject oid settled) [aliceFirst, aliceSecond, bobs]) [Nothing, Nothing, Nothing]
          Spec.assertEqWith s "and they died as one event group" (length (deathGroups settled)) 1
          Spec.assertEqWith s "so exactly one trigger reached the stack" (length (GameState.stack settled)) 1
        -- The other half, without which "fires once per turn" answers the board
        -- above correctly. A second batch, in the same turn, is a second trigger
        -- event: 5/5, where a once-per-turn limiter leaves the 4/4 above standing.
        --
        -- One death this time rather than two, which is the point: the first board
        -- is what proves a batch of several fires once, so this one only has to be
        -- a second batch.
        Spec.it s "CR 603.2c a second batch in the same turn is a second trigger event" $ do
          (bearer, aliceFirst, aliceSecond, aliceThird, bobs, board) <- townsfolkBoard
          let damaged = S.markDamage aliceFirst lethal (S.markDamage aliceSecond lethal (S.markDamage bobs lethal board))
              settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
              after = resolveWholeStack settled
              again = S.runPure S.identityAnswer (S.markDamage aliceThird lethal after) Engine.settleForPriority
              afterAgain = resolveWholeStack again
          Spec.assertEqWith s "the Townsfolk is a 5/5 after the second batch" (S.powerToughnessOf bearer afterAgain) (Just (5, 5))
          Spec.assertEqWith s "it was a 4/4 after the first" (S.powerToughnessOf bearer after) (Just (4, 4))
          Spec.assertEqWith s "the third Piker died in a group of its own" (length (deathGroups again)) 2
          Spec.assertEqWith s "and one more trigger reached the stack" (length (GameState.stack again)) 1
        -- The control that separates "once per event group" from "once per trigger
        -- scan", which the two boards above cannot tell apart: alice's Salt Road
        -- Skirmish destroys her own Goblin Piker (CR 701.8, one group), then creates
        -- two 1/1 Warrior tokens later in that same resolution, and the CR 117.5
        -- settle's state-based-action pass buries both as 0/0 under Night of Souls'
        -- Betrayal (CR 704.5f, a second group). GameState.scannedThrough is not
        -- bumped until the trigger scan, so both groups are read by one scan.
        --
        -- TWO counters is the rules answer, CR 704.3 making each pass its own single
        -- event. 4/4 is 3/3, Night's -1/-1 and two counters; a per-scan dedup leaves
        -- 3/3 and the per-object reading gives 5/5, so the three readings are three
        -- different creatures.
        --
        -- The bearer survives Night as a 2/2, which is what leaves a permanent to
        -- read the counters off. The target is pinned by ObjectId rather than left
        -- to S.identityAnswer's least Recipient.
        Spec.it s "CR 704.3 two death groups in one trigger scan are two trigger events" $ do
          swamp <- S.printingOf s registry "Swamp"
          townsfolk <- S.printingOf s registry "Vengeful Townsfolk"
          piker <- S.printingOf s registry "Goblin Piker"
          night <- S.printingOf s registry "Night of Souls' Betrayal"
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          let (bearer, withBearer) = S.addCreature townsfolk S.alice (Setup.emptyGame S.bothPlayers)
              (victim, withVictim) = S.addCreature piker S.alice withBearer
              (_, withNight) = S.addCreature night S.alice withVictim
              withLands = List.foldl' (\gs _ -> snd (S.addCreature swamp S.alice gs)) withNight [1 :: Int .. 4]
              (withSpell, spell) = S.handOne skirmish withLands
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
                _ -> S.identityAnswer p
              afterCast = S.runPure answer withSpell (S.cast S.alice spell)
              resolved = S.runPure answer afterCast Stack.resolveTop
              settled = S.runPure answer resolved Engine.settleForPriority
              twice = resolveWholeStack settled
          Spec.assertEqWith s "the Townsfolk is a 4/4: 2/2 under Night, plus two counters" (S.powerToughnessOf bearer twice) (Just (4, 4))
          Spec.assertEqWith s "it was a 2/2 before either batch" (S.powerToughnessOf bearer withLands) (Just (2, 2))
          Spec.assertEqWith s "the Piker and both tokens reached a graveyard" (length (filter (\zc -> ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard) (S.zoneChangesOf settled))) 3
          Spec.assertEqWith s "in two event groups, both inside one scan" (length (deathGroups settled)) 2
          Spec.assertEqWith s "so two triggers reached the stack" (length (GameState.stack settled)) 2
        -- CR 608.2f over the MOVES themselves. alice's Synthetic Twofold Interment
        -- puts four Goblins into their owners' graveyards in one instruction --
        -- three of hers and bob's -- and her Townsfolk grows by exactly one.
        --
        -- 4/4 is the whole discrimination, and the three Goblins of hers are what
        -- give it room: 6/6 is the per-mover reading, one counter for each of her
        -- creatures the sweep buried, and 3/3 is silence. bob's Goblin is in the
        -- sweep for the first board's reason -- the Filter's "you control" half is
        -- observed false only where a creature under another controller dies in the
        -- same event.
        --
        -- The Townsfolk is a Citizen Human and survives the sweep, which is what
        -- leaves a permanent to read the counter off; its own payload acts on
        -- itself, so a board that buried the bearer could show nothing. No Zombie
        -- stands here, so the card's second clause takes nothing and this board
        -- reads one instruction alone.
        Spec.it s "CR 608.2f four Goblins swept into graveyards give the Townsfolk one counter" $ do
          swamp <- S.printingOf s registry "Swamp"
          townsfolk <- S.printingOf s registry "Vengeful Townsfolk"
          piker <- S.printingOf s registry "Goblin Piker"
          interment <- S.printingOf s registry "Synthetic Twofold Interment"
          let (bearer, withBearer) = S.addCreature townsfolk S.alice (Setup.emptyGame S.bothPlayers)
              (aliceFirst, withFirst) = S.addCreature piker S.alice withBearer
              (aliceSecond, withSecond) = S.addCreature piker S.alice withFirst
              (aliceThird, withThird) = S.addCreature piker S.alice withSecond
              (bobs, withBobs) = S.addCreature piker S.bob withThird
              withLands = List.foldl' (\gs _ -> snd (S.addCreature swamp S.alice gs)) withBobs [1 :: Int .. 3]
              (withSpell, spell) = S.handOne interment withLands
              afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
              resolved = S.runPure S.identityAnswer afterCast Stack.resolveTop
              settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
              after = resolveWholeStack settled
          Spec.assertEqWith s "the Townsfolk is a 4/4: one +1/+1 counter for the whole sweep" (S.powerToughnessOf bearer after) (Just (4, 4))
          Spec.assertEqWith s "it was a 3/3 before the sweep" (S.powerToughnessOf bearer withLands) (Just (3, 3))
          Spec.assertEqWith s "all four Goblins left the battlefield" (fmap (\oid -> Game.lookupObject oid resolved) [aliceFirst, aliceSecond, aliceThird, bobs]) [Nothing, Nothing, Nothing, Nothing]
          Spec.assertEqWith s "and they moved as one event group" (length (deathGroups resolved)) 1
          Spec.assertEqWith s "so exactly one trigger reached the stack" (length (GameState.stack settled)) 1
        -- The control the board above needs, on the same road and inside the same
        -- resolution: the Interment's SECOND clause is its own instruction, so a
        -- Goblin and a Zombie leave in two event groups and the Townsfolk takes two
        -- counters. 5/5 is the rules answer, CR 608.2c making each clause its own
        -- action; 4/4 is a bracket drawn around the whole resolution rather than
        -- around one instruction's fold, which is the exact way the board above
        -- could be passed for the wrong reason.
        --
        -- One creature per clause, which is the point: the board above proves that
        -- a sweep of several fires once, so this one only has to be two sweeps --
        -- and both sit inside ONE CR 117.5 scan, where two casts would have been
        -- told apart by the scan boundary alone.
        Spec.it s "CR 608.2c two clauses of one resolution are two trigger events" $ do
          swamp <- S.printingOf s registry "Swamp"
          townsfolk <- S.printingOf s registry "Vengeful Townsfolk"
          piker <- S.printingOf s registry "Goblin Piker"
          zombie <- S.printingOf s registry "Whipstitched Zombie"
          interment <- S.printingOf s registry "Synthetic Twofold Interment"
          let (bearer, withBearer) = S.addCreature townsfolk S.alice (Setup.emptyGame S.bothPlayers)
              (goblin, withGoblin) = S.addCreature piker S.alice withBearer
              (undead, withUndead) = S.addCreature zombie S.alice withGoblin
              withLands = List.foldl' (\gs _ -> snd (S.addCreature swamp S.alice gs)) withUndead [1 :: Int .. 3]
              (withSpell, spell) = S.handOne interment withLands
              afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
              resolved = S.runPure S.identityAnswer afterCast Stack.resolveTop
              settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
              after = resolveWholeStack settled
          Spec.assertEqWith s "the Townsfolk is a 5/5: one +1/+1 counter per clause" (S.powerToughnessOf bearer after) (Just (5, 5))
          Spec.assertEqWith s "it was a 3/3 before the spell" (S.powerToughnessOf bearer withLands) (Just (3, 3))
          Spec.assertEqWith s "both creatures left the battlefield" (fmap (\oid -> Game.lookupObject oid resolved) [goblin, undead]) [Nothing, Nothing]
          Spec.assertEqWith s "in two event groups, both inside one scan" (length (deathGroups resolved)) 2
          Spec.assertEqWith s "so two triggers reached the stack" (length (GameState.stack settled)) 2
        -- "YOU CONTROL", the ControlledBy arm (CR 109.5 against the ability's
        -- controller, CR 603.3a). The pair to the first board, differing in exactly
        -- one thing: the same fixture, the same settle, and only bob's Piker
        -- damaged. Its own board cannot prove this and the first board cannot
        -- either -- once the batch is deduped to one trigger, admitting bob's
        -- creature alongside alice's two changes no counter.
        Spec.it s "CR 109.5 you control: a batch holding only an opponent's creature fires nothing" $ do
          (bearer, _, _, _, bobs, board) <- townsfolkBoard
          let settled = S.runPure S.identityAnswer (S.markDamage bobs lethal board) Engine.settleForPriority
              after = resolveWholeStack settled
          Spec.assertEqWith s "the Townsfolk is still a 3/3 once the stack is drained" (S.powerToughnessOf bearer after) (Just (3, 3))
          Spec.assertEqWith s "though bob's Piker did die" (Game.lookupObject bobs settled) Nothing
          Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack settled)) 0
        -- "OTHER", the Filter's Not IsSource arm, and the pair to the board below:
        -- the Townsfolk's own death IS a creature alice controls dying, so the
        -- silence has to come from the exclusion.
        --
        -- The stack rather than a counter, here and below, for the same reason: this
        -- card's payload acts on itself and the bearer is gone, so "the ability
        -- triggered" is the whole of what a board can show.
        Spec.it s "CR 603.6c other: the Townsfolk dying alone fires nothing" $ do
          (bearer, _, _, _, _, board) <- townsfolkBoard
          let settled = S.runPure S.identityAnswer (S.markDamage bearer 3 board) Engine.settleForPriority
          Spec.assertEqWith s "the Townsfolk died" (Game.lookupObject bearer settled) Nothing
          Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack settled)) 0
        -- CR 603.10a's own Example, on the batch reading: the bearer swept up in the
        -- very batch it is watching still sees its group-mates, because the rule
        -- reads "the appearance of objects immediately prior to the event" and both
        -- were standing then. One damaged Piker apart from the board above, so the
        -- two differ in exactly one thing.
        --
        -- The stack rather than a counter, and necessarily: this card's payload acts
        -- on itself, and there is no permanent left to carry one.
        Spec.it s "CR 603.10a the Townsfolk dying in the batch still sees the Piker beside it" $ do
          (bearer, aliceFirst, _, _, _, board) <- townsfolkBoard
          let settled = S.runPure S.identityAnswer (S.markDamage bearer 3 (S.markDamage aliceFirst lethal board)) Engine.settleForPriority
          Spec.assertEqWith s "one trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "both died" (fmap (\oid -> Game.lookupObject oid settled) [bearer, aliceFirst]) [Nothing, Nothing]
          Spec.assertEqWith s "in one event group" (length (deathGroups settled)) 1
        -- The condition is the card's rather than this spec's, as it is for Meren:
        -- what the three boards above played out is the printed Filter, and a
        -- transcription that drifted to PermanentDies would answer 5/5 above.
        Spec.it s "Vengeful Townsfolk's printed condition is PermanentsDie over another creature you control" $ do
          townsfolk <- S.printingOf s registry "Vengeful Townsfolk"
          Spec.assertEqWith
            s
            "one triggered ability, batch-scoped"
            (fmap TriggeredAbility.condition (Face.triggeredAbilities (S.combinedFace townsfolk)))
            [ TriggerCondition.PermanentsDie
                ( Filter.Type.And
                    [ Filter.Type.HasCardType CardType.Creature,
                      Filter.Type.ControlledBy PlayerRelation.You,
                      Filter.Type.Not Filter.Type.IsSource
                    ]
                )
            ]

-- Meren of Clan Nel Toth's SECOND ability, the half permanentDiesSpec above does
-- not cover: "At the beginning of your end step, choose target
-- creature card in your graveyard. If that card's mana value is less than or
-- equal to the number of experience counters you have, return it to the
-- battlefield. Otherwise, put it into your hand."
--
-- A CR 603.2b step trigger on CR 513's end step, a CR 115.2 clause (a) target in
-- a graveyard (Raise Dead's pool), and a destination that depends on a
-- comparison. That last part is the reason this card sat half-transcribed: the
-- ISA has no branch, and #614 proposed a purpose-built two-destination opcode
-- for it. It needs no such opcode. CR 608.2e's clause is already the unit a condition
-- rides (Pawl.Types.Clause.condition), so the printed sentence is TWO clauses of
-- one mode sharing one target slot, each gated by one half of the comparison --
-- and Resolve.gateHolds reads each gate as its own clause is applied (CR
-- 608.2c's written order), never once up front.
--
-- The two gates are exclusive by ARITHMETIC rather than by negation: CR 202.3's
-- mana value and a counter tally are whole numbers, so "greater than n" is
-- "at least n + 1", which is how Pawl.Engine.Keyword.evolve spells rule 702.100a's
-- own strict comparison. Pawl.Types.Condition has no Not, and wanting one here
-- would be the wrong instinct anyway: Condition.holds reads an unanswerable
-- quantity as False, so a negated gate would fire on the very board where the
-- first clause had already moved the card out from under it.
--
-- That same ordering makes the "+ 1" itself UNOBSERVABLE, and no case below
-- proves it: on the one board where strict and non-strict differ -- an equal
-- mana value and count -- the first clause has already moved the card, so the
-- second clause's slot names an id CR 400.7 retired and the move is a no-op
-- either way. Dropping the "+ 1" leaves this whole group green. It is written
-- strictly because that is what the card says, not because a case fences it.
--
-- The cases below vary ONE thing, the experience-counter count -- 3, then 1, then
-- 2 -- over one board and one target of mana value 2, and the branches land the
-- card in different ZONES, so no arithmetic slip can make one read as the other.
-- On the count-of-3 board every other number differs from it: Meren's own mana
-- value is 4, the Thragtusk beside the target in the graveyard is 5, the Bonded
-- Construct on the battlefield is 1, and alice controls 2 creatures -- so a gate
-- reading the SOURCE's mana value, the wrong slot's, or the board's population
-- rather than the counters lands in a zone that case does not accept. The
-- count-of-1 board is the otherwise branch and the count-of-2 board the printed
-- boundary.
merenEndStepSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
merenEndStepSpec s registry =
  let experienceOf = S.playerCounterOf PlayerCounterKind.Experience
      pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
      -- alice's end step, staged directly as Pawl.EventTriggerSpec's
      -- ezuriExperienceSpec stages its beginning of combat: Engine.runStep is what writes the CR 603.2b
      -- StepBegan record this trigger matches.
      atEndStep pid gs =
        gs
          { GameState.phase = Phase.Ending EndingStep.EndStep,
            GameState.activePlayer = pid,
            GameState.priority = Just pid
          }
      -- alice's Meren and a Bonded Construct on the battlefield, her Goblin Piker
      -- (mana value 2) and Thragtusk (mana value 5) in her graveyard, her hand
      -- empty. Everything is ARRANGED rather than cast, so no entry gives out an
      -- experience counter of its own and every counter is one a case put there.
      board = do
        meren <- S.printingOf s registry "Meren of Clan Nel Toth"
        construct <- S.printingOf s registry "Bonded Construct"
        piker <- S.printingOf s registry "Goblin Piker"
        thragtusk <- S.printingOf s registry "Thragtusk"
        let (_, withMeren) = S.addCreature meren S.alice (Setup.emptyGame S.bothPlayers)
            (_, withConstruct) = S.addCreature construct S.alice withMeren
            (pikerId, withPiker) = S.addGraveyardCard piker S.alice withConstruct
            (thragtuskId, gs) = S.addGraveyardCard thragtusk S.alice withPiker
        pure (pikerId, thragtuskId, gs)
      -- The Piker by id rather than by S.identityAnswer's least Recipient, which
      -- would take whichever graveyard card sorts first.
      aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      aimAt oid p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
        _ -> S.identityAnswer p
      -- The one knob: how many experience counters alice has as her end step
      -- begins. Everything else about the two boards is the same object graph.
      withExperience n = do
        (pikerId, thragtuskId, base) <- board
        let gs = S.addPlayerCounter PlayerCounterKind.Experience n S.alice base
        pure (pikerId, thragtuskId, gs, S.runPure (aimAt pikerId) (atEndStep S.alice gs) (Engine.runStep >> Engine.priorityLoop))
   in Spec.describe s "Meren of Clan Nel Toth's end step" $ do
        -- The mana value is AT MOST the count, so the first clause's gate holds
        -- and the card is reanimated. The second clause's gate -- 2 at least 4 --
        -- is then false, and the hand stays empty. That assertion is about the
        -- BOARD rather than about the gate: per the note above, a second gate that
        -- held would no-op anyway on the id the first clause retired.
        Spec.it s "CR 603.2b mana value 2 against three experience counters returns the card to the battlefield" $ do
          (pikerId, thragtuskId, before, after) <- withExperience 3
          Spec.assertEqWith s "alice has three experience counters as the step begins" (experienceOf S.alice before) 3
          Spec.assertBool s (notElem pikerId (Game.zoneMembers Zone.Graveyard S.alice after)) "the targeted card left the graveyard (CR 400.7)"
          Spec.assertEqWith s "and a Goblin Piker is on the battlefield under her control" (S.countOnBattlefieldByName pikerName S.alice after) 1
          Spec.assertEqWith s "her hand is still empty, so the otherwise clause did not also fire" (S.handSize S.alice after) 0
          Spec.assertBool s (elem thragtuskId (Game.zoneMembers Zone.Graveyard S.alice after)) "the untargeted Thragtusk stayed in the graveyard"
          Spec.assertEqWith s "and reading the counters did not spend them" (experienceOf S.alice after) 3
        -- The other branch, one counter instead of three: 2 is not at most 1, so
        -- the first gate fails and the second -- 2 is at least 1 + 1 -- carries
        -- the card to the hand instead. The battlefield assertion is what makes
        -- this the OTHER branch rather than a trigger that did nothing.
        Spec.it s "CR 603.2b the same card against one experience counter goes to the hand instead" $ do
          (pikerId, thragtuskId, before, after) <- withExperience 1
          Spec.assertEqWith s "alice has one experience counter as the step begins" (experienceOf S.alice before) 1
          Spec.assertBool s (notElem pikerId (Game.zoneMembers Zone.Graveyard S.alice after)) "the targeted card left the graveyard"
          Spec.assertEqWith s "no Goblin Piker reached the battlefield" (S.countOnBattlefieldByName pikerName S.alice after) 0
          Spec.assertEqWith s "alice's hand holds exactly one card" (S.handSize S.alice after) 1
          Spec.assertEqWith s "and it is the Piker" (S.countByName pikerName S.alice after) 1
          Spec.assertBool s (elem thragtuskId (Game.zoneMembers Zone.Graveyard S.alice after)) "the untargeted Thragtusk stayed in the graveyard"
        -- The BOUNDARY the printed "less than or equal to" names, which neither
        -- case above sits on: the count equals the mana value, and the card is
        -- reanimated rather than bounced. This case alone does not discriminate a
        -- gate that counted the board -- 2 is also alice's creature count, and
        -- deliberately so, since the boundary is what fixes the number -- which is
        -- what the two cases above are for.
        Spec.it s "CR 603.2b an equal mana value and count take the battlefield branch, not the otherwise one" $ do
          (pikerId, _, _, after) <- withExperience 2
          Spec.assertBool s (notElem pikerId (Game.zoneMembers Zone.Graveyard S.alice after)) "the targeted card left the graveyard"
          Spec.assertEqWith s "a Goblin Piker is on the battlefield" (S.countOnBattlefieldByName pikerName S.alice after) 1
          Spec.assertEqWith s "and her hand is empty" (S.handSize S.alice after) 0
        -- CR 115.2 clause (a) and CR 603.3d, which hands a triggered ability's
        -- targeting to CR 601.2c: the choice is a real one. Both
        -- creature cards in alice's own graveyard are offered and nothing else
        -- is, so the cases above are answered by the pinned target rather than by
        -- a pool with one member the engine could not get wrong.
        Spec.it s "CR 115.2 both creature cards in her graveyard are offered, and neither of bob's" $ do
          (pikerId, thragtuskId, base) <- board
          bolt <- S.printingOf s registry "Lightning Bolt"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withBolt) = S.addGraveyardCard bolt S.alice base
              (theirs, gs) = S.addGraveyardCard piker S.bob withBolt
              recordTargets :: Prompt.Prompt r -> State.State [Map.Map SlotName.SlotName (Natural, Set.Set Recipient.Recipient)] r
              recordTargets p = case p of
                Prompt.ChooseTargets _ _ _ sets -> do
                  State.modify' (<> [sets])
                  pure (aimAt pikerId p)
                _ -> pure (aimAt pikerId p)
              (_, offered) =
                State.runState (Engine.runGame recordTargets (atEndStep S.alice gs) (Engine.runStep >> Engine.priorityLoop)) []
          Spec.assertEqWith
            s
            "one target slot, offering exactly the two creature cards in alice's graveyard"
            (fmap (fmap snd . Map.elems) offered)
            [[Set.fromList [Recipient.ToObject pikerId, Recipient.ToObject thragtuskId]]]
          Spec.assertBool s (elem theirs (Game.zoneMembers Zone.Graveyard S.bob gs)) "bob's identical Piker was in his graveyard to be excluded (CR 400.1)"

-- CR 603.6c's FIRST written form, and the whole of its first clause:
-- "Leaves-the-battlefield abilities trigger when a permanent moves from the
-- battlefield to another zone ... written as, but aren't limited to, 'When
-- [this object] leaves the battlefield, . . . .'" ANY other zone -- which is the
-- whole of what separates it from the SECOND written form, the one CR 700.4
-- abbreviates as "dies" and diesTriggerSpec above covers.
--
-- Thragtusk, {4}{G} Creature -- Beast 5/3: "When this creature enters, you gain
-- 5 life. When this creature leaves the battlefield, create a 3/3 green Beast
-- creature token." The two halves of the same card are here because the enters
-- trigger is what proves the leaves trigger is not merely firing on every zone
-- change the permanent is party to.
--
-- Unsummon is what makes CR 400.7e's public-zone proviso a real test rather
-- than one satisfied by construction, which is all it could be while SelfDies
-- was the only look-back condition: a bounced Thragtusk HAS left the
-- battlefield, so the ability triggers, but it went to a hand, which CR 400.2
-- makes a hidden zone -- so "can find the new object that it became in the zone
-- it moved to when the ability triggered, IF THAT ZONE IS A PUBLIC ZONE"
-- withholds Pawl.Engine.Binding.became.
--
-- Doomed Traveler bounced is the regression guard on the other side: the two
-- conditions must not be conflated, so the card that prints "dies" must stay
-- silent for exactly the event that fires the card that prints "leaves the
-- battlefield".
leavesBattlefieldSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
leavesBattlefieldSpec s registry =
  let -- alice: one Mountain (Lightning Bolt's {R}), a Thragtusk in play, and the
      -- Bolt in hand. S.identityAnswer targets the least Recipient, and
      -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
      -- creature on the board is the target without a bespoke interpreter.
      -- Three damage is lethal to a 5/3 (CR 704.5g).
      boltBoard = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        thragtusk <- S.printingOf s registry "Thragtusk"
        let (tusk, withTusk) = S.addCreature thragtusk S.alice (S.landsInPlay mountain 1)
        pure (tusk, S.handOne lightningBolt withTusk)
      -- The same board with a bounce spell instead of a burn spell, for the
      -- creature the caller names. One Island pays Unsummon's {U}.
      bounceBoard printing = do
        island <- S.printingOf s registry "Island"
        unsummon <- S.printingOf s registry "Unsummon"
        victim <- S.printingOf s registry printing
        let (oid, withVictim) = S.addCreature victim S.alice (S.landsInPlay island 1)
        pure (oid, S.handOne unsummon withVictim)
      -- Cast the one spell in hand, resolve it, then settle -- CR 117.5's
      -- boundary is where state-based actions run and the trigger scan sees the
      -- departure -- and hand back both the settled state (the trigger on the
      -- stack) and the state after the trigger itself resolves.
      castIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            resolved = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      beastsOf pid gs =
        filter
          -- CR 111.4: Thragtusk does not specify the token's name, so the name
          -- is its subtype plus the word "Token".
          (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Beast Token"))
          (Game.zoneMembers Zone.Battlefield pid gs)
      assertOneBeast after =
        case beastsOf S.alice after of
          [beast] -> do
            Spec.assertEqWith s "3/3" (Projection.powerOf beast after, Projection.toughnessOf beast after) (Just 3, Just 3)
            -- CR 202.2b/202.2e: a token has no mana cost, so the colour
            -- indicator is the only thing making it green.
            Spec.assertEqWith s "green" (Projection.colorsOf beast after) (Set.singleton Color.Green)
            Spec.assertEqWith s "Beast" (Projection.subtypesOf beast after) (Set.singleton Subtype.Beast)
          other -> Spec.assertFailure s ("expected exactly one Beast token, got " <> show (length other))
      tuskName = CardName.MkCardName $ Text.pack "Thragtusk"
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      -- Every slot stamped on every object currently on the stack, which for
      -- these boards is the one placed trigger.
      stackSlots gs =
        concatMap (Map.toList . Map.mapMaybe Binding.onlyOne . Binding.targetsOf . maybe Map.empty Object.bindings . flip Game.lookupObject gs) (GameState.stack gs)
   in Spec.describe s "LeavesTheBattlefield" $ do
        -- The destination this condition SHARES with "dies", so the wider
        -- condition is not merely the narrower one's complement: a Thragtusk
        -- that dies has also left the battlefield.
        Spec.it s "CR 603.6c whole card: Lightning Bolt kills Thragtusk and its leaves-the-battlefield trigger makes a 3/3 Beast" $ do
          (_, board) <- boltBoard
          let (settled, after) = castIt board
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Set.member tuskName (namesIn Zone.Graveyard S.alice settled)) "the Thragtusk is in the graveyard by then"
          assertOneBeast after
        -- The destination "dies" does NOT reach, and the reason this condition
        -- has to exist at all (CR 700.4 is a graveyard, CR 603.6c is any zone).
        -- The hand is also a HIDDEN zone (CR 400.2), which is what makes the
        -- next case a real branch rather than a proviso.
        Spec.it s "CR 603.6c whole card: Unsummon bounces Thragtusk and the leaves-the-battlefield trigger still makes a 3/3 Beast" $ do
          (_, board) <- bounceBoard "Thragtusk"
          let (settled, after) = castIt board
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Set.member tuskName (namesIn Zone.Hand S.alice settled)) "the Thragtusk is in its owner's hand, not a graveyard"
          Spec.assertBool s (not (Set.member tuskName (namesIn Zone.Graveyard S.alice settled))) "and nowhere near a graveyard"
          assertOneBeast after
        -- CR 400.7e's proviso, which is the new plumbing this condition needed:
        -- "can find the new object that it became in the zone it moved to when
        -- the ability triggered, IF THAT ZONE IS A PUBLIC ZONE." A hand is not
        -- one (CR 400.2), so the slot must be ABSENT -- naming the card in hand
        -- would hand the ability an object the rule does not let it find.
        --
        -- CR 113.7a's source slot is still stamped, and is asserted here so
        -- that the absence above is read as a decision about `became` rather
        -- than as a trigger that was placed with no bindings at all.
        Spec.it s "CR 400.7e a bounce to a HIDDEN zone binds no became slot, though the source slot is still stamped" $ do
          (tusk, board) <- bounceBoard "Thragtusk"
          let (settled, _) = castIt board
              slots = stackSlots settled
          Spec.assertEqWith s "the departed permanent is CR 113.7a's source" (lookup Binding.triggerSource slots) (Just (Recipient.ToObject tusk))
          Spec.assertEqWith s "and CR 400.7e's became is absent for a hidden destination" (lookup Binding.became slots) Nothing
        -- The public destination, side by side with the hidden one: the same
        -- condition, the same card, and the slot IS bound -- so its absence
        -- above is CR 400.7e's proviso doing work rather than the condition
        -- simply never binding anything.
        Spec.it s "CR 400.7e a death to a PUBLIC zone does bind became, for the same condition" $ do
          (tusk, board) <- boltBoard
          let (settled, _) = castIt board
              slots = stackSlots settled
          Spec.assertEqWith s "the departed permanent is still CR 113.7a's source" (lookup Binding.triggerSource slots) (Just (Recipient.ToObject tusk))
          case lookup Binding.became slots of
            Just (Recipient.ToObject graveyardId) -> do
              Spec.assertBool s (graveyardId /= tusk) "became is the CR 400.7 incarnation, a different id"
              Spec.assertEqWith s "and it is the graveyard card" (fmap Face.name (Game.faceOf graveyardId settled)) (Just tuskName)
            other -> Spec.assertFailure s ("expected became to name an object, got " <> show other)
        -- THE REGRESSION GUARD. Doomed Traveler prints "dies", not "leaves the
        -- battlefield", and CR 700.4 makes that a graveyard and nothing else. A
        -- bounce is the event that fires Thragtusk two cases up, so conflating
        -- the two conditions would show up here as a Spirit that should not
        -- exist.
        Spec.it s "CR 700.4 a Doomed Traveler bounced by Unsummon does NOT fire its dies trigger" $ do
          (_, board) <- bounceBoard "Doomed Traveler"
          let (settled, _) = castIt board
          Spec.assertBool s (Set.member (CardName.MkCardName $ Text.pack "Doomed Traveler") (namesIn Zone.Hand S.alice settled)) "the Traveler left the battlefield for a hand"
          Spec.assertEqWith s "and nothing triggered" (length (GameState.stack settled)) 0
          Spec.assertEqWith s "so no Spirit was made" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Spirit Token") S.alice settled) 0
        -- CR 400.7e's proviso in isolation, one case per zone CR 400.2
        -- classifies, so the branch is pinned to the RULE rather than to the two
        -- destinations the boards above happen to reach. "Graveyard,
        -- battlefield, stack, exile, ante, and command are public zones ...
        -- Library and hand are hidden zones."
        Spec.it s "CR 400.2 eventBindings binds became for every PUBLIC destination and for no hidden one" $ do
          let departed = ObjectId.MkObjectId 1
              arrived = ObjectId.MkObjectId 2
              leftFor to = Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice TriggerCondition.SelfLeavesTheBattlefield (GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield to) S.emptyCharacteristics))
              bound = Map.singleton Binding.became (Binding.toObject arrived)
          Spec.assertEqWith s "a graveyard is public" (leftFor Zone.Graveyard) bound
          Spec.assertEqWith s "exile is public" (leftFor Zone.Exile) bound
          Spec.assertEqWith s "the stack is public" (leftFor Zone.Stack) bound
          Spec.assertEqWith s "the command zone is public" (leftFor Zone.Command) bound
          Spec.assertEqWith s "a hand is hidden" (leftFor Zone.Hand) Map.empty
          Spec.assertEqWith s "a library is hidden" (leftFor Zone.Library) Map.empty
        -- CR 603.6c's "to ANOTHER zone", which is the one destination this
        -- condition rejects. Pawl.Engine.Event.recordTokenEntry files a
        -- battlefield-to-battlefield pseudo-move whose departed id is the new
        -- token's own, so a token bearing this ability would fire on its own
        -- creation if the guard were dropped -- and no card in the pool makes
        -- such a token, which is exactly why the guard needs a test of its own.
        Spec.it s "CR 603.6c a battlefield-to-battlefield pseudo-move is not a departure" $ do
          let token = ObjectId.MkObjectId 1
              entry = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange token token Zone.Battlefield Zone.Battlefield) S.emptyCharacteristics)
              gone = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange token (ObjectId.MkObjectId 2) Zone.Battlefield Zone.Exile) S.emptyCharacteristics)
              matches = Event.matchesTrigger (Setup.emptyGame S.bothPlayers) token S.alice TriggerCondition.SelfLeavesTheBattlefield
          Spec.assertBool s (not (matches entry)) "a token's own entry is not a departure"
          Spec.assertBool s (matches gone) "but the same token being exiled is"
        -- The card's other half, and the proof that the leaves trigger is
        -- scoped to DEPARTURES: Thragtusk arriving on the battlefield is a zone
        -- change involving the same permanent, and only the enters trigger may
        -- see it.
        Spec.it s "CR 603.6a whole card: casting Thragtusk gains 5 life, and its leaves trigger stays silent on the way in" $ do
          forest <- S.printingOf s registry "Forest"
          thragtusk <- S.printingOf s registry "Thragtusk"
          let (settled, after) = castIt (S.handOne thragtusk (S.landsInPlay forest 5))
          Spec.assertEqWith s "exactly one trigger, the enters one" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "alice is still at 20 until it resolves" (S.lifeOf S.alice settled) (Just 20)
          Spec.assertEqWith s "and at 25 once it does" (S.lifeOf S.alice after) (Just 25)
          Spec.assertEqWith s "with no Beast token anywhere" (length (beastsOf S.alice after)) 0

-- The events a trigger condition GENUINELY fires on (Event.matchesTrigger's own
-- arms are the spec), so eventBindings is exercised through its matching arm
-- rather than through its `_ -> Map.empty` fallthrough. A pair that did not
-- match would pin nothing: both sides would read empty for every condition.
--
-- The INHERENT conditions are the exception, and it is the rules' rather than an
-- oversight: CR 725.2's and CR 702.179d's abilities hang on no card, so
-- Event.matchesTrigger answers False for them whatever the event and their real
-- matchers live in Pawl.Engine.Monarch and Pawl.Engine.Speed. Their arms below
-- name the event the RULE names anyway. What the pin says of those two is
-- therefore weaker but not vacuous -- that neither claims a slot the log could
-- never bind -- and it is what fails if either grows a binding arm here without
-- eventBindingSlots being told.
--
-- A NON-EMPTY LIST rather than one event, because Event.eventBindingSlots
-- answers the guaranteed FLOOR -- the slots bound for every event a condition
-- admits -- and a condition that binds a slot for some of its events and not
-- others cannot be pinned by any single one of them. Most conditions are
-- represented by a one-element list, for which the floor is that event's exact
-- keyset. The exceptions are SelfLeavesTheBattlefield, whose two destinations
-- differ because CR 400.7e binds `became` for the public one and withholds it
-- for the hidden one (CR 400.2); SelfIsDealtDamage, which admits both of CR
-- 120.3's damage kinds; and SelfBecomesBlockedByOneOrMore and
-- CreatureBecomesBlockedByAtLeast, which each admit rule 509.3e's two producers
-- under two different event constructors.
--
-- Exhaustive with no wildcard, which is half of what keeps the pin honest -- a
-- new TriggerCondition fails to compile here. The other half, the list below, is
-- hand-kept and cannot be forced; add the new constructor there too.
--
-- The departing id every Moved event here carries, shared with the pin so that
-- the state it hands Event.eventBindings can file CR 608.2h's record under it.
-- Nothing is ever stocked under the ARRIVING id, deliberately: see the pin.
representativeDeparted :: ObjectId.ObjectId
representativeDeparted = ObjectId.MkObjectId 1

-- A triggered ability with this condition and no payload, for the representative
-- GameEvent.AbilityTriggered below. The record names the ABILITY beside its
-- source, and that arm's floor does not depend on what the ability does.
bareAbility :: TriggerCondition.TriggerCondition -> TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)
bareAbility condition =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = condition,
      TriggeredAbility.modal = Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }

representativeEvents :: TriggerCondition.TriggerCondition -> NonEmpty.NonEmpty GameEvent.GameEvent
representativeEvents cond =
  let departed = representativeDeparted
      arrived = ObjectId.MkObjectId 2
      moved from to = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange departed arrived from to) S.emptyCharacteristics)
      combatDamage =
        GameEvent.DamageDealt
          (DamageEvent.MkDamageEvent departed (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Combat)
      one e = e NonEmpty.:| []
      -- CR 614.5's identity for a shield the BEARER's own resolution installed,
      -- which is what "prevented this way" compares against.
      preventedByBearer = CandidateId.OfFloating (FloatingCandidate.MkFloatingCandidate departed (Timestamp.MkTimestamp 1))
   in case cond of
        TriggerCondition.SelfEnters -> one (moved Zone.Stack Zone.Battlefield)
        TriggerCondition.PermanentEnters _ -> one (moved Zone.Stack Zone.Battlefield)
        TriggerCondition.StepBegins (StepBegins.MkStepBegins phase _) -> one (GameEvent.StepBegan (StepBegan.MkStepBegan phase S.alice))
        -- CR 603.8: a state trigger matches a game STATE, so no log entry fires
        -- it at all (Event.matchesTrigger's StateIs arm answers False for every
        -- event). Any event is therefore as representative as any other.
        TriggerCondition.StateIs _ -> one (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice))
        TriggerCondition.SelfDealsCombatDamageToPlayer -> one combatDamage
        -- CR 120.3's event pointed the other way, at the BEARER -- so the pair
        -- really matches. TWO of them, combat and noncombat, because this
        -- condition is the one damage arm that admits both: a floor claimed for
        -- one kind and not the other would come apart here.
        TriggerCondition.SelfIsDealtDamage ->
          GameEvent.DamageDealt (DamageEvent.MkDamageEvent arrived (Recipient.ToCreature departed) 2 False False False 0 Nothing DamageKind.Noncombat)
            NonEmpty.:| [GameEvent.DamageDealt (DamageEvent.MkDamageEvent arrived (Recipient.ToCreature departed) 3 False False False 0 Nothing DamageKind.Combat)]
        -- The same event read by a bystander, and the only one this condition
        -- admits.
        TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> one combatDamage
        -- The same one event, and NOT because the batch reading matches nothing:
        -- Event.matchesTrigger answers alike for both (its batch arm delegates to
        -- the singular's), CR 603.2c's once-per-step scoping living in
        -- Event.eventTriggers instead -- PermanentsDie's posture below.
        TriggerCondition.PermanentsDealCombatDamageToPlayer _ -> one combatDamage
        TriggerCondition.CreatureDealtCombatDamageToMonarch -> one combatDamage
        -- CR 726.2's pair, matched by Pawl.Engine.Initiative.inherentPending rather
        -- than by Event.matchesTrigger, the monarch's condition above's posture.
        TriggerCondition.CreaturesDealtCombatDamageToInitiative -> one combatDamage
        TriggerCondition.PlayerTookInitiative -> one (GameEvent.TookInitiative S.bob)
        -- CR 702.179d's own event. Like the monarch's condition above it, this
        -- one is matched by Pawl.Engine.Speed.inherentPending rather than by
        -- Event.matchesTrigger, which answers False for it whatever the event --
        -- so the pin here is that an inherent condition binds nothing from the
        -- log, which is what Event.eventBindingSlots claims for it.
        TriggerCondition.OpponentLostLifeDuringYourTurn -> one (GameEvent.LifeLost (LifeChange.MkLifeChange S.bob 2))
        TriggerCondition.SelfCycled -> one (GameEvent.Discarded (Discarded.MkDiscarded S.alice departed DiscardCause.ToPayCyclingCost))
        -- CR 702.94a's cause, so the event is one this condition genuinely
        -- admits; an Ordinary reveal would pin nothing.
        TriggerCondition.SelfRevealedForMiracle -> one (GameEvent.Revealed (Revealed.MkRevealed S.alice departed RevealCause.ForMiracle S.emptyCharacteristics))
        -- BOTH causes, which is this condition's whole difference from
        -- SelfCycled above: CR 702.29a makes cycling a discard, so an ordinary
        -- discard and a cycle are each an event it genuinely admits, and an arm
        -- that read the cause would pin nothing for one of them.
        TriggerCondition.SelfDiscarded ->
          GameEvent.Discarded (Discarded.MkDiscarded S.alice departed DiscardCause.Ordinary)
            NonEmpty.:| [GameEvent.Discarded (Discarded.MkDiscarded S.alice departed DiscardCause.ToPayCyclingCost)]
        TriggerCondition.PlayerDiscards _ -> one (GameEvent.Discarded (Discarded.MkDiscarded S.alice departed DiscardCause.Ordinary))
        -- The CYCLING cause, which is the only one this condition admits -- an
        -- Ordinary discard is an event it rejects, and eventBindings is consulted
        -- only for a match, so it would pin nothing.
        TriggerCondition.PlayerCycles _ -> one (GameEvent.Discarded (Discarded.MkDiscarded S.alice departed DiscardCause.ToPayCyclingCost))
        -- The ordinal matches the condition's own, so the event is one this
        -- condition genuinely admits -- an event it rejected would pin nothing,
        -- eventBindings being consulted only for a match.
        TriggerCondition.PlayerDrawsNthCard (PlayerDrawsNthCard.MkPlayerDrawsNthCard _ nth) -> one (GameEvent.Drew (Drew.MkDrew S.alice nth))
        -- CR 508.5's defending player, and deliberately NOT the attacker's own
        -- controller: eventBindings binds this field under `thatPlayer`, so an
        -- arm that bound the attacking side instead would still agree with
        -- eventBindingSlots here if the two coincided.
        TriggerCondition.SelfAttacks _ -> one (GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared departed S.carol 1))
        -- The same declaration event. This one binds NOTHING off it, which is
        -- what eventBindingSlots claims and what the pin here checks -- the
        -- defending player the event carries is not rule 702.149a's to read.
        TriggerCondition.SelfAttacksWithAnother _ -> one (GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared departed S.carol 1))
        -- The same declaration event, with the count that makes it CR 506.5's
        -- alone -- and the ATTACKER is what this one binds, where SelfAttacks
        -- above binds the defending player off the very same event.
        TriggerCondition.CreatureAttacksAlone _ -> one (GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared departed S.carol 1))
        -- The same declaration event again, and the ATTACKER is what this one
        -- binds too -- CR 508.5's defending player is the thing it MATCHES on, so
        -- an arm that bound that player instead would disagree with
        -- eventBindingSlots here.
        TriggerCondition.CreatureAttacksYou -> one (GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared departed S.carol 1))
        -- The GROUPED declaration event instead, which is CR 508.3b's arity: one
        -- per target the declaration named. carol again, and it is the PLAYER this
        -- one binds -- the arm above binds the attacker off its own event, and the
        -- pin is what keeps the two from drifting together.
        TriggerCondition.AttachedPlayerIsAttacked -> one (GameEvent.BecameAttacked (BecameAttacked.MkBecameAttacked S.bob (AttackTarget.OfPlayer S.carol)))
        -- CR 508.3d's third arity: the once-per-DECLARATION event, naming the
        -- player who declared. Nothing is bound off it -- rule 508.3d names a set
        -- of creatures rather than one -- so this pins an EMPTY floor, which is
        -- the pin the two arms above would break if either grew a binding here.
        -- carol serves every relation, the floor being empty either way.
        TriggerCondition.PlayerAttacks _ -> one (GameEvent.AttackersDeclared S.carol)
        -- The same declaration event once more, and the same EMPTY floor: rule
        -- 508.3c's Filter narrows the creatures rather than naming one, so there
        -- is nothing to bind here either.
        TriggerCondition.PlayerAttacksWith {} -> one (GameEvent.AttackersDeclared S.carol)
        -- The GROUPED event once more, and the same PLAYER slot the arm three
        -- above pins -- CR 508.3e's ATTACKED player, which is bob here and not
        -- the carol who declared, so an arm that bound the attacking side
        -- instead would disagree with eventBindingSlots on this event.
        TriggerCondition.PlayerAttacksPlayer {} -> one (GameEvent.BecameAttacked (BecameAttacked.MkBecameAttacked S.carol (AttackTarget.OfPlayer S.bob)))
        -- The same declaration event once more. Rule 702.105a binds NOTHING off
        -- it, SelfAttacksWithAnother's case: the player it compares is read from
        -- Combat.attackers and then never named again.
        TriggerCondition.SelfAttacksPlayerWithMostLife -> one (GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared departed S.carol 1))
        -- The GROUPED blocking event, which is CR 509.3a's arity: one per blocking
        -- creature, whatever it was declared against.
        TriggerCondition.SelfBlocks -> one (GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared departed 1))
        -- The same event with the count read, which is all CR 509.3e adds.
        TriggerCondition.SelfBlocksAtLeast _ -> one (GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared departed 2))
        -- The same grouped event once more, with the count IGNORED: CR 509.3e's
        -- filtered form reads the attackers off Combat.blockers instead, and
        -- binds nothing off the log.
        TriggerCondition.SelfBlocksOneOrMore _ -> one (GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared departed 1))
        -- The PAIRWISE event instead: CR 509.3b's bearer is the BLOCKER too, and
        -- the attacker beside it is what this one binds.
        TriggerCondition.SelfBlocksCreature _ -> one (GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = departed, BecameBlocking.attacker = ObjectId.MkObjectId 41, BecameBlocking.putOntoBattlefield = False, BecameBlocking.attackerWasBlocked = False, BecameBlocking.blockersBefore = Set.empty}))
        -- CR 508.5's defending player again, and carol for SelfAttacks' reason
        -- above: eventBindings binds this field under `thatPlayer`.
        TriggerCondition.SelfBecomesBlocked -> one (GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked departed S.carol 1))
        -- The same declaration event SelfBlocks names, with the ids the other way
        -- round: this condition's bearer is the ATTACKER, and the blocker is what
        -- it binds.
        TriggerCondition.SelfBecomesBlockedBy _ -> one (GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = ObjectId.MkObjectId 41, BecameBlocking.attacker = departed, BecameBlocking.putOntoBattlefield = False, BecameBlocking.attackerWasBlocked = False, BecameBlocking.blockersBefore = Set.empty}))
        TriggerCondition.PermanentBecomesBlockedBy _ -> one (GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = ObjectId.MkObjectId 41, BecameBlocking.attacker = departed, BecameBlocking.putOntoBattlefield = False, BecameBlocking.attackerWasBlocked = False, BecameBlocking.blockersBefore = Set.empty}))
        -- The GROUPED attacking-side event, which is what makes this one fire
        -- once where the arm above fires per blocker. carol on SelfBecomesBlocked's
        -- reasoning -- and this one binds that player nothing, which is the
        -- difference the pin catches.
        --
        -- The SECOND list longer than one, and for the same reason as
        -- CreatureBecomesBlockedByAtLeast below: rule 509.3e's second producer,
        -- the arrival of a creature put onto the battlefield blocking an
        -- attacker that was already blocked, is admitted under a different event
        -- constructor. This condition names a SET rather than an object, so both
        -- events bind nothing and the intersection is empty -- which is what an
        -- eventBindings arm added here without eventBindingSlots being told
        -- would break.
        TriggerCondition.SelfBecomesBlockedByOneOrMore _ ->
          GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked departed S.carol 1)
            NonEmpty.:| [GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = ObjectId.MkObjectId 42, BecameBlocking.attacker = departed, BecameBlocking.putOntoBattlefield = True, BecameBlocking.attackerWasBlocked = True, BecameBlocking.blockersBefore = Set.singleton (ObjectId.MkObjectId 43)})]
        -- The same grouped event once more, with the ids read the other way from
        -- every arm above it: the bearer is a BYSTANDER, so `departed` sits in
        -- the attacker position and is what this one binds -- an arm that bound
        -- the bearer instead would pin the empty set here.
        --
        -- The LAST list longer than one, and rule 509.3e's second producer is
        -- why: this condition also admits the arrival of a creature put onto the
        -- battlefield blocking, which names the attacker in a different payload
        -- and so needs its own eventBindings arm. An arm added to matchesTrigger
        -- and forgotten there would bind nothing for this event, Seifer's "that
        -- attacking creature" would resolve to nothing, and the board would be
        -- indistinguishable from one where the trigger never fired -- so the
        -- intersection is the fence for exactly that.
        TriggerCondition.CreatureBecomesBlockedByAtLeast {} ->
          GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked departed S.carol 1)
            NonEmpty.:| [GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = ObjectId.MkObjectId 42, BecameBlocking.attacker = departed, BecameBlocking.putOntoBattlefield = True, BecameBlocking.attackerWasBlocked = True, BecameBlocking.blockersBefore = Set.singleton (ObjectId.MkObjectId 43)})]
        -- The same declaration's unblocked branch, which carries the attacker
        -- and nothing else -- so the floor it pins is the empty set.
        TriggerCondition.SelfAttacksUnblocked -> one (GameEvent.AttackerUnblocked departed)
        TriggerCondition.SelfPutIntoGraveyardFromLibrary -> one (moved Zone.Library Zone.Graveyard)
        -- Every origin zone is admitted, but the floor is the same for all of
        -- them: the destination is always a graveyard, which CR 400.2 makes
        -- public, so CR 400.7e never withholds anything and one event says as
        -- much as any list would.
        TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> one (moved Zone.Hand Zone.Graveyard)
        -- BOTH events this condition admits, which is the whole reason it
        -- exists: CR 712.21's second card is announced by a CardArrived event
        -- rather than a Moved one, and the floor has to hold for each.
        TriggerCondition.CardPutIntoGraveyard _ ->
          moved Zone.Hand Zone.Graveyard NonEmpty.:| [GameEvent.CardArrived (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard)]
        TriggerCondition.SelfDies -> one (moved Zone.Battlefield Zone.Graveyard)
        TriggerCondition.PermanentDies _ -> one (moved Zone.Battlefield Zone.Graveyard)
        -- The same one event, and NOT because the batch reading matches nothing:
        -- Event.matchesTrigger answers alike for both (its PermanentsDie arm
        -- delegates to PermanentDies'), CR 603.2c's once-per-batch scoping living
        -- in Event.eventTriggers instead. So the floor this pin asserts is the
        -- same floor, read off the same event.
        TriggerCondition.PermanentsDie _ -> one (moved Zone.Battlefield Zone.Graveyard)
        -- CR 603.6c admits every destination, and CR 400.2 splits them into
        -- public and hidden, so both sides of CR 400.7e's proviso have to be
        -- here for the floor to be the honest answer. Rule 603.6c's second
        -- trigger event is the third: CR 800.4a's departure reaches no zone at
        -- all, so there is no arriving object for CR 400.7e to offer and it
        -- binds nothing -- which is what keeps the floor empty.
        TriggerCondition.SelfLeavesTheBattlefield ->
          moved Zone.Battlefield Zone.Graveyard NonEmpty.:| [moved Zone.Battlefield Zone.Hand, GameEvent.LeftTheGame departed]
        -- The bystander reading of the arm above, whose three events are the same
        -- three. Its CR 400.7e arrival is withheld for the same two reasons, but
        -- the floor is NOT empty: CR 603.10a's departed permanent is bound by
        -- every one of the three, which is what these events pin.
        TriggerCondition.PermanentLeavesTheBattlefield _ ->
          moved Zone.Battlefield Zone.Graveyard NonEmpty.:| [moved Zone.Battlefield Zone.Hand, GameEvent.LeftTheGame departed]
        -- The one destination the arm above's three events narrow to, and the
        -- only event this condition admits at all: CR 400.2 makes a hand hidden,
        -- so CR 400.7e withholds the arrival; what the floor holds is the
        -- departed permanent and CR 400.3's owner, read off CR 608.2h's record
        -- of `departed` in the pin's state.
        TriggerCondition.PermanentReturnedToHand _ -> one (moved Zone.Battlefield Zone.Hand)
        -- The same one event, and NOT because the batch reading matches nothing:
        -- Event.matchesTrigger answers alike for both (its batch arm delegates to
        -- the singular's), CR 603.2c's once-per-batch scoping living in
        -- Event.eventTriggers instead -- PermanentsDie's posture below.
        TriggerCondition.PermanentsReturnedToHand _ -> one (moved Zone.Battlefield Zone.Hand)
        -- The zone read on the DEPARTURE side instead: every destination is
        -- admitted, and the floor is the same for all of them, so one event says
        -- as much as a list would -- SelfPutIntoGraveyardFromAnywhere's reasoning
        -- pointed the other way. Empty either way: this condition binds nothing.
        TriggerCondition.CardLeavesGraveyard {} -> one (moved Zone.Graveyard Zone.Exile)
        -- SelfDies' event, since CR 700.4 is the same word: the haunted creature
        -- is put into a graveyard from the battlefield. Which permanent it is
        -- rides GameState.haunting rather than the event, so one event says all
        -- there is to say here.
        TriggerCondition.HauntedCreatureDies -> one (moved Zone.Battlefield Zone.Graveyard)
        TriggerCondition.SpellOrAbilityCounters _ ->
          one (GameEvent.SpellCountered (Countering.MkCountering departed arrived S.alice))
        -- CR 615.13: the recipient has to be a PLAYER, this condition being
        -- scoped to damage that would be dealt to one -- an event naming a
        -- creature matches nothing and would pin the floor at empty.
        TriggerCondition.DamageToPlayerPrevented _ -> one (GameEvent.DamagePrevented (DamagePrevented.MkDamagePrevented preventedByBearer arrived (Map.singleton (Recipient.ToPlayer S.bob) 2)))
        -- The same event read for its IDENTITY instead: rule 615.13's "this way"
        -- needs the prevention to be the BEARER's own, and an event naming
        -- another object matches nothing and would pin the floor at empty. The
        -- condition's own Filter is the trivial one in everyTriggerCondition
        -- below, so the damage's source may be any id.
        TriggerCondition.SelfPreventsDamage _ -> one (GameEvent.DamagePrevented (DamagePrevented.MkDamagePrevented preventedByBearer arrived (Map.singleton (Recipient.ToCreature departed) 2)))
        -- CR 119.9's own event, and the only one this condition admits: the
        -- payload is a player and an amount, and the amount is the floor.
        TriggerCondition.PlayerGainsLife _ -> one (GameEvent.LifeGained (LifeChange.MkLifeChange S.bob 2))
        -- The same event, the batch reading admitting exactly what the per-seat
        -- one does: the two differ in Event.batchScoped, never in the matcher.
        TriggerCondition.PlayersGainLife _ -> one (GameEvent.LifeGained (LifeChange.MkLifeChange S.bob 2))
        -- The loss condition's own event, and the only one it admits, on the
        -- gain arm's reasoning: same payload shape, same amount floor.
        TriggerCondition.PlayerLosesLife _ -> one (GameEvent.LifeLost (LifeChange.MkLifeChange S.bob 2))
        -- CR 714.2b: a placement on the BEARER that crosses the chapter. The
        -- bearer here is `departed`, the id Event.matchesTrigger is asked about
        -- below, and the counts straddle N so the event really matches.
        TriggerCondition.SelfCountersReached (SelfCountersReached.MkSelfCountersReached kind n) -> one (GameEvent.CountersPut (CounterChange.MkCounterChange departed kind 0 n))
        -- CR 716.2a's own event, on `departed` and not the bearer, the arm above's
        -- shape: the pair does not match, which is what pins the floor.
        TriggerCondition.SelfBecomesClassLevel n -> one (GameEvent.ClassLevelSet (ClassLevelChange.MkClassLevelChange departed (ClassLevel.MkClassLevel 1) n))
        -- CR 310.12b: a removal on the BEARER that took the last counter, so the
        -- event really matches the condition Event.matchesTrigger is asked about.
        TriggerCondition.SelfLastCounterRemoved kind -> one (GameEvent.CountersRemoved (CounterChange.MkCounterChange departed kind 1 0))
        -- Its any-amount mirror, on a pair that does NOT reach zero -- so an
        -- implementation that had cased on `after` would find no match here and the
        -- eventBindingSlots pin below would see nothing stamped.
        TriggerCondition.SelfCountersRemoved kind -> one (GameEvent.CountersRemoved (CounterChange.MkCounterChange departed kind 3 1))
        -- CR 603.2c's batch placement, on the same event the chapter arm above
        -- names and read the other way round: the id is the SUBJECT the Filter is
        -- applied to rather than the bearer. The Filter below is the trivial one,
        -- so what decides the match is whether the subject can be viewed at all --
        -- and the floor is empty either way, a batch condition binding nothing.
        TriggerCondition.PermanentsGetCounters (CounterPlacement.MkCounterPlacement kind _) -> one (GameEvent.CountersPut (CounterChange.MkCounterChange departed kind 0 1))
        -- Its per-permanent scope, on the same event: the two share a payload and a
        -- matcher, and what parts them here is the floor -- this one names ONE
        -- permanent, so `became` is stamped and claimed where the batch above
        -- claims nothing.
        TriggerCondition.PermanentGetsCounters (CounterPlacement.MkCounterPlacement kind _) -> one (GameEvent.CountersPut (CounterChange.MkCounterChange departed kind 0 1))
        -- CR 601.2i's own event, and the only one this condition admits. Both
        -- halves are bound whichever ids the event names -- the spell under
        -- `thatSpell`, the caster under `thatPlayer` -- so the two sides agree
        -- on the pair.
        TriggerCondition.SpellCast {} -> one (GameEvent.SpellCast (SpellWasCast.MkSpellWasCast S.alice arrived S.emptyCharacteristics (Just Zone.Hand)))
        -- The same event, and the only one this condition admits either. It binds
        -- nothing whichever ids the event names, since the spell IS the bearer.
        TriggerCondition.SelfCast -> one (GameEvent.SpellCast (SpellWasCast.MkSpellWasCast S.alice arrived S.emptyCharacteristics (Just Zone.Hand)))
        -- CR 601.2c's event, with the bearer as the targeted object and alice as
        -- the targeting object's controller -- an event BOTH relations admit,
        -- since matchesTrigger reads `you` from the bearer's side and this list
        -- pins the binding rather than the match.
        TriggerCondition.SelfBecomesTargeted _ -> one (GameEvent.BecameTarget (BecameTarget.MkBecameTarget (Recipient.ToObject departed) arrived StackObjectKind.Ability S.alice))
        -- The same event one recipient over: a targeted PLAYER, which is the axis
        -- separating this condition from the arm above. The kind is taken FROM
        -- the condition so each inhabitant listed below gets an event it
        -- genuinely admits -- Dormant Gomazoa's Just Spell and Amulet of
        -- Safekeeping's Nothing, for which Spell is as representative as Ability.
        -- The controller is bob, an opponent of the targeted alice.
        TriggerCondition.ControllerBecomesTarget c -> one (GameEvent.BecameTarget (BecameTarget.MkBecameTarget (Recipient.ToPlayer S.alice) arrived (Maybe.fromMaybe StackObjectKind.Spell (ControllerBecomesTarget.kind c)) S.bob))
        -- CR 709.5h's own event, on the BEARER and naming the same door the
        -- condition does, so the pair really matches -- the door below is the one
        -- everyTriggerCondition names.
        TriggerCondition.SelfHalfUnlocked half -> one (GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked departed S.alice half False))
        -- CR 709.5i's own event, with the flag SET and alice as the player who
        -- unlocked -- an unset flag, or an actor the relation refuses, matches
        -- nothing, and would pin the floor against an event this condition does
        -- not admit.
        TriggerCondition.RoomFullyUnlocked _ -> one (GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked departed S.alice (CardName.MkCardName (Text.pack "Steaming Sauna")) True))
        -- EVERY event any branch admits, concatenated, which is what makes the
        -- intersection below the honest floor for an AnyOf: a slot one branch
        -- binds and another does not must not be claimed.
        TriggerCondition.AnyOf conditions -> case conditions of
          [] -> one (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice))
          c : cs -> Foldable.foldr1 (<>) (fmap representativeEvents (c NonEmpty.:| cs))
        -- CR 708.7's own event, and the only one this condition admits, on the
        -- BEARER -- so the pair really matches.
        TriggerCondition.SelfTurnedFaceUp -> one (GameEvent.TurnedFaceUp departed)
        -- CR 701.27a's own event, and the only one this condition admits, on the
        -- BEARER and naming the same face the condition does -- so the pair
        -- really matches; the face below is the one everyTriggerCondition names.
        TriggerCondition.SelfTransformedInto name -> one (GameEvent.Transformed (Transformed.MkTransformed departed S.emptyCharacteristics {PC.names = Set.singleton name}))
        -- CR 701.27e's event for the bystander form, on `departed` for the reason
        -- the PermanentTurnedFaceUp arm below gives: the Filter this condition is
        -- instantiated with is the trivial one, which the sampled characteristics
        -- satisfy however empty they are.
        TriggerCondition.PermanentTransforms _ -> one (GameEvent.Transformed (Transformed.MkTransformed departed S.emptyCharacteristics))
        -- The same event for the watcher-scoped form, and the only one it admits.
        -- `departed` again, so the pair really matches: the Filter this condition
        -- is instantiated with below is the trivial one, which admits whatever the
        -- id resolves to.
        TriggerCondition.PermanentTurnedFaceUp _ -> one (GameEvent.TurnedFaceUp departed)
        -- CR 702.112b's own event, and the only one this condition admits, on
        -- `departed` for the arm above's reason.
        TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated d _) -> one (GameEvent.BecameDesignated (BecameDesignated.MkBecameDesignated d departed))
        -- CR 702.100b's own event, and the only one this condition admits. The
        -- pair does NOT match -- the condition is self-scoped and `departed` is
        -- not the bearer -- which pins the floor for a matching pair too, since
        -- this arm binds nothing either way.
        TriggerCondition.SelfEvolves -> one (GameEvent.Evolved departed)
        -- CR 702.134c's own event, and the only one this condition admits. TWO
        -- distinct ids, which is what the pin needs here: eventBindings stamps the
        -- SECOND under `thatMentoredCreature`, so an arm that bound the mentor
        -- instead would still agree with eventBindingSlots if the two coincided.
        -- Whether the pair matches on the board below does not matter, eventBindings
        -- reading the event rather than the attachment.
        TriggerCondition.AttachedCreatureMentors -> one (GameEvent.Mentored (Mentored.MkMentored departed arrived))
        -- CR 700.4's battlefield-to-graveyard move, the only event this condition
        -- admits. TWO distinct ids, which is what the pin needs: eventBindings
        -- stamps `departed` under CR 303.4b's `thatDepartedPermanent` and the
        -- BEARER's own arrival under CR 400.7f's `became`, so an arm that bound
        -- the move's arrival for either would still agree with eventBindingSlots
        -- if the ids coincided. Whether the departed permanent is really the
        -- bearer's host does not matter, eventBindings reading the event rather
        -- than the attachment.
        TriggerCondition.AttachedCreatureDies -> one (GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics))
        -- CR 701.26a's own event, and the only one this condition admits. Whether
        -- the tapped permanent is the bearer's host does not matter here:
        -- eventBindings claims nothing either way, and the floor is what this
        -- pins.
        TriggerCondition.AttachedCreatureBecomesTapped -> one (GameEvent.BecameTapped departed)
        -- CR 702.149c's own event, and the only one this condition admits, on
        -- `departed` for SelfEvolves' reason: the pair does not match, which pins
        -- the floor for a matching pair too, this arm binding nothing either way.
        TriggerCondition.SelfTrains -> one (GameEvent.Trained departed)
        -- CR 701.21a's own event, and the only one this condition admits. The
        -- pair need not actually match -- `departed` is no artifact on the empty
        -- board -- which is fine for what this pins: the arm binds the event's
        -- player under every relation the condition admits, so the floor is the
        -- same either way.
        TriggerCondition.PermanentSacrificed {} -> one (GameEvent.PermanentSacrificed (PermanentWasSacrificed.MkPermanentWasSacrificed S.alice departed))
        -- CR 603.3b's own event, and the only one this condition admits. The
        -- pair does NOT actually match here -- `departed` projects as no Saga on
        -- the empty board Event.matchesTrigger is asked about -- which is fine
        -- for what this pins: eventBindings contributes nothing for this
        -- condition under any event, so the floor is empty either way.
        TriggerCondition.SagaFinalChapterTriggers _ ->
          one
            ( GameEvent.AbilityTriggered
                AbilityTriggered.MkAbilityTriggered
                  { AbilityTriggered.source = TriggerSource.OfObject departed,
                    AbilityTriggered.controller = S.alice,
                    AbilityTriggered.ability = bareAbility (TriggerCondition.SelfCountersReached (SelfCountersReached.MkSelfCountersReached CounterKind.Lore 3))
                  }
            )
        -- CR 725.1's own event, and the only one this condition admits. CR 725.3
        -- makes it name exactly one player, so there is no second shape of the
        -- event for the floor to differ on. bob rather than the perspective
        -- player, on the SelfAttacks arm's reasoning: an arm that stamped CR
        -- 109.5's "you" instead of the crowned player would still agree with
        -- eventBindingSlots here if the two coincided.
        TriggerCondition.PlayerBecomesMonarch _ -> one (GameEvent.BecameMonarch S.bob)
        -- CR 603.7's own event, and the only shape of it: Engine.sampleControl mints
        -- a ControlChanged only where the two players differ, so there is no
        -- same-player shape for the floor to come apart on. The ids and seats are
        -- arbitrary -- this condition binds nothing from the log, which is what
        -- Event.eventBindingSlots claims for it.
        TriggerCondition.LoseControlOfBound _ -> one (GameEvent.ControlChanged (ControlChanged.MkControlChanged departed S.alice S.bob))
        -- CR 309.4c's own event. The dungeon id and the room are arbitrary: this
        -- condition binds nothing from the log, which is what
        -- Event.eventBindingSlots claims for it.
        TriggerCondition.RoomEntered _ -> one (GameEvent.VentureMarkerEntered (VentureMarkerEntered.MkVentureMarkerEntered S.alice departed RoomIndex.topmost))
        -- CR 701.22d's own event, and the only one this condition admits. bob
        -- rather than the perspective player, on the PlayerBecomesMonarch arm's
        -- reasoning: an arm that stamped CR 109.5's "you" instead of the scrying
        -- player would still agree with eventBindingSlots here if the two
        -- coincided.
        TriggerCondition.PlayerScries _ -> one (GameEvent.Scried S.bob)
        -- CR 701.54d's own event, the PlayerScries arm's shape and reasoning.
        TriggerCondition.RingTemptsPlayer _ -> one (GameEvent.RingTempted S.bob)
        -- CR 309.7's own event, and the only one this condition admits. bob
        -- rather than the perspective player, on the PlayerScries arm's reasoning.
        TriggerCondition.PlayerCompletesDungeon _ -> one (GameEvent.DungeonCompleted S.bob)
        -- CR 701.25d's own event, the arm above's twin. A DISTINCT event, which
        -- is what keeps this pin honest: an arm matching a scry here would claim
        -- the floor for the wrong keyword action.
        TriggerCondition.PlayerSurveils _ -> one (GameEvent.Surveiled S.bob)
        TriggerCondition.PlayerRollsDice _ -> one (GameEvent.DiceRolled S.bob)
        -- CR 705.2's own event, and the only one this condition admits. A WON
        -- flip, since neither a lost one nor rule 705.2's winnerless one matches
        -- at all -- and bob rather than the perspective player, on the
        -- PlayerScries arm's reasoning.
        TriggerCondition.PlayerWinsCoinFlip _ -> one (GameEvent.CoinFlipped CoinFlipped.MkCoinFlipped {CoinFlipped.flipper = S.bob, CoinFlipped.won = Just True})
        -- CR 702.170a's own event, and the only one this condition admits. On
        -- `departed`, which is not the bearer on the board below -- so the pair
        -- does not match, which pins the floor for a matching pair too, this
        -- condition binding nothing either way.
        TriggerCondition.SelfBecomesPlotted -> one (GameEvent.Plotted departed)
        -- CR 701.44b's own event, and the only one this condition admits, on
        -- `departed` for the arm above's reason.
        TriggerCondition.PermanentExplores _ -> one (GameEvent.Explored departed)
        -- CR 701.43a's own event, and the only one this condition admits, on
        -- `departed` for SelfEvolves' reason: the pair does not match, which pins
        -- the floor for a matching pair too, this condition binding nothing
        -- either way.
        TriggerCondition.SelfExerted -> one (GameEvent.Exerted departed)
        -- CR 701.3a's own event, and the only one this condition admits. The HOST
        -- is `departed`, the bearer position, so the pair really matches; the
        -- attachment is `arrived`, and the Filter this condition is instantiated
        -- with below is the trivial one, which admits whatever that id resolves
        -- to.
        TriggerCondition.SelfBecomesAttachedBy _ -> one (GameEvent.BecameAttached (BecameAttached.MkBecameAttached arrived (Recipient.ToCreature departed)))
        -- CR 603.12: a reflexive matches no log entry at all
        -- (Event.matchesTrigger's Reflexive arm answers False for every event),
        -- so any event is as representative as any other -- StateIs' arm above.
        TriggerCondition.Reflexive -> one (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice))

-- Every TriggerCondition, one inhabitant each. The payloads are arbitrary:
-- eventBindings and eventBindingSlots both ignore them, which is itself part of
-- what the pin asserts.
everyTriggerCondition :: [TriggerCondition.TriggerCondition]
everyTriggerCondition =
  [ TriggerCondition.SelfEnters,
    TriggerCondition.PermanentEnters Filter.Type.IsSource,
    TriggerCondition.CardPutIntoGraveyard Filter.Type.IsSource,
    TriggerCondition.PermanentDies Filter.Type.IsSource,
    TriggerCondition.PermanentsDie Filter.Type.IsSource,
    TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.EachTurn),
    TriggerCondition.StateIs (Condition.Type.Compares (Compares.MkCompares (Quantity.Type.Literal 0) Comparison.Exactly (Quantity.Type.Literal 0))),
    TriggerCondition.SelfDealsCombatDamageToPlayer,
    TriggerCondition.SelfIsDealtDamage,
    TriggerCondition.PermanentDealsCombatDamageToPlayer (Filter.Type.And []),
    TriggerCondition.PermanentsDealCombatDamageToPlayer (Filter.Type.And []),
    TriggerCondition.CreatureDealtCombatDamageToMonarch,
    TriggerCondition.CreaturesDealtCombatDamageToInitiative,
    TriggerCondition.PlayerTookInitiative,
    TriggerCondition.OpponentLostLifeDuringYourTurn,
    TriggerCondition.SelfCycled,
    TriggerCondition.SelfRevealedForMiracle,
    TriggerCondition.SelfDiscarded,
    TriggerCondition.PlayerDiscards PlayerRelation.Opponent,
    TriggerCondition.PlayerCycles PlayerRelation.You,
    TriggerCondition.PlayerCycles PlayerRelation.Opponent,
    TriggerCondition.PlayerDrawsNthCard (PlayerDrawsNthCard.MkPlayerDrawsNthCard PlayerRelation.You 2),
    TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
    TriggerCondition.SelfAttacksWithAnother (Filter.Type.And []),
    TriggerCondition.CreatureAttacksAlone (Filter.Type.And []),
    TriggerCondition.CreatureAttacksYou,
    TriggerCondition.AttachedPlayerIsAttacked,
    TriggerCondition.PlayerAttacks PlayerRelation.You,
    TriggerCondition.PlayerAttacks PlayerRelation.Opponent,
    TriggerCondition.PlayerAttacks PlayerRelation.AnyPlayer,
    TriggerCondition.PlayerAttacksWith (PlayerAttacksWith.MkPlayerAttacksWith PlayerRelation.You (Filter.Type.And []) 1),
    TriggerCondition.PlayerAttacksWith (PlayerAttacksWith.MkPlayerAttacksWith PlayerRelation.Opponent (Filter.Type.And []) 1),
    TriggerCondition.PlayerAttacksWith (PlayerAttacksWith.MkPlayerAttacksWith PlayerRelation.AnyPlayer (Filter.Type.And []) 1),
    -- The floor above one, Military Intelligence's: a fourth row rather than a
    -- fourth relation, since the count and the relation are independent fields
    -- and only the count is new.
    TriggerCondition.PlayerAttacksWith (PlayerAttacksWith.MkPlayerAttacksWith PlayerRelation.You (Filter.Type.And []) 2),
    TriggerCondition.PlayerAttacksPlayer (PlayerAttacksPlayer.MkPlayerAttacksPlayer PlayerRelation.You PlayerRelation.AnyPlayer),
    TriggerCondition.PlayerAttacksPlayer (PlayerAttacksPlayer.MkPlayerAttacksPlayer PlayerRelation.Opponent PlayerRelation.AnyPlayer),
    TriggerCondition.PlayerAttacksPlayer (PlayerAttacksPlayer.MkPlayerAttacksPlayer PlayerRelation.AnyPlayer PlayerRelation.AnyPlayer),
    TriggerCondition.PlayerAttacksPlayer (PlayerAttacksPlayer.MkPlayerAttacksPlayer PlayerRelation.Opponent PlayerRelation.You),
    TriggerCondition.PlayerAttacksPlayer (PlayerAttacksPlayer.MkPlayerAttacksPlayer PlayerRelation.AnyPlayer PlayerRelation.Opponent),
    TriggerCondition.SelfAttacksPlayerWithMostLife,
    TriggerCondition.SelfBlocks,
    TriggerCondition.SelfBlocksAtLeast 2,
    TriggerCondition.SelfBlocksCreature (Filter.Type.And []),
    TriggerCondition.SelfBecomesBlocked,
    TriggerCondition.SelfBlocksOneOrMore (Filter.Type.And []),
    TriggerCondition.SelfBecomesBlockedBy (Filter.Type.And []),
    TriggerCondition.SelfBecomesBlockedByOneOrMore (Filter.Type.And []),
    TriggerCondition.CreatureBecomesBlockedByAtLeast (CreatureBecomesBlockedByAtLeast.MkCreatureBecomesBlockedByAtLeast PlayerRelation.Opponent 2),
    TriggerCondition.SelfAttacksUnblocked,
    TriggerCondition.SelfPutIntoGraveyardFromLibrary,
    TriggerCondition.SelfPutIntoGraveyardFromAnywhere,
    TriggerCondition.SelfDies,
    TriggerCondition.SelfLeavesTheBattlefield,
    TriggerCondition.PermanentLeavesTheBattlefield Filter.Type.IsSource,
    TriggerCondition.PermanentReturnedToHand Filter.Type.IsSource,
    TriggerCondition.PermanentsReturnedToHand Filter.Type.IsSource,
    TriggerCondition.CardLeavesGraveyard (CardLeavesGraveyard.MkCardLeavesGraveyard Filter.Type.IsSource TurnScope.EachTurn),
    TriggerCondition.HauntedCreatureDies,
    TriggerCondition.SpellOrAbilityCounters PlayerRelation.You,
    TriggerCondition.DamageToPlayerPrevented PlayerRelation.You,
    TriggerCondition.SelfPreventsDamage (Filter.Type.And []),
    TriggerCondition.PlayerGainsLife PlayerRelation.You,
    TriggerCondition.PlayersGainLife PlayerRelation.AnyPlayer,
    TriggerCondition.PlayerLosesLife PlayerRelation.Opponent,
    TriggerCondition.SelfCountersReached (SelfCountersReached.MkSelfCountersReached CounterKind.Lore 1),
    TriggerCondition.SelfBecomesClassLevel (ClassLevel.MkClassLevel 2),
    TriggerCondition.SelfLastCounterRemoved CounterKind.Defense,
    TriggerCondition.SelfCountersRemoved CounterKind.Loyalty,
    TriggerCondition.PermanentsGetCounters (CounterPlacement.MkCounterPlacement CounterKind.MinusOneMinusOne (Filter.Type.And [])),
    TriggerCondition.PermanentGetsCounters (CounterPlacement.MkCounterPlacement CounterKind.MinusOneMinusOne (Filter.Type.And [])),
    -- BOTH scopes, unlike StepBegins' one above: the TurnScope is new on this
    -- condition, and the pin below asserts eventBindingSlots against what
    -- eventBindings stamps for every event -- so an arm that had cased on the
    -- scope and stamped nothing under one of them would go unseen if only one
    -- were listed.
    TriggerCondition.SpellCast (SpellCast.MkSpellCast Filter.Type.IsSource TurnScope.EachTurn Nothing Nothing),
    TriggerCondition.SpellCast (SpellCast.MkSpellCast Filter.Type.IsSource TurnScope.OpponentsTurn Nothing Nothing),
    -- And the zone axis, listed for the TurnScope pair's reason one field over:
    -- an arm that cased on the zone and stamped nothing when one was named would
    -- go unseen if every entry here left it Nothing.
    TriggerCondition.SpellCast (SpellCast.MkSpellCast Filter.Type.IsSource TurnScope.EachTurn (Just Zone.Hand) Nothing),
    -- And the ordinal axis, for the same reason again: Clarion Spirit's "your
    -- second spell each turn" narrows which cast fires the ability and stamps
    -- nothing of its own, which an entry leaving it Nothing could not show.
    TriggerCondition.SpellCast (SpellCast.MkSpellCast Filter.Type.IsSource TurnScope.EachTurn Nothing (Just 2)),
    TriggerCondition.SelfCast,
    -- BOTH relations, for the SpellCast pair's reason just above: the arm cases
    -- on the relation, and one that stamped nothing under the other half would go
    -- unseen if only rule 702.21a's own Opponent were listed.
    TriggerCondition.SelfBecomesTargeted PlayerRelation.Opponent,
    TriggerCondition.SelfBecomesTargeted PlayerRelation.You,
    -- BOTH inhabitants, for the SelfBecomesTargeted pair's reason just above and
    -- one axis more: the payload narrows on a relation AND on a kind, and the
    -- arity change alone would leave a single entry compiling. Amulet of
    -- Safekeeping's pair first, then Dormant Gomazoa's.
    TriggerCondition.ControllerBecomesTarget (ControllerBecomesTarget.MkControllerBecomesTarget PlayerRelation.Opponent Nothing),
    TriggerCondition.ControllerBecomesTarget (ControllerBecomesTarget.MkControllerBecomesTarget PlayerRelation.AnyPlayer (Just StackObjectKind.Spell)),
    TriggerCondition.SelfHalfUnlocked (CardName.MkCardName (Text.pack "Steaming Sauna")),
    TriggerCondition.RoomFullyUnlocked PlayerRelation.You,
    -- Balemurk Leech's own pair, and not an arbitrary one: PermanentEnters binds
    -- `became` while RoomFullyUnlocked binds nothing, so the intersection is
    -- EMPTY -- which is the case the union-versus-intersection call in
    -- Event.eventBindingSlots turns on. A union would claim `became` here and the
    -- pin below would catch it.
    TriggerCondition.AnyOf [TriggerCondition.PermanentEnters Filter.Type.IsSource, TriggerCondition.RoomFullyUnlocked PlayerRelation.You],
    TriggerCondition.SelfTurnedFaceUp,
    TriggerCondition.SelfTransformedInto (CardName.MkCardName (Text.pack "Blightsower Thallid")),
    TriggerCondition.PermanentTransforms (Filter.Type.And []),
    TriggerCondition.PermanentTurnedFaceUp (Filter.Type.And []),
    TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated Designation.Renowned (Filter.Type.And [])),
    TriggerCondition.SelfEvolves,
    TriggerCondition.AttachedCreatureMentors,
    TriggerCondition.AttachedCreatureDies,
    TriggerCondition.AttachedCreatureBecomesTapped,
    TriggerCondition.SelfTrains,
    -- ALL THREE relations, on the PlayerAttacksWith rows' reasoning above: an
    -- eventBindings arm that had cased on the relation and stamped nothing under
    -- one of them would go unseen if only one were listed. Vengeful Tracker
    -- prints the Opponent form and Mayhem Devil the AnyPlayer one.
    TriggerCondition.PermanentSacrificed (PermanentSacrificed.MkPermanentSacrificed PlayerRelation.You (Filter.Type.And [])),
    TriggerCondition.PermanentSacrificed (PermanentSacrificed.MkPermanentSacrificed PlayerRelation.Opponent (Filter.Type.And [])),
    TriggerCondition.PermanentSacrificed (PermanentSacrificed.MkPermanentSacrificed PlayerRelation.AnyPlayer (Filter.Type.And [])),
    TriggerCondition.SagaFinalChapterTriggers PlayerRelation.You,
    -- BOTH relations, on the SpellCast pair's reasoning above: an eventBindings
    -- arm that had cased on the relation and stamped nothing under one of them
    -- would go unseen if only one were listed. Custodi Lich prints the You form
    -- and Garland, Royal Kidnapper the Opponent one, and both stamp the crowned
    -- player -- which is the claim this list exists to keep honest.
    TriggerCondition.PlayerBecomesMonarch PlayerRelation.You,
    TriggerCondition.PlayerBecomesMonarch PlayerRelation.Opponent,
    TriggerCondition.LoseControlOfBound (SlotName.MkSlotName (Text.pack "target")),
    TriggerCondition.RoomEntered RoomIndex.topmost,
    -- BOTH relations for each of the two, on the PlayerBecomesMonarch pair's
    -- reasoning: an eventBindings arm that had cased on the relation and stamped
    -- nothing under one of them would go unseen if only one were listed.
    TriggerCondition.PlayerCompletesDungeon PlayerRelation.You,
    TriggerCondition.PlayerCompletesDungeon PlayerRelation.Opponent,
    TriggerCondition.PlayerScries PlayerRelation.You,
    TriggerCondition.PlayerScries PlayerRelation.Opponent,
    TriggerCondition.PlayerSurveils PlayerRelation.You,
    TriggerCondition.PlayerSurveils PlayerRelation.Opponent,
    TriggerCondition.PlayerRollsDice PlayerRelation.You,
    TriggerCondition.PlayerRollsDice PlayerRelation.Opponent,
    TriggerCondition.PlayerWinsCoinFlip PlayerRelation.You,
    TriggerCondition.PlayerWinsCoinFlip PlayerRelation.Opponent,
    TriggerCondition.SelfBecomesPlotted,
    TriggerCondition.PermanentExplores (Filter.Type.And []),
    TriggerCondition.SelfExerted,
    TriggerCondition.SelfBecomesAttachedBy (Filter.Type.And []),
    TriggerCondition.Reflexive,
    TriggerCondition.RingTemptsPlayer PlayerRelation.You,
    TriggerCondition.RingTemptsPlayer PlayerRelation.Opponent,
    TriggerCondition.PermanentBecomesBlockedBy (Filter.Type.And [])
  ]

-- CR 702.46 soulshift N, the first minted keyword ability that TARGETS A CARD IN
-- A GRAVEYARD -- CR 115.2's clause (a) pool, which until now only card data
-- (Raise Dead) reached. Its dies condition is afterlife's and its optional
-- targeted clause is provoke's; what is new is the pool and the filter.
--
-- Kami of Empty Graves, {3}{B} Creature -- Spirit 4/1, whose entire text box is
-- "Soulshift 3". Murder does the killing, modularSpec's reason.
--
-- Alice's graveyard is seeded so that every way the minted filter could be wrong
-- picks a DIFFERENT card, and each wrong card has a SMALLER ObjectId than the
-- right one -- S.identityAnswer takes the least legal recipient, so a widened
-- filter is not merely permitted to go wrong, it is made to:
--
--   * Goblin Piker -- in the graveyard, not a Spirit. Drops out on the subtype.
--   * Shimatsu the Bloodcloaked -- a Spirit, mana value 4. Drops out on rule
--     702.46a's "N or less", which here is 3.
--   * bob's own Disowned Ancestor -- a Spirit of mana value 1 in the WRONG
--     graveyard (CR 400.1), which is what makes PlayerScope.You load-bearing.
--
-- leaving alice's Disowned Ancestor ({B} Creature -- Spirit Warrior) as the only
-- legal target. The dead Kami's own graveyard incarnation is a Spirit too and is
-- excluded by its mana value of 4 rather than by an "another" the rule does not
-- print.
-- Pays wherever `who` is offered a resolution cost, and answers everything else
-- as S.identityAnswer does -- so a transcript with no ChooseToPay in it says the
-- prompt was never raised rather than that the offer was refused. The Decider is
-- checked alongside the player because CR 723.1 can part the two; nothing in
-- these fixtures controls anybody, so they must agree.
paysFor :: PlayerId.PlayerId -> Prompt.Prompt r -> r
paysFor who p = case p of
  Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
    | d == who && player == who ->
        PaymentDecision.Pays
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  cyclingTriggerSpec s registry
  graveyardTriggerSpec s registry
  gaeasBlessingSpec s registry
  graveyardEffectZoneTriggerSpec s registry
  droughtUpkeepSpec s registry
  commandZoneTriggerSpec s registry
  serraAvatarSpec s registry
  planarVoidSpec s registry
  diesTriggerSpec s registry
  permanentDiesSpec s registry
  permanentsDieSpec s registry
  merenEndStepSpec s registry
  leavesBattlefieldSpec s registry
