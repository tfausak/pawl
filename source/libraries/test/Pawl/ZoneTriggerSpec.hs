-- Pawl.Engine.Trigger over zone changes: the graveyard, exile and command
-- zones, entering and leaving the battlefield, and CR 603.10a's look-back at
-- the game state the leaving object left behind. The machinery is
-- Pawl.TriggerSpec.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

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
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
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
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.ClassLevelChange as ClassLevelChange
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
-- Aliased Condition.Type, not Condition, per the project-wide convention
-- (CardSpec's note): the evaluator module Pawl.Engine.Condition may later be imported
-- and must not collide.
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ControlChanged as ControlChanged
import qualified Pawl.Types.ControllerBecomesTarget as ControllerBecomesTarget
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
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
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HalfUnlocked as HalfUnlocked
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Mentored as Mentored
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerDrawsNthCard as PlayerDrawsNthCard
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.StackObjectKind as StackObjectKind
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.Transformed as Transformed
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.VentureMarkerEntered as VentureMarkerEntered
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

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
          discarded = S.runPure S.identityAnswer gs (Cost.payComponent PaymentMoment.OutsideResolution S.alice S.noSource (CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))))
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
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped buried) buried))) []
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice buried)) "it is in the graveyard"
        -- "from your library" doing real work, half two: dying is a move to
        -- the same graveyard from the battlefield, and is not this trigger.
        Spec.it s "CR 113.6k Narcomoeba dying from the BATTLEFIELD does not trigger" $ do
          narcomoeba <- S.printingOf s registry "Narcomoeba"
          let (creature, gs) = S.addCreature narcomoeba S.alice (Setup.emptyGame S.bothPlayers)
              died = S.runPure S.identityAnswer gs (Event.changeZone creature Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped died) died))) []
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
          Spec.assertEqWith s "and a creature entering fires nothing" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped entered) entered))) []
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
              scanned = fst (Event.gatherTriggers (Event.unscannedGrouped resolved) resolved)
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
          Spec.assertEqWith s "and nothing triggered -- it never arrived from a library in this batch" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped resolved) resolved))) []
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
          Spec.assertEqWith s "and the narrow scan of that batch offered nothing" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped resolved) resolved))) []
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
            (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped gs) gs)))
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
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped begun) begun))) []

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
            (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped atEnd) atEnd)))
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
          Spec.assertEqWith s "no trigger gathered" (length (fst (Event.gatherTriggers (Event.unscannedGrouped atBobs) atBobs))) 0
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
          let moveTo to = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange creature creature Zone.Battlefield to) S.emptyCharacteristics)
              matches = Event.matchesTrigger gs creature S.alice TriggerCondition.SelfPutIntoGraveyardFromAnywhere
          Spec.assertBool s (matches (moveTo Zone.Graveyard)) "a graveyard-bound move matches"
          Spec.assertBool s (not (matches (moveTo Zone.Exile))) "an exile-bound move does not"
        -- The gameplay-level companion to the pair above: an Avatar exiled off
        -- the battlefield really does leave nothing on the stack.
        Spec.it s "CR 603.6 a Serra Avatar EXILED from the battlefield triggers nothing" $ do
          (creature, gs) <- cardIn S.addCreature
          let exiled = S.runPure S.identityAnswer gs (Event.changeZone creature Zone.Exile)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped exiled) exiled))) []
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Exile S.alice exiled)) "it is in exile"

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
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped exiled) exiled))) []
          Spec.assertBool s (Set.member travelerName (namesIn Zone.Exile S.alice exiled)) "it is in exile"
        -- The other half of "from the battlefield": the same card discarded
        -- reaches the same graveyard and has not died (CR 700.4).
        Spec.it s "CR 700.4 a Traveler discarded from the HAND does not trigger" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          let (traveler, gs) = S.addHandCard doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
              discarded = S.runPure S.identityAnswer gs (Event.changeZone traveler Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped discarded) discarded))) []
        -- Self-scoped: SOME OTHER creature dying is not this Traveler's
        -- death, even though the Traveler is right there to see it.
        Spec.it s "CR 603.6c another creature dying does not fire the Traveler's trigger" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withTraveler) = S.addCreature doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, gs) = S.addCreature piker S.alice withTraveler
              died = S.runPure S.identityAnswer gs (Event.changeZone pikerId Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped died) died))) []
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
          Spec.assertEqWith s "so the trigger is hers, not its owner's" (fmap PendingTrigger.controller (fst (Event.gatherTriggers (Event.unscannedGrouped died) died))) [S.alice]

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
      sourcesOf gs = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped gs) gs))
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

-- CR 603.2c's second sentence, and the fork it forces on the written form
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
                  GameEvent.Moved (Moved.MkMoved zc _)
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
              leftFor to = Event.eventBindings Nothing TriggerCondition.SelfLeavesTheBattlefield (GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield to) S.emptyCharacteristics))
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
              entry = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange token token Zone.Battlefield Zone.Battlefield) S.emptyCharacteristics)
              gone = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange token (ObjectId.MkObjectId 2) Zone.Battlefield Zone.Exile) S.emptyCharacteristics)
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
-- keyset. Two are not: SelfLeavesTheBattlefield, whose two destinations differ
-- because CR 400.7e binds `became` for the public one and withholds it for the
-- hidden one (CR 400.2), and SelfIsDealtDamage, which admits both of CR 120.3's
-- damage kinds.
--
-- Exhaustive with no wildcard, which is half of what keeps the pin honest -- a
-- new TriggerCondition fails to compile here. The other half, the list below, is
-- hand-kept and cannot be forced; add the new constructor there too.
representativeEvents :: TriggerCondition.TriggerCondition -> NonEmpty.NonEmpty GameEvent.GameEvent
representativeEvents cond =
  let departed = ObjectId.MkObjectId 1
      arrived = ObjectId.MkObjectId 2
      moved from to = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange departed arrived from to) S.emptyCharacteristics)
      combatDamage =
        GameEvent.DamageDealt
          (DamageEvent.MkDamageEvent departed (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Combat)
      one e = e NonEmpty.:| []
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
        TriggerCondition.CreatureDealtCombatDamageToMonarch -> one combatDamage
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
        TriggerCondition.SelfBlocksCreature _ -> one (GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = departed, BecameBlocking.attacker = ObjectId.MkObjectId 41, BecameBlocking.putOntoBattlefield = False}))
        -- CR 508.5's defending player again, and carol for SelfAttacks' reason
        -- above: eventBindings binds this field under `thatPlayer`.
        TriggerCondition.SelfBecomesBlocked -> one (GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked departed S.carol))
        -- The same declaration event SelfBlocks names, with the ids the other way
        -- round: this condition's bearer is the ATTACKER, and the blocker is what
        -- it binds.
        TriggerCondition.SelfBecomesBlockedBy _ -> one (GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = ObjectId.MkObjectId 41, BecameBlocking.attacker = departed, BecameBlocking.putOntoBattlefield = False}))
        -- The GROUPED attacking-side event, which is what makes this one fire
        -- once where the arm above fires per blocker. carol on SelfBecomesBlocked's
        -- reasoning -- and this one binds that player nothing, which is the
        -- difference the pin catches.
        TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> one (GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked departed S.carol))
        -- The same grouped event once more, with the ids read the other way from
        -- every arm above it: the bearer is a BYSTANDER, so `departed` sits in
        -- the attacker position and is what this one binds -- an arm that bound
        -- the bearer instead would pin the empty set here.
        TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> one (GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked departed S.carol))
        -- The same declaration's unblocked branch, which carries the attacker
        -- and nothing else -- so the floor it pins is the empty set.
        TriggerCondition.SelfAttacksUnblocked -> one (GameEvent.AttackerUnblocked departed)
        TriggerCondition.SelfPutIntoGraveyardFromLibrary -> one (moved Zone.Library Zone.Graveyard)
        -- Every origin zone is admitted, but the floor is the same for all of
        -- them: the destination is always a graveyard, which CR 400.2 makes
        -- public, so CR 400.7e never withholds anything and one event says as
        -- much as any list would.
        TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> one (moved Zone.Hand Zone.Graveyard)
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
        -- three and whose floor is empty for the same two reasons.
        TriggerCondition.PermanentLeavesTheBattlefield _ ->
          moved Zone.Battlefield Zone.Graveyard NonEmpty.:| [moved Zone.Battlefield Zone.Hand, GameEvent.LeftTheGame departed]
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
        TriggerCondition.DamageToPlayerPrevented _ -> one (GameEvent.DamagePrevented (DamagePrevented.MkDamagePrevented (Recipient.ToPlayer S.bob) 2))
        -- CR 119.9's own event, and the only one this condition admits: the
        -- payload is a player and an amount, and the amount is the floor.
        TriggerCondition.PlayerGainsLife _ -> one (GameEvent.LifeGained (LifeChange.MkLifeChange S.bob 2))
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
        TriggerCondition.SelfHalfUnlocked half -> one (GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked departed half False))
        -- CR 709.5i's own event, with the flag SET -- an unset one matches
        -- nothing, and would pin the floor against an event this condition does
        -- not admit.
        TriggerCondition.RoomFullyUnlocked _ -> one (GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked departed (CardName.MkCardName (Text.pack "Steaming Sauna")) True))
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
        TriggerCondition.SelfTransformedInto name -> one (GameEvent.Transformed (Transformed.MkTransformed departed (Set.singleton name)))
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
        -- admits. Whether the departed permanent is the bearer's host does not
        -- matter here: eventBindings claims nothing either way, and the floor is
        -- what this pins.
        TriggerCondition.AttachedCreatureDies -> one (GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics))
        -- CR 701.26a's own event, and the only one this condition admits. Whether
        -- the tapped permanent is the bearer's host does not matter here for the
        -- AttachedCreatureDies arm's reason: eventBindings claims nothing either
        -- way, and the floor is what this pins.
        TriggerCondition.AttachedCreatureBecomesTapped -> one (GameEvent.BecameTapped departed)
        -- CR 702.149c's own event, and the only one this condition admits, on
        -- `departed` for SelfEvolves' reason: the pair does not match, which pins
        -- the floor for a matching pair too, this arm binding nothing either way.
        TriggerCondition.SelfTrains -> one (GameEvent.Trained departed)
        -- CR 701.21a's own event, and the only one this condition admits. The
        -- payload is arbitrary: the condition compares nothing, so any sacrifice
        -- matches and the floor is the same for all of them.
        TriggerCondition.PermanentSacrificed -> one (GameEvent.PermanentSacrificed (PermanentSacrificed.MkPermanentSacrificed S.alice departed))
        -- CR 603.3b's own event, and the only one this condition admits. The
        -- pair does NOT actually match here -- `departed` projects as no Saga on
        -- the empty board Event.matchesTrigger is asked about -- which is fine
        -- for what this pins: eventBindings contributes nothing for this
        -- condition under any event, so the floor is empty either way.
        TriggerCondition.SagaFinalChapterTriggers _ ->
          one (GameEvent.AbilityTriggered (AbilityTriggered.MkAbilityTriggered departed S.alice (TriggerCondition.SelfCountersReached (SelfCountersReached.MkSelfCountersReached CounterKind.Lore 3))))
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
        -- CR 701.25d's own event, the arm above's twin. A DISTINCT event, which
        -- is what keeps this pin honest: an arm matching a scry here would claim
        -- the floor for the wrong keyword action.
        TriggerCondition.PlayerSurveils _ -> one (GameEvent.Surveiled S.bob)
        TriggerCondition.PlayerRollsDice _ -> one (GameEvent.DiceRolled S.bob)
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
    TriggerCondition.PermanentDies Filter.Type.IsSource,
    TriggerCondition.PermanentsDie Filter.Type.IsSource,
    TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.EachTurn),
    TriggerCondition.StateIs (Condition.Type.Compares (Compares.MkCompares (Quantity.Type.Literal 0) Comparison.Exactly (Quantity.Type.Literal 0))),
    TriggerCondition.SelfDealsCombatDamageToPlayer,
    TriggerCondition.SelfIsDealtDamage,
    TriggerCondition.PermanentDealsCombatDamageToPlayer (Filter.Type.And []),
    TriggerCondition.CreatureDealtCombatDamageToMonarch,
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
    TriggerCondition.PlayerAttacksWith (PlayerAttacksWith.MkPlayerAttacksWith PlayerRelation.You (Filter.Type.And [])),
    TriggerCondition.PlayerAttacksWith (PlayerAttacksWith.MkPlayerAttacksWith PlayerRelation.Opponent (Filter.Type.And [])),
    TriggerCondition.PlayerAttacksWith (PlayerAttacksWith.MkPlayerAttacksWith PlayerRelation.AnyPlayer (Filter.Type.And [])),
    TriggerCondition.PlayerAttacksPlayer PlayerRelation.You,
    TriggerCondition.PlayerAttacksPlayer PlayerRelation.Opponent,
    TriggerCondition.PlayerAttacksPlayer PlayerRelation.AnyPlayer,
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
    TriggerCondition.HauntedCreatureDies,
    TriggerCondition.SpellOrAbilityCounters PlayerRelation.You,
    TriggerCondition.DamageToPlayerPrevented PlayerRelation.You,
    TriggerCondition.PlayerGainsLife PlayerRelation.You,
    TriggerCondition.PlayerLosesLife PlayerRelation.Opponent,
    TriggerCondition.SelfCountersReached (SelfCountersReached.MkSelfCountersReached CounterKind.Lore 1),
    TriggerCondition.SelfBecomesClassLevel (ClassLevel.MkClassLevel 2),
    TriggerCondition.SelfLastCounterRemoved CounterKind.Defense,
    TriggerCondition.SelfCountersRemoved CounterKind.Loyalty,
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
    TriggerCondition.PermanentTurnedFaceUp (Filter.Type.And []),
    TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated Designation.Renowned (Filter.Type.And [])),
    TriggerCondition.SelfEvolves,
    TriggerCondition.AttachedCreatureMentors,
    TriggerCondition.AttachedCreatureDies,
    TriggerCondition.AttachedCreatureBecomesTapped,
    TriggerCondition.SelfTrains,
    TriggerCondition.PermanentSacrificed,
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
    TriggerCondition.PlayerScries PlayerRelation.You,
    TriggerCondition.PlayerScries PlayerRelation.Opponent,
    TriggerCondition.PlayerSurveils PlayerRelation.You,
    TriggerCondition.PlayerSurveils PlayerRelation.Opponent,
    TriggerCondition.PlayerRollsDice PlayerRelation.You,
    TriggerCondition.PlayerRollsDice PlayerRelation.Opponent,
    TriggerCondition.SelfBecomesPlotted,
    TriggerCondition.PermanentExplores (Filter.Type.And []),
    TriggerCondition.SelfExerted,
    TriggerCondition.SelfBecomesAttachedBy (Filter.Type.And []),
    TriggerCondition.Reflexive
  ]

-- CR 603.6c's first written form read by a BYSTANDER -- Super Shredder {1}{B}
-- Legendary Creature -- Mutant Ninja Human 1/1, "Whenever another permanent
-- leaves the battlefield, put a +1/+1 counter on Super Shredder."
--
-- leavesBattlefieldSpec above is the SELF-scoped half of the same rule, and
-- permanentDiesSpec is this same bystander scoping one rule narrower. What this
-- group has to prove that neither of those does is that the watcher sees a
-- departure it had no part in, to a destination CR 700.4 does not reach: the
-- printed word "another" is Not IsSource in the condition's own Filter, and
-- nothing else narrows it -- "permanent" is no predicate here, since
-- matchesTrigger has already required the battlefield as the origin.
--
-- The bearer is bob's, the departing permanent alice's, so a condition that had
-- quietly read "you control" would answer these cases 0.
permanentLeavesTheBattlefieldSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentLeavesTheBattlefieldSpec s registry =
  let -- bob's Super Shredder watching, alice's Goblin Piker as the victim, and
      -- `lands` of alice's to pay with. The Shredder is bob's so that no case
      -- here can pass on a controller check the condition does not make.
      shredderBoard lands = do
        shredder <- S.printingOf s registry "Super Shredder"
        piker <- S.printingOf s registry "Goblin Piker"
        let (shredderId, withShredder) = S.addCreature shredder S.bob lands
            (pikerId, gs) = S.addCreature piker S.alice withShredder
        pure (shredderId, pikerId, gs)
      -- Cast the one spell in hand at `oid`, resolve it, settle (CR 117.5 is
      -- where the scan sees the departure), then resolve the trigger the settle
      -- placed. Aimed by ID: S.identityAnswer takes the least Recipient, and
      -- with two creatures on the board that is whichever id sorts first rather
      -- than the one the case is about.
      castAt :: ObjectId.ObjectId -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      castAt oid (gs, spellId) =
        let answer :: Prompt.Prompt r -> r
            answer p = case p of
              -- FILTERED rather than built: which Recipient constructor a pool
              -- offers is the pool's business (Angelic Edict's is Permanents,
              -- Unsummon's is Creatures), and a hand-built one of the other
              -- shape is silently dropped at CR 608.2b's re-read.
              Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just oid) . Recipient.objectOf) . snd) sets
              _ -> S.identityAnswer p
            cast = S.runPure answer gs (S.cast S.alice spellId)
            resolved = S.runPure answer cast Stack.resolveTop
            settled = S.runPure answer resolved Engine.settleForPriority
         in (settled, S.runPure answer settled Stack.resolveTop)
      -- CR 113.7a's source id for every triggered ability currently on the stack.
      triggerSourcesOn gs =
        Maybe.mapMaybe
          ( \oid -> case fmap Object.source (Game.lookupObject oid gs) of
              Just (Source.OfTrigger triggered) -> Just (TriggeredAbilitySource.source triggered)
              _ -> Nothing
          )
          (GameState.stack gs)
   in Spec.describe s "PermanentLeavesTheBattlefield" $ do
        -- The gameplay-level proof, and the destination that makes this
        -- condition rather than PermanentDies the one under test: CR 400.2
        -- makes exile a public zone that is not a graveyard, so a dies trigger
        -- would stay silent here.
        Spec.it s "CR 603.6c whole card: alice's Piker is exiled and bob's Super Shredder grows" $ do
          plains <- S.printingOf s registry "Plains"
          edict <- S.printingOf s registry "Angelic Edict"
          (shredderId, pikerId, board) <- shredderBoard (S.landsInPlay plains 5)
          let (settled, after) = castAt pikerId (S.handOne edict board)
          Spec.assertEqWith s "the Shredder is a 1/1 before anything leaves" (Projection.powerOf shredderId board, Projection.toughnessOf shredderId board) (Just 1, Just 1)
          Spec.assertEqWith s "the Piker really was exiled, not destroyed" (fmap Object.zone (Game.lookupObject pikerId settled)) Nothing
          Spec.assertEqWith s "and it is bob's Shredder that grew, off alice's permanent" (Projection.powerOf shredderId after, Projection.toughnessOf shredderId after) (Just 2, Just 2)
          Spec.assertEqWith s "one counter, from one departure" (fmap (Map.lookup CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject shredderId after)) (Just (Just 1))
        -- CR 400.2's hidden half of the same rule: a bounce reaches a HAND, and
        -- the watcher still sees it. The bearer reads the departing permanent
        -- from CR 608.2h last known information, which is what makes this
        -- answerable at all -- there is no public incarnation to read.
        Spec.it s "CR 603.6c whole card: Unsummon bounces the Piker to a hidden zone and the Shredder still grows" $ do
          island <- S.printingOf s registry "Island"
          unsummon <- S.printingOf s registry "Unsummon"
          (shredderId, pikerId, board) <- shredderBoard (S.landsInPlay island 1)
          let (settled, after) = castAt pikerId (S.handOne unsummon board)
          Spec.assertEqWith s "the Piker is in its owner's hand" (Game.lookupObject pikerId settled) Nothing
          Spec.assertEqWith s "the Shredder grew on a departure to a hidden zone" (Projection.powerOf shredderId after, Projection.toughnessOf shredderId after) (Just 2, Just 2)
        -- CR 603.10a's look-back, which this condition needs for the reason
        -- PermanentDies needs it and one step further: the WATCHER can be gone
        -- too. alice's Day of Judgment destroys her Piker and bob's Shredder in
        -- one CR 704.3 batch, so by the CR 117.5 boundary the ability's own
        -- bearer is a card in a graveyard -- and CR 603.10a reads "the existence
        -- of those abilities ... immediately prior to the event", so it triggers
        -- anyway.
        --
        -- The stack is what this case can read: the counter the trigger puts on
        -- "Super Shredder" lands on a card in a graveyard and changes nothing
        -- observable, so what the rule decides here is whether the ability
        -- reached the stack at all. ONE trigger, not two -- the Shredder's own
        -- departure is in the same batch and the printed "another" declines it.
        Spec.it s "CR 603.10a a Shredder swept alongside the Piker still sees the Piker go" $ do
          plains <- S.printingOf s registry "Plains"
          dayOfJudgment <- S.printingOf s registry "Day of Judgment"
          (shredderId, _, board) <- shredderBoard (S.landsInPlay plains 4)
          let (withSpell, spell) = S.handOne dayOfJudgment board
              afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
              swept = S.runPure S.identityAnswer afterCast Stack.resolveTop
              settled = S.runPure S.identityAnswer swept Engine.settleForPriority
          Spec.assertEqWith s "the sweep left no creatures, the watcher included" (Set.size (Set.filter (`Projection.isCreatureOf` settled) (GameState.battlefield settled))) 0
          -- CR 113.7a: the placed ability's source is the id the Shredder had on
          -- the battlefield, which is what makes this a statement about WHOSE
          -- ability reached the stack rather than about how many did.
          Spec.assertEqWith s "the Shredder's own ability still reached the stack, exactly once" (triggerSourcesOn settled) [shredderId]

        -- The printed "another", with the two Filters side by side on ONE
        -- departure so the exclusion is the only thing that differs. Without it
        -- a Super Shredder that left the battlefield would see itself go.
        Spec.it s "CR 603.6c the printed \"another\" is the only thing declining the bearer's own departure" $ do
          plains <- S.printingOf s registry "Plains"
          (shredderId, _, board) <- shredderBoard (S.landsInPlay plains 0)
          let gone = S.runPure S.identityAnswer board (Event.changeZone shredderId Zone.Exile)
              moves = filter (\e -> case e of GameEvent.Moved {} -> True; _ -> False) (S.eventsOf gone)
          case moves of
            [departure] -> do
              Spec.assertBool s (Event.matchesTrigger gone shredderId S.bob (TriggerCondition.PermanentLeavesTheBattlefield (Filter.Type.And [])) departure) "a Filter without the exclusion admits the Shredder's own departure"
              Spec.assertBool s (not (Event.matchesTrigger gone shredderId S.bob (TriggerCondition.PermanentLeavesTheBattlefield (Filter.Type.Not Filter.Type.IsSource)) departure)) "so the printed \"another\" is the only thing declining it"
            other -> Spec.assertFailure s ("expected exactly one zone change, got " <> show (length other))

-- CR 603.6c's penultimate sentence -- "An ability that attempts to do something
-- to the card that left the battlefield checks for it only in the first zone
-- that it went to" -- said positively by CR 400.7e: "Abilities that trigger when
-- an object moves from one zone to another ... can find the new object that it
-- became in the zone it moved to when the ability triggered, if that zone is a
-- public zone."
--
-- Endless Cockroaches, {1}{B}{B} Creature -- Insect 1/1, "When this creature
-- dies, return it to its owner's hand." Two different objects hide inside that
-- one printed word "it": the ability's SOURCE (CR 113.7a -- the permanent that
-- died, which CR 603.10a's look-back reads from CR 608.2h last known
-- information) and the CARD it became in the graveyard, which is what the
-- effect has to move. CR 400.7 minted a fresh id for the second, so the two are
-- not interchangeable and they are not one slot.
--
-- Also the home of the pin on Event.eventBindingSlots (the last case): that
-- classification restates in one dimension what eventBindings says in two, so
-- the two are compared here rather than trusted to agree.
--
-- The structural twin is Narcomoeba's `MoveToZone "self" Battlefield` in
-- graveyardTriggerSpec: same opcode, same slot shape. There "self" IS the
-- arriving card, because SelfPutIntoGraveyardFromLibrary matches on the
-- ARRIVING incarnation; here it is not, because CR 603.10a makes this condition
-- match on the DEPARTING one. That contrast is why there are two slots.
becameSlotSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
becameSlotSpec s registry =
  let -- alice: one Mountain (Lightning Bolt's {R}), the Cockroaches in play, and
      -- the Bolt in hand. S.identityAnswer targets the least Recipient, and
      -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
      -- creature on the board is the target without a bespoke interpreter.
      roachBoard = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        cockroaches <- S.printingOf s registry "Endless Cockroaches"
        let (roachId, withRoaches) = S.addCreature cockroaches S.alice (S.landsInPlay mountain 1)
        pure (roachId, S.handOne lightningBolt withRoaches)
      -- Cast the Bolt, resolve it (3 damage marked on a 1/1), settle -- CR
      -- 704.5g destroys it and the same CR 117.5 settle's trigger scan sees the
      -- death -- then resolve the trigger.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      roachName = CardName.MkCardName $ Text.pack "Endless Cockroaches"
   in Spec.describe s "CR 400.7e the card it became" $ do
        -- The gameplay-level proof, cast to resolution. The discriminating
        -- assertion is the HAND: an effect reading the trigger's source would
        -- name the dead battlefield id and move nothing, leaving the card in
        -- the graveyard where the state-based action put it.
        Spec.it s "CR 603.6c whole card: Lightning Bolt kills Endless Cockroaches and its dies trigger returns the card to hand" $ do
          (_, board) <- roachBoard
          let (settled, after) = boltIt board
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Set.member roachName (namesIn Zone.Graveyard S.alice settled)) "the card is in the graveyard when the trigger is placed"
          Spec.assertBool s (Set.member roachName (namesIn Zone.Hand S.alice after)) "and in hand once it resolves"
          Spec.assertBool s (not (Set.member roachName (namesIn Zone.Graveyard S.alice after))) "no longer in the graveyard"
          -- The Bolt itself is the graveyard's only remaining tenant, so the
          -- assertion above cannot be passing because the graveyard is read
          -- from the wrong player's zone.
          Spec.assertEqWith s "only the Bolt is left there" (namesIn Zone.Graveyard S.alice after) (Set.singleton (CardName.MkCardName $ Text.pack "Lightning Bolt"))
        -- The two slots, side by side on the placed trigger. CR 113.7a's
        -- source is the id that DIED and no longer resolves; CR 400.7e's
        -- "became" is the graveyard card, which does.
        Spec.it s "CR 113.7a the self slot keeps the departed id while became names the graveyard card" $ do
          (roachId, board) <- roachBoard
          let (settled, _) = boltIt board
              bindingsOn oid = maybe Map.empty Object.bindings (Game.lookupObject oid settled)
              slots = concatMap (Map.toList . Map.mapMaybe Binding.onlyOne . Binding.targetsOf . bindingsOn) (GameState.stack settled)
              slotFor name = lookup name slots
          Spec.assertEqWith s "self is the permanent that died" (slotFor Binding.triggerSource) (Just (Recipient.ToObject roachId))
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject roachId settled)) "and that id is gone (CR 400.7)"
          case slotFor Binding.became of
            Just (Recipient.ToObject graveyardId) -> do
              Spec.assertBool s (graveyardId /= roachId) "became is a different id"
              Spec.assertEqWith s "and it is the graveyard card" (fmap Face.name (Game.faceOf graveyardId settled)) (Just roachName)
              -- The spent Bolt is in that graveyard too, so membership is the
              -- assertion rather than the whole zone.
              Spec.assertBool s (elem graveyardId (Game.zoneMembers Zone.Graveyard S.alice settled)) "in alice's graveyard"
            other -> Spec.assertFailure s ("expected became to name an object, got " <> show other)
        -- eventBindings in isolation, so the binding is pinned to the rule
        -- rather than to one card's payload. CR 400.7e's "the new object that
        -- it became in the zone it moved to" is ZoneChange.object, never
        -- ZoneChange.departed, which is what matchesTrigger matched on.
        Spec.it s "CR 400.7e eventBindings binds the ARRIVING id, not the departed one" $ do
          let departed = ObjectId.MkObjectId 1
              arrived = ObjectId.MkObjectId 2
              died = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics)
          Spec.assertEqWith s "became names the graveyard incarnation" (Event.eventBindings Nothing TriggerCondition.SelfDies died) (Map.singleton Binding.became (Binding.toObject arrived))
        -- A condition that is not a look-back gets no such slot: Narcomoeba's
        -- bearer IS the arriving card, so binding it again would be a second
        -- name for the same object.
        Spec.it s "CR 113.6k a library-to-graveyard trigger binds nothing" $ do
          let oid = ObjectId.MkObjectId 1
              milled = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange oid oid Zone.Library Zone.Graveyard) S.emptyCharacteristics)
          Spec.assertEqWith s "no became slot" (Event.eventBindings Nothing TriggerCondition.SelfPutIntoGraveyardFromLibrary milled) Map.empty
        -- CR 400.7f's arm, the one place the slot comes from something other than
        -- the event: the BEARER's own arrival, which eventTriggers computes off
        -- the batch. Pinned in isolation so the rule is stated once here and once
        -- on the board in screamsFromWithinSpec, and so that the absent case --
        -- CR 704.5n's Equipment bearer, or an Aura sent anywhere but its owner's
        -- graveyard -- is visible as the empty map rather than as a wrong id.
        Spec.it s "CR 400.7f eventBindings binds the BEARER's graveyard incarnation, not the event's" $ do
          let departed = ObjectId.MkObjectId 1
              arrived = ObjectId.MkObjectId 2
              bearerArrived = ObjectId.MkObjectId 3
              hostDied = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics)
          Spec.assertEqWith
            s
            "became names the Aura's incarnation, not the host's"
            (Event.eventBindings (Just bearerArrived) TriggerCondition.AttachedCreatureDies hostDied)
            (Map.singleton Binding.became (Binding.toObject bearerArrived))
          Spec.assertEqWith s "and nothing at all where the bearer reached no graveyard" (Event.eventBindings Nothing TriggerCondition.AttachedCreatureDies hostDied) Map.empty
        -- The pin on Event.eventBindingSlots, the per-CONDITION slot set the
        -- card lint asks (CardSpec's "every slot a triggered ability reads is
        -- bound for its condition"). That function is a second statement of
        -- what eventBindings already says, and eventBindings cases on
        -- (condition, event) PAIRS, so nothing in the types keeps the two
        -- agreeing: a new binding arm added there and forgotten here would
        -- silently un-lint the new slot. Comparing the keys eventBindings
        -- actually produces against what the classification claims is what
        -- makes the drift a failing test.
        --
        -- The INTERSECTION over the events a condition admits, because the
        -- classification answers the guaranteed floor rather than the union: a
        -- slot the lint says is available must be bound for every event that
        -- could have placed the trigger. Two conditions have a list longer than
        -- one -- SelfLeavesTheBattlefield, where the two destinations disagree
        -- about `became`, and SelfIsDealtDamage, where CR 120.3's two damage
        -- kinds agree on `thatMuch` and so make the floor a real one; for every
        -- other the intersection is exactly that single event's keyset.
        --
        -- The BEARER ARRIVAL argument is held constant at a present one, and is
        -- not a second dimension of the intersection: it is CR 400.7f's datum
        -- rather than a shape the event can take, and eventBindingSlots'
        -- AttachedCreatureDies arm carries the argument that CR 704.5m makes it
        -- present for every bearer a printing can put under that condition.
        -- Holding it Nothing instead would pin the OTHER reading, under which no
        -- card could read `became` there at all.
        Spec.it s "CR 603.2 eventBindingSlots names exactly the keys eventBindings stamps for EVERY event a condition admits" $ do
          let bearerBecame = Just (ObjectId.MkObjectId 3)
          mapM_
            ( \cond ->
                let stamped = fmap (Map.keysSet . Event.eventBindings bearerBecame cond) (representativeEvents cond)
                 in Spec.assertEqWith s ("the slots bound for " <> show cond) (Event.eventBindingSlots cond) (foldr Set.intersection (NonEmpty.head stamped) (NonEmpty.tail stamped))
            )
            everyTriggerCondition

-- CR 400.7e's slot under a BYSTANDER's dies trigger, the last of the four
-- conditions that bind it (SelfDies in becameSlotSpec above,
-- SelfLeavesTheBattlefield in leavesBattlefieldSpec, PermanentEnters in
-- aetherFlashSpec below, and this): "Abilities that trigger when an object moves
-- from one zone to another ... can find the new object that it became in the
-- zone it moved to when the ability triggered, if that zone is a public zone."
--
-- Promise of Tomorrow, {2}{W} Enchantment, "Whenever a creature you control
-- dies, exile it." The bearer is a THIRD object -- neither the creature that
-- died nor its graveyard incarnation -- which is what makes "it" unambiguous
-- here where becameSlotSpec's Endless Cockroaches had to keep two incarnations
-- of one card apart. The card's OTHER ability, which reads CR 607.2a's link back
-- off what this one exiled, is promiseOfTomorrowReturnSpec below.
--
-- The discriminating assertion is WHICH id the payload moves. CR 603.10a makes
-- Event.matchesTrigger's PermanentDies arm match on ZoneChange.departed, so
-- "you control" is answerable from CR 608.2h last known information; but CR
-- 400.7 deleted that id when the creature died, so the effect has to be handed
-- ZoneChange.object -- the card now in the graveyard -- instead. Reading
-- `departed` here would leave the creature sitting in the graveyard, which is
-- exactly what assertions (a) and (b) below rule out.
--
-- CR 400.7e's public-zone proviso needs no guard: the PermanentDies arm has
-- already required battlefield-to-graveyard, and CR 400.2 lists the graveyard
-- among the public zones. SelfLeavesTheBattlefield is the condition where the
-- proviso does real work, and it is guarded there.
--
-- Goblin Piker is the victim rather than a token, deliberately: CR 111.7 makes
-- a token in a zone other than the battlefield cease to exist, so a token
-- exiled out of a graveyard would leave nothing to observe and (a) would pass
-- for the wrong reason.
--
-- Two seats, because "a creature YOU control" needs a creature somebody else
-- controls to be separated from. Bob's Ogre Sentry standing untouched is that
-- separation.
promiseOfTomorrowSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
promiseOfTomorrowSpec s registry =
  let promiseBoard = do
        promise <- S.printingOf s registry "Promise of Tomorrow"
        piker <- S.printingOf s registry "Goblin Piker"
        sentry <- S.printingOf s registry "Ogre Sentry"
        let empty = Setup.emptyGame S.bothPlayers
            (_, withPromise) = S.addCreature promise S.alice empty
            (pikerId, withPiker) = S.addCreature piker S.alice withPromise
            (sentryId, withSentry) = S.addCreature sentry S.bob withPiker
        pure (pikerId, sentryId, withSentry)
      -- Destroy alice's creature outright (CR 701.8a), settle so the CR 117.5
      -- boundary scans the death and places the trigger, then resolve it.
      killIt oid gs =
        let killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [oid])
            settled = S.runPure S.identityAnswer killed Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      namesIn zone pid gs =
        fmap Face.name (Maybe.mapMaybe (\oid -> Game.faceOf oid gs) (Game.zoneMembers zone pid gs))
      pikerName = CardName.MkCardName $ Text.pack "Goblin Piker"
   in Spec.describe s "CR 400.7e the card a BYSTANDER's dies trigger names" $ do
        Spec.it s "CR 700.4 whole card: alice's Goblin Piker dies and Promise of Tomorrow exiles the graveyard card" $ do
          (pikerId, sentryId, board) <- promiseBoard
          let (settled, after) = killIt pikerId board
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject pikerId settled)) "the battlefield id is gone (CR 400.7)"
          Spec.assertEqWith s "and the card is in the graveyard when the trigger is placed" (namesIn Zone.Graveyard S.alice settled) [pikerName]
          -- (a), (b) and (c) as one tuple. (a) and (b) are the same fact from
          -- both sides: an effect handed ZoneChange.departed would move an id
          -- CR 400.7 deleted, so the exile would be empty and the graveyard
          -- would still hold the Piker. (c) is the "you control" separation.
          Spec.assertEqWith
            s
            "exiled, out of the graveyard, and bob's creature untouched"
            (namesIn Zone.Exile S.alice after, namesIn Zone.Graveyard S.alice after, Set.member sentryId (GameState.battlefield after))
            ([pikerName], [], True)
        -- eventBindings in isolation, so the binding is pinned to CR 400.7e
        -- rather than to Promise of Tomorrow's payload. The contrast with the
        -- SelfDies arm above is only in which object the BEARER is; the slot
        -- names ZoneChange.object either way.
        Spec.it s "CR 400.7e eventBindings binds the ARRIVING id for PermanentDies too" $ do
          let departed = ObjectId.MkObjectId 1
              arrived = ObjectId.MkObjectId 2
              died = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics)
          Spec.assertEqWith s "became names the graveyard incarnation" (Event.eventBindings Nothing (TriggerCondition.PermanentDies (Filter.Type.HasCardType CardType.Creature)) died) (Map.singleton Binding.became (Binding.toObject arrived))

-- CR 607.2a's linked pair read from the SECOND ability, which is the half
-- promiseOfTomorrowSpec above could not reach: "At the beginning of each end
-- step, if you control no creatures, sacrifice this enchantment and return all
-- cards exiled with it to the battlefield under your control."
--
-- Three separations, each a different way the return could be wrong:
--
--   * IDENTITY, not count. Wall of Stone sits in exile from the start, put there
--     by no ability at all, so GameState.exiledWith files it against nothing.
--     A return that swept exile rather than the linked set would bring it back.
--
--   * CONTROL, not ownership. The Ogre Sentry is BOB's card under alice's
--     control when it dies, so CR 110.2a's "unless the effect states otherwise"
--     is observable: "under your control" hands it to alice, while
--     EntryRiders.underOwner would hand it to bob. The Goblin Piker, which alice
--     owns, cannot tell those apart on its own.
--
--   * "YOU control no creatures", not "no creatures". Bob's Hill Giant is on the
--     battlefield in BOTH legs, so a CR 603.4 clause reading the whole
--     battlefield would never fire at all.
--
-- The negative leg is the same board with the Hill Giant under ALICE instead of
-- bob -- one difference, and it is the clause's subject. Alice then still
-- controls a creature when the end step begins, the ability does not trigger,
-- and nothing moves: Promise stays on the battlefield and both cards stay in
-- exile.
--
-- The two deaths are dealt one at a time so that exactly one trigger is on the
-- stack per settle; two at once would make CR 603.3b's ordering a question the
-- fixture answerer would have to answer, and the order is not what is under
-- test.
--
-- WHAT IS NOT COVERED: an exiled card that leaves exile by some other means
-- before the return. Resolve.recordExiledWith drops such a key on its next
-- window (its own comment carries the argument, and CR 400.7 mints the departed
-- card a new id besides), but no card in the pool can pull a card out of exile
-- between Promise's two abilities, so there is nothing to drive it with. The
-- source leaving the battlefield first IS covered, and by construction: the
-- sacrifice runs BEFORE the return in the same resolution, so every assertion
-- below is already read off a board where the linking object is in the
-- graveyard.
promiseOfTomorrowReturnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
promiseOfTomorrowReturnSpec s registry =
  let board giantUnder = do
        promise <- S.printingOf s registry "Promise of Tomorrow"
        piker <- S.printingOf s registry "Goblin Piker"
        sentry <- S.printingOf s registry "Ogre Sentry"
        giant <- S.printingOf s registry "Hill Giant"
        wall <- S.printingOf s registry "Wall of Stone"
        let empty = Setup.emptyGame S.bothPlayers
            (promiseId, withPromise) = S.addCreature promise S.alice empty
            (pikerId, withPiker) = S.addCreature piker S.alice withPromise
            -- Bob's card, alice's permanent (CR 108.4).
            (sentryId, withSentry) = S.addCreature sentry S.bob withPiker
            stolen = S.giveControl sentryId S.alice withSentry
            (giantId, withGiant) = S.addCreature giant S.bob stolen
            seated = if giantUnder == S.alice then S.giveControl giantId S.alice withGiant else withGiant
            (wallId, withWall) = S.addExiledCard wall S.alice seated
        pure (promiseId, pikerId, sentryId, wallId, withWall)
      -- Destroy one permanent (CR 701.8a), settle so the CR 117.5 boundary scans
      -- the death and places Promise's dies trigger, then resolve that trigger.
      killIt oid gs =
        let killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [oid])
            settled = S.runPure S.identityAnswer killed Engine.settleForPriority
         in S.runPure S.identityAnswer settled Stack.resolveTop
      -- Record the end step's beginning, place what it gathers (CR 603.3), and
      -- resolve the one ability that can be there. BOTH states come back: the
      -- stack is empty after the resolution either way, so "the ability did not
      -- trigger" is only readable at the placement.
      endStepOf gs =
        let began = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice)] gs
            placed = S.runPure S.identityAnswer began Engine.settleForPriority
         in (placed, S.runPure S.identityAnswer placed Stack.resolveTop)
      nameOf oid gs = fmap Face.name (Game.faceOf oid gs)
      -- The battlefield permanents whose PROJECTED controller is pid, by name.
      -- Sorted, since GameState.battlefield is a set and the returned cards are
      -- minted fresh ids in no order the card states.
      controlledNames pid gs =
        List.sort (Maybe.mapMaybe (\oid -> if Projection.controllerOf oid gs == Just pid then nameOf oid gs else Nothing) (Set.toList (GameState.battlefield gs)))
      -- Everything in exile by name, whoever owns it -- CR 400.1 makes exile one
      -- shared zone, and the Sentry there is bob's card.
      exiledNames gs = List.sort (Maybe.mapMaybe (`nameOf` gs) (Set.toList (GameState.exile gs)))
      -- Both graveyards, since CR 400.1 makes the graveyard a per-player zone and
      -- the Sentry is bob's card.
      namesInGraveyards gs = List.sort (concatMap (\pid -> Maybe.mapMaybe (`nameOf` gs) (Game.zoneMembers Zone.Graveyard pid gs)) (NonEmpty.toList S.bothPlayers))
      named = CardName.MkCardName . Text.pack
   in Spec.describe s "CR 607.2a Promise of Tomorrow returns the cards it exiled" $ do
        Spec.it s "CR 700.4 whole card: the linked cards come back under alice, the unlinked one stays in exile" $ do
          (promiseId, pikerId, sentryId, wallId, board0) <- board S.bob
          let exiled = killIt sentryId (killIt pikerId board0)
          -- The premise, stated so a failure below cannot be a failure of the
          -- first ability wearing the second's clothes.
          Spec.assertEqWith s "both deaths were exiled, alongside the decoy" (exiledNames exiled) (fmap named ["Goblin Piker", "Ogre Sentry", "Wall of Stone"])
          Spec.assertEqWith s "and alice controls no creatures" (controlledNames S.alice exiled) [named "Promise of Tomorrow"]
          Spec.assertEqWith s "while bob still has one on the battlefield" (controlledNames S.bob exiled) [named "Hill Giant"]
          let (placed, after) = endStepOf exiled
          Spec.assertEqWith s "CR 603.4: the ability triggered" (length (GameState.stack placed)) 1
          Spec.assertBool s (not (S.onBattlefield promiseId after)) "CR 701.21a: the enchantment sacrificed itself"
          Spec.assertEqWith s "and it is in alice's graveyard" (fmap Face.name (Maybe.mapMaybe (`Game.faceOf` after) (Game.zoneMembers Zone.Graveyard S.alice after))) [named "Promise of Tomorrow"]
          -- The three separations, as one tuple so a failure says which.
          Spec.assertEqWith
            s
            "the linked pair returned under alice, the decoy stayed, bob kept only his Giant"
            (controlledNames S.alice after, exiledNames after, controlledNames S.bob after)
            (fmap named ["Goblin Piker", "Ogre Sentry"], [named "Wall of Stone"], [named "Hill Giant"])
          -- CR 400.7: what came back is a NEW object, which is why the assertion
          -- above reads names rather than ids. `pikerId` and `sentryId` would
          -- have been the wrong ids twice over -- the battlefield incarnations
          -- died two moves ago -- so the ids compared here are the ones exile
          -- actually held.
          Spec.assertBool s (Set.disjoint (GameState.exile exiled) (GameState.battlefield after)) "no exiled id is on the battlefield"
          Spec.assertBool s (Maybe.isJust (Game.lookupObject wallId after)) "and the decoy, which never moved, keeps its id"
        -- The one-difference control: bob's Hill Giant moves to alice, so alice
        -- controls a creature at the beginning of the end step and CR 603.4 stops
        -- the ability triggering at all.
        Spec.it s "CR 603.4 control: alice keeping ANY creature leaves the exiled cards where they are" $ do
          (promiseId, pikerId, sentryId, _, board0) <- board S.alice
          let exiled = killIt sentryId (killIt pikerId board0)
          Spec.assertEqWith s "exile holds the same three cards" (exiledNames exiled) (fmap named ["Goblin Piker", "Ogre Sentry", "Wall of Stone"])
          Spec.assertEqWith s "but alice still controls the Giant" (controlledNames S.alice exiled) (fmap named ["Hill Giant", "Promise of Tomorrow"])
          let (placed, after) = endStepOf exiled
          Spec.assertEqWith s "CR 603.4: nothing was placed on the stack" (length (GameState.stack placed)) 0
          Spec.assertBool s (S.onBattlefield promiseId after) "the enchantment is still there"
          Spec.assertEqWith s "and exile is unchanged" (exiledNames after) (fmap named ["Goblin Piker", "Ogre Sentry", "Wall of Stone"])
        -- CR 607.2b: the SAME resolution, with Rest in Peace on the battlefield
        -- so that the sacrifice in Promise's own effect list is replaced by an
        -- exile. That exile is a "direct result of a replacement event caused
        -- by" Rest in Peace, so the Promise CARD links to Rest in Peace and not
        -- to the Promise permanent whose ability is resolving; CR 607.2a's link
        -- is scoped to cards exiled "as a result of an instruction to exile them
        -- in the first ability", which this is not. Filed against the resolving
        -- source, the enchantment would return itself to the battlefield out of
        -- its own exile.
        --
        -- Rest in Peace is placed AFTER both deaths, not on the starting board:
        -- its replacement would otherwise exile the Piker and the Sentry on the
        -- way to the graveyard, so neither would die, Promise's first ability
        -- would never trigger, and the linked set would be empty -- which is
        -- also the control this leg needs, since an over-narrowed fix that filed
        -- nothing at all would leave the battlefield just as empty of returns.
        -- Placed rather than cast, so its own "exile all graveyards" trigger
        -- does not fire and file a second set of arrivals.
        Spec.it s "CR 607.2b the card Rest in Peace's replacement exiles is linked to IT, not to the resolving enchantment" $ do
          (promiseId, pikerId, sentryId, wallId, board0) <- board S.bob
          rip <- S.printingOf s registry "Rest in Peace"
          let exiled = killIt sentryId (killIt pikerId board0)
              (ripId, guarded) = S.addCreature rip S.alice exiled
              (placed, after) = endStepOf guarded
          Spec.assertEqWith s "CR 603.4: the ability still triggered" (length (GameState.stack placed)) 1
          Spec.assertBool s (not (S.onBattlefield promiseId after)) "CR 701.21a: the enchantment sacrificed itself"
          -- The discriminating pair. Under the ambient diff the Promise card is
          -- filed against its own permanent, so it comes back on the battlefield
          -- and leaves exile; under CR 607.2b it is Rest in Peace's, so it stays
          -- in exile beside the decoy and only the two cards Promise itself
          -- exiled return.
          Spec.assertEqWith
            s
            "only the cards Promise itself exiled returned, and the replaced sacrifice stayed in exile"
            (controlledNames S.alice after, exiledNames after)
            (fmap named ["Goblin Piker", "Ogre Sentry", "Rest in Peace"], fmap named ["Promise of Tomorrow", "Wall of Stone"])
          -- The falsifier: with Rest in Peace out, nothing can be in a graveyard
          -- at all, so a green result above cannot be a sacrifice that quietly
          -- went to the graveyard instead of being replaced.
          Spec.assertEqWith s "CR 614.6: the sacrifice was replaced, so no graveyard holds anything" (namesInGraveyards after) []
          Spec.assertBool s (Maybe.isJust (Game.lookupObject wallId after)) "and the decoy, which never moved, keeps its id"
          Spec.assertBool s (S.onBattlefield ripId after) "Rest in Peace itself never moved"

-- CR 603.4's intervening "if" on a LOOK-BACK trigger, which is the one shape
-- where the clause has to be read against an object that no longer exists:
-- "When the trigger event occurs, the ability checks whether the stated
-- condition is true. The ability triggers only if it is." CR 608.2a repeats the
-- check as the ability resolves.
--
-- Deathknell Berserker, {1}{B} Creature -- Elf Berserker 2/2: "When this
-- creature dies, if its power was 3 or greater, create a 2/2 black Zombie
-- Berserker creature token." Both readings of "its power" that are available
-- without CR 608.2h are wrong -- the id is gone, so a live projection describes
-- nothing at all, and the card sitting in the graveyard has its printed 2.
-- Only last known information answers 3.
--
-- Bad Moon supplies the third power, and it does so through the LAYERS
-- (CR 613.4c, layer 7c), which is what makes the graveyard card's printed value
-- visibly the wrong answer rather than merely a different route to the same one.
lookBackInterveningSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lookBackInterveningSpec s registry =
  let berserkerBoard withBadMoon = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        berserker <- S.printingOf s registry "Deathknell Berserker"
        badMoon <- S.printingOf s registry "Bad Moon"
        let lands = S.landsInPlay mountain 1
            moonAdded = if withBadMoon then snd (S.addCreature badMoon S.alice lands) else lands
            (berserkerId, withBerserker) = S.addCreature berserker S.alice moonAdded
        pure (berserkerId, S.handOne lightningBolt withBerserker)
      -- The Bolt targets the least Recipient, and S.addCreature hands out
      -- ascending ids, so Bad Moon (added first) would sort before the
      -- Berserker if it were a legal target -- it is an enchantment, and
      -- Lightning Bolt's pool is AnyTarget, so the Berserker is the only
      -- creature and the Bolt finds it.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      tokensOf pid gs =
        filter
          -- CR 111.4: the name is BOTH subtypes plus "Token", which is exactly
          -- the rule's own Dwarven Reinforcements example.
          (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Zombie Berserker Token"))
          (Game.zoneMembers Zone.Battlefield pid gs)
   in Spec.describe s "CR 603.4 an intervening if over last known information" $ do
        Spec.it s "CR 603.4 with Bad Moon the Berserker died at power 3 and its trigger fires" $ do
          (berserkerId, board) <- berserkerBoard True
          let (settled, after) = boltIt board
          Spec.assertEqWith s "it was a 3/3 while it lived" (Projection.powerOf berserkerId (fst board)) (Just 3)
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
          case tokensOf S.alice after of
            [token] -> do
              -- Printed 2/2, and 3/3 on this board: the token is black, so
              -- the same Bad Moon that made its maker a 3/3 pumps it in turn
              -- (CR 613.4c, layer 7c). Asserting the projection rather than
              -- the printed pair is what keeps the two facts from being
              -- confused for one another.
              Spec.assertEqWith s "printed 2/2" (maybe (Nothing, Nothing) (\f -> (Face.power f, Face.toughness f)) (Game.faceOf token after)) (Just (Power.MkPower (Quantity.Type.Literal 2)), Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
              Spec.assertEqWith s "3/3 under Bad Moon" (Projection.powerOf token after, Projection.toughnessOf token after) (Just 3, Just 3)
              Spec.assertEqWith s "black" (Projection.colorsOf token after) (Set.singleton Color.Black)
              Spec.assertEqWith s "Zombie Berserker" (Projection.subtypesOf token after) (Set.fromList [Subtype.Zombie, Subtype.Berserker])
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- CR 603.4's "otherwise it does nothing" -- and "does nothing" means
        -- the ability never reaches the stack at all, not that it resolves to
        -- no effect.
        Spec.it s "CR 603.4 without Bad Moon it died at power 2 and does not trigger at all" $ do
          (berserkerId, board) <- berserkerBoard False
          let (settled, after) = boltIt board
          Spec.assertEqWith s "a 2/2 while it lived" (Projection.powerOf berserkerId (fst board)) (Just 2)
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack settled) []
          Spec.assertEqWith s "and no token was made" (tokensOf S.alice after) []

-- The same rule read against what CR 613 does NOT leave behind. CR 122.1a folds
-- a +1/+1 counter into the object's power and toughness at layer 7c, so a
-- projection taken as the creature died records the RESULT of the counter and
-- not the counter -- and "if it had a +1/+1 counter on it" is a question about
-- the counter. LastKnown.counters is where the counter itself is filed, and
-- Quantity.ObjectCounters is what reads it back.
--
-- Promising Duskmage, {2}{B} Creature -- Human Warlock 2/3: "When this creature
-- dies, if it had a +1/+1 counter on it, draw a card."
--
-- Murder rather than a damage spell does the killing on purpose: the counter
-- moves the Duskmage from 2/3 to 3/4, so any lethal-damage removal would need a
-- different amount per leg and the two legs would stop being one fixture. CR
-- 701.8a's destroy does not care what the toughness is.
--
-- Both of CR 603.4's reads are covered, and separately:
--
--   * the GATHER read (Event.interveningHolds) by the two legs -- the trigger
--     reaches the stack with the counter and does not reach it without.
--   * the RESOLUTION read (CR 608.2a, Pawl.Engine.Stack's OfTrigger arm) by the
--     third case, which lets the trigger onto the stack and then empties the
--     record it reads, so a wired re-check removes the ability and an unwired
--     one draws anyway.
counterLookBackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
counterLookBackSpec s registry =
  let duskmageBoard withCounter = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        duskmage <- S.printingOf s registry "Promising Duskmage"
        let lands = S.landsInPlay swamp 3
            (duskmageId, withDuskmage) = S.addCreature duskmage S.alice lands
            countered =
              if withCounter
                then S.addCounter CounterKind.PlusOnePlusOne 1 duskmageId withDuskmage
                else withDuskmage
            -- CR 104.3c: five cards is more library than any leg can draw
            -- through, so nothing here loses the game before the assertion runs.
            -- No leg advances a turn either, so no draw step spends one.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) countered [1 .. 5 :: Int]
        pure (duskmageId, S.handOne murder stocked)
      -- Murder's pool is Pool.Creatures and the Duskmage is the only creature on
      -- the board, so identityAnswer's least Recipient is it.
      murderIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)
      -- The COUNTER, not the power it produces. CR 122.1a makes 3/4 the visible
      -- consequence of the +1/+1 counter and this trigger asks about neither the
      -- 3 nor the 4, so the two facts are asserted apart.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "CR 603.4 an intervening if over last known COUNTERS" $ do
        Spec.it s "CR 122.1 with a +1/+1 counter the Duskmage's death trigger draws a card" $ do
          (duskmageId, board) <- duskmageBoard True
          let (settled, after) = murderIt board
          Spec.assertEqWith s "it had one +1/+1 counter while it lived" (Map.lookup CounterKind.PlusOnePlusOne (countersOn duskmageId (fst board))) (Just 1)
          Spec.assertEqWith s "and CR 122.1a made it a 3/4, which is a DIFFERENT fact" (Projection.powerOf duskmageId (fst board), Projection.toughnessOf duskmageId (fst board)) (Just 3, Just 4)
          Spec.assertEqWith s "one card in hand and five in library to start" (S.handSize S.alice (fst board), librarySize S.alice (fst board)) (1, 5)
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "the Duskmage is in the graveyard" (Set.member duskmageId (GameState.battlefield after)) False
          -- The DELTA: the Murder left the hand and one card arrived from the
          -- library. Zero in hand and five in library is the other leg's answer,
          -- so the two cannot be satisfied by one implementation.
          Spec.assertEqWith s "one card drawn" (S.handSize S.alice after, librarySize S.alice after) (1, 4)
        -- CR 603.4's "otherwise it does nothing": without the counter the
        -- ability never triggers at all.
        Spec.it s "CR 603.4 with no counter the same death draws nothing" $ do
          (duskmageId, board) <- duskmageBoard False
          let (settled, after) = murderIt board
          Spec.assertEqWith s "no counters on it at all" (countersOn duskmageId (fst board)) Map.empty
          Spec.assertEqWith s "a plain 2/3" (Projection.powerOf duskmageId (fst board), Projection.toughnessOf duskmageId (fst board)) (Just 2, Just 3)
          Spec.assertEqWith s "one card in hand and five in library to start" (S.handSize S.alice (fst board), librarySize S.alice (fst board)) (1, 5)
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack settled) []
          Spec.assertEqWith s "the Duskmage is in the graveyard just the same" (Set.member duskmageId (GameState.battlefield after)) False
          Spec.assertEqWith s "no card drawn" (S.handSize S.alice after, librarySize S.alice after) (0, 5)

        -- CR 608.2a on its own. The record the resolution re-check reads is
        -- emptied while the trigger sits on the stack -- something no rule can
        -- do to last known information, which is exactly why it isolates the
        -- second read: only a wired re-check can notice.
        Spec.it s "CR 608.2a the intervening if is checked AGAIN as the ability resolves" $ do
          (duskmageId, board) <- duskmageBoard True
          let cast = S.runPure S.identityAnswer (fst board) (S.cast S.alice (snd board))
              destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
              settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
              forgotten =
                settled
                  { GameState.lastKnown =
                      Map.adjust
                        (\lk -> lk {LastKnown.counters = Map.empty})
                        duskmageId
                        (GameState.lastKnown settled)
                  }
              after = S.runPure S.identityAnswer forgotten Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack on the gather read" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and was removed on the resolution read, drawing nothing" (S.handSize S.alice after, librarySize S.alice after) (0, 5)
          Spec.assertEqWith s "the stack is empty either way" (GameState.stack after) []

-- CR 702.93a undying and CR 702.79a persist: one sentence in two counter kinds,
-- minted by Pawl.Engine.Keyword.returns.
--
-- Young Wolf, {G} Creature -- Wolf 1/1, and Putrid Goblin, {1}{B} Creature --
-- Zombie Goblin 2/2. Each keyword is the card's whole text box, so nothing else
-- printed there can be producing the return.
--
-- KILLED TWICE, rather than the counter being placed by hand as
-- counterLookBackSpec's Duskmage does: the second death is what proves the
-- counter actually arrived, since CR 603.4's "if it had no counters" reads it off
-- CR 608.2h last known information. Murder rather than damage, for that spec's
-- reason -- the two deaths are at different toughnesses.
--
-- The two keywords are one group because rules 702.79a and 702.93a are one
-- sentence: what separates the legs is the counter kind, and persist's is the one
-- that makes the returned permanent SMALLER (CR 122.1a).
undyingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
undyingSpec s registry =
  let -- The named creature on alice's board with two Murders in hand and six
      -- Swamps, which is {1}{B}{B} twice with nothing left over. No untap step
      -- runs between the two casts.
      board name = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        creature <- S.printingOf s registry name
        let (oid, withCreature) = S.addCreature creature S.alice (S.landsInPlay swamp 6)
            (gs1, firstMurder) = S.handOne murder withCreature
            (secondMurder, gs2) = S.addHandCard murder S.alice gs1
        pure (oid, gs2, firstMurder, secondMurder)
      -- Cast the Murder, resolve it, settle -- CR 704.5g buries the creature and
      -- the same CR 117.5 scan sees the death -- then resolve what it placed.
      murderWith spellId gs =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      named name gs = filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack name))) (Set.toList (GameState.battlefield gs))
      inGraveyard name pid gs = elem (CardName.MkCardName (Text.pack name)) (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers Zone.Graveyard pid gs))
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- One leg per keyword: die, come back with the counter at the stated size,
      -- die again, stay dead.
      twiceKilled name kind power toughness =
        Spec.it s ("CR 702 " <> name <> " returns once, with its counter, and not a second time") $ do
          (firstId, gs, m1, m2) <- board name
          let (settled, after) = murderWith m1 gs
          Spec.assertEqWith s "the dies trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject firstId after)) "CR 400.7: the permanent that died is a spent id"
          case named name after of
            [backId] -> do
              Spec.assertEqWith s "it entered with exactly one counter, of that kind" (countersOn backId after) (Map.singleton kind 1)
              Spec.assertEqWith s "CR 122.1a resizes it" (Projection.powerOf backId after, Projection.toughnessOf backId after) (Just power, Just toughness)
              Spec.assertEqWith s "CR 110.2a: under its owner's control" (Projection.controllerOf backId after) (Just S.alice)
              Spec.assertBool s (not (inGraveyard name S.alice after)) "and no longer in the graveyard"
              let (settled2, after2) = murderWith m2 after
              Spec.assertEqWith s "CR 603.4: with the counter on it, the second death triggers nothing" (GameState.stack settled2) []
              Spec.assertEqWith s "so nothing comes back" (named name after2) []
              Spec.assertBool s (inGraveyard name S.alice after2) "and it stays in the graveyard"
            other -> Spec.assertFailure s ("expected exactly one " <> name <> " back on the battlefield, got " <> show other)
   in Spec.describe s "CR 702.93 undying and CR 702.79 persist" $ do
        twiceKilled "Young Wolf" CounterKind.PlusOnePlusOne 2 2
        twiceKilled "Putrid Goblin" CounterKind.MinusOneMinusOne 1 1
        -- CR 110.2a's "unless the effect states otherwise". Alice steals bob's
        -- Wolf and kills it, so CR 603.3a hands ALICE the dies trigger -- which
        -- is what makes this discriminating, since a return under the ability's
        -- controller and a return under the owner are the same board at one
        -- seat. Lightning Bolt does the killing so the whole fixture is red.
        Spec.it s "CR 110.2a a stolen Young Wolf comes back under its OWNER's control" $ do
          mountain <- S.printingOf s registry "Mountain"
          treason <- S.printingOf s registry "Act of Treason"
          bolt <- S.printingOf s registry "Lightning Bolt"
          wolf <- S.printingOf s registry "Young Wolf"
          let (wolfId, withWolf) = S.addCreature wolf S.bob (S.landsInPlay mountain 4)
              (gs1, treasonId) = S.handOne treason withWolf
              (boltId, gs2) = S.addHandCard bolt S.alice gs1
              stolen = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs2 (S.cast S.alice treasonId)) Stack.resolveTop
              burned = S.runPure S.identityAnswer (S.runPure S.identityAnswer stolen (S.cast S.alice boltId)) Stack.resolveTop
              settled = S.runPure S.identityAnswer burned Engine.settleForPriority
              after = S.runPure S.identityAnswer settled Stack.resolveTop
          Spec.assertEqWith s "alice controls the Wolf when it dies" (Projection.controllerOf wolfId stolen) (Just S.alice)
          Spec.assertEqWith s "CR 603.3a: so the dies trigger is alice's" (fmap (\oid -> Projection.controllerOf oid settled) (GameState.stack settled)) [Just S.alice]
          case named "Young Wolf" after of
            [backId] -> do
              Spec.assertEqWith s "and it returns under bob's" (Projection.controllerOf backId after) (Just S.bob)
              Spec.assertEqWith s "with its +1/+1 counter" (countersOn backId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            other -> Spec.assertFailure s ("expected the Wolf back on the battlefield, got " <> show other)

-- CR 702.135a afterlife N: "When this permanent is put into a graveyard from the
-- battlefield, create N 1/1 white and black Spirit creature tokens with flying."
-- Minted by Pawl.Engine.Keyword.afterlife on the same TriggerCondition.SelfDies
-- undying and persist take, and the FIRST minted keyword ability that creates a
-- token -- so what is under test is a whole card, minted in the engine rather
-- than read from card data.
--
-- Ministrant of Obligation, {2}{W} Creature -- Human Cleric 2/1, whose entire
-- text box is "Afterlife 2". Nothing else printed on it can be making tokens,
-- and N is 2 rather than 1 so a mint that ignored the keyword's payload and
-- created one token would fail.
--
-- Every characteristic rule 702.135a states is asserted, because the mint writes
-- each of them out by hand and a wrong one compiles: 1/1, both colours, the
-- Spirit creature type and flying. Both colours matter most -- the pool's other
-- Spirit token, Doomed Traveler's, is white alone, so a mint copied from it
-- would pass everything else.
afterlifeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
afterlifeSpec s registry =
  let spiritsAfterKilling name = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        creature <- S.printingOf s registry name
        let (oid, withCreature) = S.addCreature creature S.alice (S.landsInPlay swamp 3)
            (gs, spellId) = S.handOne murder withCreature
            cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
        pure (oid, settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      spirits gs =
        filter
          (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Spirit Token")))
          (Set.toList (GameState.battlefield gs))
   in Spec.describe s "CR 702.135 afterlife" $ do
        Spec.it s "CR 702.135a a dying Ministrant of Obligation leaves two 1/1 white and black flying Spirits" $ do
          (ministrantId, settled, after) <- spiritsAfterKilling "Ministrant of Obligation"
          Spec.assertEqWith s "the dies trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject ministrantId after)) "CR 400.7: the permanent that died is a spent id"
          case spirits after of
            [first, second] -> do
              let describes oid =
                    ( Projection.powerOf oid after,
                      Projection.toughnessOf oid after,
                      Projection.colorsOf oid after,
                      Projection.subtypesOf oid after,
                      Projection.cardTypesOf oid after,
                      Projection.hasKeyword Keyword.Type.Flying oid after,
                      Projection.controllerOf oid after
                    )
                  expected =
                    ( Just (1 :: Integer),
                      Just (1 :: Integer),
                      Set.fromList [Color.White, Color.Black],
                      Set.singleton Subtype.Spirit,
                      Set.singleton CardType.Creature,
                      True,
                      Just S.alice
                    )
              Spec.assertEqWith s "the first is rule 702.135a's token exactly" (describes first) expected
              Spec.assertEqWith s "and so is the second" (describes second) expected
            other -> Spec.assertFailure s ("expected exactly two Spirit tokens, got " <> show other)
        -- The other half of the same board: a creature WITHOUT afterlife dying to
        -- the same Murder leaves nothing behind, so the tokens above are the
        -- keyword's doing and not the fixture's.
        Spec.it s "CR 702.135a a dying creature without afterlife leaves none" $ do
          (pikerId, settled, after) <- spiritsAfterKilling "Goblin Piker"
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject pikerId after)) "the Piker died to the same Murder"
          Spec.assertEqWith s "but nothing triggered" (GameState.stack settled) []
          Spec.assertEqWith s "and no tokens were created" (spirits after) []
        -- CR 702.135b: "if a permanent has multiple instances of afterlife, each
        -- triggers separately". Asserted of the MINT, as renown's multiplicity
        -- is, no card in the pool printing afterlife twice.
        Spec.it s "CR 702.135b each instance of afterlife is its own ability" $ do
          Spec.assertEqWith s "afterlife 2 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afterlife 2) 2)) [Keyword.afterlife 2, Keyword.afterlife 2]
          Spec.assertEqWith s "and afterlife 3 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afterlife 3) 1)) [Keyword.afterlife 3]
          Spec.assertBool s (Keyword.afterlife 2 /= Keyword.afterlife 3) "and the N reaches the minted ability"

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

-- The pay-or-not answers in a transcript, in order.
payResponses :: [Response.Response] -> [Response.Response]
payResponses = filter isPayResponse

isPayResponse :: Response.Response -> Bool
isPayResponse response = case response of
  Response.ChoseToPay _ -> True
  _ -> False

-- CR 702.123 fabricate N: "When this permanent enters, you may put N +1/+1
-- counters on it. If you don't, create N 1/1 colorless Servo artifact creature
-- tokens." Rule 702.123a prints CR 118.12a's rewriting already done, so the
-- minted clause is one PayGate over
-- CostComponent.PutPlusOneCountersOnThis and the tokens are its "if you don't"
-- branch -- afterlife's mint with a gate on it, and the first minted keyword
-- ability that offers a COST at resolution. (Soulshift's and provoke's clauses
-- ask a question there too, but a printed "may" rather than a cost.)
--
-- Glint-Sleeve Artisan, {2}{W} Creature -- Dwarf Artificer 2/2, whose entire
-- text box is "Fabricate 1". Every reading is a different board: 3/3 with the
-- counter, 2/2 plus a Servo without it, 4/4 under Hardened Scales.
--
-- The first two cases start from the SAME board and the SAME settled trigger and
-- differ in NOTHING but alice's answer.
fabricateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fabricateSpec s registry =
  let named name gs = filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack name))) (Set.toList (GameState.battlefield gs))
      artisansOn = named "Glint-Sleeve Artisan"
      -- The bearer cast from alice's hand off three lands and resolved, with its
      -- CR 603.6a enters trigger settled onto the stack but NOT resolved.
      -- `others` go onto alice's battlefield first.
      entersOnStack land bearer others =
        let base = List.foldl' (\g p -> snd (S.addCreature p S.alice g)) (S.landsInPlay land 3) others
            (gs, spellId) = S.handOne bearer base
            cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
         in S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
      boardOf landName bearerName others = do
        land <- S.printingOf s registry landName
        bearer <- S.printingOf s registry bearerName
        rest <- traverse (S.printingOf s registry) others
        pure (entersOnStack land bearer rest)
      board = boardOf "Plains" "Glint-Sleeve Artisan"
   in Spec.describe s "CR 702.123 fabricate" $ do
        Spec.it s "CR 702.123a paying the counter leaves the Artisan a 3/3 and makes no Servo" $ do
          onStack <- board []
          let ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
          case artisansOn onStack of
            [artisanId] -> do
              -- The controls: the Artisan really entered, and its trigger really
              -- reached the stack, before anything below is read.
              Spec.assertEqWith s "it entered as a 2/2 with no counters" (S.powerToughnessOf artisanId onStack, S.counterOf CounterKind.PlusOnePlusOne artisanId onStack) (Just (2, 2), 0)
              Spec.assertEqWith s "CR 603.6a: its enters trigger is on the stack" (length (GameState.stack onStack)) 1
              Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
              Spec.assertEqWith s "CR 122.6: one +1/+1 counter went on" (S.counterOf CounterKind.PlusOnePlusOne artisanId after) 1
              Spec.assertEqWith s "CR 613.4c: so it reads 3/3" (S.powerToughnessOf artisanId after) (Just (3, 3))
              Spec.assertEqWith s "CR 118.12a: the paid branch made no Servo" (S.tokensOf after) []
            other -> Spec.assertFailure s ("expected one Glint-Sleeve Artisan, got " <> show (length other))
        -- Every characteristic rule 702.123a states is asserted, because the mint
        -- writes each of them out by hand and a wrong one compiles. Colorless
        -- matters most: the pool's other minted token, afterlife's Spirit, is
        -- white and black, so a mint copied from it would pass everything else.
        Spec.it s "CR 702.123a declining creates a 1/1 colorless Servo artifact creature" $ do
          onStack <- board []
          let ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
          case artisansOn onStack of
            [artisanId] -> do
              Spec.assertEqWith s "alice was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
              Spec.assertEqWith s "no counter went on" (S.counterOf CounterKind.PlusOnePlusOne artisanId after) 0
              Spec.assertEqWith s "so it is still a 2/2" (S.powerToughnessOf artisanId after) (Just (2, 2))
              case S.tokensOf after of
                [servoId] ->
                  Spec.assertEqWith
                    s
                    "the token is rule 702.123a's exactly"
                    ( fmap Face.name (Game.faceOf servoId after),
                      S.powerToughnessOf servoId after,
                      Projection.colorsOf servoId after,
                      Projection.cardTypesOf servoId after,
                      Projection.subtypesOf servoId after,
                      Projection.controllerOf servoId after
                    )
                    ( -- CR 111.4 names it, CR 105.2 makes an object with no mana
                      -- cost and no colour indicator colorless, and CR 111.2 gives
                      -- it to the ability's controller.
                      Just (CardName.MkCardName (Text.pack "Servo Token")),
                      Just (1, 1),
                      Set.empty,
                      Set.fromList [CardType.Artifact, CardType.Creature],
                      Set.singleton Subtype.Servo,
                      Just S.alice
                    )
                other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
            other -> Spec.assertFailure s ("expected one Glint-Sleeve Artisan, got " <> show (length other))
        -- CR 614.16 over a cost paid DURING a resolution: the board differs from
        -- the first case in nothing but the Hardened Scales, and it applies,
        -- because CR 118.12 pays this cost as the ability resolves and CR 609.1
        -- makes what happens then an effect of that ability.
        Spec.it s "CR 614.16 Hardened Scales sees fabricate's counter, so the Artisan reads 4/4" $ do
          onStack <- board ["Hardened Scales"]
          let after = S.runPure (paysFor S.alice) onStack Stack.resolveTop
          case artisansOn onStack of
            [artisanId] -> do
              Spec.assertEqWith s "it still entered as a 2/2" (S.powerToughnessOf artisanId onStack) (Just (2, 2))
              Spec.assertEqWith s "one counter became two" (S.counterOf CounterKind.PlusOnePlusOne artisanId after) 2
              Spec.assertEqWith s "so it reads 4/4" (S.powerToughnessOf artisanId after) (Just (4, 4))
              Spec.assertEqWith s "and still no Servo" (S.tokensOf after) []
            other -> Spec.assertFailure s ("expected one Glint-Sleeve Artisan, got " <> show (length other))
        -- The keyword's N, at gameplay level and on BOTH halves. Weaponcraft
        -- Enthusiast, {2}{B} Creature -- Aetherborn Artificer 0/1, whose entire
        -- text box is "Fabricate 2": a mint that dropped the payload would put
        -- one counter on (1/2, not 2/3) and make one Servo, and 0/1 keeps every
        -- reading a different number from the Artisan's.
        Spec.it s "CR 702.123a fabricate 2 puts two counters on the Enthusiast" $ do
          onStack <- boardOf "Swamp" "Weaponcraft Enthusiast" []
          let after = S.runPure (paysFor S.alice) onStack Stack.resolveTop
          case named "Weaponcraft Enthusiast" onStack of
            [enthusiastId] -> do
              Spec.assertEqWith s "it entered as a 0/1" (S.powerToughnessOf enthusiastId onStack) (Just (0, 1))
              Spec.assertEqWith s "two +1/+1 counters went on" (S.counterOf CounterKind.PlusOnePlusOne enthusiastId after) 2
              Spec.assertEqWith s "so it reads 2/3" (S.powerToughnessOf enthusiastId after) (Just (2, 3))
              Spec.assertEqWith s "and no Servo" (S.tokensOf after) []
            other -> Spec.assertFailure s ("expected one Weaponcraft Enthusiast, got " <> show (length other))
        Spec.it s "CR 702.123a declining fabricate 2 creates two Servos" $ do
          onStack <- boardOf "Swamp" "Weaponcraft Enthusiast" []
          let after = S.runPure S.identityAnswer onStack Stack.resolveTop
          case named "Weaponcraft Enthusiast" onStack of
            [enthusiastId] -> do
              Spec.assertEqWith s "no counter went on" (S.counterOf CounterKind.PlusOnePlusOne enthusiastId after) 0
              Spec.assertEqWith s "so it is still a 0/1" (S.powerToughnessOf enthusiastId after) (Just (0, 1))
              Spec.assertEqWith s "and there are two Servos" (length (S.tokensOf after)) 2
            other -> Spec.assertFailure s ("expected one Weaponcraft Enthusiast, got " <> show (length other))
        -- CR 702.123b: "if a permanent has multiple instances of fabricate, each
        -- triggers separately". Asserted of the MINT, as afterlife's multiplicity
        -- is, no card in the pool printing fabricate twice.
        Spec.it s "CR 702.123b each instance of fabricate is its own ability" $ do
          Spec.assertEqWith s "fabricate 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Fabricate 1) 2)) [Keyword.fabricate 1, Keyword.fabricate 1]
          Spec.assertEqWith s "and fabricate 2 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Fabricate 2) 1)) [Keyword.fabricate 2]
          Spec.assertBool s (Keyword.fabricate 1 /= Keyword.fabricate 2) "and the N reaches the minted ability"

-- CR 702.21a ward [cost]: "Whenever this permanent becomes the target of a spell
-- or ability an opponent controls, counter that spell or ability unless that
-- player pays [cost]." A TRIGGER and not a targeting restriction, which is what
-- the first case proves: the spell is announced and paid for normally, and the
-- ability goes on the stack over it.
--
-- Tomakul Honor Guard, {1}{G} Creature -- Human Soldier 3/1, whose entire text
-- box is "Ward {2}", and Giant Growth as the thing that targets it: +3/+3 makes
-- the two outcomes 6/4 and 3/1, and a stack that never resolved reads 3/1 too --
-- which is why every case asserts the stack's height as well.
--
-- THREE SEATS. carol is neither the ward's controller nor the caster, so "an
-- opponent controls it" and "bob controls it" are different sentences.
--
-- THE PAIR: bob's Giant Growth and alice's are on the SAME board, aimed at the
-- SAME permanent, off the same three Forests each. The only difference is who
-- casts, which is the whole of rule 702.21a's "an opponent controls" -- and both
-- casters can afford the ward cost afterwards, so a spell that survives did so
-- because the ability never fired rather than because nobody could have paid.
--
-- `paysFor S.bob` throughout, which is what makes the payer assertion real:
-- alice is never paid for, so an engine that offered rule 702.21a's cost to the
-- ward's own controller falls through to Declines and counters the spell.
wardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wardSpec s registry =
  let board forest guard growth =
        let withLands = S.landsFor forest S.bob 3 (S.landsFor forest S.alice 3 S.threePlayerGame)
            (guardId, withGuard) = S.addCreature guard S.alice withLands
            (bobsGrowth, withBobs) = S.addHandCard growth S.bob withGuard
            (alicesGrowth, gs) = S.addHandCard growth S.alice withBobs
         in ( guardId,
              bobsGrowth,
              alicesGrowth,
              gs
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      -- The Honor Guard is the board's only creature, so identityAnswer's
      -- lowest-id choice of target is the only choice there is -- nothing here
      -- searches for a permanent that makes the assertion pass.
      castAndSettle caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.settleForPriority
      boardOf = do
        forest <- S.printingOf s registry "Forest"
        guard <- S.printingOf s registry "Tomakul Honor Guard"
        growth <- S.printingOf s registry "Giant Growth"
        pure (board forest guard growth)
   in Spec.describe s "CR 702.21 ward" $ do
        Spec.it s "CR 702.21a an opponent's spell fires ward, and declining counters it" $ do
          (guardId, bobsGrowth, _, gs) <- boardOf
          let onStack = castAndSettle S.bob bobsGrowth gs
              ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
          -- The controls: the spell really was cast -- rule 702.21a is not a CR
          -- 115 targeting restriction -- and the ability really reached the
          -- stack over it.
          Spec.assertEqWith s "the Growth and the ward trigger are both on the stack" (length (GameState.stack onStack)) 2
          Spec.assertEqWith s "the Guard is still a 3/1 with the Growth unresolved" (S.powerToughnessOf guardId onStack) (Just (3, 1))
          -- alice is the one paid for, and she is never asked: the offer went to
          -- bob, who declined through the identity fallback.
          Spec.assertEqWith s "bob was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
          Spec.assertEqWith s "CR 701.6a: the Growth was countered, so the stack is empty" (length (GameState.stack after)) 0
          Spec.assertEqWith s "and it is in bob's graveyard" (Seq.length (Map.findWithDefault Seq.empty S.bob (GameState.graveyard after))) 1
          Spec.assertEqWith s "so the Guard is still a 3/1" (S.powerToughnessOf guardId after) (Just (3, 1))
        -- The same board and the same cast as the case above, differing in
        -- NOTHING but bob's answer.
        Spec.it s "CR 702.21a paying the ward cost leaves the spell to resolve" $ do
          (guardId, bobsGrowth, _, gs) <- boardOf
          let onStack = castAndSettle S.bob bobsGrowth gs
              ((_, after), transcript) = Replay.record (paysFor S.bob) onStack (Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop)
          Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
          Spec.assertEqWith s "nothing was countered, so the Growth resolved" (S.powerToughnessOf guardId after) (Just (6, 4))
          Spec.assertEqWith s "and bob's graveyard holds the spent Growth" (Seq.length (Map.findWithDefault Seq.empty S.bob (GameState.graveyard after))) 1
        -- The relation, moved on its own: alice casts her OWN Giant Growth at
        -- her own warded creature, off her own three Forests. Rule 702.21a's
        -- condition is "a spell or ability AN OPPONENT controls", so nothing
        -- triggers -- and `paysFor S.bob` would leave a Declines in the
        -- transcript if the ability had fired and offered alice the cost.
        Spec.it s "CR 702.21a the ward controller's OWN spell fires nothing" $ do
          (guardId, _, alicesGrowth, gs) <- boardOf
          let onStack = castAndSettle S.alice alicesGrowth gs
              ((_, after), transcript) = Replay.record (paysFor S.bob) onStack Stack.resolveTop
          Spec.assertEqWith s "only the Growth is on the stack" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "nobody was offered a ward cost" (payResponses transcript) []
          Spec.assertEqWith s "and the Growth resolved" (S.powerToughnessOf guardId after) (Just (6, 4))
        -- The BEARER, moved on its own, and the pair's third axis after the
        -- relation: bob's Giant Growth names an ordinary creature standing
        -- beside the Honor Guard instead of the Guard. Rule 702.21a's subject is
        -- "this permanent", so a BecameTarget naming anything else must not fire
        -- it -- and with the Guard the board's only creature (every case above)
        -- an engine that fired on ANY targeted object would be green throughout.
        --
        -- The Piker is added here rather than in `board` so the cases above keep
        -- the one-creature board their identityAnswer targeting relies on, and
        -- this case pins its own target instead.
        Spec.it s "CR 702.21a a spell naming a DIFFERENT permanent fires nothing" $ do
          (guardId, bobsGrowth, _, gs) <- boardOf
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, withPiker) = S.addCreature piker S.alice gs
              aimedAtPiker :: Prompt.Prompt r -> r
              aimedAtPiker p = case p of
                Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (== Recipient.ToCreature pikerId) candidates) sets
                _ -> paysFor S.bob p
              onStack = S.runPure aimedAtPiker (S.runPure aimedAtPiker withPiker (S.cast S.bob bobsGrowth)) Engine.settleForPriority
              ((_, after), transcript) = Replay.record aimedAtPiker onStack Stack.resolveTop
          Spec.assertEqWith s "only the Growth is on the stack" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "nobody was offered a ward cost" (payResponses transcript) []
          Spec.assertEqWith s "and the Growth resolved onto the PIKER" (S.powerToughnessOf pikerId after) (Just (5, 4))
          Spec.assertEqWith s "leaving the Guard a 3/1" (S.powerToughnessOf guardId after) (Just (3, 1))
        -- Rule 702.21 states no "each instance" sentence, so two instances are
        -- two abilities for CR 603.2's general reason. Asserted of the MINT, as
        -- fabricate's multiplicity is: no printing carries ward twice.
        Spec.it s "CR 603.2 each instance of ward is its own ability" $ do
          let cost n = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []
          Spec.assertEqWith s "ward {2} held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Ward (cost 2)) 2)) [Keyword.ward (cost 2), Keyword.ward (cost 2)]
          Spec.assertBool s (Keyword.ward (cost 2) /= Keyword.ward (cost 3)) "and the cost reaches the minted ability"

-- CR 118.12a over MORE THAN ONE PAYER: "[Do something] unless [a player does
-- something else]" means "[A player may do something else]. If [that player
-- doesn't], [do something]", and a card that says "each opponent" makes that
-- offer once per opponent -- in CR 101.4's APNAP order, each answer gating that
-- opponent's own instruction and nobody else's.
--
-- Rishadan Cutpurse, {2}{U} Creature -- Human Pirate 1/1, whose entire text box
-- is "When this creature enters, each opponent sacrifices a permanent of their
-- choice unless they pay {1}." The gate's payer is PlayerRef.Relative Opponent
-- and its IfNotPaid branch binds the seats that declined under
-- Binding.gatePlayers, which the Effect.PlayerSacrifices reads as "they".
--
-- THREE SEATS, and the point of them: with two, "each opponent" and "the one
-- opponent" are the same sentence, so an engine that offered the cost once and
-- applied one answer to everybody would be green. Here bob PAYS and carol does
-- not, so the two opponents must end the resolution on different boards.
--
-- The observable is HOW MANY PERMANENTS EACH SEAT CONTROLS, never a prompt
-- count: bob keeps his (paying taps a Forest, it does not lose him one) and
-- carol loses one. The three seats are stocked to three DIFFERENT sizes so no
-- two readings of the rule produce the same triple.
cutpurseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cutpurseSpec s registry =
  let controls pid gs = length (Projection.controls pid gs)
      board island forest swamp piker cutpurse =
        let withLands = S.landsFor swamp S.carol 3 (S.landsFor forest S.bob 2 (S.landsFor island S.alice 3 S.threePlayerGame))
            withPikers = List.foldl' (\acc pid -> snd (S.addCreature piker pid acc)) withLands [S.bob, S.carol, S.carol]
            (gs, cutpurseId) = S.handOne cutpurse withPikers
         in (cutpurseId, gs)
      -- The Cutpurse cast off alice's three Islands and resolved, with its CR
      -- 603.6a enters trigger settled onto the stack but NOT resolved.
      entersOnStack cutpurseId gs =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice cutpurseId)
         in S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
      boardOf = do
        island <- S.printingOf s registry "Island"
        forest <- S.printingOf s registry "Forest"
        swamp <- S.printingOf s registry "Swamp"
        piker <- S.printingOf s registry "Goblin Piker"
        cutpurse <- S.printingOf s registry "Rishadan Cutpurse"
        let (cutpurseId, gs) = board island forest swamp piker cutpurse
        pure (entersOnStack cutpurseId gs)
   in Spec.describe s "CR 118.12 a gate offered to each opponent" $ do
        Spec.it s "CR 118.12a bob pays and carol does not, so only carol sacrifices" $ do
          onStack <- boardOf
          let ((_, after), transcript) = Replay.record (paysFor S.bob) onStack Stack.resolveTop
          -- The controls: the Cutpurse really entered and its trigger really
          -- reached the stack, and the three seats really start apart.
          Spec.assertEqWith s "CR 603.6a: its enters trigger is on the stack" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "alice, bob and carol start with 4, 3 and 5 permanents" (controls S.alice onStack, controls S.bob onStack, controls S.carol onStack) (4, 3, 5)
          -- THE BEHAVIOUR: carol alone lost a permanent. bob's answer bought him
          -- out of an edict carol's answer did not buy her out of, and alice --
          -- not an opponent of herself -- was never in it.
          Spec.assertEqWith s "CR 118.12a: carol alone sacrificed" (controls S.alice after, controls S.bob after, controls S.carol after) (4, 3, 4)
          -- The supporting check: two offers, in CR 101.4's APNAP order, with
          -- alice never asked.
          Spec.assertEqWith s "CR 101.4: bob was asked first and paid, then carol declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays, Response.ChoseToPay PaymentDecision.Declines]
        -- The same board and the same trigger as the case above, differing in
        -- NOTHING but who is paid for: bob now declines too, so both edicts land
        -- and the pair tells "carol was asked" apart from "carol was swept up in
        -- bob's answer".
        Spec.it s "CR 118.12a nobody pays, so both opponents sacrifice" $ do
          onStack <- boardOf
          let ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
          Spec.assertEqWith s "CR 118.12a: both opponents sacrificed and alice did not" (controls S.alice after, controls S.bob after, controls S.carol after) (4, 2, 4)
          Spec.assertEqWith s "both were asked, and both declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines, Response.ChoseToPay PaymentDecision.Declines]
        -- The other limb, moved on its own: both opponents pay, so the IfNotPaid
        -- branch selects nobody and the clause does nothing at all.
        Spec.it s "CR 118.12a both opponents pay, so nobody sacrifices" $ do
          onStack <- boardOf
          let paysForEither p = case p of
                Prompt.ChooseToPay {} -> PaymentDecision.Pays
                _ -> S.identityAnswer p
              after = S.runPure paysForEither onStack Stack.resolveTop
          Spec.assertEqWith s "CR 118.12a: the paid branch sacrificed nothing" (controls S.alice after, controls S.bob after, controls S.carol after) (4, 3, 5)

-- CR 601.2c read from the TARGETED PLAYER's side: "the chosen objects and/or
-- players each become a target of that spell". Dormant Gomazoa, {1}{U}{U}
-- Creature -- Jellyfish 5/5: "Flying / This creature enters tapped. / This
-- creature doesn't untap during your untap step. / Whenever you become the
-- target of a spell, you may untap this creature."
--
-- The first card in the pool that watches a PLAYER become a target, and the
-- reason GameEvent.BecameTarget carries a Recipient rather than an ObjectId.
--
-- "A SPELL", not "a spell or ability" -- which is why BecameTarget also carries
-- a StackObjectKind, and why the Ravenous Rats leg is here: CR 602.2b and CR
-- 603.3d route an ability through the same rule 601.2c, and the matcher is pure
-- with no GameState to look that stack object up in.
--
-- TAPPEDNESS IS THE SIGNAL, never power or toughness: CR 502.3 keeps this
-- creature from untapping in its own untap step while CR 701.26b's Untap action
-- is what the trigger performs, so the two answers "the trigger fired" and "it
-- did not" are exactly untapped and tapped.
--
-- Every leg runs under `taking`, an ACCEPTING answerer. S.identityAnswer
-- declines a CR 603.5 "may" (Replay.defaultAnswer), which would leave the
-- Gomazoa tapped in every leg and make the three absences below prove nothing.
gomazoaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gomazoaSpec s registry =
  let -- Accepts CR 603.5's "may" and pins every target slot at one recipient.
      -- The offered set is FILTERED rather than replaced, so a leg that names a
      -- recipient the engine never offered chooses nothing instead of quietly
      -- passing a target the CR 608.2b re-read would drop.
      taking :: Recipient.Recipient -> Prompt.Prompt r -> r
      taking r p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (== r) candidates) sets
        _ -> S.identityAnswer p
      tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)
      -- THREE SEATS: alice controls the Gomazoa, bob casts everything, and carol
      -- is a target that is neither. Two seats would collapse "a player who is
      -- not you" onto the caster.
      board = do
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        gomazoa <- S.printingOf s registry "Dormant Gomazoa"
        fatigue <- S.printingOf s registry "Fatigue"
        rats <- S.printingOf s registry "Ravenous Rats"
        growth <- S.printingOf s registry "Giant Growth"
        -- Three colors because the three producers are {1}{U}, {1}{B} and {G};
        -- one leg casts one spell, so the lands never compete.
        let g0 = S.landsFor forest S.bob 1 (S.landsFor swamp S.bob 2 (S.landsFor island S.bob 2 S.threePlayerGame))
            (gomazoaId, g1) = S.addCreature gomazoa S.alice g0
            -- alice's control for the untap-step leg: an ordinary creature under
            -- the same player, tapped the same way, differing only in the
            -- restriction.
            (pikerId, g2) = S.addCreature piker S.alice g1
            (fatigueId, g3) = S.addHandCard fatigue S.bob g2
            (ratsId, g4) = S.addHandCard rats S.bob g3
            (growthId, g5) = S.addHandCard growth S.bob g4
            -- One card in alice's hand, which is the Ravenous Rats leg's control:
            -- the ability really did name her.
            (_, g6) = S.addHandCard island S.alice g5
            -- CR 104.3c: nothing here draws, but an empty library is a loss
            -- waiting for any leg that advances.
            stocked = List.foldl' (\g pid -> snd (S.addLibraryCard island pid g)) g6 [S.alice, S.bob, S.carol, S.alice, S.bob, S.carol]
            tapped = S.tapObject pikerId (S.tapObject gomazoaId stocked)
        pure
          ( gomazoaId,
            pikerId,
            fatigueId,
            ratsId,
            growthId,
            tapped
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.bob,
                GameState.priority = Just S.bob
              }
          )
      -- Cast one spell at `r`, settle so any trigger reaches the stack, then
      -- resolve the top under the same accepting answerer. `taking r` is written
      -- out at each use rather than bound once: it is polymorphic in the prompt's
      -- answer type, and a let-bound copy would be pinned to one.
      castThenResolve r caster oid gs =
        let onStack = S.runPure (taking r) (S.runPure (taking r) gs (S.cast caster oid)) Engine.settleForPriority
            ((_, after), transcript) = Replay.record (taking r) onStack Stack.resolveTop
         in (onStack, after, transcript)
   in Spec.describe s "CR 601.2c a player becoming the target of a spell" $ do
        -- The positive the three absences below rest on. Fatigue rather than
        -- Ancestral Recall on purpose: it draws nothing, so CR 704.5b cannot end
        -- the game mid-leg and no life total moves.
        Spec.it s "CR 601.2c whole card: a spell targeting the Gomazoa's controller untaps it" $ do
          (gomazoaId, _, fatigueId, _, _, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToPlayer S.alice) S.bob fatigueId gs
          Spec.assertEqWith s "the Gomazoa starts tapped" (tapStateOf gomazoaId gs) (Just TapState.Tapped)
          Spec.assertEqWith s "CR 701.26b the trigger untapped it" (tapStateOf gomazoaId after) (Just TapState.Untapped)
          Spec.assertEqWith s "CR 603.5 its controller was asked the may exactly once" (optionalResponses transcript) [Response.ChoseOptional OptionalDecision.Exercises]
          Spec.assertEqWith s "the Fatigue and the trigger were both on the stack" (length (GameState.stack onStack)) 2
        -- CR 109.5: the same board and the same spell, differing in NOTHING but
        -- which player it names. carol is neither the Gomazoa's controller nor
        -- the caster.
        Spec.it s "CR 109.5 a spell targeting another player leaves it tapped" $ do
          (gomazoaId, _, fatigueId, _, _, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToPlayer S.carol) S.bob fatigueId gs
          Spec.assertEqWith s "the Gomazoa is still tapped" (tapStateOf gomazoaId after) (Just TapState.Tapped)
          Spec.assertEqWith s "and nobody was offered the may" (optionalResponses transcript) []
          Spec.assertEqWith s "only the Fatigue was on the stack" (length (GameState.stack onStack)) 1
        -- CR 112.1 against CR 113.3, which is the whole of BecameTarget.kind:
        -- Ravenous Rats' SPELL targets nothing and its enters-trigger targets an
        -- opponent, so the only BecameTarget on this board is an ability's.
        Spec.it s "CR 112.1 an ABILITY targeting the Gomazoa's controller leaves it tapped" $ do
          (gomazoaId, _, _, ratsId, _, gs) <- board
          let atAlice = Recipient.ToPlayer S.alice
              entered = S.runPure (taking atAlice) (S.runPure (taking atAlice) gs (S.cast S.bob ratsId)) Stack.resolveTop
              triggerOnStack = S.runPure (taking atAlice) entered Engine.settleForPriority
              ((_, after), transcript) = Replay.record (taking atAlice) triggerOnStack Stack.resolveTop
          Spec.assertEqWith s "the Gomazoa is still tapped" (tapStateOf gomazoaId after) (Just TapState.Tapped)
          Spec.assertEqWith s "and nobody was offered the may" (optionalResponses transcript) []
          -- The controls, which are what stop this leg passing because nothing
          -- happened at all: the ability reached the stack and really named her.
          Spec.assertEqWith s "CR 603.3d the Rats' enters trigger reached the stack" (length (GameState.stack triggerOnStack)) 1
          Spec.assertEqWith s "and alice discarded the card it targeted her for" (S.handSize S.alice after) 0
        -- CR 115.1's other kind of target on the same event: the Gomazoa itself
        -- becomes one, which is SelfBecomesTargeted's condition and not this one.
        Spec.it s "CR 115.1 the Gomazoa becoming a target itself is not its controller becoming one" $ do
          (gomazoaId, _, _, _, growthId, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToCreature gomazoaId) S.bob growthId gs
          Spec.assertEqWith s "the Gomazoa is still tapped" (tapStateOf gomazoaId after) (Just TapState.Tapped)
          Spec.assertEqWith s "and nobody was offered the may" (optionalResponses transcript) []
          Spec.assertEqWith s "only the Growth was on the stack" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "and it resolved onto the Gomazoa" (S.powerToughnessOf gomazoaId after) (Just (8, 8))
        -- The card's third line, which is what makes the trigger worth printing:
        -- CR 502.3's turn-based untap passes the Gomazoa over while CR 701.26b's
        -- Untap action above does not. The Piker is the pair's other half.
        Spec.it s "CR 502.3 the Gomazoa does not untap in its controller's untap step, and an ordinary creature does" $ do
          (gomazoaId, pikerId, _, _, _, gs) <- board
          let untapped = S.runPure S.identityAnswer (gs {GameState.activePlayer = S.alice}) (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
          Spec.assertEqWith s "the Gomazoa is still tapped" (tapStateOf gomazoaId untapped) (Just TapState.Tapped)
          Spec.assertEqWith s "and the Piker untapped" (tapStateOf pikerId untapped) (Just TapState.Untapped)

-- CR 601.2c read from the targeted PLAYER's side again, with the two narrowings
-- Dormant Gomazoa above does not print. Amulet of Safekeeping, {2} Artifact:
-- "Whenever you become the target of a spell or ability an opponent controls,
-- counter that spell or ability unless its controller pays {1}. / Creature
-- tokens get -1/-0."
--
-- What is new over the Gomazoa is the whole of the condition's payload: the
-- ABILITY half of CR 601.2c (its "a spell or ability" narrows to neither limb,
-- so CR 602.2b's and CR 603.3d's road counts too), and a PlayerRelation over CR
-- 405.4's controller of the targeting object. On the payload side it is the
-- first CARD-authored payGate whose payer is a reserved BINDING slot rather than
-- a target slot -- rule 702.21a's ward mints that shape, and Amulet writes it.
--
-- LIFE AND HAND SIZE ARE THE SIGNALS, never a stack height or a graveyard
-- length: a countered Lightning Bolt is exactly "alice is still at 20", and a
-- countered discard ability is exactly "alice still holds her card". The stack
-- and graveyard readings are controls, and they come AFTER.
--
-- THREE SEATS: alice controls the Amulet, bob casts everything, and carol is a
-- targeted player who is neither.
amuletSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
amuletSpec s registry =
  let -- Pins every target slot at one recipient AND pays wherever `who` is
      -- offered a cost. The offered set is FILTERED rather than replaced,
      -- gomazoaSpec's reason: a leg naming a recipient the engine never offered
      -- chooses nothing instead of quietly passing a target CR 608.2b's re-read
      -- would drop.
      aimedPaying :: Recipient.Recipient -> PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedPaying r who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (== r) candidates) sets
        _ -> paysFor who p
      goblinTokens gs = filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Goblin Token"))) (Set.toList (GameState.battlefield gs))
      board = do
        mountain <- S.printingOf s registry "Mountain"
        swamp <- S.printingOf s registry "Swamp"
        island <- S.printingOf s registry "Island"
        amulet <- S.printingOf s registry "Amulet of Safekeeping"
        piker <- S.printingOf s registry "Goblin Piker"
        bolt <- S.printingOf s registry "Lightning Bolt"
        rats <- S.printingOf s registry "Ravenous Rats"
        fodder <- S.printingOf s registry "Dragon Fodder"
        -- bob's five lands cover every leg's spell AND the {1} on top of it:
        -- {R} plus one for the Bolt, {1}{B} plus one for the Rats, {1}{R} for
        -- the Fodder. alice's one Mountain covers her own Bolt and nothing else.
        let g0 = S.landsFor swamp S.bob 2 (S.landsFor mountain S.bob 3 (S.landsFor mountain S.alice 1 S.threePlayerGame))
            (amuletId, g1) = S.addCreature amulet S.alice g0
            -- The static leg's other half: a NONTOKEN creature standing beside
            -- the tokens, which is what makes Filter.IsToken load-bearing.
            (pikerId, g2) = S.addCreature piker S.bob g1
            (bobsBolt, g3) = S.addHandCard bolt S.bob g2
            (ratsId, g4) = S.addHandCard rats S.bob g3
            (fodderId, g5) = S.addHandCard fodder S.bob g4
            -- alice's ONE card in hand, which is both the Ravenous Rats leg's
            -- precondition -- a hand to discard from -- and the relation leg's
            -- spell.
            (alicesBolt, g6) = S.addHandCard bolt S.alice g5
            -- CR 104.3c: nothing here draws, but an empty library is a loss
            -- waiting for any leg that advances.
            stocked = List.foldl' (\g pid -> snd (S.addLibraryCard island pid g)) g6 [S.alice, S.bob, S.carol, S.alice, S.bob, S.carol]
        pure
          ( amuletId,
            pikerId,
            bobsBolt,
            ratsId,
            fodderId,
            alicesBolt,
            stocked
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.bob,
                GameState.priority = Just S.bob
              }
          )
      -- Cast one spell at `r`, settle so any trigger reaches the stack, then
      -- resolve the stack DOWN under the same answerer. The answerer is written
      -- out at each use rather than bound once: it is polymorphic in the
      -- prompt's answer type, and a let-bound copy would be pinned to one.
      --
      -- TWO resolutions, not one, and that is what makes the life readings
      -- load-bearing: a trigger that counters nothing leaves the Bolt sitting on
      -- the stack, and one resolution cannot tell that apart from a Bolt that
      -- was countered. Stack.resolveTop is a no-op on an empty stack, so the
      -- second is harmless when the first emptied it.
      resolveDown = Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop
      castThenResolve r payer caster oid gs =
        let onStack = S.runPure (aimedPaying r payer) (S.runPure (aimedPaying r payer) gs (S.cast caster oid)) Engine.settleForPriority
            ((_, after), transcript) = Replay.record (aimedPaying r payer) onStack resolveDown
         in (onStack, after, transcript)
   in Spec.describe s "CR 601.2c a player becoming the target of a spell OR an ability" $ do
        -- The positive, and the whole point of the card.
        Spec.it s "CR 601.2c whole card: an opponent's SPELL naming alice is countered when its controller declines" $ do
          (_, _, bobsBolt, _, _, _, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToPlayer S.alice) S.carol S.bob bobsBolt gs
          Spec.assertEqWith s "CR 701.6a the Bolt never resolved, so alice is still at 20" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "CR 405.4 bob, the Bolt's controller, was asked exactly once and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
          -- The controls: the spell really was cast, and the trigger really
          -- reached the stack over it.
          Spec.assertEqWith s "the Bolt and the trigger were both on the stack" (length (GameState.stack onStack)) 2
          Spec.assertEqWith s "CR 701.6a and the countered Bolt is in bob's graveyard" (Seq.length (Map.findWithDefault Seq.empty S.bob (GameState.graveyard after))) 1
        -- The same board and the same cast as the case above, differing in
        -- NOTHING but bob's answer.
        Spec.it s "CR 118.12a paying the {1} leaves the spell to resolve" $ do
          (_, _, bobsBolt, _, _, _, gs) <- board
          let (_, after, transcript) = castThenResolve (Recipient.ToPlayer S.alice) S.bob S.bob bobsBolt gs
          Spec.assertEqWith s "the Bolt resolved onto alice" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
        -- The ABILITY half, which is the whole of the payload's `kind = Nothing`
        -- and the leg Dormant Gomazoa's printing cannot reach: Ravenous Rats'
        -- SPELL targets nothing, and its CR 603.3d enters trigger targets alice.
        Spec.it s "CR 113.3c an opponent's TRIGGERED ABILITY naming alice is countered too" $ do
          (_, _, _, ratsId, _, _, gs) <- board
          let atAlice = Recipient.ToPlayer S.alice
              entered = S.runPure (aimedPaying atAlice S.carol) (S.runPure (aimedPaying atAlice S.carol) gs (S.cast S.bob ratsId)) Stack.resolveTop
              triggerOnStack = S.runPure (aimedPaying atAlice S.carol) entered Engine.settleForPriority
              ((_, after), transcript) = Replay.record (aimedPaying atAlice S.carol) triggerOnStack resolveDown
          Spec.assertEqWith s "alice starts with one card in hand" (S.handSize S.alice gs) 1
          Spec.assertEqWith s "CR 701.6a the discard ability was countered, so alice still holds it" (S.handSize S.alice after) 1
          Spec.assertEqWith s "bob was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
          -- The controls that stop this leg passing because nothing happened at
          -- all: the Rats' SPELL resolved, and its ability really reached the
          -- stack under the Amulet's trigger.
          Spec.assertEqWith s "the Rats are on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Ravenous Rats")) S.bob after) 1
          Spec.assertEqWith s "CR 603.3d the Rats' trigger and the Amulet's were both on the stack" (length (GameState.stack triggerOnStack)) 2
        -- The RELATION, moved on its own: the same board and the same spell at
        -- the same seat, differing only in who casts it. CR 405.4's controller
        -- is then alice herself, whom PlayerRelation.Opponent excludes.
        Spec.it s "CR 405.4 the Amulet controller's OWN spell naming her fires nothing" $ do
          (_, _, _, _, _, alicesBolt, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToPlayer S.alice) S.carol S.alice alicesBolt gs
          Spec.assertEqWith s "her own Bolt resolved onto her" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "and nobody was offered the {1}" (payResponses transcript) []
          Spec.assertEqWith s "only the Bolt was on the stack" (length (GameState.stack onStack)) 1
        -- The RECIPIENT, moved on its own: the same cast by the same player, one
        -- seat over. CR 109.5's "you" is the Amulet's controller, and carol is
        -- neither that nor the caster.
        Spec.it s "CR 109.5 an opponent's spell naming ANOTHER player fires nothing" $ do
          (_, _, bobsBolt, _, _, _, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToPlayer S.carol) S.carol S.bob bobsBolt gs
          Spec.assertEqWith s "the Bolt resolved onto carol" (S.lifeOf S.carol after) (Just 17)
          Spec.assertEqWith s "and alice is untouched" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "and nobody was offered the {1}" (payResponses transcript) []
          Spec.assertEqWith s "only the Bolt was on the stack" (length (GameState.stack onStack)) 1
        -- The card's second line, CR 111.6 read through CR 613.1g's layer 7:
        -- Dragon Fodder's two 1/1 Goblin TOKENS against a nontoken Goblin Piker
        -- standing on the same board. CR 704.5f does not reach the tokens -- a
        -- 0/1 is alive -- so this stays a power/toughness reading.
        Spec.it s "CR 111.6 creature TOKENS get -1/-0 and nontoken creatures do not" $ do
          (_, pikerId, _, _, fodderId, _, gs) <- board
          let resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast S.bob fodderId)) Stack.resolveTop
          Spec.assertEqWith s "CR 613.1g the two Goblin tokens are 0/1" (fmap (\oid -> S.powerToughnessOf oid resolved) (goblinTokens resolved)) [Just (0, 1), Just (0, 1)]
          Spec.assertEqWith s "and the nontoken Piker is untouched" (S.powerToughnessOf pikerId resolved) (Just (2, 1))

-- The CR 603.5 "may" answers in a transcript, in order -- paysFor's
-- payResponses one Response constructor over. A transcript with no
-- ChooseOptional in it says the prompt was never raised, which is what makes an
-- absence leg above assert more than a tap state.
optionalResponses :: [Response.Response] -> [Response.Response]
optionalResponses = filter isOptionalResponse

isOptionalResponse :: Response.Response -> Bool
isOptionalResponse response = case response of
  Response.ChoseOptional _ -> True
  _ -> False

-- Blind Hunter, a Creature -- Bat: "Flying / Haunt (When this creature dies,
-- exile it haunting target creature.) / When this creature enters or the
-- creature it haunts dies, target player loses 2 life and you gain 2 life."
-- The pool's witness for an ability that functions FROM EXILE (CR 113.6k, CR
-- 702.55c), and the card rule 113.6k's own example is written about.
--
-- THREE SEATS, because the rule names three different players: alice owns the
-- Blind Hunter and so controls both of its triggers (CR 603.3a on the
-- battlefield, CR 108.4a in exile), bob controls the creature it haunts, and
-- carol is what "target player loses 2 life" names. A two-player board would
-- collapse the last two and could not tell "the haunted creature's controller"
-- from "the target".
--
-- The board is played out rather than assembled: alice bolts her own Blind
-- Hunter, haunt's dies trigger exiles the card haunting bob's Piker, and a
-- second bolt kills that Piker. Only then is the exile-zone ability asked for.
hauntSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hauntSpec s registry =
  let -- Every target slot pinned at one recipient, never "the least legal one":
      -- both Pikers are legal haunt targets and all three players are legal
      -- "target player"s, so S.identityAnswer would answer by sort order and the
      -- assertions could not tell which object the effect reached.
      aimAt :: Recipient.Recipient -> Prompt.Prompt r -> r
      aimAt r p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton r)) sets
        _ -> S.identityAnswer p
      board = do
        mountain <- S.printingOf s registry "Mountain"
        bolt <- S.printingOf s registry "Lightning Bolt"
        hunter <- S.printingOf s registry "Blind Hunter"
        piker <- S.printingOf s registry "Goblin Piker"
        let g0 = S.landsFor mountain S.alice 2 S.threePlayerGame
            (hunterId, g1) = S.addCreature hunter S.alice g0
            (victimId, g2) = S.addCreature piker S.bob g1
            -- An IDENTICAL second Piker under the same player, never haunted:
            -- the pair is what makes the link observable, since the two differ
            -- in nothing else.
            (bystanderId, g3) = S.addCreature piker S.bob g2
            (g4, firstBolt) = S.handOne bolt g3
            (secondBolt, g5) = S.addHandCard bolt S.alice g4
            -- CR 104.3c: nothing here draws, but an empty library is a loss
            -- waiting for any leg that advances a turn.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard mountain S.alice g)) g5 [1 .. 5 :: Int]
        pure (hunterId, victimId, bystanderId, firstBolt, secondBolt, stocked)
      -- Cast a Lightning Bolt at one creature and resolve it. The bolt's own
      -- target is pinned here; whatever the death triggers ask is pinned by the
      -- caller's own answerer, since the two choices are different questions.
      boltAt :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      boltAt oid spell gs =
        let cast = S.runPure (aimAt (Recipient.ToCreature oid)) gs (S.cast S.alice spell)
         in S.runPure (aimAt (Recipient.ToCreature oid)) cast Stack.resolveTop
      -- CR 704 kills the creature and CR 603.3 puts the death trigger on the
      -- stack, choosing its targets (CR 603.3d) under `answer`; then the trigger
      -- resolves.
      settleAndResolve :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> (GameState.GameState, GameState.GameState)
      settleAndResolve answer gs =
        let settled = S.runPure answer gs Engine.settleForPriority
         in (settled, S.runPure answer settled Stack.resolveTop)
      -- The whole first half: the Blind Hunter dies and haunt exiles it haunting
      -- bob's Piker.
      haunted (hunterId, victimId, _, firstBolt, _, gs) =
        settleAndResolve (aimAt (Recipient.ToCreature victimId)) (boltAt hunterId firstBolt gs)
      -- The one difference between the pair below and the board above: the
      -- haunting card sits in a graveyard instead of exile. CR 400.7 mints a
      -- fresh id for it, so rule 702.55b's link is re-filed under that id --
      -- otherwise the two boards would differ in the LINK as well as in the zone
      -- and the negative would pass for the wrong reason.
      fromExileToGraveyard gs = case Set.toList (GameState.exile gs) of
        [oid] ->
          let (mNew, moved) = S.runPureWith S.identityAnswer gs (Event.changeZoneReturning oid Zone.Graveyard)
              link = Map.lookup oid (GameState.haunting gs)
           in case (mNew, link) of
                (Just newId, Just hauntedId) -> moved {GameState.haunting = Map.insert newId hauntedId (Map.delete oid (GameState.haunting moved))}
                _ -> moved
        _ -> gs
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "CR 702.55 haunt" $ do
        -- The proving case for the whole unit: the ability fires from EXILE.
        Spec.it s "CR 702.55c whole card: the haunting card in exile sees the creature it haunts die" $ do
          fixture@(_, victimId, _, _, secondBolt, _) <- board
          let (_, exiled) = haunted fixture
              killed = boltAt victimId secondBolt exiled
              (placed, after) = settleAndResolve (aimAt (Recipient.ToPlayer S.carol)) killed
          Spec.assertEqWith s "one card is in exile" (Set.size (GameState.exile exiled)) 1
          Spec.assertEqWith s "and it haunts exactly one object" (Map.size (GameState.haunting exiled)) 1
          Spec.assertEqWith s "haunt itself changed nobody's life total" (lives exiled) (Just 20, Just 20, Just 20)
          Spec.assertEqWith s "the exile-zone trigger reached the stack in that settle" (length (GameState.stack placed)) 1
          -- Three different answers on three different seats, which is what the
          -- three seats are for: the target loses, the haunting card's OWNER
          -- gains (CR 108.4a gives a card in exile no other controller), and the
          -- player whose creature died is untouched.
          Spec.assertEqWith s "carol lost 2, alice gained 2, bob is untouched" (lives after) (Just 22, Just 20, Just 18)
        -- The link, not the zone: same board, same exile, and the creature that
        -- dies is the OTHER Piker.
        Spec.it s "CR 702.55b only the creature the card haunts fires it" $ do
          fixture@(_, _, bystanderId, _, secondBolt, _) <- board
          let (_, exiled) = haunted fixture
              killed = boltAt bystanderId secondBolt exiled
              (placed, after) = settleAndResolve (aimAt (Recipient.ToPlayer S.carol)) killed
          Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack placed)) 0
          Spec.assertEqWith s "and no life total moved" (lives after) (Just 20, Just 20, Just 20)
        -- The zone, not the link: the same haunting card, still haunting the same
        -- Piker, moved to a graveyard. CR 113.6k puts this ability in exile alone,
        -- so a graveyard reading of it must find nothing -- which is what
        -- eventTriggers' `inExile` source is for, since `inGraveyards` filters on
        -- the same functionsIn call and rejects it.
        Spec.it s "CR 113.6k the same ability does not fire from a graveyard" $ do
          fixture@(_, victimId, _, _, secondBolt, _) <- board
          let (_, exiled) = haunted fixture
              moved = fromExileToGraveyard exiled
              killed = boltAt victimId secondBolt moved
              (placed, after) = settleAndResolve (aimAt (Recipient.ToPlayer S.carol)) killed
          Spec.assertEqWith s "the card is out of exile" (Set.size (GameState.exile moved)) 0
          Spec.assertEqWith s "and still haunts the Piker" (Map.size (GameState.haunting moved)) 1
          Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack placed)) 0
          Spec.assertEqWith s "and no life total moved" (lives after) (Just 20, Just 20, Just 20)

soulshiftSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulshiftSpec s registry =
  let ancestorName = CardName.MkCardName (Text.pack "Disowned Ancestor")
      -- Rule 702.46a's "you may", exercised. S.identityAnswer declines it, which
      -- is what the declining leg below rides.
      exercising :: Prompt.Prompt r -> r
      exercising p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      board = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        kami <- S.printingOf s registry "Kami of Empty Graves"
        piker <- S.printingOf s registry "Goblin Piker"
        shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
        ancestor <- S.printingOf s registry "Disowned Ancestor"
        let (kamiId, g1) = S.addCreature kami S.alice (S.landsInPlay swamp 3)
            (pikerId, g2) = S.addGraveyardCard piker S.alice g1
            (shimatsuId, g3) = S.addGraveyardCard shimatsu S.alice g2
            (theirsId, g4) = S.addGraveyardCard ancestor S.bob g3
            (mineId, g5) = S.addGraveyardCard ancestor S.alice g4
            -- CR 104.3c: nothing here draws, but a stocked library keeps a leg
            -- from ending on an empty one.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g5 [1 .. 5 :: Int]
        pure (kamiId, (pikerId, shimatsuId, theirsId, mineId), S.handOne murder stocked)
      -- Cast the Murder, resolve it (the Kami dies), settle so the death trigger
      -- is gathered and its target chosen (CR 603.3d), then resolve the trigger.
      murderIt :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      murderIt answer (gs, spellId) =
        let cast = S.runPure answer gs (S.cast S.alice spellId)
            destroyed = S.runPure answer cast Stack.resolveTop
            settled = S.runPure answer destroyed Engine.settleForPriority
         in (settled, S.runPure answer settled Stack.resolveTop)
      graveyardOf = Game.zoneMembers Zone.Graveyard
   in Spec.describe s "Soulshift" $ do
        -- The proving case.
        Spec.it s "CR 702.46a whole card: the dead Kami returns the one Spirit card its N reaches" $ do
          (kamiId, (pikerId, shimatsuId, theirsId, mineId), gs) <- board
          let (settled, after) = murderIt exercising gs
          Spec.assertEqWith s "the dies trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (not (S.onBattlefield kamiId after)) "and the Kami is gone"
          Spec.assertEqWith s "alice's hand holds the Ancestor" (S.countByName ancestorName S.alice after) 1
          Spec.assertBool s (notElem mineId (graveyardOf S.alice after)) "the id it was targeted under has left her graveyard (CR 400.7)"
          Spec.assertBool s (elem pikerId (graveyardOf S.alice after)) "the Piker stayed: not a Spirit"
          Spec.assertBool s (elem shimatsuId (graveyardOf S.alice after)) "Shimatsu stayed: mana value 4 against soulshift 3"
          Spec.assertBool s (elem theirsId (graveyardOf S.bob after)) "bob's identical Ancestor stayed: the wrong graveyard (CR 400.1)"
          Spec.assertEqWith s "so does the dead Kami itself, a Spirit of mana value 4, beside the spent Murder" (length (graveyardOf S.alice after)) 4
        -- CR 603.5's "may" is a real fork, and the control for the case above --
        -- same board, same Murder, and the trigger still reaches the stack.
        Spec.it s "CR 603.5 declining the may returns nothing" $ do
          (_, (_, _, _, mineId), gs) <- board
          let (settled, after) = murderIt S.identityAnswer gs
          Spec.assertEqWith s "the trigger reached the stack all the same" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "but alice's hand is empty" (S.countByName ancestorName S.alice after) 0
          Spec.assertBool s (elem mineId (graveyardOf S.alice after)) "and the Ancestor is where it was"
        -- CR 702.46b: "if a permanent has multiple instances of soulshift, each
        -- triggers separately". Asserted of the MINT, as afterlife's multiplicity
        -- is: Forked-Branch Garami prints "soulshift 4, soulshift 4" and is not in
        -- the pool.
        Spec.it s "CR 702.46b each instance of soulshift is its own ability" $ do
          Spec.assertEqWith s "soulshift 3 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Soulshift 3) 2)) [Keyword.soulshift 3, Keyword.soulshift 3]
          Spec.assertEqWith s "and soulshift 4 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Soulshift 4) 1)) [Keyword.soulshift 4]
          Spec.assertBool s (Keyword.soulshift 3 /= Keyword.soulshift 4) "and the N reaches the minted ability"

-- Radiant Fountain, a Land: "When this land enters, you gain 2 life. / {T}: Add
-- {C}." A nonbasic land whose whole text box is one triggered ability and one
-- activated one, which is what makes it the pool's witness for CR 305.7's
-- "It loses all abilities generated from its rules text" reaching a TRIGGER.
--
-- The entry is staged the way Pawl.TriggerSpec's other entry fixtures stage it:
-- the permanent is placed, its Moved event appended to the log directly, and the
-- scan run at the next settle. CR 603.6a checks every battlefield permanent against
-- the event, and it reads each one's PROJECTION -- so a Blood Moon that has already
-- made the Fountain a Mountain leaves nothing there to trigger. The appended event
-- carries no sampled board, so this is the live-fallback reading in
-- Event.eventTriggers rather than the per-group one; the Blood Moon was standing
-- before the entry either way, so the two agree here.
strippedTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
strippedTriggerSpec s registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      entering oid gs =
        let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
         in resolveAll (settle (S.withEvents [GameEvent.Moved (Moved.MkMoved moved (Projection.project oid gs))] gs))
   in Spec.describe s "CR 305.7 strips a triggered ability" $ do
        Spec.it s "CR 603.6a Radiant Fountain's entry trigger gains its controller 2 life" $ do
          radiantFountain <- S.printingOf s registry "Radiant Fountain"
          let (fountainId, gs) = S.addCreature radiantFountain S.alice (Setup.emptyGame S.bothPlayers)
              after = entering fountainId gs
          Spec.assertEqWith s "20 + 2" (S.lifeOf S.alice after) (Just 22)
          Spec.assertBool s (ManaType.Colorless `elem` Mana.manaTypesOf fountainId after) "and it taps for colorless"
        Spec.it s "CR 305.7 under Blood Moon the same entry triggers nothing" $ do
          radiantFountain <- S.printingOf s registry "Radiant Fountain"
          bloodMoon <- S.printingOf s registry "Blood Moon"
          let (_, withMoon) = S.addCreature bloodMoon S.alice (Setup.emptyGame S.bothPlayers)
              (fountainId, gs) = S.addCreature radiantFountain S.alice withMoon
              after = entering fountainId gs
          Spec.assertBool s (Set.member Subtype.Mountain (Projection.subtypesOf fountainId after)) "it entered as a Mountain"
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack after) []
          Spec.assertEqWith s "and no life was gained" (S.lifeOf S.alice after) (Just 20)
          -- CR 305.7's last clause, on the same board: the printed mana ability
          -- goes and the new basic land type's replaces it.
          Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf fountainId after) "red instead"
          Spec.assertBool s (ManaType.Colorless `notElem` Mana.manaTypesOf fountainId after) "colorless gone"

-- CR 702.19b: assign each blocker exactly its lethal threshold and let the
-- excess trample through to the defending player. S.aggressiveAnswer cannot be
-- used here -- its AssignCombatDamage arm dumps the whole amount onto the first
-- CREATURE recipient it finds, so nothing would ever reach a player and the
-- trigger under test would never have an event to match.
tramplingAnswer :: Prompt.Prompt r -> r
tramplingAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockerEntries = Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds)
        spent = sum (fmap snd blockerEntries)
        leftover = if n >= spent then n - spent else 0
        toBlockers = Map.fromList blockerEntries
     in case filter (not . S.isCreatureRecipient) (Map.keys thresholds) of
          d : _ -> Map.insert d leftover toBlockers
          [] -> toBlockers
  _ -> S.aggressiveAnswer p

-- CR 603.10's FIRST sentence for a BYSTANDER: "objects that exist immediately
-- after an event are checked to see if the event matched any trigger
-- conditions". The scan runs once, at the CR 117.5 boundary, after CR 704.3's
-- state-based actions -- so a permanent that was on the battlefield when some
-- OTHER event in the same batch happened, and is gone by the time the scan
-- looks, has to be recovered from CR 608.2h last known information exactly as
-- CR 603.10a's look-back already recovers a departure event's own permanent.
--
-- Lightning Skelemental is the card: {B}{R}{R} Creature -- Elemental Skeleton
-- 6/1, "Trample, haste / Whenever this creature deals combat damage to a
-- player, that player discards two cards. / At the beginning of the end step,
-- sacrifice this creature." Its 1 toughness and CR 702.19b's trample are what
-- put the trigger's event and the bearer's death in ONE batch: the excess
-- reaches bob while the blocker's damage kills the Skelemental at the very next
-- CR 704.5g check, before any player gets priority.
bystanderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bystanderSpec s registry =
  Spec.describe s "Bystander" $ do
    -- Which side of CR 603.10 each condition falls on, asserted directly.
    -- Event.looksBack is a TOTAL case, so -Werror already forces a new condition
    -- to be classified; what it cannot force is the classification being the
    -- RIGHT one, and the four arms below are the ones a plausible wrong reading
    -- would flip. Each is the rule read against the constructor's own printed
    -- sentence:
    --
    --   * CR 603.10a names leaves-the-battlefield abilities, so "dies" is one
    --     (CR 700.4 narrows the destination, which does not leave the family);
    --   * and it names sacrifice triggers in as many words;
    --   * CR 603.6c's own last sentence puts "put into a graveyard from
    --     anywhere" OUTSIDE that family, which is the arm a wildcard would have
    --     gotten wrong in the expensive direction;
    --   * CR 708.8 leaves a permanent turned face up ON the battlefield, so
    --     there is no departure for a look-back to recover.
    Spec.it s "CR 603.10a the look-back families are the ones that rule lists" $ do
      Spec.assertBool s (Event.looksBack (TriggerCondition.PermanentDies (Filter.Type.And []))) "a dies trigger is a leaves-the-battlefield ability"
      Spec.assertBool s (Event.looksBack TriggerCondition.SelfLeavesTheBattlefield) "and so is the wider written form"
      Spec.assertBool s (Event.looksBack TriggerCondition.PermanentSacrificed) "a sacrifice trigger is named outright"
      Spec.assertBool s (not (Event.looksBack TriggerCondition.SelfPutIntoGraveyardFromAnywhere)) "CR 603.6c says put-into-a-graveyard-from-anywhere is not one"
      Spec.assertBool s (not (Event.looksBack (TriggerCondition.PermanentTurnedFaceUp (Filter.Type.And [])))) "and CR 708.8 leaves a turned-up permanent on the battlefield"
      -- CR 603.1b: one ability, several conditions -- it looks back if any of
      -- them does, and the pair below differ only in whether one does.
      Spec.assertBool s (Event.looksBack (TriggerCondition.AnyOf [TriggerCondition.SelfEnters, TriggerCondition.SelfDies])) "an AnyOf containing one looks back"
      Spec.assertBool s (not (Event.looksBack (TriggerCondition.AnyOf [TriggerCondition.SelfEnters, TriggerCondition.SelfTurnedFaceUp]))) "and one containing none does not"
    -- The proving test. bob holds THREE cards, so "discarded once" (one left)
    -- is distinguishable from "discarded twice" (none) and from "not at all"
    -- (three).
    Spec.it s "CR 603.10 whole cards: Lightning Skelemental dies to its blocker and STILL makes bob discard two" $ do
      skelemental <- S.printingOf s registry "Lightning Skelemental"
      piker <- S.printingOf s registry "Goblin Piker"
      case S.combatBoardOf [skelemental] [piker] of
        (base, [attacker], [blocker]) -> do
          let gs = List.foldl' (\g _ -> snd (S.addHandCard piker S.bob g)) base [(), (), ()]
              after = S.runCombat tramplingAnswer gs
          Spec.assertEqWith s "bob starts with three cards" (S.handSize S.bob gs) 3
          Spec.assertEqWith s "CR 702.19b: five trampled through to bob" (S.lifeOf S.bob after) (Just 15)
          Spec.assertBool s (not (S.onBattlefield attacker after)) "CR 704.5g: the Piker's two killed the 6/1"
          Spec.assertBool s (not (S.onBattlefield blocker after)) "and the Piker died to its one"
          Spec.assertEqWith s "the Skelemental is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
          Spec.assertEqWith s "and bob discarded two, exactly once" (S.handSize S.bob after) 1
        _ -> Spec.assertFailure s "fixture should give alice one attacker and bob one blocker"
    -- The control leg, which passes with or without the bystander recovery:
    -- unblocked, the Skelemental is still on the battlefield at the boundary,
    -- so `onBattlefield` carries it and the same trigger fires from the
    -- ordinary candidate source. It is what makes the card data and the
    -- reserved "that player" slot innocent when the leg above fails.
    Spec.it s "CR 510.1b control: an UNBLOCKED Skelemental survives and makes bob discard two the ordinary way" $ do
      skelemental <- S.printingOf s registry "Lightning Skelemental"
      piker <- S.printingOf s registry "Goblin Piker"
      case S.combatBoardOf [skelemental] [] of
        (base, [attacker], []) -> do
          let gs = List.foldl' (\g _ -> snd (S.addHandCard piker S.bob g)) base [(), (), ()]
              after = S.runCombat tramplingAnswer gs
          Spec.assertBool s (S.onBattlefield attacker after) "the Skelemental is still on the battlefield"
          Spec.assertEqWith s "bob took all six" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "and discarded two" (S.handSize S.bob after) 1
        _ -> Spec.assertFailure s "fixture should give alice one attacker and bob no blockers"
    -- The OTHER shape the rule reaches, at the gather rather than through a
    -- whole turn: a CR 603.2b step trigger whose bearer is gone by the
    -- boundary. Khabál Ghoul ("At the beginning of each end step, put a +1/+1
    -- counter on Khabál Ghoul for each creature that died this turn") is the
    -- bearer; the end step's beginning and the Ghoul's own death are two
    -- events in one unscanned batch, and the step event comes FIRST, so
    -- nothing about the Ghoul's own departure event can be what recovers it.
    Spec.it s "CR 603.10 a StepBegins bearer that dies later in the same batch still triggers" $ do
      khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
      let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
          began = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice)] gs0
          dead = S.runPure S.identityAnswer began (Event.destroy Regenerability.Regenerable [ghoul])
          triggers = fst (Event.gatherTriggers (Event.unscannedGrouped dead) dead)
      Spec.assertEqWith s "the Ghoul really did leave the battlefield" (Game.lookupObject ghoul dead) Nothing
      Spec.assertEqWith s "its step trigger still fired" (fmap PendingTrigger.source triggers) [TriggerSource.OfObject ghoul]
      Spec.assertEqWith s "under alice, who controlled it as it left (CR 603.3a)" (fmap PendingTrigger.controller triggers) [S.alice]
    -- The discriminating twin: a bearer that left the battlefield BEFORE the
    -- step began did not exist immediately after that event, and gets nothing.
    -- Same board, same two events, opposite order.
    Spec.it s "CR 603.10 a bearer that had already left before the event does NOT trigger" $ do
      khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
      let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
          dead = S.runPure S.identityAnswer gs0 (Event.destroy Regenerability.Regenerable [ghoul])
          began = S.runPure S.identityAnswer dead (State.modify' (Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice))))
          triggers = fst (Event.gatherTriggers (Event.unscannedGrouped began) began)
      Spec.assertEqWith s "the Ghoul is gone" (Game.lookupObject ghoul began) Nothing
      Spec.assertEqWith s "and nothing triggered" (fmap PendingTrigger.source triggers) []

-- CR 113.6m read off a BYSTANDER: the half of CR 603.10's first sentence
-- `bystanderSpec` above recovers, asked of a permanent whose ability functions
-- only in a graveyard.
--
-- The rule the recovery is not allowed to lose: "an ability whose cost or effect
-- specifies that it moves the object it's on out of a particular zone functions
-- only in that zone". A bystander is recovered from CR 608.2h last known
-- information, but what it is recovered AS is a permanent that was ON THE
-- BATTLEFIELD when the event happened -- so one of its abilities that functions
-- only in a graveyard was no more watching then than it would be now.
--
-- CR 603.10a is deliberately NOT this case. There the rule's own "unless its
-- trigger condition ... specifies that the object is put into that zone" arm
-- decides, and that half of the clause is unimplemented (#819) -- the Aura half
-- beside it is read, in `screamsFromWithinSpec` below; a bystander carries any
-- condition at all, so nothing about either reaches here.
--
-- The pair, chosen so that ONE derivation is the only difference between them:
--
--   * Squee, Goblin Nabob ({2}{R} Legendary Creature -- Goblin 1/1, "At the
--     beginning of your upkeep, you may return this card from your graveyard to
--     your hand"). CR 113.6k cannot reach it -- an upkeep condition triggers
--     perfectly well from the battlefield -- so only the effect's own words say
--     graveyard.
--   * Bitterblossom ({1}{B} Kindred Enchantment -- Faerie, "At the beginning of
--     your upkeep, create a 1/1 black Faerie Rogue creature token with flying and
--     you lose 1 life") as the control: the SAME trigger condition, on the same
--     battlefield, leaving in the same batch, with an effect that names no zone.
--     CR 113.6's default keeps it functioning on the battlefield.
--
-- (Both names, costs, type lines, P/T and oracle texts checked against Scryfall.)
--
-- Both leave the battlefield AFTER the upkeep begins and inside one unscanned
-- batch, which is `bystanderSpec`'s Khabál Ghoul shape: the step event comes
-- first, so nothing about either permanent's own departure event can be what
-- offers it.
bystanderZoneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bystanderZoneSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      squeeName = CardName.MkCardName (Text.pack "Squee, Goblin Nabob")
      faerieToken = CardName.MkCardName (Text.pack "Faerie Rogue Token")
      -- Takes every "you may", so a trigger that fires is observable as the card
      -- it moved rather than as a prompt count.
      exercising p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- alice's upkeep begins with Squee and Bitterblossom on her battlefield;
      -- `remove` then takes both off inside the same batch. Answers with the two
      -- battlefield ids and the sources the gather produced.
      board remove = do
        squee <- S.printingOf s registry "Squee, Goblin Nabob"
        bitterblossom <- S.printingOf s registry "Bitterblossom"
        let (squeeId, g1) = S.addCreature squee S.alice (Setup.emptyGame S.bothPlayers)
            (blossomId, g2) = S.addCreature bitterblossom S.alice g1
            began =
              S.withEvents
                [GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)]
                (g2 {GameState.phase = upkeep, GameState.activePlayer = S.alice})
            after = S.runPure S.identityAnswer began (remove [squeeId, blossomId])
        pure (squeeId, blossomId, after, fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped after) after)))
   in Spec.describe s "BystanderZone" $ do
        -- The proving leg. EXILE rather than a graveyard on purpose: it leaves
        -- the bystander reading as the only source that could offer Squee's
        -- ability at all, so the assertion cannot pass on the strength of the
        -- graveyard filtering that already landed with CR 113.6m's trigger half.
        Spec.it s "CR 113.6m a bystander's graveyard-functioning trigger does not fire from the battlefield it just left" $ do
          (squeeId, blossomId, after, sources) <- board (mapM_ (\oid -> Event.changeZone oid Zone.Exile))
          Spec.assertEqWith s "Squee really left the battlefield" (Game.lookupObject squeeId after) Nothing
          Spec.assertEqWith s "and so did the Bitterblossom" (Game.lookupObject blossomId after) Nothing
          Spec.assertBool s (null (Game.zoneMembers Zone.Graveyard S.alice after)) "neither card is in a graveyard, so no graveyard reading can be doing this"
          Spec.assertEqWith
            s
            "only the Bitterblossom, whose effect names no zone, is recovered as a bystander"
            sources
            [TriggerSource.OfObject blossomId]
        -- The same board with the ordinary destination. The battlefield
        -- incarnation still gets nothing, which is what this change is; the
        -- graveyard incarnation CR 400.7 mints is a different object under a
        -- different id, and CR 603.10's first sentence is what withholds THAT one
        -- from an event that predates its arrival -- the leg below is where that
        -- is proved, so this one still reads only the battlefield id.
        Spec.it s "CR 113.6m the same holds when the bystander dies to a graveyard" $ do
          (squeeId, blossomId, after, sources) <- board (Event.destroy Regenerability.Regenerable)
          Spec.assertEqWith s "Squee really left the battlefield" (Game.lookupObject squeeId after) Nothing
          Spec.assertBool s (TriggerSource.OfObject squeeId `notElem` sources) "the battlefield incarnation triggered nothing"
          Spec.assertBool s (TriggerSource.OfObject blossomId `elem` sources) "and the control still did"
        -- The ARRIVAL side of the same rule, driven through the turn machinery
        -- rather than through a hand-built log. CR 603.2b's step event is
        -- recorded as alice's upkeep begins, and only then does CR 704.3's check
        -- bury a Squee that CR 122.1a's -1/-1 counter had already taken to zero
        -- toughness (CR 704.5f) -- a strictly later event group of the same
        -- CR 117.5 batch. So the graveyard incarnation CR 400.7 minted did not
        -- exist immediately after the step began, and CR 603.10's first sentence
        -- checks an event against the objects that did: its CR 113.6m upkeep
        -- ability is no witness to an upkeep that had already begun.
        Spec.it s "CR 603.10 a card that reaches a graveyard mid-batch is no witness to an earlier event" $ do
          squee <- S.printingOf s registry "Squee, Goblin Nabob"
          bitterblossom <- S.printingOf s registry "Bitterblossom"
          let (squeeId, g1) = S.addCreature squee S.alice (Setup.emptyGame S.bothPlayers)
              (_, g2) = S.addCreature bitterblossom S.alice g1
              doomed = S.addCounter CounterKind.MinusOneMinusOne 1 squeeId g2
              atUpkeep =
                doomed
                  { GameState.phase = upkeep,
                    GameState.activePlayer = S.alice,
                    GameState.priority = Just S.alice,
                    GameState.remaining = Seq.drop 1 (GameState.remaining doomed)
                  }
              after = S.runPure exercising atUpkeep Engine.runStep
              squeesIn zone = filter (\oid -> fmap Face.name (Game.faceOf oid after) == Just squeeName) (Game.zoneMembers zone S.alice after)
          -- The discriminating assertion, and gameplay-level: the trigger that
          -- must not fire is the only thing that could move this card, and where
          -- the card ends up is what a player would see. `exercising` takes the
          -- "you may", so a fired trigger is a Squee in the hand.
          Spec.assertEqWith s "no Squee reached alice's hand" (length (squeesIn Zone.Hand)) 0
          -- The board really is the one the claim is about: a Squee IS in the
          -- graveyard at the boundary, under the fresh id CR 400.7 minted, so a
          -- live read of that zone would have offered it.
          Spec.assertEqWith s "and one Squee is in the graveyard" (length (squeesIn Zone.Graveyard)) 1
          Spec.assertBool s (squeeId `notElem` squeesIn Zone.Graveyard) "under a fresh id, not the battlefield one"
          -- The control, and the falsifier for over-narrowing: Bitterblossom's
          -- own upkeep trigger saw the same step event and fired, so "no Squee
          -- trigger" is not what a silenced batch looks like.
          Spec.assertEqWith s "the Bitterblossom's upkeep trigger still fired" (S.countOnBattlefieldByName faerieToken S.alice after) 1

-- CR 400.7e's slot read from the OTHER direction of a zone change: an entry.
-- "Abilities that trigger when an object moves from one zone to another ... can
-- find the new object that it became in the zone it moved to when the ability
-- triggered, if that zone is a public zone" -- and CR 400.2 lists the
-- battlefield among the public zones, so an enters trigger's payload may name
-- the entrant with no proviso to check.
--
-- Aether Flash, {2}{R}{R} Enchantment, "Whenever a creature enters, this
-- enchantment deals 2 damage to it." Soul Warden proved the CONDITION
-- (Pawl.TriggerSpec's permanentEntersSpec); its "you gain 1 life" names nothing
-- about the creature that entered. This is the first card whose EFFECT refers back to the
-- entrant.
--
-- The contrast with becameSlotSpec is the point of reusing one slot name.
-- There the bearer and the entrant are two incarnations of ONE card, and
-- `became` is the second of them; here the bearer is the enchantment and the
-- entrant is a different card entirely. CR 400.7e distinguishes neither
-- situation: it names "the new object that IT became", where "it" is whatever
-- moved, and the moved object being the bearer is a fact about the condition
-- rather than about the slot.
--
-- Goblin Piker ({1}{R} Creature -- Goblin Warrior 2/1) and Ogre Sentry ({1}{R}
-- Creature -- Ogre Warrior 3/3, defender) are the pair: identical costs and
-- colors, so two Mountains cast either, and the ONLY difference the test can be
-- reading is the toughness the 2 damage is measured against.
aetherFlashSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aetherFlashSpec s registry =
  let -- alice: Aether Flash already on the battlefield and two Mountains, with
      -- one creature card in hand. Casting it is the only thing on offer, so
      -- S.identityAnswer needs no bespoke interpreter.
      flashBoard creature = do
        mountain <- S.printingOf s registry "Mountain"
        aetherFlash <- S.printingOf s registry "Aether Flash"
        entrant <- S.printingOf s registry creature
        let (flashId, withFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
        pure (flashId, S.handOne entrant withFlash)
      castIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
         in S.runPure S.identityAnswer cast Engine.priorityLoop
      namesIn zone pid gs =
        fmap Face.name (Maybe.mapMaybe (\oid -> Game.faceOf oid gs) (Game.zoneMembers zone pid gs))
      damageEventsIn gs = Maybe.mapMaybe Event.damageOf (S.eventsOf gs)
      -- CR 120.3e's marked damage on the one battlefield permanent with this
      -- name. Nothing if it is not there, or if there is more than one of it.
      markedOn name gs =
        case filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just name) (Set.toList (GameState.battlefield gs)) of
          [oid] -> fmap Object.damage (Game.lookupObject oid gs)
          _ -> Nothing
      pikerName = CardName.MkCardName $ Text.pack "Goblin Piker"
      sentryName = CardName.MkCardName $ Text.pack "Ogre Sentry"
   in Spec.describe s "CR 400.7e the entrant an enters trigger names" $ do
        -- The gameplay-level proof, cast to resolution. The discriminating
        -- assertion is the GRAVEYARD: an ability whose `became` slot went
        -- unbound would resolve, find nothing under it and silently deal no
        -- damage, leaving a live 2/1 on the battlefield.
        Spec.it s "CR 603.6a whole card: a Goblin Piker enters and Aether Flash's 2 damage kills it (CR 704.5g)" $ do
          (flashId, board) <- flashBoard "Goblin Piker"
          let after = castIt board
          Spec.assertEqWith s "the Piker is not on the battlefield" (S.countOnBattlefieldByName pikerName S.alice after) 0
          Spec.assertEqWith s "it is in the graveyard, once" (namesIn Zone.Graveyard S.alice after) [pikerName]
          -- Falsifiers. The damage went to the creature, not to a player
          -- (CR 120.1a admits only battles, creatures and planeswalkers), and
          -- Aether Flash did not damage itself into the graveyard either.
          Spec.assertEqWith s "alice's life is untouched" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "bob's too" (S.lifeOf S.bob after) (Just 20)
          Spec.assertBool s (Set.member flashId (GameState.battlefield after)) "and the enchantment is still on the battlefield"
          Spec.assertEqWith s "exactly one damage event, of 2" (fmap DamageEvent.amount (damageEventsIn after)) [2]
        -- The control, differing only in the entrant's toughness: 2 damage
        -- marked on a 3/3 is not lethal (CR 704.5g compares the total marked
        -- against toughness), so the creature stays and CARRIES the mark. The
        -- marked damage is what proves the effect landed at all -- without it
        -- "still on the battlefield" would also be what a no-op looks like.
        Spec.it s "CR 704.5g the control: an Ogre Sentry survives the same 2 damage, marked" $ do
          (_, board) <- flashBoard "Ogre Sentry"
          let after = castIt board
          Spec.assertEqWith s "the Sentry is on the battlefield" (S.countOnBattlefieldByName sentryName S.alice after) 1
          Spec.assertEqWith s "the graveyard is empty" (namesIn Zone.Graveyard S.alice after) []
          Spec.assertEqWith s "with 2 damage marked on it" (markedOn sentryName after) (Just 2)
        -- eventBindings in isolation, so the binding is pinned to CR 400.7e
        -- rather than to Aether Flash's payload. The entrant is
        -- ZoneChange.object -- for an ENTRY the arriving incarnation is what
        -- the event is about, so `departed` would be the pre-move id of a card
        -- that is not on the battlefield at all.
        Spec.it s "CR 400.7e eventBindings binds the ENTRANT under became" $ do
          let castCard = ObjectId.MkObjectId 1
              entered = ObjectId.MkObjectId 2
              entry = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange castCard entered Zone.Stack Zone.Battlefield) S.emptyCharacteristics)
          Spec.assertEqWith s "became names the permanent that entered" (Event.eventBindings Nothing (TriggerCondition.PermanentEnters (Filter.Type.HasCardType CardType.Creature)) entry) (Map.singleton Binding.became (Binding.toObject entered))
        -- CR 603.6a's "EACH TIME an event puts one or more permanents onto
        -- the battlefield" met with a per-entrant payload: Dragon Fodder
        -- ({1}{R} Sorcery, "create two 1/1 red Goblin creature tokens") makes
        -- two entrants in one event, so one Aether Flash places two triggers
        -- and each has to name ITS OWN. Both tokens dying is what says so --
        -- two triggers sharing one binding would kill one token twice and
        -- leave the other standing.
        --
        -- A token is also the one entrant whose Moved event is
        -- battlefield-to-battlefield (Event.recordTokenEntry's pseudo-move,
        -- where `departed` and `object` are the same fresh id), so this is the
        -- shape where reading either field would look identical. It is here as
        -- the reminder that the fields agree for a token and only for a token.
        -- CR 704.5d then removes the dead tokens from the graveyard, so the
        -- damage's proof is the DamageDealt log, not a graveyard census.
        Spec.it s "CR 603.6a two tokens enter together and each trigger names its own" $ do
          mountain <- S.printingOf s registry "Mountain"
          aetherFlash <- S.printingOf s registry "Aether Flash"
          dragonFodder <- S.printingOf s registry "Dragon Fodder"
          let (_, withFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
              (gs, spellId) = S.handOne dragonFodder withFlash
              after = castIt (gs, spellId)
          Spec.assertEqWith s "no Goblin token survived" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Token") S.alice after) 0
          Spec.assertEqWith s "two damage events of 2, one per token" (fmap DamageEvent.amount (damageEventsIn after)) [2, 2]
          Spec.assertEqWith s "and they were dealt to two different objects" (Set.size (Set.fromList (fmap DamageEvent.target (damageEventsIn after)))) 2
        -- CR 608.2h, the case Aether Flash makes reachable with no second
        -- card: "if the effect requires information from a specific object
        -- ... the effect uses the current information of that object if it's
        -- in the public zone it was expected to be in". Two Aether Flashes,
        -- one 2/1 entrant, two triggers -- and the first one's damage kills it
        -- at the next state-based-action check, so the second resolves with
        -- its entrant already gone from the battlefield it was expected to be
        -- on. CR 400.7 minted a fresh id for the graveyard card, so the effect
        -- does not follow it there.
        Spec.it s "CR 608.2h a second Aether Flash resolves with the entrant already dead, and deals nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          aetherFlash <- S.printingOf s registry "Aether Flash"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, oneFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
              (_, twoFlashes) = S.addCreature aetherFlash S.alice oneFlash
              (gs, spellId) = S.handOne piker twoFlashes
              cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
              -- The creature spell resolves and enters; the settle's CR 117.5
              -- scan places both triggers.
              entered = S.runPure S.identityAnswer cast Stack.resolveTop
              placed = S.runPure S.identityAnswer entered Engine.settleForPriority
              -- One trigger resolves for 2, and the settle after it is where
              -- CR 704.5g destroys the 2/1.
              hit = S.runPure S.identityAnswer placed Stack.resolveTop
              buried = S.runPure S.identityAnswer hit Engine.settleForPriority
              -- The second trigger, resolving against an id CR 400.7 deleted.
              after = S.runPure S.identityAnswer buried Stack.resolveTop
          Spec.assertEqWith s "both triggers were placed" (length (GameState.stack placed)) 2
          Spec.assertEqWith s "the Piker is dead before the second resolves" (S.countOnBattlefieldByName pikerName S.alice buried) 0
          Spec.assertEqWith s "one damage event so far" (fmap DamageEvent.amount (damageEventsIn buried)) [2]
          Spec.assertEqWith s "the second trigger did resolve" (GameState.stack after) []
          Spec.assertEqWith s "and dealt nothing: still one damage event" (fmap DamageEvent.amount (damageEventsIn after)) [2]
          Spec.assertEqWith s "the card is in the graveyard once, not twice" (namesIn Zone.Graveyard S.alice after) [pikerName]

-- Bitterblossom {1}{B} Kindred Enchantment -- Faerie: "At the beginning of your
-- upkeep, you lose 1 life and create a 1/1 black Faerie Rogue creature token
-- with flying." The pool's first KINDRED card (CR 308), and so the first object
-- of any kind that carries a creature type without being a creature.
kindredSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kindredSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- Bitterblossom on alice's battlefield, alice's upkeep begun. The middle
      -- state is the one where the trigger is on the stack (CR 603.3b); the last
      -- is after it has resolved, so a Faerie Rogue token stands beside it.
      board bitterblossom =
        let (oid, gs) = S.addCreature bitterblossom S.alice (Setup.emptyGame S.bothPlayers)
            onStack = settle (beginUpkeep gs)
         in (oid, onStack, resolveAll onStack)
      slot = SlotName.MkSlotName (Text.pack "target")
      -- "Target Faerie ...", drawn from a pool: the same Pool + Filter machinery
      -- every printed target slot goes through (Pawl.ResolveSpec's "target
      -- Wall" is the same shape over a creature type that behaves ordinarily).
      faeriesIn pool gs =
        Map.findWithDefault
          Set.empty
          slot
          (Target.legalSets (Just S.alice) Map.empty S.noSource (Map.singleton slot (TargetSlot.required pool (Just (Filter.Type.HasSubtype Subtype.Faerie)))) gs)
   in Spec.describe s "Kindred" $ do
        -- The proving test for CR 308. CR 308.2 makes the kindred subtypes
        -- "the same as the set of creature subtypes", so the ENCHANTMENT
        -- answers a creature-type filter -- and CR 110.4's six permanent types
        -- do not include kindred, so it still answers no to being a creature.
        -- The token it makes is the control: a real Faerie creature, in both
        -- pools, which is what keeps the enchantment's absence from the
        -- creature pool from being a fixture that simply produced nothing.
        Spec.it s "CR 308.2 a Kindred Enchantment is a legal \"target Faerie permanent\", and CR 110.4 keeps it out of \"target Faerie creature\"" $ do
          bitterblossom <- S.printingOf s registry "Bitterblossom"
          let (blossomId, _, after) = board bitterblossom
              permanents = faeriesIn Pool.Permanents after
              creatures = faeriesIn Pool.Creatures after
          Spec.assertBool s (Set.member Subtype.Faerie (Projection.subtypesOf blossomId after)) "the enchantment carries the creature type Faerie"
          Spec.assertBool s (not (Projection.isCreatureOf blossomId after)) "and is not a creature"
          Spec.assertBool s (Set.member (Recipient.ToObject blossomId) permanents) "so \"target Faerie permanent\" offers it"
          Spec.assertEqWith s "alongside the token it made, and nothing else" (Set.size permanents) 2
          Spec.assertBool s (not (Set.member (Recipient.ToCreature blossomId) creatures)) "\"target Faerie creature\" does not"
          Spec.assertEqWith s "the token is the only one of those" (Set.size creatures) 1
        -- CR 308.1: "casting and resolving a kindred card follows the rules for
        -- ... the other card type", and here the other type is Enchantment, so
        -- nothing about the trigger is kindred-specific. What this pins is that
        -- the whole printed ability runs -- CR 603.3a's "your upkeep" (the
        -- ability controller's, CR 109.5), the life payment, and CR 111.1's
        -- token -- through the ordinary priority loop.
        Spec.it s "CR 603.3a Bitterblossom's upkeep trigger costs its controller 1 life and mints a 1/1 flying black Faerie Rogue" $ do
          bitterblossom <- S.printingOf s registry "Bitterblossom"
          let (blossomId, onStack, after) = board bitterblossom
          Spec.assertBool s (not (null (GameState.stack onStack))) "the upkeep trigger really reached the stack"
          Spec.assertEqWith s "no life was lost before it resolved" (S.lifeOf S.alice onStack) (Just 20)
          Spec.assertEqWith s "alice paid the 1 life" (S.lifeOf S.alice after) (Just 19)
          case filter (/= blossomId) (Set.toList (GameState.battlefield after)) of
            [token] -> do
              Spec.assertEqWith s "1/1" (Projection.powerOf token after, Projection.toughnessOf token after) (Just 1, Just 1)
              -- CR 202.2b/202.2e: a token has no mana cost, so the colour
              -- indicator is the only thing making it black.
              Spec.assertEqWith s "black" (Projection.colorsOf token after) (Set.singleton Color.Black)
              Spec.assertEqWith s "Faerie Rogue" (Projection.subtypesOf token after) (Set.fromList [Subtype.Faerie, Subtype.Rogue])
              Spec.assertBool s (Map.member Keyword.Type.Flying (Projection.keywordsOf token after)) "with flying"
              Spec.assertBool s (Projection.isCreatureOf token after) "and it, unlike its maker, IS a creature"
            other -> Spec.assertFailure s ("expected exactly one token beside Bitterblossom, got " <> show (length other) <> " other permanents")

-- CR 113.6m's Aura clause, over a whole card. The rule pins an ability whose
-- effect moves its own object out of a zone to that zone, "unless its trigger
-- condition ... specifies that ... the object it enchants leaves the
-- battlefield" -- and Screams from Within ({1}{B}{B} Enchantment -- Aura,
-- "Enchant creature / Enchanted creature gets -1/-1. / When enchanted creature
-- dies, return this card from your graveyard to the battlefield") is the
-- printing that turns on it. (Name, cost, type line and oracle text checked
-- against Scryfall.)
--
-- Without the clause `Event.zoneFunctionedFrom` answers Graveyard off the
-- effect, `Event.functionsIn Zone.Battlefield` is False, and `eventTriggers`'
-- `battlefieldAbilitiesOf` filter drops the ability -- so the ability never
-- triggers at all and the Aura sits in the graveyard CR 704.5m put it in. With
-- the clause it answers Nothing, `zonesTriggeredFrom` gives the battlefield, and
-- the trigger goes on the stack.
--
-- CR 700.4 is what makes the printed "dies" one of the departures the clause
-- names; CR 603.10a is what lets the trigger see a host that has already left;
-- and CR 608.2h's Pawl.Types.LastKnown.attachedTo is what lets the MATCH see the
-- link, CR 704.5m having taken the Aura off the battlefield in the same
-- CR 117.5 batch.
--
-- AND CR 400.7f is what lets the PAYLOAD act. Its effect moves "this card" out
-- of the graveyard, and CR 113.7a's source slot carries the battlefield id CR
-- 400.7 has already replaced -- so the card names Binding.became instead, which
-- Event.eventBindings stamps with the incarnation the Aura's own burial minted.
-- Both sentences of the rule are exercised below: the CR 704.5m burial at a
-- later SBA pass in the first leg, and "at the same time the enchanted permanent
-- left the battlefield" -- one wrath, one EventGroup -- in the second.
--
-- WHEN THE BOARD IS READ. CR 704.5m fires AGAIN on the Aura the moment it
-- arrives attached to nothing, so a reading taken after the next SBA pass finds
-- it back in the graveyard whether the rule is implemented or not. The zone
-- assertions below are therefore taken between Stack.resolveTop and that pass,
-- and say so.
--
-- The board. alice: the enchanted Hill Giant, plus a second Hill Giant. bob: a
-- Goblin Piker. Hill Giants rather than Pikers on alice's side because the
-- Aura's own -1/-1 would take a 2/1 to zero toughness (CR 704.5f) and bury the
-- enchanted creature before the test acts. The census below is per NAME and by
-- COUNT rather than by presence, which is what tells the Aura's own arrival from
-- the other one this batch mints: an arm binding the HOST's graveyard
-- incarnation instead returns a second Hill Giant and no Aura.
screamsFromWithinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
screamsFromWithinSpec s registry =
  let board = do
        screams <- S.printingOf s registry "Screams from Within"
        hillGiant <- S.printingOf s registry "Hill Giant"
        piker <- S.printingOf s registry "Goblin Piker"
        let (enchanted, g1) = S.addCreature hillGiant S.alice (Setup.emptyGame S.bothPlayers)
            (_, g2) = S.addCreature hillGiant S.alice g1
            (_, g3) = S.addCreature piker S.bob g2
            (aura, g4) = S.addCreature screams S.alice g3
        pure (enchanted, aura, screams, S.attach aura enchanted g4)
      -- Kill the enchanted creature, then run CR 117.5's own settle: the SBA
      -- pass buries the host and the now-unattached Aura, and the trigger is put
      -- on the stack after it.
      kill victim gs = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [victim] >> Engine.settleForPriority)
      -- Resolve the placed trigger and stop, WITHOUT settling again: CR 704.5m
      -- would bury the returned Aura on the next pass, and a board read after
      -- that cannot tell the fixed engine from the broken one.
      resolve gs = S.runPure S.identityAnswer gs Stack.resolveTop
      screamsName = CardName.MkCardName (Text.pack "Screams from Within")
      -- The battlefield as three counts, one per printing: the Aura back, the
      -- surviving Hill Giant, and bob's untouched Piker. Game.zoneMembers indexes
      -- the battlefield by OWNER (CR 108.3), which is the seat each was made
      -- under here.
      census gs =
        fmap
          (\(name, pid) -> S.countOnBattlefieldByName (CardName.MkCardName (Text.pack name)) pid gs)
          [("Screams from Within", S.alice), ("Hill Giant", S.alice), ("Goblin Piker", S.bob)]
      inGraveyard gs = length (filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just screamsName) (Game.zoneMembers Zone.Graveyard S.alice gs))
   in Spec.describe s "ScreamsFromWithin" $ do
        -- The proving leg. The discriminating quantity is what the stack holds:
        -- the Aura's trigger with the clause, nothing without it.
        Spec.it s "CR 113.6m an Aura whose trigger watches its host's death functions from the battlefield" $ do
          (enchanted, aura, screams, gs) <- board
          let after = kill enchanted gs
          Spec.assertEqWith
            s
            "CR 113.6m the Aura's death trigger is on the stack"
            (fmap (\oid -> fmap Object.source (Game.lookupObject oid after)) (GameState.stack after))
            (fmap (\ab -> Just (Source.OfTrigger TriggeredAbilitySource.MkTriggeredAbilitySource {TriggeredAbilitySource.source = aura, TriggeredAbilitySource.ability = ab, TriggeredAbilitySource.createdAt = Nothing})) (Face.triggeredAbilities (S.combinedFace screams)))
          -- The preconditions the assertion above rests on, AFTER it so neither
          -- can absorb a mutation aimed at the clause.
          Spec.assertEqWith s "the enchanted Hill Giant really died" (Game.lookupObject enchanted after) Nothing
          Spec.assertEqWith s "and CR 704.5m really took the Aura off the battlefield" (Game.lookupObject aura after) Nothing
          Spec.assertEqWith
            s
            "so the trigger was gathered off CR 608.2h last known information"
            (fmap LastKnown.attachedTo (Map.lookup aura (GameState.lastKnown after)))
            (Just (Just (Recipient.ToCreature enchanted)))
        -- CR 603.10a's own case, and the leg that makes `looksBack`'s arm
        -- load-bearing: the Aura leaves the battlefield in the SAME event group
        -- as its host -- a wrath -- so the live board holds neither, and the
        -- trigger can only be gathered from `sameGroup`, which is narrowed to the
        -- look-back conditions. CR 400.7f's proviso names this case in as many
        -- words ("put into that graveyard at the same time the enchanted
        -- permanent left the battlefield").
        Spec.it s "CR 603.10a the trigger still fires when the Aura dies in the same batch as its host" $ do
          (enchanted, aura, screams, gs) <- board
          let after = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [enchanted, aura] >> Engine.settleForPriority)
          Spec.assertEqWith
            s
            "CR 603.10a the Aura's death trigger is still on the stack"
            (fmap (\oid -> fmap Object.source (Game.lookupObject oid after)) (GameState.stack after))
            (fmap (\ab -> Just (Source.OfTrigger TriggeredAbilitySource.MkTriggeredAbilitySource {TriggeredAbilitySource.source = aura, TriggeredAbilitySource.ability = ab, TriggeredAbilitySource.createdAt = Nothing})) (Face.triggeredAbilities (S.combinedFace screams)))
          Spec.assertEqWith s "and both really left the battlefield in one batch" (fmap (`Game.lookupObject` after) [enchanted, aura]) [Nothing, Nothing]
        -- CR 400.7f's second sentence, and the unit's own proving leg: the Aura
        -- reached the graveyard "as a result of being put there as a state-based
        -- action for not being attached to a permanent", at a LATER SBA pass than
        -- its host's death, and the payload finds it there.
        --
        -- The census runs FIRST so no precondition can absorb a mutation, and it
        -- is the quantity no partial fix reaches by another route: the effect
        -- moves an object out of a graveyard, and only the right id puts an Aura
        -- on the battlefield rather than a Hill Giant or nothing.
        Spec.it s "CR 400.7f the payload finds the Aura's graveyard incarnation and returns it" $ do
          (enchanted, aura, _, gs) <- board
          let placed = kill enchanted gs
              after = resolve placed
          Spec.assertEqWith s "CR 400.7f the Aura is back on the battlefield, and only the Aura came back" (census after) [1, 1, 1]
          -- CR 400.7: a THIRD incarnation, so the returned permanent is neither
          -- the id that stood on the battlefield nor the one that was in the
          -- graveyard. The preconditions follow it.
          Spec.assertEqWith s "under a new id, the battlefield one being gone" (Game.lookupObject aura after) Nothing
          Spec.assertEqWith s "and alice's graveyard no longer holds it" (inGraveyard after) 0
          Spec.assertEqWith s "the trigger really resolved" (length (GameState.stack after)) 0
          Spec.assertEqWith s "and the board before it resolved held no Aura, one in the graveyard" (census placed, inGraveyard placed) ([0, 1, 1], 1)
        -- The same rule's FIRST sentence, on the wrath board above: host and Aura
        -- reach their graveyards in one EventGroup, "at the same time the
        -- enchanted permanent left the battlefield", and the payload finds it
        -- there too. Same reading moment and same census as the leg above.
        Spec.it s "CR 400.7f the payload finds it when the Aura died in the same batch as its host" $ do
          (enchanted, aura, _, gs) <- board
          let placed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [enchanted, aura] >> Engine.settleForPriority)
              after = resolve placed
          Spec.assertEqWith s "CR 400.7f the simultaneously buried Aura is back on the battlefield" (census after) [1, 1, 1]
          Spec.assertEqWith s "under a new id here too" (Game.lookupObject aura after) Nothing
        -- The over-rejection leg, and the reason the clause is a case on the
        -- CONDITION rather than an unconditional Nothing: Squee, Goblin Nabob's
        -- upkeep trigger says nothing about an enchanted object, so CR 113.6m's
        -- main sentence still pins it to the graveyard.
        Spec.it s "CR 113.6m control: an ordinary graveyard-recursion trigger is still pinned to the graveyard" $ do
          squee <- S.printingOf s registry "Squee, Goblin Nabob"
          screams <- S.printingOf s registry "Screams from Within"
          Spec.assertEqWith s "Squee's ability functions only in the graveyard" (fmap Event.zoneFunctionedFrom (Face.triggeredAbilities (S.combinedFace squee))) [Just Zone.Graveyard]
          Spec.assertEqWith s "the Aura's names no zone at all" (fmap Event.zoneFunctionedFrom (Face.triggeredAbilities (S.combinedFace screams))) [Nothing]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  cyclingTriggerSpec s registry
  graveyardTriggerSpec s registry
  gaeasBlessingSpec s registry
  graveyardEffectZoneTriggerSpec s registry
  droughtUpkeepSpec s registry
  commandZoneTriggerSpec s registry
  serraAvatarSpec s registry
  diesTriggerSpec s registry
  permanentDiesSpec s registry
  permanentsDieSpec s registry
  merenEndStepSpec s registry
  leavesBattlefieldSpec s registry
  permanentLeavesTheBattlefieldSpec s registry
  becameSlotSpec s registry
  promiseOfTomorrowSpec s registry
  promiseOfTomorrowReturnSpec s registry
  lookBackInterveningSpec s registry
  counterLookBackSpec s registry
  undyingSpec s registry
  afterlifeSpec s registry
  fabricateSpec s registry
  wardSpec s registry
  cutpurseSpec s registry
  gomazoaSpec s registry
  amuletSpec s registry
  soulshiftSpec s registry
  hauntSpec s registry
  screamsFromWithinSpec s registry
  strippedTriggerSpec s registry
  bystanderSpec s registry
  bystanderZoneSpec s registry
  aetherFlashSpec s registry
  kindredSpec s registry
