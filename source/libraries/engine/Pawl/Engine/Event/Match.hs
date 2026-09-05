-- The sole casing on TriggerCondition (CR 603.2): does this condition match
-- this event, given the game it happened in. Pure over the condition and the
-- event; the scan that asks it for every trigger source is
-- Pawl.Engine.Event.Trigger, and the funnel that raises the events is
-- Pawl.Engine.Event. Split out of Pawl.Engine.Event for size; nothing here
-- reaches the loop.
module Pawl.Engine.Event.Match where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Saga as Saga
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BecameAttached as BecameAttached
import qualified Pawl.Types.BecameAttacked as BecameAttacked
import qualified Pawl.Types.BecameBlocking as BecameBlocking
import qualified Pawl.Types.BecameDesignated as BecameDesignated
import qualified Pawl.Types.BecameTarget as BecameTarget
import Pawl.Types.Binding (Binding)
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import qualified Pawl.Types.CardLeavesGraveyard as CardLeavesGraveyard
import qualified Pawl.Types.ClassLevelChange as ClassLevelChange
import qualified Pawl.Types.CoinFlipped as CoinFlipped
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.ControlChanged as ControlChanged
import qualified Pawl.Types.ControllerBecomesTarget as ControllerBecomesTarget
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterPlacement as CounterPlacement
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.CreatureBecomesBlockedByAtLeast as CreatureBecomesBlockedByAtLeast
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePrevented as DamagePrevented
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Discarded as Discarded
import qualified Pawl.Types.Drew as Drew
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HalfUnlocked as HalfUnlocked
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Mentored as Mentored
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.PermanentWasSacrificed as PermanentWasSacrificed
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerAttacksPlayer as PlayerAttacksPlayer
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Types.PlayerDrawsNthCard as PlayerDrawsNthCard
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.SelfCountersReached as SelfCountersReached
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.Transformed as Transformed
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.VentureMarkerEntered as VentureMarkerEntered
import Pawl.Types.Zone (Zone)
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- CR 508.3a / 608.2i: how many times this object has been declared as an attacker
-- this turn, read out of the turn-scoped event log. Only Combat.declareAttackers
-- appends the event, which keeps CR 508.4's creature put onto the battlefield
-- attacking -- one that never attacked -- out of the count.
declarationsOf :: ObjectId -> GameState -> Int
declarationsOf bearer gs =
  let declaredIt event = case event of
        GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared oid _ _) -> oid == bearer
        _ -> False
   in length (Seq.filter (declaredIt . LoggedEvent.event) (GameState.events gs))

-- Clarion Spirit's "your SECOND spell each turn": which of the turn's matching
-- casts this one is, counting from one.
--
-- The window is the whole event log, which is exactly "this turn" for
-- Pawl.Engine.PlayerEffect.castsThisTurn's reason -- Engine.handoffTurn clears it
-- at the handoff. So the TurnScope beside the ordinal narrows which turns the
-- ability watches and never which casts are counted; on a turn the scope refuses,
-- the condition has already answered False without reaching here.
--
-- Counted over the casts the SAME condition admits, since the printed sentence
-- puts the ordinal inside the description -- so the Filter and the zone are
-- applied to each earlier cast too. Each earlier one is read through
-- Count.snapshotView, the CR 608.2h snapshot its event recorded, because CR
-- 601.2a's stack incarnation is long gone for every cast but the one being
-- matched.
--
-- POSITIONAL rather than a count of the whole log. CR 601.2i files the cast
-- before CR 603.2 checks the condition against it, so the log already holds this
-- cast -- and it may hold LATER ones too, since one CR 117.5 boundary can cover
-- several casts. The walk therefore stops at this cast's own entry, which the
-- spell's id names uniquely (CR 400.7 mints it and nothing else ever bears it),
-- and counts it: the turn's second cast answers 2 whatever was cast after it.
-- An event that is not in the log at all -- a fixture appending one directly --
-- is read as the latest, which is what the whole walk then counts against.
--
-- No board tells the walk from a count of the WHOLE log: mutating it away leaves
-- the suite green, because every cast a player can make gets its own CR 117.5
-- scan and no card in the pool casts twice in one resolution -- though a clause
-- list holding two Effect.OfferCast would, and the DSL admits one. So the stop
-- is a fence resting on the rule's ordering rather than a behaviour a test
-- proves.
castOrdinal :: Filter.Context -> Filter.Type.Filter Keyword.Type.Keyword -> Maybe Zone -> ObjectId -> GameState -> Natural
castOrdinal context predicate fromZone spell gs =
  let isThisCast cast = SpellWasCast.spell cast == spell
      earlier = Seq.takeWhileL (maybe True (not . isThisCast) . Game.castOf . LoggedEvent.event) (GameState.events gs)
      counted entry = case Game.castOf (LoggedEvent.event entry) of
        Nothing -> False
        Just cast ->
          maybe True (\z -> SpellWasCast.zone cast == Just z) fromZone
            && maybe False (\view -> Filter.matches context view predicate) (Count.snapshotView gs EventShape.SpellCast (LoggedEvent.event entry))
   in 1 + Natural.length (Seq.filter counted earlier)

-- CR 102.1: does this turn belong to the scope? `active` is "the player whose
-- turn it is", and `own` is the seat the scope is read against -- the player
-- Pawl.Types.TurnScope deliberately names none of, since each reader supplies
-- its own: CR 109.5's "you" for a triggered ability (CR 603.3a), the CR 602.2
-- activator for an activated one.
--
-- OpponentsTurn is CR 102.3's relation rather than an enumeration of opponents:
-- the active player is one, which in a two-player game (CR 102.2) and a
-- Free-for-All (CR 806.1) is any other seat, and in a game between teams is a
-- seat on another team. Teams.areOpponents is the one predicate.
turnScopeAdmits :: Teams.Teams -> TurnScope.TurnScope -> PlayerId -> PlayerId -> Bool
turnScopeAdmits teams scope active own = case scope of
  TurnScope.EachTurn -> True
  TurnScope.ControllersTurn -> active == own
  TurnScope.OpponentsTurn -> Teams.areOpponents teams own active

-- CR 122's removal as the two bearer-scoped counter-removal conditions read it:
-- the before/after pair of a GameEvent.CountersRemoved that took counters of
-- `wanted` off `bearer`, and Nothing for every other event.
--
-- ONE exhaustive case shared by TriggerCondition.SelfLastCounterRemoved and
-- TriggerCondition.SelfCountersRemoved rather than a copy each, because the two
-- ask the identical question of the identical constructor and differ only in what
-- they then do with the pair. A new GameEvent constructor still breaks the build
-- here, which is what the exhaustive list is for.
countersRemovedFrom :: ObjectId -> CounterKind.CounterKind Keyword.Type.Keyword -> GameEvent -> Maybe (Natural, Natural)
countersRemovedFrom bearer wanted event = case event of
  GameEvent.CountersRemoved (CounterChange.MkCounterChange oid kind before after)
    | oid == bearer && kind == wanted ->
        Just (before, after)
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.BecameAttached {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing
  GameEvent.Scried _ -> Nothing
  GameEvent.DungeonCompleted _ -> Nothing
  GameEvent.Surveiled _ -> Nothing
  GameEvent.DiceRolled _ -> Nothing
  GameEvent.ClassLevelSet _ -> Nothing
  GameEvent.Plotted _ -> Nothing
  GameEvent.Explored _ -> Nothing
  GameEvent.Exerted _ -> Nothing
  GameEvent.BecameAttacked _ -> Nothing
  GameEvent.AttackersDeclared _ -> Nothing
  GameEvent.BecameTapped _ -> Nothing
  GameEvent.BecameUntapped _ -> Nothing
  GameEvent.TappedForMana _ -> Nothing
  GameEvent.CoinFlipped {} -> Nothing
  GameEvent.RingTempted _ -> Nothing
  GameEvent.CardArrived _ -> Nothing
  GameEvent.Moved {} -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.TookInitiative _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BecameBlocking {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.Transformed {} -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing

-- The same question against a slot environment, for the one condition whose subject
-- is an object named EARLIER rather than the bearer or a class of objects
-- (TriggerCondition.LoseControlOfBound). Projection.controllerOf's `-Given` shape:
-- the plain name above defaults the extra argument, so a caller with nothing to say
-- says nothing.
--
-- An empty map is not a special case -- it simply names no slot, so the one arm
-- that reads a slot finds none and answers False.
matchesTriggerGiven :: Map.Map SlotName.SlotName Binding -> GameState -> ObjectId -> PlayerId -> TriggerCondition -> GameEvent -> Bool
matchesTriggerGiven bindings gs bearer you cond event = case cond of
  -- CR 603.6a: the bearer's own object entered the battlefield.
  TriggerCondition.SelfEnters -> case event of
    GameEvent.Moved (Moved.MkMoved zc _ _) -> ZoneChange.object zc == bearer && ZoneChange.to zc == Zone.Battlefield
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 603.6a's "whenever a [type] enters": a permanent the Filter admits
  -- entered the battlefield. The bearer frames the match rather than being it --
  -- it is the Filter.Context's source (so `Not IsSource` is Soul Warden's
  -- "another"), and its controller is the perspective CR 109.5 gives "you" in
  -- "a creature YOU CONTROL enters".
  TriggerCondition.PermanentEnters f -> case event of
    GameEvent.Moved (Moved.MkMoved zc _ _)
      | ZoneChange.to zc == Zone.Battlefield ->
          -- Deliberately NOT the snapshot the Moved event carries: that is the
          -- object as it last existed in the zone it LEFT, and reading it here
          -- would answer CR 603.6b backwards. The entrant's characteristics come
          -- from the game as it stands, which is what CR 603.10 asks for.
          --
          -- viewWithLastKnown rather than viewOfObject, so an entrant that has
          -- already left again -- a creature entering as a 0/0 and buried by CR
          -- 704.5f before the CR 117.5 boundary -- is still read as it was on the
          -- battlefield (CR 608.2h) instead of vanishing from the match.
          --
          -- Recomputed per (bearer, entry event) pair rather than shared: this is
          -- handed the GameState and nothing else. Forced only inside this arm, so
          -- a board with no such ability pays nothing.
          --
          -- Nothing is an entrant that is gone AND filed no last known information,
          -- about which no Filter can honestly answer.
          let entrant = ZoneChange.object zc
           in case Projection.viewWithLastKnown entrant gs entrant of
                Nothing -> False
                Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 603.2b: this step began, on a turn the scope admits.
  TriggerCondition.StepBegins (StepBegins.MkStepBegins wanted scope) -> case event of
    GameEvent.StepBegan (StepBegan.MkStepBegan began active) ->
      began == wanted && turnScopeAdmits (Game.teams gs) scope active you
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 603.8: a state trigger is not an event trigger. It never matches an entry
  -- in the log; stateTriggers below is its whole story.
  TriggerCondition.StateIs _ -> False
  -- CR 510.1b / 510.2: the bearer dealt COMBAT damage to a PLAYER. Combat damage
  -- already records a DamageDealt event, so this is a filter over the log.
  TriggerCondition.SelfDealsCombatDamageToPlayer -> case event of
    GameEvent.DamageDealt ev ->
      DamageEvent.source ev == bearer
        && DamageEvent.kind ev == DamageKind.Combat
        && isPlayerRecipient (DamageEvent.target ev)
    GameEvent.Moved {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 120.3: the bearer was DEALT damage -- enrage's event. The arm above with the
  -- identity check moved from the event's SOURCE to its RECIPIENT.
  --
  -- Neither of that arm's other two tests, and deliberately: rule 120.3 is about
  -- damage being dealt however it was dealt, and Ripjaw Raptor's printed phrase
  -- qualifies it in no way, so a Prodigal Sorcerer's ping fires this exactly as
  -- combat damage does. This is the damage arm that breaks the local pattern.
  --
  -- Recipient.objectOf, not a ToCreature test: CR 120.3's recipient may be any
  -- permanent (a planeswalker, a battle), and the bearer's own id is what decides
  -- the match either way. Nothing is dealt damage while the id is a player's, which
  -- is what the Nothing arm falls through on.
  TriggerCondition.SelfIsDealtDamage -> case event of
    GameEvent.DamageDealt ev -> Recipient.objectOf (DamageEvent.target ev) == Just bearer
    GameEvent.Moved {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- The same event read by a BYSTANDER (CR 510.1b / 510.2): a permanent the Filter
  -- admits dealt combat damage to a player. The Filter reads the event's DAMAGER,
  -- the bearer contributing only CR 109.5's "you" and the Filter.Context's source
  -- -- which is what would make Filter.IsSource the self-scoped reading.
  --
  -- viewWithLastKnown, not fullView: CR 603.10's first sentence wants the damager
  -- as it existed immediately after the damage, and pawl scans the log after CR
  -- 704's pass, so a trampler that connected and died to its blocker in the same
  -- CR 510.2 event is already gone. CR 608.2h's record is what still answers "was
  -- it a Wolf". No board in the pool reaches that -- Tovolar's Wolves are vanilla
  -- and unblocked -- so this is a fence rather than a tested branch, as is the
  -- DamageKind test beside it: no card in the pool makes a Wolf or Werewolf deal
  -- NONCOMBAT damage while a Tovolar watches.
  TriggerCondition.PermanentDealsCombatDamageToPlayer f -> case event of
    GameEvent.DamageDealt ev ->
      DamageEvent.kind ev == DamageKind.Combat
        && isPlayerRecipient (DamageEvent.target ev)
        && ( let damager = DamageEvent.source ev
              in case Projection.viewWithLastKnown damager gs damager of
                   Nothing -> False
                   Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
           )
    GameEvent.Moved {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 725.2: never matched via a card's bearer -- the monarch's crown-steal is
  -- an inherent ability of no object, so its real match lives in
  -- Pawl.Engine.Monarch.inherentMatch, not here.
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  -- CR 726.2, for the same reason one rule over: the initiative's two
  -- card-invisible conditions are gathered from the recorded events by
  -- Pawl.Engine.Initiative.inherentPending, not here.
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> False
  TriggerCondition.PlayerTookInitiative -> False
  -- CR 702.179d: the same, one rule over. The speed-increase ability hangs on no
  -- object either, so Pawl.Engine.Speed.inherentPending is where it is matched.
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  -- CR 603.12: a reflexive matches no log entry at all. Its trigger event
  -- happened during the resolution that CREATED it, which CR 603.12's own
  -- exception makes a question about the entry's existence rather than about the
  -- log -- so Event.delayedPending fires it with no event and this answers False
  -- for every one, StateIs' posture.
  TriggerCondition.Reflexive -> False
  -- CR 702.29c: the bearer IS the card that was cycled. The event carries the CR
  -- 400.7 incarnation, which is the object the scan offers as the bearer.
  --
  -- The CAUSE makes this narrower than the discard condition below, and is the
  -- whole of rule 702.29c's "to pay an activation cost of a cycling ability": an
  -- ordinary discard of a card that HAS cycling reaches the same graveyard through
  -- the same funnel and must fire nothing.
  TriggerCondition.SelfCycled -> case event of
    GameEvent.Discarded (Discarded.MkDiscarded _ oid DiscardCause.ToPayCyclingCost) -> oid == bearer
    GameEvent.Discarded (Discarded.MkDiscarded _ _ DiscardCause.Ordinary) -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 702.94a: the bearer IS the card that was revealed, and the reveal was
  -- miracle's own. SelfCycled's shape one rule over, cause and all -- and for the
  -- same reason: the same card shown by an ordinary reveal reaches the same log
  -- through the same funnel and must fire nothing. That is rule 702.94a's "THIS
  -- WAY", and CR 603.11 is what makes the two halves one ability.
  --
  -- The Ordinary arm is a REGRESSION FENCE rather than proven behaviour, and so
  -- is `revealedInHand`'s: the pool's ordinary hand reveals -- an activation cost
  -- paid from a hidden zone, and CR 614.1c's as-enters reveal (Rustic Clachan) --
  -- are on cards that carry no miracle, so no board tells the two readings apart
  -- and neither gate can be broken on its own while the other stands. Written
  -- because the rule says it.
  --
  -- The id in the event is the incarnation that reached the hand, which is the
  -- object the hand source offers as the bearer -- no CR 400.7 step separates
  -- them, since CR 701.20b moves nothing.
  --
  -- The FIRST-DRAW gate is NOT re-asked here. Rule 702.94a states it on the
  -- static half, so it decides whether the reveal happens at all
  -- (Event.offerMiracleReveal); a reveal that happened cannot have been the wrong
  -- draw, and re-reading GameState.drawsThisTurn at the scan would ask about the
  -- board one CR 117.5 boundary later.
  TriggerCondition.SelfRevealedForMiracle -> case event of
    GameEvent.Revealed (Revealed.MkRevealed _ oid RevealCause.ForMiracle _) -> oid == bearer
    GameEvent.Revealed (Revealed.MkRevealed _ _ RevealCause.Ordinary _) -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 701.9a: the bearer IS the card that was discarded. SelfCycled's shape
  -- above with the CAUSE dropped, which is the whole difference between the two:
  -- CR 702.29a makes cycling a discard, so "when you discard this card" fires on
  -- a cycle as well as on an ordinary discard, where rule 702.29c's "to pay an
  -- activation cost of a cycling ability" admits only the one cause.
  --
  -- The discarding player is not compared against anything. CR 701.9a moves the
  -- card from its OWNER's hand, and CR 113.8 makes that owner the controller of
  -- an ability of a card in a graveyard, so the two seats coincide by
  -- construction and there is no PlayerRelation to read.
  TriggerCondition.SelfDiscarded -> case event of
    GameEvent.Discarded (Discarded.MkDiscarded _ oid _) -> oid == bearer
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 701.9a: a card was discarded, by a player the relation admits. The
  -- discarding player comes from the event; CR 109.5 fixes "you" as the
  -- ability's controller (CR 603.3a), and PlayerRelation.holds is what each arm
  -- MEANS -- Megrim's "an opponent" is CR 102.3's player not on your team, which
  -- is every other player in a free-for-all (CR 806.1) and at two seats (CR
  -- 102.2). Every relation-carrying condition below
  -- reads it, so they cannot drift apart.
  --
  -- The bearer is NOT part of the match, unlike every Self- condition here: the
  -- enchantment watches someone else's hand and has nothing to do with the card
  -- that left it.
  --
  -- CR 702.29d -- "these abilities trigger only once when a card is cycled" --
  -- needs no clause of its own, and the DiscardCause is ignored for that reason
  -- rather than by omission. CR 702.29a makes cycling a discard, so a cycled
  -- card must fire this; the cycle is ONE Discarded event, so it fires it once.
  -- TriggerSpec's "CR 702.29d cycling a card fires the discard trigger exactly
  -- once" is the test that proves it.
  TriggerCondition.PlayerDiscards relation -> case event of
    GameEvent.Discarded (Discarded.MkDiscarded discarder _ _) -> PlayerRelation.holds (Game.teams gs) relation you discarder
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- The discard arm above narrowed by the CAUSE, which is the whole of the
  -- difference: CR 702.29a makes cycling a discard, so an ordinary discard
  -- reaches the same log through the same funnel and must fire nothing here.
  -- That cause is what rule 702.29c calls "to pay an activation cost of a
  -- cycling ability"; rule 702.29c itself defines only the self-scoped phrase,
  -- and what fixes the "you" of this watcher-scoped one is CR 603.3a, read
  -- through PlayerRelation.holds so a new relation is answered once.
  --
  -- The bearer is NOT part of the match, on PlayerDiscards' posture: Prickly
  -- Marmoset watches its controller's hand and has nothing to do with the card
  -- that left it.
  --
  -- CR 702.29d needs no clause: one cycle is one Discarded event, so this fires
  -- once by construction, exactly as the discard arm above does.
  TriggerCondition.PlayerCycles relation -> case event of
    GameEvent.Discarded (Discarded.MkDiscarded discarder _ DiscardCause.ToPayCyclingCost) -> PlayerRelation.holds (Game.teams gs) relation you discarder
    GameEvent.Discarded (Discarded.MkDiscarded _ _ DiscardCause.Ordinary) -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 121.1: a card was DRAWN, by a player the relation admits, and it was that
  -- player's `nth` draw of the turn. The ordinal comes off the event, which
  -- Event.drawCard stamped from GameState.drawsThisTurn as the draw happened;
  -- CR 109.5 fixes "you" as the ability's controller (CR 603.3a), and an
  -- opponent is every other player for the reason PlayerDiscards gives.
  --
  -- The bearer is NOT part of the match, exactly as for PlayerDiscards: Erudite
  -- Wizard is a creature watching a library, and the card drawn is nothing to do
  -- with it.
  --
  -- Equality on the ordinal, which is what makes "your second card" fire once in
  -- a turn with five draws.
  TriggerCondition.PlayerDrawsNthCard (PlayerDrawsNthCard.MkPlayerDrawsNthCard relation nth) -> case event of
    GameEvent.Drew (Drew.MkDrew drawer ordinal) ->
      ordinal
        == nth
        && PlayerRelation.holds (Game.teams gs) relation you drawer
    GameEvent.Discarded {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 725.1: a player BECAME the monarch. Matched against the event the
  -- crowning records, so every route through CR 725.1's "an effect instructs a
  -- player to become the monarch" fires it alike: Effect.BecomeMonarch records
  -- this event whichever MonarchTarget named the player, which covers an entry
  -- trigger's crown, a targeted crown and CR 725.2's stolen crown.
  --
  -- CR 725.4's departure reassignment records the same event, so the crown
  -- moving because the monarch left is matched alike. TriggerSpec's "CR 725.4 a
  -- departure crowns alice, and that crowning fires her edict" proves it.
  --
  -- The event carries exactly one player, which is CR 725.3 ("Only one player can
  -- be the monarch at a time") rather than a simplification -- so the relation is
  -- the whole comparison and there is no filter to apply. The bearer is NOT part
  -- of the match: Custodi Lich watches a designation, not itself.
  TriggerCondition.PlayerBecomesMonarch relation -> case event of
    GameEvent.BecameMonarch crowned -> PlayerRelation.holds (Game.teams gs) relation you crowned
    GameEvent.TookInitiative _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 508.3a: the bearer was DECLARED as an attacker. Matched against the
  -- declaration event rather than Combat.attackers, which keeps that rule's last
  -- sentence true -- a creature put onto the battlefield attacking is in the
  -- record and has no event here.
  TriggerCondition.SelfAttacks frequency -> case event of
    GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared oid _ _) ->
      oid == bearer && case frequency of
        TriggerFrequency.EveryTime -> True
        -- "For the first time each turn". The declaration being matched is
        -- already in the log when the scan reaches here, so "the first time" is
        -- "the only one so far", and the log's clearing at turn handoff is what
        -- makes it "each turn". Counted per BEARER, and CR 400.7 mints a new
        -- object on a zone change, so a creature that left and returned attacks
        -- for the first time again.
        TriggerFrequency.FirstTimeEachTurn -> declarationsOf bearer gs <= 1
    -- The other declaration. CR 509.1a's blocker is not an attacker, and a
    -- creature can be both this combat.
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 702.149a: the bearer was declared as an attacker, and at least one OTHER
  -- attacking creature satisfies the Filter. SelfAttacks' event and its identity
  -- check, with an existential over the rest of the declaration added.
  --
  -- The companions come from Combat.attackers, which at this moment IS the
  -- declaration: CR 508.2b puts these triggers on the stack before any player gets
  -- priority, so the only creatures CR 508.4 could add have had no window to
  -- arrive. The event log would answer the same question one turn too widely: it
  -- keeps an earlier combat phase's declarations, where the combat record is
  -- cleared per phase.
  --
  -- The source's power is supplied here rather than left Nothing, which is what
  -- makes CR 702.149a's PowerGreaterThanSource evaluable at a trigger match at
  -- all; it is a thunk for the reason Target.admittedGiven's is. Both it and each
  -- candidate view read through the bearer's last known information (CR 608.2h),
  -- a REGRESSION FENCE rather than a live path: the bearer was declared an
  -- attacker a moment ago and nothing has had priority since.
  TriggerCondition.SelfAttacksWithAnother f -> case event of
    GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared oid _ _)
      | oid == bearer ->
          let viewOf = Projection.viewWithLastKnown bearer gs
              context = Filter.contextComparingPower (Game.teams gs) (Just you) bearer (Filter.power =<< viewOf bearer)
              -- Rule 702.149a's "OTHER". Not independently observable while the
              -- Filter's comparison is strict -- nothing has power greater than
              -- its own -- so dropping it leaves the suite green; it is here
              -- because the rule says it, not because a test proves it.
              admits other = other /= bearer && maybe False (\view -> Filter.matches context view f) (viewOf other)
           in any admits (Map.keys (Combat.attackers (GameState.combat gs)))
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 506.5: a creature the Filter admits was declared as an attacker, and it
  -- was the ONLY one the declaration named. The same event SelfAttacks reads,
  -- with the count taken instead of the bearer's identity.
  --
  -- `count == 1` and never a floor, unlike SelfBlocksAtLeast's `>= n`: rule 506.5
  -- says "the ONLY creature declared as an attacker", which is one number.
  --
  -- The attacker's characteristics come from the game as it stands, which is CR
  -- 603.10's normal reading -- CR 508.1k has already made the creature attacking,
  -- and CR 508.2's triggers go on the stack before any player gets priority.
  -- viewWithLastKnown for PermanentEnters' reason.
  TriggerCondition.CreatureAttacksAlone f -> case event of
    GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared attacker _ count)
      | count == 1 ->
          case Projection.viewWithLastKnown attacker gs attacker of
            Nothing -> False
            Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 508.3a's second sentence: some creature was declared as an attacker, and CR
  -- 508.5's defending player for it is the bearer's controller. SelfAttacks' event
  -- with the identity check moved from the ATTACKER to the DEFENDER.
  --
  -- "You or a planeswalker you control" needs no second test and no board read: CR
  -- 508.5/508.5a already resolve an attacked planeswalker to its controller, and
  -- Combat.declareAttackers stamped that player onto the event. Where
  -- SelfAttacksPlayerWithMostLife below has to consult Combat.attackers -- rule
  -- 702.105a says "the player", so an attacked planeswalker must not count -- this
  -- condition wants exactly the field the event carries.
  --
  -- No Filter over the attacker and no count: CR 508.1a admits only creatures, and
  -- this fires once per declared attacker (CR 508.3a), not once per declaration --
  -- which is PlayerAttacks (CR 508.3d) and AttachedPlayerIsAttacked (CR 508.3b)
  -- below, each against its own event.
  TriggerCondition.CreatureAttacksYou -> case event of
    GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared _ defending _) -> defending == you
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 508.3d: the player the payload names declared one or more attackers. The
  -- once-per-DECLARATION arity, matched against the once-per-declaration event --
  -- CreatureAttacksYou above reads the per-attacker one and
  -- AttachedPlayerIsAttacked below the per-target one, and no grouping happens
  -- here for their reason: this function sees one event at a time.
  --
  -- Rule 508.3d's "[a player]" as the card printed it, against CR 109.5's `you`.
  -- Read off the event, which Combat.declareAttackers stamps with CR 508.1's
  -- declaring player, rather than off GameState.activePlayer: the two agree
  -- today, CR 508.1 letting only the active player declare, but the rule asks
  -- who declared and the event is the record of that.
  --
  -- No bearer test, where SelfAttacks pins one: rule 508.3d's subject is a
  -- player, so the bearer only frames whose declaration this is --
  -- CreatureAttacksAlone's bystanding posture. A Boggart Prankster held out of
  -- combat still triggers on its controller's attack.
  --
  -- Pawl.EventTriggerSpec's Avatar Roku, Firebender group proves the relation is
  -- READ: its two boards -- an opponent declares, then Roku's own controller
  -- does -- falsify hardcoding You and hardcoding Opponent respectively. Roku's
  -- payload adds mana rather than targeting, which is why those boards see a
  -- difference where Boggart Prankster's "target attacking Goblin you control"
  -- cannot.
  --
  -- The You NARROWING is still a regression fence rather than a proven
  -- behaviour: answering True unconditionally leaves the whole suite green,
  -- because the corpus's one You producer is Boggart Prankster, and on any board
  -- where a non-controller declares, its trigger has no legal target and CR
  -- 603.3d removes it either way. What would observe it is a card printing
  -- "whenever you attack" whose payload does not need its controller to have an
  -- attacker.
  TriggerCondition.PlayerAttacks relation -> case event of
    GameEvent.AttackersDeclared attacker -> PlayerRelation.holds (Game.teams gs) relation you attacker
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
  -- CR 508.3c: the player the payload names declared at least the payload's
  -- number of attackers the Filter admits. The arm above narrowed, against the
  -- same once-per-DECLARATION event, because that is the arity every printing of
  -- "attacks with" takes -- the quantifier is in the printed sentence ("one or
  -- more Birds", "two or more creatures"), so a declaration naming four Birds
  -- fires it once. CR 508.3a's per-attacker event sits below answering False:
  -- matching it here would fire once per declared Bird instead.
  --
  -- AT LEAST, never exactly, which is how the printed "N or more" reads and how
  -- TriggerCondition.SelfBlocksAtLeast reads its own rule.
  --
  -- The creatures come from Combat.declaredAttackers rather than from the event,
  -- which carries only who declared, and NOT from Combat.attackers as
  -- SelfAttacksWithAnother reads it: CR 508.4 says a creature put onto the
  -- battlefield attacking never "attacked", and putOntoBattlefieldAttacking
  -- writes the second map and not this one. A REGRESSION FENCE rather than a
  -- proved behaviour: reading Combat.attackers here leaves the suite green,
  -- since no board reaches a declaration with such a creature already in combat.
  --
  -- Exact at this moment for SelfAttacksWithAnother's reason -- CR 508.2b puts
  -- every trigger from this declaration on the stack together, so no player has
  -- had priority since.
  --
  -- Rule 508.3c's "that player CONTROLS" is Combat.joinedUnder, CR 506.4's
  -- record of who controlled each combatant as it joined. Not independently
  -- observable: CR 508.1a lets only the active player declare, so every id in
  -- declaredAttackers joined under the declarer, and dropping the comparison
  -- leaves the suite green. It is here because the rule says it.
  --
  -- viewWithLastKnown and the Filter context framed by the bearer, exactly as
  -- SelfBlocksOneOrMore's arm below does it. Nothing is bound, so the context's
  -- empty slot map is honest here.
  TriggerCondition.PlayerAttacksWith (PlayerAttacksWith.MkPlayerAttacksWith relation f floor_) -> case event of
    GameEvent.AttackersDeclared attacker
      | PlayerRelation.holds (Game.teams gs) relation you attacker ->
          let combat = GameState.combat gs
              admits oid =
                Map.lookup oid (Combat.joinedUnder combat) == Just attacker
                  && maybe False (\view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f) (Projection.viewWithLastKnown oid gs oid)
           in Natural.length (filter admits (Set.toList (Combat.declaredAttackers combat))) >= floor_
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
  -- CR 508.3b: the player this ability's source is attached to was attacked.
  -- CreatureAttacksYou's question asked once per DECLARATION instead, which is
  -- the whole of what separates them: this matches the grouped
  -- GameEvent.BecameAttacked, recorded once per distinct target, so the arity is
  -- the event's and no dedup happens here.
  --
  -- The subject comes from Object.attachedTo (CR 303.4m), read live rather than
  -- through last known information as AttachedCreatureDies reads it: that arm
  -- matches an event that may have taken the bearer with it, and a declaration of
  -- attackers moves nothing.
  --
  -- ONLY AttackTarget.OfPlayer matches. CR 508.1b lists player, planeswalker and
  -- battle separately and rule 508.3b asks about the one attacked, so a creature
  -- sent at a planeswalker the enchanted player controls leaves this silent --
  -- where CreatureAttacksYou, reading CR 508.5's defending player, would fire.
  TriggerCondition.AttachedPlayerIsAttacked -> case event of
    GameEvent.BecameAttacked payload ->
      case Recipient.playerOf =<< (Object.attachedTo =<< Game.lookupObject bearer gs) of
        Just enchanted -> BecameAttacked.target payload == AttackTarget.OfPlayer enchanted
        Nothing -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 508.3e: the player the payload names declared attackers, and at least one
  -- of them was sent at a PLAYER. AttachedPlayerIsAttacked's event and its
  -- per-TARGET arity, with the subject read off a relation instead of off the
  -- bearer's attachment -- so one declaration split across two opponents fires
  -- this TWICE where PlayerAttacks above fires once, which is what parts rule
  -- 508.3e from rule 508.3d.
  --
  -- Both sides come off the EVENT. Combat.declareAttackers stamps CR 508.1's
  -- declaring player onto it beside the target, which is what lets this arm ask
  -- rule 508.3e's question at all; reading GameState.activePlayer for the
  -- attacker would agree today, rule 508.1 letting only the active player
  -- declare, but the rule asks who declared.
  --
  -- Both sides are QUALIFIED, each by its own relation: Lulu, Stern Guardian's
  -- "whenever an opponent attacks you" is Opponent over the declarer and You
  -- over the target, where Seifer's "whenever you attack a player" is You and
  -- AnyPlayer. Both are read against CR 109.5's "you" -- the ability's
  -- controller -- and not against each other, so Opponent on the ATTACKED side
  -- would say "somebody other than me was attacked" rather than restate CR
  -- 506.2a's requirement that the defending player be an opponent of the
  -- attacker.
  --
  -- ONLY AttackTarget.OfPlayer, which is rule 508.3e's last sentence in as many
  -- words: "it won't trigger if a creature attacks a planeswalker or a battle".
  -- The other exclusion in that sentence -- a creature put onto the battlefield
  -- attacking -- holds by construction instead, CR 508.4 saying such a creature
  -- was never declared and Combat.putOntoBattlefieldAttacking recording no
  -- event.
  --
  -- No bearer test, PlayerAttacks' bystanding posture above: rule 508.3e's two
  -- subjects are both players, so a Seifer held out of combat still triggers on
  -- its controller's attack.
  TriggerCondition.PlayerAttacksPlayer subjects -> case event of
    GameEvent.BecameAttacked payload -> case BecameAttacked.target payload of
      AttackTarget.OfPlayer attacked ->
        PlayerRelation.holds (Game.teams gs) (PlayerAttacksPlayer.attacker subjects) you (BecameAttacked.attacker payload)
          && PlayerRelation.holds (Game.teams gs) (PlayerAttacksPlayer.attacked subjects) you attacked
      AttackTarget.OfPlaneswalker _ -> False
      AttackTarget.OfBattle _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 702.105a: the bearer was declared attacking A PLAYER, and no player still in
  -- the game has more life than that one. SelfAttacks' event and its identity
  -- check, with the comparison added.
  --
  -- Whom the bearer attacked comes from Combat.attackers rather than from the
  -- event, which carries CR 508.5's DEFENDING player instead -- the same id for an
  -- attacked planeswalker or battle, where rule 702.105a names the player. Reading
  -- the record here is exact for SelfAttacksWithAnother's reason.
  --
  -- Non-strict, which is rule 702.105a's "or tied for most life", and over
  -- Game.stillPlaying rather than every seat the game began with: a player who has
  -- left (CR 800.4a) has no life total left to be beaten.
  TriggerCondition.SelfAttacksPlayerWithMostLife -> case event of
    GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared oid _ _)
      | oid == bearer ->
          case Map.lookup bearer (Combat.attackers (GameState.combat gs)) of
            Just (AttackTarget.OfPlayer attacked) ->
              let lifeOf pid = fmap Player.life (Map.lookup pid (GameState.players gs))
               in case lifeOf attacked of
                    Nothing -> False
                    Just theirs -> all (maybe True (<= theirs) . lifeOf) (Game.stillPlaying gs)
            _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 509.3a: the bearer was DECLARED as a blocker. SelfAttacks' mirror, and
  -- matched against GameEvent.BlocksDeclared for that arm's reason -- CR 509.4's
  -- creature put onto the battlefield blocking is in Combat.blockers, and the
  -- only event it records on the blocking side is a GameEvent.BecameBlocking
  -- this condition does not read, which is rule 509.3a's last sentence.
  --
  -- The attacking creatures the declaration named are neither compared nor bound:
  -- this condition is CR 509.3a's, which names none. SelfBlocksCreature's arm
  -- below is rule 509.3b's, which does.
  --
  -- The GROUPED event, which is rule 509.3a's "only once each combat for that
  -- creature, even if it blocks multiple creatures": a blocker declared against
  -- two attackers makes two BecameBlocking and one BlocksDeclared, so matching
  -- the pairwise one here would fire twice.
  TriggerCondition.SelfBlocks -> case event of
    GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared blocker _) -> blocker == bearer
    GameEvent.BecameBlocking {} -> False
    -- The bearer BECOMING blocked is the other side of the same declaration and
    -- not this condition: CR 509.3a's creature is the one doing the blocking.
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 509.3b: the PAIRWISE event, which is that rule's "once for each attacking
  -- creature the creature with the ability blocks" -- and the difference from
  -- SelfBlocks above, together with the attacker eventBindings stamps under
  -- Binding.blockedCreature.
  --
  -- The Filter is a predicate over that ATTACKER, read from the game as it
  -- stands, which is rule 509.3f's "at the point blockers are declared" for
  -- SelfBecomesBlockedBy's reason below. viewWithLastKnown for that arm's reason
  -- too -- an attacker already gone (CR 608.2h) is still read as it was on the
  -- battlefield.
  TriggerCondition.SelfBlocksCreature f -> case event of
    GameEvent.BecameBlocking b
      | BecameBlocking.blocker b == bearer,
        -- CR 509.3b's last sentence: "It won't trigger if the creature is put
        -- onto the battlefield blocking." CR 509.3d's arm below is where that
        -- same event does fire, which is the whole reason it is recorded.
        --
        -- Proved rather than fenced, since Aetherplasm: it puts a creature
        -- CARD onto the battlefield blocking, so the arrival brings its own
        -- text. Pawl.CombatEffectSpec puts Loyal Sentry out that way against a
        -- 2/2 and reddens here when this guard is dropped, with the same Sentry
        -- DECLARED as the control leg where it does fire.
        not (BecameBlocking.putOntoBattlefield b) ->
          let attacker = BecameBlocking.attacker b
           in case Projection.viewWithLastKnown attacker gs attacker of
                Nothing -> False
                Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.BecameBlocking {} -> False
    -- CR 509.3a's grouped event is the once-per-combat one, and matching it here
    -- would lose a blocker's second attacker.
    GameEvent.BlocksDeclared {} -> False
    -- CR 509.3c's grouped event is the bearer BECOMING blocked, which is not a
    -- block by it.
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 509.3e: the bearer blocked at least `n` creatures. SelfBlocks with the
  -- count read, on the very same grouped event -- which is what makes rule
  -- 509.3e's "when blockers are declared" the moment this fires.
  --
  -- Rule 509.3e's "effects that add or remove blockers" also cause it to trigger,
  -- and no such effect is in the pool: the count is the declaration's (#1146).
  TriggerCondition.SelfBlocksAtLeast n -> case event of
    GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared blocker count) -> blocker == bearer && count >= n
    GameEvent.BecameBlocking {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 509.3e: the bearer blocked at least one creature the Filter admits. The
  -- same grouped event SelfBlocks and SelfBlocksAtLeast read, so the printed "one
  -- or more" fires once for the whole declaration; SelfBlocksCreature's arm above
  -- is the per-attacker reading, and two admitted attackers tells them apart.
  --
  -- The attackers come from Combat.blockers rather than from the event, which
  -- carries a count and no ids -- the count being unfiltered, and this condition
  -- asking about a quality. That map is keyed by ATTACKER, so the bearer's own
  -- entries are the ones whose blocker set holds it. Exact at this moment for
  -- SelfAttacksWithAnother's reason: CR 509.2a puts these triggers on the stack
  -- before any player gets priority, so the record still holds the declaration
  -- that made the event.
  --
  -- viewWithLastKnown, and the Filter context framed by the bearer, exactly as
  -- SelfBecomesBlockedBy's arm below does it.
  TriggerCondition.SelfBlocksOneOrMore f -> case event of
    GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared blocker _)
      | blocker == bearer ->
          let admits attacker = maybe False (\view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f) (Projection.viewWithLastKnown attacker gs attacker)
              blocked = [attacker | (attacker, blockers) <- Map.toList (Combat.blockers (GameState.combat gs)), Set.member bearer blockers]
           in any admits blocked
    GameEvent.BlocksDeclared {} -> False
    -- The PAIRWISE event is CR 509.3b's, and matching it here would fire once per
    -- attacker blocked rather than once for the declaration.
    GameEvent.BecameBlocking {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 509.3c: the bearer BECAME a blocked creature, which CR 509.1h makes the
  -- declaration's other product. SelfBlocks' arm above is the mirror.
  --
  -- One event per blocked attacker is what Combat.declareBlockers records, so
  -- matching it once is rule 509.3c's "only once each combat for that creature".
  -- A match on GameEvent.BecameBlocking's attacker would fire once per blocker
  -- instead; Pawl.TriggerSpec's two-blocker case is what tells the two apart.
  TriggerCondition.SelfBecomesBlocked -> case event of
    GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked oid _ _) -> oid == bearer
    -- The same declaration's other branch, and CR 509.1h makes the two exclusive
    -- for any one attacker.
    GameEvent.AttackerUnblocked _ -> False
    -- CR 509.4's creature put onto the battlefield blocking never "blocked", but
    -- that is not why this is False: a blocker's own declaration is CR 509.3a's
    -- event, whoever it was declared against.
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 509.3d: a creature the Filter admits became a blocking creature FOR the
  -- bearer. The pair on GameEvent.BecameBlocking is read from the ATTACKING
  -- side, which is what makes this fire once per blocker where
  -- SelfBecomesBlocked's arm above fires once per attacker.
  --
  -- BecameBlocking.putOntoBattlefield is deliberately not read: rule 509.3d's
  -- third sentence is "In addition, it will trigger if a creature is put onto
  -- the battlefield blocking that creature", the one form of CR 509.3 that CR
  -- 509.4's "never blocked" does not silence. CR 509.3b's arm above reads the
  -- flag, and that difference is the rule.
  --
  -- The blocker's characteristics come from the game as it stands, which is rule
  -- 509.3f's "at the point it becomes a blocking creature": CR 509.2a puts these
  -- triggers on the stack before any player gets priority, so nothing has had a
  -- window to change them. viewWithLastKnown for PermanentEnters' reason -- a
  -- blocker already gone (CR 608.2h) is still read as it was on the battlefield.
  TriggerCondition.SelfBecomesBlockedBy f -> case event of
    GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = blocker, BecameBlocking.attacker = attacker})
      | attacker == bearer ->
          case Projection.viewWithLastKnown blocker gs blocker of
            Nothing -> False
            Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    -- The GROUPED event is CR 509.3c's, and matching it here would collapse two
    -- blockers into one trigger.
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 509.3d read by a BYSTANDER: the Filter is asked of the ATTACKER, where
  -- the arm above asks it of the blocker and compares the attacker against the
  -- bearer. CR 701.54c's three-temptation tier is the caller, and its Filter is
  -- Pawl.Engine.Ring.yourRingBearer -- the emblem is not in the event at all, so
  -- nothing here compares an id to the bearer.
  --
  -- BecameBlocking.putOntoBattlefield is not read, for the arm above's reason:
  -- rule 509.3d's third sentence admits a creature put onto the battlefield
  -- blocking.
  --
  -- No arm on GameEvent.AttackerBlocked, which is CR 509.3c's grouped event:
  -- rule 701.54c sacrifices "the blocking creature", one per blocker, and the
  -- grouped event names none of them.
  TriggerCondition.PermanentBecomesBlockedBy f -> case event of
    GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.attacker = attacker}) ->
      case Projection.viewWithLastKnown attacker gs attacker of
        Nothing -> False
        Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.BlocksDeclared {} -> False
    -- The GROUPED event is CR 509.3c's, and matching it here would collapse two
    -- blockers into one trigger.
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 509.3e read from the attacking side: the bearer became blocked, by at
  -- least one creature the Filter admits. The GROUPED event, which is the printed
  -- "one or more" -- the arm above fires once per blocker, and two admitted
  -- blockers tells the two apart.
  --
  -- The blockers come from Combat.blockers for SelfBlocksOneOrMore's reason:
  -- GameEvent.AttackerBlocked names the attacker, CR 508.5's defending player and
  -- CR 509.3e's count, and no blocker at all. The map is keyed by attacker, so the
  -- bearer's own entry is the whole answer here.
  --
  -- That entry is the LIVE one, where the arm below takes its set off the event.
  -- The two agree wherever a board can tell them apart: on CR 509.1's road the
  -- event is recorded after the declaration's own write and nothing else has run
  -- (CR 509.2a), and on CR 509.4's road CR 509.3c withholds it unless the
  -- attacker was unblocked, so the only ids the live entry adds are arrivals from
  -- the same batch -- and a batch is one Resolve.Create's tokens, minted from one
  -- spec, which no Filter can tell apart.
  TriggerCondition.SelfBecomesBlockedByOneOrMore f ->
    let admits blocker = maybe False (\view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f) (Projection.viewWithLastKnown blocker gs blocker)
     in case event of
          GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked attacker _ _)
            | attacker == bearer -> any admits (Set.toList (Map.findWithDefault Set.empty bearer (Combat.blockers (GameState.combat gs))))
          GameEvent.AttackerBlocked {} -> False
          GameEvent.AttackerUnblocked _ -> False
          -- Rule 509.3e's second sentence -- "effects that add or remove
          -- blockers can also cause such abilities to trigger" -- for the
          -- producer the pool has: a creature PUT ONTO THE BATTLEFIELD blocking.
          -- The arm CreatureBecomesBlockedByAtLeast carries below, with the
          -- count traded for this condition's Filter, which is the only
          -- difference between the two conditions.
          --
          -- The CROSSING and not the arrival: the bearer becomes blocked by one
          -- or more admitted creatures at the moment the FIRST admitted one
          -- joins, so an arrival that merely adds a second is no new event. That
          -- is what `not (any admits prior)` says, where the count arm says
          -- `== n`. Removing blockers cannot reach this form either -- a
          -- departure only shrinks the admitted set.
          --
          -- `prior` is BecameBlocking.blockersBefore, recorded at the arrival
          -- rather than reconstructed here: CR 509.2a puts these triggers on the
          -- stack before any player gets priority, so the bearer's live entry
          -- holds this arrival AND every arrival that came after it in the same
          -- batch. Deleting this one from that entry left the batch's others in,
          -- and each of a doubled pair then read the other as a prior blocker.
          -- A REGRESSION FENCE on this arm, where the count arm below proves it:
          -- no pooled board reaches a doubled arrival this Filter admits, Flash
          -- Foliage's Saprolings being green where the one filtered printing
          -- asks for black.
          --
          -- A blocker that has since left keeps its id in that entry, and
          -- `admits` reads it through CR 608.2h last known information -- which
          -- is the right answer rather than a leak. An attacker declared blocked
          -- by an admitted creature became blocked by one THEN, so a later
          -- arrival is no new becoming however the first one ended.
          --
          -- BecameBlocking.attackerWasBlocked is "GameEvent.AttackerBlocked was
          -- not also recorded for this same arrival": CR 509.3c withholds that
          -- event exactly when the attacker was already blocked, and without
          -- this conjunct an arrival that is the attacker's FIRST blocker would
          -- be answered by the arm above and by this one both. A REGRESSION
          -- FENCE rather than proved behaviour: no pooled board can reach an
          -- arrival that is admitted AND lands on an unblocked attacker.
          -- Aetherplasm has to be blocking to put anything out, so the attacker
          -- is blocked by then whatever became of the declared blocker; and
          -- Flash Foliage, which can name an unblocked attacker, makes a GREEN
          -- Saproling where the only filtered printing on this side asks for
          -- black. Relaxing the conjunct to a wildcard leaves the whole suite
          -- green.
          --
          -- BecameBlocking.putOntoBattlefield is load-bearing beside it:
          -- Combat.declareBlockers records this same constructor once per
          -- declared PAIR with that flag clear, so an arm without it would
          -- answer an ordinary declaration the arm above has already answered,
          -- once more per blocker.
          --
          -- Not implemented: an effect that causes a creature already on the
          -- battlefield to block, which records no event at all (#1146).
          GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.blocker = blocker, BecameBlocking.attacker = attacker, BecameBlocking.putOntoBattlefield = True, BecameBlocking.attackerWasBlocked = True, BecameBlocking.blockersBefore = prior})
            | attacker == bearer -> admits blocker && not (any admits (Set.toList prior))
          GameEvent.BecameBlocking {} -> False
          GameEvent.BlocksDeclared {} -> False
          GameEvent.AttackerDeclared {} -> False
          GameEvent.Moved {} -> False
          GameEvent.DamageDealt _ -> False
          GameEvent.StepBegan {} -> False
          GameEvent.SpellCast {} -> False
          GameEvent.DamagePrevented {} -> False
          GameEvent.BecameMonarch _ -> False
          GameEvent.TookInitiative _ -> False
          GameEvent.Discarded {} -> False
          GameEvent.Drew {} -> False
          GameEvent.Revealed {} -> False
          GameEvent.SpellCountered _ -> False
          GameEvent.HalfUnlocked {} -> False
          GameEvent.TurnedFaceUp _ -> False
          GameEvent.Transformed {} -> False
          GameEvent.BecameDesignated {} -> False
          GameEvent.Evolved _ -> False
          GameEvent.Mentored {} -> False
          GameEvent.Trained _ -> False
          GameEvent.PermanentSacrificed {} -> False
          GameEvent.AbilityTriggered {} -> False
          GameEvent.LoyaltyAbilityActivated _ -> False
          GameEvent.LifeLost {} -> False
          GameEvent.LifeGained {} -> False
          GameEvent.CountersPut {} -> False
          GameEvent.CountersRemoved {} -> False
          GameEvent.ControlChanged {} -> False
          GameEvent.VentureMarkerEntered {} -> False
          GameEvent.BecameTarget {} -> False
          GameEvent.BecameAttached {} -> False
          GameEvent.LeftTheGame _ -> False
          GameEvent.Milled {} -> False
          GameEvent.Scried _ -> False
          GameEvent.DungeonCompleted _ -> False
          GameEvent.Surveiled _ -> False
          GameEvent.DiceRolled _ -> False
          GameEvent.ClassLevelSet _ -> False
          GameEvent.Plotted _ -> False
          GameEvent.Explored _ -> False
          GameEvent.Exerted _ -> False
          GameEvent.BecameAttacked _ -> False
          GameEvent.AttackersDeclared _ -> False
          GameEvent.BecameTapped _ -> False
          GameEvent.BecameUntapped _ -> False
          GameEvent.TappedForMana _ -> False
          GameEvent.CoinFlipped {} -> False
          GameEvent.RingTempted _ -> False
          GameEvent.CardArrived _ -> False
  -- CR 509.3e read by a BYSTANDER on the attacking side: a creature attacking a
  -- player the PlayerRelation admits became blocked by at least `n` creatures.
  -- The arm above with its Filter traded for a count, and the identity check on
  -- the bearer dropped -- Seifer, Balamb Rival watches everybody's attackers,
  -- including its own controller's, so nothing here compares an id to the bearer.
  --
  -- TWO events, which is rule 509.3e's two producers, and each answers for the
  -- moment it names. GameEvent.AttackerBlocked is the BECOMING: the whole
  -- declaration fires it once, and `>=` and never `==` there is rule 509.3e's
  -- last sentence. GameEvent.BecameBlocking is one arrival joining a creature
  -- that was blocked already, which crosses a floor rather than clearing one, so
  -- the arm on it reads `==`.
  --
  -- Each count comes off its own EVENT and not off Combat.blockers, which by the
  -- time a condition is scanned holds every arrival that came after the one being
  -- answered: CR 509.2a puts these triggers on the stack before any player gets
  -- priority, and CR 614.16 can double one token-making effect into several
  -- arrivals at once. Reading the record instead made a doubled arrival jump the
  -- floor rather than land on it, and made the becoming it rode count the
  -- arrivals that followed it.
  --
  -- Whom the attacker attacked comes from Combat.attackers and only
  -- AttackTarget.OfPlayer answers: CR 508.1b lists player, planeswalker and
  -- battle separately, so a creature sent at an opponent's planeswalker is not
  -- attacking that opponent -- where CR 508.5's defending player, which the event
  -- carries, would resolve to them. SelfAttacksPlayerWithMostLife reads the same
  -- record for the same reason.
  TriggerCondition.CreatureBecomesBlockedByAtLeast (CreatureBecomesBlockedByAtLeast.MkCreatureBecomesBlockedByAtLeast relation n) ->
    let attacksAdmittedPlayer attacker = case Map.lookup attacker (Combat.attackers (GameState.combat gs)) of
          Just (AttackTarget.OfPlayer attacked) -> PlayerRelation.holds (Game.teams gs) relation you attacked
          _ -> False
     in case event of
          GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked attacker _ blockers) -> attacksAdmittedPlayer attacker && blockers >= n
          -- Rule 509.3e's second sentence -- "effects that add or remove
          -- blockers can also cause such abilities to trigger" -- for the one
          -- producer the pool has: a creature PUT ONTO THE BATTLEFIELD
          -- blocking an attacker that was ALREADY blocked, which is Flash
          -- Foliage's Saproling joining a declared blocker. That road records
          -- no GameEvent.AttackerBlocked at all --
          -- Combat.putOntoBattlefieldBlocking withholds it under CR 509.3c's
          -- "only if the attacking creature was an unblocked creature at that
          -- time" -- so the arm above never sees the arrival, and the guard
          -- there is CR 509.3c's own and must not be weakened to reach it.
          --
          -- Removing blockers cannot reach THIS form of the rule: the count
          -- only falls, and a floor is never crossed upwards by a departure.
          --
          -- The flag is load-bearing. Combat.declareBlockers records this same
          -- constructor once per declared PAIR with it clear, so an unguarded
          -- arm would answer an ordinary declaration the arm above has already
          -- answered, once more per blocker.
          --
          -- `+ 1 == n` here where the arm above reads `>= n`, and the two are
          -- not in disagreement. The rule's "at least" is about how many
          -- creatures blocked at one becoming, which is one number the arm above
          -- reads once. This arrival takes the tally from `before` to one more
          -- than that, so it crosses the floor exactly when the floor is that
          -- one more; `>=` would fire again on every further arrival, where the
          -- attacker did not newly become blocked by that many creatures.
          --
          -- Simultaneous arrivals fall out of the same reading rather than
          -- needing a batch: each carries its own `before`, so a doubled pair
          -- taking a block from one to three lands on two with the first of them
          -- and overshoots with the second.
          --
          -- BecameBlocking.attackerWasBlocked in place of the `n >= 2` conjunct
          -- reading the live record needed, and it says the same thing in the
          -- rule's own terms: CR 509.3c withholds GameEvent.AttackerBlocked
          -- exactly when this field is set, so the two arms split every arrival
          -- between them and neither answers one the other did. The becoming an
          -- arrival at an unblocked attacker makes is the arm above's, count and
          -- all -- which is what stops a doubled pair landing on an unblocked
          -- attacker from firing once for the becoming and once for its second
          -- token.
          GameEvent.BecameBlocking (BecameBlocking.MkBecameBlocking {BecameBlocking.attacker = attacker, BecameBlocking.putOntoBattlefield = True, BecameBlocking.attackerWasBlocked = True, BecameBlocking.blockersBefore = before}) ->
            attacksAdmittedPlayer attacker && Natural.length before + 1 == n
          GameEvent.BecameBlocking {} -> False
          GameEvent.AttackerUnblocked _ -> False
          GameEvent.BlocksDeclared {} -> False
          GameEvent.AttackerDeclared {} -> False
          GameEvent.Moved {} -> False
          GameEvent.DamageDealt _ -> False
          GameEvent.StepBegan {} -> False
          GameEvent.SpellCast {} -> False
          GameEvent.DamagePrevented {} -> False
          GameEvent.BecameMonarch _ -> False
          GameEvent.TookInitiative _ -> False
          GameEvent.Discarded {} -> False
          GameEvent.Drew {} -> False
          GameEvent.Revealed {} -> False
          GameEvent.SpellCountered _ -> False
          GameEvent.HalfUnlocked {} -> False
          GameEvent.TurnedFaceUp _ -> False
          GameEvent.Transformed {} -> False
          GameEvent.BecameDesignated {} -> False
          GameEvent.Evolved _ -> False
          GameEvent.Mentored {} -> False
          GameEvent.Trained _ -> False
          GameEvent.PermanentSacrificed {} -> False
          GameEvent.AbilityTriggered {} -> False
          GameEvent.LoyaltyAbilityActivated _ -> False
          GameEvent.LifeLost {} -> False
          GameEvent.LifeGained {} -> False
          GameEvent.CountersPut {} -> False
          GameEvent.CountersRemoved {} -> False
          GameEvent.ControlChanged {} -> False
          GameEvent.VentureMarkerEntered {} -> False
          GameEvent.BecameTarget {} -> False
          GameEvent.BecameAttached {} -> False
          GameEvent.LeftTheGame _ -> False
          GameEvent.Milled {} -> False
          GameEvent.Scried _ -> False
          GameEvent.DungeonCompleted _ -> False
          GameEvent.Surveiled _ -> False
          GameEvent.DiceRolled _ -> False
          GameEvent.ClassLevelSet _ -> False
          GameEvent.Plotted _ -> False
          GameEvent.Explored _ -> False
          GameEvent.Exerted _ -> False
          GameEvent.BecameAttacked _ -> False
          GameEvent.AttackersDeclared _ -> False
          GameEvent.BecameTapped _ -> False
          GameEvent.BecameUntapped _ -> False
          GameEvent.TappedForMana _ -> False
          GameEvent.CoinFlipped {} -> False
          GameEvent.RingTempted _ -> False
          GameEvent.CardArrived _ -> False
  -- CR 509.1h: the bearer became an UNBLOCKED creature, which the glossary's
  -- "attacks and isn't blocked" entry sends here. SelfBecomesBlocked's arm above
  -- is the other branch of the same turn-based action, and no attacker can
  -- produce both events in one declaration.
  --
  -- The event alone answers it. Reading Combat.blockers instead would answer for
  -- the board at scan time, and rule 509.1h's last sentence is what that gets
  -- wrong: an attacker whose blockers all left combat is still blocked.
  TriggerCondition.SelfAttacksUnblocked -> case event of
    GameEvent.AttackerUnblocked oid -> oid == bearer
    -- CR 509.1i's declaration events are the blocked side of the same action; an
    -- attacker this one names got a blocker, so it is not unblocked.
    GameEvent.AttackerDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 603.6: a zone-change trigger matched on BOTH ends of the move, library to
  -- graveyard. The bearer is the incarnation the card became on arrival per CR
  -- 400.7e, a graveyard being public (CR 400.2). The pair is also what makes CR
  -- 113.6k put this ability in the graveyard rather than on the battlefield.
  --
  -- `from` is the half that does the work: the same card discarded out of a hand
  -- or dying off the battlefield reaches the same graveyard and must not trigger.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> case event of
    GameEvent.Moved (Moved.MkMoved zc _ _) ->
      ZoneChange.object zc == bearer
        && ZoneChange.from zc == Zone.Library
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 603.6 with NO origin zone: the destination is the whole condition, so a
  -- discard, a mill, a countered spell and a death all match. `from` is
  -- deliberately unread, which is the one line separating this from the two
  -- conditions on either side of it.
  --
  -- Matched on `object`, the arriving incarnation, and NOT on `departed`: CR
  -- 603.6c's last sentence takes this out of the leaves-the-battlefield family,
  -- so CR 603.10a's look-back does not reach it and CR 603.10's normal reading --
  -- the objects that exist immediately after the event -- applies. That is also
  -- what makes the graveyard the one zone the scan has to find the bearer in,
  -- however far away the card started.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> case event of
    GameEvent.Moved (Moved.MkMoved zc _ _) ->
      ZoneChange.object zc == bearer
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    -- Not implemented: CR 712.21's second card IS put into the graveyard, so a
    -- component printing this condition should fire for it too; no meld pair in
    -- data/cards/ prints one (#3106).
    GameEvent.CardArrived _ -> False
  -- CR 603.6 read by a BYSTANDER: "whenever another card is put into a graveyard
  -- from anywhere" (Planar Void). The destination is the whole of the zone test,
  -- as it is for SelfPutIntoGraveyardFromAnywhere above; what differs is who is
  -- watching, so the bearer frames the match rather than being it -- it is the
  -- Filter.Context's source (so `Not IsSource` is "another") and its controller
  -- is CR 109.5's "you".
  --
  -- TWO EVENTS, which is CR 712.21's own Example: the Moved event names the
  -- first card put into the graveyard and a CardArrived event names each card
  -- after it, so a melded permanent's death fires this condition twice while
  -- SelfDies and PermanentDies, which read the Moved event alone, fire once.
  --
  -- Matched on `object`, the ARRIVING card, and filtered over it: CR 603.6c's
  -- last sentence keeps this out of the leaves-the-battlefield family, so CR
  -- 603.10a's look-back does not reach it and CR 603.10's normal first sentence
  -- governs -- the objects that exist immediately after the event. That is why
  -- the filter reads the card in the graveyard rather than CR 608.2h last known
  -- information, PermanentDies' opposite choice.
  --
  -- viewWithLastKnown aimed at the arriving card twice over, PermanentDies'
  -- posture: the live graveyard card for the ordinary case, and CR 608.2h for
  -- the card that has left the graveyard again by the CR 117.5 boundary.
  --
  -- The bearer's OWN departure is excluded, and by the same sentence of CR
  -- 603.10: a permanent put into a graveyard does not exist on the battlefield
  -- immediately after the event that put it there, so it is not among the
  -- objects checked. eventTriggers offers it all the same -- `leftBattlefield`
  -- is CR 603.10a's look-back, unrestricted by this condition's own exclusion --
  -- and `Not IsSource` cannot do the excluding, CR 400.7 having minted the
  -- graveyard card a fresh id that the bearer's own id never equals.
  TriggerCondition.CardPutIntoGraveyard f ->
    let admits zc =
          ZoneChange.to zc == Zone.Graveyard
            && ZoneChange.departed zc /= bearer
            && let arrived = ZoneChange.object zc
                in case Projection.viewWithLastKnown arrived gs arrived of
                     Nothing -> False
                     Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
     in case event of
          GameEvent.Moved (Moved.MkMoved zc _ _) -> admits zc
          GameEvent.CardArrived zc -> admits zc
          GameEvent.DamageDealt _ -> False
          GameEvent.StepBegan {} -> False
          GameEvent.SpellCast {} -> False
          GameEvent.DamagePrevented {} -> False
          GameEvent.BecameMonarch _ -> False
          GameEvent.TookInitiative _ -> False
          GameEvent.Discarded {} -> False
          GameEvent.Drew {} -> False
          GameEvent.Revealed {} -> False
          GameEvent.AttackerDeclared {} -> False
          GameEvent.BecameBlocking {} -> False
          GameEvent.BlocksDeclared {} -> False
          GameEvent.AttackerBlocked {} -> False
          GameEvent.AttackerUnblocked _ -> False
          GameEvent.SpellCountered _ -> False
          GameEvent.HalfUnlocked {} -> False
          GameEvent.TurnedFaceUp _ -> False
          GameEvent.Transformed {} -> False
          GameEvent.BecameDesignated {} -> False
          GameEvent.Evolved _ -> False
          GameEvent.Mentored {} -> False
          GameEvent.Trained _ -> False
          GameEvent.PermanentSacrificed {} -> False
          GameEvent.AbilityTriggered {} -> False
          GameEvent.LoyaltyAbilityActivated _ -> False
          GameEvent.LifeLost {} -> False
          GameEvent.LifeGained {} -> False
          GameEvent.CountersPut {} -> False
          GameEvent.CountersRemoved {} -> False
          GameEvent.ControlChanged {} -> False
          GameEvent.VentureMarkerEntered {} -> False
          GameEvent.BecameTarget {} -> False
          GameEvent.BecameAttached {} -> False
          GameEvent.LeftTheGame _ -> False
          GameEvent.Milled {} -> False
          GameEvent.Scried _ -> False
          GameEvent.DungeonCompleted _ -> False
          GameEvent.Surveiled _ -> False
          GameEvent.DiceRolled _ -> False
          GameEvent.ClassLevelSet _ -> False
          GameEvent.Plotted _ -> False
          GameEvent.Explored _ -> False
          GameEvent.Exerted _ -> False
          GameEvent.BecameAttacked _ -> False
          GameEvent.AttackersDeclared _ -> False
          GameEvent.BecameTapped _ -> False
          GameEvent.BecameUntapped _ -> False
          GameEvent.TappedForMana _ -> False
          GameEvent.CoinFlipped {} -> False
          GameEvent.RingTempted _ -> False
  -- CR 603.6c narrowed by CR 700.4's definition of "dies": the bearer was put into
  -- a graveyard from the battlefield. Both ends are load-bearing -- `from` keeps a
  -- permanent DISCARDED out of a hand silent, and `to` keeps one EXILED off the
  -- battlefield silent, the latter having left the battlefield without dying.
  -- SelfLeavesTheBattlefield below is the other condition, and naming this one
  -- after the printed word is what keeps them apart.
  --
  -- Matched on `departed`, NOT `object`: CR 603.10a makes leaves-the-battlefield
  -- abilities look back in time, so the bearer offered here is the permanent as it
  -- was immediately before the event, never the CR 400.7 incarnation.
  TriggerCondition.SelfDies -> case event of
    GameEvent.Moved (Moved.MkMoved zc _ _) ->
      ZoneChange.departed zc == bearer
        && ZoneChange.from zc == Zone.Battlefield
        && ZoneChange.to zc == Zone.Graveyard
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- The same rule and zone pair as SelfDies, watched by a BYSTANDER. The bearer
  -- frames the match rather than being it, as for PermanentEnters: it is the
  -- Filter.Context's source (so `Not IsSource` is "another"), and its controller
  -- is CR 109.5's "you".
  --
  -- Matched on `departed`, with characteristics from CR 608.2h last known
  -- information rather than a live read -- both CR 603.10a's look-back. The
  -- PermanentEnters arm makes the opposite choice, rightly, since CR 603.6b puts an
  -- entrant's continuous effects on the moment it is on the battlefield. A dead
  -- permanent has no live reading left, and reading the graveyard card instead
  -- would answer "you control" wrong rather than not at all: CR 108.4a substitutes
  -- the OWNER, so a stolen creature would be credited back to the player who no
  -- longer had it when it died.
  --
  -- viewWithLastKnown aimed at the deceased twice over, which is how it is asked
  -- for the snapshot: it takes the last-known branch only for the id it is
  -- anchored to, and only once that id is gone.
  --
  -- Nothing is a permanent that is gone AND filed no last known information, about
  -- which no Filter can honestly answer.
  TriggerCondition.PermanentDies f -> case event of
    GameEvent.Moved (Moved.MkMoved zc _ _)
      | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard ->
          let deceased = ZoneChange.departed zc
           in case Projection.viewWithLastKnown deceased gs deceased of
                Nothing -> False
                Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 603.2c's batch reading of the arm above (Vengeful Townsfolk's "whenever ONE
  -- OR MORE other creatures you control die"). Delegated rather than duplicated
  -- because the per-EVENT question is the same one: which deaths this condition
  -- admits is PermanentDies' answer, filter, look-back and all.
  --
  -- What makes it fire ONCE for a whole sweep is not here. This matcher's contract
  -- is that it sees one event at a time, so it cannot count occurrences of a CR
  -- 704.3 / CR 608.2f batch; `batchScoped` below marks the condition and
  -- eventTriggers keeps the first pending trigger per (bearer, ability) within each
  -- Pawl.Types.EventGroup. So this arm answering True for every member of the batch
  -- is deliberate, not a missing dedup: the arm and the dedup are two halves of one
  -- rule, and matchesTrigger alone is not the whole of it.
  TriggerCondition.PermanentsDie f -> matchesTriggerGiven bindings gs bearer you (TriggerCondition.PermanentDies f) event
  -- CR 603.2c's batch reading of PermanentDealsCombatDamageToPlayer (Pia Nalaar,
  -- Chief Mechanic's "whenever ONE OR MORE artifact creatures you control deal
  -- combat damage to a player"), delegated for PermanentsDie's reason: which
  -- damage events this condition admits is the singular arm's answer, filter,
  -- kind and recipient alike, and firing once for the CR 510.2 step is
  -- `batchScoped` below plus eventTriggers' dedup, never this arm.
  TriggerCondition.PermanentsDealCombatDamageToPlayer f -> matchesTriggerGiven bindings gs bearer you (TriggerCondition.PermanentDealsCombatDamageToPlayer f) event
  -- CR 700.4's "dies" once more, asked of the permanent the bearer is attached
  -- to: PermanentDies' battlefield-to-graveyard pair, matched on
  -- ZoneChange.departed for that arm's reason (CR 603.10a).
  --
  -- CR 603.10a's look-back, and the ONE reading of the link: the deceased's own
  -- CR 608.2h record of what was attached to it, filed by the zone-change funnel
  -- from the pre-move board. Neither reading off the BEARER survives to the CR
  -- 117.5 boundary -- CR 704.5m buries an Aura in the same SBA batch, and CR
  -- 704.5n clears an Equipment's Object.attachedTo in it -- so the host is where
  -- the attachment as of the event still exists, for both bearer shapes at once.
  -- Pawl.Types.LastKnown.attached is the field; CR 303.4m is what makes an
  -- Equipment's "equipped creature" the same link an Aura's "enchanted creature"
  -- is.
  --
  -- No characteristic of the deceased is read, unlike PermanentDies -- the
  -- attachment link already says which permanent this is about, so there is
  -- nothing more for last known information to answer.
  --
  -- Empty for a departure the funnel never filed, which no event this arm admits
  -- can be: a battlefield-to-graveyard move is the funnel's own.
  TriggerCondition.AttachedCreatureDies -> case event of
    GameEvent.Moved (Moved.MkMoved zc _ _)
      | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard ->
          maybe False (Set.member bearer . LastKnown.attached) (Map.lookup (ZoneChange.departed zc) (GameState.lastKnown gs))
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 701.26a's tap, asked of the permanent the bearer is attached to
  -- (Betrayal's "whenever enchanted creature becomes tapped"). The event names
  -- whichever permanent turned sideways; Object.attachedTo says whether that is
  -- this bearer's host.
  --
  -- CR 603.2e's exclusions are discharged where the event is MINTED, in `tap`
  -- above, rather than here: a permanent that entered tapped never transitioned,
  -- and rule 701.26a's "only untapped permanents can be tapped" makes a repeat tap
  -- no event at all. So this arm asks only whose tap it was.
  --
  -- The host is read through Recipient.objectOf, AttachedCreatureMentors' route
  -- for CR 303.4's other destination: an Aura enchanting a PLAYER has no host id
  -- to compare and answers False, as does one attached to nothing.
  --
  -- LIVE and off the BEARER, where AttachedCreatureDies reads CR 608.2h's record
  -- of the deceased instead. That look-back is load-bearing there because the
  -- same SBA batch that kills the host takes the link away (CR 704.5m, CR
  -- 704.5n); here the host is still standing -- it has just become tapped -- so
  -- the link is on the board to be read.
  TriggerCondition.AttachedCreatureBecomesTapped -> case event of
    GameEvent.BecameTapped tapped ->
      let hostOfBearer = Object.attachedTo =<< Game.lookupObject bearer gs
       in (Recipient.objectOf =<< hostOfBearer) == Just tapped
    -- CR 701.26b's untap is the other transition of the same status, and no
    -- "becomes tapped" condition reads it.
    GameEvent.BecameUntapped _ -> False
    -- CR 106.12a's event is a DIFFERENT one, and stated rather than folded
    -- into the arm above: a mana activation writes both, and matching this
    -- one here would fire Betrayal twice off one tap.
    GameEvent.TappedForMana _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 701.26b through CR 603.2: the bearer is the permanent that rotated
  -- upright (Oreskos Sun Guide). SelfTurnedFaceUp's shape -- a bare comparison
  -- of ids, with none of the permanent's characteristics read, so no CR 608.2h
  -- fallback is reachable.
  --
  -- CR 603.2e's exclusions are discharged where the event is MINTED, in
  -- Pawl.Engine.Event.untap and Pawl.Engine.Engine.untapAll, rather than here: a
  -- permanent that entered untapped never transitioned, and rule 701.26b's
  -- "only tapped permanents can be untapped" makes a repeat untap no event at
  -- all. So this arm asks only whose untap it was.
  TriggerCondition.SelfBecomesUntapped -> case event of
    GameEvent.BecameUntapped untapped -> untapped == bearer
    -- CR 701.26a's tap is the other direction of the same status and a
    -- different event; nothing folds the two together.
    GameEvent.BecameTapped _ -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 106.12a's "is tapped for mana" read off the same attachment link the
  -- arm above watches, and its whole difference from that one: this fires only
  -- where CR 106.12's activation produced mana, so Icy Manipulator's tap of the
  -- enchanted land misses it and Wild Growth adds nothing.
  --
  -- LIVE and off the BEARER, the arm above's reason: nothing has moved, so the
  -- attachment link is on the board to be read.
  TriggerCondition.AttachedPermanentTappedForMana -> case event of
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana tapped ->
      let hostOfBearer = Object.attachedTo =<< Game.lookupObject bearer gs
       in (Recipient.objectOf =<< hostOfBearer) == Just tapped
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 603.6c taken whole. The `from` half matches SelfDies'; the `to` half is
  -- where they part company, this one asking only that the destination be ANOTHER
  -- zone.
  --
  -- The `to /= Battlefield` guard is that rule's own word "another", and is
  -- load-bearing: recordMintedEntry files a battlefield-to-battlefield pseudo-move
  -- whose `departed` is the arrival's own id, so a permanent minted straight onto
  -- the battlefield -- a token, a melded permanent, a conjured card -- would fire
  -- this condition on its own creation without it.
  --
  -- Matched on `departed` for SelfDies' reason (CR 603.10a).
  --
  -- CR 603.6c's OTHER trigger event is the second arm: a phased-in permanent
  -- leaving the game because its owner left it (CR 800.4a). No zone pair to
  -- check there -- the permanent was on the battlefield or the event would not
  -- have been recorded, and CR 702.26k's exclusion of a phased-out one is
  -- applied where the event is emitted, in Pawl.Engine.Departure.
  --
  -- SelfDies deliberately does NOT take the same arm: CR 700.4 makes "dies" a
  -- move to a graveyard, and leaving the game reaches no zone at all.
  TriggerCondition.SelfLeavesTheBattlefield -> case event of
    GameEvent.Moved (Moved.MkMoved zc _ _) ->
      ZoneChange.departed zc == bearer
        && ZoneChange.from zc == Zone.Battlefield
        && ZoneChange.to zc /= Zone.Battlefield
    GameEvent.LeftTheGame oid -> oid == bearer
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
  -- The SAME two trigger events read by a BYSTANDER: PermanentDies' Filter over
  -- CR 608.2h last known information, asked of the arm above's wider destination
  -- test and of its CR 800.4a leaving-the-game form alike.
  --
  -- viewWithLastKnown aimed at the departed id twice over, PermanentDies'
  -- posture and its reasons: CR 603.10a's look-back is what makes "you control"
  -- answerable about a permanent that is a card in a graveyard -- or in a hand,
  -- or nowhere at all -- by the time the scan runs. A permanent that filed no
  -- last known information is one no Filter can honestly answer about, so it
  -- matches nothing.
  TriggerCondition.PermanentLeavesTheBattlefield f ->
    let admits departed = case Projection.viewWithLastKnown departed gs departed of
          Nothing -> False
          Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
     in case event of
          GameEvent.Moved (Moved.MkMoved zc _ _)
            | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc /= Zone.Battlefield ->
                admits (ZoneChange.departed zc)
          GameEvent.Moved {} -> False
          GameEvent.LeftTheGame oid -> admits oid
          GameEvent.Milled {} -> False
          GameEvent.Scried _ -> False
          GameEvent.DungeonCompleted _ -> False
          GameEvent.Surveiled _ -> False
          GameEvent.DiceRolled _ -> False
          GameEvent.ClassLevelSet _ -> False
          GameEvent.Plotted _ -> False
          GameEvent.Explored _ -> False
          GameEvent.Exerted _ -> False
          GameEvent.BecameAttacked _ -> False
          GameEvent.AttackersDeclared _ -> False
          GameEvent.BecameTapped _ -> False
          GameEvent.BecameUntapped _ -> False
          GameEvent.TappedForMana _ -> False
          GameEvent.CoinFlipped {} -> False
          GameEvent.RingTempted _ -> False
          GameEvent.CardArrived _ -> False
          GameEvent.DamageDealt _ -> False
          GameEvent.StepBegan {} -> False
          GameEvent.SpellCast {} -> False
          GameEvent.DamagePrevented {} -> False
          GameEvent.BecameMonarch _ -> False
          GameEvent.TookInitiative _ -> False
          GameEvent.Discarded {} -> False
          GameEvent.Drew {} -> False
          GameEvent.Revealed {} -> False
          GameEvent.AttackerDeclared {} -> False
          GameEvent.BecameBlocking {} -> False
          GameEvent.BlocksDeclared {} -> False
          GameEvent.AttackerBlocked {} -> False
          GameEvent.AttackerUnblocked _ -> False
          GameEvent.SpellCountered _ -> False
          GameEvent.HalfUnlocked {} -> False
          GameEvent.TurnedFaceUp _ -> False
          GameEvent.Transformed {} -> False
          GameEvent.BecameDesignated {} -> False
          GameEvent.Evolved _ -> False
          GameEvent.Mentored {} -> False
          GameEvent.Trained _ -> False
          GameEvent.PermanentSacrificed {} -> False
          GameEvent.AbilityTriggered {} -> False
          GameEvent.LoyaltyAbilityActivated _ -> False
          GameEvent.LifeLost {} -> False
          GameEvent.LifeGained {} -> False
          GameEvent.CountersPut {} -> False
          GameEvent.CountersRemoved {} -> False
          GameEvent.ControlChanged {} -> False
          GameEvent.VentureMarkerEntered {} -> False
          GameEvent.BecameTarget {} -> False
          GameEvent.BecameAttached {} -> False
  -- CR 603.2c's batch reading of PermanentReturnedToHand (Tameshi, Reality
  -- Architect's "whenever ONE OR MORE noncreature permanents are returned to
  -- hand"), delegated for PermanentsDie's reason: which moves this condition
  -- admits is the arm below's answer, and firing once for the batch is
  -- `batchScoped` below plus eventTriggers' dedup, never this arm.
  TriggerCondition.PermanentsReturnedToHand f -> matchesTriggerGiven bindings gs bearer you (TriggerCondition.PermanentReturnedToHand f) event
  -- CR 603.6c's family with the DESTINATION pinned: PermanentLeavesTheBattlefield's
  -- match above, narrowed to the one zone this condition names. CR 110.1 is what makes the
  -- origin implicit -- a permanent is on the battlefield, so "returned to hand"
  -- is battlefield-to-hand and admits no other pair.
  --
  -- The same viewWithLastKnown read on ZoneChange.departed, and CR 603.10a
  -- cites it twice over here: this is a leaves-the-battlefield ability AND an
  -- ability that triggers when an object all players can see is put into a
  -- hand. Without the look-back "you control" would be asked of a card in a
  -- hand, which CR 108.4 gives no controller.
  --
  -- CR 603.6c's leaving-the-game form is declined, unlike PermanentLeavesTheBattlefield's
  -- arm: a permanent that leaves the game reaches no zone at all, so it is not
  -- returned to anyone's hand.
  TriggerCondition.PermanentReturnedToHand f ->
    let admits departed = case Projection.viewWithLastKnown departed gs departed of
          Nothing -> False
          Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
     in case event of
          GameEvent.Moved (Moved.MkMoved zc _ _)
            | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Hand ->
                admits (ZoneChange.departed zc)
          GameEvent.Moved {} -> False
          GameEvent.LeftTheGame _ -> False
          GameEvent.Milled {} -> False
          GameEvent.Scried _ -> False
          GameEvent.DungeonCompleted _ -> False
          GameEvent.Surveiled _ -> False
          GameEvent.DiceRolled _ -> False
          GameEvent.ClassLevelSet _ -> False
          GameEvent.Plotted _ -> False
          GameEvent.Explored _ -> False
          GameEvent.Exerted _ -> False
          GameEvent.BecameAttacked _ -> False
          GameEvent.AttackersDeclared _ -> False
          GameEvent.BecameTapped _ -> False
          GameEvent.BecameUntapped _ -> False
          GameEvent.TappedForMana _ -> False
          GameEvent.CoinFlipped {} -> False
          GameEvent.RingTempted _ -> False
          GameEvent.CardArrived _ -> False
          GameEvent.DamageDealt _ -> False
          GameEvent.StepBegan {} -> False
          GameEvent.SpellCast {} -> False
          GameEvent.DamagePrevented {} -> False
          GameEvent.BecameMonarch _ -> False
          GameEvent.TookInitiative _ -> False
          GameEvent.Discarded {} -> False
          GameEvent.Drew {} -> False
          GameEvent.Revealed {} -> False
          GameEvent.AttackerDeclared {} -> False
          GameEvent.BecameBlocking {} -> False
          GameEvent.BlocksDeclared {} -> False
          GameEvent.AttackerBlocked {} -> False
          GameEvent.AttackerUnblocked _ -> False
          GameEvent.SpellCountered _ -> False
          GameEvent.HalfUnlocked {} -> False
          GameEvent.TurnedFaceUp _ -> False
          GameEvent.Transformed {} -> False
          GameEvent.BecameDesignated {} -> False
          GameEvent.Evolved _ -> False
          GameEvent.Mentored {} -> False
          GameEvent.Trained _ -> False
          GameEvent.PermanentSacrificed {} -> False
          GameEvent.AbilityTriggered {} -> False
          GameEvent.LoyaltyAbilityActivated _ -> False
          GameEvent.LifeLost {} -> False
          GameEvent.LifeGained {} -> False
          GameEvent.CountersPut {} -> False
          GameEvent.CountersRemoved {} -> False
          GameEvent.ControlChanged {} -> False
          GameEvent.VentureMarkerEntered {} -> False
          GameEvent.BecameTarget {} -> False
          GameEvent.BecameAttached {} -> False
  -- CR 702.55b/702.55c: SelfDies' zone pair, asked of the object the BEARER
  -- HAUNTS rather than of the bearer itself -- so the id compared against
  -- ZoneChange.departed is the one GameState.haunting files the bearer under, and
  -- a bearer that haunts nothing matches nothing.
  --
  -- Matched on `departed` for PermanentDies' reason (CR 603.10a): the haunt
  -- ability targeted the permanent as it was on the battlefield, and that is the
  -- id the link was written with.
  --
  -- NO characteristic of the deceased is read, where PermanentDies reads a whole
  -- Filter: rule 702.55b keeps "the creature it haunts" pointing at the object
  -- targeted "regardless of whether or not that object is still a creature", so a
  -- creature that was turned into a Treasure and then destroyed still fires this.
  TriggerCondition.HauntedCreatureDies -> case event of
    GameEvent.Moved (Moved.MkMoved zc _ _) ->
      ZoneChange.from zc == Zone.Battlefield
        && ZoneChange.to zc == Zone.Graveyard
        && Map.lookup bearer (GameState.haunting gs) == Just (ZoneChange.departed zc)
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 603.10a's third look-back family: PermanentReturnedToHand's match with the
  -- zone pinned on the DEPARTURE side instead -- the origin is a graveyard and the
  -- destination is wherever the card went. CR 400.3 files a card in its owner's
  -- graveyard, so "your graveyard" is an OwnedBy conjunct of the Filter rather
  -- than a zone this arm could name (Pawl.Types.Zone names no player).
  --
  -- The same viewWithLastKnown read on ZoneChange.departed, and CR 603.10a is
  -- what makes it the only possible read: the card is no longer in the graveyard
  -- by the time CR 603.10 checks, so a live lookup of the id that left answers
  -- nothing at all.
  --
  -- The TurnScope comes from the GAME STATE, the SpellCast arm's reason: no
  -- characteristic of the departing card says whose turn it is, and CR 109.5 with
  -- CR 603.3a fixes "you" as the ability's controller.
  --
  -- GameEvent.LeftTheGame is declined, the PermanentReturnedToHand arm's reason
  -- one zone over: CR 800.4a's departure reaches no zone, so no zone change names
  -- a graveyard it came out of.
  TriggerCondition.CardLeavesGraveyard (CardLeavesGraveyard.MkCardLeavesGraveyard f scope) ->
    let admits departed = case Projection.viewWithLastKnown departed gs departed of
          Nothing -> False
          Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
     in case event of
          GameEvent.Moved (Moved.MkMoved zc _ _)
            | ZoneChange.from zc == Zone.Graveyard ->
                turnScopeAdmits (Game.teams gs) scope (GameState.activePlayer gs) you
                  && admits (ZoneChange.departed zc)
          GameEvent.Moved {} -> False
          GameEvent.LeftTheGame _ -> False
          GameEvent.Milled {} -> False
          GameEvent.Scried _ -> False
          GameEvent.DungeonCompleted _ -> False
          GameEvent.Surveiled _ -> False
          GameEvent.DiceRolled _ -> False
          GameEvent.ClassLevelSet _ -> False
          GameEvent.Plotted _ -> False
          GameEvent.Explored _ -> False
          GameEvent.Exerted _ -> False
          GameEvent.BecameAttacked _ -> False
          GameEvent.AttackersDeclared _ -> False
          GameEvent.BecameTapped _ -> False
          GameEvent.BecameUntapped _ -> False
          GameEvent.TappedForMana _ -> False
          GameEvent.CoinFlipped {} -> False
          GameEvent.RingTempted _ -> False
          GameEvent.CardArrived _ -> False
          GameEvent.DamageDealt _ -> False
          GameEvent.StepBegan {} -> False
          GameEvent.SpellCast {} -> False
          GameEvent.DamagePrevented {} -> False
          GameEvent.BecameMonarch _ -> False
          GameEvent.TookInitiative _ -> False
          GameEvent.Discarded {} -> False
          GameEvent.Drew {} -> False
          GameEvent.Revealed {} -> False
          GameEvent.AttackerDeclared {} -> False
          GameEvent.BecameBlocking {} -> False
          GameEvent.BlocksDeclared {} -> False
          GameEvent.AttackerBlocked {} -> False
          GameEvent.AttackerUnblocked _ -> False
          GameEvent.SpellCountered _ -> False
          GameEvent.HalfUnlocked {} -> False
          GameEvent.TurnedFaceUp _ -> False
          GameEvent.Transformed {} -> False
          GameEvent.BecameDesignated {} -> False
          GameEvent.Evolved _ -> False
          GameEvent.Mentored {} -> False
          GameEvent.Trained _ -> False
          GameEvent.PermanentSacrificed {} -> False
          GameEvent.AbilityTriggered {} -> False
          GameEvent.LoyaltyAbilityActivated _ -> False
          GameEvent.LifeLost {} -> False
          GameEvent.LifeGained {} -> False
          GameEvent.CountersPut {} -> False
          GameEvent.CountersRemoved {} -> False
          GameEvent.ControlChanged {} -> False
          GameEvent.VentureMarkerEntered {} -> False
          GameEvent.BecameTarget {} -> False
          GameEvent.BecameAttached {} -> False
  -- CR 701.6a: a spell was countered, by a spell or ability whose controller the
  -- relation admits. The countering source's controller comes from the event,
  -- captured as the counter happened, and CR 109.5/603.3a fix "you" as the
  -- ability's controller.
  --
  -- The bearer is NOT part of the match -- the PlayerDiscards posture rather than
  -- any Self- condition's -- since the bearer is a permanent and the countering is
  -- done by a spell somewhere else.
  --
  -- Neither "can't be countered" gate needs a clause here -- CR 113.6g's on the
  -- spell, CR 613.11's on a permanent's static ability: a spell that can't be
  -- countered is not countered at all (CR 101.2), so `counter` records nothing
  -- and there is no event for this arm to see.
  TriggerCondition.SpellOrAbilityCounters relation -> case event of
    GameEvent.SpellCountered c -> PlayerRelation.holds (Game.teams gs) relation you (Countering.controller c)
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 615.13: a prevention effect was applied and prevented some damage, and the
  -- damage it prevented was addressed to a player the relation admits. CR 109.5 /
  -- 603.3a fix "you" as the ability's controller, exactly as PlayerDiscards and
  -- SpellOrAbilityCounters do.
  --
  -- The bearer is NOT part of the match: Selfless Squire is a creature watching
  -- damage addressed to its controller, and CR 615.13 says nothing about which
  -- object the ability is on.
  --
  -- ONE fire per recorded event, and the record is already grouped per prevention
  -- effect per batch (Replacement.groupPreventions), which is where CR 615.13's
  -- "one or more simultaneous damage events" is honoured. Nothing here has to
  -- count.
  --
  -- Damage prevented to a PERMANENT is silence rather than a miss: the printed
  -- sentence says "to you", and the recipients the event carries are what
  -- distinguishes the two. One record can hold BOTH, a shield covering a player
  -- and their permanents being one application (Divine Deflection), so the match
  -- is over the map's player entries rather than over a single recipient.
  --
  -- ABOVE 0, which is CR 615.12's case one recipient at a time: an application
  -- that was inert against the damage aimed at this player prevented none of it,
  -- however much of the same batch it stopped elsewhere, and rule 615.13 fires
  -- only where some was prevented.
  TriggerCondition.DamageToPlayerPrevented relation -> case event of
    GameEvent.DamagePrevented prevented ->
      let admits (recipient, amount) =
            amount > 0 && case recipient of
              Recipient.ToPlayer pid -> PlayerRelation.holds (Game.teams gs) relation you pid
              Recipient.ToCreature _ -> False
              Recipient.ToPlaneswalker _ -> False
              Recipient.ToBattle _ -> False
              Recipient.ToObject _ -> False
              -- Unreachable: CR 406.4's pile is a candidate at CR 601.2c and
              -- never something damage is dealt to.
              Recipient.ToPile _ -> False
       in any admits (Map.toList (DamagePrevented.amounts prevented))
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 615.13's other reading: the damage was prevented THIS WAY -- by a
  -- prevention effect the BEARER's own card prints (Phyrexian Vindicator).
  -- Replacement.printedBy is that question, and it plus the Filter below is the
  -- whole of the match: rule 615.13 says nothing about whom the damage was
  -- addressed to, and neither does either printed sentence.
  --
  -- The identity, never the recipient: the Vindicator's shield covers only
  -- itself, so a recipient test would be a second name for a fact the identity
  -- already settles -- and one prevention effect of another object covering the
  -- SAME recipient is exactly the case this arm has to answer False for.
  --
  -- The Filter is over CR 120.1's SOURCE of the damage that did not happen --
  -- Samite Ministration's "damage from a black or red source" -- and the bearer
  -- contributes only CR 109.5's "you" and the Filter.Context's source, exactly as
  -- in the PermanentDealsCombatDamageToPlayer arm above. viewWithLastKnown for
  -- that arm's reason: CR 608.2h's record is what still answers "was it black"
  -- for a source that died to the very batch this prevented. A fence rather than
  -- a tested branch, as it is there -- Samite Ministration's shield covers a
  -- PLAYER, so nothing that reaches this arm kills the damage's source.
  --
  -- Filter.IsSource is meaningless in THIS position where it is meaningful
  -- there: the context's source is the bearer, and the bearer is the object whose
  -- prevention effect applied, never the object that would have dealt the damage.
  TriggerCondition.SelfPreventsDamage f -> case event of
    GameEvent.DamagePrevented prevented ->
      Replacement.printedBy (DamagePrevented.by prevented) == Just bearer
        && ( let damager = DamagePrevented.source prevented
              in case Projection.viewWithLastKnown damager gs damager of
                   Nothing -> False
                   Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
           )
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- Its batch sibling delegates to it, PermanentsGetCounters' posture below:
  -- which gains the condition admits is the same question either way, and what
  -- separates the two is `batchScoped` plus eventTriggers' dedup, never this
  -- matcher.
  TriggerCondition.PlayersGainLife relation -> matchesTriggerGiven bindings gs bearer you (TriggerCondition.PlayerGainsLife relation) event
  -- CR 119.9: a source caused a player the relation admits to gain life. The
  -- gaining player comes from the event; CR 109.5 / 603.3a fix "you" as the
  -- ability's controller, exactly as PlayerDiscards, SpellOrAbilityCounters and
  -- DamageToPlayerPrevented above do.
  --
  -- The bearer is NOT part of the match: Ajani's Pridemate is a creature watching
  -- its controller's life total, and CR 119.9 says nothing about which object the
  -- ability is on.
  --
  -- No zero check here. CR 119.9's "if a player gains 0 life, no life gain event
  -- has occurred" is enforced where the event is RECORDED -- Resolve's GainLife
  -- arm and Damage's lifelink pass both guard their own zero -- so a
  -- GameEvent.LifeGained in the log is by construction a gain of more than 0, and
  -- a second guard here would be a second place for that invariant to live.
  --
  -- LOSING life is not a near miss but a different event, and the LifeLost arm
  -- below is where that shows: one damage event can record both, and only the
  -- gain fires this.
  TriggerCondition.PlayerGainsLife relation -> case event of
    GameEvent.LifeGained (LifeChange.MkLifeChange pid _) -> PlayerRelation.holds (Game.teams gs) relation you pid
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- A player the relation admits LOST life -- Exquisite Blood's "whenever an
  -- opponent loses life". The losing player comes from the event; CR 109.5 /
  -- 603.3a fix "you" as the ability's controller, exactly as PlayerGainsLife
  -- above does.
  --
  -- The bearer is NOT part of the match: Exquisite Blood is an enchantment
  -- watching somebody else's life total, and nothing about the condition names
  -- the object the ability is on.
  --
  -- Which life-total movements are a loss is settled at the RECORDING sites and
  -- not here, since the rules print no CR 119.9 for this direction: CR 119.3's
  -- instructed loss, CR 119.2 / 120.3a's damage, and CR 119.4's paid life all
  -- write GameEvent.LifeLost, while CR 120.3b's infect diversion, CR 615.6's
  -- prevented damage and damage taken by a permanent write none. See
  -- Pawl.Types.TriggerCondition.PlayerLosesLife.
  --
  -- No zero check either, for the reason the gain arm gives: every producer
  -- guards its own zero, so a GameEvent.LifeLost in the log is by construction a
  -- loss of more than 0.
  --
  -- GAINING life is a different event, not a signed version of this one: one
  -- lifelink source's damage can record a loss and a gain together, and only the
  -- loss fires this.
  TriggerCondition.PlayerLosesLife relation -> case event of
    GameEvent.LifeLost (LifeChange.MkLifeChange pid _) -> PlayerRelation.holds (Game.teams gs) relation you pid
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 714.2b: counters of this kind were put onto the BEARER, and the count
  -- crossed N going up. Both halves of the rule's sentence are here -- see
  -- Pawl.Types.TriggerCondition.SelfCountersReached for why the intervening "if"
  -- is not split off into TriggeredAbility.intervening.
  --
  -- `before < n` is what stops a chapter re-firing: History of Benalia going from
  -- two lore counters to three crosses III and nothing else, and a later counter
  -- taking it from three to four crosses none of its chapters again.
  --
  -- `n <= after` rather than `n == after`, because one placement can cross several
  -- thresholds at once (CR 714.2b says "at least N", not "exactly N"): a Saga
  -- given two lore counters while it has none fires chapters I and II together.
  -- Read ahead (CR 702.155a) is the mechanic that wants the equality instead, and
  -- then only on the turn the Saga entered -- Saga.chapterTriggers, which asks
  -- Saga.readAheadRestricted of the bearer.
  --
  -- The narrowing does NOT leak past Sagas even though this arm is generic over
  -- counter kind: readAheadRestricted gates on Keyword.ReadAhead, which rule
  -- 702.155a puts only on Saga cards, so the extra conjunct is inert for every
  -- other counter this condition watches. A CounterKind.Lore test here would
  -- restate that gate rather than add to it.
  --
  -- Projected LAZILY, and Saga.chapterTriggers asks `after == n` first, so the
  -- whole-board projection is forced only where the two readings could differ.
  TriggerCondition.SelfCountersReached (SelfCountersReached.MkSelfCountersReached wanted n) -> case event of
    GameEvent.CountersPut (CounterChange.MkCounterChange oid kind before after) ->
      oid == bearer && kind == wanted && Saga.chapterTriggers (Saga.readAheadRestricted (Projection.project bearer gs) gs bearer) before after n
    -- Rule 714.2b says "are PUT onto", so a removal crosses nothing: a Saga whose
    -- lore counters were taken off and put back fires its chapters again.
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 716.2a: the BEARER's class level crossed N going up. "When this Class
  -- becomes level N" is printed inside the level-N section, so CR 716.2a's static
  -- half is what grants it, and CR 603.10 is what lets it see its own arrival:
  -- "becomes level N" is not on that rule's exhaustive look-back list, so the
  -- abilities checked are the ones existing immediately AFTER the level changed.
  --
  -- `before < n` is what stops a later bar re-firing an earlier section's
  -- trigger: a Class going from level 2 to level 3 crosses 3 and nothing else.
  --
  -- `n <= after` rather than `n == after`, SelfCountersReached's reading of the
  -- same shape: one write can cross several thresholds, so a Class set straight
  -- from level 1 to level 3 fires both sections' triggers. No level bar can do
  -- that (CR 716.2a's "only if this Class is level N-1"), which is why the two
  -- readings coincide for every printing in `data/cards/` -- Effect.SetClassLevel
  -- is an opcode, not a bar, and this is the reading the rule states.
  --
  -- Saga.crossed says the same sentence for lore counters and is deliberately not
  -- reused: it is over Naturals, and its haddock ties it to CR 714.2b and to the
  -- agreement CR 704.5s's state-based action needs with it. A class level is a
  -- ClassLevel and has no state-based action to agree with.
  TriggerCondition.SelfBecomesClassLevel n -> case event of
    GameEvent.ClassLevelSet (ClassLevelChange.MkClassLevelChange oid before after) -> oid == bearer && before < n && n <= after
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 310.12b: the LAST counter of this kind came off the BEARER. The mirror of
  -- the arm above, and narrower in the way rule 310.12b is narrower than rule
  -- 714.2b: there is no threshold to cross, only a count that reached none.
  --
  -- `after == 0` alone, with no `before > 0` conjunct: GameEvent.CountersRemoved is
  -- recorded only where something actually came off, so an event that removed
  -- nothing is not in the log to match. That invariant is the record's, stated on
  -- the constructor, exactly as CountersPut's "before < after" is.
  TriggerCondition.SelfLastCounterRemoved wanted -> maybe False ((==) 0 . snd) (countersRemovedFrom bearer wanted event)
  -- "Whenever one or more [kind] counters are removed from this permanent"
  -- (Chandra, Fire Artisan): the arm above's any-amount mirror, dropping every
  -- read of the AFTER count. Three of four loyalty counters coming off matches
  -- here and not there, which is what keeps the two from collapsing.
  --
  -- No "one or more" conjunct either, and for the arm above's reason: the record
  -- exists only where something came off.
  TriggerCondition.SelfCountersRemoved wanted -> Maybe.isJust (countersRemovedFrom bearer wanted event)
  -- CR 603.2c's PER-PERMANENT placement (Wickersmith's Tools' "whenever one or
  -- more -1/-1 counters are put on A CREATURE"): counters of this kind landed on
  -- a permanent the Filter admits. One event at a time, so a batch that touched
  -- three creatures matches here three times and fires three triggers -- the
  -- rule's second sentence and its own Example.
  --
  -- The Filter is read against the PERMANENT that took the counters, not against
  -- the batch: there is no batch at this level, and CR 122.6's placement names
  -- one object.
  --
  -- No "one or more" conjunct: GameEvent.CountersPut is recorded only where the
  -- before/after pair grew, an invariant stated on the constructor, so a placement
  -- of nothing is not in the log to match. Nor is one needed for the other half of
  -- that phrase -- settleCounters records ONE event per settled placement, so a
  -- creature given two counters at once is one match and one trigger.
  --
  -- viewWithLastKnown rather than viewOfObject, PermanentTurnedFaceUp's posture: a
  -- permanent that took counters and was gone before the CR 117.5 boundary is
  -- still read as it stood (CR 608.2h) instead of dropping out of the match --
  -- which is exactly the CR 704.5f victim a -1/-1 counter made.
  --
  -- The bearer frames the match rather than being it: it is the Filter.Context's
  -- source, and its controller is the perspective CR 109.5 gives "you".
  --
  -- Its batch sibling delegates HERE, PermanentsDie's posture above: which
  -- placements the condition admits is the same question either way, and what
  -- separates the two is `batchScoped` below plus eventTriggers' dedup, never
  -- this matcher.
  TriggerCondition.PermanentsGetCounters p -> matchesTriggerGiven bindings gs bearer you (TriggerCondition.PermanentGetsCounters p) event
  TriggerCondition.PermanentGetsCounters (CounterPlacement.MkCounterPlacement wanted f) -> case event of
    GameEvent.CountersPut (CounterChange.MkCounterChange oid kind _ _)
      | kind == wanted -> case Projection.viewWithLastKnown oid gs oid of
          Nothing -> False
          Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 601.2i's "any abilities that trigger when a spell is cast": a spell the
  -- Filter admits became cast. The bearer frames the match rather than being it,
  -- as for PermanentEnters -- it is the Filter.Context's source, and its
  -- controller is the perspective CR 109.5 gives "you" in "whenever YOU CAST an
  -- instant or sorcery spell".
  --
  -- The CASTER comes from the event and is handed to the view as the spell's
  -- controller, which is what makes Filter.ControlledBy You answer the printed
  -- "you". Not read off the object: Event.changeZone stamps Object.enteredUnder
  -- only for a battlefield entry, so a stack object falls back to its OWNER
  -- (Projection.defaultControllerOf) -- the same player for every cast in the
  -- pool today, and the wrong one the moment a card lets somebody cast a card
  -- they do not own. CR 601.2a settles it the other way: the player casting the
  -- spell is its controller.
  --
  -- The spell is read LIVE off the stack rather than from a snapshot, which is
  -- what separates this arm from PermanentDies'. CR 601.2i's trigger event is
  -- the spell BECOMING cast, and CR 601.2a leaves it on the stack "until it
  -- resolves, it's countered, or a rule or effect moves it elsewhere" -- none of
  -- which can have happened before the scan, since the cast is the last thing
  -- Cast.castSpell does. So no CR 608.2h fallback is reachable, and the `Nothing`
  -- below is the id naming nothing at all, about which no Filter can honestly
  -- answer.
  --
  -- The TurnScope is the second half, and it comes from the GAME STATE rather
  -- than from the event: GameEvent.SpellCast records the caster and the spell,
  -- never the turn, and CR 601.2i's trigger is checked in the same settle the
  -- cast happened in -- so the active player standing now is the one the cast
  -- happened under. Read against `you`, CR 109.5's controller of the ability (CR
  -- 603.3a), exactly as the StepBegins arm above reads its own.
  TriggerCondition.SpellCast (SpellCast.MkSpellCast f scope fromZone ordinal) -> case event of
    GameEvent.SpellCast (SpellWasCast.MkSpellWasCast caster spell _ castFrom) -> case Game.lookupObject spell gs of
      Nothing -> False
      Just _ ->
        turnScopeAdmits (Game.teams gs) scope (GameState.activePlayer gs) you
          -- CR 601.2a's zone, read off the EVENT and not off the spell: rule
          -- 400.7 left the stack incarnation with no memory of it. A condition
          -- that names no zone admits every cast, which is what almost every
          -- printing writes.
          && maybe True (\z -> castFrom == Just z) fromZone
          && Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) (Projection.viewOfSpell caster spell gs) f
          -- Clarion Spirit's "your SECOND spell each turn", asked LAST so the
          -- log walk happens only for a cast the rest of the condition already
          -- admits.
          && maybe True (castOrdinal (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) f fromZone spell gs ==) ordinal
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 601.2i, self-scoped: the spell that became cast IS the bearer. A bare
  -- comparison of ids and no Filter at all, which is what separates this arm
  -- from SpellCast's above -- nothing about the spell is read, so no projection
  -- can come up empty and no CR 608.2h fallback is reachable.
  --
  -- No TurnScope either, so no turnScopeAdmits: CR 601.2i says nothing about
  -- whose turn it is, and no printing narrows its own cast by one.
  --
  -- The bearer is the STACK object, which is the same id GameEvent.SpellCast
  -- carries: CR 601.2a puts the card on the stack as it is cast and leaves it
  -- there, so eventTriggers' `spellCast` source offers exactly that incarnation.
  TriggerCondition.SelfCast -> case event of
    GameEvent.SpellCast (SpellWasCast.MkSpellWasCast _ spell _ _) -> spell == bearer
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 601.2c, self-scoped: the object that became a target IS the bearer, a bare
  -- comparison of ids in SelfCast's shape and for its reason -- nothing about the
  -- targeting spell is read, so no projection can come up empty.
  --
  -- The RELATION is read off the event's own controller field rather than off the
  -- board, which is what CR 405.4 asks for: the answer wanted is who controlled
  -- the targeting object as it was announced, and by the time a ward trigger is
  -- gathered that object may already have changed hands or left.
  --
  -- Rule 702.21a's own relation is Opponent; You is admitted because the
  -- condition is stated over a PlayerRelation, and a card printing the other half
  -- would read the same field.
  TriggerCondition.SelfBecomesTargeted relation -> case event of
    GameEvent.BecameTarget t ->
      Recipient.objectOf (BecameTarget.targeted t) == Just bearer
        && PlayerRelation.holds (Game.teams gs) relation you (BecameTarget.controller t)
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.SpellCast {} -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
  -- CR 601.2c from the PLAYER's side, the sibling above one recipient over:
  -- Dormant Gomazoa's "whenever you become the target of a spell" and Amulet of
  -- Safekeeping's "whenever you become the target of a spell or ability an
  -- opponent controls". The recipient is compared with CR 109.5's "you" -- CR
  -- 603.3a's controller of the bearer as the ability triggered -- rather than
  -- with the bearer, which is the whole of what separates this from ward.
  --
  -- The KIND conjunct is the arm's rules content, and it is a Maybe: CR 112.1
  -- makes a spell a card on the stack, while CR 602.2b and CR 603.3d route an
  -- activated and a triggered ability through the same rule 601.2c. Gomazoa names
  -- Spell and Ravenous Rats' targeted discard must not untap it; Amulet names
  -- neither limb and takes both.
  --
  -- The RELATION is ward's, read off the same BecameTarget.controller field --
  -- CR 405.4's controller of the targeting object, which is who Amulet offers the
  -- {1} to. Gomazoa's AnyPlayer fires on its own spells as readily as on an
  -- opponent's.
  TriggerCondition.ControllerBecomesTarget c -> case event of
    GameEvent.BecameTarget t ->
      Recipient.playerOf (BecameTarget.targeted t) == Just you
        && maybe True (== BecameTarget.kind t) (ControllerBecomesTarget.kind c)
        && PlayerRelation.holds (Game.teams gs) (ControllerBecomesTarget.relation c) you (BecameTarget.controller t)
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.SpellCast {} -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
  -- CR 709.5h: the bearer is the permanent that was given the designation, and
  -- the door named is the one it was given for. A bare comparison of an id and a
  -- name, in SelfEnters' shape and for its reason -- nothing about the entrant's
  -- characteristics is read, so there is no CR 608.2h fallback to reach for.
  --
  -- The half is checked as well as the bearer, which is the arm's whole content:
  -- CR 709.5h fires "when a player unlocks a PARTICULAR half", so a Room whose
  -- other door was the one that opened must not fire this ability.
  TriggerCondition.SelfHalfUnlocked half -> case event of
    GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked oid _ name _) -> oid == bearer && name == half
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 708.7 through CR 603.2: the bearer is the permanent that turned over.
  -- SelfEnters' shape -- a bare comparison of ids, with nothing about the
  -- permanent's characteristics read, so no CR 608.2h fallback is reachable.
  --
  -- The BEARER check is what keeps one player's face-up Skirk Marauder from
  -- firing off a different permanent turning over; Pawl.FaceDownSpec seats a
  -- second face-down permanent on the same board to prove it.
  --
  -- Nothing here asks whether the permanent had ALREADY entered the battlefield.
  -- CR 708.7 leaves turning face up something only a permanent can be doing, and
  -- FaceDown.performTurnFaceUp is the sole writer of this event -- CR 708.3's
  -- face-down ENTRY writes a Moved event and never this one, which is what makes
  -- CR 708.8's last sentence fall out rather than needing a clause.
  TriggerCondition.SelfTurnedFaceUp -> case event of
    GameEvent.TurnedFaceUp oid -> oid == bearer
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 701.27e through CR 603.2: the bearer is the permanent that transformed,
  -- and the names the event carries are what it turned INTO. SelfTurnedFaceUp
  -- above is the neighbouring shape, plus the name -- and CR 701.27b is why
  -- they are two conditions rather than one, turning over and turning face up
  -- being different game actions.
  --
  -- Both halves are read off the EVENT, neither off the board. The board at the
  -- CR 117.5 scan is not the board CR 701.27e asks about: a permanent that
  -- transformed twice before the scan shows only its last face, which would
  -- answer this arm wrong on both events. Pawl.Types.Transformed carries the
  -- sample for that reason.
  --
  -- Set.member rather than equality because CR 709.4a admits several names and
  -- CR 708.2a admits none; a permanent with no name matches nothing, which is
  -- the rule's own answer rather than a guard.
  TriggerCondition.SelfTransformedInto name -> case event of
    GameEvent.Transformed (Transformed.MkTransformed oid pc) -> oid == bearer && Set.member name (PC.names pc)
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 701.27e's OTHER written form, read by a bystander: "whenever a permanent
  -- you control transforms into a non-Human creature" (Cult of the Waxing Moon).
  -- PermanentTurnedFaceUp's shape against SelfTurnedFaceUp's, one rule along --
  -- the bearer frames the match rather than being it, contributing CR 109.5's
  -- perspective and the Filter.Context's source and nothing else.
  --
  -- The CHARACTERISTICS come off the event and everything else off the board,
  -- which is what Count.overlaySnapshot is for: CR 701.27e pins the specified
  -- characteristic to the instant the turn finished, and a wholly live read
  -- would answer wrong for a permanent that turned twice before the CR 117.5
  -- scan, which is the argument Pawl.Types.Transformed makes for carrying the
  -- sample.
  --
  -- Not implemented: the same at-event read for the axes a
  -- ProjectedCharacteristics cannot carry, which CR 109.3 excludes from an
  -- object's characteristics but CR 603.10 and CR 603.2 pin all the same -- a
  -- trigger's condition is checked against the objects as they were immediately
  -- after the event. Control and ATTACHMENT are the two, and attachment is the
  -- one a printing reaches: Neglected Heirloom's "when equipped creature
  -- transforms" is `HasAttached IsSource`, and on a board where the equipped
  -- creature turns into a noncreature the CR 704.5n unattach runs at the same CR
  -- 117.5 boundary, BEFORE triggers go on the stack, so the live read here finds
  -- nothing attached and the ability never fires (#2050).
  --
  -- viewWithLastKnown rather than viewOfObject, PermanentTurnedFaceUp's reason:
  -- a permanent that turned over and left before the CR 117.5 boundary is still
  -- read as it was on the battlefield (CR 608.2h).
  TriggerCondition.PermanentTransforms f -> case event of
    GameEvent.Transformed (Transformed.MkTransformed oid pc) -> case Projection.viewWithLastKnown oid gs oid of
      Nothing -> False
      Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) (Count.overlaySnapshot pc view) f
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 708.7's OTHER written form, read by a bystander: "whenever a permanent is
  -- turned face up". PermanentEnters' shape against SelfEnters', and for its
  -- reason -- the bearer frames the match rather than being it. The bearer is the
  -- Filter.Context's source (so `Not IsSource` would be an "another"), and its
  -- controller is the perspective CR 109.5 gives "you" in Deathmist Raptor's "a
  -- permanent YOU CONTROL is turned face up".
  --
  -- NO comparison against the bearer at all, which is the whole difference from
  -- the arm above: Aven Farseer bears no morph ability and so can never be the
  -- permanent that turned over, and a bearer check here would make its ability
  -- dead text. Pawl.FaceDownSpec's "a permanent turning face up puts Aven
  -- Farseer's counter on the FARSEER" is what proves the scope, asserting the
  -- counter's landing place by object id on a board where the watcher and the
  -- subject are two permanents.
  --
  -- Read LIVE off the game as it stands, like PermanentEnters and unlike
  -- PermanentDies: CR 708.8 leaves the permanent on the battlefield with its
  -- normal copiable values restored, so there is nothing for CR 603.10a's
  -- look-back to recover and the live read is what CR 603.10's first sentence
  -- asks for. It is also the only read that can answer a narrowed form correctly
  -- -- CR 708.2 gives a face-down permanent only the characteristics its listing
  -- names and never the card's, so a Filter applied to the pre-turning object
  -- would decline every "a Dragon is turned face up" there is.
  --
  -- viewWithLastKnown rather than viewOfObject for PermanentEnters' reason: a
  -- permanent turned face up and gone again before the CR 117.5 boundary is still
  -- read as it was on the battlefield (CR 608.2h) instead of vanishing from the
  -- match. Nothing is a permanent that is gone AND filed no last known
  -- information, about which no Filter can honestly answer.
  TriggerCondition.PermanentTurnedFaceUp f -> case event of
    GameEvent.TurnedFaceUp oid -> case Projection.viewWithLastKnown oid gs oid of
      Nothing -> False
      Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 702.112b: a permanent the Filter admits was given the renowned designation.
  -- PermanentTurnedFaceUp's arm above, line for line, and for its reasons: the
  -- permanent is read as it stands (viewWithLastKnown for CR 608.2h, a designation
  -- being no zone change), and the bearer contributes only CR 109.5's perspective
  -- and the Filter.Context's source -- which is what makes Filter.IsSource the
  -- self-scoped reading.
  -- CR 702.100b: the BEARER evolved. SelfEnters' arm -- a bare id comparison, no
  -- view and no Filter -- which is what makes it answerable for a creature that
  -- has since left: the marker is about an event, not about the object now.
  TriggerCondition.SelfEvolves -> case event of
    GameEvent.Evolved oid -> oid == bearer
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 702.134c: the creature the BEARER IS ATTACHED TO mentored another. The
  -- event's first id is rule 702.134c's "first creature", the mentor, and this arm
  -- asks only whether that is the bearer's host -- the pairing with the second
  -- creature is the event's, decided where the mentor ability resolved.
  --
  -- CR 301.5a's "equipped creature" read off Object.attachedTo, which is where the
  -- Equipment records it, through Recipient.objectOf for CR 303.4's other
  -- destination: an Equipment attached to nothing, or an Aura enchanting a player,
  -- has no host id to compare and answers False.
  --
  -- A LIVE read, PermanentTurnedFaceUp's posture: nothing here is a zone change, so
  -- CR 603.10a's look-back does not reach this condition, and the attachment as it
  -- stands when the trigger is gathered is CR 603.2's own reading of "equipped
  -- creature".
  TriggerCondition.AttachedCreatureMentors -> case event of
    GameEvent.Mentored (Mentored.MkMentored mentor _) ->
      (Recipient.objectOf =<< Object.attachedTo =<< Game.lookupObject bearer gs) == Just mentor
    GameEvent.Evolved _ -> False
    GameEvent.Trained _ -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 702.149c: the BEARER trained. SelfEvolves' arm above, line for line and for
  -- its reasons -- a bare id comparison, no view and no Filter, so a creature that
  -- has since left the battlefield is still answered about the event.
  TriggerCondition.SelfTrains -> case event of
    GameEvent.Trained oid -> oid == bearer
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    -- The event the RULE distinguishes this condition from: +1/+1 counters arriving
    -- say nothing about what put them, which is why rule 702.149c needs a marker at
    -- all.
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    -- Nor the declaration rule 702.149a's own trigger reads: training FIRES on an
    -- attack and this condition fires on that ability resolving, so an ability
    -- removed before it resolves (CR 608.2b) trains nothing.
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated wanted f) -> case event of
    -- The designations must MATCH, not merely both be present: Valeron Wardens'
    -- renown trigger must not fire when a creature you control becomes monstrous.
    GameEvent.BecameDesignated (BecameDesignated.MkBecameDesignated got oid)
      | got /= wanted -> False
      | otherwise -> case Projection.viewWithLastKnown oid gs oid of
          Nothing -> False
          Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 709.5i: "such an ability triggers when that permanent has one of the two
  -- unlocked designations and gets the other, or when it has neither designation
  -- and gains both." The whole of that sentence is the flag the event carries,
  -- decided where the designation was written; this arm reads it and asks who
  -- controls the permanent.
  --
  -- NOT scoped to the bearer, which is where this parts company with
  -- SelfHalfUnlocked above: Balemurk Leech is a creature watching every Room on
  -- the board, so the bearer contributes only CR 109.5's perspective through
  -- `you`.
  --
  -- The relation is resolved against the player who UNLOCKED, which is rule
  -- 709.5i's own subject ("when a player 'fully unlocks' a permanent") read
  -- through CR 109.5's "you". The Room's CONTROLLER is the other reading and is
  -- rejected: the printed sentence puts "you" in the subject position and leaves
  -- "a Room" unqualified, so it is the act and not the permanent that "you"
  -- selects. The two readings agree on every printing, every printed unlock
  -- naming a Room its own controller controls; the actor is carried on the event
  -- because by the time this runs the board may have moved on.
  TriggerCondition.RoomFullyUnlocked relation -> case event of
    GameEvent.HalfUnlocked (HalfUnlocked.MkHalfUnlocked _ actor _ fully) ->
      fully && PlayerRelation.holds (Game.teams gs) relation you actor
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 603.10a's sacrifice family: the log entry Event.sacrifice writes is the
  -- whole of the answer, which is exactly what makes the condition worth having.
  -- CR 700.4 makes every sacrifice a death, so the Moved event the same sacrifice
  -- records is indistinguishable from a destruction's or a mill's -- an arm that
  -- read the zone change instead would fire on both.
  --
  -- BOTH halves of the printed sentence are compared, against the two fields the
  -- event carries. CR 701.21a's sacrificing player is related to CR 109.5's `you`
  -- -- Vengeful Tracker's "an opponent" -- and the permanent is put to the Filter.
  -- Mayhem Devil's unrestricted wording spells itself out as AnyPlayer over the
  -- trivial Filter rather than as an absent payload.
  --
  -- viewWithLastKnown aimed at the sacrificed permanent twice over, PermanentDies'
  -- posture and CR 603.10a's own look-back: this event is recorded BEFORE the
  -- move, so by the time the trigger is gathered the permanent has left the
  -- battlefield, and a TOKEN has ceased to exist outright -- CR 111.7's
  -- parenthetical says the ability triggers anyway, which a live read could not
  -- honour. A Treasure sacrificed for mana is the case.
  --
  -- Nothing is a permanent that is gone AND filed no last known information, about
  -- which no Filter can honestly answer.
  TriggerCondition.PermanentSacrificed (PermanentSacrificed.MkPermanentSacrificed relation f) -> case event of
    GameEvent.PermanentSacrificed ev
      | PlayerRelation.holds (Game.teams gs) relation you (PermanentWasSacrificed.player ev) ->
          let victim = PermanentWasSacrificed.permanent ev
           in case Projection.viewWithLastKnown victim gs victim of
                Nothing -> False
                Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    -- CR 700.4 again, from this side: a sacrifice DOES record a Moved event, and
    -- matching it here would answer twice for one sacrifice.
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 603.1b: "a triggered ability may have more than one trigger condition".
  -- `any`, because the printed sentence joins them with "and whenever": each
  -- clause is its own occasion for the ability to trigger. Two clauses matching
  -- the SAME event fire the ability once here, which is also the right answer --
  -- CR 603.2 matches an event against "a triggered ability's trigger event", and
  -- the ability is one ability.
  TriggerCondition.AnyOf conditions -> any (\c -> matchesTriggerGiven bindings gs bearer you c event) conditions
  -- CR 603.3b's second class, the one condition in this type whose event is
  -- another ability triggering: "whenever the final chapter ability of a Saga you
  -- control triggers".
  --
  -- The event carries the triggered ability's SOURCE, its CONTROLLER as it
  -- triggered (CR 603.3a) and the ABILITY, and all three are read:
  --
  --   * the ability must be a chapter ability (CR 714.2b, through Saga.chapterOf,
  --     so this and the SelfCountersReached arm above cannot drift about what a
  --     chapter symbol is);
  --   * its chapter must be the source's FINAL chapter number (CR 714.2d), which
  --     is why the source's projection is read rather than the event alone;
  --   * the source must be a Saga with one or more chapter abilities
  --     (Saga.tracksLore, CR 714.1 / 704.5s's own phrase). CR 714.2d gives a
  --     permanent with no chapter abilities a final chapter number of 0, so
  --     without that conjunct a "{r0}" chapter ability on any permanent at all
  --     would match -- no card prints one, and the gate is still the one the
  --     rule states.
  --
  -- NOT self-scoped: Historian's Boon is an enchantment watching somebody else's
  -- permanent, so the bearer contributes only CR 109.5's perspective through
  -- `you`, which the PlayerRelation reads the event's controller against.
  --
  -- The source is read LIVE (Projection.project), which needs no CR 608.2h
  -- fallback and is not a shortcut: CR 704.5s's exemption keeps a Saga on the
  -- battlefield for exactly as long as a chapter ability of its own has triggered
  -- and not yet left the stack, so the Saga whose final chapter fired this event
  -- is still standing at the CR 117.5 boundary that scans for it. A Saga a
  -- replacement or another player's effect took away in the same batch projects
  -- as an object with no subtypes, which Saga.tracksLore declines -- the same
  -- silence CR 603.10 would give a look-back that found nothing (#1028).
  TriggerCondition.SagaFinalChapterTriggers relation -> case event of
    -- A SOURCELESS inherent ability (CR 725.2, CR 702.179d) is never a chapter
    -- ability of a Saga, there being no Saga behind it to read lore counters off.
    GameEvent.AbilityTriggered record -> case AbilityTriggered.source record of
      TriggerSource.Sourceless -> False
      TriggerSource.OfObject srcId ->
        PlayerRelation.holds (Game.teams gs) relation you (AbilityTriggered.controller record)
          && ( let pc = Projection.project srcId gs
                in Saga.tracksLore pc && Saga.chapterOf (AbilityTriggered.ability record) == Just (Saga.finalChapterOf pc)
             )
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 603.7: "when you lose control of the creature" -- Ray of Command's third
  -- sentence. The slot is read out of CR 603.7c's captured environment, so the
  -- creature this asks about is the one the spell targeted rather than whatever
  -- creature happens to change hands.
  --
  -- Binding.objectSlots and not Binding.objectsOf: the printed sentence is about
  -- ONE creature, and a slot holding a group or several targets names none of them
  -- here rather than any one of them.
  --
  -- `before == you` and nothing about `after`, because losing control is the whole
  -- of what the sentence asks. Engine.sampleControl only mints the event when the
  -- two players differ, so no separate "and somebody else has it now" conjunct is
  -- needed for the match to mean a change.
  --
  -- The `== Just oid` conjunct is a REGRESSION FENCE rather than a proven
  -- behaviour: no board in the pool can tell it from `Map.member slot`. The only
  -- control changes a Ray of Command board sees are the reversions of the creatures
  -- its controller stole, all in one CR 514.2 sweep, and the ability taps its slot
  -- whichever of them matched -- CR 603.7b spending the one shot either way. A card
  -- whose controller can lose control of a DIFFERENT permanent while this entry is
  -- armed is what would observe it.
  TriggerCondition.LoseControlOfBound slot -> case event of
    GameEvent.ControlChanged (ControlChanged.MkControlChanged oid before _) ->
      Map.lookup slot (Binding.objectSlots bindings) == Just oid && before == you
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 309.4c: "when you move your venture marker into THIS room", so the
  -- dungeon card the marker is on must be the bearer and the room must be this
  -- ability's own. Not reached from here: eventTriggers' command-zone source is
  -- CR 114.4's and takes emblems alone, so a dungeon card is never offered and
  -- Pawl.Engine.Dungeon.roomPending is what gathers a room ability. A regression
  -- fence, written to agree with that gatherer rather than to differ. The two
  -- cannot disagree on any dungeon card: CR 309.4c gives every room ability the
  -- same trigger condition, which the rulebook supplies and the card does not
  -- print, so only the effect varies and neither collector reads it.
  TriggerCondition.RoomEntered room -> case event of
    GameEvent.VentureMarkerEntered (VentureMarkerEntered.MkVentureMarkerEntered _ oid entered) -> oid == bearer && entered == room
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.StepBegan {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
  -- CR 701.22d: this player scried. The relation reads the scryer against CR
  -- 109.5's "you", the ability's controller (CR 603.3a) --
  -- PlayerBecomesMonarch's shape, and Matoya, Archon Elder is the You form.
  --
  -- The EVENT and nothing else: how many cards moved, and whether any could,
  -- is CR 701.22a's business and CR 701.22d says explicitly that neither
  -- narrows this. Pawl.Engine.Resolve.Effect.scryOne records the event outside its
  -- own prompt guard for that sentence.
  TriggerCondition.PlayerScries relation -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried scryer -> PlayerRelation.holds (Game.teams gs) relation you scryer
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 701.54d: "whenever the Ring tempts you" (Nazgul). PlayerScries' shape
  -- above, and Nazgul is the You form.
  --
  -- The EVENT and nothing else: rule 701.54d says a player is tempted whenever
  -- they complete CR 701.54a's actions "even if some or all of those actions
  -- were impossible", so a temptation that designated nobody matches this.
  -- Pawl.Engine.Ring.tempt records the event outside the branch that
  -- designates, for that sentence.
  TriggerCondition.RingTemptsPlayer relation -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted tempted -> PlayerRelation.holds (Game.teams gs) relation you tempted
    GameEvent.CardArrived _ -> False
  -- CR 309.7: this player completed a dungeon. The relation reads the
  -- completing player against CR 109.5's "you", the ability's controller (CR
  -- 603.3a) -- PlayerScries' shape above, and Dungeon Crawler is the You form.
  --
  -- CR 309.6 makes the completing player the dungeon card's OWNER, which is
  -- what Pawl.Engine.Dungeon.remove records; a controller reading is not
  -- available to differ from it, since a dungeon card has no controller.
  TriggerCondition.PlayerCompletesDungeon relation -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted completer -> PlayerRelation.holds (Game.teams gs) relation you completer
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 701.25d, the arm above's twin and Matoya, Archon Elder's other branch.
  -- A surveil that put nothing into a graveyard matches, which is what a
  -- condition built on CR 701.25a's zone changes could not do.
  TriggerCondition.PlayerSurveils relation -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled surveiller -> PlayerRelation.holds (Game.teams gs) relation you surveiller
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 706.1: this player rolled a die, the relation reading the roller against
  -- CR 109.5's "you" as PlayerScries above does. Feywild Trickster is the You
  -- form.
  --
  -- The EVENT alone. What the die SHOWED is deliberately not a bar here: CR
  -- 706.7 has the planar die firing this very condition while every effect
  -- reading a numerical result ignores it, so a condition gated on the result
  -- would be the wrong shape rather than a stricter one (#934).
  --
  -- Not implemented: CR 706.6's ignored roll, which "is considered to have never
  -- happened" and triggers nothing -- nothing in data/cards ignores or rerolls a
  -- roll (#2083), so no recorded event is one this must skip.
  TriggerCondition.PlayerRollsDice relation -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled roller -> PlayerRelation.holds (Game.teams gs) relation you roller
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 705.2: this player WON a coin flip, the relation reading the flipper
  -- against CR 109.5's "you" as PlayerRollsDice above does. Tavern Scoundrel is
  -- the You form.
  --
  -- The OUTCOME is a bar here, where the roll above deliberately has none: CR
  -- 705.2 names the win itself, so a lost flip is a recorded event this must not
  -- match. CR 705.2's last sentence is why one seat answers -- "no other players
  -- are involved" -- so the losing side of a won flip is nobody.
  TriggerCondition.PlayerWinsCoinFlip relation -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    -- A flip with NO outcome (CR 705.2's first sentence) is not a won one, so
    -- Nothing answers False exactly as Just False does.
    GameEvent.CoinFlipped flipped -> CoinFlipped.won flipped == Just True && PlayerRelation.holds (Game.teams gs) relation you (CoinFlipped.flipper flipped)
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 702.170a / 702.170c: the bearer's own card became plotted. Self-scoped, so the
  -- match is the id and nothing else -- and the id the event carries is the
  -- CR 400.7 incarnation in exile, which is the bearer here because
  -- Event.eventTriggers finds this ability through its exile scan rather than
  -- on the battlefield (zonesTriggeredFrom below).
  TriggerCondition.SelfBecomesPlotted -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted plotted -> plotted == bearer
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 701.44b: a permanent the Filter admits completed an explore.
  -- Wildgrowth Walker's "a creature you control" describes the EXPLORER, so
  -- the bearer only frames the match: it is the Filter.Context's source and
  -- its controller is CR 109.5's "you".
  --
  -- viewWithLastKnown aimed at the explorer twice over, PermanentDies' posture
  -- and CR 701.44c's instruction in as many words: a permanent that explored
  -- and has since left is read as it last was, so "you control" answers with
  -- the player who controlled it rather than CR 108.4a's owner substitute.
  --
  -- Nothing is an explorer that is gone AND filed no last known information,
  -- about which no Filter can honestly answer.
  TriggerCondition.PermanentExplores f -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored explorer -> case Projection.viewWithLastKnown explorer gs explorer of
      Nothing -> False
      Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 701.43d / 607.2h: the BEARER was exerted. SelfEvolves' arm above, line
  -- for line: CR 701.43a records the event only for the permanent actually
  -- exerted, so WHOSE exert it was is the whole question, and CR 607.2h's
  -- linkage needs nothing more because Pawl.Engine.Combat.declareAttackers
  -- records the event only where the static ability offered the cost.
  TriggerCondition.SelfExerted -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached {} -> False
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted oid -> oid == bearer
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False
  -- CR 701.3a read from the HOST: something became attached to the BEARER, and
  -- the Filter narrows WHAT. Two questions, and the split is the condition's
  -- shape -- a bare id comparison for the host (SelfEnters' arm) and a Filter
  -- read for the attachment (PermanentTurnedFaceUp's arm), which is why the
  -- constructor carries one payload and matches on two objects.
  --
  -- Recipient.objectOf and not equality on the whole Recipient: CR 701.3a's
  -- destination is tagged by the attaching permanent's own rules text
  -- (Pawl.Engine.Attach.attachmentFor), so an Aura arrives as a ToCreature and
  -- an Equipment as a ToCreature while nothing promises the bearer would be
  -- named the same way twice. A ToPlayer host has no object and declines here,
  -- which is right: the bearer of this condition is a permanent.
  --
  -- viewWithLastKnown for PermanentTurnedFaceUp's reason -- an attachment that
  -- is gone by the CR 117.5 boundary is still read as it was (CR 608.2h)
  -- instead of vanishing from the match.
  TriggerCondition.SelfBecomesAttachedBy f -> case event of
    GameEvent.Moved {} -> False
    GameEvent.DamageDealt _ -> False
    GameEvent.StepBegan {} -> False
    GameEvent.SpellCast {} -> False
    GameEvent.DamagePrevented {} -> False
    GameEvent.BecameMonarch _ -> False
    GameEvent.TookInitiative _ -> False
    GameEvent.Discarded {} -> False
    GameEvent.Drew {} -> False
    GameEvent.Revealed {} -> False
    GameEvent.AttackerDeclared {} -> False
    GameEvent.BecameBlocking {} -> False
    GameEvent.BlocksDeclared {} -> False
    GameEvent.AttackerBlocked {} -> False
    GameEvent.AttackerUnblocked _ -> False
    GameEvent.SpellCountered _ -> False
    GameEvent.HalfUnlocked {} -> False
    GameEvent.TurnedFaceUp _ -> False
    GameEvent.Transformed {} -> False
    GameEvent.BecameDesignated {} -> False
    GameEvent.Evolved _ -> False
    GameEvent.Mentored {} -> False
    GameEvent.Trained _ -> False
    GameEvent.PermanentSacrificed {} -> False
    GameEvent.AbilityTriggered {} -> False
    GameEvent.LoyaltyAbilityActivated _ -> False
    GameEvent.LifeLost {} -> False
    GameEvent.LifeGained {} -> False
    GameEvent.CountersPut {} -> False
    GameEvent.CountersRemoved {} -> False
    GameEvent.ControlChanged {} -> False
    GameEvent.VentureMarkerEntered {} -> False
    GameEvent.BecameTarget {} -> False
    GameEvent.BecameAttached a ->
      Recipient.objectOf (BecameAttached.host a) == Just bearer
        && ( case Projection.viewWithLastKnown (BecameAttached.attachment a) gs (BecameAttached.attachment a) of
               Nothing -> False
               Just view -> Filter.matches (Filter.contextFor (Game.teams gs) (Just you) (Just bearer)) view f
           )
    GameEvent.LeftTheGame _ -> False
    GameEvent.Milled {} -> False
    GameEvent.Scried _ -> False
    GameEvent.DungeonCompleted _ -> False
    GameEvent.Surveiled _ -> False
    GameEvent.DiceRolled _ -> False
    GameEvent.ClassLevelSet _ -> False
    GameEvent.Plotted _ -> False
    GameEvent.Explored _ -> False
    GameEvent.Exerted _ -> False
    GameEvent.BecameAttacked _ -> False
    GameEvent.AttackersDeclared _ -> False
    GameEvent.BecameTapped _ -> False
    GameEvent.BecameUntapped _ -> False
    GameEvent.TappedForMana _ -> False
    GameEvent.CoinFlipped {} -> False
    GameEvent.RingTempted _ -> False
    GameEvent.CardArrived _ -> False

-- Whether a damage recipient is a player (CR 120.1): a total discriminator over
-- Recipient, so the combat-damage-to-player trigger matcher stays non-partial.
isPlayerRecipient :: Recipient.Recipient -> Bool
isPlayerRecipient r = case r of
  Recipient.ToPlayer _ -> True
  Recipient.ToCreature _ -> False
  Recipient.ToPlaneswalker _ -> False
  Recipient.ToBattle _ -> False
  Recipient.ToObject _ -> False
  Recipient.ToPile _ -> False
