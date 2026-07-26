module Pawl.Sba where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Departure as Departure
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Target as Target
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.Departure as Departure.Type
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Pool as Pool
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Zone as Zone

-- CR 704.5a (life <= 0), CR 704.5b (drawing from an empty library), and CR
-- 704.5c (ten or more poison counters). Two-Headed Giant's shared-poison variant
-- (CR 704.6b / 810) is out of scope (design.md §6).
losesNow :: GameState -> PlayerId -> Bool
losesNow gs pid = case Map.lookup pid (GameState.players gs) of
  Nothing -> False
  Just player ->
    Player.status player == Status.Playing
      && ( Player.life player <= 0
             || Set.member pid (GameState.drewFromEmpty gs)
             || Map.findWithDefault 0 PlayerCounterKind.Poison (Player.counters player) >= 10
         )

-- CR 704.5h: a creature with toughness > 0 dealt damage by a deathtouch source
-- since the last SBA check is destroyed. "Deathtouch source" is read from the
-- event's deal-time bit (CR 702.2e last-known information), NOT re-derived now --
-- so a source that lost deathtouch (Humility) or left after dealing damage is
-- still judged by what it was. See the M3b spec, section 4. Now read from the
-- WATERMARKED slice of the turn log, not a drained queue.
woundedByDeathtouch :: GameState -> ObjectId -> Bool
woundedByDeathtouch gs oid =
  let hits ev =
        DamageEvent.target ev == Recipient.ToCreature oid
          && DamageEvent.amount ev > 0
          && DamageEvent.dealtByDeathtouch ev
   in any hits (Event.unscannedDamage gs)

-- CR 704.5f: toughness 0 or less -- a put-into-graveyard, NOT a destruction, so
-- ungated by indestructible and NOT saved by regeneration (CR 701.19a).
--
-- The isCreature guard is not redundant. Only creatures have printed toughness
-- today, so toughnessOf already implies it -- but a Vehicle (CR 301.7) has P/T
-- while not being a creature, and 704.5f/g must not touch it. Ask the
-- classification, never the identity.
-- Takes the object's already-projected characteristics (checkStateBasedActions
-- projects the whole board once, per CR 704.4 simultaneity, rather than
-- re-projecting per object).
zeroToughness :: PC.ProjectedCharacteristics -> Bool
zeroToughness pc =
  Set.member CardType.Creature (PC.cardTypes pc)
    && case PC.toughness pc of
      Nothing -> False
      Just t -> t <= 0

-- CR 704.5g/h: a creature destroyed by lethal marked damage or by a deathtouch
-- source. A DESTRUCTION -- indestructible-gated (CR 700.4) and regeneration-
-- interceptable (CR 701.19a via Event.destroy). Excludes 704.5f (that is
-- zeroToughness), so toughness here is > 0.
destroyedBySba :: GameState -> PC.ProjectedCharacteristics -> ObjectId -> Bool
destroyedBySba gs pc oid =
  let isCreature = Set.member CardType.Creature (PC.cardTypes pc)
      indestructible = Set.member Keyword.Indestructible (PC.keywords pc)
   in isCreature && not indestructible && case PC.toughness pc of
        Nothing -> False
        Just toughness ->
          toughness > 0
            && ( ( case Game.lookupObject oid gs of
                     Nothing -> False
                     Just obj -> toInteger (Object.damage obj) >= toughness
                 )
                   || woundedByDeathtouch gs oid
               )

-- CR 704.5m: "If an Aura is attached to an illegal object or player, or is not
-- attached to an object or player, that Aura is put into its owner's graveyard."
-- Three clauses: unattached, attached to an id that is no longer a permanent, and
-- attached to one its own enchant ability no longer admits (CR 303.4c's "as
-- defined by its enchant ability and other applicable effects").
--
-- The third clause reads `pcs` -- the SAME pre-pass Projection.projectAll
-- performStateBasedActions computed once for every other CR 704.4 classification
-- -- via stillLegalEnchant below, instead of calling Target.stillLegal directly.
-- stillLegal reaches Target.legalRecipients -> basePool Pool.Creatures ->
-- creatureRecipients -> Projection.isCreatureOf, and THAT is `project oid gs` --
-- a fresh `gather` PER Aura. Every other classify here shares one `gather`
-- precisely because gather's neighbouring lesson (Projection.hs's `liveGiven`
-- comment) is that recomputing it inside a per-object loop makes project
-- O(permanents^3) per SBA sweep; calling stillLegal here reintroduced that same
-- shape one level down (20 permanents with 2 attached Auras costing ~40 extra
-- `gather`s and ~400 extra `project`s per pass).
--
-- CR 303.4d's first clause -- an Aura can't enchant itself -- is the `oid == self`
-- arm. Unreachable in this pool (a Creatures enchant spec cannot name the Aura
-- spell on the stack), written anyway because it costs one comparison. CR
-- 303.4d's SECOND clause -- an Aura that's also a creature can't enchant
-- anything -- is not implemented (#194).
--
-- A put-into-graveyard, NOT a destruction: CR 704.5m says "put into its owner's
-- graveyard", so this goes through Event.changeZone and consults neither
-- indestructible (CR 702.12b) nor a regeneration shield (CR 701.19a).
fallsOff :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> Bool
fallsOff pcs gs oid = case Game.cardOf oid gs of
  Nothing -> False
  Just card -> case Card.Type.enchant card of
    Nothing -> False
    Just spec -> case Game.lookupObject oid gs of
      Nothing -> False
      Just obj -> case Object.attachedTo obj of
        Nothing -> True
        Just target ->
          target == oid
            || not (stillLegalEnchant pcs gs oid spec target)

-- CR 303.4c / 608.2b: is `target` still a legal recipient for the enchanting
-- Aura `source`'s spec, read against `pcs` -- the pre-pass projection every
-- other classification in performStateBasedActions shares (CR 704.4
-- simultaneity), not a re-projection?
--
-- Pool.Creatures with no Filter is the ONLY shape any Card.enchant carries in
-- this pool today (Unholy Strength's "enchant creature"). Target.creatureRecipients
-- (Target.hs) tags every candidate it produces ToCreature, drawn from the
-- battlefield objects owned by a still-playing player (Game.stillPlaying);
-- with no Filter left to narrow that set, "still legal" reduces EXACTLY to
-- "still a creature, on the battlefield, owned by a player still in the game" --
-- which is what the Pool.Creatures arm below reads off `pcs` (a Map.lookup) plus
-- one owner check. This is not an approximation: `pcs Map.! target`, when it
-- exists, IS `Projection.project target gs` (projectAll folds the SAME gathered
-- candidate list stillLegal would rebuild from scratch), and a missing key means
-- exactly what Target.creatureRecipients' own battlefield scan would have missed
-- it for -- target is not on the battlefield at all.
--
-- Any OTHER shape -- a non-Creatures pool, or a Creatures spec that DOES carry a
-- Filter -- has no producer in this pool today (a second enchant pool, CR
-- 702.5d's enchant-player Auras, is #190). Rather than assume the
-- Creatures-with-no-Filter shape holds regardless (a shortcut that would go
-- silently wrong the day it stops holding), this falls through to the general,
-- slower Target.stillLegal, which reuses the SAME legality Cast/Resolve already
-- judge.
--
-- That fallback is general in its POOL and FILTER, not in its recipient TAG: it
-- still hard-codes Recipient.ToCreature, which is what Pool.Creatures produces
-- (Target.creatureRecipients). A Pool.Permanents enchant spec tags candidates
-- ToObject instead, so the membership test would fail and wrongly bury the Aura.
-- The tag must be derived from the spec's own Pool before a second enchant pool
-- exists (#190).
stillLegalEnchant :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> TargetSpec.TargetSpec -> ObjectId -> Bool
stillLegalEnchant pcs gs source spec target = case spec of
  TargetSpec.MkTargetSpec Pool.Creatures Nothing ->
    case Map.lookup target pcs of
      Nothing -> False
      Just pc ->
        Set.member CardType.Creature (PC.cardTypes pc)
          && case Game.lookupObject target gs of
            Nothing -> False
            Just obj -> List.elem (Object.owner obj) (Game.stillPlaying gs)
  _ -> Target.stillLegal source (Recipient.ToCreature target) spec gs

-- CR 704.3: repeat until no state-based action is performed. ONE pass here, with
-- the repeat living in Engine's CR 117.5 settle loop (settleForPriority). A
-- single pass is NOT sufficient IN GENERAL -- CR 704.5m's Aura falls off only on
-- the pass AFTER its creature dies -- so settleForPriority is the entry point a
-- caller wanting a settled board should use. Engine.runStep's own two direct,
-- unlooped calls are the exception, each safe for reasons local to that call
-- site, not repeated here.
-- CR 704.5n: "If an Equipment or Fortification is attached to an illegal
-- permanent or to a player, it becomes unattached from that permanent or player.
-- It remains on the battlefield."
--
-- The shape difference from CR 704.5m, which is why this is a separate
-- classification rather than another clause of fallsOff: an Aura DIES, an
-- Equipment merely DETACHES and stays. CR 301.5c says the same from the card
-- type's side -- "An Equipment that equips an illegal or nonexistent permanent
-- becomes unattached from that permanent but remains on the battlefield."
--
-- "Illegal" is CR 301.5: "An Equipment can be attached to a creature. It can't
-- legally be attached to anything that isn't a creature." So a host that is gone,
-- or that is not a creature, is illegal -- and `pcs` answers both at once, since
-- an object no longer on the battlefield has no entry in it.
--
-- Reads the shared pre-pass projection for the same reason fallsOff does (see its
-- haddock): a per-object project here would reintroduce the cubic sweep. An
-- Equipment whose creature dies THIS pass therefore detaches on the NEXT one,
-- exactly as an Aura falls off on the next one.
--
-- CR 704.5n's "or to a player" clause is unreachable: Object.attachedTo names an
-- object, so an Equipment attached to a player cannot be represented (#190).
becomesUnattached :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> Bool
becomesUnattached pcs gs oid = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Object.attachedTo obj of
    Nothing -> False
    Just host ->
      let isEquipment = case Map.lookup oid pcs of
            Nothing -> False
            Just pc -> Set.member Subtype.Equipment (PC.subtypes pc)
          hostIsCreature = case Map.lookup host pcs of
            Nothing -> False
            Just pc -> Set.member CardType.Creature (PC.cardTypes pc)
       in isEquipment && not hostIsCreature

checkStateBasedActions :: Game ()
checkStateBasedActions = Monad.void performStateBasedActions

-- One SBA pass, also reporting whether any state-based action was PERFORMED (a
-- creature buried or a player departed). CR 704.4: the caller repeats the check
-- while that flag is True. The flag lets the CR 117.5 settle loop (Engine) decide
-- whether to repeat WITHOUT a deep GameState comparison.
--
-- Monadic since P5: CR 704.5f's put-into-graveyard and CR 704.5g's destruction
-- both go through funnels that can now raise a CR 616 replacement prompt (a
-- creature dying with two applicable death-replacements genuinely must ask its
-- controller which to apply). M3g's decider re-entrancy already permits prompting
-- from inside the settle loop.
performStateBasedActions :: Game Bool
performStateBasedActions = do
  gs <- State.get
  let -- CR 704.5f/g are checked against the state BEFORE any of them apply: SBAs
      -- are simultaneous. Project the whole board once (one gather) and judge each
      -- object against it, rather than re-projecting per object.
      pcs = Projection.projectAll gs
      classify oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc
          -- CR 704.5f wins when both apply: toughness <= 0 is a put-into-graveyard.
          | zeroToughness pc -> Just False
          | destroyedBySba gs pc oid -> Just True
          | otherwise -> Nothing
      onBattlefield = Set.toList (GameState.battlefield gs)
      toGraveyard = filter (\oid -> classify oid == Just False) onBattlefield
      toDestroy = filter (\oid -> classify oid == Just True) onBattlefield
      -- CR 704.5q / 122.3: a permanent with both a +1/+1 and a -1/-1 counter has N
      -- of each removed (N = min). Computed against the SAME pre-pass state as the
      -- bury/destroy classification, which is what makes the ordering immaterial:
      -- net P/T is preserved, so it can neither cause nor prevent a death.
      annihilateOne oid = case Game.lookupObject oid gs of
        Nothing -> Nothing
        Just obj ->
          let cs = Object.counters obj
              plus = Map.findWithDefault 0 CounterKind.PlusOnePlusOne cs
              minus = Map.findWithDefault 0 CounterKind.MinusOneMinusOne cs
              n = min plus minus
           in if n > 0 then Just (oid, n) else Nothing
      annihilations = Maybe.mapMaybe annihilateOne onBattlefield
      -- CR 704.5m: an Aura attached to nothing, or to something its enchant
      -- ability no longer admits. Judged against the SAME pre-pass pcs/gs as
      -- every other classification above -- see fallsOff's Haddock for why an
      -- Aura whose creature dies THIS pass survives it and falls off the next.
      unattachedAuras = filter (fallsOff pcs gs) onBattlefield
      -- CR 704.5n: computed from the same pre-pass state, for the same reason.
      detaching = filter (becomesUnattached pcs gs) onBattlefield
      -- CR 704.5h's window is "since the last SBA check", so the watermark is the
      -- log length AS THIS PASS BEGAN: every 704.5h victim was computed from that
      -- same pre-pass state, and the Moved events this pass itself appends carry
      -- no damage. The record is never removed.
      watermark :: Natural
      watermark = fromIntegral (Seq.length (GameState.events gs))
  -- CR 704.5f: a plain put-into-graveyard (regeneration cannot save it).
  Monad.mapM_ (\oid -> Event.changeZone oid Zone.Graveyard) toGraveyard
  -- CR 704.5m: the Aura follows its creature. A plain put-into-graveyard.
  Monad.mapM_ (\oid -> Event.changeZone oid Zone.Graveyard) unattachedAuras
  -- CR 704.5n: the Equipment does NOT follow its creature -- it detaches and
  -- stays. Not a zone change, so unlike the Aura above it does not funnel through
  -- Pawl.Event: no Moved event, no replacement, no trigger.
  State.modify' (\g -> g {GameState.objects = List.foldl' (\m oid -> Map.adjust (\o -> o {Object.attachedTo = Nothing}) oid m) (GameState.objects g) detaching})
  -- CR 704.5g/h: destruction through the funnel (regeneration may replace it).
  Monad.mapM_ Event.destroy toDestroy
  destroyed <- State.get
  let leaving = filter (losesNow destroyed) (Game.stillPlaying destroyed)
      departed = foldr (Departure.depart Departure.Type.Lost) destroyed leaving
      -- CR 704.5d: a token in any zone other than the battlefield ceases to exist.
      -- Computed from the post-bury state so a token that just died (now in the
      -- graveyard) or was redirected (Rest in Peace -> exile) is removed here; its
      -- move already emitted a zone-change event, so a future dies-trigger still
      -- sees it (CR 111.7's parenthetical). Keyed to "not on the battlefield",
      -- never to a specific zone, so exile is caught too.
      isVanishingToken oid = case Game.lookupObject oid departed of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfToken _ -> Object.zone obj /= Zone.Battlefield
          _ -> False
      vanishing = filter isVanishingToken (Map.keys (GameState.objects departed))
      ceaseToExist g oid = case Game.lookupObject oid g of
        Nothing -> g
        Just obj ->
          let g1 = Game.removeFromZones (Object.owner obj) oid g
           in g1 {GameState.objects = Map.delete oid (GameState.objects g1)}
      vanished = List.foldl' ceaseToExist departed vanishing
      removeN n c = let c' = c - n in if c' == 0 then Nothing else Just c'
      balance g (oid, n) =
        let strip obj = obj {Object.counters = Map.update (removeN n) CounterKind.MinusOneMinusOne (Map.update (removeN n) CounterKind.PlusOnePlusOne (Object.counters obj))}
         in g {GameState.objects = Map.adjust strip oid (GameState.objects g)}
      outcome = Departure.outcomeAfterLeaving leaving departed
      drained = vanished {GameState.damageScannedThrough = watermark}
      balanced = List.foldl' balance drained annihilations
      -- A state-based action was performed iff a creature was buried or destroyed
      -- (a regenerated creature still counts, which the CR 704.4 settle loop
      -- re-checks and -- because the regen healed the damage -- terminates), a
      -- player left, a token ceased to exist, an Aura fell off (CR 704.5m), or an
      -- Equipment detached (CR 704.5n).
      acted = not (null toGraveyard) || not (null toDestroy) || not (null leaving) || not (null vanishing) || not (null annihilations) || not (null unattachedAuras) || not (null detaching)
  -- CR 104.1: a game ends the moment a result is reached, so a later pass may
  -- not replace one. The existing result therefore wins; this pass only settles
  -- an outcome when the game did not already have one. Same ordering as
  -- Departure.leaveGame -- the two doors that write GameState.result agree.
  State.put balanced {GameState.result = GameState.result balanced <|> outcome}
  pure acted
