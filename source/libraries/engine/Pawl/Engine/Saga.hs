-- | CR 714, Saga cards: the chapter ability (CR 714.2), the lore counter a Saga
-- enters with (CR 714.3a), the turn-based action that advances it (CR 505.4 /
-- 703.4f / 714.3c) and the state-based action that sacrifices it once its story
-- is told (CR 704.5s / 714.4).
--
-- Pawl.Engine.Speed's sibling, and kept apart from Pawl.Engine.Sba and
-- Pawl.Engine.Engine for the same reason those two give: they own WHEN a
-- state-based action is checked and WHEN a turn-based action runs, not what any
-- one of them means.
--
-- Casing on Subtype.Saga here is casing on the RULEBOOK, exactly as
-- Pawl.Engine.Speed cases on Keyword.StartYourEngines: rule 714 is as much a part
-- of the comprehensive rules as rule 704. Nothing here asks which EFFECT a chapter
-- ability carries -- only which NUMBER its chapter symbol prints -- which is the
-- invariant that matters.
--
-- Imports no Pawl.Engine.Projection, deliberately: CR 714.3a's intrinsic ability
-- is minted by Projection.intrinsicReplacementsOf from `entryReplacementsOf`
-- below, so a dependency the other way would be a cycle. The two callers that need
-- a controller (Pawl.Engine.Sba's CR 704.5s pass and Pawl.Engine.Engine's CR 505.4
-- action) look one up themselves.
--
-- READ AHEAD (CR 702.155 / 714.3b) is not implemented (#841). Its absence is what
-- makes `entryReplacementsOf` unconditional and what makes `crossed` below read
-- "at least N" rather than "exactly N".
module Pawl.Engine.Saga where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Filter as Filter
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import Pawl.Types.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 714.2b's threshold crossing: did a count going from `before` to `after`
-- pass chapter `n`? "Was less than N and became at least N", verbatim.
--
-- Shared with Pawl.Engine.Event.matchesTrigger's SelfCountersReached arm rather
-- than written twice, since the SBA below has to agree with the trigger matcher
-- exactly: CR 704.5s exempts a Saga whose chapter ability has triggered, and a
-- second copy of this comparison drifting from the first would sacrifice a Saga
-- with an ability still waiting to go on the stack.
--
-- "At least N", not "exactly N": one placement can cross several chapters at once,
-- so a Saga handed two lore counters from nothing fires I and II together. Read
-- ahead (CR 702.155a) is the mechanic that narrows this to an equality, and it is
-- not implemented (#841).
crossed :: Natural -> Natural -> Natural -> Bool
crossed before after n = before < n && n <= after

-- | CR 714.2 / 714.2a: is this a chapter ability, and if so which chapter?
--
-- The whole of what the rules core knows about one. CR 714.2 calls a chapter
-- symbol a keyword ability representing a triggered ability, and CR 714.2b spells
-- that triggered ability out -- so a chapter ability is not a separate kind of
-- card data here, it is an ordinary entry in `triggeredAbilities` whose condition
-- is the one rule 714.2b writes. CR 714.2c's "{rN1}, {rN2}--[Effect]" needs no
-- representation at all: that rule says the shorthand MEANS two abilities, so a
-- card prints two entries sharing one modal.
chapterOf :: TriggeredAbility card -> Maybe Natural
chapterOf ability = case TriggeredAbility.condition ability of
  TriggerCondition.SelfCountersReached CounterKind.Lore n -> Just n
  _ -> Nothing

-- | CR 714.2d: the chapter numbers a permanent's abilities print.
--
-- Reads the PROJECTED abilities, never the printed ones, for the reason
-- Pawl.Engine.Speed.startingEngines gives about keywords: the layer system grants
-- and removes abilities, so a Saga under Humility (CR 613.1f) has no chapter
-- abilities and every rule below that asks for "one or more" answers no.
--
-- Not gated on the Saga subtype. CR 714.2 puts chapter symbols on Sagas, and each
-- caller below applies that gate where its own rule states it -- CR 714.3c and CR
-- 704.5s both say "Saga" in so many words.
chaptersOf :: PC.ProjectedCharacteristics -> [Natural]
chaptersOf = Maybe.mapMaybe chapterOf . PC.triggeredAbilities

-- | CR 714.2d: "A Saga's final chapter number is the greatest value among chapter
-- abilities it has. If a Saga somehow has no chapter abilities, its final chapter
-- number is 0."
--
-- The rule legislates the empty maximum card-shape by card-shape, which is why
-- this returns a Natural where Pawl.Engine.Count's Aggregation.Greatest returns
-- Nothing over an empty set. Both are right: that fold has no rule handing it a
-- value and this one does.
finalChapterOf :: PC.ProjectedCharacteristics -> Natural
finalChapterOf pc = case chaptersOf pc of
  [] -> 0
  ns -> maximum ns

-- | CR 205.3h / 714.1: is this permanent a Saga?
isSaga :: PC.ProjectedCharacteristics -> Bool
isSaga = Set.member Subtype.Saga . PC.subtypes

-- | CR 714.3c / 505.4 / 704.5s: "a Saga ... with one or more chapter abilities".
-- One predicate, because all three rules name the same permanents and a second
-- copy is what the CR-citation discipline exists to prevent.
--
-- The SUBTYPE alone, though CR 505.4 and CR 703.4f both say "Saga ENCHANTMENT"
-- where CR 704.5s says "Saga permanent". The two readings cannot differ today,
-- and the reason is a capability pawl lacks rather than a claim about Magic:
-- Pawl.Types.Modification has AddCardType and no SET, so no effect in the pool can
-- take the enchantment card type away from a permanent while leaving the subtype
-- on it. The card that brings a card-type SET is the one that must make this
-- conjunction explicit -- CR 205.1a would strip an enchantment-only subtype along
-- with the type, so the honest fix then is to prune the subtype rather than to add
-- a card-type conjunct here.
tracksLore :: PC.ProjectedCharacteristics -> Bool
tracksLore pc = isSaga pc && not (null (chaptersOf pc))

-- | CR 714.3a: "Each Saga without read ahead has the intrinsic ability 'This Saga
-- enters with a lore counter on it.' This ability creates a replacement effect
-- (see rule 614.1c)."
--
-- The shape CR 306.5b's starting loyalty already uses, and minted the same way --
-- from the finished projection, by Pawl.Engine.Projection.intrinsicReplacementsOf
-- -- so the counters go through Pawl.Engine.Event.putCounters and CR 614.16 holds:
-- Doubling Season gives a Saga two lore counters as it enters, and CR 714.2b's
-- chapters I and II both fire off that one placement.
--
-- Keyed on the SUBTYPE alone and NOT on `tracksLore`, which is rule 714.3a's own
-- wording: it says "each Saga", where CR 714.3c and CR 704.5s both add "with one
-- or more chapter abilities". A Saga stripped of its abilities still enters with a
-- lore counter and then never advances.
--
-- Unconditional because read ahead (CR 702.155b) is not implemented (#841); the
-- card that brings it is the one that must split this into rule 714.3a's arm and
-- rule 714.3b's.
entryReplacementsOf :: PC.ProjectedCharacteristics -> [ReplacementEffect]
entryReplacementsOf pc =
  [ -- CR 614.1c: the entering object is the ability's own source.
  ReplacementEffect.EntryR Filter.IsSource (EntryRewrite.WithCounters CounterKind.Lore 1)
  | isSaga pc
  ]

-- | The lore counters on a permanent (CR 714.3). Zero for an object the game does
-- not hold, which is the same answer Pawl.Engine.Cost.loyaltyCountersOn gives for
-- rule 306.5c's reading.
loreOn :: ObjectId -> GameState -> Natural
loreOn oid gs = case Game.lookupObject oid gs of
  Nothing -> 0
  Just obj -> Map.findWithDefault 0 CounterKind.Lore (Object.counters obj)

-- | CR 505.4 / 703.4f / 714.3c: the Sagas a player puts a lore counter on as their
-- precombat main phase begins. The turn-based action's CLASSIFIER half, kept pure
-- so Pawl.Engine.Engine only has to perform it.
--
-- Takes the controller lookup rather than importing Pawl.Engine.Projection, for
-- the reason this module's header gives.
--
-- Ascending, so the placements and any transcript are deterministic. The ORDER is
-- unobservable in the rules -- CR 703.4f puts every counter on as one turn-based
-- action -- but the chapter abilities it fires are gathered as one batch and
-- ordered by their controller under CR 603.3b, so nothing here is deciding for
-- them.
advancing :: (ObjectId -> Maybe PlayerId) -> PlayerId -> Map ObjectId PC.ProjectedCharacteristics -> GameState -> [ObjectId]
advancing controllerOf pid pcs gs =
  let mine oid = case Map.lookup oid pcs of
        Nothing -> False
        Just pc -> tracksLore pc && controllerOf oid == Just pid
   in filter mine (Set.toAscList (GameState.battlefield gs))

-- | CR 704.5s's exemption: is this permanent "the source of a chapter ability that
-- has triggered but not yet left the stack"?
--
-- TWO halves, because the rule says "triggered" and not "is on the stack". An
-- ability that has fired but has not yet been put onto the stack is squarely
-- inside that phrase, and the engine has a window where exactly that is true:
-- Pawl.Engine.Engine.performSettle runs the CR 704.5 pass BEFORE
-- placePendingTriggers, so a Saga reaching its final chapter through CR 714.3c's
-- turn-based action meets this check with its last chapter ability still in the
-- unscanned event log. Without the second half the Saga would be sacrificed first
-- and the ability would resolve from CR 603.10a's look-back -- observable, and the
-- wrong order.
--
-- The stack half compares OBJECT IDS. CR 400.7 mints a fresh id on every zone
-- change, which is what makes that right: a Saga that flickered is a new permanent
-- and is not the source of the old one's chapter ability.
--
-- The unscanned events arrive as an ARGUMENT rather than being read off
-- GameState.scannedThrough here, because Pawl.Engine.Event owns that watermark and
-- this module sits below it in the import graph. Pawl.Engine.Sba, which has both,
-- is what joins them.
awaitingChapter :: Map ObjectId PC.ProjectedCharacteristics -> [GameEvent] -> GameState -> ObjectId -> Bool
awaitingChapter pcs events gs oid =
  let chapters = foldMap chaptersOf (Map.lookup oid pcs)
      -- On the stack: an ability object whose source is this permanent and whose
      -- condition is a chapter symbol. Read off Object.source, the same shape
      -- Pawl.Engine.Resolve.alreadyTurnedFor uses for CR 113.7a.
      --
      -- The `chapterOf` conjunct is CR 704.5s's own word -- it exempts a Saga
      -- owed a CHAPTER ability, not one owed any ability at all -- and has NO
      -- falsifier in the pool: History of Benalia's only triggered abilities are
      -- its chapters, so dropping the conjunct changes no game. CR 714.1a's Saga
      -- creature, whose second text box holds abilities independent of its chapter
      -- symbols, is the shape that would tell them apart.
      onStack sid = case fmap Object.source (Game.lookupObject sid gs) of
        Just (Source.OfTrigger srcId ability) -> srcId == oid && Maybe.isJust (chapterOf ability)
        _ -> False
      -- Triggered but not yet placed: an unscanned CR 122.6 placement on this
      -- permanent that crossed one of its chapters. The same comparison
      -- Pawl.Engine.Event.matchesTrigger makes, through the same `crossed`.
      pending event = case event of
        GameEvent.CountersPut target CounterKind.Lore before after ->
          target == oid && any (crossed before after) chapters
        _ -> False
   in any onStack (GameState.stack gs) || any pending events

-- | CR 704.5s / 714.4: the Sagas whose controllers sacrifice them, paired with the
-- sacrificing player. The state-based action's CLASSIFIER half, taking the
-- pre-pass projection so Pawl.Engine.Sba can judge it against the same board as
-- every other CR 704.5 clause (CR 704.3's simultaneity).
--
-- All three of the rule's conjuncts, in its own order: a Saga with one or more
-- chapter abilities, lore counters at or past its final chapter number, and not
-- the source of a chapter ability still owed a resolution.
--
-- "Greater than or equal to", not "equal to", so a Saga given more lore counters
-- than it has chapters is sacrificed rather than stranded.
--
-- Ascending, for `advancing`'s reason.
sacrificing :: (ObjectId -> Maybe PlayerId) -> Map ObjectId PC.ProjectedCharacteristics -> [GameEvent] -> GameState -> [(PlayerId, ObjectId)]
sacrificing controllerOf pcs events gs =
  let done oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc
          | tracksLore pc,
            loreOn oid gs >= finalChapterOf pc,
            not (awaitingChapter pcs events gs oid) ->
              fmap (\pid -> (pid, oid)) (controllerOf oid)
          | otherwise -> Nothing
   in Maybe.mapMaybe done (Set.toAscList (GameState.battlefield gs))
