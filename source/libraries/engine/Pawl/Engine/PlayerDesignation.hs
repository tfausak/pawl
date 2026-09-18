-- | CR 702.131 ascend and CR 702.195 storied: two keywords whose whole content is
-- a rest-of-game mark on a PLAYER, and the checks that grant each.
--
-- Pawl.Engine.Speed's sibling, and for its reason: rule 702 states these
-- abilities in the rulebook, so casing on Keyword.Ascend and Keyword.Storied here
-- is casing on the RULEBOOK, which Pawl.Types.Keyword's own comment licenses.
-- Nothing here asks which EFFECT anything came from.
--
-- Where it DIVERGES from speed is that neither rule is a state-based action.
-- Both are static abilities -- "any time you control ... you have ..." -- so the
-- check runs in Engine.performSettle beside Pawl.Engine.Daytime's, which is there
-- for the same reason: CR 704.3 makes "whenever a player would get priority" the
-- coarsest moment anything could observe the condition, so settling there is
-- indistinguishable from checking continuously. CR 702.131d and CR 702.195c both
-- put the reapplication of continuous effects after the gain and before the
-- trigger check, which is where performSettle's loop already puts it.
--
-- CR 702.131a is the third check here, and the one that is NOT continuous:
-- ascend on an instant or sorcery is a SPELL ability, performed once as the spell
-- resolves (`ascendOnSpellResolution`, called from Pawl.Engine.Resolve).
module Pawl.Engine.PlayerDesignation where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Keyword as Keyword.Engine
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerDesignation as PlayerDesignation
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- | CR 702.131b \/ 702.195a, checked at a CR 117.5 boundary: grant every mark a
-- player is owed. Reports whether it granted any, which is what keeps
-- Engine.performSettle's loop going -- a mark gained here turns on the "as long
-- as you have" clauses that read it, and CR 702.131d\/702.195c want those applied
-- before triggers are checked.
settle :: Game Bool
settle = do
  gs <- State.get
  let gains = gaining (Projection.projectAll gs) gs
  State.modify' (\g -> List.foldl' (\acc (pid, mark) -> grant pid mark acc) g gains)
  pure (not (null gains))

-- | The CONDITION half of both rules, read off the pre-pass projection so every
-- gain in one settle judges the same board.
--
-- The PROJECTED keywords and types, never the printed ones, for the reason
-- Speed.startingEngines states: the layer system grants and removes both, and a
-- permanent whose rules text CR 305.7 stripped carries neither word any more.
--
-- Membership of the carrier, not a count: both rules ask whether "you control"
-- such a permanent, so a second copy grants nothing extra -- and neither rule can
-- grant a mark twice, the "you don't have" clause being a standing guard rather
-- than a one-shot.
--
-- Ascending, and the two rules in a fixed order, so the pass's writes are
-- deterministic.
gaining :: Map ObjectId PC.ProjectedCharacteristics -> GameState -> [(PlayerId, PlayerDesignation.PlayerDesignation)]
gaining pcs gs =
  owed Keyword.Ascend PlayerDesignation.CitysBlessing citysBlessingThreshold (const True)
    <> owed Keyword.Storied PlayerDesignation.EnduringStory 3 storyMaterial
  where
    battlefield = Set.toList (GameState.battlefield gs)
    controlled = Maybe.mapMaybe (\oid -> fmap ((,) oid) (Projection.controllerOf oid gs)) battlefield
    carries kw pid = any (\(oid, c) -> c == pid && maybe False (Map.member kw . PC.keywords) (Map.lookup oid pcs)) controlled
    counted matches pid = length [() | (oid, c) <- controlled, c == pid, maybe False matches (Map.lookup oid pcs)]
    holds pid mark = maybe False (Set.member mark . Player.designations) (Map.lookup pid (GameState.players gs))
    owed :: Keyword.Keyword -> PlayerDesignation.PlayerDesignation -> Int -> (PC.ProjectedCharacteristics -> Bool) -> [(PlayerId, PlayerDesignation.PlayerDesignation)]
    owed kw mark threshold matches =
      [ (pid, mark)
      | pid <- Map.keys (GameState.players gs),
        carries kw pid,
        not (holds pid mark),
        counted matches pid >= threshold
      ]

-- | CR 702.195a's filter: "permanents that are artifacts, Sagas, and\/or
-- legendary". A disjunction over three different characteristics -- a card type,
-- a subtype and a supertype -- which is why it is spelled out rather than folded
-- into one membership test.
storyMaterial :: PC.ProjectedCharacteristics -> Bool
storyMaterial pc =
  Set.member CardType.Artifact (PC.cardTypes pc)
    || Set.member Subtype.Saga (PC.subtypes pc)
    || Set.member Supertype.Legendary (PC.supertypes pc)

-- | The ACTION half of both rules: "you get\/have [the mark] for the rest of the
-- game". A set-insert and nothing else -- CR 702.131c and CR 702.195b give each
-- mark no rules meaning beyond being a marker, and no rule takes either back.
grant :: PlayerId -> PlayerDesignation.PlayerDesignation -> GameState -> GameState
grant pid mark gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.designations = Set.insert mark (Player.designations p)}) pid (GameState.players gs)}

-- | CR 702.131a, ascend's OTHER half: the spell ability an instant or sorcery
-- with ascend performs as it resolves -- "if you control ten or more permanents
-- and you don't have the city's blessing, you get the city's blessing for the
-- rest of the game".
--
-- A one-shot rather than `settle`'s standing check, which is the whole of what
-- rule 702.131a differs from rule 702.131b by: reaching ten permanents later in
-- the turn grants nothing, the spell having already resolved. Nothing loops on
-- the result for that reason -- CR 702.131d's reapplication of continuous
-- effects happens at the next CR 117.5 boundary, where Engine.performSettle
-- already runs.
--
-- The keyword AND the card types are read off the projection rather than off the
-- printed face, `gaining`'s reading of CR 613.1f and CR 613.1d: a spell granted
-- ascend by a text-changing effect ascends and one whose text was blanked does
-- not, and layer 4 is what says which card types the resolving object has.
-- `Keyword.Engine.isSpellCard` is the same classification rule 702.55a's two
-- halves are told apart by.
--
-- That type test is a REGRESSION FENCE rather than a proven behaviour: the only
-- object it excludes is a resolving PERMANENT spell with ascend, and every board
-- that has one hands `settle` the same mark an instant later, so no gameplay-level
-- assertion can tell the two apart. Rule 702.131a's own wording is what it rests
-- on.
ascendOnSpellResolution :: ObjectId -> PlayerId -> Game ()
ascendOnSpellResolution oid controller = do
  gs <- State.get
  let ascends =
        Map.member Keyword.Ascend (Projection.keywordsOf oid gs)
          && Keyword.Engine.isSpellCard (Projection.cardTypesOf oid gs)
      -- "You don't have the city's blessing" is a guard the set-insert would not
      -- need; it is here because rule 702.131a states it, and because a second
      -- grant must stay unobservable.
      unblessed = not (maybe False (Set.member PlayerDesignation.CitysBlessing . Player.designations) (Map.lookup controller (GameState.players gs)))
  Monad.when (ascends && unblessed && controls gs >= citysBlessingThreshold) $
    State.modify' (grant controller PlayerDesignation.CitysBlessing)
  where
    controls gs = length [() | oid' <- Set.toList (GameState.battlefield gs), Projection.controllerOf oid' gs == Just controller]

-- | CR 702.131a's and CR 702.131b's shared "ten or more permanents". One constant
-- so the spell half and the static half cannot drift apart.
citysBlessingThreshold :: Int
citysBlessingThreshold = 10
