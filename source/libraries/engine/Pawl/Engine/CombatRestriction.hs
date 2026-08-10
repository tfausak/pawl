-- CR 508.1c / 509.1b / 613.11: the continuous effects that FORBID an attack or
-- a block. One of the modules on the axis CR 613.11 reaches past the layer
-- system (alongside Pawl.Engine.PlayerEffect, Pawl.Engine.BlockRequirement and
-- Pawl.Engine.AttackRequirement). None is a layer, and Pawl.Engine.Projection
-- sees none of them.
--
-- The only module that may CASE on Pawl.Types.CombatRestriction.
-- Pawl.Engine.Keyword constructs one -- rule 702.98a's unleash -- and reads none.
-- Pawl.Engine.Combat asks for a SET OF IDS, or for a NUMBER, and never learns
-- which card, or which keyword, produced either.
module Pawl.Engine.CombatRestriction where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype

-- CR 508.1c: which of `candidates` an effect in force right now says CAN'T
-- ATTACK. Pacifism's first half, and Blind-Spot Giant's when its gate is shut.
cantAttack :: [ObjectId] -> GameState -> Set ObjectId
cantAttack = restricted attacking

-- CR 509.1b: which of `candidates` an effect in force right now says CAN'T
-- BLOCK. Pacifism's second half, and Blind-Spot Giant's when its gate is shut.
cantBlock :: [ObjectId] -> GameState -> Set ObjectId
cantBlock = restricted blocking

-- CR 508.1c together with CR 506.5: which of `candidates` an effect in force
-- right now says can't be the ONLY creature declared as an attacker. Bonded
-- Construct.
--
-- The two above are answered ABOUT A CANDIDATE and this one is not, even though
-- all three come back as a set of ids: this set is the input to a whole
-- declaration's check (Pawl.Engine.Combat.attackDeclarationAllowed) rather than a
-- filter on the candidate list. See `restricted` below.
cantAttackAlone :: [ObjectId] -> GameState -> Set ObjectId
cantAttackAlone = restricted attackingAlone

-- CR 508.1c: the TIGHTEST bound in force right now on how many creatures may be
-- declared as attackers, or Nothing when nothing bounds it. Silent Arbiter's
-- first sentence.
--
-- Takes no candidate list, because a bound names no creature: it is not a fact
-- about anybody's creatures and it is not scoped to the controller of the card
-- stating it. Two Silent Arbiters still allow one attacker, and a "no more than
-- two" beside a "no more than one" binds at one, which is what makes the answer
-- a minimum.
attackLimit :: GameState -> Maybe Natural
attackLimit = bounded attackingMoreThan

-- CR 509.1b, the blocking counterpart. Silent Arbiter's second sentence.
blockLimit :: GameState -> Maybe Natural
blockLimit = bounded blockingMoreThan

-- The selectors, written out rather than a wildcard: an exhaustive case is what
-- makes a new arm a compile error at every site that would have to decide about
-- it.
attacking :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
attacking cr = case cr of
  CombatRestriction.CantAttack a _ -> Just a
  CombatRestriction.CantBlock _ _ -> Nothing
  CombatRestriction.CantAttackAlone _ _ -> Nothing
  CombatRestriction.CantAttackMoreThan _ _ -> Nothing
  CombatRestriction.CantBlockMoreThan _ _ -> Nothing

blocking :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
blocking cr = case cr of
  CombatRestriction.CantAttack _ _ -> Nothing
  CombatRestriction.CantBlock a _ -> Just a
  CombatRestriction.CantAttackAlone _ _ -> Nothing
  CombatRestriction.CantAttackMoreThan _ _ -> Nothing
  CombatRestriction.CantBlockMoreThan _ _ -> Nothing

attackingAlone :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
attackingAlone cr = case cr of
  CombatRestriction.CantAttack _ _ -> Nothing
  CombatRestriction.CantBlock _ _ -> Nothing
  CombatRestriction.CantAttackAlone a _ -> Just a
  CombatRestriction.CantAttackMoreThan _ _ -> Nothing
  CombatRestriction.CantBlockMoreThan _ _ -> Nothing

-- The bound selectors. A separate pair from the three above rather than a fourth
-- and fifth of them, because what they answer is a NUMBER and no Affected can
-- stand in for one -- which is the whole reason the arms exist.
attackingMoreThan :: CombatRestriction.CombatRestriction -> Maybe Natural
attackingMoreThan cr = case cr of
  CombatRestriction.CantAttack _ _ -> Nothing
  CombatRestriction.CantBlock _ _ -> Nothing
  CombatRestriction.CantAttackAlone _ _ -> Nothing
  CombatRestriction.CantAttackMoreThan n _ -> Just n
  CombatRestriction.CantBlockMoreThan _ _ -> Nothing

blockingMoreThan :: CombatRestriction.CombatRestriction -> Maybe Natural
blockingMoreThan cr = case cr of
  CombatRestriction.CantAttack _ _ -> Nothing
  CombatRestriction.CantBlock _ _ -> Nothing
  CombatRestriction.CantAttackAlone _ _ -> Nothing
  CombatRestriction.CantAttackMoreThan _ _ -> Nothing
  CombatRestriction.CantBlockMoreThan n _ -> Just n

-- CR 508.1c / CR 509.1b's second clause: the condition the creature can't
-- attack (or block) UNLESS. Read off any arm, because the clause is the same
-- sentence in both rules: which declaration a restriction forbids, in what
-- shape, and whether it is gated are all independent, and Blind-Spot Giant
-- prints one gate across two arms.
--
-- Nothing is the UNCONDITIONAL restriction (Pacifism), not a gate that fails.
gate :: CombatRestriction.CombatRestriction -> Maybe Condition.Type.Condition
gate cr = case cr of
  CombatRestriction.CantAttack _ c -> c
  CombatRestriction.CantBlock _ c -> c
  CombatRestriction.CantAttackAlone _ c -> c
  CombatRestriction.CantAttackMoreThan _ c -> c
  CombatRestriction.CantBlockMoreThan _ c -> c

-- Every combat restriction some permanent on the battlefield states right now,
-- each paired with its SOURCE and with CR 612.1's word swap over that source's
-- own text. A restriction whose gate HOLDS is not here at all, because CR
-- 508.1c / CR 509.1b's "unless" lifts it, so both readers below see only
-- restrictions in force.
--
-- The shared half of `restricted` and `bounded`, which is where the liveness
-- checks, the gate and the text change belong: those three are properties of the
-- SOURCE and of the printed sentence, and neither depends on whether the reader
-- wants a set of creatures or a number.
inForce :: GameState -> [(ObjectId, [(Subtype.Subtype, Subtype.Subtype)], CombatRestriction.CombatRestriction)]
inForce gs =
  let -- Hoisted out of the walk as AttackRequirement.instances hoists them, and
      -- both unforced until some permanent actually declares a restriction.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- CR 508.1c / CR 509.1b's second clause. A gate that HOLDS lifts the
      -- restriction, so it is dropped here; one that does not leaves it in
      -- force, which is why an ungated restriction is False here.
      --
      -- Evaluated once per RESTRICTION and not per candidate, because the
      -- clause belongs to the ability rather than to the creature it names: CR
      -- 109.5 fixes the "you" inside it as the SOURCE's controller, and
      -- Filter.IsSource names the source -- which is what makes Blind-Spot
      -- Giant's "another Giant" exclude the Giant printing the sentence.
      --
      -- Projection.fullView, matching the affected set `restricted` reads (CR
      -- 613.11). The source is on the battlefield by construction, so no CR
      -- 608.2h last known information is in play.
      --
      -- CR 612.1: the gate is REWRITTEN before it is asked, so a hacked Glacial
      -- Crasher ("can't attack unless there is a Mountain on the battlefield")
      -- counts Islands. The clause is words printed on the source's card, which
      -- is what CR 612.1 reaches, and rewriteCondition is the same descent
      -- gatherStatic applies to a static ability's CR 604.2 "as long as" gate.
      lifted source changes restriction = case gate restriction of
        Nothing -> False
        Just condition ->
          Condition.holds
            (Projection.fullView gs)
            (Filter.contextFor (Projection.controllerOf source gs) (Just source))
            gs
            source
            (if null changes then condition else Projection.rewriteCondition changes condition)
      -- CR 702: the restrictions rule 702 gives a permanent for HOLDING A
      -- KEYWORD, which until unleash (CR 702.98a) nothing produced -- every row
      -- here was printed card data. Pawl.Engine.Keyword.mintedCombatRestrictionsOf
      -- is the mint, beside the one that gives riot its replacement effect.
      --
      -- Read off the PROJECTION rather than the printed face, so a granted or
      -- removed keyword is seen (CR 613.1f). That is also why these rows skip the
      -- two ability gates the printed ones below pass: the projection has already
      -- applied both, in layer order, where `removed` asked here would drop a
      -- keyword a later-timestamped grant restored.
      --
      -- No CR 612.1 word swap either, and none is owed: what a keyword MEANS is
      -- rule 702's text, and CR 612.2 reaches words printed on the card.
      --
      -- Through the same "unless" gate the printed rows pass, with an empty text
      -- change: unleash's row is ungated, so this filter keeps nothing out today
      -- and is here so the next minted restriction that IS gated cannot slip past
      -- CR 508.1c's second clause.
      mintedRows source =
        [ (source, [], restriction)
        | anyMinted,
          restriction <- Keyword.mintedCombatRestrictionsOf (Projection.keywordsOf source gs),
          not (lifted source [] restriction)
        ]
      -- The short-circuit Pawl.Engine.Projection.replacementsAffecting takes, for
      -- its reason: projecting every permanent on every declaration would cost
      -- every board a walk that almost no board needs. One shared thunk, so the
      -- battlefield is scanned once per read rather than once per permanent.
      --
      -- Reading BASE faces is enough for the reason given there -- a keyword
      -- reaches a permanent either from its own base face or from a grant whose
      -- granting permanent is on the battlefield -- and it inherits the same hole:
      -- a minting keyword arriving through a stored continuous effect or a keyword
      -- counter is on no base face (#833).
      anyMinted = any baseCouldMint (Set.toList (GameState.battlefield gs))
      baseCouldMint oid = case Game.faceOf oid gs of
        Nothing -> False
        Just face ->
          any Keyword.mintsCombatRestriction (Face.keywords face)
            || any (any (Projection.grantsKeywordWhere Keyword.mintsCombatRestriction) . StaticAbility.modifications) (Face.staticAbilities face)
      -- CR 701.60c: "a suspected permanent has menace and 'This creature can't
      -- block' for as long as it's suspected". Decayed's row (CR 702.147a) with
      -- the keyword swapped for the designation -- aimed at the source alone,
      -- with no CR 509.1b "unless" gate -- and read off Object.suspected rather than stamped
      -- when the designation was set, so it ends when the designation does. The
      -- menace half is a characteristic, and lives in
      -- Pawl.Engine.Projection.designationGathered.
      --
      -- Outside `anyMinted`, which asks about keywords, but INSIDE `keepsAbilities`
      -- below: rule 701.60c states the restriction as quoted text, so what the
      -- designation gives the permanent is an ABILITY, and CR 305.7's strip and a
      -- CR 613.1f layer-6 removal both reach it.
      --
      -- Not implemented: CR 613.1f's ordering between the two. A removal with an
      -- EARLIER timestamp than the permanent's own leaves the ability in place,
      -- and this gate drops it anyway (#1216). The menace half has no such hole,
      -- going through the layer fold itself.
      designationRows source = case Game.lookupObject source gs of
        Just obj | Object.suspected obj -> [(source, [], CombatRestriction.CantBlock (Affected.Matching Filter.Type.IsSource) Nothing)]
        _ -> []
      -- The two ability losses the printed rows below check for, named because the
      -- designation row above asks the same question: CR 305.7's basic-land
      -- subtype set, and CR 604.2 against a CR 613.1f layer-6 removal.
      keepsAbilities source = (null setEffs || Projection.liveAfterLayers setEffs source gs) && not (removed source)
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face ->
          (if keepsAbilities source then designationRows source else []) <> mintedRows source <> case Face.combatRestrictions face of
            -- Every permanent in almost every game.
            [] -> []
            restrictions ->
              -- The same two ability losses AttackRequirement.instances asks
              -- about, as `keepsAbilities` above. Why CR 613.6 cannot rescue a
              -- restriction that has started to apply is argued in
              -- BlockRequirement.instances, as is why CR 613.11 also lets the CR
              -- 305.7 gate be liveAfterLayers rather than liveGiven.
              if keepsAbilities source
                then
                  -- CR 612.1's word swap over the source's own text, computed HERE
                  -- rather than hoisted beside setEffs: textChangesAffecting folds
                  -- the whole continuous-effect list, and the empty case above
                  -- already turned away every permanent that prints no restriction,
                  -- so the fold runs once per restriction-bearing permanent instead
                  -- of once per permanent on the battlefield.
                  --
                  -- The SOURCE's changes and not the restricted creature's: CR
                  -- 612.1 changes the words printed on THAT object, and the gate
                  -- is printed on the card stating the restriction.
                  let changes = Projection.textChangesAffecting source gs
                   in [ (source, changes, restriction)
                      | restriction <- restrictions,
                        not (lifted source changes restriction)
                      ]
                else []
   in concatMap fromPermanent (Set.toList (GameState.battlefield gs))

-- The shared walk behind the three SUBJECT-CARRYING questions above, over the
-- restrictions `select` keeps.
--
-- A set of ids and not a per-creature predicate: the caller asks this once per
-- declaration pass and then tests against the answer, where a predicate would
-- walk the whole battlefield per candidate and make the pass quadratic (#200).
-- `candidates` is the caller's chosen-from set (CR 508.1a for attacking, CR
-- 509.1a for blocking).
--
-- WHAT THE CALLER DOES WITH THE ANSWER is the caller's, and the two things done
-- with it are not interchangeable. `cantAttack` and `cantBlock` name creatures
-- that are in no legal declaration at all, so their sets are subtracted from the
-- candidate list before anything else runs; that also keeps CR 508.1d's and CR
-- 509.1c's maximizations honest, since a creature that cannot act can obey no
-- requirement. `cantAttackAlone` names creatures that are in SOME legal
-- declaration, so its set must stay on the candidate list and be asked of the
-- finished declaration instead -- subtracting it would forbid the very
-- declaration CR 508.1c's Example calls legal.
restricted :: (CombatRestriction.CombatRestriction -> Maybe Affected.Affected) -> [ObjectId] -> GameState -> Set ObjectId
restricted select candidates gs =
  let -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection -- the opposite of
      -- Projection.affects's callers inside the layer fold, which read
      -- characteristics as of their own layer.
      named source affected creature =
        Projection.affects
          source
          creature
          affected
          (Projection.project creature gs)
          gs
      -- CR 612.1 again, on the other half of the same printed sentence: the
      -- AFFECTED set is rewritten before it is asked, so a hacked "Swamps can't
      -- attack" reads Islands. Same descent gatherStatic applies to a static
      -- ability's affected clause, and the same `changes` the gate in `inForce`
      -- uses -- both clauses are words printed on the SOURCE's card, so one text
      -- change reaches both or neither.
      fromRestriction (source, changes, restriction) = case select restriction of
        Nothing -> []
        Just affected -> filter (named source (if null changes then affected else Projection.rewriteAffected changes affected)) candidates
   in Set.fromList (concatMap fromRestriction (inForce gs))

-- The shared walk behind the two BOUNDS above, over the restrictions `select`
-- keeps: the tightest of them, or Nothing where none is in force.
--
-- A MINIMUM, because CR 508.1c's restrictions are cumulative -- a declaration
-- must disobey none of them, so two bounds leave only the declarations both
-- allow. Nothing of the source survives into the answer, which is the point:
-- Pawl.Engine.Combat learns a number and never learns which card produced it.
--
-- No text change is applied, and none is owed: CR 612.1 swaps one word for
-- another, and the only words a bound prints beyond its gate are a number, which
-- no text-changing effect in the pool reaches. The gate itself was already
-- rewritten and asked in `inForce`.
bounded :: (CombatRestriction.CombatRestriction -> Maybe Natural) -> GameState -> Maybe Natural
bounded select gs =
  let tighter acc n = Just (maybe n (min n) acc)
   in List.foldl' tighter Nothing (Maybe.mapMaybe (\(_, _, restriction) -> select restriction) (inForce gs))
