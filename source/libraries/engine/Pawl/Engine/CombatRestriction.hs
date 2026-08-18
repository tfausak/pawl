-- CR 508.1c / 509.1b / 613.11: the continuous effects that FORBID an attack or
-- a block. One of the modules on the axis CR 613.11 reaches past the layer
-- system (alongside Pawl.Engine.PlayerEffect, Pawl.Engine.BlockRequirement and
-- Pawl.Engine.AttackRequirement). None is a layer, and Pawl.Engine.Projection
-- sees none of them.
--
-- Not all of them are card data: CR 701.35a's detain forbids both declarations
-- and is read off the victim (Pawl.Engine.Detain) rather than off any card's
-- printed text. See `detained`.
--
-- The only module that may CASE on Pawl.Types.CombatRestriction.
-- Pawl.Engine.Keyword constructs one -- rule 702.98a's unleash -- and reads none.
-- Pawl.Engine.Combat asks for a SET OF IDS, for a SET OF PAIRS, or for a NUMBER,
-- and never learns which card, or which keyword, produced any of them. The pairs
-- are CR 509.1b's pairwise restrictions (cantBeBlockedBy), which no set of
-- creatures could state; the Filter that decides them is evaluated here, so no
-- Filter crosses into Pawl.Engine.Combat.
module Pawl.Engine.CombatRestriction where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Detain as Detain
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedUnless as AffectedUnless
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LimitUnless as LimitUnless
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype

-- CR 508.1c: which of `candidates` an effect in force right now says CAN'T
-- ATTACK. Pacifism's first half, Blind-Spot Giant's when its gate is shut, and CR
-- 701.35a's first clause.
cantAttack :: [ObjectId] -> GameState -> Set ObjectId
cantAttack candidates gs = Set.union (restricted attacking candidates gs) (detained candidates gs)

-- CR 509.1b: which of `candidates` an effect in force right now says CAN'T
-- BLOCK. Pacifism's second half, Blind-Spot Giant's when its gate is shut, and CR
-- 701.35a's second clause.
cantBlock :: [ObjectId] -> GameState -> Set ObjectId
cantBlock candidates gs = Set.union (restricted blocking candidates gs) (detained candidates gs)

-- CR 701.35a: the detained permanents among `candidates`, which that rule forbids
-- both declarations at once -- so one reading serves both gates above.
--
-- UNIONED in here rather than added to `inForce` below, and the difference is
-- that a detain is not printed text. Every row `inForce` gathers is paired with a
-- SOURCE, rewritten by CR 612.1's word swap over that source and dropped when CR
-- 305.7 or CR 613.1f takes that source's abilities away; a detain has outlived
-- its source entirely (CR 611.2), so it would have to opt out of all three -- the
-- stored posture Pawl.Engine.BlockRequirement's own rows take. It also names no
-- Pawl.Types.Affected: rule 701.35a's subject is one permanent the resolution
-- already chose, and Pawl.Engine.Detain records it on that permanent.
detained :: [ObjectId] -> GameState -> Set ObjectId
detained candidates gs = Set.fromList (filter (`Detain.detained` gs) candidates)

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
  CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless a _) -> Just a
  CombatRestriction.CantBlock {} -> Nothing
  CombatRestriction.CantBeBlockedBy {} -> Nothing
  CombatRestriction.CantAttackAlone {} -> Nothing
  CombatRestriction.CantAttackMoreThan {} -> Nothing
  CombatRestriction.CantBlockMoreThan {} -> Nothing

blocking :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
blocking cr = case cr of
  CombatRestriction.CantAttack {} -> Nothing
  CombatRestriction.CantBlock (AffectedUnless.MkAffectedUnless a _) -> Just a
  -- The Affected here names ATTACKERS, never blockers, so this selector must not
  -- see it: answering Just would take the Ring-bearer off CR 509.1a's candidate
  -- list, which is the opposite of what CR 701.54c says.
  CombatRestriction.CantBeBlockedBy {} -> Nothing
  CombatRestriction.CantAttackAlone {} -> Nothing
  CombatRestriction.CantAttackMoreThan {} -> Nothing
  CombatRestriction.CantBlockMoreThan {} -> Nothing

attackingAlone :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
attackingAlone cr = case cr of
  CombatRestriction.CantAttack {} -> Nothing
  CombatRestriction.CantBlock {} -> Nothing
  CombatRestriction.CantBeBlockedBy {} -> Nothing
  CombatRestriction.CantAttackAlone (AffectedUnless.MkAffectedUnless a _) -> Just a
  CombatRestriction.CantAttackMoreThan {} -> Nothing
  CombatRestriction.CantBlockMoreThan {} -> Nothing

-- The PAIRWISE selector. Its own, and not a fourth of the three above, because
-- what it answers is an Affected TOGETHER WITH the Filter describing the blockers
-- barred from it: the two are one sentence, and an Affected alone would say "this
-- creature can't be blocked" full stop.
blockedBy :: CombatRestriction.CombatRestriction -> Maybe (Affected.Affected, Filter.Type.Filter Keyword.Type.Keyword)
blockedBy cr = case cr of
  CombatRestriction.CantAttack {} -> Nothing
  CombatRestriction.CantBlock {} -> Nothing
  CombatRestriction.CantBeBlockedBy (CantBeBlockedBy.MkCantBeBlockedBy a f _) -> Just (a, f)
  CombatRestriction.CantAttackAlone {} -> Nothing
  CombatRestriction.CantAttackMoreThan {} -> Nothing
  CombatRestriction.CantBlockMoreThan {} -> Nothing

-- The bound selectors. A separate pair from the four above rather than a fifth
-- and sixth of them, because what they answer is a NUMBER and no Affected can
-- stand in for one -- which is the whole reason the arms exist.
attackingMoreThan :: CombatRestriction.CombatRestriction -> Maybe Natural
attackingMoreThan cr = case cr of
  CombatRestriction.CantAttack {} -> Nothing
  CombatRestriction.CantBlock {} -> Nothing
  CombatRestriction.CantBeBlockedBy {} -> Nothing
  CombatRestriction.CantAttackAlone {} -> Nothing
  CombatRestriction.CantAttackMoreThan (LimitUnless.MkLimitUnless n _) -> Just n
  CombatRestriction.CantBlockMoreThan {} -> Nothing

blockingMoreThan :: CombatRestriction.CombatRestriction -> Maybe Natural
blockingMoreThan cr = case cr of
  CombatRestriction.CantAttack {} -> Nothing
  CombatRestriction.CantBlock {} -> Nothing
  CombatRestriction.CantBeBlockedBy {} -> Nothing
  CombatRestriction.CantAttackAlone {} -> Nothing
  CombatRestriction.CantAttackMoreThan {} -> Nothing
  CombatRestriction.CantBlockMoreThan (LimitUnless.MkLimitUnless n _) -> Just n

-- CR 508.1c / CR 509.1b's second clause: the condition the creature can't
-- attack (or block) UNLESS. Read off any arm, because the clause is the same
-- sentence in both rules: which declaration a restriction forbids, in what
-- shape, and whether it is gated are all independent, and Blind-Spot Giant
-- prints one gate across two arms.
--
-- Nothing is the UNCONDITIONAL restriction (Pacifism), not a gate that fails.
gate :: CombatRestriction.CombatRestriction -> Maybe Condition.Type.Condition
gate cr = case cr of
  CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless _ c) -> c
  CombatRestriction.CantBlock (AffectedUnless.MkAffectedUnless _ c) -> c
  CombatRestriction.CantBeBlockedBy (CantBeBlockedBy.MkCantBeBlockedBy _ _ c) -> c
  CombatRestriction.CantAttackAlone (AffectedUnless.MkAffectedUnless _ c) -> c
  CombatRestriction.CantAttackMoreThan (LimitUnless.MkLimitUnless _ c) -> c
  CombatRestriction.CantBlockMoreThan (LimitUnless.MkLimitUnless _ c) -> c

-- Every combat restriction some permanent on the battlefield -- or some object in
-- the command zone, whose abilities CR 114.4 makes function there -- states right
-- now, each paired with its SOURCE and with CR 612.1's word swap over that
-- source's own text. A restriction whose gate HOLDS is not here at all, because CR
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
      removedAfter = Projection.abilityRemovalAfter gs
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
      --
      -- CR 508.5: the gate may also name the DEFENDING PLAYER rather than the
      -- source's controller (Armored Galleon, "can't attack unless defending
      -- player controls an Island"), so the combat record's defender is supplied
      -- to Filter.ControlledByDefendingPlayer here. ONE read for the whole
      -- combat rather than one per (creature, attack target) pair: CR 508.5a
      -- determines the defending player individually for each attacking
      -- creature, and the two readings coincide on every board pawl can build,
      -- since Combat.attackTargets is derived from that single defender and
      -- Defender.playerOf answers it on all three arms -- the OfPlayer arm IS
      -- the defender, the OfPlaneswalker arm reads the record, and
      -- Combat.attackableBattles admits only battles that player protects. CR
      -- 802's attack-multiple-players option is what would separate them, and
      -- pawl has no options concept to read it from (#175).
      --
      -- Nothing outside combat, which leaves the atom False (Filter.matches) and
      -- so leaves the restriction in force -- the honest answer, there being no
      -- attack to make and no defending player to name. Filled uniformly across
      -- the arms rather than only on CantAttack: CR 508.5 pins one defending
      -- player per combat, so a block-side gate naming that player would read
      -- the same seat.
      defending = Combat.defender (GameState.combat gs)
      lifted source changes restriction = case gate restriction of
        Nothing -> False
        Just condition ->
          Condition.holds
            (Projection.fullView gs)
            (Filter.contextFor (Projection.controllerOf source gs) (Just source))
              { Filter.defendingPlayer = defending
              }
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
      -- with no CR 509.1b "unless" gate -- and read off Object.designations rather than stamped
      -- when the designation was set, so it ends when the designation does. The
      -- menace half is a characteristic, and lives in
      -- Pawl.Engine.Projection.designationGathered.
      --
      -- Outside `anyMinted`, which asks about keywords, and it takes one half of
      -- `keepsAbilities` below rather than the pair the printed rows take. Rule
      -- 701.60c states the restriction as quoted text, so what the designation
      -- gives the permanent is an ABILITY -- but an ability the RULES grant, which
      -- the two halves of that gate stand differently towards:
      --
      --   * CR 613.1f's layer-6 removal reaches it, in CR 613.7 TIMESTAMP ORDER.
      --     The grant's timestamp is the permanent's own, which is what
      --     Pawl.Engine.Projection.designationGathered stamps the menace half with,
      --     so both halves of rule 701.60c's one sentence are ordered alike: a
      --     Humility already on the battlefield when the permanent arrived is
      --     earlier and leaves both in place, and one that arrived later wipes
      --     both. `keepsAbilities`'s blanket removal question cannot tell the two
      --     apart, which is what CombatSpec's pair of boards proves.
      --   * CR 305.7's layer-4 strip is asked, as `keepsRulesText`. Not
      --     implemented: that rule reaches "all abilities generated from its rules
      --     text" and says outright that it "doesn't remove any abilities that were
      --     granted to the land by other effects", so it should not reach this one
      --     at all -- and the menace half, which no layer-4 strip touches, already
      --     does not (#1606).
      designationRows source = case Game.lookupObject source gs of
        Just obj
          | Set.member Designation.Suspected (Object.designations obj),
            keepsRulesText source,
            not (removedAfter (Object.timestamp obj) source) ->
              [(source, [], CombatRestriction.CantBlock (AffectedUnless.MkAffectedUnless (Affected.Matching Filter.Type.IsSource) Nothing))]
        _ -> []
      -- The two ability losses the printed rows below check for: CR 305.7's
      -- basic-land subtype set, and CR 604.2 against a CR 613.1f layer-6 removal.
      -- Split, because the designation row above asks the first and not the second:
      -- what a layer-6 removal costs a RULES-granted ability is a timestamp
      -- question, and only the printed rows can settle it with a bare bit.
      keepsRulesText source = null setEffs || Projection.liveAfterLayers setEffs source gs
      keepsAbilities source = keepsRulesText source && not (removed source)
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face ->
          designationRows source <> mintedRows source <> case Face.combatRestrictions face of
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
      -- CR 114.4: "abilities of emblems function in the command zone", which is
      -- what makes CR 701.54c's restriction on the emblem named The Ring do
      -- anything at all. Pawl.Engine.Projection.gatherGiven walks the same zone
      -- for the same rule, and this branch takes its posture for the gates: no
      -- liveness gate, no CR 612.1 text change and no minted rows, since the
      -- pool's CR 613.1f removers and text changers reach creatures on the
      -- battlefield and an emblem is not one (CR 114.5). The "unless" gate IS
      -- asked, because a gated emblem restriction would otherwise be wired open --
      -- no emblem in the pool prints one.
      --
      -- EMBLEMS alone, where that walk takes the whole zone: CR 114.4 is about
      -- emblems, and CR 113.6 leaves a permanent card's abilities functioning only
      -- on the battlefield. The difference bites here in a way it does not there,
      -- because two of this module's questions name no creature at all -- a
      -- commander whose card printed Silent Arbiter's bound would otherwise cap
      -- the whole table's attackers from the command zone.
      isEmblem source = case fmap Object.source (Game.lookupObject source gs) of
        Just (Source.OfEmblem _) -> True
        _ -> False
      fromCommandZone source = case Game.faceOf source gs of
        Just face
          | isEmblem source ->
              fmap
                (\restriction -> (source, [], restriction))
                (filter (not . lifted source []) (Face.combatRestrictions face))
        _ -> []
   in concatMap fromPermanent (Set.toList (GameState.battlefield gs))
        <> concatMap fromCommandZone (Set.toList (GameState.command gs))

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

-- CR 509.1b's PAIRWISE restrictions: which (blocker, attacker) pairs an effect in
-- force right now forbids. CR 701.54c's "can't be blocked by creatures with
-- greater power" and Questing Beast's "can't be blocked by creatures with power
-- 2 or less" are the pool's two producers -- one minted by the rulebook, one
-- printed on a card.
--
-- A set of PAIRS, where the three questions above answer with a set of creatures,
-- and the shape is forced by the rule rather than chosen: the restriction is
-- about a blocker RELATIVE TO an attacker, so neither side is a set on its own.
-- Pawl.Engine.Combat tests membership and learns nothing else --
-- Pawl.Types.CombatRestriction's Filter is read here and never handed over.
--
-- Computed ONCE per declaration pass and handed to every pair check, `restricted`'s
-- posture and for its reason: the caller asks this at the top of a pass where a
-- per-pair predicate would walk the battlefield inside
-- Pawl.Engine.Combat.candidateBlockDeclarations' exponential filter (#200). The
-- cross product costs |blockers| * |attackers| projections, and only on a board
-- that actually states such a restriction -- `inForce` yields nothing on every
-- other board, so the fold never reaches a candidate.
--
-- THE FILTER'S CONTEXT IS THE ATTACKER'S: its source is the attacker and its
-- perspective that attacker's controller, the pairing
-- Pawl.Engine.Combat.landwalkAllowsGiven gives a keyword-borne Filter. CR 109.5's
-- "you" would instead be the SOURCE's controller, as it is for the gate in
-- `inForce` and the affected set in `restricted`, and the two readings are
-- indistinguishable on both of the pool's producers: CR 701.54c's affected set
-- carries ControlledBy You, so the Ring-bearer's controller IS the emblem's, and
-- Questing Beast's carries IsSource, so the attacker IS the source. What is not
-- a choice is the source POWER, which CR 701.54c compares the blocker against:
-- the emblem has no power at all (CR 114.3), so only the attacker's makes the
-- comparison mean anything.
--
-- The power is read off the full projection (CR 613), as skulk's is in
-- Pawl.Engine.Combat.skulkAllowsGiven, and read at the moment the declaration is
-- checked -- CR 509.1b's second paragraph is what says nothing re-checks it after.
cantBeBlockedBy :: [ObjectId] -> [ObjectId] -> GameState -> Set (ObjectId, ObjectId)
cantBeBlockedBy blockers attackers gs =
  let named source affected creature =
        Projection.affects
          source
          creature
          affected
          (Projection.project creature gs)
          gs
      -- CR 612.1, on both of the printed sentence's halves: the attackers it
      -- names and the blockers it describes are words on the SOURCE's card, so
      -- one text change reaches both or neither -- `restricted`'s rewrite with
      -- the second position added.
      fromRestriction (source, changes, restriction) = case blockedBy restriction of
        Nothing -> []
        Just (affected, criterion) ->
          let subject = if null changes then affected else Projection.rewriteAffected changes affected
              wanted = if null changes then criterion else Filter.rewrite changes criterion
              -- Lazy in the power, which a filter naming no source-power atom
              -- never forces.
              context attacker = Filter.contextComparingPower (Projection.controllerOf attacker gs) attacker (Projection.powerOf attacker gs)
              matched attacker blocker = Filter.matches (context attacker) (Projection.viewOfObject blocker gs) wanted
              barred attacker = fmap (\blocker -> (blocker, attacker)) (filter (matched attacker) blockers)
           in concatMap barred (filter (named source subject) attackers)
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
