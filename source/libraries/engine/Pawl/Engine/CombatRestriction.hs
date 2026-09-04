-- CR 508.1c / 509.1b / 613.11: the continuous effects that FORBID an attack or
-- a block. One of the modules on the axis CR 613.11 reaches past the layer
-- system (alongside Pawl.Engine.PlayerEffect, Pawl.Engine.BlockRequirement and
-- Pawl.Engine.AttackRequirement). None is a layer, and Pawl.Engine.Projection
-- sees none of them.
--
-- Not all of them are card data: CR 701.35a's detain forbids both declarations
-- and is read off the victim (Pawl.Engine.Detain) rather than off any card's
-- printed text, and CR 508.1c's "can't attack this turn" and CR 509.1b's "can't
-- block this turn" are stored by the resolution that said them
-- (GameState.attackProhibitions, GameState.blockProhibitions). See `detained`,
-- `attackProhibited` and `blockProhibited`.
--
-- The only module that may CASE on Pawl.Types.CombatRestriction.
-- Pawl.Engine.Keyword constructs one -- rule 702.98a's unleash -- and reads none.
-- Pawl.Engine.Combat asks for a SET OF IDS, for a SET OF ROWS, or for a NUMBER,
-- and never learns which card, or which keyword, produced any of them. The rows
-- are the two pairwise restrictions, which no set of creatures could state: CR
-- 509.1b's (cantBeBlockedBy), a pair, and CR 508.1c's aimed-at one
-- (cantAttackPlayer), a triple carrying CR 506.3's kind beside the seat. The
-- Filter that decides the first and the Pawl.Types.PlayerScope that decides the
-- second are both resolved here, so neither crosses into Pawl.Engine.Combat.
module Pawl.Engine.CombatRestriction where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Defender as Defender
import qualified Pawl.Engine.Detain as Detain
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Vanguard as Vanguard
import qualified Pawl.Types.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Pawl.Types.ActiveBlockProhibition as ActiveBlockProhibition
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedUnless as AffectedUnless
import qualified Pawl.Types.AimedAt as AimedAt
import qualified Pawl.Types.AttackTargetKind as AttackTargetKind
import qualified Pawl.Types.CantAttackPlayer as CantAttackPlayer
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LimitUnless as LimitUnless
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype

-- CR 508.1c: which of `candidates` an effect in force right now says CAN'T
-- ATTACK AT ALL. Pacifism's first half, Blind-Spot Giant's when its gate is shut,
-- CR 701.35a's first clause, and Netter en-Dal's stored restriction.
--
-- The printed half is an INTERSECTION over the defending players, not a union,
-- and CR 802.3a is why: a restriction gated on the defending player (Armored
-- Galleon) applies only to the creatures attacking THAT player, so a creature it
-- reaches at one seat and not another is still able to attack -- it just cannot
-- be announced at the seat that binds. `cantAttackDefender` is that per-seat
-- residue, and Pawl.Engine.Combat.attackTargetAllowed is what reads it. A
-- restriction with no such gate reads the same at every seat, so it survives the
-- intersection and lands here as it always did.
--
-- One defending player -- every two-player game, and every multiplayer one CR
-- 507.1 rather than CR 802.2 applies to -- makes the intersection a singleton
-- reading, so nothing about those boards changed.
--
-- Detain and a stored prohibition are outside the intersection because they name
-- no defending player at all (see `detained` and `attackProhibited`): rule 701.35a
-- forbids the declaration outright.
cantAttack :: [ObjectId] -> GameState -> Set ObjectId
cantAttack candidates gs =
  let seats = case Defender.defendingPlayers gs of
        -- No defending player: the one reading `lifted` calls Nothing, which is
        -- the answer outside combat and the answer inside one nobody defends.
        [] -> [Nothing]
        players -> fmap Just players
      barredAt seat = restricted attacking seat candidates gs
   in Set.unions
        [ case fmap barredAt seats of
            -- Unreachable: `seats` is a non-empty list by the case above. Total
            -- rather than a foldr1, since an empty one would mean no restriction
            -- reaches any creature.
            [] -> Set.empty
            first : rest -> List.foldl' Set.intersection first rest,
          detained candidates gs,
          attackProhibited candidates gs
        ]

-- CR 802.3a: which (creature, defending player) pairs a CR 508.1c restriction
-- forbids -- the per-seat residue `cantAttack`'s intersection drops. Armored
-- Galleon's "can't attack unless defending player controls an Island" is the
-- pool's producer, and it is observable only where several players defend at
-- once (CR 802.2).
--
-- ONE gather for every seat, which is what `gathered` and `lifted` are split for:
-- the battlefield walk is taken once and only the gate is re-read per player.
--
-- Pawl.Engine.Combat.attackTargetAllowed maps an announcement to the player whose
-- entry it must consult (CR 508.5's defending player), so a planeswalker's
-- controller answers for an attack on their planeswalker. Nothing of the source
-- survives into the answer, `cantBeBlockedBy`'s posture.
cantAttackDefender :: [ObjectId] -> [PlayerId] -> GameState -> Set (ObjectId, PlayerId)
cantAttackDefender candidates players gs =
  let rows = gathered gs
      forSeat player =
        Set.map
          (\oid -> (oid, player))
          (restrictedIn (filter (not . lifted (Just player) gs) rows) attacking candidates gs)
   in Set.unions (fmap forSeat players)

-- CR 509.1b: which of `candidates` an effect in force right now says CAN'T
-- BLOCK. Pacifism's second half, Blind-Spot Giant's when its gate is shut, CR
-- 701.35a's second clause, and Zirda, the Dawnwaker's stored restriction.
--
-- The DEFENDING PLAYER is the one declaring blocks, which CR 509.1a names, so a
-- gate about them (CR 508.5) is read at that seat rather than at the first
-- defending player in turn order -- the caller has it in hand and hands it over.
--
-- No board proves that seat: nothing in data/cards gates a BLOCKING restriction
-- on the defending player, Armored Galleon -- the pool's only card writing
-- Filter.ControlledByDefendingPlayer, and CardSpec's "CR 508.5 no card writes
-- ControlledByDefendingPlayer outside a combat restriction" is the lint -- being
-- an attacking one. A regression fence rather than a proven behaviour, and the
-- mutation that swaps this seat for the old one leaves the suite green.
cantBlock :: Maybe PlayerId -> [ObjectId] -> GameState -> Set ObjectId
cantBlock defending candidates gs =
  Set.unions
    [ restricted blocking defending candidates gs,
      detained candidates gs,
      blockProhibited candidates gs
    ]

-- CR 508.1c / 611.1: the candidates a stored, resolution-generated restriction
-- forbids attacking. Netter en-Dal's "target creature can't attack this
-- turn".
--
-- blockProhibited's twin one rule over, sharing every argument that function's
-- comment makes and one more the CR states on both sides: CR 508.1d's
-- maximization counts requirements obeyed "without disobeying any restrictions",
-- and Pawl.Engine.Combat.attackCeilingGiven gets that for free because its
-- candidate list is Pawl.Engine.Combat.legalAttackers, already narrowed by this
-- set. A creature under a stored prohibition therefore excuses a Curse of the
-- Nightly Hunt requirement rather than deadlocking the declaration. CR 509.1c is
-- that sentence's mirror and blockProhibited reaches it the same way:
-- Pawl.Engine.Combat.blockCeilingGiven's candidates are
-- Pawl.Engine.Combat.legalBlockersGiven, narrowed through `cantBlock`, and
-- Pawl.Engine.BlockRequirement.instances mints against that narrowed list.
-- Proved on both sides by Pawl.CombatSpec's StoredAttackRestriction and
-- StoredBlockRestriction groups.
--
-- No CR 508.1c "unless" gate, for blockProhibited's reason: every gate in the
-- pool is printed beside the restriction it gates, and Pawl.Types.ForbidAttack
-- states none.
--
-- Only the rows naming no AimedAt: a row that does is CR 802.3a's per-seat
-- residue, and `cantAttackPlayer` is where it lands.
attackProhibited :: [ObjectId] -> GameState -> Set ObjectId
attackProhibited candidates gs =
  let unaimed = filter (Maybe.isNothing . ActiveAttackProhibition.aimedAt) (GameState.attackProhibitions gs)
   in Set.fromList (concatMap (storedSubjects candidates gs) unaimed)

-- Which of `candidates` a stored attack restriction covers, its
-- Pawl.Types.RestrictedCreatures read the way CR 611.2c says: a Named row is the
-- id the resolution froze, and a Matching row is a class re-read against the
-- LIVE board at each declaration, so a creature that entered after the effect
-- began is inside it. Pawl.CombatSpec's StoredClassAttackRestriction group is the
-- proof.
--
-- CR 109.5's "you" in the Filter is the STORED controller, not the source's
-- projected one: the source is a sorcery in a graveyard by now, with no
-- controller to project (Pawl.Types.ActivePlayerEffect's argument).
storedSubjects :: [ObjectId] -> GameState -> ActiveAttackProhibition.ActiveAttackProhibition -> [ObjectId]
storedSubjects candidates gs active = case ActiveAttackProhibition.affected active of
  RestrictedCreatures.Named oid -> filter (== oid) candidates
  RestrictedCreatures.Matching f ->
    let context = Filter.contextFor (Game.teams gs) (Just (ActiveAttackProhibition.controller active)) (Just (ActiveAttackProhibition.source active))
     in filter (\oid -> Filter.matches context (Projection.viewOfObject oid gs) f) candidates

-- CR 509.1b / 611.1: the candidates a stored, resolution-generated restriction
-- forbids blocking. Zirda, the Dawnwaker's "target creature can't block this
-- turn".
--
-- UNIONED in here rather than added to `inForce`, for `detained`'s reason and
-- with one more of its own. A row here has outlived its source (CR 611.2a), so
-- it opts out of the liveness checks, the CR 612.1 word swap and the CR 305.7 /
-- CR 613.1f ability gates that every printed row passes -- and it names no
-- Pawl.Types.Affected either, because the ref was read once at resolution and
-- Pawl.Engine.Resolve stored the ids it named. The extra reason is CR 509.1b's
-- "unless": every gate in the pool is printed beside the restriction it gates,
-- and Pawl.Types.ForbidBlock states none, so there is nothing here to ask.
--
-- Outside the layer system, which is CR 613.11 -- a restriction on a declaration
-- modifies the rules rather than an object's characteristics, so no
-- Pawl.Types.Modification could have carried it and Pawl.Engine.Projection sees
-- nothing of it.
blockProhibited :: [ObjectId] -> GameState -> Set ObjectId
blockProhibited candidates gs =
  let stopped = Set.fromList (fmap ActiveBlockProhibition.object (GameState.blockProhibitions gs))
   in Set.intersection (Set.fromList candidates) stopped

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
cantAttackAlone candidates gs = restricted attackingAlone (defendingSeat gs) candidates gs

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
attackLimit gs = bounded attackingMoreThan (defendingSeat gs) gs

-- CR 509.1b, the blocking counterpart. Silent Arbiter's second sentence.
blockLimit :: Maybe PlayerId -> GameState -> Maybe Natural
blockLimit = bounded blockingMoreThan

-- The selectors, written out rather than a wildcard: an exhaustive case is what
-- makes a new arm a compile error at every site that would have to decide about
-- it.
attacking :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
attacking cr = case cr of
  CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless a _) -> Just a
  CombatRestriction.CantBlock {} -> Nothing
  CombatRestriction.CantBeBlockedBy {} -> Nothing
  CombatRestriction.CantAttackPlayer {} -> Nothing
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
  CombatRestriction.CantAttackPlayer {} -> Nothing
  CombatRestriction.CantAttackAlone {} -> Nothing
  CombatRestriction.CantAttackMoreThan {} -> Nothing
  CombatRestriction.CantBlockMoreThan {} -> Nothing

attackingAlone :: CombatRestriction.CombatRestriction -> Maybe Affected.Affected
attackingAlone cr = case cr of
  CombatRestriction.CantAttack {} -> Nothing
  CombatRestriction.CantBlock {} -> Nothing
  CombatRestriction.CantBeBlockedBy {} -> Nothing
  CombatRestriction.CantAttackPlayer {} -> Nothing
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
  CombatRestriction.CantAttackPlayer {} -> Nothing
  CombatRestriction.CantAttackAlone {} -> Nothing
  CombatRestriction.CantAttackMoreThan {} -> Nothing
  CombatRestriction.CantBlockMoreThan {} -> Nothing

-- The other PAIRWISE selector, one declaration over. Its own for `blockedBy`'s
-- reason: what it answers is an Affected TOGETHER WITH the players those
-- creatures may not be announced against, and an Affected alone would say "this
-- creature can't attack" full stop. CR 506.3's kinds ride along for the same
-- reason: which announcements at that seat are barred is the rest of the
-- sentence, not a second restriction.
attackingPlayer :: CombatRestriction.CombatRestriction -> Maybe (Affected.Affected, PlayerScope.PlayerScope, Set AttackTargetKind.AttackTargetKind)
attackingPlayer cr = case cr of
  CombatRestriction.CantAttack {} -> Nothing
  CombatRestriction.CantBlock {} -> Nothing
  CombatRestriction.CantBeBlockedBy {} -> Nothing
  CombatRestriction.CantAttackPlayer (CantAttackPlayer.MkCantAttackPlayer a scope kinds _) -> Just (a, scope, kinds)
  CombatRestriction.CantAttackAlone {} -> Nothing
  CombatRestriction.CantAttackMoreThan {} -> Nothing
  CombatRestriction.CantBlockMoreThan {} -> Nothing

-- The bound selectors. A separate pair from the five above rather than a sixth
-- and seventh of them, because what they answer is a NUMBER and no Affected can
-- stand in for one -- which is the whole reason the arms exist.
attackingMoreThan :: CombatRestriction.CombatRestriction -> Maybe Natural
attackingMoreThan cr = case cr of
  CombatRestriction.CantAttack {} -> Nothing
  CombatRestriction.CantBlock {} -> Nothing
  CombatRestriction.CantBeBlockedBy {} -> Nothing
  CombatRestriction.CantAttackPlayer {} -> Nothing
  CombatRestriction.CantAttackAlone {} -> Nothing
  CombatRestriction.CantAttackMoreThan (LimitUnless.MkLimitUnless n _) -> Just n
  CombatRestriction.CantBlockMoreThan {} -> Nothing

blockingMoreThan :: CombatRestriction.CombatRestriction -> Maybe Natural
blockingMoreThan cr = case cr of
  CombatRestriction.CantAttack {} -> Nothing
  CombatRestriction.CantBlock {} -> Nothing
  CombatRestriction.CantBeBlockedBy {} -> Nothing
  CombatRestriction.CantAttackPlayer {} -> Nothing
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
  CombatRestriction.CantAttackPlayer (CantAttackPlayer.MkCantAttackPlayer _ _ _ c) -> c
  CombatRestriction.CantAttackAlone (AffectedUnless.MkAffectedUnless _ c) -> c
  CombatRestriction.CantAttackMoreThan (LimitUnless.MkLimitUnless _ c) -> c
  CombatRestriction.CantBlockMoreThan (LimitUnless.MkLimitUnless _ c) -> c

-- Every combat restriction some permanent on the battlefield -- or some object in
-- the command zone, whose abilities CR 114.4 makes function there -- states right
-- now, each paired with its SOURCE and with CR 612.1's word swap over that
-- source's own text. CR 508.1c / CR 509.1b's "unless" gate is NOT applied here:
-- `inForce` applies it, and every reader goes through that.
--
-- The split is CR 802.3a's. A gate naming the defending player has a different
-- answer per seat, so the same board has to be read once per defending player --
-- and the walk this function is (every permanent, its projection, its text
-- changes) is the expensive half, where the gate is a handful of conditions. One
-- gather, one gate reading per seat.
--
-- The shared half of `restricted` and `bounded`, which is where the liveness
-- checks and the text change belong: both are properties of the SOURCE and of the
-- printed sentence, and neither depends on whether the reader wants a set of
-- creatures or a number.
gathered :: GameState -> [(ObjectId, [(Subtype.Subtype, Subtype.Subtype)], CombatRestriction.CombatRestriction)]
gathered gs =
  let -- Hoisted out of the walk as AttackRequirement.instances hoists them, and
      -- both unforced until some permanent actually declares a restriction.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      removedAfter = Projection.abilityRemovalAfter gs
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
      -- Through `inForce`'s "unless" gate like every printed row, unleash's own
      -- being ungated: the gate is applied to the finished list rather than here,
      -- so a minted restriction that IS gated cannot slip past CR 508.1c's second
      -- clause either.
      mintedRows source =
        [ (source, [], restriction)
        | anyMinted,
          restriction <- Keyword.mintedCombatRestrictionsOf (Projection.keywordsOf source gs)
        ]
      -- The short-circuit Pawl.Engine.Projection.replacementsAffecting takes, for
      -- its reason: projecting every permanent on every declaration would cost
      -- every board a walk that almost no board needs. One shared thunk, so the
      -- battlefield is scanned once per read rather than once per permanent.
      --
      -- Reading COPIABLE values is enough for the permanents -- a keyword reaches
      -- a permanent either from its own copiable rules text or from a grant --
      -- and both halves of that read go through Pawl.Engine.Projection rather
      -- than the face, so a copy's own text and its granting text are both seen
      -- (CR 707.2a). The grants that stand nowhere on the battlefield are the
      -- other two disjuncts, mirroring Projection.gatherGiven's remaining arms as
      -- replacementsAffecting's gate does: a stored continuous effect (CR
      -- 611.2a), and a static ability functioning from the command zone, the
      -- stack, a graveyard, a hand or a library (CR 114.4, CR 113.6).
      -- Pawl.ProjectionSpec's three "still bars the ... blocker" boards are one
      -- each.
      --
      -- CR 122.1b's keyword counter is the fourth road a minting keyword takes
      -- onto a permanent, and it is on no base face and in neither of the two
      -- other-zone disjuncts: decayed is on rule 122.1b's list and mints rule
      -- 702.147a's "can't block". Pawl.CombatSpec's "CR 122.1b two decayed
      -- counters, announced as X, stop both creatures blocking" proves it,
      -- through Rot-Curse Rakshasa. The twin gate in Pawl.Engine.Projection needs no such
      -- read: rule 122.1b's list and Keyword.mintsReplacement's set are disjoint.
      anyMinted =
        any baseCouldMint (Set.toList (GameState.battlefield gs))
          || Projection.storedWrites minting gs
          || Projection.elsewhereGrants minting gs
      minting = Projection.grantsKeywordWhere Keyword.mintsCombatRestriction
      baseCouldMint oid =
        any (any (Projection.grantsKeywordWhere Keyword.mintsCombatRestriction) . StaticAbility.modifications) (Projection.staticAbilitiesOf oid gs)
          || Projection.anyCopiableKeyword Keyword.mintsCombatRestriction oid gs
          || any countedKeywordMints (Map.keys (Projection.countersOf oid gs))
      -- CR 122.1b: the keyword a keyword counter grants, asked of the counters
      -- themselves rather than of the projection that turns them into layer-6
      -- grants -- this is the SHORT-CIRCUIT, and projecting the permanent is the
      -- work it exists to skip. Exhaustive rather than a wildcard, so a later
      -- kind that grants a keyword fails to compile here instead of silently
      -- granting nothing.
      countedKeywordMints kind = case kind of
        CounterKind.Keyword kw -> Keyword.mintsCombatRestriction kw
        CounterKind.PlusOnePlusOne -> False
        CounterKind.MinusOneMinusOne -> False
        CounterKind.Loyalty -> False
        CounterKind.Lore -> False
        CounterKind.Defense -> False
        CounterKind.Time -> False
        CounterKind.Age -> False
        CounterKind.Fade -> False
        CounterKind.Shield -> False
        CounterKind.Finality -> False
        CounterKind.Stun -> False
        CounterKind.Level -> False
        CounterKind.Hone -> False
        CounterKind.Named _ -> False
      -- CR 701.60c: "a suspected permanent has menace and 'This creature can't
      -- block' for as long as it's suspected". Decayed's row (CR 702.147a) with
      -- the keyword swapped for the designation -- aimed at the source alone,
      -- with no CR 509.1b "unless" gate -- and read off Object.designations rather than stamped
      -- when the designation was set, so it ends when the designation does. The
      -- menace half is a characteristic, and lives in
      -- Pawl.Engine.Projection.designationGathered.
      --
      -- Outside `anyMinted`, which asks about keywords, and it takes NEITHER half
      -- of `keepsAbilities` below, where the printed rows take both. Rule 701.60c
      -- states the restriction as quoted text, so what the designation gives the
      -- permanent is an ABILITY -- but an ability the RULES grant rather than one
      -- generated from the permanent's rules text, which the two halves of that
      -- gate stand differently towards:
      --
      --   * CR 613.1f's layer-6 removal reaches it, in CR 613.7 TIMESTAMP ORDER.
      --     The grant's timestamp is the permanent's own, which is what
      --     Pawl.Engine.Projection.designationGathered stamps the menace half with,
      --     so both halves of rule 701.60c's one sentence are ordered alike: a
      --     Humility already on the battlefield when the permanent arrived is
      --     earlier and leaves both in place, and one that arrived later wipes
      --     both. `keepsAbilities`'s blanket removal question cannot tell the two
      --     apart, which is why this row asks `removedAfter` instead -- and what
      --     CombatSpec's pair of boards proves.
      --   * CR 305.7's layer-4 strip is NOT asked, unlike the printed rows below.
      --     That rule reaches "all abilities generated from its rules text" and
      --     then says outright that it "doesn't remove any abilities that were
      --     granted to the land by other effects" -- this one is granted by rule
      --     701.60c, so the strip spares it, and the menace half, which no layer-4
      --     strip touches, is spared alike. Proved by CombatSpec's "CR 305.7
      --     setting a suspected land's subtype spares the ability rule 701.60c
      --     granted", on a Convincing Mirage'd Dryad Arbor.
      designationRows source = case Game.lookupObject source gs of
        Just obj
          | Set.member Designation.Suspected (Object.designations obj),
            not (removedAfter (Object.timestamp obj) source) ->
              [(source, [], CombatRestriction.CantBlock (AffectedUnless.MkAffectedUnless (Affected.Matching Filter.Type.IsSource) Nothing))]
        _ -> []
      -- The two ability losses the printed rows below check for: CR 305.7's
      -- basic-land subtype set, and CR 604.2 against a CR 613.1f layer-6 removal.
      -- Split, because the designation row above asks NEITHER -- rule 701.60c's
      -- grant is not generated from rules text, and what a layer-6 removal costs a
      -- RULES-granted ability is a timestamp question that only the printed rows
      -- can settle with a bare bit.
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
                   in fmap (\restriction -> (source, changes, restriction)) restrictions
                else []
      -- CR 114.4: "abilities of emblems function in the command zone", which is
      -- what makes CR 701.54c's restriction on the emblem named The Ring do
      -- anything at all. Pawl.Engine.Projection.gatherGiven walks the same zone
      -- for the same rule, and this branch takes its posture for the gates: no
      -- liveness gate, no CR 612.1 text change and no minted rows, since the
      -- pool's CR 613.1f removers and text changers reach creatures on the
      -- battlefield and neither an emblem (CR 114.5) nor a vanguard card (CR 313.2)
      -- is one. The "unless" gate IS asked, because a gated restriction from this
      -- zone would otherwise be wired open -- nothing in the pool that functions
      -- here prints one.
      --
      -- CR 902.7 puts a face-up VANGUARD card's rows here on the same terms, so
      -- the test is Vanguard.functionsFromCommandZone -- CR 113.6p's own list --
      -- and Pawl.VanguardSpec's "CR 902.7 a vanguard's combat restriction
      -- functions from the command zone" is what proves it.
      --
      -- Rule 113.6p's list and not the whole zone: CR 113.6 leaves a permanent
      -- card's abilities functioning only on the battlefield. The difference bites
      -- here in a way it does not there, because two of this module's questions
      -- name no creature at all -- a commander whose card printed Silent Arbiter's
      -- bound would otherwise cap the whole table's attackers from the command
      -- zone.
      fromCommandZone source = case Game.faceOf source gs of
        Just face
          | Vanguard.functionsFromCommandZone source gs ->
              fmap (\restriction -> (source, [], restriction)) (Face.combatRestrictions face)
        _ -> []
   in concatMap fromPermanent (Set.toList (GameState.battlefield gs))
        <> concatMap fromCommandZone (Set.toList (GameState.command gs))

-- CR 508.1c / CR 509.1b's second clause, asked of ONE row against ONE defending
-- player. A gate that HOLDS lifts the restriction, so `inForce` drops it; one
-- that does not leaves it in force, which is why an ungated restriction is False
-- here.
--
-- Evaluated once per RESTRICTION and not per candidate, because the clause
-- belongs to the ability rather than to the creature it names: CR 109.5 fixes the
-- "you" inside it as the SOURCE's controller, and Filter.IsSource names the
-- source -- which is what makes Blind-Spot Giant's "another Giant" exclude the
-- Giant printing the sentence.
--
-- Projection.fullView, matching the affected set `restricted` reads (CR 613.11).
-- The source is on the battlefield or in the command zone by construction, so no
-- CR 608.2h last known information is in play.
--
-- CR 612.1: the gate is REWRITTEN before it is asked, so a hacked Glacial Crasher
-- ("can't attack unless there is a Mountain on the battlefield") counts Islands.
-- The clause is words printed on the source's card, which is what CR 612.1
-- reaches, and rewriteCondition is the same descent gatherStatic applies to a
-- static ability's CR 604.2 "as long as" gate.
--
-- CR 508.5: the gate may name the DEFENDING PLAYER rather than the source's
-- controller (Armored Galleon, "can't attack unless defending player controls an
-- Island"), and CR 508.5a says that is ONE specific defending player. Which one
-- is the caller's to say, and CR 802.3a is why it cannot be settled here: the
-- answer belongs to an attack target rather than to the combat.
--
-- Nothing is the reading with NO defending player to name, which leaves the atom
-- False (Filter.matches) and so leaves the restriction in force -- the honest
-- answer outside combat, there being no attack to make.
lifted :: Maybe PlayerId -> GameState -> (ObjectId, [(Subtype.Subtype, Subtype.Subtype)], CombatRestriction.CombatRestriction) -> Bool
lifted defending gs (source, changes, restriction) = case gate restriction of
  Nothing -> False
  Just condition ->
    Condition.holds
      (Projection.fullView gs)
      (Filter.contextFor (Game.teams gs) (Projection.controllerOf source gs) (Just source))
        { Filter.defendingPlayer = defending
        }
      gs
      source
      (if null changes then condition else Projection.rewriteCondition changes condition)

-- The rows `gathered` found whose CR 508.1c / CR 509.1b gate does not lift them,
-- judged against the defending player the caller names. Every reader below goes
-- through this rather than through `gathered`.
inForce :: Maybe PlayerId -> GameState -> [(ObjectId, [(Subtype.Subtype, Subtype.Subtype)], CombatRestriction.CombatRestriction)]
inForce defending gs = filter (not . lifted defending gs) (gathered gs)

-- CR 508.5a's "one specific defending player" where the reader has no attack
-- target to derive one from -- the first defending player in turn order.
--
-- Not implemented: CR 802.3a's split for the two WHOLE-DECLARATION restrictions,
-- `cantAttackAlone` and `attackLimit`. Those are facts about the declaration
-- rather than about one announcement, so there is no attack target to read a
-- defending player off, and a gate on either would be judged at this one seat.
-- Nothing in the pool gates either (#2894).
defendingSeat :: GameState -> Maybe PlayerId
defendingSeat gs = Maybe.listToMaybe (Defender.defendingPlayers gs)

-- The shared walk behind the SUBJECT-CARRYING questions above, over the
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
-- declaration CR 508.1c's Example calls legal. `cantAttackDefender` names
-- creatures that are in some legal declaration too, and for CR 802.3a's reason
-- rather than CR 506.5's: what is refused is one ANNOUNCEMENT.
restricted :: (CombatRestriction.CombatRestriction -> Maybe Affected.Affected) -> Maybe PlayerId -> [ObjectId] -> GameState -> Set ObjectId
restricted select defending candidates gs = restrictedIn (inForce defending gs) select candidates gs

-- `restricted` against rows the caller has already gated, so a reader asking the
-- same board once per defending player pays for one gather rather than one each
-- (cantAttackDefender).
restrictedIn :: [(ObjectId, [(Subtype.Subtype, Subtype.Subtype)], CombatRestriction.CombatRestriction)] -> (CombatRestriction.CombatRestriction -> Maybe Affected.Affected) -> [ObjectId] -> GameState -> Set ObjectId
restrictedIn rows select candidates gs =
  let -- CR 613.11 puts these effects after every layer, so the affected set is
      -- One whole-board projection and one grant walk for the whole walk, both
      -- unforced until some permanent actually reaches `named`.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      -- read against the FULL projection -- the opposite of
      -- Projection.affects's callers inside the layer fold, which read
      -- characteristics as of their own layer.
      named source affected creature =
        Projection.affectsOn
          pcs
          grants
          source
          creature
          affected
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
   in Set.fromList (concatMap fromRestriction rows)

-- CR 509.1b's PAIRWISE restrictions: which (blocker, attacker) pairs an effect in
-- force right now forbids. CR 701.54c's "can't be blocked by creatures with
-- greater power" is the rulebook's producer; Questing Beast's "can't be blocked
-- by creatures with power 2 or less" and Relic Runner's "can't be blocked if
-- you've cast a historic spell this turn" are the printed ones.
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
-- Pawl.Engine.Combat.bestBlockDeclaration's search (#200). The
-- cross product costs |blockers| * |attackers| projections, and only on a board
-- that actually states such a restriction -- `inForce` yields nothing on every
-- other board, so the fold never reaches a candidate.
--
-- THE FILTER'S CONTEXT IS THE ATTACKER'S: its source is the attacker and its
-- perspective that attacker's controller, the pairing
-- Pawl.Engine.Combat.landwalkAllowsGiven gives a keyword-borne Filter. CR 109.5's
-- "you" would instead be the SOURCE's controller, as it is for the gate in
-- `inForce` and the affected set in `restricted`, and the two readings are
-- indistinguishable on every one of the pool's producers: CR 701.54c's affected
-- set carries ControlledBy You, so the Ring-bearer's controller IS the emblem's,
-- and Questing Beast's and Relic Runner's carry IsSource, so the attacker IS the
-- source. What is not
-- a choice is the source POWER, which CR 701.54c compares the blocker against:
-- the emblem has no power at all (CR 114.3), so only the attacker's makes the
-- comparison mean anything.
--
-- The power is read off the full projection (CR 613), as skulk's is in
-- Pawl.Engine.Combat.skulkAllowsGiven, and read at the moment the declaration is
-- checked -- CR 509.1b's second paragraph is what says nothing re-checks it after.
cantBeBlockedBy :: Maybe PlayerId -> [ObjectId] -> [ObjectId] -> GameState -> Set (ObjectId, ObjectId)
cantBeBlockedBy defending blockers attackers gs =
  let -- One whole-board projection and one grant walk for the whole walk, both
      -- unforced until some permanent actually reaches `named`.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      named source affected creature =
        Projection.affectsOn
          pcs
          grants
          source
          creature
          affected
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
              -- CR 702.16k: the carrier of rule 702.16f's minted row IS the
              -- attacker (Affected.Matching Filter.IsSource), so the chosen
              -- player comes off the same object the power does.
              context attacker =
                (Filter.contextComparingPower (Game.teams gs) (Projection.controllerOf attacker gs) attacker (Projection.powerOf attacker gs))
                  { Filter.carrierChosenPlayer = Game.lookupObject attacker gs >>= Object.chosenPlayer
                  }
              matched attacker blocker = Filter.matches (context attacker) (Projection.viewOfObject blocker gs) wanted
              barred attacker = fmap (\blocker -> (blocker, attacker)) (filter (matched attacker) blockers)
           in concatMap barred (filter (named source subject) attackers)
   in Set.fromList (concatMap fromRestriction (inForce defending gs))

-- CR 508.1c through CR 802.3a: which (creature, player, kind) rows an effect in
-- force right now forbids -- the announcements a creature may not make against
-- those players (CR 508.1b). Blazing Archon's "creatures can't attack you" and
-- Vow of Flight's "can't attack you or planeswalkers you control" are the pool's
-- producers.
--
-- CR 506.3's kind is the third component and not a second call, because one
-- printed sentence can name several: the row is the ANNOUNCEMENT it forbids, and
-- Pawl.Engine.Combat.attackTargetKind is what turns an announcement back into
-- one. Nothing of the source or of the printed wording survives into the answer.
--
-- `cantBeBlockedBy`'s shape one declaration over, and a set of TUPLES for its
-- reason: the restriction is about a creature RELATIVE TO the announcement it
-- may not make, so no side is a set on its own. Computed once per declaration
-- pass and handed to every announcement check, and empty on every board stating
-- no such restriction, `inForce` yielding nothing for the fold to reach.
--
-- The SCOPE's perspective is CR 109.5's "you" -- the source's controller -- and
-- not the attacker's, which is the opposite pairing from `cantBeBlockedBy`: that
-- one compares a blocker against the ATTACKER it may not block, where this one
-- names players by their relation to the card printing the sentence. A source
-- whose controller cannot be found names nobody rather than everybody.
--
-- No CR 612.1 word swap on the scope, and none is owed: the swap replaces one
-- word of a printed sentence with another, and a PlayerScope prints no word a
-- text-changing effect reaches. The affected set is rewritten as `restricted`
-- rewrites it, both halves being words on the source's card.
--
-- The STORED rows naming an AimedAt are unioned in here, `attackProhibited`'s
-- posture on the other axis: Chronomantic Escape's "until your next turn,
-- creatures can't attack you" has outlived its source (CR 611.2a), so it opts out
-- of the liveness checks and the CR 612.1 swap every printed row passes, and its
-- scope is read against the controller the resolution stored (CR 109.5). One
-- answer on this axis, so Pawl.Engine.Combat.barredAnnouncements never learns
-- which carrier a row came from.
cantAttackPlayer :: [ObjectId] -> [PlayerId] -> GameState -> Set (ObjectId, PlayerId, AttackTargetKind.AttackTargetKind)
cantAttackPlayer candidates players gs =
  let rows = gathered gs
      stored active = case ActiveAttackProhibition.aimedAt active of
        Nothing -> []
        Just (AimedAt.MkAimedAt scope kinds) ->
          let barred = filter (\pid -> PlayerEffect.inScope pid (ActiveAttackProhibition.controller active) gs scope) players
           in [(creature, pid, kind) | creature <- storedSubjects candidates gs active, pid <- barred, kind <- Set.toList kinds]
      -- One whole-board projection and one grant walk for the whole walk, both
      -- unforced until some permanent actually reaches `named`.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      named source affected creature =
        Projection.affectsOn
          pcs
          grants
          source
          creature
          affected
          gs
      fromRestriction player (source, changes, restriction) = case attackingPlayer restriction of
        Nothing -> []
        Just (affected, scope, kinds) ->
          let subject = if null changes then affected else Projection.rewriteAffected changes affected
              barred = case Projection.controllerOf source gs of
                Nothing -> []
                Just you -> filter (\pid -> PlayerEffect.inScope pid you gs scope) [player]
           in [(creature, pid, kind) | creature <- filter (named source subject) candidates, pid <- barred, kind <- Set.toList kinds]
      -- CR 508.5 through CR 802.3a: this arm's own gate is read at the seat the
      -- row names, which is the player being attacked and so the defending player
      -- of that announcement. One gather, one gate reading per seat,
      -- cantAttackDefender's shape. No printing gates this arm today.
      forSeat player = concatMap (fromRestriction player) (filter (not . lifted (Just player) gs) rows)
   in Set.fromList (concatMap forSeat players <> concatMap stored (GameState.attackProhibitions gs))

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
bounded :: (CombatRestriction.CombatRestriction -> Maybe Natural) -> Maybe PlayerId -> GameState -> Maybe Natural
bounded select defending gs =
  let tighter acc n = Just (maybe n (min n) acc)
   in List.foldl' tighter Nothing (Maybe.mapMaybe (\(_, _, restriction) -> select restriction) (inForce defending gs))
