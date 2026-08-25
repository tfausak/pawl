-- CR 613.10 / 613.11: the continuous effects that affect PLAYERS and the RULES
-- OF THE GAME rather than the characteristics of objects. A sibling TIER to the
-- CR 613 layer system, not a layer in it: CR 613.1 makes the seven layers a
-- machine for computing object characteristics and nothing else, and
-- 613.10/613.11 both apply AFTER that machine has run.
--
-- The dependency is therefore ONE-WAY, and that is what keeps it well founded:
-- this module reads the layer machine's finished answers, while
-- Pawl.Engine.Projection is untouched by this module and never sees these types.
--
-- This module is the ONLY module that may case on Pawl.Types.PlayerEffect and
-- Pawl.Types.PlayerScope -- the standing Pawl.Engine.Resolve has over Effect,
-- Pawl.Engine.Projection over Modification, Pawl.Engine.Event over
-- TriggerCondition and Pawl.Engine.Expiry over Expiry. Every consumer asks a
-- TYPED QUESTION and never sees a constructor.
--
-- Pawl.Types.AffectedPlayers is the one type here that Resolve also cases on,
-- and by that same standing: it is an Effect payload, and only a resolution can
-- answer the slot its Named arm holds. This module reads the BAKED value.
module Pawl.Engine.PlayerEffect where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaFilter as ManaFilter
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.AddActivationCost as AddActivationCost
import qualified Pawl.Types.AddSpellCost as AddSpellCost
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.AppliedReduction as AppliedReduction
import Pawl.Types.CardName (CardName)
import Pawl.Types.CostAdjustments (CostAdjustments)
import qualified Pawl.Types.CostAdjustments as CostAdjustments
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.Face as Face
import Pawl.Types.Filter (Filter)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import qualified Pawl.Types.IncreaseActivationCost as IncreaseActivationCost
import qualified Pawl.Types.IncreaseSpellCost as IncreaseSpellCost
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import Pawl.Types.ManaUnit (ManaUnit)
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerEffect (PlayerEffect)
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.PlayerScope (PlayerScope)
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.ReduceActivationCost as ReduceActivationCost
import qualified Pawl.Types.ReduceSpellCost as ReduceSpellCost
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.SpendManaAsThough as SpendManaAsThough
import Pawl.Types.Subtype (Subtype)
import Pawl.Types.Timestamp (Timestamp)

-- CR 109.5: "you" on an object is its controller, and for a static ability the
-- CURRENT controller. `pid` is the player being asked about; `controller` is the
-- player the scope is anchored to. The argument order is (asked-about, anchor)
-- and the two are never interchangeable.
--
-- The anchor is CR 109.5's "you" for every caller but one: protectedFromTargeting
-- below passes the PROTECTED player, because CR 702.11c's "your opponents" are
-- the opponents of whoever has hexproof rather than of the effect's controller.
-- The parameter is named for the common case.
--
-- The board comes in because one arm's membership is a fact about it rather than
-- about the two players: see PlayerScope.ControllingMostPermanents. Every other
-- arm ignores `gs`, and the leader is computed inside the one arm that asks, so a
-- scope that never names it pays nothing.
inScope :: PlayerId -> PlayerId -> GameState -> PlayerScope -> Bool
inScope pid controller gs scope = case scope of
  PlayerScope.You -> pid == controller
  -- Every other player. Not a two-player shortcut: CR 806.1 has a free-for-all's
  -- players compete as individuals against each other, so every other player is
  -- an opponent by construction, and CR 102.2 says the same for two players.
  --
  -- CR 102.3 is the ONE reading this is wrong for -- in a game between teams a
  -- teammate is not an opponent -- and pawl has no teams to express (#175).
  PlayerScope.Opponents -> pid /= controller
  -- Thalia's ruling: "including your own".
  PlayerScope.EachPlayer -> True
  PlayerScope.ControllingMostPermanents -> permanentLeader gs == Just pid

-- The same question over a CARRIER's affected set rather than over a bare scope:
-- is `pid` one of the players this row applies to? inScope for the Scoped arm,
-- and an equality for the seat CR 601.2c chose and Pawl.Engine.Resolve baked.
--
-- Both carriers are asked through this, and the printed one only ever holds a
-- Scoped: a static ability has no target slot to have named a seat.
applies :: PlayerId -> PlayerId -> GameState -> AffectedPlayers.AffectedPlayers PlayerId -> Bool
applies pid controller gs affected = case affected of
  AffectedPlayers.Scoped scope -> inScope pid controller gs scope
  AffectedPlayers.Named seat -> pid == seat

-- Damping Engine's "a player who controls more permanents than each other
-- player", as the at-most-one player it names. Nothing when the lead is TIED,
-- which is the sentence's own answer: "more than each other player" is a strict
-- comparison, so two players on four permanents each leave the ability affecting
-- nobody at all.
--
-- CR 110.1 is what makes the tally a battlefield fold and nothing more: every
-- object on the battlefield is a permanent, so no filter is owed. The controller
-- is the PROJECTED one (CR 613.1b layer 2), so a Mind Control counts the
-- permanent for its new controller.
--
-- A player controlling nothing still has a row, which is what keeps the tie test
-- honest: on an empty board every player is tied on zero and the answer is
-- Nothing rather than an arbitrary seat.
--
-- One remaining player is VACUOUSLY the leader -- there is no other player to
-- have fewer than -- and that is the rules answer as well as this fold's. CR
-- 104.2a has already ended such a game, so nothing observes it.
permanentLeader :: GameState -> Maybe PlayerId
permanentLeader gs =
  let controls pid = length (filter (\oid -> Projection.controllerOf oid gs == Just pid) (Set.toList (GameState.battlefield gs)))
      tallies = fmap (\pid -> (pid, controls pid)) (Game.stillPlaying gs)
   in case tallies of
        [] -> Nothing
        _ ->
          let best = maximum (fmap snd tallies)
           in case filter ((== best) . snd) tallies of
                [(pid, _)] -> Just pid
                _ -> Nothing

-- The same scope as a SET rather than as a membership test -- CR 400.1's
-- per-player zones asked in the direction a zone fold needs it
-- (Pawl.Engine.Target.graveyardRecipients). Built ON inScope rather than beside
-- it, so there is exactly one reading of what a PlayerScope names.
--
-- CR 102.1: a player who has left keeps their row in GameState.players
-- (Player.status turns Departed, the key stays), so the fold is over
-- Game.stillPlaying rather than the map's keys, and no scope names a departed
-- seat.
--
-- Nothing is an ABSENT perspective, which is CR 109.5's "you" with nobody to be
-- -- the vacuous posture every player-referencing Filter atom takes. EachPlayer
-- is answerable anyway, because it never asks the perspective a question: "target
-- card in a graveyard" names the whole table whoever is reading it.
-- ControllingMostPermanents is answerable for that same reason, one arm further
-- on: its membership is a fact about the board.
playersInScope :: Maybe PlayerId -> GameState -> PlayerScope -> Maybe [PlayerId]
playersInScope perspective gs scope =
  let everyone = Game.stillPlaying gs
      relative = fmap (\you -> filter (\pid -> inScope pid you gs scope) everyone) perspective
   in case scope of
        PlayerScope.You -> relative
        PlayerScope.Opponents -> relative
        PlayerScope.EachPlayer -> Just everyone
        PlayerScope.ControllingMostPermanents -> Just (Maybe.maybeToList (permanentLeader gs))

-- CR 707.2a: the player abilities this permanent's copiable rules text gives it
-- -- its copy snapshot's when it has one, its printed face's otherwise. The
-- Pawl.Engine.Projection.staticAbilitiesOf of this axis, and written here rather
-- than beside it so that Pawl.Engine.Projection goes on never seeing
-- Pawl.Types.PlayerEffect; the one read of Binding.copyOf both share is
-- Projection.copiableSnapshotOf.
playerAbilitiesOf :: ObjectId -> GameState -> [PlayerStaticAbility.PlayerStaticAbility]
playerAbilitiesOf oid gs = case Projection.copiableSnapshotOf oid gs of
  Just snapshot -> PC.playerAbilities snapshot
  Nothing -> foldMap Face.playerAbilities (Game.faceOf oid gs)

-- CR 613.7a: the PRINTED carrier's rows -- printed as opposed to CR 611.2c's
-- stored one, the list itself being the COPIABLE one above -- one per player
-- ability on one battlefield permanent that still has it, as
-- (timestamp, source, controller, scope, effect) -- the shape `applying` below
-- sorts, filters and strips.
--
-- Top-level and shared rather than local to that function, because a second
-- question needs the same walk and needs it read DIFFERENTLY: `applying` drops
-- the rows CR 116.2d's ignore suppresses, while affectedBy below must not (see
-- its comment). Sharing the walk is what keeps CR 604.2's two ability losses and
-- CR 612.1's word swap from being restated in a second place and drifting.
--
-- UNSORTED and UNFILTERED. Both are `applying`'s job, and neither reader may
-- assume the other's.
printedRows :: GameState -> [(Timestamp, Maybe ObjectId, PlayerId, AffectedPlayers.AffectedPlayers PlayerId, PlayerEffect)]
printedRows gs =
  let -- Hoisted out of the walk exactly as Projection.gather hoists it: an
      -- inlined call would recompute the whole game's SetLandSubtype list once
      -- per permanent.
      setEffs = Projection.setLandSubtypeEffects gs
      -- Hoisted for the same reason, and a thunk until a permanent that actually
      -- has a player ability forces it -- so the ordinary board pays nothing for
      -- the CR 604.2 question below.
      removed = Projection.abilityRemoval gs
      fromPermanent oid = case playerAbilitiesOf oid gs of
        -- The overwhelming majority of permanents: no ability, so no
        -- controller projection and no CR 305.7 check is paid for.
        [] -> []
        -- CR 613.7a: a permanent's static ability produces its continuous effect
        -- with the PERMANENT's timestamp, so the row's order comes off the object
        -- rather than being minted here. Looked up INSIDE this branch and not
        -- beside the walk: the `[]` case above has already turned away every
        -- permanent that prints no player ability, so the ordinary board pays for
        -- no lookup at all. Nothing is unreachable rather than a guess -- the
        -- id came off the battlefield -- and is written only because
        -- lookupObject's type is honest about ids that do not.
        abilities -> case (Game.lookupObject oid gs, Projection.controllerOf oid gs) of
          (Nothing, _) -> []
          (_, Nothing) -> []
          (Just object, Just controller) ->
            -- TWO ability losses, exactly the pair Projection.gather asks about
            -- for a permanent's static abilities.
            --
            -- CR 305.7: a land whose subtype has been SET to a basic type
            -- loses its rules-text abilities, this one included (Blood Moon on
            -- Reliquary Tower).
            --
            -- CR 604.2: a static ability's continuous effect is active only
            -- while the permanent remains on the battlefield and has the
            -- ability, so a CR 613.1f layer-6 removal takes this one with it
            -- (Humility on Thalia). CR 613.6's rescue for an effect that has
            -- already STARTED to apply cannot reach it: CR 613.10/613.11 apply
            -- a player effect AFTER the seven layers have run, so it never
            -- started to apply before layer 6 and the cut is unconditional.
            --
            -- That same "after the layers" placement is why the CR 305.7 gate
            -- is liveAfterLayers: the projection is finished here, so the
            -- setter's affected set is read against it rather than against base
            -- characteristics, and a permanent animated into a land at layer 4
            -- is reached (Ashaya on Thalia, under Blood Moon).
            if (null setEffs || Projection.liveAfterLayers setEffs oid gs)
              && not (removed oid)
              then
                -- CR 612.1's word swap over the permanent's own text, computed
                -- HERE rather than hoisted beside setEffs above, exactly as
                -- Pawl.Engine.CombatRestriction.restricted computes it:
                -- textChangesAffecting folds the whole continuous-effect list,
                -- and the empty case above has already turned away every
                -- permanent that prints no player ability, so the fold runs
                -- once per ability-bearing permanent instead of once per
                -- permanent on the battlefield.
                let changes = Projection.textChangesAffecting oid gs
                    readAs = if null changes then id else rewritePlayerEffect changes
                    -- CR 604.2's "as long as" clause, the same gate
                    -- Projection.gatherStatic applies to the object-facing
                    -- carrier, and asked here for the player-facing one.
                    --
                    -- The VIEW is the FINISHED projection, not a bounded one:
                    -- CR 613.10 and CR 613.11 apply a player effect after the
                    -- seven layers have run, so there is no layer to bound
                    -- against -- the answer Projection.abilitiesGiven takes for
                    -- CR 702.178a's max speed gate, and for that same reason.
                    --
                    -- The PERSPECTIVE is the permanent's controller and the
                    -- source is the permanent, so CR 109.5's "you" inside the
                    -- clause is the Class controller rather than the taxed
                    -- player -- which is the whole content of "during YOUR
                    -- turn". The taxed player rides
                    -- PlayerStaticAbility.scope instead and never reaches here.
                    --
                    -- The clause takes the same CR 612.1 word swap the effect
                    -- beside it does, since one ability's two halves cannot
                    -- disagree about what a word means.
                    lives ability = case PlayerStaticAbility.condition ability of
                      Nothing -> True
                      Just c ->
                        Condition.holds
                          (Projection.fullView gs)
                          (Filter.contextFor (Just controller) (Just oid))
                          gs
                          oid
                          (if null changes then c else Projection.rewriteCondition changes c)
                 in fmap (\ability -> (Object.timestamp object, Just oid, controller, AffectedPlayers.Scoped (PlayerStaticAbility.scope ability), readAs (PlayerStaticAbility.effect ability))) (filter lives abilities)
              else []
   in concatMap fromPermanent (Set.toList (GameState.battlefield gs))

-- CR 116.2d's WHO: is `pid` a player whose game the permanent `oid` is changing
-- right now? That is the rule's own reading of who may take the special action
-- to ignore it -- every printed producer offers it to exactly the players its
-- static ability affects, whether that is "any player" over an EachPlayer scope
-- (Leonin Arbiter) or the one player a narrower scope reaches (Damping Engine's
-- "that player").
--
-- Asked over printedRows and NOT over `applying`, and the difference is the whole
-- reason that walk is shared: `applying` has already dropped the rows this
-- player's own ignore suppresses, so asking it would make the offer disappear the
-- moment it was taken. CR 116.2d forbids no repeat -- paying again spends the cost
-- again and changes nothing else -- and Pawl.SpecialActionSpec's answerer relies
-- on the offer standing.
--
-- The SOURCE and not one of its abilities, which is the narrowing
-- Pawl.Types.SpecialAction carries (#1267): one ability of the permanent
-- affecting this player is enough to offer the action, and taking it then
-- suppresses them all.
affectedBy :: PlayerId -> ObjectId -> GameState -> Bool
affectedBy pid oid gs =
  let affected (_, source, controller, scope, _) = source == Just oid && applies pid controller gs scope
   in any affected (printedRows gs)

-- CR 604.2: every player effect applying to `pid` right now. Gathered LIVE from
-- the battlefield on every read and never captured, the same posture
-- Projection.gather takes for staticAbilities -- which is why Rule of Law
-- leaving the battlefield lifts its restriction with nothing to unwind.
--
-- A Scoped set is resolved DYNAMICALLY (see Pawl.Types.PlayerScope): CR 611.2c
-- lets a rules-modifying effect reach objects that were not affected when it
-- began, so no OBJECT set is ever frozen on this axis. A Named seat is fixed, and
-- by CR 601.2c rather than by that rule -- the target was chosen as the spell was
-- cast.
--
-- The (timestamp, source, controller, scope, effect) rows go no further than this
-- module: no consumer sees more than the (source, effect) pair returned here, and
-- printedRows above is read by exactly one other question inside it.
--
-- Returned in TIMESTAMP ORDER, which is what CR 613.10 and CR 613.11 both
-- require. Sorted at this one gather rather than at each consumer, so an ordered
-- reader cannot be written that forgets to sort: the printed carrier's stamp is
-- its permanent's (CR 613.7a) and the stored carrier's is the one CR 613.7b gave
-- it as it began, and the two interleave here rather than the whole battlefield
-- preceding the whole store.
--
-- STABLE, which is what settles the one tie pawl's stamps can produce: two player
-- abilities printed on ONE permanent share that permanent's timestamp, and CR
-- 613.7's "in timestamp order" says nothing about their relative order because no
-- rule can distinguish them -- so they keep the order the card wrote them in. No
-- other tie exists, since Game.freshTimestamp hands out each stamp once.
--
-- The order changes no answer for a cost adjustment, and CR 613.11 is why: it
-- excepts cost effects from timestamp order and defers to CR 601.2f, which
-- Pawl.Engine.Cost.applyAdjustments implements itself by applying every increase
-- before any reduction. Every other consumer here folds order-independently -- a
-- disjunction of prohibitions (CR 101.2) or a sum of grants (CR 305.2) -- which
-- leaves maximumHandSize below as the axis's one ordered fold.
--
-- The SOURCE rides out alongside the effect, and only because CR 601.3a's
-- quality-bearing prohibitions read it: Null Chamber's "the chosen names" are
-- Object.chosenNames on the permanent that printed the ability, the same
-- direction Modification.AddChosenColor reads a colour. Nothing for a stored
-- CR 611.2c effect, which came from a resolved spell or ability and has no
-- permanent behind it -- and no such effect names a source-carried quality.
applying :: PlayerId -> GameState -> [(Maybe ObjectId, PlayerEffect)]
applying pid gs =
  let printed = printedRows gs
      -- CR 611.2c: the stored carrier. Its controller is read off the record and
      -- never re-derived -- see Pawl.Types.ActivePlayerEffect -- while its scope is
      -- resolved live, exactly as the printed carrier's is.
      --
      -- Neither gate above touches it, because it is not an ability for CR 613.1f
      -- to remove: CR 611.2a gives a resolved spell's continuous effect a duration
      -- of its own. Humility cannot take back a Silence that has already resolved.
      --
      -- No CR 612.1 rewrite either, and for the same reason read the other way: a
      -- text-changing effect changes the words printed on an OBJECT, and this
      -- carrier has none behind it. The words a stored effect holds were fixed by
      -- the spell that made it, whose own text a swap reaches while it is still on
      -- the stack (Pawl.Engine.Projection.rewriteEffect).
      storedOne active =
        ( ActivePlayerEffect.timestamp active,
          Nothing,
          ActivePlayerEffect.controller active,
          ActivePlayerEffect.scope active,
          ActivePlayerEffect.effect active
        )
      stored = fmap storedOne (GameState.playerEffects gs)
      -- CR 116.2d: a player who has paid to ignore a permanent's static ability
      -- sees none of that permanent's player abilities. Filtered HERE, at the
      -- one gather every consumer reads through, so the ignore reaches all of
      -- them at once and cannot be forgotten by a later gate.
      --
      -- Only the PRINTED carrier can be ignored, which is exact rather than a
      -- shortcut: CR 116.2d's subject is "effects from static abilities", and a
      -- stored effect came from a resolution instead (Silence). Those arrive
      -- with source Nothing and so match nothing here.
      keep (_, source, controller, scope, _) =
        applies pid controller gs scope
          && not (any (ignores source) (GameState.ignoredAbilities gs))
      ignores source ignored =
        IgnoredAbility.player ignored == pid
          && Just (IgnoredAbility.source ignored) == source
      effectOf (_, source, _, _, effect) = (source, effect)
      stampOf (timestamp, _, _, _, _) = timestamp
   in fmap effectOf (List.sortOn stampOf (filter keep (printed <> stored)))

-- CR 612.1's subtype word swap over a PlayerEffect, the CR 613.10/613.11 axis's
-- answer to Pawl.Engine.Projection.rewriteModification. An Artificial Evolution
-- resolved at an Edgewalker moves "Cleric spells you cast cost {W}{B} less to
-- cast" onto the new word, because the word naming which spells the ability
-- discounts is text printed on the permanent like any other.
--
-- HERE and not beside rewriteModification, which is where the printed-text
-- rewrites for objects live: this module is the only one that may case on
-- PlayerEffect: Pawl.Engine.Projection carries the rows opaquely through
-- ProjectedCharacteristics.playerAbilities so a copy acquires them (CR 707.2a),
-- and never looks inside one. The
-- shape Pawl.Engine.CombatRestriction takes for a restriction -- destructure the
-- module's own type, hand each inner value to the module that owns it -- is the
-- one taken here, with Pawl.Engine.Filter.rewrite doing the descent.
--
-- Exhaustive rather than a catch-all, for rewriteModification's stated reason: a
-- later arm that can hold a word must break this build instead of silently
-- keeping the printed one.
--
-- CR 612.2's family gate is not restated at the Filter descent, for the reason
-- Filter.rewrite's own comment gives: a HasSubtype atom may name a word of any
-- family, so the family the word is used AS is the family it belongs to, and the
-- exact lookup already asks CR 612.2's question. A Magical Hack's land-type pair
-- therefore leaves Edgewalker's Cleric alone.
rewritePlayerEffect :: [(Subtype, Subtype)] -> PlayerEffect -> PlayerEffect
rewritePlayerEffect pairs effect = case effect of
  -- The arms carrying a Filter, which is the only place in this type a subtype
  -- word can hide. Thalia's "noncreature spells", Vedalken Orrery's "spells",
  -- Prowling Serpopard's "creature spells", Heartstone's "activated abilities of
  -- creatures", Damping Engine's "artifact, creature, or enchantment spells",
  -- Oppressive Rays' "enchanted creature" and Yawgmoth's Will's "spells" and
  -- Omniscience's "spells" name none today;
  -- Edgewalker's "Cleric spells" does, and Haakon's "Knight spells" would.
  PlayerEffect.IncreaseSpellCost (IncreaseSpellCost.MkIncreaseSpellCost f n) -> PlayerEffect.IncreaseSpellCost (IncreaseSpellCost.MkIncreaseSpellCost (Filter.rewrite pairs f) n)
  PlayerEffect.IncreaseActivationCost (IncreaseActivationCost.MkIncreaseActivationCost f n) -> PlayerEffect.IncreaseActivationCost (IncreaseActivationCost.MkIncreaseActivationCost (Filter.rewrite pairs f) n)
  PlayerEffect.ReduceSpellCost x -> PlayerEffect.ReduceSpellCost x {ReduceSpellCost.whichSpells = Filter.rewrite pairs (ReduceSpellCost.whichSpells x)}
  PlayerEffect.ReduceActivationCost (ReduceActivationCost.MkReduceActivationCost f family targets cost floor_) -> PlayerEffect.ReduceActivationCost (ReduceActivationCost.MkReduceActivationCost (Filter.rewrite pairs f) family (fmap (Filter.rewrite pairs) targets) cost floor_)
  -- The two arms with a word in TWO places: their own criterion ("nontoken
  -- Rebels"), and the criterion inside each component they add ("sacrifice a
  -- LAND", "sacrifice a SWAMP"). Both descend, which is Filter.rewriteCost's
  -- reading of CR 612.2 carried to a component that is added to a cost rather
  -- than printed in one. The scale beside them names a COLOUR, which CR 612.2's
  -- subtype pairs cannot reach.
  PlayerEffect.AddActivationCost (AddActivationCost.MkAddActivationCost f components scale) -> PlayerEffect.AddActivationCost (AddActivationCost.MkAddActivationCost (Filter.rewrite pairs f) (fmap (Filter.rewriteComponent pairs) components) scale)
  PlayerEffect.AddSpellCost (AddSpellCost.MkAddSpellCost f components scale) -> PlayerEffect.AddSpellCost (AddSpellCost.MkAddSpellCost (Filter.rewrite pairs f) (fmap (Filter.rewriteComponent pairs) components) scale)
  PlayerEffect.CastAsThoughItHadFlash f -> PlayerEffect.CastAsThoughItHadFlash (Filter.rewrite pairs f)
  PlayerEffect.CantBeCountered f -> PlayerEffect.CantBeCountered (Filter.rewrite pairs f)
  PlayerEffect.CantCastMatching f -> PlayerEffect.CantCastMatching (Filter.rewrite pairs f)
  PlayerEffect.CastFromGraveyard f -> PlayerEffect.CastFromGraveyard (Filter.rewrite pairs f)
  PlayerEffect.CastFromHandWithoutPayingManaCost f -> PlayerEffect.CastFromHandWithoutPayingManaCost (Filter.rewrite pairs f)
  -- The rest name no word a subtype pair could reach. The two chosen-name arms
  -- carry nothing at all -- CR 201.4's names are read off the source's
  -- Object.chosenNames -- and CR 612.2's second sentence says a subtype swap
  -- could not touch a card name even if they did. A count, a mana filter and a
  -- player scope are not words either.
  PlayerEffect.CantCastSpells -> effect
  PlayerEffect.CantCastMoreThan _ -> effect
  PlayerEffect.CantCastChosenName -> effect
  PlayerEffect.CantPlayLandChosenName -> effect
  PlayerEffect.PlayAdditionalLands _ -> effect
  PlayerEffect.NoMaximumHandSize -> effect
  PlayerEffect.SetMaximumHandSize _ -> effect
  PlayerEffect.DontLoseUnspentMana _ -> effect
  PlayerEffect.SpendManaAsThough _ -> effect
  PlayerEffect.CantBeTargetedBy _ -> effect
  PlayerEffect.DamageCantBePrevented _ -> effect
  PlayerEffect.CantSearchLibraries -> effect
  PlayerEffect.CantBecomeMonarch -> effect
  PlayerEffect.CastOnlyAtSorcerySpeed -> effect
  PlayerEffect.CantPlayLands -> effect
  PlayerEffect.PlayLandsFromGraveyard -> effect

-- CR 601.2i: how many spells this player has cast this turn. A fold over the
-- whole event log, which is exactly "this turn" because Engine.handoffTurn clears
-- it at the handoff and no reader ever drains it (scannedThrough is a watermark,
-- not a consumption). Rule of Law's ruling demands precisely this: the whole
-- turn, including spells cast before it was on the battlefield.
--
-- A Natural rather than an Integer, because a count of log entries cannot be
-- negative and CR 502.2's reader (GameState.spellsCastLastTurn, snapshotted by
-- Engine.beginTurnOf) holds one.
--
-- Read out of castsPerPlayer below rather than folding the log itself, so the
-- per-seat map Engine.beginTurnOf snapshots and this one seat's count are the
-- same fold.
castsThisTurn :: PlayerId -> GameState -> Natural
castsThisTurn pid gs = Map.findWithDefault 0 pid (castsPerPlayer gs)

-- castsThisTurn asked of every player at once, which is what
-- GameState.castsLastTurn is a snapshot of. ONE fold rather than a second reader
-- of the same log: the scalar above is defined in terms of this, so the per-seat
-- map and the one-seat count cannot disagree.
--
-- SPARSE: a player who cast nothing has no entry, and every reader takes 0 for an
-- absent one.
castsPerPlayer :: GameState -> Map.Map PlayerId Natural
castsPerPlayer gs =
  Map.fromListWith
    (+)
    (fmap (\cast -> (SpellWasCast.player cast, 1)) (Maybe.mapMaybe (Game.castOf . LoggedEvent.event) (Foldable.toList (GameState.events gs))))

-- CR 601.3: a player can begin to cast a spell only if no rule or effect
-- prohibits it. The prohibit half. Cast.permitsCastWhileSearching is not the
-- general allow half of CR 601.3 -- it is only the Panglacial Wurm timing
-- exception, one specific instance of "allows".
--
-- CR 101.2 is why this folds as a DISJUNCTION: a "can't" effect takes precedence
-- over anything allowing or directing. One applicable prohibition is enough and
-- nothing outvotes it.
--
-- Takes the SPELL, as one half NAMED: CR 709.3a evaluates only the chosen half
-- to see if it can be cast, so the name compared is that half's own and a split
-- card is asked this question once per half. Two of the prohibitions are
-- quality-free -- "can't cast spells", "can't cast more than one spell" -- and
-- so ignore both arguments; CR 601.3a's quality-bearing shape is what the rest
-- need them for (Null Chamber's "spells with the chosen names", Damping Engine's
-- "artifact, creature, or enchantment spells").
--
-- ONE name rather than the set the play-side twin below takes, and by RULE
-- rather than for want of one: CR 709.3b leaves a spell on the stack the
-- characteristics of the half being cast alone, and the proposal has already
-- fixed that half. CR 709.4a's "one of its names" therefore has one candidate
-- here, which is why naming "Wax" stops Wax and leaves Wane castable (CR
-- 709.3a).
--
-- BOTH the object and the name, because CR 601.3a's qualities come in two kinds
-- and neither argument answers the other's. A name is compared AS A NAME, and the
-- caller takes it off the chosen face -- the only place it could come from, since
-- the card is still in the zone it is cast from and a face-down proposal
-- carries CR 708.2a's empty name rather than the card's. Any other quality is a
-- Filter over the spell's characteristics, which is a question about the OBJECT
-- and is asked of the proposal's projection through matchesObject, the same
-- direction spellCostAdjustments reads Thalia's tax. `gs` must therefore be
-- Cast.asProposed-stamped for the half being asked about, as it already had to be
-- for the adjustments.
--
-- CR 601.3a's LOOKAHEAD rides on the Filter arm, in choiceCouldEscape below: a
-- prohibition that names the spell as it stands is ignored when a choice still to
-- be made during the proposal could take it out of the class. X is the one such
-- choice pawl has, and Void Winnower against Molten Disaster is what observes it.
--
-- The two choices CR 601.2b names as preceding the announcement -- "choosing to
-- cast a spell with flashback from a graveyard or choosing to cast a creature
-- with morph face down" -- need no search, because each is its own Action here:
-- the offer is per (half, facing) pair and this predicate is asked once per pair
-- with that pair's own name. CR 708.2a's empty name is what a morph cast brings,
-- which is why a Null Chamber naming the card stops its face-up cast and not its
-- face-down one -- Pawl.FaceDownSpec's "CR 708.4 a prohibition naming the card
-- stops the face-up cast and not the morph one".
--
-- A NAME cannot be searched over, and no card asks it to: CR 201.1 fixes a
-- spell's name with the half and the facing, both of which are already chosen
-- here.
prohibitsCasting :: PlayerId -> ObjectId -> CardName -> GameState -> Bool
prohibitsCasting pid oid name gs =
  let cast = castsThisTurn pid gs
      prohibits (source, effect) = case effect of
        PlayerEffect.CantCastSpells -> True
        PlayerEffect.CantCastMoreThan limit -> cast >= limit
        -- CR 601.3a / 614.1c: the quality is the name chosen as the SOURCE
        -- entered, so an ability whose permanent has chosen nothing prohibits
        -- nothing.
        PlayerEffect.CantCastChosenName -> Set.member name (chosenNamesOf source gs)
        -- CR 305.1: playing a land is a special action and never a cast, so the
        -- play-side twin stops nothing here. Pawl.Engine.Action.playableLands is
        -- the gate that reads it.
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost {} -> False
        PlayerEffect.IncreaseActivationCost {} -> False
        PlayerEffect.ReduceSpellCost {} -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.AddActivationCost {} -> False
        PlayerEffect.AddSpellCost {} -> False
        -- CR 305.2 raises how many LANDS may be played, and a land is never
        -- cast (CR 305.1), so this grant reaches nothing here.
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.SpendManaAsThough _ -> False
        -- CR 702.18a / 702.11c restrict TARGETING, not casting: a player with
        -- shroud may cast anything, and Pawl.Engine.Target.targetable is where
        -- the restriction lands (CR 115.4, CR 601.2c).
        PlayerEffect.CantBeTargetedBy _ -> False
        -- CR 601.3b ALLOWS, and this is the prohibit half: a permission is not a
        -- prohibition, and CR 101.2 would let a prohibition outvote it anyway.
        -- mayCastAsThoughItHadFlash below is where it is read.
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        -- CR 701.6a is about a spell or ability ALREADY on the stack, and CR
        -- 601.3 is about beginning to cast one: an uncounterable spell is not a
        -- spell anyone is more or less allowed to cast.
        PlayerEffect.CantBeCountered _ -> False
        -- CR 615.12 edits what CR 615.1's shields do to a damage event. Nobody
        -- is more or less allowed to cast a spell for it.
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
        -- CR 601.3a's other quality shape: a Filter over the spell's own
        -- characteristics rather than over its name, read off the proposal's
        -- projection so Damping Engine's "artifact, creature, or enchantment
        -- spells" reaches the face a morph proposal actually shows.
        --
        -- And CR 601.3a's LOOKAHEAD on top of it, which is a question only a
        -- quality-bearing prohibition can be asked: the spell is in the class
        -- right now, and the player may begin casting it anyway if a choice they
        -- have not yet made could take it out.
        --
        -- The quality can be the ZONE the cast is from (Filter.IsInZone): both
        -- callers ask this before CR 601.2a's move, so the object the view is
        -- taken of still lies where the cast would take it from.
        PlayerEffect.CantCastMatching criterion ->
          matchesObjectFrom source criterion oid gs && not (choiceCouldEscape source criterion oid gs)
        -- CR 307.5 / Teferi, Mage of Zhalfir: outside that rule's moment this
        -- player casts nothing. Turn.sorcerySpeedWindow is CR 307.5's three
        -- conjuncts and the window CR 307.1 already shares, so there is one copy
        -- rather than a fourth that could drift.
        --
        -- HERE and not in Cast.timingOk, and the choice is load-bearing rather
        -- than stylistic. That disjunction is asked by `castable` alone;
        -- `castableWhenOffered` deliberately omits it, because CR 608.2g's cast
        -- and Panglacial Wurm's mid-search cast are the rules' own window being
        -- excepted -- and keeps CR 601.3's PROHIBIT limb, which is the limb this
        -- clause is. A timingOk conjunct would therefore let an opponent's
        -- mid-search Wurm out from under Teferi, which CR 101.2 forbids.
        -- Pawl.CastSpec's "CR 307.5 the offered path: an opponent's mid-search
        -- cast is refused too" is the case that holds it here.
        --
        -- Both call sites ask this BEFORE CR 601.2a moves the card to the stack,
        -- which is what keeps the empty-stack conjunct honest: were it asked
        -- after the move, the spell itself would make the window False and the
        -- clause would prohibit every cast.
        PlayerEffect.CastOnlyAtSorcerySpeed -> not (Turn.sorcerySpeedWindow pid gs)
        -- CR 305.1 again, exactly as CantPlayLandChosenName above: a land is
        -- played and never cast, so the unrestricted play-side prohibition stops
        -- nothing here either.
        PlayerEffect.CantPlayLands -> False
        -- CR 601.3's other half: this arm ALLOWS a cast the rules would refuse,
        -- and no permission prohibits anything. mayCastFromGraveyard below is
        -- where it is read.
        PlayerEffect.CastFromGraveyard _ -> False
        PlayerEffect.PlayLandsFromGraveyard -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
   in any prohibits (applying pid gs)

-- CR 305.1: does any effect prohibit `pid` from PLAYING a land with this name?
-- The play-side twin of prohibitsCasting above, and a separate question rather
-- than a widening of it: CR 305.1 makes playing a land a special action that
-- never uses the stack, so a land is never a spell and none of the cast-side
-- prohibitions reaches it (Silence stops no land).
--
-- Takes the card's NAMES rather than one name, where prohibitsCasting above
-- takes one: nothing has singled out a half here, so what the player is playing
-- is the card as their hand shows it -- CR 709.4's combined view, which has a
-- name per half. CR 709.4a is then a membership test, and a chosen name stops
-- the land if it is ONE of them. No printed land has two, so the set is a
-- singleton in this pool.
--
-- And takes NO object, where prohibitsCasting above does: the two prohibitions
-- read here narrow by a name or by nothing at all, and no printed sentence
-- narrows a land play by a quality a Filter would state ("can't play nonbasic
-- lands"). One that did would want the object, for the reason the cast side wants
-- it.
--
-- A DISJUNCTION for CR 101.2's reason.
prohibitsPlayingLand :: PlayerId -> Set.Set CardName -> GameState -> Bool
prohibitsPlayingLand pid names gs =
  let prohibits (source, effect) = case effect of
        PlayerEffect.CantPlayLandChosenName -> not (Set.disjoint names (chosenNamesOf source gs))
        -- CR 305.1 in the direction CantCastChosenName below takes: Teferi's
        -- clause narrows when this player may CAST, and playing a land is never
        -- casting. CR 305.1's own window happens to be the same three conjuncts,
        -- but Action.legalActions is what asks that of a land play, and an
        -- effect that moved it would have to say so.
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        -- Damping Engine's "can't play lands", which narrows nothing: every land
        -- this player could play is stopped, so the name goes unread.
        PlayerEffect.CantPlayLands -> True
        -- CR 305.1 once more: a permission naming the zone a SPELL may be cast
        -- from stops no land play, which is why Yawgmoth's Will's "you may play
        -- lands ... from your graveyard" is the arm below rather than this one.
        PlayerEffect.CastFromGraveyard _ -> False
        -- And the play-side permission allows rather than prohibits, so it is
        -- False here for the reason every permission is: this question is only
        -- ever "does something stop THIS land". mayPlayLandsFromGraveyard below
        -- is where the grant is read.
        PlayerEffect.PlayLandsFromGraveyard -> False
        -- CR 305.1 again, in the other direction: a prohibition on CASTING says
        -- nothing about a special action, so Silence and Rule of Law leave a
        -- land play alone. CR 305.2's and CR 305.3's limits are the closed
        -- half's and are asked by Action.legalActions -- the first as a count
        -- (landPlaysAllowed below is only its left-hand side), the second as
        -- part of Turn.sorcerySpeedWindow. Neither is a question about WHICH
        -- land, which is all this one asks.
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.IncreaseSpellCost {} -> False
        PlayerEffect.IncreaseActivationCost {} -> False
        PlayerEffect.ReduceSpellCost {} -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.AddActivationCost {} -> False
        PlayerEffect.AddSpellCost {} -> False
        -- CR 305.2 raises HOW MANY lands may be played, never WHICH: a grant is
        -- no permission for a land this rule stops. landPlaysAllowed below is
        -- the gate that reads it.
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        -- CR 305.1 again: a land is never cast, so a permission about the timing
        -- of a CAST has nothing to widen here either.
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        -- CR 305.1 again: a land is never put on the stack, so nothing about
        -- countering reaches a land play.
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
        -- CR 305.1 once more: Damping Engine's own cast half stops no land play,
        -- however its Filter reads -- which is exactly why its one printed
        -- sentence declares two abilities.
        PlayerEffect.CantCastMatching _ -> False
        -- CR 118.9's alternative cost says what a SPELL pays, and a land is
        -- never cast (CR 305.1) -- so this permission neither allows nor
        -- prohibits a land play, whatever its Filter reads.
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
   in any prohibits (applying pid gs)

-- CR 701.23: does any effect prohibit `pid` from searching a library?
--
-- Takes no library, unlike the two prohibitions above taking a name: no printed
-- effect narrows WHICH library (#1269). Asked of the SEARCHER, who need not own
-- the library being read -- an unqualified "can't search" stops them either way.
-- See Pawl.Types.PlayerEffect.CantSearchLibraries.
--
-- A DISJUNCTION for CR 101.2's reason.
prohibitsSearching :: PlayerId -> GameState -> Bool
prohibitsSearching pid gs =
  let prohibits effect = case effect of
        PlayerEffect.CantSearchLibraries -> True
        -- Every other arm is about casting, playing, targeting, countering,
        -- paying or keeping mana. CR 701.23's search is an action a player takes
        -- while FOLLOWING an instruction that has already resolved, so none of
        -- them reaches it -- Silence stops the spell, never the search a
        -- resolved one performs.
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost {} -> False
        PlayerEffect.IncreaseActivationCost {} -> False
        PlayerEffect.ReduceSpellCost {} -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.AddActivationCost {} -> False
        PlayerEffect.AddSpellCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands -> False
        PlayerEffect.CastFromGraveyard _ -> False
        PlayerEffect.PlayLandsFromGraveyard -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
   in any (prohibits . snd) (applying pid gs)

-- CR 725 / 101.2: is `pid` forbidden from becoming the monarch? CR 725.4 asks the
-- question in the rulebook's own words -- "the next player in turn order who can
-- become the monarch" -- and CR 725.1 and CR 725.3 ask it nowhere, which is why
-- the ordinary crowning route needs CR 101.2 to read this at all: those two rules
-- ALLOW a crowning, and a "can't" outranks them.
--
-- Takes no source and no route, unlike the two casting prohibitions above taking
-- a name: the restriction Jared Carthalion prints is on the PLAYER, and rule 725
-- gives the designation no parts for a narrowing to name. See
-- Pawl.Types.PlayerEffect.CantBecomeMonarch.
--
-- A DISJUNCTION for CR 101.2's reason.
prohibitsBecomingMonarch :: PlayerId -> GameState -> Bool
prohibitsBecomingMonarch pid gs =
  let prohibits effect = case effect of
        PlayerEffect.CantBecomeMonarch -> True
        -- Every other arm is about casting, playing, targeting, countering,
        -- searching, paying or keeping mana. CR 725.1's designation is none of
        -- those: it is handed out by a resolving effect or by rule 725.2 itself,
        -- and no prohibition on casting reaches an effect that has already
        -- resolved.
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost {} -> False
        PlayerEffect.IncreaseActivationCost {} -> False
        PlayerEffect.ReduceSpellCost {} -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.AddActivationCost {} -> False
        PlayerEffect.AddSpellCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.SpendManaAsThough _ -> False
        -- CR 702.18a/702.11c stop a SPELL from choosing this player as a target.
        -- CR 725.1's effect need not target to crown them (Palace Jailer's "you
        -- become the monarch" names nobody), so shroud is not an eligibility
        -- restriction; where a card does target (Jared's own first clause), CR
        -- 115.1's own check turns it away before this one is asked.
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands -> False
        PlayerEffect.CastFromGraveyard _ -> False
        PlayerEffect.PlayLandsFromGraveyard -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
   in any (prohibits . snd) (applying pid gs)

-- CR 201.4: the card names chosen for this effect's source, as it entered (CR
-- 614.1c) or while it resolved (CR 608.2c) -- Object.chosenNames. Empty for an
-- effect with no source -- a stored CR 611.2c row -- and for a permanent that
-- chose nothing.
--
-- The empty set is the answer that matches NO object rather than every object,
-- which is the shape CR 201.2a describes for an object with no name: having no
-- name is not sharing one.
chosenNamesOf :: Maybe ObjectId -> GameState -> Set.Set CardName
chosenNamesOf source gs = maybe Set.empty Object.chosenNames (source >>= \oid -> Game.lookupObject oid gs)

-- Does this OBJECT match a player effect's Filter? Shared by the four questions
-- that carry one -- CR 601.2f's cost adjustments in both of their moments, CR
-- 601.3b's timing permission and CR 701.6a's countering prohibition -- so that
-- "which objects does this effect name?" has one reading.
--
-- Named for the OBJECT rather than for a spell, because that is the whole of what
-- it can answer and reading it as "does this spell match" is what made #90 a
-- rules trap: Thalia's `Not (HasCardType Creature)` is true of a noncreature
-- PERMANENT, so an activation cost routed through the spell gathering below would
-- be taxed. WHICH objects an effect is asked about is the constructor's to say
-- (see spellCostAdjustments and activationCostAdjustments), never this function's.
--
-- Evaluated against the PROJECTED view (Projection.viewOfObject) -- a card type
-- is CR 613.1d layer 4 and a colour is CR 613.1e layer 5 -- never a printed
-- characteristic. The perspective is the object's own controller (CR 109.5). Runs
-- through the identity-blind Filter.matches: this module never learns which
-- card produced the Filter.
--
-- A SPELL is what two of the four callers can ever hand it, a battlefield
-- PERMANENT what activationCostAdjustments does, and cantBeCountered reaches CR
-- 113.9's abilities on the stack besides. Nothing here needs a case for any of
-- them: an ability has no card, so the view carries no card type and no colour,
-- and every atom naming a quality is simply false of it while `And []` stays
-- true.
--
-- The SOURCE is the row's own, threaded from `applying` by every caller and
-- never Nothing by default -- which is what makes CR 303.4b's IsHostOfSource
-- answerable here at all. It was a literal Nothing until this function grew the
-- argument, so Oppressive Rays' "activated abilities of ENCHANTED CREATURE"
-- matched nothing whatever the board held; see #1242. A stored CR 611.2c effect
-- legitimately has none: it came from a resolved spell and there is no permanent
-- behind it -- `applying` hands Nothing for every stored row, for the reason its
-- own haddock gives -- so CR 303.4b's atom is simply False there.
--
-- The source's HOST comes off the board here rather than riding in the row,
-- because it is a map lookup with no projection behind it
-- (Pawl.Engine.Filter.sourceAttachedTo says so) and reading it live is what CR
-- 611.2c asks for: an Aura moved to another creature taxes the new one from that
-- moment.
matchesObjectFrom :: Maybe ObjectId -> Filter Keyword -> ObjectId -> GameState -> Bool
matchesObjectFrom src filter_ oid gs =
  Filter.matches (contextFrom src oid gs) (Projection.viewOfObject oid gs) filter_

-- The Context every match in this module is made against: CR 109.5's "you" is
-- the AFFECTED object's own controller, and the source is the row's.
contextFrom :: Maybe ObjectId -> ObjectId -> GameState -> Filter.Context
contextFrom src oid gs =
  (Filter.contextFor (Projection.controllerOf oid gs) src)
    { Filter.sourceAttachedTo = src >>= \s -> Projection.hostOf s gs
    }

-- CR 601.3a's LOOKAHEAD, asked of a prohibition that matches the spell as it
-- stands: could a choice still to be made during this spell's proposal cause the
-- criterion to stop naming it? A True here is the rule's "the player may begin to
-- cast the spell, ignoring the effect", so prohibitsCasting negates it.
--
-- ONE choice can do it in pawl, and X is that choice. CR 202.3e gives a variable
-- a contribution of zero off the stack, so a card's mana value in hand is fixed
-- while the spell's is not, and Void Winnower's "spells with even mana values"
-- lands on opposite sides of the two for an {X}{R}{R} card (Molten Disaster).
-- Every other characteristic a Filter can read is settled before the announcement
-- CR 601.2b governs: a card type, a colour, a supertype and a subtype are all
-- fixed by the half and the facing, and both of the choices CR 601.2b names as
-- preceding the announcement -- flashback from a graveyard, a face-down morph --
-- are their own Action here, so each is asked this question with its own answers
-- rather than searched for.
--
-- The other proposal choices reach nothing: CR 601.2b's modes, CR 601.2b's
-- targets and CR 601.2f's cost payments change no characteristic of the spell
-- this criterion can read.
--
-- Over the PRINTED MANA COST's variables, which is exactly where CR 202.3 reads
-- a mana value from -- an X in an additional cost buys the caster nothing here,
-- because it is not in the mana cost at all. Not implemented: CR 107.3b's clamp,
-- which leaves 0 as the only legal X for a spell cast while paying neither its
-- mana cost nor an alternative cost including X (#1362).
--
-- FINITE by Filter.manaValueThresholds' argument: two steps past the greatest
-- literal the criterion compares against, nothing but parity is left to change,
-- so `climb + 2` samples have seen every verdict the criterion can give. A cost
-- with no variable never gets here at all.
--
-- Asked ONCE, and nothing re-asks it: CR 601.3a lets the player begin "ignoring
-- the effect", so a player who then announces an X that leaves the spell in the
-- prohibited class still casts it.
choiceCouldEscape :: Maybe ObjectId -> Filter Keyword -> ObjectId -> GameState -> Bool
choiceCouldEscape src criterion oid gs =
  let variables = variablesIn oid gs
      -- The SAME context matchesObjectFrom builds, and it has to be: this asks
      -- whether that match could flip, so a context that answered an atom
      -- differently would be asking about a different criterion.
      context = contextFrom src oid gs
      view = Projection.viewOfObject oid gs
      escapes base =
        let limit = 2 + maximum (0 : Filter.manaValueThresholds criterion)
            climb = if limit <= base then 0 else div (limit - base + variables - 1) variables
            reachable = fmap (\x -> base + variables * x) [0 .. climb + 2]
         in any (\mv -> not (Filter.matches context view {Filter.manaValue = Just mv} criterion)) reachable
   in variables > 0 && maybe False escapes (Filter.manaValue view)

-- How many variables the object's mana cost prints -- CR 107.3's {X}, counted
-- rather than tested, because a cost printing the symbol twice moves the mana
-- value in steps of two as X climbs, and a step of two never changes its parity.
--
-- The MANA COST FACE (CR 202.3b), which is the face the mana value itself is read
-- from, so the count and the base it steps from come off the same face.
variablesIn :: ObjectId -> GameState -> Integer
variablesIn oid gs =
  let symbolsOf face = foldMap ManaCost.unwrap (Face.manaCost face)
      variable symbol = symbol == ManaSymbol.Variable
   in toInteger (length (filter variable (foldMap symbolsOf (Game.manaCostFaceOf oid gs))))

-- CR 613.11 / 601.2f: the cost increases, the cost reductions and the additional
-- non-mana components that apply to `pid` CASTING `oid`.
--
-- The SPELL half of CR 601.2f, and the constructors it gathers are the whole of
-- the discriminator this module has: an arm read here is one whose sentence says
-- "spells", so Thalia's tax cannot reach an activation cost however her Filter
-- reads. activationCostAdjustments below is the other half.
--
-- No floor: no printed spell-cost reducer states Heartstone's sentence, so every
-- reduction gathered here is paired with a floor of zero and CR 601.2f's own {0}
-- is the only floor a spell's total has.
--
-- matchesObjectFrom is called only from inside an arm that already matched a
-- cost-modifying constructor, so a board with no Thalia and no Medallion runs no
-- projections at all.
spellCostAdjustments :: PlayerId -> ObjectId -> GameState -> CostAdjustments
spellCostAdjustments pid oid gs =
  let matching :: Maybe ObjectId -> Filter Keyword -> a -> Maybe a
      matching source criterion amount = if matchesObjectFrom source criterion oid gs then Just amount else Nothing
      increaseOf (source, effect) = case effect of
        PlayerEffect.IncreaseSpellCost (IncreaseSpellCost.MkIncreaseSpellCost criterion amount) -> matching source criterion amount
        -- Oppressive Rays, turned away by the CONSTRUCTOR and not by its Filter
        -- -- the mirror of what keeps Thalia off an activation cost in
        -- activationCostAdjustmentsGiven below, and the same #90.
        PlayerEffect.IncreaseActivationCost {} -> Nothing
        PlayerEffect.ReduceSpellCost {} -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.AddActivationCost {} -> Nothing
        PlayerEffect.AddSpellCost {} -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands -> Nothing
        PlayerEffect.CastFromGraveyard _ -> Nothing
        PlayerEffect.PlayLandsFromGraveyard -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
      reductionOf (source, effect) = case effect of
        PlayerEffect.ReduceSpellCost (ReduceSpellCost.MkReduceSpellCost criterion amount coloredOnly) ->
          fmap (\a -> (a, coloredOnly)) (matching source criterion amount)
        PlayerEffect.IncreaseSpellCost {} -> Nothing
        PlayerEffect.IncreaseActivationCost {} -> Nothing
        -- The arms this whole split exists for: an ability's reduction is not a
        -- spell's, and neither is an ability's added component, so both are
        -- gathered by activationCostAdjustments and never here. AddSpellCost is
        -- gathered here, by additionOf below, and never there.
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.AddActivationCost {} -> Nothing
        PlayerEffect.AddSpellCost {} -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands -> Nothing
        PlayerEffect.CastFromGraveyard _ -> Nothing
        PlayerEffect.PlayLandsFromGraveyard -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
      -- CR 601.2f's "plus all additional costs", the non-mana half, reaching a
      -- SPELL: CR 118.8's "or applied to a spell or ability from another effect"
      -- (Drought's "Spells cost an additional \"Sacrifice a Swamp\" to cast").
      -- The sibling of activationCostAdjustments' own additionOf below, and
      -- CONCATENATED for its reason -- two effects each adding a cost both add
      -- it. Each component keeps its effect's SCALE; Pawl.Engine.Cost.plusComponents
      -- is where that is cashed, since only it holds the cost being adjusted.
      additionOf (source, effect) = case effect of
        PlayerEffect.AddSpellCost (AddSpellCost.MkAddSpellCost criterion components scale) ->
          matching source criterion (fmap ((,) scale) components)
        PlayerEffect.IncreaseSpellCost {} -> Nothing
        PlayerEffect.IncreaseActivationCost {} -> Nothing
        PlayerEffect.ReduceSpellCost {} -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.AddActivationCost {} -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands -> Nothing
        PlayerEffect.CastFromGraveyard _ -> Nothing
        PlayerEffect.PlayLandsFromGraveyard -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
      effects = applying pid gs
   in CostAdjustments.MkCostAdjustments
        { CostAdjustments.increases = Maybe.mapMaybe increaseOf effects,
          -- Every spell-cost reduction carries a floor of ZERO, for the reason
          -- the header gives: no printed spell-cost reducer states Heartstone's
          -- sentence. The coloured-mana confinement rides through from the card
          -- (Edgewalker), CR 118.7b-d being the default it overrides.
          CostAdjustments.reductions =
            fmap
              (\(amount, coloredOnly) -> AppliedReduction.MkAppliedReduction amount 0 coloredOnly)
              (Maybe.mapMaybe reductionOf effects),
          -- A spell's own PRINTED additional costs are NOT among these: those are
          -- card text and arrive through Pawl.Engine.Cost.plus at CR 601.2b. What
          -- is gathered here is CR 118.8's other half, a cost applied to the
          -- spell from another effect.
          CostAdjustments.components = concat (Maybe.mapMaybe additionOf effects)
        }

-- CR 613.11 / 601.2f: the cost reductions that apply to `pid` ACTIVATING an
-- ability of `srcId`, with the floor those reductions impose (CR 101.1 card
-- text), plus the additional non-mana components other effects add to that cost
-- (CR 601.2f's "plus all additional costs", reaching an activation cost by CR
-- 602.2b). The ABILITY half of CR 601.2f, and the sibling of
-- spellCostAdjustments above.
--
-- The criterion is matched against `srcId`, the ability's SOURCE PERMANENT, which
-- is what Heartstone's "activated abilities of creatures" narrows -- so the same
-- matchesObject that reads a spell's characteristics for the caster reads a
-- permanent's here, and the permanent is projected rather than printed for the
-- same reason (an animated Vehicle's abilities are a creature's).
--
-- `family` is the SECOND criterion, and the one the source filter cannot say:
-- which rule-702 keyword minted the ability being activated, as the caller
-- derived it (Pawl.Engine.Keyword.familyGranting). A reducer carrying no
-- grantedBy ignores it, which is every reducer whose sentence says "activated
-- abilities"; Fluctuator's says "cycling abilities" and carries CR 702.29a's
-- family. Compared and never inspected further: a KeywordFamily is a rulebook
-- designator, so nothing here learns what the reduced ability DOES.
--
-- `targets` is the THIRD criterion and the one that arrives from a different
-- MOMENT: CR 601.2c's announced targets, which CR 601.2f's reductions may name
-- (Dwarven Mauler's "equip abilities you activate that target this creature").
-- A caller that has not reached CR 601.2c hands the empty set, and every reducer
-- whose sentence names a target is then simply inapplicable -- see
-- Pawl.Engine.Activate.activateAbility, which gathers once before the targets
-- exist and again after.
--
-- The MANA increases are gathered too (Oppressive Rays), and CR 601.2f orders
-- every one of them before any reduction -- which is Cost.applyAdjustments'
-- doing rather than this gather's, since the record has no order in it. The
-- non-mana additions beside them are a different field for
-- Pawl.Types.CostAdjustments.components' stated reason.
--
-- Each reduction keeps ITS OWN floor rather than the pool taking the maximum: the
-- sentence says "this effect", so an effect that states no floor is not bound by
-- another's, and Pawl.Engine.Cost.applyAdjustments applies each floor as its own
-- reduction lands.
activationCostAdjustments :: Set.Set ObjectId -> Maybe KeywordFamily.KeywordFamily -> PlayerId -> ObjectId -> GameState -> CostAdjustments
activationCostAdjustments targets family pid srcId gs = activationCostAdjustmentsGiven (applying pid gs) targets family srcId gs

-- The same gather given the effect list the CALLER has already taken, which is
-- the half a per-permanent loop wants: `applying` is a walk of everything in
-- play asking each what player abilities it prints, and it does not depend on
-- which permanent's ability is being adjusted -- so a caller that asks this per
-- ROUTE of per PERMANENT takes an identical one every time (#1073's shape,
-- Pawl.Engine.Cost.manaActivationsGiven the caller).
--
-- IT MUST BE `applying pid gs`'s OWN ANSWER for the same pid and the same
-- board. Nothing in the type says so, and the wrapper above is what a caller
-- with no list of its own uses.
--
-- The rows arrive PAIRED WITH THEIR SOURCE and not stripped to bare effects,
-- because CR 303.4b's "enchanted" is a fact about the row's own permanent: the
-- criterion is matched through matchesObjectFrom, which needs it.
activationCostAdjustmentsGiven :: [(Maybe ObjectId, PlayerEffect)] -> Set.Set ObjectId -> Maybe KeywordFamily.KeywordFamily -> ObjectId -> GameState -> CostAdjustments
activationCostAdjustmentsGiven effects targets family srcId gs =
  let -- CR 601.2c's chosen targets, asked of ReduceActivationCost's third
      -- criterion: Dwarven Mauler's "equip abilities you activate THAT TARGET
      -- THIS CREATURE". ANY rather than all, which is what the sentence says of
      -- an ability announcing more than one target, and False on an empty set --
      -- an ability that targets nothing is not an ability that targets this
      -- creature.
      --
      -- Matched through the same matchesObjectFrom the other two criteria use, so
      -- the filter is asked against the TARGET's projection with the reducer's own
      -- permanent as Pawl.Engine.Filter's source -- which is what makes
      -- Filter.IsSource read "this creature" here.
      aims source criterion = any (\oid -> matchesObjectFrom source criterion oid gs) (Set.toList targets)
      -- CR 601.2f's MANA increase, the half CostAdjustments.increases was an
      -- empty literal for until Oppressive Rays gave it a producer; see #1242.
      -- No family beside the criterion, IncreaseActivationCost's own reason.
      increaseOf (source, effect) = case effect of
        PlayerEffect.IncreaseActivationCost (IncreaseActivationCost.MkIncreaseActivationCost criterion amount) ->
          if matchesObjectFrom source criterion srcId gs then Just amount else Nothing
        -- Thalia, turned away by the CONSTRUCTOR and not by her Filter, which is
        -- what keeps her off Mindslaver's activation (#90) -- the reading
        -- Pawl.Types.PlayerEffect's IncreaseActivationCost haddock states.
        PlayerEffect.IncreaseSpellCost {} -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.ReduceSpellCost {} -> Nothing
        PlayerEffect.AddActivationCost {} -> Nothing
        PlayerEffect.AddSpellCost {} -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands -> Nothing
        PlayerEffect.CastFromGraveyard _ -> Nothing
        PlayerEffect.PlayLandsFromGraveyard -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
      reductionOf (source, effect) = case effect of
        PlayerEffect.ReduceActivationCost (ReduceActivationCost.MkReduceActivationCost criterion granted aimedAt amount floor_) ->
          -- Never confined to coloured mana: no printed activation-cost reducer
          -- states Edgewalker's sentence, so CR 118.7b-d's spill stands.
          if matchesObjectFrom source criterion srcId gs && maybe True (\g -> Just g == family) granted && maybe True (aims source) aimedAt
            then Just (AppliedReduction.MkAppliedReduction amount floor_ False)
            else Nothing
        -- The non-mana addition, gathered by `additionOf` below: CR 601.2f's
        -- arithmetic has nothing to do to a component, so it never joins the
        -- reductions.
        PlayerEffect.AddActivationCost {} -> Nothing
        PlayerEffect.AddSpellCost {} -> Nothing
        -- Thalia and Sapphire Medallion, turned away by the constructor and not
        -- by their Filters, which is exactly what keeps a noncreature permanent's
        -- activated ability untaxed (#90).
        PlayerEffect.IncreaseSpellCost {} -> Nothing
        PlayerEffect.IncreaseActivationCost {} -> Nothing
        PlayerEffect.ReduceSpellCost {} -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands -> Nothing
        PlayerEffect.CastFromGraveyard _ -> Nothing
        PlayerEffect.PlayLandsFromGraveyard -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
      -- CR 601.2f's "plus all additional costs", the non-mana half: Brutal
      -- Suppression's "Sacrifice a land". Gathered against the SAME criterion
      -- reading the reductions use -- the ability's source permanent -- and
      -- CONCATENATED rather than resolved between, because two effects each
      -- adding a cost both add it (CR 601.2f totals them all in).
      additionOf (source, effect) = case effect of
        PlayerEffect.AddActivationCost (AddActivationCost.MkAddActivationCost criterion components scale) ->
          if matchesObjectFrom source criterion srcId gs then Just (fmap ((,) scale) components) else Nothing
        PlayerEffect.AddSpellCost {} -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.IncreaseSpellCost {} -> Nothing
        PlayerEffect.IncreaseActivationCost {} -> Nothing
        PlayerEffect.ReduceSpellCost {} -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands -> Nothing
        PlayerEffect.CastFromGraveyard _ -> Nothing
        PlayerEffect.PlayLandsFromGraveyard -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
   in CostAdjustments.MkCostAdjustments
        { CostAdjustments.increases = Maybe.mapMaybe increaseOf effects,
          CostAdjustments.reductions = Maybe.mapMaybe reductionOf effects,
          CostAdjustments.components = concat (Maybe.mapMaybe additionOf effects)
        }

-- CR 601.3b: may `pid` begin to cast `oid` as though it had flash (Vedalken
-- Orrery)? By CR 702.8a that means "any time you could cast an instant", which CR
-- 117.1a's first sentence makes "any time they have priority" -- so a True here
-- is the whole of the widening, and Pawl.Engine.Cast.timingOk needs no second
-- window to compare against.
--
-- The typed question Cast.timingOk asks BESIDE Cast.instantSpeed rather than
-- inside it, and that is the point of the constructor. Flash is a permission a
-- CARD carries about casting ITSELF, so folding this into instantSpeed -- let
-- alone into the CR 307.1 window under it (Turn.sorcerySpeedWindow) -- would make
-- an equip ability on the same board instant-speed, which CR 307.5 does not say.
-- Pawl.PlayerEffectSpec's Vedalken Orrery group proves both halves on one board:
-- the creature spell is castable on the opponent's turn, and the Equipment's CR
-- 307.5 equip ability is still not offered.
--
-- A DISJUNCTION, and for the opposite of CR 101.2's reason: this is a permission,
-- and two permissions naming different spells are not in conflict, so one
-- applicable effect is enough and no list of them can outvote another. CR
-- 613.11's timestamp order has nothing to order here.
--
-- matchesObject is called only from inside the arm that already matched, so a
-- board with no such effect on it runs no projections at all -- the posture
-- costAdjustments takes above.
--
-- CR 601.3b's SECOND SENTENCE is not implemented: the rule lets a player begin
-- casting as though a spell had flash when a choice still to be made during the
-- proposal could give the spell the qualities the effect names, and nothing here
-- searches choice space (#721).
--
-- Takes the OBJECT and not the half being proposed, and reaches the half through
-- the STATE instead: Pawl.Engine.Cast.castable asks this of an
-- asProposed-stamped state, so Projection.viewOfObject shows the chosen half
-- rather than CR 709.4's combined view. The posture spellCostAdjustments takes,
-- through the same matchesObject.
mayCastAsThoughItHadFlash :: PlayerId -> ObjectId -> GameState -> Bool
mayCastAsThoughItHadFlash pid oid gs =
  let allows (source, effect) = case effect of
        PlayerEffect.CastAsThoughItHadFlash criterion -> matchesObjectFrom source criterion oid gs
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost {} -> False
        PlayerEffect.IncreaseActivationCost {} -> False
        PlayerEffect.ReduceSpellCost {} -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.AddActivationCost {} -> False
        PlayerEffect.AddSpellCost {} -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands -> False
        -- The other CR 601.3 permission on this axis, and the two do not
        -- compose: that one names a ZONE and this question is about a TIME, so a
        -- card in a graveyard still waits for its own window.
        PlayerEffect.CastFromGraveyard _ -> False
        PlayerEffect.PlayLandsFromGraveyard -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
   in any allows (applying pid gs)

-- CR 601.3: may `pid` cast `oid` from a graveyard because an EFFECT says so?
--
-- The typed question Pawl.Engine.Cast.permitsCastFromGraveyard asks beside the
-- object-scoped Pawl.Types.CastingPermission.CastFromGraveyard it already read,
-- so that module never sees a PlayerEffect constructor and neither permission is
-- expressed in terms of the other. One card carrying flashback and one player
-- holding Yawgmoth's Will's grant are two rules (CR 702.34a, CR 601.3) reaching
-- the same gate.
--
-- Asks nothing about WHOSE graveyard: Pawl.Engine.Cast.zoneCandidates hands this
-- only the cards in `pid`'s own graveyard (CR 400.1's per-player zone), which is
-- the "your graveyard" every printing of this permission says.
--
-- A DISJUNCTION, for the reason Pawl.Types.PlayerEffect.CastFromGraveyard gives:
-- one applicable permission is enough, so CR 613.11's timestamp order has
-- nothing to order.
--
-- matchesObject is called only from inside the arm that already matched, so a
-- board with no such effect on it runs no projections at all -- the posture
-- mayCastAsThoughItHadFlash takes above.
--
-- Takes the OBJECT and not the half being proposed, and reaches the half through
-- the STATE, exactly as mayCastAsThoughItHadFlash does above: the callers stamp
-- the proposal through Pawl.Engine.Cast.asProposed first, so the same
-- matchesObject reads the chosen half rather than CR 709.4's combined view.
mayCastFromGraveyard :: PlayerId -> ObjectId -> GameState -> Bool
mayCastFromGraveyard pid oid gs =
  let allows (source, effect) = case effect of
        PlayerEffect.CastFromGraveyard criterion -> matchesObjectFrom source criterion oid gs
        -- The other CR 601.3 permission on this axis names a TIME, and this
        -- question is about a ZONE.
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost {} -> False
        PlayerEffect.IncreaseActivationCost {} -> False
        PlayerEffect.ReduceSpellCost {} -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.AddActivationCost {} -> False
        PlayerEffect.AddSpellCost {} -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
        -- A PROHIBITION, and CR 601.3 asks the two halves separately:
        -- prohibitsCasting above is where Damping Engine and Silence are read.
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands -> False
        -- The play-side twin names the same ZONE and still answers nothing here:
        -- CR 305.1 makes playing a land a special action, so a grant to play
        -- lands from a graveyard permits no cast (Crucible of Worlds lets nobody
        -- cast anything).
        PlayerEffect.PlayLandsFromGraveyard -> False
        -- A COST and not a permission at all: Omniscience says what a spell
        -- pays, never where it may be cast from, so it opens no graveyard.
        -- mayCastFromHandWithoutPayingManaCost below is its one reader, and CR
        -- 601.3's permission is still owed separately.
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
   in any allows (applying pid gs)

-- CR 118.9 / Omniscience: may `pid` cast `oid` from their hand without paying
-- its mana cost, because an EFFECT applies that alternative cost to it?
--
-- The typed question Pawl.Engine.Cost.candidateCostsFor asks in its hand arm, so
-- that module never sees a PlayerEffect constructor. NOT a permission and so not
-- asked beside the two above: a card in a hand is already castable, and what
-- this adds is one more CR 601.2b candidate. Answering True where CR 601.3
-- forbids the cast anyway changes nothing, because Pawl.Engine.Cast.castable
-- demands a permission alongside an affordable candidate.
--
-- A DISJUNCTION, for mayCastFromGraveyard's reason: an alternative cost is
-- OFFERED rather than imposed (CR 118.9b), so two effects offering the same
-- {0} give the player the same one choice and CR 613.11's timestamp order has
-- nothing to order. CR 118.9a's "only one alternative cost" is not this
-- function's to enforce -- it is CR 601.2b's announcement picking one candidate
-- from the list.
--
-- Takes the OBJECT and reaches the half through the STATE, exactly as the two
-- above do: the caller has stamped the proposal through
-- Pawl.Engine.Cast.asProposed, so matchesObject reads the half being cast.
mayCastFromHandWithoutPayingManaCost :: PlayerId -> ObjectId -> GameState -> Bool
mayCastFromHandWithoutPayingManaCost pid oid gs =
  let allows (source, effect) = case effect of
        PlayerEffect.CastFromHandWithoutPayingManaCost criterion -> matchesObjectFrom source criterion oid gs
        -- The CR 601.3 permissions, which say WHERE a spell may be cast from and
        -- WHEN. Neither states a cost, which is the whole reason this arm is its
        -- own: Yawgmoth's Will's cast pays the card's printed cost.
        PlayerEffect.CastFromGraveyard _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.PlayLandsFromGraveyard -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        -- CR 118.7's increases and reductions, which change what a cost COMES
        -- TO; this arm supplies a different cost to start from (CR 118.9c). The
        -- two compose at Pawl.Engine.Cost.total, which is handed whichever
        -- candidate CR 601.2b settled on -- so a reduction applied to {0} still
        -- floors at {0} and neither reader knows of the other.
        PlayerEffect.IncreaseSpellCost {} -> False
        PlayerEffect.IncreaseActivationCost {} -> False
        PlayerEffect.ReduceSpellCost {} -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.AddActivationCost {} -> False
        PlayerEffect.AddSpellCost {} -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
        -- The PROHIBITIONS, read at their own gate (prohibitsCasting above): CR
        -- 101.2 makes a "can't" beat any cost this offers, and folding them here
        -- would let a permission outvote one.
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands -> False
   in any allows (applying pid gs)

-- CR 305.1: may `pid` play a land from their graveyard because an EFFECT says
-- so? Crucible of Worlds' whole sentence, and the play half of Yawgmoth's Will's
-- first one.
--
-- The PLAY-side sibling of mayCastFromGraveyard above, and read at a different
-- gate for the reason CR 305.1 gives: playing a land is a special action that
-- never uses the stack, so Pawl.Engine.Action.playableLands asks this where
-- Pawl.Engine.Cast.castableZones asks that one. Neither permission implies the
-- other, and Yawgmoth's Will declares both arms because its sentence says both.
--
-- Asks nothing about WHICH land and so takes no ObjectId, where the cast side
-- takes one: the arm carries no Filter (see the type), and the zone is a
-- per-player pile (CR 400.1) whose members the caller has already selected.
--
-- A DISJUNCTION, for mayCastFromGraveyard's reason: one applicable permission is
-- enough, so CR 613.11's timestamp order has nothing to order.
mayPlayLandsFromGraveyard :: PlayerId -> GameState -> Bool
mayPlayLandsFromGraveyard pid gs =
  let allows effect = case effect of
        PlayerEffect.PlayLandsFromGraveyard -> True
        -- The cast-side twin of this one: same zone, but a land is played and
        -- never cast (CR 305.1), so it reaches no land play.
        PlayerEffect.CastFromGraveyard _ -> False
        -- CR 305.2's COUNT, which says nothing about a zone. The two compose in
        -- Pawl.Engine.Action.legalActions -- that one bounds how many plays,
        -- this one widens where they may come from -- without either knowing of
        -- the other.
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost {} -> False
        PlayerEffect.IncreaseActivationCost {} -> False
        PlayerEffect.ReduceSpellCost {} -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.AddActivationCost {} -> False
        PlayerEffect.AddSpellCost {} -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
        -- The PROHIBITIONS, which prohibitsPlayingLand above is what reads: CR
        -- 101.2 makes a "can't" beat this permission, and the two are folded at
        -- separate gates so that neither can outvote the other by accident.
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands -> False
        -- A cost, not a zone: CR 118.9's grant says what a spell PAYS and
        -- widens no pile a land may be played from.
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
   in any (allows . snd) (applying pid gs)

-- CR 702.18a / 702.11c: is `pid` protected from being the target of a spell or
-- ability controlled by `caster`?
--
-- The typed question Pawl.Engine.Target.targetable asks for its
-- Recipient.ToPlayer arm, so that module never sees a PlayerEffect constructor.
-- It answers the PLAYER halves of shroud and hexproof; the permanent halves are
-- keywords and stay where the other keyword reads are.
--
-- The scope is read against `pid`, the PROTECTED player: CR 702.11c's "you" is
-- the player who has hexproof, and "your opponents" are theirs. That is a
-- DIFFERENT anchor from the one PlayerStaticAbility.scope uses (the effect's
-- controller), which is why the argument order here is (caster, protected) and
-- inScope's is (asked-about, controller). See
-- Pawl.Types.PlayerEffect.CantBeTargetedBy for where the two would come apart.
--
-- A Nothing caster is a question with no CR 109.5 "you" in it. Hexproof does not
-- stop it -- nobody's opponent -- while shroud stops it anyway, because
-- EachPlayer never asks the caster a question. That is exactly the carve-out
-- playersInScope above already makes for the same constructor, and it is the
-- rules answer too: CR 702.18a names no player at all.
--
-- MEMBERSHIP, never a tally: CR 702.18b and CR 702.11h make multiple instances
-- redundant for the player half as well as the permanent half.
protectedFromTargeting :: Maybe PlayerId -> PlayerId -> GameState -> Bool
protectedFromTargeting caster pid gs =
  let stops effect = case effect of
        PlayerEffect.CantBeTargetedBy scope -> case caster of
          Just who -> inScope who pid gs scope
          Nothing -> case scope of
            PlayerScope.EachPlayer -> True
            PlayerScope.Opponents -> False
            PlayerScope.You -> False
            -- Unlike EachPlayer, this one does ask the caster a question -- "is
            -- the caster the player controlling the most permanents?" -- and a
            -- sourceless spell or ability is nobody, so it is not that player.
            PlayerScope.ControllingMostPermanents -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost {} -> False
        PlayerEffect.IncreaseActivationCost {} -> False
        PlayerEffect.ReduceSpellCost {} -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.AddActivationCost {} -> False
        PlayerEffect.AddSpellCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        -- CR 701.6a grants no targeting immunity: Pawl.Types.Counterability
        -- says the same about CR 113.6g, and a Cancel at a spell Spider-Punk
        -- protects still targets it legally and still resolves.
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands -> False
        PlayerEffect.CastFromGraveyard _ -> False
        PlayerEffect.PlayLandsFromGraveyard -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
   in any (stops . snd) (applying pid gs)

-- CR 305.2: the number of lands a player may normally play during their turn.
-- The base of landPlaysAllowed below, named for the same reason
-- defaultMaximumHandSize is: the rule states a number, and a bare literal in the
-- gate would not say which rule it came from.
defaultLandPlays :: Natural
defaultLandPlays = 1

-- CR 305.2 / 305.2a: how many lands `pid` may play this turn -- the LEFT-hand
-- side of CR 305.2a's comparison, whose right-hand side is
-- GameState.landsPlayed. Pawl.Engine.Action.legalActions compares them, so that
-- module never learns which effect raised the number.
--
-- A SUM over every applicable grant, added to CR 305.2's normal one. CR 305.2
-- says continuous effects "may increase this number", and an increase composes:
-- Exploration beside Azusa is one plus one plus two. Nothing makes them
-- redundant -- CR 702.18b and CR 702.11h say so for a KEYWORD, and CR 305.2
-- states no such rule -- so this is a tally and not a membership test, which is
-- the opposite posture from protectedFromTargeting above.
--
-- Saturating addition is not a concern here and the type says why: a Natural
-- cannot overflow, and every summand is one.
--
-- Read LIVE through `applying`, like every other question in this module, so an
-- Exploration destroyed after the extra land was played simply stops being found
-- (CR 604.2) -- and the already-played count outliving the grant is exactly CR
-- 305.2b, which then permits no further play.
landPlaysAllowed :: PlayerId -> GameState -> Natural
landPlaysAllowed pid gs =
  let grantOf effect = case effect of
        PlayerEffect.PlayAdditionalLands extra -> Just extra
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        -- CR 305.1's name-based prohibition stops ONE land rather than changing
        -- the turn's allowance, which is why Action.playableLands asks it per
        -- card and this per player.
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.IncreaseSpellCost {} -> Nothing
        PlayerEffect.IncreaseActivationCost {} -> Nothing
        PlayerEffect.ReduceSpellCost {} -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.AddActivationCost {} -> Nothing
        PlayerEffect.AddSpellCost {} -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands -> Nothing
        PlayerEffect.CastFromGraveyard _ -> Nothing
        PlayerEffect.PlayLandsFromGraveyard -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
   in defaultLandPlays + sum (Maybe.mapMaybe (grantOf . snd) (applying pid gs))

-- CR 402.2: a player's maximum hand size, normally seven cards. NOT CR 103.5's
-- starting hand size, which is a different seven (Mulligan.openingHand) that
-- this constant deliberately does not share -- the rules keep them apart, and
-- Reliquary Tower changes only one of them.
defaultMaximumHandSize :: Natural
defaultMaximumHandSize = 7

-- CR 402.2 / 613.11: this player's maximum hand size. Nothing IS "no maximum
-- hand size" (Reliquary Tower) -- never a sentinel, and never a very large
-- number.
--
-- The axis's ONE ordered fold, and the only reader here that could be: CR 613.11
-- applies these effects in timestamp order, and a removal and a set DISAGREE, so
-- the last one applied wins and nothing outvotes it (Reliquary Tower's own ruling
-- names the pair -- a Tower that entered after The Ten Rings leaves the controller
-- with no maximum, and the reverse order leaves them with ten). Every other
-- question in this module folds order-independently, which is why `applying`
-- sorts once for all of them rather than each sorting for itself.
--
-- A left fold from CR 402.2's seven rather than a search for the newest effect:
-- "applied in timestamp order" is a sequence of edits to one value, and reading
-- only the last one would be a different rule the moment an arm composes with
-- what it finds instead of replacing it (a maximum hand size INCREASED by two,
-- #1238).
maximumHandSize :: PlayerId -> GameState -> Maybe Natural
maximumHandSize pid gs =
  let apply current effect = case effect of
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize limit -> Just limit
        PlayerEffect.CantCastSpells -> current
        PlayerEffect.CantCastMoreThan _ -> current
        PlayerEffect.CantCastChosenName -> current
        PlayerEffect.CantPlayLandChosenName -> current
        PlayerEffect.IncreaseSpellCost {} -> current
        PlayerEffect.IncreaseActivationCost {} -> current
        PlayerEffect.ReduceSpellCost {} -> current
        PlayerEffect.ReduceActivationCost {} -> current
        PlayerEffect.AddActivationCost {} -> current
        PlayerEffect.AddSpellCost {} -> current
        PlayerEffect.PlayAdditionalLands _ -> current
        PlayerEffect.DontLoseUnspentMana _ -> current
        PlayerEffect.SpendManaAsThough _ -> current
        PlayerEffect.CantBeTargetedBy _ -> current
        PlayerEffect.CastAsThoughItHadFlash _ -> current
        PlayerEffect.CantBeCountered _ -> current
        PlayerEffect.DamageCantBePrevented _ -> current
        PlayerEffect.CantSearchLibraries -> current
        PlayerEffect.CantBecomeMonarch -> current
        PlayerEffect.CantCastMatching _ -> current
        PlayerEffect.CastOnlyAtSorcerySpeed -> current
        PlayerEffect.CantPlayLands -> current
        PlayerEffect.CastFromGraveyard _ -> current
        PlayerEffect.PlayLandsFromGraveyard -> current
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> current
   in List.foldl' (\current row -> apply current (snd row)) (Just defaultMaximumHandSize) (applying pid gs)

-- CR 500.5 / 106.4 / 613.11: which of the unspent mana in this player's pool do
-- they keep as a step or phase ends (Upwelling, Omnath Locus of Mana)? The typed
-- question Pawl.Engine.Mana.emptyManaPools asks, so the turn-based action of CR
-- 703.4q never learns which effect answered it.
--
-- A PER-UNIT predicate rather than a Bool about the whole pool, because CR 106.4
-- says the player loses "this mana" and a card may name only some of it: Omnath,
-- Locus of Mana
-- keeps green and drops the rest of the same pool. The pid and the state are
-- taken FIRST so a caller sweeping a pool resolves the applicable effects once
-- and asks the units afterwards.
--
-- A DISJUNCTION over the applicable effects: two retention effects that name
-- different mana are not in conflict, so a unit either effect keeps is kept, and
-- an empty list keeps nothing. CR 613.11's timestamp order has nothing to order
-- here -- unlike a cost adjustment, no answer depends on which ran first.
--
-- Read LIVE through `applying`, like every other question here, so an Upwelling
-- destroyed earlier in the step is simply not found and the pools empty with
-- nothing to unwind (CR 604.2).
--
-- NOT the whole of what the pool keeps, and deliberately not. A card that keeps
-- only the mana it just added (Shizuko, Caller of Autumn) says different things
-- about two manas of one pool, which no player-axis filter can express, so its
-- retention rides the UNIT instead (Pawl.Types.ManaRetention).
-- Pawl.Engine.Mana.emptyManaPools takes the disjunction of the two carriers.
keepsUnspentMana :: PlayerId -> GameState -> ManaUnit -> Bool
keepsUnspentMana pid gs =
  let keeps effect = case effect of
        PlayerEffect.DontLoseUnspentMana f -> Just f
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.IncreaseSpellCost {} -> Nothing
        PlayerEffect.IncreaseActivationCost {} -> Nothing
        PlayerEffect.ReduceSpellCost {} -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.AddActivationCost {} -> Nothing
        PlayerEffect.AddSpellCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands -> Nothing
        PlayerEffect.CastFromGraveyard _ -> Nothing
        PlayerEffect.PlayLandsFromGraveyard -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
      filters = Maybe.mapMaybe (keeps . snd) (applying pid gs)
   in \unit -> any (\f -> ManaFilter.matches f unit) filters

-- CR 609.4b / 613.11: the clauses saying what this player may spend their mana
-- as though it were (Celestial Dawn). The typed question Pawl.Engine.Mana asks
-- at its funnel, so that module never sees a PlayerEffect constructor.
--
-- The CLAUSES and not an answer, because the answer is per-mana and this is
-- per-player: Pawl.Engine.Mana.spendableAs folds them against one mana type,
-- which is where the ManaFilter and CR 106.1b's types belong. Taking pid and the
-- state FIRST, as keepsUnspentMana does, so a caller resolving one payment
-- gathers the effects once.
--
-- Read LIVE through `applying`, like every other question here, so a Celestial
-- Dawn destroyed with a payment half made is simply not found (CR 604.2).
--
-- No ordering. CR 613.11 sorts continuous effects by timestamp, and there is
-- nothing here to sort: `spendableAs` unions what the applicable clauses permit,
-- and union does not care which ran first.
spendManaAsThough :: PlayerId -> GameState -> [SpendManaAsThough.SpendManaAsThough]
spendManaAsThough pid gs =
  let spends effect = case effect of
        PlayerEffect.SpendManaAsThough clause -> Just clause
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.IncreaseSpellCost {} -> Nothing
        PlayerEffect.IncreaseActivationCost {} -> Nothing
        PlayerEffect.ReduceSpellCost {} -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.AddActivationCost {} -> Nothing
        PlayerEffect.AddSpellCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands -> Nothing
        PlayerEffect.CastFromGraveyard _ -> Nothing
        PlayerEffect.PlayLandsFromGraveyard -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
   in Maybe.mapMaybe (spends . snd) (applying pid gs)

-- CR 701.6a / 613.11: can this spell or ability on the stack be countered
-- (Spider-Punk, Prowling Serpopard)? The typed question
-- Pawl.Engine.Event.counter asks at its funnel, so that module never sees a
-- PlayerEffect constructor.
--
-- Takes the player who controls the VICTIM, which CR 113.8 supplies for the
-- ability half -- "the controller of an activated ability on the stack is the
-- player who activated it" -- and CR 601.2a for the spell half. Never the
-- countering spell's controller: the protection is the victim's.
--
-- Takes the VICTIM's id as well, for the Filter: Prowling Serpopard protects
-- only "creature spells", which is a question about the victim's own
-- characteristics and not about who controls it. Read through the same
-- matchesObject as CR 601.2f's cost adjustments and CR 601.3b's timing
-- permission, so "which objects does this effect name?" has one reading on this
-- axis.
--
-- CR 113.9's other subject needs no case of its own. An ability on the stack has
-- no card behind it -- Game.faceOf answers Nothing for one -- so
-- Projection.viewOfObject hands the matcher an object with no card types and no
-- colours: `And []` is true of it (Spider-Punk still stops a Stifle) and any
-- atom naming a quality is false of it (Prowling Serpopard does not). The rule
-- lives in the card's own filter rather than in a branch here.
--
-- A DISJUNCTION for CR 101.2's reason, the shape every prohibition here takes:
-- one applicable "can't" is enough and nothing outvotes it. CR 613.11's
-- timestamp order has nothing to order, since no answer depends on which ran
-- first.
--
-- Read LIVE through `applying`, like every other question in this module, so a
-- Spider-Punk destroyed earlier in the turn is simply not found (CR 604.2).
cantBeCountered :: PlayerId -> ObjectId -> GameState -> Bool
cantBeCountered pid oid gs =
  let stops (source, effect) = case effect of
        PlayerEffect.CantBeCountered criterion -> matchesObjectFrom source criterion oid gs
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost {} -> False
        PlayerEffect.IncreaseActivationCost {} -> False
        PlayerEffect.ReduceSpellCost {} -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.AddActivationCost {} -> False
        PlayerEffect.AddSpellCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        -- Spider-Punk's OTHER sentence, and no part of this answer: CR 615.12
        -- is about a damage event and CR 701.6a about an object on the stack.
        -- The two travel together on one card and share nothing.
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands -> False
        PlayerEffect.CastFromGraveyard _ -> False
        PlayerEffect.PlayLandsFromGraveyard -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
   in any stops (applying pid gs)

-- CR 615.12 / 613.11: every "damage can't be prevented" effect standing right
-- now (Spider-Punk, Excruciator), each paired with the SOURCE of the ability
-- that says it. The typed question Pawl.Engine.Replacement.preventable asks, so
-- neither that module nor Pawl.Engine.Event ever sees a PlayerEffect
-- constructor.
--
-- Hands back the PATTERNS rather than a yes/no, because CR 615.12's sentence is
-- about a damage EVENT and the printed narrowings name a quality of one:
-- Excruciator's names its own source. Reading a pattern against an event is CR
-- 615.1's own arithmetic, which Pawl.Engine.Replacement.matchesDamagePattern
-- owns for the shields, and one reading of what a DamagePattern means is worth
-- more than a boolean asked here.
--
-- The SOURCE rides out because the pattern's Filter is resolved against it --
-- Filter.IsSource is Excruciator's clause naming the permanent that prints it,
-- and CR 109.5's "you" inside such a filter is that permanent's controller -- and
-- it is the Maybe `applying` already carries: Nothing for a stored CR 611.2c
-- effect, which has no permanent behind it, and no printing pairs one with a
-- self-naming pattern.
--
-- Gathered from EVERY still-playing player rather than from one, because
-- `applying` is indexed by player and this effect is not. That reading is EXACT
-- for PlayerScope.EachPlayer, the scope Spider-Punk's possessive-free sentence
-- writes: EachPlayer is in scope for everybody, so "some player has it applying"
-- and "it applies" are the same fact. Duplicated rows are why the caller folds
-- with `any` and not with a count.
--
-- A narrower scope would make them come apart, and no card may write one:
-- Pawl.CardSpec's "no card narrows CR 615.12" lint sweeps both carriers
-- `applying` folds together -- the printed static ability and the stored
-- Effect.AffectPlayers -- and rejects any that pairs this effect with another
-- scope. There is nothing for a scope to select here anyway: CR 615.12's
-- sentence names a damage event, and its narrowings narrow the event rather
-- than the table.
--
-- CR 102.1's departed seats are already excluded, since Game.stillPlaying is
-- what `applying`'s own scope resolution folds over.
--
-- Read LIVE through `applying`, like every other question in this module, so an
-- Excruciator destroyed earlier in the turn simply stops being found and the
-- shields on the board go back to working (CR 604.2).
unpreventable :: GameState -> [(Maybe ObjectId, DamagePattern.DamagePattern)]
unpreventable gs =
  let says (src, effect) = case effect of
        PlayerEffect.DamageCantBePrevented pattern_ -> Just (src, pattern_)
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands -> Nothing
        PlayerEffect.CastFromGraveyard _ -> Nothing
        PlayerEffect.PlayLandsFromGraveyard -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.IncreaseSpellCost {} -> Nothing
        PlayerEffect.IncreaseActivationCost {} -> Nothing
        PlayerEffect.ReduceSpellCost {} -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.AddActivationCost {} -> Nothing
        PlayerEffect.AddSpellCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
   in concatMap (\pid -> Maybe.mapMaybe says (applying pid gs)) (Game.stillPlaying gs)
