-- CR 613.10 / 613.11: the continuous effects that affect PLAYERS and the RULES
-- OF THE GAME rather than the characteristics of objects. A sibling TIER to the
-- CR 613 layer system, not a layer in it: CR 613.1 makes the seven layers a
-- machine for computing object characteristics and nothing else, and
-- 613.10/613.11 both apply AFTER that machine has run.
--
-- The dependency is therefore ONE-WAY, and that is what keeps it well founded:
-- this module reads the layer machine's finished answers, while
-- Pawl.Engine.Projection is untouched by this module.
--
-- This module is the only module that may case on Pawl.Types.PlayerEffect and
-- Pawl.Types.PlayerScope for what an effect MEANS -- the standing
-- Pawl.Engine.Resolve has over Effect,
-- Pawl.Engine.Projection over Modification, Pawl.Engine.Event over
-- TriggerCondition and Pawl.Engine.Expiry over Expiry. Every consumer asks a
-- TYPED QUESTION and never sees a constructor.
--
-- Pawl.Types.AffectedPlayers is the one type here that Resolve also cases on,
-- and by that same standing: it is an Effect payload, and only a resolution can
-- answer the slot its Named arm holds. This module reads the BAKED value.
--
-- The one other module that cases on PlayerEffect is Pawl.Engine.Projection,
-- through rewritePlayerEffect, and it asks nothing about meaning: CR 612.1's word
-- swap walks the type's STRUCTURE for a Filter that could hold a subtype word,
-- and hands the answer straight back. Living there is what lets rewriteEffect
-- reach a restriction a resolution stores.
module Pawl.Engine.PlayerEffect where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Containers.ListUtils as ListUtils
import qualified Data.Functor.Const as Functor
import qualified Data.Functor.Identity as Functor
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.IgnoredAbility as IgnoredAbility
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.ManaFilter as ManaFilter
import qualified Pawl.Engine.Players as Players
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.Rewrite as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.SourceContext as SourceContext
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Engine.Vanguard as Vanguard
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivationCriteria as ActivationCriteria
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.AlternativeActivationCost as AlternativeActivationCost
import qualified Pawl.Types.AppliedReduction as AppliedReduction
import qualified Pawl.Types.CantSearchLibraries as CantSearchLibraries
import qualified Pawl.Types.CastFromZone as CastFromZone
import qualified Pawl.Types.CostAddition as CostAddition
import Pawl.Types.CostAdjustments (CostAdjustments)
import qualified Pawl.Types.CostAdjustments as CostAdjustments
import qualified Pawl.Types.CostChange as CostChange
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostModifier as CostModifier
import qualified Pawl.Types.CostSubject as CostSubject
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.ExileLink as ExileLink
import qualified Pawl.Types.Face as Face
import Pawl.Types.Filter (Filter)
import qualified Pawl.Types.Filter as Filter.Type
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.InZone as InZone
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.KeywordDesignator as KeywordDesignator
import qualified Pawl.Types.LoyaltyKind as LoyaltyKind
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import Pawl.Types.ManaUnit (ManaUnit)
import qualified Pawl.Types.ModifiedRoll as ModifiedRoll
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectSnapshot as ObjectSnapshot
import qualified Pawl.Types.PastActivation as PastActivation
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import qualified Pawl.Types.PermissionLimit as PermissionLimit
import qualified Pawl.Types.PermissionPool as PermissionPool
import qualified Pawl.Types.PermissionVerb as PermissionVerb
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import Pawl.Types.PlayerEffect (PlayerEffect)
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import Pawl.Types.PlayerScope (PlayerScope)
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.PlotFromZone as PlotFromZone
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.RowSource (RowSource)
import qualified Pawl.Types.RowSource as RowSource
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SourceChoices as SourceChoices
import qualified Pawl.Types.SpendManaAsThough as SpendManaAsThough
import qualified Pawl.Types.StatedFlip as StatedFlip
import Pawl.Types.Timestamp (Timestamp)
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.VariableChoice as VariableChoice
import qualified Pawl.Types.Zone as Zone

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
-- The board comes in because two arms' membership is a fact about it rather than
-- about the two players: PlayerScope.ControllingMostPermanents reads who leads,
-- and PlayerScope.Related Opponent reads CR 808.1's teams. Each is computed inside the
-- arm that asks, so a scope that names neither pays nothing.
inScope :: PlayerId -> PlayerId -> GameState -> PlayerScope -> Bool
inScope pid controller gs scope = case scope of
  -- PlayerRelation.holds, the judge every player-naming position shares: CR
  -- 102.3's opponents are every player not on the controller's team, and
  -- AnyPlayer includes the controller (Thalia's ruling: "including your own").
  PlayerScope.Related relation -> PlayerRelation.holds (Game.teams gs) relation controller pid
  PlayerScope.ControllingMostPermanents -> permanentLeader gs == Just pid

-- The same question over a CARRIER's affected set rather than over a bare scope:
-- is `pid` one of the players this row applies to? inScope for the Scoped arm,
-- and an equality for the seat CR 601.2c chose and Pawl.Engine.Resolve baked.
--
-- Both carriers are asked through this, and the printed one only ever holds a
-- Scoped: a static ability has no target slot to have named a seat.
--
-- CR 801.10: a row reaches only players within its controller's range of
-- influence, which is cut here rather than in inScope, whose other readers ask
-- about a relation between two players and not about an effect's reach.
-- Pawl.RangeOfInfluenceSpec's "CR 801.10 a player ability does not reach a
-- player outside its controller's range" proves it.
applies :: PlayerId -> PlayerId -> GameState -> AffectedPlayers.AffectedPlayers PlayerId -> Bool
applies pid controller gs affected =
  Game.inRangeOf controller pid gs && case affected of
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
  let grants = Projection.controlGrants gs
      controls pid = length (filter (\oid -> Projection.controllerOfGiven grants oid gs == Just pid) (Set.toList (GameState.battlefield gs)))
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
-- (Pawl.Engine.Target.graveyardRecipients), and the offer a resolving "choose
-- a player" makes (CR 608.2d). A relation is Pawl.Engine.Players.related's: the
-- players still in the game (CR 102.1) within the perspective's range (CR
-- 801.5a / 801.10), so no scope names a departed seat or one out of reach.
--
-- Nothing is an ABSENT perspective, which is CR 109.5's "you" with nobody to be
-- -- the vacuous posture every player-referencing Filter atom takes. AnyPlayer
-- is answerable anyway, because it never asks the perspective a question: "target
-- card in a graveyard" names the whole table whoever is reading it.
-- ControllingMostPermanents is answerable for that same reason, one arm further
-- on: its membership is a fact about the board.
playersInScope :: Maybe PlayerId -> GameState -> PlayerScope -> Maybe [PlayerId]
playersInScope perspective gs scope = case scope of
  PlayerScope.Related relation -> Players.related perspective gs relation
  PlayerScope.ControllingMostPermanents -> Just (filter (`elem` Players.table perspective gs) (Maybe.maybeToList (permanentLeader gs)))

-- CR 707.2a: the player abilities this permanent's copiable rules text gives it
-- -- its copy snapshot's when it has one, its printed face's otherwise. The
-- Pawl.Engine.Projection.View.staticAbilitiesOf of this axis, and written here rather
-- than beside it so that Pawl.Engine.Projection goes on never seeing
-- Pawl.Types.PlayerEffect; the one accessor both share is
-- Projection.copiableSnapshotOf, which is also where CR 709.5's copied halves
-- are forked out for every reader at once. Read off Projection.textBoxHolderOf
-- first, for CR 612.5, as staticAbilitiesOf is. CR 612.6's full text is the
-- other way the list changes hands, read off Projection.fullTextOf as
-- Projection.ruleAbilitiesOf reads it; Pawl.ProjectionSpec's "CR 612.6 the
-- Shapeshifter as Gnat Miser cuts bob's maximum hand size" proves it.
playerAbilitiesOf :: ObjectId -> GameState -> [PlayerStaticAbility.PlayerStaticAbility]
playerAbilitiesOf carrier gs =
  let oid = Projection.textBoxHolderOf carrier gs
   in case Projection.fullTextOf carrier gs of
        Just pc -> PC.playerAbilities pc
        Nothing -> case Projection.carriedSnapshotOf carrier oid gs of
          Just snapshot -> PC.playerAbilities snapshot
          -- CR 709.5: a copy of a Room reads the copied card's halves against its OWN
          -- designations, which Game.faceOf already does, so it lands here --
          -- Projection.copiableSnapshotOf answering Nothing is what puts it here, the
          -- fork being made once for all six readers rather than per reader.
          Nothing -> foldMap Face.playerAbilities (Game.faceOf oid gs)

-- CR 613.7a: the PRINTED carrier's rows -- printed as opposed to CR 611.2c's
-- stored one, the list itself being the COPIABLE one above -- one per player
-- ability on one battlefield permanent that still has it, as
-- (timestamp, source, ability name, reaches, effect) -- the shape `applying`
-- below sorts, filters and strips. `reaches` answers whether the row applies to
-- a given player: its scope, CR 801.10's range, and its condition asked about
-- that player.
--
-- Top-level and shared rather than local to that function, because a second
-- question needs the same walk and needs it read DIFFERENTLY: `applying` drops
-- the rows CR 116.2d's ignore suppresses, while affectedBy below must not (see
-- its comment). Sharing the walk is what keeps CR 604.2's two ability losses and
-- CR 612.1's word swap from being restated in a second place and drifting.
--
-- UNSORTED and UNFILTERED. Both are `applying`'s job, and neither reader may
-- assume the other's.
printedRows :: GameState -> [(Timestamp, Maybe ObjectId, Maybe AbilityName.AbilityName, PlayerId -> Bool, PlayerEffect)]
printedRows gs =
  let -- Hoisted out of the walk exactly as Projection.gather hoists it: an
      -- inlined call would recompute the whole game's SetLandSubtype list once
      -- per permanent.
      setEffs = Projection.setLandSubtypeEffects gs
      -- Hoisted for the same reason, and a thunk until a permanent that actually
      -- has a player ability forces it -- so the ordinary board pays nothing for
      -- the CR 604.2 question below.
      removed = Projection.namedAbilityRemoval gs
      -- One permanent's rows, each ability stamped and read through `changes`,
      -- CR 612.1's word swap over the text it is printed in -- none for a
      -- granted one (CR 612.3).
      rowsOf changes oid controller stamped =
        let readAs = if null changes then id else Projection.rewritePlayerEffect changes
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
            -- turn". The taxed player is PlayerRef.Candidate instead,
            -- substituted per affected player (Condition.forCandidate):
            -- Angelic Arbiter's "each opponent who cast a spell this
            -- turn" asks each opponent about themselves. The same player
            -- is the Context's recipient, so Ethersworn Canonist's "each
            -- player who has cast a nonartifact spell" counts their own
            -- casts through Filter.ControlledByRecipient.
            --
            -- The clause takes the same CR 612.1 word swap the effect
            -- beside it does, since one ability's two halves cannot
            -- disagree about what a word means.
            lives ability pid = case PlayerStaticAbility.condition ability of
              Nothing -> True
              Just c ->
                Condition.holds
                  (Projection.fullView gs)
                  ((Projection.sourceContext gs (Just controller) oid) {Filter.recipient = Just pid})
                  gs
                  oid
                  (Condition.forCandidate pid (if null changes then c else Projection.rewriteCondition changes c))
            reaches ability pid =
              applies pid controller gs (AffectedPlayers.Scoped (PlayerStaticAbility.scope ability))
                && lives ability pid
         in fmap (\(stamp, ability) -> (stamp, Just oid, PlayerStaticAbility.name ability, reaches ability, readAs (PlayerStaticAbility.effect ability))) stamped
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
            -- Asked per ability, by its name, so a removal naming one
            -- leaves the others (Pawl.ActivateSpec's Synthetic Hushed
            -- Warden group).
            --
            -- That same "after the layers" placement is why the CR 305.7 gate
            -- is liveAfterLayers: the projection is finished here, so the
            -- setter's affected set is read against it rather than against base
            -- characteristics, and a permanent animated into a land at layer 4
            -- is reached (Ashaya on Thalia, under Blood Moon).
            let kept = filter (\ability -> not (removed (PlayerStaticAbility.name ability) oid)) abilities
             in if (null setEffs || Projection.liveAfterLayers setEffs oid gs) && not (null kept)
                  then
                    -- CR 612.1's word swap over the permanent's own text, computed
                    -- HERE rather than hoisted beside setEffs above, exactly as
                    -- Pawl.Engine.CombatRestriction.restricted computes it:
                    -- readerTextChanges folds the whole continuous-effect list,
                    -- and the empty case above has already turned away every
                    -- permanent that prints no player ability, so the fold runs
                    -- once per ability-bearing permanent instead of once per
                    -- permanent on the battlefield.
                    rowsOf (Projection.readerTextChanges oid gs) oid controller (fmap ((,) (Projection.staticTimestampOf oid object gs)) kept)
                  else []
      -- CR 613.1f / 613.10: the player abilities layer 6 GRANTED a permanent
      -- (Nerd Rage's "You have no maximum hand size"), read off the finished
      -- projection because only the fold knows who a static ability's grant
      -- reaches. The ability is the RECEIVER's (CR 113.7), so its "you" is the
      -- receiver's controller, and its timestamp is the later of the
      -- receiver's and the granting effect's (CR 613.7a) -- the receiver's own,
      -- since a text-box exchange leaves a grant where it is (CR 612.3).
      -- None of the printed gates above applies: the
      -- fold's own removal already took any grant a later removal reaches (CR
      -- 613.7), CR 305.7 strips only a land's rules text, and CR 612.3 keeps a
      -- text change out of a granted ability. One whole-board projection, and
      -- only on a board where something grants a player ability.
      -- Pawl.PlayerEffectSpec's NerdRage group proves it.
      pcs = Projection.projectAll gs
      grantedFrom oid = case PC.grantedPlayerAbilities (Projection.projectGiven pcs oid gs) of
        [] -> []
        abilities -> case (Game.lookupObject oid gs, Projection.controllerOf oid gs) of
          (Just object, Just controller) -> rowsOf [] oid controller (fmap (Bifunctor.first (max (Object.timestamp object))) abilities)
          _ -> []
      onBattlefield = Set.toList (GameState.battlefield gs)
   in concatMap fromPermanent onBattlefield
        <> (if Projection.grantsPlayerAbilityAnywhere gs then concatMap grantedFrom onBattlefield else [])

-- CR 116.2d's WHO: is `pid` a player whose game the permanent `oid` is changing
-- right now? That is the rule's own reading of who may take the special action
-- to ignore it -- every printed producer offers it to exactly the players its
-- static ability affects, whether that is "any player" over an AnyPlayer scope
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
-- The SOURCE **and** the ability's name, which is CR 116.2d's own grain -- "the
-- effect from that ability". A permanent's other abilities do not make the offer:
-- a player the named ability is not changing the game for is offered nothing,
-- however much the rest of the permanent is doing to them. Damping Engine's one
-- sentence declares two rows carrying one name, so either of them affecting this
-- player offers the action and taking it suppresses both.
affectedBy :: PlayerId -> ObjectId -> AbilityName.AbilityName -> GameState -> Bool
affectedBy pid oid name gs =
  let affected (_, source, abilityName, reaches, _) =
        source == Just oid && abilityName == Just name && reaches pid
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
-- The (timestamp, source, name, reaches, effect) rows go no further than this
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
-- direction Modification.AddChosenColor reads a colour. A stored CR 611.2c effect
-- carries one too -- ActivePlayerEffect.source is the object that resolved -- and
-- it is what makes Filter.IsSource answerable for Lava Burst's self-naming
-- clause, which was vacuously False while this walk hardcoded Nothing. A stored
-- row's source also carries the choices baked as it began (storedSource), which
-- contextFor below reads in place of the object's.
applying :: PlayerId -> GameState -> [(RowSource, PlayerEffect)]
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
      -- the stack (Pawl.Engine.Projection.Rewrite.rewriteEffect).
      --
      -- The SOURCE is the object that resolved (ActivePlayerEffect.source), so a
      -- stored effect answers Filter.IsSource about itself -- which is what Lava
      -- Burst's "if Lava Burst would deal damage" needs and what a hardcoded
      -- Nothing here made vacuously False.
      storedOne active =
        ( ActivePlayerEffect.timestamp active,
          storedSource active,
          -- CR 611.2a: a resolved spell's continuous effect is not an ability, so
          -- there is no printed name for CR 116.2d's ignore to have named -- which
          -- is the same reason `notIgnored` below is applied to the printed
          -- carrier alone.
          Nothing,
          \who -> applies who (ActivePlayerEffect.controller active) gs (ActivePlayerEffect.scope active),
          ActivePlayerEffect.effect active
        )
      -- CR 611.2a: a row whose window has not begun applies to nobody yet --
      -- Sphinx's Decree's "during that player's next turn" (Expiry.begun).
      stored = fmap storedOne (filter (Expiry.begun gs . ActivePlayerEffect.expiry) (GameState.playerEffects gs))
      -- CR 116.2d: a player who has paid to ignore a permanent's static ability
      -- sees no row that ability produced -- "the effect from that ability",
      -- matched by the name its face gives it, so a permanent's OTHER abilities
      -- keep applying. Filtered HERE, at the one gather every consumer reads
      -- through, so the ignore reaches all of that ability's readers at once and
      -- cannot be forgotten by a later gate.
      --
      -- An UNNAMED row can never be suppressed, which is exact: a face that names
      -- no ability grants no ignore either (Pawl.AbilitySlotLintSpec joins the
      -- two), so there is nothing that could have been paid for.
      --
      -- Only the PRINTED carrier can be ignored, which is exact rather than a
      -- shortcut: CR 116.2d's subject is "effects from static abilities", and a
      -- stored effect came from a resolution instead (Silence). Applied to
      -- `printed` alone rather than to the concatenation, and that placement is
      -- load-bearing now that a stored row names its source: an activated
      -- ability of a permanent stores rows under that permanent's own id, which
      -- a shared filter would suppress on a rule the ability is not subject to.
      notIgnored (_, source, name, _, _) = not (any (\s -> IgnoredAbility.ignoredBy pid s name gs) (RowSource.object source))
      applyingScope (_, _, _, reaches, _) = reaches pid
      effectOf (_, source, _, _, effect) = (source, effect)
      stampOf (timestamp, _, _, _, _) = timestamp
      printedOne (timestamp, source, name, reaches, effect) = (timestamp, liveSource source, name, reaches, effect)
   in fmap effectOf (List.sortOn stampOf (filter applyingScope (filter notIgnored (fmap printedOne printed) <> stored)))

-- A printed row's source: its choices are read off the object live (CR 604.2).
liveSource :: Maybe ObjectId -> RowSource
liveSource source = RowSource.MkRowSource source Nothing

-- A stored row's source, carrying the choices baked as it began (CR 608.2h).
storedSource :: ActivePlayerEffect.ActivePlayerEffect -> RowSource
storedSource active = RowSource.MkRowSource (Just (ActivePlayerEffect.source active)) (Just (ActivePlayerEffect.choices active))

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
-- Read out of Game.castsPerPlayer rather than folding the log itself, so the
-- per-seat map Engine.beginTurnOf snapshots and this one seat's count are the
-- same fold.
castsThisTurn :: PlayerId -> GameState -> Natural
castsThisTurn pid gs = Map.findWithDefault 0 pid (Game.castsPerPlayer gs)

-- CR 601.3: a player can begin to cast a spell only if no rule or effect
-- prohibits it. The prohibit half. Cast.permitsCastWhileSearching is not the
-- general allow half of CR 601.3 -- it is only the Panglacial Wurm timing
-- exception, one specific instance of "allows".
--
-- CR 101.2 is why this folds as a DISJUNCTION: a "can't" effect takes precedence
-- over anything allowing or directing. One applicable prohibition is enough and
-- nothing outvotes it.
--
-- Takes the SPELL as the proposal shows it: CR 709.3a evaluates only the chosen
-- half to see if it can be cast, so a split card is asked this question once
-- per half. Two of the prohibitions are quality-free -- "can't cast spells",
-- "can't cast more than one spell"; CR 601.3a's quality-bearing shape is a
-- Filter over the spell's characteristics (Null Chamber's "spells with the
-- chosen names" is Filter.HasChosenName, Damping Engine's "artifact, creature,
-- or enchantment spells"), asked of the proposal's projection through
-- matchesObjectFor, the same direction spellCostAdjustments reads Thalia's tax.
-- `gs` must therefore be Cast.asProposed-stamped for the half and facing being
-- asked about: CR 709.3b leaves the view one half's name, which is why naming
-- "Wax" stops Wax and leaves Wane castable, and a face-down proposal carries CR
-- 708.2a's empty name rather than the card's.
--
-- CR 601.3a's LOOKAHEAD rides on the Filter arm, in choiceCouldEscape below: a
-- prohibition that names the spell as it stands is ignored when a choice still to
-- be made during the proposal could take it out of the class. X is the one such
-- choice searched for there, and Void Winnower against Molten Disaster is what
-- observes it. `variable` says whether X is a choice at all for the candidate
-- being asked about: CR 107.3b fixes it at 0 for a cost that pays neither the
-- mana cost nor an alternative cost including X, so the caller -- which judges
-- each of CR 601.2b's candidates on its own (Cast.candidateAllowed) -- hands in
-- the candidate's answer.
--
-- CR 702.103b's bestow is the other one, and it does not come through this
-- function at all: Pawl.Engine.Cast.castable asks this predicate once per
-- CANDIDATE, each time against the board that candidate's own announcement
-- produces (Cast.proposedFor), which is what CR 702.103d asks for and is why
-- Aether Storm stops Nyxborn Rollicker's printed cast and not its bestowed one.
--
-- The two choices CR 601.2b names as preceding the announcement -- "choosing to
-- cast a spell with flashback from a graveyard or choosing to cast a creature
-- with morph face down" -- need no search, because each is its own Action here:
-- the offer is per (half, facing) pair and this predicate is asked once per pair
-- on that pair's own proposal. CR 708.2a's empty name is what a morph cast
-- brings, which is why a Null Chamber naming the card stops its face-up cast and
-- not its face-down one -- Pawl.FaceDownSpec's "CR 708.4 a prohibition naming
-- the card stops the face-up cast and not the morph one".
prohibitsCasting :: PlayerId -> ObjectId -> VariableChoice.VariableChoice -> GameState -> Bool
prohibitsCasting pid oid variable gs =
  let cast = castsThisTurn pid gs
      prohibits (source, effect) = case effect of
        PlayerEffect.CantCastSpells -> True
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan limit -> cast >= limit
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        -- CR 305.2 raises how many LANDS may be played, and a land is never
        -- cast (CR 305.1), so this grant reaches nothing here.
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        -- CR 702.18a / 702.11c restrict TARGETING, not casting: a player with
        -- shroud may cast anything, and Pawl.Engine.Target.targetable is where
        -- the restriction lands (CR 115.4, CR 601.2c).
        PlayerEffect.CantBeTargetedBy _ -> False
        -- CR 601.3b ALLOWS, and this is the prohibit half: a permission is not a
        -- prohibition, and CR 101.2 would let a prohibition outvote it anyway.
        -- mayCastAsThoughItHadFlash below is where it is read.
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        -- CR 701.6a is about a spell or ability ALREADY on the stack, and CR
        -- 601.3 is about beginning to cast one: an uncounterable spell is not a
        -- spell anyone is more or less allowed to cast.
        PlayerEffect.CantBeCountered _ -> False
        -- CR 615.12 edits what CR 615.1's shields do to a damage event, and CR
        -- 614.9's redirections likewise. Nobody is more or less allowed to cast
        -- a spell for either.
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
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
        --
        -- CR 109.5's "you" is the CASTER and not the card's controller, which it
        -- has none of in a hand -- see matchesObjectFor below, and see #2169 for what
        -- observes the difference.
        PlayerEffect.CantCastMatching criterion ->
          matchesObjectFor pid source criterion oid gs && not (choiceCouldEscape pid source criterion oid variable gs)
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
        -- CR 305.1: a land is played and never cast, so the play-side
        -- prohibition stops nothing here, however its Filter reads.
        -- Pawl.Engine.Action.playableLands is the gate that reads it.
        PlayerEffect.CantPlayLands _ -> False
        -- CR 601.3's other half: this arm ALLOWS a cast the rules would refuse,
        -- and no permission prohibits anything. mayCastFromGraveyard below is
        -- where it is read.
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
   in any prohibits (applying pid gs)

-- CR 602.5 / 101.2 / Sen Triplets: does an effect stop this player activating
-- an ability under this rule-702 stamp (Pawl.Types.ActivatedAbility.keyword)?
-- The activation-side twin of prohibitsCasting above, and its own question
-- rather than a widening of that one: CR 602.1 makes an activated ability
-- neither a spell nor a special action, so nothing on the cast axis reaches it
-- and Silence stops no activation.
--
-- A row naming no designator refuses every ability (Sen Triplets); one naming
-- a designator refuses only the abilities it designates, Kang the Conqueror's
-- "power-up abilities" -- a citation compared to a citation, as
-- ActivationCriteria.grantedBy is.
--
-- Given the rows the caller has already gathered, which is what lets
-- Pawl.Engine.Cost.manaActivationsGiven ask it inside its own hoisted sweep
-- (#1073); `prohibitsActivating` is the wrapper for a caller holding no list.
prohibitsActivatingGiven :: Maybe Keyword -> [(RowSource, PlayerEffect)] -> Bool
prohibitsActivatingGiven stamp effects =
  let bars effect = case effect of
        PlayerEffect.CantActivateAbilities designator -> maybe True (\d -> any (Keyword.designates d) stamp) designator
        _ -> False
   in any (bars . snd) effects

prohibitsActivating :: Maybe Keyword -> PlayerId -> GameState -> Bool
prohibitsActivating stamp pid gs = prohibitsActivatingGiven stamp (applying pid gs)

-- CR 305.1: does any effect prohibit `pid` from PLAYING this land?
-- The play-side twin of prohibitsCasting above, and a separate question rather
-- than a widening of it: CR 305.1 makes playing a land a special action that
-- never uses the stack, so a land is never a spell and none of the cast-side
-- prohibitions reaches it (Silence stops no land).
--
-- Takes the OBJECT, for the reason prohibitsCasting above does: CantPlayLands
-- narrows by a quality a Filter states (City in a Bottle's "a name originally
-- printed in the Arabian Nights expansion", Null Chamber's chosen names), and
-- that is a question about the card in the zone rather than about the player.
-- The Filter is read against the card AS THAT ZONE SHOWS IT: CR 712.8a's front
-- face, and CR 709.4's combined view, under which Filter.HasChosenName is CR
-- 709.4a's "one of its names".
--
-- A DISJUNCTION for CR 101.2's reason.
prohibitsPlayingLand :: PlayerId -> ObjectId -> GameState -> Bool
prohibitsPlayingLand pid oid gs =
  let prohibits (source, effect) = case effect of
        -- CR 305.1 in the direction CantCastMatching below takes: Teferi's
        -- clause narrows when this player may CAST, and playing a land is never
        -- casting. CR 305.1's own window happens to be the same three conjuncts,
        -- but Action.legalActions is what asks that of a land play, and an
        -- effect that moved it would have to say so.
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        -- CR 305.1's quality-bearing prohibition, the play-side twin of
        -- CantCastMatching below: City in a Bottle's list of names, and Damping
        -- Engine's `And []`, which every land matches.
        --
        -- No CR 601.3a lookahead beside it, where the cast side has one: a land
        -- play announces no choice that could move a characteristic, CR 305.1
        -- being a special action with no proposal to make.
        PlayerEffect.CantPlayLands criterion -> matchesObjectFor pid source criterion oid gs
        -- CR 305.1 once more: a permission naming the zone a SPELL may be cast
        -- from stops no land play, which is why Yawgmoth's Will's "you may play
        -- lands ... from your graveyard" is the arm below rather than this one.
        PlayerEffect.CastFrom _ -> False
        -- And the play-side permission allows rather than prohibits, so it is
        -- False here for the reason every permission is: this question is only
        -- ever "does something stop THIS land". mayPlayLandsFrom below is where
        -- the grant is read.
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        -- CR 305.1 again, in the other direction: a prohibition on CASTING says
        -- nothing about a special action, so Silence and Rule of Law leave a
        -- land play alone. CR 305.2's and CR 305.3's limits are the closed
        -- half's and are asked by Action.legalActions -- the first as a count
        -- (landPlaysAllowed below is only its left-hand side), the second as
        -- part of Turn.sorcerySpeedWindow. Neither is a question about WHICH
        -- land, which is all this one asks.
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        -- CR 305.2 raises HOW MANY lands may be played, never WHICH: a grant is
        -- no permission for a land this rule stops. landPlaysAllowed below is
        -- the gate that reads it.
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        -- CR 305.1 again: a land is never cast, so a permission about the timing
        -- of a CAST has nothing to widen here either.
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        -- CR 305.1 again: a land is never put on the stack, so nothing about
        -- countering reaches a land play.
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        -- CR 305.1 once more: Damping Engine's own cast half stops no land play,
        -- however its Filter reads -- which is exactly why its one printed
        -- sentence declares two abilities.
        PlayerEffect.CantCastMatching _ -> False
        -- CR 118.9's alternative cost says what a SPELL pays, and a land is
        -- never cast (CR 305.1) -- so this permission neither allows nor
        -- prohibits a land play, whatever its Filter reads.
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
   in any prohibits (applying pid gs)

-- CR 701.23: does any effect prohibit `pid` from searching `owner`'s library,
-- where the search is caused by a spell or ability `causeController` controls?
--
-- Three players and not one, because a printed prohibition narrows on both of
-- the other two axes: Leonin Arbiter says "libraries" and stops the searcher
-- whoever owns the library and whatever caused the look, while Ashiok, Dream
-- Render reaches only an opponent's own library and only a search that
-- opponent's own spell or ability caused. Both narrowings are read from the
-- PROHIBITED player, which is NOT CR 109.5's "you" -- that rule would make them
-- Ashiok's controller. See Pawl.Types.CantSearchLibraries.
--
-- The cause is the CONTROLLER of the object being followed (CR 405.4), which
-- Pawl.Engine.Resolve holds while it follows the instruction.
--
-- A DISJUNCTION for CR 101.2's reason.
prohibitsSearching :: PlayerId -> PlayerId -> PlayerId -> GameState -> Bool
prohibitsSearching pid owner causeController gs =
  let prohibits effect = case effect of
        PlayerEffect.CantSearchLibraries narrowing ->
          inScope owner pid gs (CantSearchLibraries.library narrowing)
            && inScope causeController pid gs (CantSearchLibraries.cause narrowing)
        -- CR 702.16 states no clause about searching, its consequences being
        -- targeting, enchanting, equipping, blocking and damage.
        PlayerEffect.HasProtectionFrom _ -> False
        -- Every other arm is about casting, playing, targeting, countering,
        -- paying, keeping mana, gaining or losing life, or how a coin flip came
        -- out. CR 701.23's search is
        -- an action a player takes
        -- while FOLLOWING an instruction that has already resolved, so none of
        -- them reaches it -- Silence stops the spell, never the search a
        -- resolved one performs.
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
   in any (prohibits . snd) (applying pid gs)

-- CR 101.2 with CR 122.1: does an effect in force right now say that `pid` CAN'T
-- GET counters of `kind`? Solemnity's first sentence and Melira, Sylvok
-- Outcast's first.
--
-- The PLAYER twin of Pawl.Engine.CounterRestriction.prohibited, and asked at the
-- same point in the same story: Pawl.Engine.Event.putPlayerCounters, after CR
-- 616.1's loop has settled the placement, about the SETTLED player and kind.
--
-- A DISJUNCTION for CR 101.2's reason: one effect saying it can't happen beats
-- every rule and effect that allows or directs it, so a second row permitting
-- nothing cannot undo the first.
prohibitsCounters :: PlayerId -> PlayerCounterKind.PlayerCounterKind -> GameState -> Bool
prohibitsCounters pid kind gs =
  let prohibits effect = case effect of
        -- Nothing is Solemnity's "counters", every kind; Just is Melira's "poison
        -- counters" and refuses that kind alone.
        PlayerEffect.CantGetCounters named -> Maybe.maybe True (== kind) named
        -- Every other arm is about casting, playing, targeting, countering,
        -- searching, paying, keeping mana, gaining or losing life, rule 702
        -- protection or how a coin flip came out. CR 122.1's counters are placed by an effect that has
        -- already resolved or by a rule, so none of them reaches one -- Silence
        -- stops the spell, never the counters a resolved one puts on a player.
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
   in -- CR 810.10c: a teammate who can't get poison counters stops the team,
      -- since the team shares them (Game.counterSharers).
      any (\member -> any (prohibits . snd) (applying member gs)) (Game.counterSharers kind pid gs)

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
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        -- Every other arm is about casting, playing, targeting, countering,
        -- searching, paying, keeping mana, gaining or losing life, rule 702
        -- protection or how a coin flip came out. CR 725.1's designation is none of
        -- those: it is handed out by a resolving effect or by rule 725.2 itself,
        -- and no prohibition on casting reaches an effect that has already
        -- resolved.
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        -- CR 702.18a/702.11c stop a SPELL from choosing this player as a target.
        -- CR 725.1's effect need not target to crown them (Palace Jailer's "you
        -- become the monarch" names nobody), so shroud is not an eligibility
        -- restriction; where a card does target (Jared's own first clause), CR
        -- 115.1's own check turns it away before this one is asked.
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
   in any (prohibits . snd) (applying pid gs)

-- CR 701.32 / 904.9 / 101.2: may no scheme be set in motion right now (All in
-- Good Time)? The printed sentence names no player, so it is gathered from
-- every still-playing seat, `unpreventable`'s posture: "some player has it
-- applying" is "it applies". Pawl.Engine.Archenemy.setInMotion is the reader.
schemesCantBeSetInMotion :: GameState -> Bool
schemesCantBeSetInMotion gs =
  let prohibits effect = case effect of
        PlayerEffect.CantSetSchemesInMotion -> True
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantAttackWithCreatures -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
   in any (\pid -> any (prohibits . snd) (applying pid gs)) (Game.stillPlaying gs)

-- CR 508.1c / 101.2: is `pid` forbidden from attacking with creatures? A
-- restriction on the PLAYER (Angelic Arbiter), read by
-- Pawl.Engine.CombatRestriction.cantAttack for every creature that player
-- controls. A DISJUNCTION for CR 101.2's reason.
prohibitsAttackingWithCreatures :: PlayerId -> GameState -> Bool
prohibitsAttackingWithCreatures pid gs =
  let prohibits effect = case effect of
        PlayerEffect.CantAttackWithCreatures -> True
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
   in any (prohibits . snd) (applying pid gs)

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
-- matched nothing whatever the board held; see #1242. A stored CR 611.2c row
-- names one too (ActivePlayerEffect.source), so Filter.IsSource and
-- Filter.IsHostOfSource are answerable under a stored row rather than vacuously
-- False -- which is what Lava Burst's self-naming redirection clause needs.
-- CR 303.4b's atom still answers False for the usual stored row, but by the
-- BOARD rather than by fiat: an instant's source is in a graveyard enchanting
-- nothing by the time the row is read, and its host is a live lookup
-- (Pawl.Engine.SourceContext) that finds none.
--
-- The source's HOST comes off the board here rather than riding in the row:
-- reading it live is what CR 611.2c asks for, so an Aura moved to another
-- creature taxes the new one from that moment.
matchesObjectFrom :: RowSource -> Filter Keyword -> ObjectId -> GameState -> Bool
matchesObjectFrom src filter_ oid gs =
  Filter.matches (contextFrom src oid gs) (Projection.viewOfObject oid gs) filter_

-- matchesObjectFrom with CR 109.5's "you" NAMED rather than read off the object.
--
-- Two callers: castPermissionsFrom, whose "you" is the grant's controller, and
-- prohibitsCasting's CR 601.3a arm, under one rule: a card being cast
-- has no controller yet (CR 108.4 gives a card in a hand none), so rule 109.5's
-- "you" for it is the player who would control the spell -- the caster. The two
-- coincide wherever the caster owns the card, which is why nothing observed the
-- difference until a permission could open somebody else's hand, see #2169: Drannith
-- Magistrate's "from anywhere other than their hand" is Filter.OwnedBy You, and
-- read at the OWNER's perspective that conjunct is vacuously true of every card
-- in every hand.
matchesObjectFor :: PlayerId -> RowSource -> Filter Keyword -> ObjectId -> GameState -> Bool
matchesObjectFor you src filter_ oid gs =
  Filter.matches (contextFor (Just you) src gs) (Projection.viewOfObject oid gs) filter_

-- The Context every match in this module is made against: CR 109.5's "you" is
-- the AFFECTED object's own controller, and the source is the row's.
contextFrom :: RowSource -> ObjectId -> GameState -> Filter.Context
contextFrom src oid gs = contextFor (Projection.controllerOf oid gs) src gs

-- contextFrom with the perspective supplied, which matchesObjectFor above is the
-- one caller of.
--
-- A printed row's CR 607.2d choices come from Projection.framedBy, read
-- through CR 608.2h's last known information. A stored row's are its own
-- instead, baked as it began (RowSource.choices): CR 608.2h determines them
-- once, so Cheering Fanatic naming a second card leaves the first row's alone
-- (the Cheering Fanatic scenario under data/scenarios/cost proves it).
--
-- The baked colour and subtype are a fence, not a proven behaviour: CR 614.1c's
-- as-enters choice (Pawl.Engine.Event's EntryRewrite arms) is the only writer of
-- either, so no source changes them under a stored row today.
contextFor :: Maybe PlayerId -> RowSource -> GameState -> Filter.Context
contextFor you row gs =
  let src = RowSource.object row
      frame s = Projection.framedWith (Maybe.fromMaybe (SourceContext.choicesOf s gs) (RowSource.choices row)) s gs
   in maybe id frame src (Filter.contextFor (Game.teams gs) you src)

-- CR 601.3a's LOOKAHEAD, asked of a prohibition that matches the spell as it
-- stands: could a choice still to be made during this spell's proposal cause the
-- criterion to stop naming it? A True here is the rule's "the player may begin to
-- cast the spell, ignoring the effect", so prohibitsCasting negates it.
--
-- ONE choice is SEARCHED for here, and X is that choice. CR 202.3e gives a
-- variable a contribution of zero off the stack, so a card's mana value in hand
-- is fixed while the spell's is not, and Void Winnower's "spells with even mana
-- values" lands on opposite sides of the two for an {X}{R}{R} card (Molten
-- Disaster).
--
-- The other choices that move a characteristic a Filter can read are each ASKED
-- with their own answers rather than searched for, and every one of them is an
-- announcement pawl has already made by the time this runs: flashback from a
-- graveyard and a face-down morph are their own Action (CR 601.2b's two named
-- pre-announcement choices), and CR 702.103b's bestow -- which really does rewrite
-- a card type and a subtype at CR 601.2b -- is judged one candidate at a time by
-- Pawl.Engine.Cast.castable, against the board CR 702.103d says to judge it on
-- (Cast.proposedFor). A colour and a supertype are fixed by the half and the
-- facing and move at no step of the announcement.
--
-- The other proposal choices reach nothing: CR 601.2b's modes, CR 601.2b's
-- targets and CR 601.2f's cost payments change no characteristic of the spell
-- this criterion can read.
--
-- Over the PRINTED MANA COST's variables, which is exactly where CR 202.3 reads
-- a mana value from -- an X in an additional cost buys the caster nothing here,
-- because it is not in the mana cost at all.
--
-- And only where X is the caster's to announce: CR 107.3b leaves 0 as the only
-- legal X for a spell cast while paying neither its mana cost nor an alternative
-- cost including X, so under that candidate there is no choice to search and
-- the printed mana value is the one the prohibition judges. Pawl.PlayerEffectSpec's
-- "CR 107.3b a free cast fixes X at 0, so the {X} spell is refused as even" is
-- the proof, off Omniscience.
--
-- Sampled by reachableManaValues below. Lenience to BEGIN only: CR 601.2e
-- judges the announced X, which prohibitsAtManaValue below asks.
choiceCouldEscape :: PlayerId -> RowSource -> Filter Keyword -> ObjectId -> VariableChoice.VariableChoice -> GameState -> Bool
choiceCouldEscape you src criterion oid variable gs =
  let variables = case variable of
        VariableChoice.Announced -> variablesIn oid gs
        VariableChoice.FixedAtZero -> 0
      -- The SAME context matchesObjectFor builds, and it has to be: this asks
      -- whether that match could flip, so a context that answered an atom
      -- differently would be asking about a different criterion -- the caster's
      -- perspective included.
      context = contextFor (Just you) src gs
      view = Projection.viewOfObject oid gs
      escapes base = any (\mv -> not (Filter.matches context view {Filter.manaValue = Just mv} criterion)) (reachableManaValues criterion variables base)
   in maybe False escapes (Filter.manaValue view)

-- CR 202.3e: the mana values a spell whose mana cost prints `variables` X's can
-- have once X is announced, from `base`, its mana value off the stack --
-- enough of them to see every verdict `criterion` can give. Empty when nothing
-- varies.
--
-- FINITE by Filter.manaValueThresholds' argument: two steps past the greatest
-- literal the criterion compares against, nothing but parity is left to change,
-- so `climb + 2` samples have seen every verdict. choiceCouldEscape above and
-- choiceCouldApply below are its two readers.
reachableManaValues :: Filter Keyword -> Integer -> Integer -> [Integer]
reachableManaValues criterion variables base
  | variables <= 0 = []
  | otherwise =
      let limit = 2 + maximum (0 : Filter.manaValueThresholds criterion)
          climb = if limit <= base then 0 else div (limit - base + variables - 1) variables
       in fmap (\x -> base + variables * x) [0 .. climb + 2]

-- How many variables the object's mana cost prints -- CR 107.3's {X}, counted
-- rather than tested, because a cost printing the symbol twice moves the mana
-- value in steps of two as X climbs, and a step of two never changes its parity.
--
-- The MANA COST FACES (CR 202.3b), which are the faces the mana value itself is
-- read from, so the count and the base it steps from come off the same faces --
-- all of them, since CR 202.3c reads a melded permanent's cost off two cards and
-- an {X} on either would move the sum.
variablesIn :: ObjectId -> GameState -> Integer
variablesIn oid gs = sum (fmap (maybe 0 Quantity.variablesOf . Face.manaCost) (Game.manaCostFacesOf oid gs))

-- CR 601.2e: the legality check choiceCouldEscape's lenience defers to. Once X
-- is announced the spell's mana value is fixed (CR 202.3e), so a quality-bearing
-- prohibition is asked again at `manaValue`, against the proposal's view of
-- `oid` otherwise. Only CantCastMatching, the one prohibition reading a quality X
-- moves; prohibitsCasting judged the rest at the proposal. Pawl.CastPermissionSpec's
-- "CR 601.2e an announced X that leaves the spell even takes the cast back" is
-- the proof, off Void Winnower.
prohibitsAtManaValue :: PlayerId -> ObjectId -> Integer -> GameState -> Bool
prohibitsAtManaValue pid oid manaValue gs =
  let view = (Projection.viewOfObject oid gs) {Filter.manaValue = Just manaValue}
      prohibits (source, effect) = case effect of
        PlayerEffect.CantCastMatching criterion -> Filter.matches (contextFor (Just pid) source gs) view criterion
        _ -> False
   in any prohibits (applying pid gs)

-- CR 601.2e / 601.3: prohibitsAtManaValue's permission-side twin. Does the CR
-- 601.3 permission the cast is made under still admit `oid` at `manaValue`?
-- Judged as castPermissionsFrom judged it, with only the mana value replaced;
-- Nothing, a cast needing no permission, always does. Pawl.CastPermissionSpec's
-- "CR 601.2e Serra Paragon admits Protean Hydra at X = 2 and not at X = 3" is
-- the proof.
--
-- The permission names only its source and grant, so the rows under that pair
-- are looked up again for their RowSource, which carries a stored row's baked
-- choices (CR 608.2h); any of them admitting it is enough. A pair no row answers
-- to any more falls back to the source read live. A fence: no card in the pool
-- stores a cast permission that reads a choice.
admitsAtManaValue :: PlayerId -> Maybe (ObjectId, CastFromZone.CastFromZone) -> ObjectId -> Integer -> GameState -> Bool
admitsAtManaValue pid permission oid manaValue gs = case permission of
  Nothing -> True
  Just (sid, grant) ->
    let view = (Projection.viewOfObject oid gs) {Filter.manaValue = Just manaValue}
        admits source = Filter.matches (contextFrom source oid gs) view (CastFromZone.matching grant)
        rows = [source | (source, PlayerEffect.CastFrom granted) <- applying pid gs, RowSource.object source == Just sid, granted == grant]
     in if null rows then admits (liveSource (Just sid)) else any admits rows

-- CR 613.11 / 601.2f: the cost increases, the cost reductions and the additional
-- non-mana components that apply to `pid` CASTING `oid` -- `modifierAdjustments`
-- over the rows whose subject is CostSubject.Spells and whose criterion matches
-- the spell. activationCostAdjustments below is the other subject.
--
-- matchesObjectFrom is called only for a ModifyCost row, so a board with no
-- Thalia and no Medallion runs no projections at all.
--
-- `targets` is CR 601.2c's announcement, which `whichTargets` and `perTarget`
-- read; empty for a caller standing before CR 601.2c.
spellCostAdjustments :: Set.Set Recipient.Recipient -> PlayerId -> ObjectId -> GameState -> CostAdjustments
spellCostAdjustments targets pid oid gs =
  let reaches source modifier = case CostModifier.subject modifier of
        -- Not implemented: a spell's `onlyFirst` (#4924), which
        -- Pawl.Codec.CostModifier refuses.
        CostSubject.Spells -> Maybe.isNothing (CostModifier.onlyFirst modifier) && matchesObjectFrom source (CostModifier.matching modifier) oid gs
        -- Oppressive Rays, turned away by the SUBJECT and not by its Filter
        -- -- the mirror of what keeps Thalia off an activation cost in
        -- activationCostAdjustmentsGiven below (#90).
        CostSubject.Activations _ -> False
   in modifierAdjustments
        [(source, modifier) | (source, PlayerEffect.ModifyCost modifier) <- applying pid gs, reaches source modifier]
        (\source wanted -> any (perTargetMatches pid source gs wanted) (Set.toList targets))
        (\source wanted -> perTargetCount targets pid source gs wanted)

-- CR 601.2f, once for both subjects (CR 602.2b): the adjustments the given
-- modifiers make, every one of them already known to reach the payment. `aims`
-- answers `whichTargets` -- SOME announced target matches, once however many do
-- (Kopala, Warden of Waves' ruling) -- and `counted` answers `perTarget`, the
-- distinct announced targets matching, which repeats the change.
--
-- A repeated reduction is its amount REPEATED rather than multiplied, as
-- Pawl.Engine.Cost.selfReductions repeats Thrasta's, so a typed amount needs no
-- arithmetic; a repeated addition is its components repeated. Each reduction
-- keeps its OWN floor and colour confinement (Pawl.Types.AppliedReduction),
-- which Pawl.Engine.Cost.applyAdjustments applies as that reduction lands.
--
-- Additions are CONCATENATED rather than resolved between, because two effects
-- each adding a cost both add it, and each keeps its SCALE for
-- Pawl.Engine.Cost.plusComponents to cash against the cost being adjusted.
modifierAdjustments :: [(RowSource, CostModifier.CostModifier)] -> (RowSource -> Filter Keyword -> Bool) -> (RowSource -> Filter Keyword -> Natural) -> CostAdjustments
modifierAdjustments rows aims counted =
  let applying' = [(source, modifier, maybe 1 (counted source) (CostModifier.perTarget modifier)) | (source, modifier) <- rows, maybe True (aims source) (CostModifier.whichTargets modifier)]
   in CostAdjustments.MkCostAdjustments
        { CostAdjustments.increases = [amount * times | (_, modifier, times) <- applying', CostChange.Increase amount <- [CostModifier.change modifier]],
          CostAdjustments.reductions =
            [ reduction {AppliedReduction.amount = ManaCost.MkManaCost (concat (List.genericReplicate times (ManaCost.unwrap (AppliedReduction.amount reduction))))}
            | (_, modifier, times) <- applying',
              CostChange.Reduce reduction <- [CostModifier.change modifier]
            ],
          -- A spell's own PRINTED additional costs are NOT among these: those are
          -- card text and arrive through Pawl.Engine.Cost.plus at CR 601.2b. What
          -- is gathered here is CR 118.8's other half, a cost applied from
          -- another effect.
          CostAdjustments.components =
            [ (CostAddition.scale addition, component)
            | (_, modifier, times) <- applying',
              CostChange.Add addition <- [CostModifier.change modifier],
              component <- concat (List.genericReplicate times (CostAddition.components addition))
            ]
        }

-- Whether a cost change applying to `pid` casting `oid` reads the spell's
-- targets (`whichTargets` or `perTarget`), so a gate standing before CR 601.2c
-- has to search the aimings (Pawl.Engine.Cost.readsTargets).
spellCostReadsTargets :: PlayerId -> ObjectId -> GameState -> Bool
spellCostReadsTargets pid oid gs =
  let readsTargets (source, effect) = case effect of
        PlayerEffect.ModifyCost modifier ->
          CostModifier.subject modifier == CostSubject.Spells
            && (Maybe.isJust (CostModifier.whichTargets modifier) || Maybe.isJust (CostModifier.perTarget modifier))
            && matchesObjectFrom source (CostModifier.matching modifier) oid gs
        _ -> False
   in any readsTargets (applying pid gs)

-- CR 601.2c / 601.2f: how many times a per-target cost change applies -- the
-- DISTINCT objects and players among the announced `targets` that `wanted`
-- matches, against each one's own view with the effect's source framing it and
-- `targetPerspective`'s "you". Distinct, not per instance of the word: CR
-- 601.2c's "the chosen objects and/or players each become a target", and Hinata,
-- Dawn-Crowned's ruling counts a creature chosen for two words once. So
-- Filter.View's `targetCount`, which counts instances, is not this. Merging by
-- referent is a fence: the announcement's union already merges a recipient named
-- twice, and no test names one object under two of CR 115.4's tags.
perTargetCount :: Set.Set Recipient.Recipient -> PlayerId -> RowSource -> GameState -> Filter Keyword -> Natural
perTargetCount targets pid source gs wanted =
  let referent r = (Recipient.objectOf r, Recipient.playerOf r)
   in Natural.length (Set.map referent (Set.filter (perTargetMatches pid source gs wanted) targets))

-- Does one announced target count towards a per-target change? perTargetCount's
-- question of a single recipient.
perTargetMatches :: PlayerId -> RowSource -> GameState -> Filter Keyword -> Recipient.Recipient -> Bool
perTargetMatches pid source gs wanted r =
  maybe False (\view -> Filter.matches (contextFor (Just (targetPerspective pid source gs)) source gs) view wanted) (Projection.viewOfRecipient (Projection.fullView gs) gs r)

-- CR 109.5: the "you" a cost modifier's TARGET criteria are read with -- the
-- controller of the row's source, which is Kopala, Warden of Waves' "a Merfolk
-- you control" under a row scoped to that controller's opponents. Not the
-- target's own controller, which would make Professor Hojo's "a creature you
-- control" true of every creature, and not the paying player, who is Kopala's
-- opponent. Pawl.CostSpec's kopalaSpec on three seats proves it.
--
-- The paying player where the source has no controller to read -- a stored
-- row whose spell has left the stack -- which coincides with the source's
-- controller for every such row in `data/cards/`, each scoped to "you". A fence,
-- not a proof.
targetPerspective :: PlayerId -> RowSource -> GameState -> PlayerId
targetPerspective pid source gs = Maybe.fromMaybe pid (RowSource.object source >>= \oid -> Projection.controllerOf oid gs)

-- Every question a cost change in force for `pid` asks of ONE target, answered:
-- its `whichTargets` and its `perTarget` (modifierAdjustments' `aims` and
-- `counted`). These are the only
-- reads the gathered adjustments make of CR 601.2c's targets, so two targets
-- answering alike are interchangeable to them (Pawl.Engine.Cost.aimingSignature).
-- Asked of every such change in force rather than only those reaching the cost
-- at hand: an extra question only splits targets further, which stays exact.
targetQuestions :: PlayerId -> GameState -> Recipient.Recipient -> [Bool]
targetQuestions pid gs r =
  let ask (source, effect) = case effect of
        PlayerEffect.ModifyCost modifier ->
          let matches wanted = case CostModifier.subject modifier of
                CostSubject.Spells -> perTargetMatches pid source gs wanted r
                CostSubject.Activations _ -> maybe False (\oid -> matchesObjectFor (targetPerspective pid source gs) source wanted oid gs) (Recipient.objectOf r)
           in fmap matches (Maybe.catMaybes [CostModifier.whichTargets modifier, CostModifier.perTarget modifier])
        _ -> []
   in concatMap ask (applying pid gs)

-- Professor Hojo's "the FIRST activated ability you activate during your turn"
-- and Kíli the Resourceful's "the first equip ability you activate each turn":
-- in a turn the scope admits, is there no activation by `pid` earlier this turn
-- matching the criteria? Reduced or not, and whether or not the effect asking
-- existed yet (Tezzeret, Betrayer of Flesh's ruling). Asked of
-- GameState.activationsThisTurn's snapshots rather than the live board, with the
-- criteria Pawl.Types.CostModifier carries; `source` is the
-- asking effect's own permanent, which Filter.IsSource reads.
firstActivation :: PlayerId -> RowSource -> Filter Keyword -> Maybe (KeywordDesignator.KeywordDesignator Keyword) -> Maybe AbilityKind.AbilityKind -> Maybe (Filter Keyword) -> GameState -> TurnScope.TurnScope -> Bool
firstActivation pid source criterion granted wantedKind aimedAt gs scope =
  let snapshotView snapshot =
        (Count.viewOfSnapshot False (ObjectSnapshot.controller snapshot) (Just (ObjectSnapshot.owner snapshot)) False Map.empty (ObjectSnapshot.characteristics snapshot))
          { Filter.identity = Just (ObjectSnapshot.object snapshot)
          }
      matchesSnapshot you criterion' snapshot = Filter.matches (contextFor you source gs) (snapshotView snapshot) criterion'
      admits past =
        PastActivation.activator past == pid
          && matchesSnapshot (ObjectSnapshot.controller (PastActivation.source past)) criterion (PastActivation.source past)
          && maybe True (\g -> any (Keyword.designates g) (PastActivation.keyword past)) granted
          && maybe True (== PastActivation.kind past) wantedKind
          && maybe True (\f -> any (matchesSnapshot (Just (targetPerspective pid source gs)) f) (PastActivation.targets past)) aimedAt
   in Turn.turnScopeAdmits gs scope (GameState.activePlayer gs) pid
        && not (any admits (GameState.activationsThisTurn gs))

-- CR 118.9 through CR 602.2b: the costs `pid` may pay RATHER THAN the
-- activation cost of an ability stamped `stamp` -- Kíli the Resourceful's {0}
-- for "the equip cost of the first equip ability you activate each turn".
-- Offered at CR 601.2b by Pawl.Engine.Activatable.costsFor; CR 118.9d's
-- increases and reductions then apply to whichever is chosen.
alternativeActivationCosts :: PlayerId -> Maybe Keyword -> GameState -> [ManaCost.ManaCost]
alternativeActivationCosts pid stamp gs =
  let offered (source, effect) = case effect of
        PlayerEffect.AlternativeActivationCost (AlternativeActivationCost.MkAlternativeActivationCost granted onlyFirst amount) ->
          if any (Keyword.designates granted) stamp && maybe True (firstActivation pid source (Filter.Type.And []) (Just granted) Nothing Nothing gs) onlyFirst
            then Just amount
            else Nothing
        _ -> Nothing
   in Maybe.mapMaybe offered (applying pid gs)

-- CR 613.11 / 601.2f through CR 602.2b: the cost increases, reductions and
-- additional components that apply to `pid` ACTIVATING an ability of `srcId`
-- -- `modifierAdjustments` over the rows whose subject is
-- CostSubject.Activations, the sibling of spellCostAdjustments above.
--
-- The criterion is matched against `srcId`, the ability's SOURCE PERMANENT
-- (Heartstone's "activated abilities of creatures"), projected rather than
-- printed (an animated Vehicle's abilities are a creature's). `stamp`, `kind`
-- and `loyalty` are the facts about the ABILITY that Pawl.Types.ActivationCriteria
-- asks: its rule-702 keyword (Fluctuator's "cycling abilities"), CR 605.1a's
-- classification (Zirda's "that aren't mana abilities") and CR 606.2's (Carth
-- the Lion's "loyalty abilities"). The caller answers them, because only the
-- caller has the ability -- Pawl.Engine.Cost.manaActivationAdjustmentsGiven is
-- the mana window and says so. Each is compared and never inspected further: a
-- rulebook citation or classification, never what the ability does.
--
-- `targets` arrives from a different MOMENT: CR 601.2c's announced targets,
-- which `whichTargets` and `perTarget` read (Dwarven Mauler, Kopala). A caller
-- that has not reached CR 601.2c hands the empty set -- see
-- Pawl.Engine.Activate.activateAbility, which gathers once before the targets
-- exist and again after -- and a caller that has to MEASURE the cost before
-- that moment searches the aimings (Pawl.Engine.Activatable.aimingSomewhere).
activationCostAdjustments :: Set.Set ObjectId -> Maybe Keyword -> AbilityKind.AbilityKind -> LoyaltyKind.LoyaltyKind -> PlayerId -> ObjectId -> GameState -> CostAdjustments
activationCostAdjustments targets stamp kind loyalty pid srcId gs = activationCostAdjustmentsGiven (applying pid gs) pid targets stamp kind loyalty srcId gs

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
activationCostAdjustmentsGiven :: [(RowSource, PlayerEffect)] -> PlayerId -> Set.Set ObjectId -> Maybe Keyword -> AbilityKind.AbilityKind -> LoyaltyKind.LoyaltyKind -> ObjectId -> GameState -> CostAdjustments
activationCostAdjustmentsGiven effects pid targets stamp kind loyalty srcId gs =
  let -- CR 601.2c's chosen targets: Dwarven Mauler's "equip abilities you
      -- activate THAT TARGET THIS CREATURE", Kopala's "that target a Merfolk you
      -- control". False on an empty set -- an ability that targets nothing is
      -- not an ability that targets this creature.
      --
      -- Asked against the TARGET's projection with the modifier's own permanent
      -- as Pawl.Engine.Filter's source, which is what makes Filter.IsSource read
      -- "this creature" here, and with `targetPerspective`'s "you".
      matchingTargets source wanted = filter (\oid -> matchesObjectFor (targetPerspective pid source gs) source wanted oid gs) (Set.toList targets)
      -- The source filter, then the three facts about the ABILITY being
      -- activated that no source filter can answer (Pawl.Types.ActivationCriteria):
      -- its rule-702 keyword off `stamp`, CR 605.1a's `kind` and CR 606.2's
      -- `loyalty`, each compared and never inspected further. Then Professor
      -- Hojo's "first", asked with the same criteria.
      reaches source modifier = case CostModifier.subject modifier of
        CostSubject.Activations criteria ->
          let granted = ActivationCriteria.grantedBy criteria
              wantedKind = ActivationCriteria.whichKind criteria
              criterion = CostModifier.matching modifier
           in matchesObjectFrom source criterion srcId gs
                && maybe True (\g -> any (Keyword.designates g) stamp) granted
                && maybe True (== kind) wantedKind
                && maybe True (== loyalty) (ActivationCriteria.whichLoyalty criteria)
                && maybe True (firstActivation pid source criterion granted wantedKind (CostModifier.whichTargets modifier) gs) (CostModifier.onlyFirst modifier)
        -- Thalia and Sapphire Medallion, turned away by the SUBJECT and not by
        -- their Filters, which is what keeps a noncreature permanent's activated
        -- ability untaxed (#90).
        CostSubject.Spells -> False
      gathered =
        modifierAdjustments
          [(source, modifier) | (source, PlayerEffect.ModifyCost modifier) <- effects, reaches source modifier]
          (\source wanted -> not (null (matchingTargets source wanted)))
          (\source wanted -> Natural.length (matchingTargets source wanted))
      -- CR 702.193a/b: power-up's own reduction by "this permanent's mana cost",
      -- the rule's and not an effect's, so it is asked of the ability's stamp
      -- rather than gathered off a row. Read off the projection, so a copy
      -- reduces by the mana cost it copied (CR 707.2); CR 107.3g's {X} is 0.
      -- Pawl.ActivateSpec's "Power-up (CR 702.193)" group proves it.
      ruleReduction
        | any Keyword.reducesByManaCostOnEntry stamp && matchesObjectFrom (liveSource (Just srcId)) Filter.Type.EnteredThisTurn srcId gs =
            [ AppliedReduction.MkAppliedReduction (ManaCost.MkManaCost (filter (/= ManaSymbol.Variable) (ManaCost.unwrap manaCost))) 0 False
            | manaCost <- Maybe.maybeToList (PC.manaCost (Projection.project srcId gs))
            ]
        | otherwise = []
   in gathered {CostAdjustments.reductions = ruleReduction <> CostAdjustments.reductions gathered}

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
-- CR 601.3b's SECOND SENTENCE rides the same arm, in choiceCouldApply below: the
-- criterion is asked of the spell as it stands AND of what a choice still to be
-- made during the proposal could make of it.
--
-- Takes the OBJECT and not the half being proposed, and reaches the half through
-- the STATE instead: Pawl.Engine.Cast.castable asks this of an
-- asProposed-stamped state, so Projection.viewOfObject shows the chosen half
-- rather than CR 709.4's combined view. The posture spellCostAdjustments takes,
-- through the same matchesObject.
mayCastAsThoughItHadFlash :: PlayerId -> ObjectId -> GameState -> Bool
mayCastAsThoughItHadFlash pid oid gs = any (grantReaches castFlashGrant oid gs) (applying pid gs)

-- CR 601.1a / 601.3b: may `pid` PLAY `oid` as though it had flash -- the
-- widened window Pawl.Engine.Action.landTimingOk reads beside
-- Cast.flashOn for CR 116.2a's land-play window, exactly as
-- mayCastAsThoughItHadFlash above is what Cast.timingOk reads beside
-- Cast.instantSpeed for a cast.
--
-- Reads ONLY MayPlayAsThoughItHadFlash, and not CastAsThoughItHadFlash beside
-- it: CR 601.3b's own rule is about beginning to CAST (Vedalken Orrery), and a
-- land is never cast (CR 305.1), so a cast-scoped grant moves no land-play
-- window. mayCastAsThoughItHadFlash above is the one place a play-scoped grant
-- widens a CAST; this is the one place it widens a land PLAY, and neither
-- folds into the other.
mayPlayAsThoughItHadFlash :: PlayerId -> ObjectId -> GameState -> Bool
mayPlayAsThoughItHadFlash pid oid gs = any (grantReaches landPlayFlashGrant oid gs) (applying pid gs)

-- CR 602.5d / 602.5e: may `pid` activate an ability under this rule-702 keyword
-- any time they could cast an instant (Leonin Shikari's "equip abilities")?
-- Compared against Pawl.Types.ActivatedAbility.keyword, as prohibitsActivating
-- above is, so reconfigure -- no equip ability, per Forge Anew's ruling -- is
-- not reached. A disjunction, for mayCastAsThoughItHadFlash's reason.
activatesKeywordAtInstantSpeed :: Maybe Keyword -> PlayerId -> GameState -> Bool
activatesKeywordAtInstantSpeed stamp pid gs =
  let grants effect = case effect of
        PlayerEffect.ActivateKeywordAtInstantSpeed designator -> any (Keyword.designates designator) stamp
        _ -> False
   in any (grants . snd) (applying pid gs)

-- CR 606.3: may `pid` activate the loyalty abilities of `srcId` any time they
-- could cast an instant (The Wandering Emperor)? The criterion is asked of the
-- permanent, from the effect's source, so `IsSource` is "her loyalty abilities".
activatesLoyaltyAtInstantSpeed :: PlayerId -> ObjectId -> GameState -> Bool
activatesLoyaltyAtInstantSpeed pid srcId gs =
  let grants (source, effect) = case effect of
        PlayerEffect.ActivateLoyaltyAtInstantSpeed criterion -> matchesObjectFrom source criterion srcId gs
        _ -> False
   in any grants (applying pid gs)

-- One row of `applying`, asked whether the grant `grantOf` reads off it reaches
-- `oid`: CR 601.3b's first sentence through matchesObjectFrom, and its second --
-- what a choice still to be made during the proposal could make of the card --
-- through choiceCouldApply.
grantReaches :: (PlayerEffect -> Maybe (Filter Keyword)) -> ObjectId -> GameState -> (RowSource, PlayerEffect) -> Bool
grantReaches grantOf oid gs (source, effect) =
  maybe False (\criterion -> matchesObjectFrom source criterion oid gs || choiceCouldApply source criterion oid gs) (grantOf effect)

-- CR 601.1a / 601.3b: the criterion a grant on the "as though it had flash"
-- axis applies to a LAND PLAY, or Nothing. Only the play-scoped grant (Scout's
-- Warning) reaches a land: CR 601.3b's own rule is about beginning to CAST
-- (Vedalken Orrery), and a land is never cast (CR 305.1). Exhaustive, so a new
-- PlayerEffect is named here by -Werror; castFlashGrant below leans on that.
landPlayFlashGrant :: PlayerEffect -> Maybe (Filter Keyword)
landPlayFlashGrant effect = case effect of
  PlayerEffect.MayPlayAsThoughItHadFlash criterion -> Just criterion
  PlayerEffect.ActivateKeywordAtInstantSpeed _ -> Nothing
  PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> Nothing
  PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
  PlayerEffect.CantCastSpells -> Nothing
  PlayerEffect.CantActivateAbilities _ -> Nothing
  PlayerEffect.CantCastMoreThan _ -> Nothing
  PlayerEffect.ModifyCost {} -> Nothing
  PlayerEffect.AlternativeActivationCost {} -> Nothing
  PlayerEffect.PlayAdditionalLands _ -> Nothing
  PlayerEffect.NoMaximumHandSize -> Nothing
  PlayerEffect.SetMaximumHandSize _ -> Nothing
  PlayerEffect.IncreaseMaximumHandSize _ -> Nothing
  PlayerEffect.ReduceMaximumHandSize _ -> Nothing
  PlayerEffect.DontLoseUnspentMana _ -> Nothing
  PlayerEffect.LoseLifeForUnspentMana -> Nothing
  PlayerEffect.SpendManaAsThough _ -> Nothing
  PlayerEffect.CantBeTargetedBy _ -> Nothing
  PlayerEffect.CantBeCountered _ -> Nothing
  PlayerEffect.DamageCantBePrevented _ -> Nothing
  PlayerEffect.DamageCantBeRedirected _ -> Nothing
  PlayerEffect.CantSearchLibraries _ -> Nothing
  PlayerEffect.HasProtectionFrom _ -> Nothing
  PlayerEffect.CantBecomeMonarch -> Nothing
  PlayerEffect.CantSetSchemesInMotion -> Nothing
  PlayerEffect.CantAttackWithCreatures -> Nothing
  PlayerEffect.CantCastMatching _ -> Nothing
  PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
  PlayerEffect.CantPlayLands _ -> Nothing
  PlayerEffect.CastFrom _ -> Nothing
  PlayerEffect.PlayLandsFrom _ -> Nothing
  PlayerEffect.PlotFrom _ -> Nothing
  PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
  PlayerEffect.CantGetCounters _ -> Nothing
  PlayerEffect.StateCoinFlip _ -> Nothing
  PlayerEffect.ModifyDieRoll _ -> Nothing
  PlayerEffect.AdditionalVotes _ -> Nothing
  PlayerEffect.AdditionalSurveilCards _ -> Nothing
  PlayerEffect.CantGainLife -> Nothing
  PlayerEffect.CantLoseLife -> Nothing

-- The same axis for a CAST: the cast-scoped grant, and -- CR 601.1a, casting
-- being one way to play a card -- every grant landPlayFlashGrant admits. The
-- fallthrough is to that EXHAUSTIVE function, so a new constructor still owes an
-- arm.
castFlashGrant :: PlayerEffect -> Maybe (Filter Keyword)
castFlashGrant effect = case effect of
  PlayerEffect.CastAsThoughItHadFlash criterion -> Just criterion
  _ -> landPlayFlashGrant effect

-- CR 601.3b's LOOKAHEAD, asked of a permission that does NOT name the spell as it
-- stands: could a choice still to be made during this spell's proposal cause the
-- criterion to start naming it? A True here is the rule's "that player may begin
-- to cast that spell as though it had flash", so mayCastAsThoughItHadFlash above
-- takes it as a disjunct. CR 601.3a's twin, choiceCouldEscape above, is the same
-- question asked of a prohibition and answered in the other direction.
--
-- THREE choices can do it in pawl. Bestow is one: CR 702.103b makes a
-- spell cast bestowed an Aura enchantment with enchant creature, which is a card
-- type and a subtype a criterion can read, and rule 601.3b's own example is that
-- card in that hand. Projection.bestowedView is what the choice would make of the
-- object, applied to no state -- nothing is stamped, since the player has not
-- chosen anything yet.
--
-- X is the other, CR 202.3e's: the card in hand counts X as zero, so a
-- permission naming a mana value it lacks may still name the spell, sampled as
-- choiceCouldEscape samples it. Pawl.CastPermissionSpec's "CR 601.3b Untimely
-- Aluren lets Protean Hydra begin on the opponent's turn" is the proof;
-- Cast.flashRefusedAt judges the X announced.
--
-- Prototype is the third: CR 718.3a/b and 601.3e judge a prototyped spell by its
-- inset cost, and so its colour and mana value. The hypothetical stamps
-- Object.prototyped onto a state that is only read, which is how
-- Projection.View.withPrototype is asked; a card with no prototype views the
-- same either way. Pawl.CastPermissionSpec's "CR 601.3b / 718.3b Yeva lets Rust
-- Goliath begin prototyped on the opponent's turn" is the proof.
--
-- A HYPOTHETICAL here where CR 601.3a's prohibition side takes the stamp instead,
-- and the difference is which question the caller is asking. Pawl.Engine.Cast
-- offers one action per (half, facing) pair and asks THIS gate once for it, since
-- rule 601.3b's permission is about when the proposal may begin rather than about
-- any one candidate; the prohibition is asked per candidate, so it can afford to
-- name the board each announcement would produce.
--
-- Morph reaches this and needs none of it: CR 708.4 makes a face-down cast a
-- proposal of its own, so Cast.castableSpells offers it as a separate action and
-- Cast.asProposed has already stamped the facing by the time this is asked. The
-- face-down spell's own characteristics are what the criterion above matches,
-- which is why the choice space this searches holds only the choices made DURING
-- one proposal.
--
-- Asks only whether the choice EXISTS, not whether its cost is payable, which is
-- choiceCouldEscape's posture toward X: the rule lets the player BEGIN.
-- Cast.castable's own affordability conjunct is what refuses a card no cost of
-- which can be paid, and Cast.windowedCandidates withholds a cost whose own
-- choice leaves the permission not naming the spell (flashNamesCandidate).
--
-- Read off the PROJECTION's keywords, which is where Cost.costsFor reads the same
-- ability from and for its CR 613.1 reason: a bestow granted where the card lies
-- offers rule 702.103a's choice as much as a printed one does.
choiceCouldApply :: RowSource -> Filter Keyword -> ObjectId -> GameState -> Bool
choiceCouldApply src criterion oid gs =
  let bestowable = not (null (Keyword.bestowCosts (Map.keysSet (Projection.keywordsOf oid gs))))
      prototyped = gs {GameState.objects = Map.adjust (\o -> o {Object.prototyped = True}) oid (GameState.objects gs)}
   in (bestowable && Filter.matches (contextFrom src oid gs) (Projection.bestowedView oid gs) criterion)
        || xCouldApply src criterion oid VariableChoice.Announced gs
        || matchesObjectFrom src criterion oid prototyped

-- choiceCouldApply's X half: could some X the candidate lets its caster announce
-- (CR 107.3b fixes it at 0 under FixedAtZero) make `criterion` name `oid`?
xCouldApply :: RowSource -> Filter Keyword -> ObjectId -> VariableChoice.VariableChoice -> GameState -> Bool
xCouldApply src criterion oid variable gs =
  let context = contextFrom src oid gs
      view = Projection.viewOfObject oid gs
      variables = case variable of
        VariableChoice.Announced -> variablesIn oid gs
        VariableChoice.FixedAtZero -> 0
      announceable base = any (\mv -> Filter.matches context view {Filter.manaValue = Just mv} criterion) (reachableManaValues criterion variables base)
   in maybe False announceable (Filter.manaValue view)

-- CR 601.3b asked of ONE candidate, on the board its own choice produces
-- (Cast.proposedFor): does a cast-scoped flash grant name the spell that
-- candidate makes, searching only the X still to be announced? The choice the
-- candidate stands for has been made, so bestow is not searched again.
-- Pawl.Engine.Cast.windowedCandidates is the caller.
flashNamesCandidate :: PlayerId -> ObjectId -> VariableChoice.VariableChoice -> GameState -> Bool
flashNamesCandidate pid oid variable gs =
  let names (source, effect) = maybe False (\criterion -> matchesObjectFrom source criterion oid gs || xCouldApply source criterion oid variable gs) (castFlashGrant effect)
   in any names (applying pid gs)

-- CR 601.2e / 601.3b: does a grant on castFlashGrant's axis applying to `pid`
-- name `oid` at `manaValue`? mayCastAsThoughItHadFlash with the mana value
-- replaced and no lookahead, since the choices it searched have been made.
-- Pawl.Engine.Cast.flashRefusedAt is the one caller.
mayCastAsThoughItHadFlashAt :: PlayerId -> ObjectId -> Integer -> GameState -> Bool
mayCastAsThoughItHadFlashAt pid oid manaValue gs =
  let view = (Projection.viewOfObject oid gs) {Filter.manaValue = Just manaValue}
      admits (source, effect) = maybe False (Filter.matches (contextFrom source oid gs) view) (castFlashGrant effect)
   in any admits (applying pid gs)

-- CR 400.1: whose copies of a zone a reference inside a permission applying to
-- `pid` names. The perspective is the AFFECTED player and not the row's
-- controller, which is what makes PlayerRef.Relative You read as "your graveyard"
-- for whoever the permission reached: Yawgmoth's Will affects its controller
-- alone, and a grant to the whole table would mean each player's own pile.
--
-- Pawl.Engine.Players.named's reading over no bindings, judged from the
-- permitted player over every player still in the game. The slot-reading arms name nobody, their bindings being gone by the time a
-- stored row is read (Pawl.Engine.Resolve bakes them while they are still
-- there), and so do the excluding arms -- a permission is the direction where
-- widening on an unanswerable reference reads WEAKER than printed. Candidate
-- names nobody, no fold running here. A permission naming nobody opens
-- nothing.
zoneOwners :: PlayerId -> PlayerRef.PlayerRef -> GameState -> [PlayerId]
zoneOwners pid ref gs =
  Maybe.fromMaybe [] $
    Players.named
      Players.MkReads
        { Players.perspective = Just pid,
          Players.bound = False,
          Players.slotPlayers = const Nothing,
          Players.slotObject = const Nothing,
          Players.controllerOf = (`Projection.controllerOf` gs),
          Players.ownerOf = fmap Object.owner . (`Game.lookupObject` gs),
          Players.roster = Game.stillPlaying gs,
          Players.reaches = const True
        }
      gs
      ref

-- Does this permission's zone reference name the zone `oid` lies in, and the
-- player whose copy of it that is?
--
-- CR 400.1 gives each player their own copy of such a zone and CR 400.3 keeps a
-- card in its owner's, so the object's owner is the seat
-- compared -- and the comparison is this function's rather than
-- Pawl.Engine.Cast.zoneCandidates', which offers every player's copy; see #2169.
opensZoneOf :: PlayerId -> Zone.Zone -> ObjectId -> InZone.InZone -> GameState -> Bool
opensZoneOf pid zone oid inZone gs =
  InZone.zone inZone == zone
    && maybe False (\obj -> elem (Object.owner obj) (zoneOwners pid (InZone.player inZone) gs)) (Game.lookupObject oid gs)

-- CR 601.3: may `pid` cast `oid` from this zone because an EFFECT says so?
--
-- The typed question Pawl.Engine.Cast.castableZones asks beside the
-- object-scoped Pawl.Types.CastingPermission.CastFromGraveyard it already read,
-- so that module never sees a PlayerEffect constructor and neither permission is
-- expressed in terms of the other. One card carrying flashback and one player
-- holding Yawgmoth's Will's grant are two rules (CR 702.34a, CR 601.3) reaching
-- the same gate.
--
-- Asks WHOSE copy, which the candidate list used to answer: zoneCandidates offers
-- every player's hand and graveyard so that a permission can name somebody else's
-- (Sen Triplets), and the owner conjunct moved here with it; see #2169. Yawgmoth's
-- Will writes PlayerRef.Relative You and reaches no other graveyard for it.
--
-- Says nothing about the TOP of a library, which stays Pawl.Engine.Cast.pileCandidates'
-- half of the question: that hands this the top card alone, so a permission worded
-- "from the top of your library" (Garruk's Horde) comes out of the two together. The
-- object-scoped Pawl.Types.CastingPermission.CastFromLibraryWhileSearching is
-- emphatically NOT read here -- Panglacial Wurm's permission is scoped to a
-- search in progress (Pawl.Engine.Cast.castableWhileSearching), and reading it
-- here would let it be cast off the top at any time.
--
-- A DISJUNCTION, for the reason Pawl.Types.PlayerEffect.CastFrom gives: one
-- applicable permission is enough, so CR 613.11's timestamp order has nothing to
-- order.
--
-- matchesObject is called only from inside the arm that already matched, so a
-- board with no such effect on it runs no projections at all -- the posture
-- mayCastAsThoughItHadFlash takes above.
--
-- Takes the OBJECT and not the half being proposed, and reaches the half through
-- the STATE, exactly as mayCastAsThoughItHadFlash does above: the callers stamp
-- the proposal through Pawl.Engine.Cast.asProposed first, so the same
-- matchesObject reads the chosen half rather than CR 709.4's combined view.
mayCastFrom :: PlayerId -> Zone.Zone -> ObjectId -> GameState -> Bool
mayCastFrom pid zone oid gs = not (null (castPermissionsFrom pid zone oid gs))

-- The permissions mayCastFrom answers off: every CR 601.3 CastFrom row applying
-- to `pid` that opens `zone` for `oid` and has a use left, paired with the object
-- that granted it, in `applying`'s timestamp order.
--
-- A row whose limit is spent is ABSENT rather than flagged, which is what makes
-- one function serve both the gate and the spend: castPermissionOptions below
-- reads this same list, so the permission the cast is offered under and the one
-- it consumes cannot disagree.
castPermissionsFrom :: PlayerId -> Zone.Zone -> ObjectId -> GameState -> [(Maybe ObjectId, CastFromZone.CastFromZone)]
castPermissionsFrom pid zone oid gs =
  let allows (source, effect) = case effect of
        PlayerEffect.CastFrom grant ->
          opensZoneOf pid zone oid (CastFromZone.from grant) gs
            && inPool (RowSource.object source) (CastFromZone.pool grant)
            && matchesObjectFor (holder (RowSource.object source)) source (CastFromZone.matching grant) oid gs
            && unspentPermission pid (RowSource.object source) grant gs
        -- The other CR 601.3 permission on this axis names a TIME, and this
        -- question is about a ZONE.
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        -- A PROHIBITION, and CR 601.3 asks the two halves separately:
        -- prohibitsCasting above is where Damping Engine and Silence are read.
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        -- The play-side twin can name the same ZONE and still answers nothing
        -- here: CR 305.1 makes playing a land a special action, so a grant to
        -- play lands from a graveyard permits no cast (Crucible of Worlds lets
        -- nobody cast anything).
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        -- A COST and not a permission at all: Omniscience says what a spell
        -- pays, never where it may be cast from, so it opens no zone.
        -- mayCastFromHandWithoutPayingManaCost below is its one reader, and CR
        -- 601.3's permission is still owed separately.
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
      grantOf (source, effect) = case effect of
        PlayerEffect.CastFrom grant -> Just (RowSource.object source, grant)
        _ -> Nothing
      -- CR 109.5: the Filter's "you" is the granting ability's controller, not
      -- the card's -- an exiled card's is its owner (CR 108.4a), which would
      -- make Dawnhand Dissident's "cards you own" true of every card.
      holder source = Maybe.fromMaybe pid (source >>= \sid -> Projection.controllerOf sid gs)
      -- CR 607.2a: the linked set is the cards GameState.exiledWith files
      -- against this very source object.
      inPool source pool = case pool of
        PermissionPool.EveryCard -> True
        PermissionPool.CardsExiledWithSource -> Maybe.isJust source && fmap ExileLink.source (Map.lookup oid (GameState.exiledWith gs)) == source
   in Maybe.mapMaybe grantOf (filter allows (applying pid gs))

-- CR 601.3: has this permission a use left this turn? Unlimited always; a
-- once-each-turn one only while GameState.castPermissionsUsedThisTurn does not
-- already record it under the object that granted it, and a once-each-of-your-
-- turns one only on `pid`'s own turn as well, its team's under CR 805.4a.
--
-- A row carrying NO source answers True and is never spent. Unreachable rather
-- than a policy: `applying` stamps every printed row with its permanent and
-- every stored one with ActivePlayerEffect.source, so the Maybe has no inhabitant
-- here.
unspentPermission :: PlayerId -> Maybe ObjectId -> CastFromZone.CastFromZone -> GameState -> Bool
unspentPermission pid source grant gs =
  let unspent = case source of
        Nothing -> True
        Just sid -> Set.notMember grant (Map.findWithDefault Set.empty sid (GameState.castPermissionsUsedThisTurn gs))
   in case CastFromZone.limit grant of
        PermissionLimit.Unlimited -> True
        PermissionLimit.OnceEachTurn -> unspent
        PermissionLimit.OnceEachOfYourTurns -> Turn.isActive gs pid && unspent

-- CR 601.3 / 400.7h: the ways a cast of `oid` out of `zone` can be made, for
-- Pawl.Engine.Cast.choosePlayPermission. Asked of the PRE-MOVE state by
-- Pawl.Engine.Cast.castSpellWith, so the permissions offered are the ones the
-- gate read, and spent only once the announcement has succeeded, so a rejected
-- or reversed cast (CR 601.2e, CR 733.1) spends nothing.
--
-- Open without any of them where something else is CR 601.3's "rule or effect"
-- allowing the cast: the rules for a card in its owner's hand, and an effect
-- offering the cast during resolution or a permission an exiled card carries for
-- itself (`offered`, CR 608.2g; Pawl.Engine.Cast.castWays). A permission the
-- OBJECT carries is not weighed here: every one pawl
-- has is a keyword's, offering its own CR 601.2b candidate (escape, flashback),
-- so the candidate the cast chose says whether it was made under it --
-- Pawl.Engine.Cast.castProposed asks nothing then, and
-- Pawl.CastPermissionSpec's "CR 702.138a an escaped Chimera takes neither Serra
-- Paragon's use nor its rider" proves it.
castPermissionOptions :: (ObjectId -> Bool) -> Bool -> PlayerId -> Zone.Zone -> ObjectId -> GameState -> [Maybe (ObjectId, CastFromZone.CastFromZone)]
castPermissionOptions rides offered pid zone oid gs =
  let owned = fmap Object.owner (Game.lookupObject oid gs) == Just pid
   in permissionOptions rides (offered || (zone == Zone.Hand && owned)) (castPermissionsFrom pid zone oid gs)

-- CR 601.3 / 305.1: the ways a play admitted by `usable` can be made, one per
-- distinguishable outcome -- Nothing, where `open` says the play needs none of
-- them, then each permission, the unlimited ones before the budgeted ones and
-- each group in timestamp order. Two ways are ONE where they spend the same
-- budget, give the same rider (`rides` answers whether a source's permission
-- gives one, Pawl.Engine.Event.permissionRiders), add the same costs (CR
-- 118.8) and take off the same reduction (CR 601.2f), and the first is kept: a riderless, costless unlimited permission
-- beside an open zone changes nothing.
-- So the head spends nothing wherever something admits the play for free.
--
-- Every budgeted permission is a way of its own, two copies of one card
-- included: whose use is spent shows once the other leaves the battlefield.
permissionOptions :: (ObjectId -> Bool) -> Bool -> [(Maybe ObjectId, CastFromZone.CastFromZone)] -> [Maybe (ObjectId, CastFromZone.CastFromZone)]
permissionOptions rides open usable =
  let rows = [(sid, grant) | (Just sid, grant) <- usable]
      (unlimited, limited) = List.partition ((== PermissionLimit.Unlimited) . CastFromZone.limit . snd) rows
      key option = (permissionSpent option, option >>= \(sid, _) -> if rides sid then Just sid else Nothing, permissionCosts option, permissionReductions option)
   in ListUtils.nubOrdOn key ([Nothing | open] <> fmap Just (unlimited <> limited))

-- CR 118.8: the additional costs a cast made under `option` pays on top of its
-- other costs -- none for a cast made under no permission.
permissionCosts :: Maybe (ObjectId, CastFromZone.CastFromZone) -> [CostComponent.CostComponent Keyword]
permissionCosts = foldMap (CastFromZone.additionalCosts . snd)

-- CR 601.2f: the reduction a spell cast under `option` takes -- Urianger
-- Augurelt's "spells you cast this way cost {2} less", generic mana, in
-- Pawl.Types.CandidateCost's `reductions` shape -- and none for a cast made
-- under no permission.
permissionReductions :: Maybe (ObjectId, CastFromZone.CastFromZone) -> [ManaCost.ManaCost]
permissionReductions option =
  [ ManaCost.MkManaCost [ManaSymbol.Generic n]
  | Just (_, grant) <- [option],
    let n = CastFromZone.reduction grant,
    n > 0
  ]

-- The budget a play made under `option` spends: a once-each-turn permission's,
-- and nothing for an unlimited one or for none.
permissionSpent :: Maybe (ObjectId, CastFromZone.CastFromZone) -> Maybe (ObjectId, CastFromZone.CastFromZone)
permissionSpent option = case option of
  Just (_, grant) | CastFromZone.limit grant /= PermissionLimit.Unlimited -> option
  _ -> Nothing

-- Spend the permission permissionSpent named, once the play it was asked about
-- has happened -- `consume`'s twin one axis over, and cleared at the turn
-- handoff (Pawl.Engine.Engine.beginTurnOf) rather than by any reader.
spendCastPermission :: Maybe (ObjectId, CastFromZone.CastFromZone) -> GameState -> GameState
spendCastPermission spent gs = case spent of
  Nothing -> gs
  Just (sid, grant) -> gs {GameState.castPermissionsUsedThisTurn = Map.insertWith Set.union sid (Set.singleton grant) (GameState.castPermissionsUsedThisTurn gs)}

-- CR 305.1 / 601.3: the Play-verb permissions (Pawl.Types.PermissionVerb) that
-- let `pid` play the land `oid` out of `zone` -- castPermissionsFrom's rows
-- narrowed to the ones that name a land play, so the budget the two sides spend
-- is one key.
landPermissionsFrom :: PlayerId -> Zone.Zone -> ObjectId -> GameState -> [(Maybe ObjectId, CastFromZone.CastFromZone)]
landPermissionsFrom pid zone oid gs = filter (\(_, grant) -> CastFromZone.verb grant == PermissionVerb.Play) (castPermissionsFrom pid zone oid gs)

-- The (zone, owner) piles some Play-verb permission opens to `pid`, for
-- Pawl.Engine.Action.playableLands to draw candidates from; which of their
-- lands it admits is landPermissionsFrom's per-card answer.
playPermissionPiles :: PlayerId -> GameState -> [(Zone.Zone, PlayerId)]
playPermissionPiles pid gs =
  [ (InZone.zone inZone, owner)
  | (_, PlayerEffect.CastFrom grant) <- applying pid gs,
    CastFromZone.verb grant == PermissionVerb.Play,
    let inZone = CastFromZone.from grant,
    owner <- zoneOwners pid (InZone.player inZone) gs
  ]

-- CR 305.1 / 400.7i: the ways a play of the land `oid` can be made, asked of
-- the PRE-MOVE state for castPermissionOptions' reason. Open without any
-- Play-verb permission where the land is in its owner's hand, a PlayLandsFrom
-- grant (Crucible of Worlds) opens its pile at no cost, or the land's own
-- costless mayhem admits it (mayPlayByMayhem) -- so a once-each-turn permission
-- (Serra Paragon) standing beside mayhem is a choice, not a spend.
landPermissionOptions :: (ObjectId -> Bool) -> PlayerId -> ObjectId -> GameState -> [Maybe (ObjectId, CastFromZone.CastFromZone)]
landPermissionOptions rides pid oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj ->
    let zone = Object.zone obj
        free = (zone == Zone.Hand && Object.owner obj == pid) || elem (zone, Object.owner obj) (playLandPiles pid gs) || mayPlayByMayhem pid oid gs
     in permissionOptions rides free (landPermissionsFrom pid zone oid gs)

-- CR 702.187c: may `pid` play `oid` out of their graveyard under the card's own
-- costless mayhem? It is in `pid`'s graveyard (rule 702.187c's "your
-- graveyard"), has the keyword there (CR 702.187a, read off the projection, so
-- a granted instance counts), and `pid` discarded THIS object this turn -- CR
-- 400.7 makes a card that left the graveyard and came back a new object with no
-- discard behind it. Read live at the moment of the play, as the mayhem cast's
-- own clause is (Pawl.Engine.Cost.candidateCostsGiven).
--
-- A per-CARD permission read off the card, so Pawl.Engine.Action.playableLands
-- asks it of each graveyard member rather than opening the pile.
mayPlayByMayhem :: PlayerId -> ObjectId -> GameState -> Bool
mayPlayByMayhem pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Object.zone obj == Zone.Graveyard
      && Object.owner obj == pid
      && Keyword.hasCostlessMayhem (Map.keysSet (Projection.keywordsOf oid gs))
      && Game.discardedThisTurnBy pid oid gs

-- CR 702.170f: the (zone, owner) piles some PlotFrom grant opens to `pid`, for
-- Pawl.Engine.Plot to draw candidates from, playPermissionPiles' shape; which of
-- their cards it covers is mayPlotFrom's per-card answer.
plotPiles :: PlayerId -> GameState -> [(Zone.Zone, PlayerId)]
plotPiles pid gs =
  [ (InZone.zone inZone, owner)
  | (_, PlayerEffect.PlotFrom grant) <- applying pid gs,
    let inZone = PlotFromZone.from grant,
    owner <- zoneOwners pid (InZone.player inZone) gs
  ]

-- CR 702.170f: does some PlotFrom grant let plot function for `oid` in `zone`
-- for `pid`? The zone and its owner are opensZoneOf's, the criterion
-- matchesObjectFrom's, castPermissionsFrom's two conjuncts; the narrowing to a
-- library's top card is the caller's (Pawl.Engine.Cast.pileCandidates).
mayPlotFrom :: PlayerId -> Zone.Zone -> ObjectId -> GameState -> Bool
mayPlotFrom pid zone oid gs =
  or
    [ opensZoneOf pid zone oid (PlotFromZone.from grant) gs
        && matchesObjectFrom source (PlotFromZone.matching grant) oid gs
    | (source, PlayerEffect.PlotFrom grant) <- applying pid gs
    ]

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
-- Takes the OBJECT and reaches the half through the STATE, exactly as the one
-- above does: the caller has stamped the proposal through
-- Pawl.Engine.Cast.asProposed, so matchesObject reads the half being cast.
--
-- WHOSE hand is a conjunct here rather than a field on the arm, which is the one
-- place this differs from mayCastFrom: rule 118.9's grant is worded "from YOUR
-- hand" on every printing, so the hand is the grantee's by the sentence and no
-- card has another to name. It stopped being free once Sen Triplets could put
-- alice's cast in bob's hand, where alice's Omniscience says nothing (see #2169) --
-- and the caller can no longer supply it, that arm now asking the CASTER rather
-- than the card's owner.
mayCastFromHandWithoutPayingManaCost :: PlayerId -> ObjectId -> GameState -> Bool
mayCastFromHandWithoutPayingManaCost pid oid gs =
  let inTheirOwnHand = fmap Object.owner (Game.lookupObject oid gs) == Just pid
      allows (source, effect) = case effect of
        PlayerEffect.CastFromHandWithoutPayingManaCost criterion -> matchesObjectFrom source criterion oid gs
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
        -- The CR 601.3 permissions, which say WHERE a spell may be cast from and
        -- WHEN. Neither states a cost, which is the whole reason this arm is its
        -- own: Yawgmoth's Will's cast pays the card's printed cost.
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        -- CR 118.7's increases and reductions, which change what a cost COMES
        -- TO; this arm supplies a different cost to start from (CR 118.9c). The
        -- two compose at Pawl.Engine.Cost.total, which is handed whichever
        -- candidate CR 601.2b settled on -- so a reduction applied to {0} still
        -- floors at {0} and neither reader knows of the other.
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        -- The PROHIBITIONS, read at their own gate (prohibitsCasting above): CR
        -- 101.2 makes a "can't" beat any cost this offers, and folding them here
        -- would let a permission outvote one.
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
   in inTheirOwnHand && any allows (applying pid gs)

-- CR 305.1: which piles may `pid` play a land from because an EFFECT says so?
-- Crucible of Worlds' whole sentence, the play half of Yawgmoth's Will's first
-- one, and Sen Triplets' "you may play lands ... from that player's hand".
--
-- The PLAY-side sibling of mayCastFrom above, and read at a different gate for
-- the reason CR 305.1 gives: playing a land is a special action that never uses
-- the stack, so Pawl.Engine.Action.playableLands asks this where
-- Pawl.Engine.Cast.castableZones asks that one. Neither permission implies the
-- other, and Yawgmoth's Will declares both arms because its sentence says both.
--
-- A LIST of (zone, owner) piles rather than a Bool about one, which is the shape
-- the caller wants once a grant can name somebody else's hand: the cast side is
-- handed a candidate and asks about it, and this side has to produce the
-- candidates. Duplicates are left in -- the caller reads members off each pile,
-- and Pawl.Engine.Action.playableLands nubs the ids it ends up with.
--
-- Asks nothing about WHICH land, where the cast side takes an ObjectId: the arm
-- carries no Filter (see the type), so a grant opens a named pile or none of it.
-- How much of a pile that is stays the ZONE's answer -- a named library is its
-- top card (Pawl.Engine.Cast.pileCandidates) -- and never this list's.
playLandPiles :: PlayerId -> GameState -> [(Zone.Zone, PlayerId)]
playLandPiles pid gs =
  let piles effect = case effect of
        PlayerEffect.PlayLandsFrom inZone -> fmap ((,) (InZone.zone inZone)) (zoneOwners pid (InZone.player inZone) gs)
        -- The cast-side twin, which can name the same zone. A Cast-verb grant
        -- reaches no land play (CR 305.1: a land is never cast), and a Play-verb
        -- one narrows by card and spends a budget, so playPermissionPiles and
        -- landPermissionsFrom answer for it rather than this unfiltered list.
        PlayerEffect.CastFrom _ -> []
        -- CR 702.170f's plot is a special action of its own and plays nothing.
        PlayerEffect.PlotFrom _ -> []
        -- CR 305.2's COUNT, which says nothing about a zone. The two compose in
        -- Pawl.Engine.Action.legalActions -- that one bounds how many plays,
        -- this one widens where they may come from -- without either knowing of
        -- the other.
        PlayerEffect.PlayAdditionalLands _ -> []
        PlayerEffect.CastAsThoughItHadFlash _ -> []
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> []
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> []
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> []
        PlayerEffect.CantCastSpells -> []
        PlayerEffect.CantActivateAbilities _ -> []
        PlayerEffect.CantCastMoreThan _ -> []
        PlayerEffect.ModifyCost {} -> []
        PlayerEffect.AlternativeActivationCost {} -> []
        PlayerEffect.NoMaximumHandSize -> []
        PlayerEffect.SetMaximumHandSize _ -> []
        PlayerEffect.IncreaseMaximumHandSize _ -> []
        PlayerEffect.ReduceMaximumHandSize _ -> []
        PlayerEffect.DontLoseUnspentMana _ -> []
        PlayerEffect.LoseLifeForUnspentMana -> []
        PlayerEffect.SpendManaAsThough _ -> []
        PlayerEffect.CantBeTargetedBy _ -> []
        PlayerEffect.CantBeCountered _ -> []
        PlayerEffect.DamageCantBePrevented _ -> []
        PlayerEffect.DamageCantBeRedirected _ -> []
        PlayerEffect.CantSearchLibraries _ -> []
        PlayerEffect.HasProtectionFrom _ -> []
        PlayerEffect.CantBecomeMonarch -> []
        PlayerEffect.CantSetSchemesInMotion -> []
        PlayerEffect.CantAttackWithCreatures -> []
        -- The PROHIBITIONS, which prohibitsPlayingLand above is what reads: CR
        -- 101.2 makes a "can't" beat this permission, and the two are folded at
        -- separate gates so that neither can outvote the other by accident.
        PlayerEffect.CantCastMatching _ -> []
        PlayerEffect.CastOnlyAtSorcerySpeed -> []
        PlayerEffect.CantPlayLands _ -> []
        -- A cost, not a zone: CR 118.9's grant says what a spell PAYS and
        -- widens no pile a land may be played from.
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> []
        PlayerEffect.CantGetCounters _ -> []
        PlayerEffect.StateCoinFlip _ -> []
        PlayerEffect.ModifyDieRoll _ -> []
        PlayerEffect.AdditionalVotes _ -> []
        PlayerEffect.AdditionalSurveilCards _ -> []
        PlayerEffect.CantGainLife -> []
        PlayerEffect.CantLoseLife -> []
   in concatMap (piles . snd) (applying pid gs)

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
-- controller), which is why the two player arguments here read (caster,
-- protected) and inScope's read (asked-about, controller). See
-- Pawl.Types.PlayerEffect.CantBeTargetedBy for where the two would come apart.
--
-- A Nothing caster is a question with no CR 109.5 "you" in it. Hexproof does not
-- stop it -- nobody's opponent -- while shroud stops it anyway, because
-- AnyPlayer never asks the caster a question. That is exactly the carve-out
-- playersInScope above already makes for the same constructor, and it is the
-- rules answer too: CR 702.18a names no player at all.
--
-- MEMBERSHIP, never a tally: CR 702.18b and CR 702.11h make multiple instances
-- redundant for the player half as well as the permanent half.
--
-- Takes the gathered rows rather than gathering them, where every other question
-- here takes a PlayerId: Pawl.Engine.Target.targetable asks this and
-- protectedFromGiven below about the SAME player in one breath, and `applying`
-- forces Projection.abilityRemoval, a whole-board gather, the moment any
-- permanent carries a player ability. One walk for two questions is why there is
-- no PlayerId-taking wrapper beside this -- nothing would call it.
protectedFromTargeting :: [(RowSource, PlayerEffect)] -> Maybe PlayerId -> PlayerId -> GameState -> Bool
protectedFromTargeting rows caster pid gs =
  let stops effect = case effect of
        PlayerEffect.CantBeTargetedBy scope -> case caster of
          Just who -> inScope who pid gs scope
          Nothing -> case scope of
            PlayerScope.Related relation -> PlayerRelation.holdsFor (Game.teams gs) relation (Just pid) Nothing
            -- Unlike AnyPlayer, this one does ask the caster a question -- "is
            -- the caster the player controlling the most permanents?" -- and a
            -- sourceless spell or ability is nobody, so it is not that player.
            PlayerScope.ControllingMostPermanents -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        -- CR 701.6a grants no targeting immunity: Pawl.Types.Counterability
        -- says the same about CR 113.6g, and a Cancel at a spell Spider-Punk
        -- protects still targets it legally and still resolves.
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
   in any (stops . snd) rows

-- CR 702.16b / 702.16c: does `pid` have protection from the object `oid` -- is
-- `oid` a source with a quality one of that player's protection abilities
-- states?
--
-- The typed question Pawl.Engine.Target.targetable, Pawl.Engine.Attach and
-- Pawl.Engine.Sba ask, so none of those modules sees a PlayerEffect constructor.
-- It answers the PLAYER half of rule 702.16; the permanent half is a keyword and
-- stays where the other keyword reads are (Pawl.Engine.Target.protectionQuality,
-- Pawl.Engine.Keyword.mintedAttachRestrictionsFor).
--
-- ONE predicate for both consequences rather than one per rule, because rule
-- 702.16 states one relation -- has protection from this object -- and rules
-- 702.16b and 702.16c each read it and then say what it forbids. Which
-- consequence follows is the caller's, exactly as it is for the keyword.
--
-- MEMBERSHIP, never a tally: CR 702.16m makes multiple instances of protection
-- from the same quality redundant for the player case as much as the permanent
-- one.
protectedFrom :: ObjectId -> PlayerId -> GameState -> Bool
protectedFrom oid pid gs = protectedFromGiven (applying pid gs) Nothing oid gs

-- protectedFrom against an already-gathered row list, for
-- protectedFromTargetingGiven's reason above. `aimer` is Filter.aimingController:
-- Just the controller of the spell or ability aiming at the player, for rule
-- 702.16k's targeting clause, and Nothing at the Aura bar. The cast-prohibition
-- scenario "an opponent's ability still can't target the protected player once
-- she steals its source in response" proves it.
protectedFromGiven :: [(RowSource, PlayerEffect)] -> Maybe (Maybe PlayerId) -> ObjectId -> GameState -> Bool
protectedFromGiven rows aimer oid gs =
  let stops (source, effect) = case effect of
        -- CR 702.16a's quality, matched against the object's projected view.
        -- Runed Halo's chosen name is Filter.HasChosenName, answered off the
        -- row's source (contextFor). CR 702.16j's "protection from everything"
        -- is the empty conjunction, which matches every source.
        --
        -- CR 109.5's "you" is the CARRIER's controller, as at rule 702.16e's
        -- shield (Pawl.Engine.Replacement's fromProtectedPlayer), not the matched
        -- object's: Absolute Virtue's "each of your opponents" names the
        -- protected player's opponents, which the aiming spell's own controller
        -- would make nobody. The cr-702-16i scenarios under
        -- data/scenarios/cast-prohibition prove the targeting and Aura halves.
        PlayerEffect.HasProtectionFrom quality ->
          Filter.matches
            ((contextFor (RowSource.object source >>= (`Projection.controllerOf` gs)) source gs) {Filter.aimingController = aimer})
            (Projection.viewOfObject oid gs)
            quality
        -- CR 702.18a and CR 702.11c are a different immunity, and a narrower
        -- one: they stop TARGETING alone, where rule 702.16 also bars an Aura
        -- and, by rule 702.16e, prevents damage. Read at their own gate
        -- (protectedFromTargeting above).
        PlayerEffect.CantBeTargetedBy _ -> False
        -- Every other arm is about casting, playing, countering, searching,
        -- paying, keeping mana, gaining or losing life, or how a coin flip came
        -- out. None of them says
        -- anything about which objects may reach this player.
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
   in any stops rows

-- CR 702.16e's damage half, as the (protected player, carrier, quality) rows
-- Pawl.Engine.Replacement.collect mints a CR 615.1 shield from: "any damage that
-- would be dealt by sources that have the stated quality to a permanent or player
-- with protection is prevented."
--
-- The third consequence of the one relation protectedFrom above answers, and the
-- only one this module cannot answer itself: the other two are gates a caller
-- asks (Pawl.Engine.Target.targetable, Pawl.Engine.Sba.fallsOff), where a
-- prevention is a replacement effect that has to exist as a row before an event
-- is proposed. So this returns the CARRIER and leaves the row to that module,
-- which is where every other prevention shield is built.
--
-- The QUALITY as the row wrote it, never the set of things it currently matches:
-- for Runed Halo that quality is Filter.HasChosenName, which the shield's
-- Context answers off the carrier, so CR 201.4's names stay a LIVE read
-- at the damage event (CR 609.7b's recheck) rather than a set frozen when the row
-- was gathered. A STORED row hands over the choices it baked as it began (CR
-- 608.2h) for that Context to use instead; the Synthetic Name Ward scenario
-- under data/scenarios/cast-prohibition proves it. A card-stated quality needs
-- the same treatment for CR 613's sake and gets it for free, being a Filter
-- either way.
--
-- A row with no carrier makes no row here, which costs nothing today -- both
-- carriers on this axis name their source (see `applying`) -- and is the honest
-- answer rather than a shield from nowhere.
--
-- ONE ROW PER (player, carrier), which is CR 702.16m's redundancy arriving for
-- free on the far side: two carriers naming one card give the player two
-- PreventAll rows, and the second has nothing left to prevent.
--
-- A walk per SEAT, which is what `applying` is, rather than one gather over the
-- axis: CR 116.2d's ignore and every scope in Pawl.Types.PlayerScope are asked
-- about a particular player, so a single-pass version would restate both.
protectionCarriers :: GameState -> [(PlayerId, ObjectId, Maybe SourceChoices.SourceChoices, Filter Keyword)]
protectionCarriers gs =
  let carrier pid (source, effect) = case effect of
        -- The quality the CARD states, handed on as written: the shield's source
        -- side is this Filter, so the three consequences of rule 702.16 read one
        -- quality (protectedFromGiven above is the other two).
        PlayerEffect.HasProtectionFrom quality -> fmap (\oid -> (pid, oid, RowSource.choices source, quality)) (RowSource.object source)
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CantSearchLibraries _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantActivateAbilities _ -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.ModifyCost {} -> Nothing
        PlayerEffect.AlternativeActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.IncreaseMaximumHandSize _ -> Nothing
        PlayerEffect.ReduceMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.LoseLifeForUnspentMana -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> Nothing
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> Nothing
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.DamageCantBeRedirected _ -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantSetSchemesInMotion -> Nothing
        PlayerEffect.CantAttackWithCreatures -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands _ -> Nothing
        PlayerEffect.CastFrom _ -> Nothing
        PlayerEffect.PlayLandsFrom _ -> Nothing
        PlayerEffect.PlotFrom _ -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
        PlayerEffect.CantGetCounters _ -> Nothing
        PlayerEffect.StateCoinFlip _ -> Nothing
        PlayerEffect.ModifyDieRoll _ -> Nothing
        PlayerEffect.AdditionalVotes _ -> Nothing
        PlayerEffect.AdditionalSurveilCards _ -> Nothing
        PlayerEffect.CantGainLife -> Nothing
        PlayerEffect.CantLoseLife -> Nothing
   in concatMap (\pid -> Maybe.mapMaybe (carrier pid) (applying pid gs)) (Game.stillPlaying gs)

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
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> Nothing
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> Nothing
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantActivateAbilities _ -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.ModifyCost {} -> Nothing
        PlayerEffect.AlternativeActivationCost {} -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.IncreaseMaximumHandSize _ -> Nothing
        PlayerEffect.ReduceMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.LoseLifeForUnspentMana -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.DamageCantBeRedirected _ -> Nothing
        PlayerEffect.CantSearchLibraries _ -> Nothing
        PlayerEffect.HasProtectionFrom _ -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantSetSchemesInMotion -> Nothing
        PlayerEffect.CantAttackWithCreatures -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands _ -> Nothing
        PlayerEffect.CastFrom _ -> Nothing
        PlayerEffect.PlayLandsFrom _ -> Nothing
        PlayerEffect.PlotFrom _ -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
        PlayerEffect.CantGetCounters _ -> Nothing
        PlayerEffect.StateCoinFlip _ -> Nothing
        PlayerEffect.ModifyDieRoll _ -> Nothing
        PlayerEffect.AdditionalVotes _ -> Nothing
        PlayerEffect.AdditionalSurveilCards _ -> Nothing
        PlayerEffect.CantGainLife -> Nothing
        PlayerEffect.CantLoseLife -> Nothing
   in defaultLandPlays + sum (Maybe.mapMaybe (grantOf . snd) (applying pid gs))

-- CR 701.38a: the one vote every seat casts, before rule 701.38d's extras.
defaultVotes :: Natural
defaultVotes = 1

-- CR 701.38d: how many ballots `pid` casts in one vote -- rule 701.38a's own
-- one, plus every vote an effect gives them (Brago's Representative). Rule
-- 701.38d puts them all "at the same time the player would otherwise have
-- voted", which is Resolve.Effect's business; this answers only how many.
--
-- A SUM over every applicable grant, landPlaysAllowed's posture and for its
-- reason: rule 701.38d says an effect gives "multiple votes" and states no rule
-- making two such effects redundant, so two Representatives are three votes.
--
-- Read LIVE through `applying`, like every other question in this module. The
-- vote itself reads it ONCE, off the board the vote begins on, since nothing
-- happens between two seats' ballots that could change it.
--
-- Not implemented: an OPTIONAL additional vote, Ballot Broker's and The
-- Valeyard's "you may vote an additional time" (#4409).
votesAllowed :: PlayerId -> GameState -> Natural
votesAllowed pid gs =
  let grantOf effect = case effect of
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantActivateAbilities _ -> Nothing
        PlayerEffect.CantCastMoreThan {} -> Nothing
        PlayerEffect.ModifyCost {} -> Nothing
        PlayerEffect.AlternativeActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands {} -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize {} -> Nothing
        PlayerEffect.IncreaseMaximumHandSize {} -> Nothing
        PlayerEffect.ReduceMaximumHandSize {} -> Nothing
        PlayerEffect.DontLoseUnspentMana {} -> Nothing
        PlayerEffect.LoseLifeForUnspentMana -> Nothing
        PlayerEffect.SpendManaAsThough {} -> Nothing
        PlayerEffect.CantBeTargetedBy {} -> Nothing
        PlayerEffect.HasProtectionFrom {} -> Nothing
        PlayerEffect.CastAsThoughItHadFlash {} -> Nothing
        PlayerEffect.MayPlayAsThoughItHadFlash {} -> Nothing
        PlayerEffect.ActivateKeywordAtInstantSpeed {} -> Nothing
        PlayerEffect.ActivateLoyaltyAtInstantSpeed {} -> Nothing
        PlayerEffect.CantBeCountered {} -> Nothing
        PlayerEffect.DamageCantBePrevented {} -> Nothing
        PlayerEffect.DamageCantBeRedirected {} -> Nothing
        PlayerEffect.CantSearchLibraries {} -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantSetSchemesInMotion -> Nothing
        PlayerEffect.CantAttackWithCreatures -> Nothing
        PlayerEffect.CantCastMatching {} -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands {} -> Nothing
        PlayerEffect.CastFrom {} -> Nothing
        PlayerEffect.PlayLandsFrom {} -> Nothing
        PlayerEffect.PlotFrom {} -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost {} -> Nothing
        PlayerEffect.CantGetCounters {} -> Nothing
        PlayerEffect.StateCoinFlip {} -> Nothing
        PlayerEffect.ModifyDieRoll _ -> Nothing
        PlayerEffect.AdditionalVotes extra -> Just extra
        PlayerEffect.AdditionalSurveilCards _ -> Nothing
        PlayerEffect.CantGainLife -> Nothing
        PlayerEffect.CantLoseLife -> Nothing
   in defaultVotes + sum (Maybe.mapMaybe (grantOf . snd) (applying pid gs))

-- CR 701.25b: how many cards beyond the instruction's own N `pid` looks at
-- when they surveil (Enhanced Surveillance). A SUM over every applicable
-- grant, votesAllowed's posture: rule 701.25b makes no two such effects
-- redundant, and the card's own ruling (2018-10-05) makes two of them four.
--
-- Not implemented: the card's "may" is not asked; the extra cards are always
-- looked at (#4391).
surveilExtra :: PlayerId -> GameState -> Natural
surveilExtra pid gs =
  let grantOf effect = case effect of
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantActivateAbilities _ -> Nothing
        PlayerEffect.CantCastMoreThan {} -> Nothing
        PlayerEffect.ModifyCost {} -> Nothing
        PlayerEffect.AlternativeActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands {} -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize {} -> Nothing
        PlayerEffect.IncreaseMaximumHandSize {} -> Nothing
        PlayerEffect.ReduceMaximumHandSize {} -> Nothing
        PlayerEffect.DontLoseUnspentMana {} -> Nothing
        PlayerEffect.LoseLifeForUnspentMana -> Nothing
        PlayerEffect.SpendManaAsThough {} -> Nothing
        PlayerEffect.CantBeTargetedBy {} -> Nothing
        PlayerEffect.HasProtectionFrom {} -> Nothing
        PlayerEffect.CastAsThoughItHadFlash {} -> Nothing
        PlayerEffect.MayPlayAsThoughItHadFlash {} -> Nothing
        PlayerEffect.ActivateKeywordAtInstantSpeed {} -> Nothing
        PlayerEffect.ActivateLoyaltyAtInstantSpeed {} -> Nothing
        PlayerEffect.CantBeCountered {} -> Nothing
        PlayerEffect.DamageCantBePrevented {} -> Nothing
        PlayerEffect.DamageCantBeRedirected {} -> Nothing
        PlayerEffect.CantSearchLibraries {} -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantSetSchemesInMotion -> Nothing
        PlayerEffect.CantAttackWithCreatures -> Nothing
        PlayerEffect.CantCastMatching {} -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands {} -> Nothing
        PlayerEffect.CastFrom {} -> Nothing
        PlayerEffect.PlayLandsFrom {} -> Nothing
        PlayerEffect.PlotFrom {} -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost {} -> Nothing
        PlayerEffect.CantGetCounters {} -> Nothing
        PlayerEffect.StateCoinFlip {} -> Nothing
        PlayerEffect.ModifyDieRoll _ -> Nothing
        PlayerEffect.AdditionalVotes _ -> Nothing
        PlayerEffect.AdditionalSurveilCards extra -> Just extra
        PlayerEffect.CantGainLife -> Nothing
        PlayerEffect.CantLoseLife -> Nothing
   in sum (Maybe.mapMaybe (grantOf . snd) (applying pid gs))

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
-- The fold's SEED and not one more effect applied to a seven: CR 902.5b makes CR
-- 313.6's hand modifier part of what the maximum hand size IS, where CR 613.11's
-- effects are applied to that number afterwards. A vanguard whose modifier is -1
-- beside a Minamo Scrollkeeper leaves a maximum of seven, and beside a Reliquary
-- Tower leaves none at all -- the removal has a number to remove either way.
--
-- A left fold from CR 402.2's seven rather than a search for the newest effect:
-- "applied in timestamp order" is a sequence of edits to one value, and reading
-- only the last one would be a different rule for the two ADJUSTING arms, which
-- compose with what they find instead of replacing it (Minamo Scrollkeeper's
-- "increased by one", Gnat Miser's "reduced by one").
--
-- Those two adjust the Maybe rather than reaching past it, which is the whole of
-- how they meet a removal: fmap over Nothing is Nothing, so an adjustment applied
-- after Reliquary Tower has nothing to adjust and the player still has no maximum
-- (Reliquary Tower's own ruling). Applied after a set, it adjusts the number that
-- was set.
--
-- CR 107.1b is the reduction's floor: the calculation determining a maximum hand
-- size cannot yield a negative number, and no exception in that rule is a hand
-- size. Written as a comparison rather than as Natural subtraction, which would
-- throw rather than floor.
maximumHandSize :: PlayerId -> GameState -> Maybe Natural
maximumHandSize pid gs =
  let apply current effect = case effect of
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize limit -> Just limit
        PlayerEffect.IncreaseMaximumHandSize extra -> fmap (extra +) current
        PlayerEffect.ReduceMaximumHandSize fewer -> fmap (\limit -> if fewer >= limit then 0 else limit - fewer) current
        PlayerEffect.CantCastSpells -> current
        PlayerEffect.CantActivateAbilities _ -> current
        PlayerEffect.CantCastMoreThan _ -> current
        PlayerEffect.ModifyCost {} -> current
        PlayerEffect.AlternativeActivationCost {} -> current
        PlayerEffect.PlayAdditionalLands _ -> current
        PlayerEffect.DontLoseUnspentMana _ -> current
        PlayerEffect.LoseLifeForUnspentMana -> current
        PlayerEffect.SpendManaAsThough _ -> current
        PlayerEffect.CantBeTargetedBy _ -> current
        PlayerEffect.CastAsThoughItHadFlash _ -> current
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> current
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> current
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> current
        PlayerEffect.CantBeCountered _ -> current
        PlayerEffect.DamageCantBePrevented _ -> current
        PlayerEffect.DamageCantBeRedirected _ -> current
        PlayerEffect.CantSearchLibraries _ -> current
        PlayerEffect.HasProtectionFrom _ -> current
        PlayerEffect.CantBecomeMonarch -> current
        PlayerEffect.CantSetSchemesInMotion -> current
        PlayerEffect.CantAttackWithCreatures -> current
        PlayerEffect.CantCastMatching _ -> current
        PlayerEffect.CastOnlyAtSorcerySpeed -> current
        PlayerEffect.CantPlayLands _ -> current
        PlayerEffect.CastFrom _ -> current
        PlayerEffect.PlayLandsFrom _ -> current
        PlayerEffect.PlotFrom _ -> current
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> current
        PlayerEffect.CantGetCounters _ -> current
        PlayerEffect.StateCoinFlip _ -> current
        PlayerEffect.ModifyDieRoll _ -> current
        PlayerEffect.AdditionalVotes _ -> current
        PlayerEffect.AdditionalSurveilCards _ -> current
        PlayerEffect.CantGainLife -> current
        PlayerEffect.CantLoseLife -> current
   in List.foldl' (\current row -> apply current (snd row)) (Just (Vanguard.handSize defaultMaximumHandSize pid gs)) (applying pid gs)

-- CR 500.5 / 106.4 / 613.11: which of the unspent mana in this player's pool do
-- they keep as a step or phase ends (Upwelling, Omnath Locus of Mana)? The typed
-- question Pawl.Engine.Mana.emptiedManaPools asks, so the turn-based action of CR
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
-- Pawl.Engine.Mana.emptiedManaPools takes the disjunction of the two carriers.
keepsUnspentMana :: PlayerId -> GameState -> ManaUnit -> Bool
keepsUnspentMana pid gs =
  let keeps effect = case effect of
        PlayerEffect.DontLoseUnspentMana f -> Just f
        PlayerEffect.LoseLifeForUnspentMana -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantActivateAbilities _ -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.ModifyCost {} -> Nothing
        PlayerEffect.AlternativeActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.IncreaseMaximumHandSize _ -> Nothing
        PlayerEffect.ReduceMaximumHandSize _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> Nothing
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> Nothing
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.DamageCantBeRedirected _ -> Nothing
        PlayerEffect.CantSearchLibraries _ -> Nothing
        PlayerEffect.HasProtectionFrom _ -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantSetSchemesInMotion -> Nothing
        PlayerEffect.CantAttackWithCreatures -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands _ -> Nothing
        PlayerEffect.CastFrom _ -> Nothing
        PlayerEffect.PlayLandsFrom _ -> Nothing
        PlayerEffect.PlotFrom _ -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
        PlayerEffect.CantGetCounters _ -> Nothing
        PlayerEffect.StateCoinFlip _ -> Nothing
        PlayerEffect.ModifyDieRoll _ -> Nothing
        PlayerEffect.AdditionalVotes _ -> Nothing
        PlayerEffect.AdditionalSurveilCards _ -> Nothing
        PlayerEffect.CantGainLife -> Nothing
        PlayerEffect.CantLoseLife -> Nothing
      filters = Maybe.mapMaybe (keeps . snd) (applying pid gs)
   in \unit -> any (\f -> ManaFilter.matches f unit) filters

-- CR 500.5 / 119.3 / Yurlok of Scorch Thrash: does this player lose life for the
-- unspent mana rule 500.5's sweep is about to take? The typed question
-- Pawl.Engine.Mana.emptyManaPools asks beside the keepsUnspentMana its sweep
-- asks, so the
-- turn-based action of CR 703.4q never learns which effect answered it.
--
-- A BOOL about the whole pool, where its neighbour is a per-unit predicate, and
-- why the carrier takes no payload: the printed sentence charges for "that much"
-- and names no mana, so there is nothing to ask a unit and nothing to narrow by.
-- HOW MUCH is what the sweep removed, which the caller alone knows.
--
-- A DISJUNCTION, which is the card's own ruling -- "multiple Yurloks on the
-- battlefield won't cause additional life loss" -- so a second applicable effect
-- changes no answer and CR 613.11's timestamp order has nothing to order.
--
-- Read LIVE through `applying`, so a Yurlok that left the battlefield during the
-- step charges nobody (CR 604.2).
losesLifeForUnspentMana :: PlayerId -> GameState -> Bool
losesLifeForUnspentMana pid gs =
  let charges effect = case effect of
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost _ -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> True
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
   in any (charges . snd) (applying pid gs)

-- CR 119.7: does an effect say this player can't gain life? The typed question
-- Pawl.Engine.Event.resolveLifeGain asks at CR 614.1's funnel, so that module
-- never sees a PlayerEffect constructor.
--
-- A STATIC RESTRICTION rather than a replacement, which is why the answer is
-- consulted AHEAD of the proposal: CR 119.7's last clause says a replacement
-- effect that would replace the gain "won't do anything", and a row ordered and
-- spent by CR 616.1 could not produce that.
--
-- A DISJUNCTION, CR 101.2's posture: one applicable "can't" outranks every
-- permission, so a second changes no answer and CR 613.11's timestamp order has
-- nothing to order.
--
-- Read LIVE through `applying`, so a Giant Cindermaw that left the battlefield
-- stops restricting anybody (CR 604.2).
--
-- CR 810.9g: asked of every player sharing this one's life total
-- (Game.lifeSharers), since a teammate who can't gain life stops the team.
prohibitsGainingLife :: PlayerId -> GameState -> Bool
prohibitsGainingLife pid gs =
  let prohibits effect = case effect of
        PlayerEffect.CantGainLife -> True
        -- Every other arm is about casting, playing, targeting, countering,
        -- searching, paying, keeping mana, rule 702 protection, how a coin flip
        -- came out or losing life. CR 119.7's restriction is none of those.
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost _ -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantLoseLife -> False
   in any (\member -> any (prohibits . snd) (applying member gs)) (Game.lifeSharers pid gs)

-- CR 119.8: does an effect say this player can't lose life? prohibitsGainingLife
-- one direction over, read by Pawl.Engine.Event.resolveLifeLoss ahead of its
-- proposal and by Event.canPayLife, which rule 119.8's last sentence makes
-- refuse every amount but CR 119.4b's 0.
--
-- CR 120.3a is what makes this reach DAMAGE: damage dealt to a player causes
-- that player to lose life, and it is the RESULT that this stops. The damage is
-- still dealt, so a lifelink gain, a wither counter and an infect poison counter
-- all still happen -- Pawl.DamageSpec's Platinum Emperion case is the proof.
--
-- CR 810.9h: asked of every player sharing this one's life total, the
-- prohibitsGainingLife reading; data/scenarios/team/cr-810-9h-* prove it.
prohibitsLosingLife :: PlayerId -> GameState -> Bool
prohibitsLosingLife pid gs =
  let prohibits effect = case effect of
        PlayerEffect.CantLoseLife -> True
        -- Every other arm is about casting, playing, targeting, countering,
        -- searching, paying, keeping mana, rule 702 protection, how a coin flip
        -- came out or gaining life. CR 119.8's restriction is none of those --
        -- LoseLifeForUnspentMana included, which CHARGES life rather than saying
        -- anything about whether a charge may land.
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost _ -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
   in any (\member -> any (prohibits . snd) (applying member gs)) (Game.lifeSharers pid gs)

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
        PlayerEffect.LoseLifeForUnspentMana -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantActivateAbilities _ -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.ModifyCost {} -> Nothing
        PlayerEffect.AlternativeActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.IncreaseMaximumHandSize _ -> Nothing
        PlayerEffect.ReduceMaximumHandSize _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> Nothing
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> Nothing
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.DamageCantBeRedirected _ -> Nothing
        PlayerEffect.CantSearchLibraries _ -> Nothing
        PlayerEffect.HasProtectionFrom _ -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantSetSchemesInMotion -> Nothing
        PlayerEffect.CantAttackWithCreatures -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands _ -> Nothing
        PlayerEffect.CastFrom _ -> Nothing
        PlayerEffect.PlayLandsFrom _ -> Nothing
        PlayerEffect.PlotFrom _ -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
        PlayerEffect.CantGetCounters _ -> Nothing
        PlayerEffect.StateCoinFlip _ -> Nothing
        PlayerEffect.ModifyDieRoll _ -> Nothing
        PlayerEffect.AdditionalVotes _ -> Nothing
        PlayerEffect.AdditionalSurveilCards _ -> Nothing
        PlayerEffect.CantGainLife -> Nothing
        PlayerEffect.CantLoseLife -> Nothing
   in Maybe.mapMaybe (spends . snd) (applying pid gs)

-- CR 609.4b / 113.6d: spendManaAsThough plus, for a payment casting a spell,
-- the clauses that spell's own abilities state about the mana spent to cast it
-- (Tome of the Infinite's perpetual grant), read off its live projection. CR
-- 601.2a has the spell on the stack under `pid` by the time it is paid for, and
-- a perpetual grant reaches it in the hand too, which is where the castability
-- gate asks. The scenario
-- a-printed-spellbook-is-offered-whole-and-the-card-randomness-named-is-the-one
-- proves it.
spendManaAsThoughFor :: PlayerId -> PaymentSubject.PaymentSubject -> GameState -> [SpendManaAsThough.SpendManaAsThough]
spendManaAsThoughFor pid subject gs =
  spendManaAsThough pid gs <> case subject of
    PaymentSubject.Casting oid -> PC.grantedSpendManaAsThough (Projection.project oid gs)
    PaymentSubject.Activating {} -> []
    PaymentSubject.Unlocking {} -> []
    PaymentSubject.TurningFaceUp {} -> []
    PaymentSubject.ForNeither -> []

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
        PlayerEffect.CantActivateAbilities _ -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.ModifyCost {} -> False
        PlayerEffect.AlternativeActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.SetMaximumHandSize _ -> False
        PlayerEffect.IncreaseMaximumHandSize _ -> False
        PlayerEffect.ReduceMaximumHandSize _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.LoseLifeForUnspentMana -> False
        PlayerEffect.SpendManaAsThough _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> False
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> False
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> False
        -- Spider-Punk's OTHER sentence, and no part of this answer: CR 615.12
        -- and CR 614.9 are about a damage event and CR 701.6a about an object on
        -- the stack. The two travel together on one card and share nothing.
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.DamageCantBeRedirected _ -> False
        PlayerEffect.CantSearchLibraries _ -> False
        PlayerEffect.HasProtectionFrom _ -> False
        PlayerEffect.CantBecomeMonarch -> False
        PlayerEffect.CantSetSchemesInMotion -> False
        PlayerEffect.CantAttackWithCreatures -> False
        PlayerEffect.CantCastMatching _ -> False
        PlayerEffect.CastOnlyAtSorcerySpeed -> False
        PlayerEffect.CantPlayLands _ -> False
        PlayerEffect.CastFrom _ -> False
        PlayerEffect.PlayLandsFrom _ -> False
        PlayerEffect.PlotFrom _ -> False
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> False
        PlayerEffect.CantGetCounters _ -> False
        PlayerEffect.StateCoinFlip _ -> False
        PlayerEffect.ModifyDieRoll _ -> False
        PlayerEffect.AdditionalVotes _ -> False
        PlayerEffect.AdditionalSurveilCards _ -> False
        PlayerEffect.CantGainLife -> False
        PlayerEffect.CantLoseLife -> False
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
-- it is the Maybe `applying` already carries. A stored CR 611.2c effect names one
-- too: it is the object that resolved (Lava Burst's own "if Lava Burst would deal
-- damage"), so Filter.IsSource is answerable on both carriers -- the Maybe is the
-- shape Filter.contextFor takes rather than a row that lacks an id.
--
-- Gathered from EVERY still-playing player rather than from one, because
-- `applying` is indexed by player and this effect is not. That reading is EXACT
-- for PlayerScope.Related AnyPlayer, the scope Spider-Punk's possessive-free sentence
-- writes: AnyPlayer is in scope for everybody, so "some player has it applying"
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
        PlayerEffect.DamageCantBePrevented pattern_ -> Just (RowSource.object src, pattern_)
        PlayerEffect.DamageCantBeRedirected _ -> Nothing
        PlayerEffect.CantSearchLibraries _ -> Nothing
        PlayerEffect.HasProtectionFrom _ -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantSetSchemesInMotion -> Nothing
        PlayerEffect.CantAttackWithCreatures -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands _ -> Nothing
        PlayerEffect.CastFrom _ -> Nothing
        PlayerEffect.PlayLandsFrom _ -> Nothing
        PlayerEffect.PlotFrom _ -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
        PlayerEffect.CantGetCounters _ -> Nothing
        PlayerEffect.StateCoinFlip _ -> Nothing
        PlayerEffect.ModifyDieRoll _ -> Nothing
        PlayerEffect.AdditionalVotes _ -> Nothing
        PlayerEffect.AdditionalSurveilCards _ -> Nothing
        PlayerEffect.CantGainLife -> Nothing
        PlayerEffect.CantLoseLife -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantActivateAbilities _ -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.ModifyCost {} -> Nothing
        PlayerEffect.AlternativeActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.IncreaseMaximumHandSize _ -> Nothing
        PlayerEffect.ReduceMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.LoseLifeForUnspentMana -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> Nothing
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> Nothing
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> Nothing
   in concatMap (\pid -> Maybe.mapMaybe says (applying pid gs)) (Game.stillPlaying gs)

-- CR 614.9: the patterns of every "that damage can't be ... dealt instead to
-- another permanent or player" applying right now (Lava Burst), each paired with
-- the SOURCE of the effect that says it. The redirection twin of `unpreventable`
-- above, and a separate gather for the reason its own carrier is a separate arm:
-- the two sentences are separable, so a board carrying one must not answer the
-- other's question.
--
-- Hands back the PATTERNS, `unpreventable`'s posture and for its reason: CR
-- 614.9's sentence is about a damage EVENT and the printed narrowing names its
-- qualities -- Lava Burst's names both its own source and a recipient -- which
-- Pawl.Engine.Replacement.matchesDamagePattern already knows how to read.
--
-- What the caller does with a match DIFFERS from the prevention side's, and the
-- rulebook is why: CR 615.12 keeps an inapplicable prevention in the applicable
-- set ("any applicable prevention effects are still applied to it"), where CR
-- 614.9 states no such twin -- its only "the effect does nothing" case is a
-- destination that left the battlefield or a player who left the game. So
-- Pawl.Engine.Replacement.applies filters the redirection out of CR 616.1's
-- choice rather than applying it inertly.
--
-- Gathered from EVERY still-playing player, and read LIVE, for the two reasons
-- `unpreventable` gives: PlayerScope.Related AnyPlayer is the only scope a card may
-- pair with this carrier (Pawl.CardSpec lints it), so "some player has it
-- applying" and "it applies" are the same fact.
unredirectable :: GameState -> [(Maybe ObjectId, DamagePattern.DamagePattern)]
unredirectable gs =
  let says (src, effect) = case effect of
        PlayerEffect.DamageCantBeRedirected pattern_ -> Just (RowSource.object src, pattern_)
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries _ -> Nothing
        PlayerEffect.HasProtectionFrom _ -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantSetSchemesInMotion -> Nothing
        PlayerEffect.CantAttackWithCreatures -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands _ -> Nothing
        PlayerEffect.CastFrom _ -> Nothing
        PlayerEffect.PlayLandsFrom _ -> Nothing
        PlayerEffect.PlotFrom _ -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
        PlayerEffect.CantGetCounters _ -> Nothing
        PlayerEffect.StateCoinFlip _ -> Nothing
        PlayerEffect.ModifyDieRoll _ -> Nothing
        PlayerEffect.AdditionalVotes _ -> Nothing
        PlayerEffect.AdditionalSurveilCards _ -> Nothing
        PlayerEffect.CantGainLife -> Nothing
        PlayerEffect.CantLoseLife -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantActivateAbilities _ -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.ModifyCost {} -> Nothing
        PlayerEffect.AlternativeActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.IncreaseMaximumHandSize _ -> Nothing
        PlayerEffect.ReduceMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.LoseLifeForUnspentMana -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> Nothing
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> Nothing
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> Nothing
   in concatMap (\pid -> Maybe.mapMaybe says (applying pid gs)) (Game.stillPlaying gs)

-- CR 705.3: every statement in force right now about a coin flip `pid` would
-- flip -- the face it is stated to come up, and whether `pid` is stated to win
-- it. Edgar, King of Figaro is the whole producer.
--
-- A LIST rather than a first or a last, because rule 705.3 puts no limit on how
-- many effects state a result and gives no order for reading them.
-- Pawl.Engine.Coin is what combines them, and is where the rule's reading of a
-- disagreement is argued.
--
-- Returns the statements UNFILTERED by Edgar's "the first time ... each turn":
-- that narrowing counts the turn's flips, which is Pawl.Engine.Coin's question
-- rather than this module's.
--
-- In timestamp order, `applying`'s order, so a combiner that cares which
-- statement came last can have it.
statedFlips :: PlayerId -> GameState -> [StatedFlip.StatedFlip]
statedFlips pid gs =
  let says effect = case effect of
        PlayerEffect.StateCoinFlip statement -> Just statement
        PlayerEffect.ModifyDieRoll _ -> Nothing
        PlayerEffect.AdditionalVotes _ -> Nothing
        PlayerEffect.AdditionalSurveilCards _ -> Nothing
        PlayerEffect.CantGainLife -> Nothing
        PlayerEffect.CantLoseLife -> Nothing
        -- Every other arm is about casting, playing, targeting, countering,
        -- searching, paying, keeping mana, gaining or losing life, or rule 702
        -- protection. CR 705.1's flip is
        -- none of those --
        -- it happens while an instruction that already resolved is being
        -- followed, or inside an entry replacement.
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantActivateAbilities _ -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.ModifyCost {} -> Nothing
        PlayerEffect.AlternativeActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.IncreaseMaximumHandSize _ -> Nothing
        PlayerEffect.ReduceMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.LoseLifeForUnspentMana -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> Nothing
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> Nothing
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.DamageCantBeRedirected _ -> Nothing
        PlayerEffect.CantSearchLibraries _ -> Nothing
        PlayerEffect.HasProtectionFrom _ -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantSetSchemesInMotion -> Nothing
        PlayerEffect.CantAttackWithCreatures -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands _ -> Nothing
        PlayerEffect.CastFrom _ -> Nothing
        PlayerEffect.PlayLandsFrom _ -> Nothing
        PlayerEffect.PlotFrom _ -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
        PlayerEffect.CantGetCounters _ -> Nothing
   in Maybe.mapMaybe (says . snd) (applying pid gs)

-- CR 706.2's third sentence: every modifier in force right now over a die roll
-- `pid` is about to make from an instruction that knows nothing about it
-- (Clam-I-Am, Wall of Fortune, Night Shift of the Living Dead).
--
-- `statedFlips` above one rule over, and the same division of labour: a LIST
-- rather than a first or a last, because rule 706.2 puts no limit on how many
-- sources modify one roll, and Pawl.Engine.Dice is what reads them against the
-- die that came up.
--
-- In timestamp order, `applying`'s order, so a reader that cares which modifier
-- came last can have it. Rule 706.2b gives the ROLLER the pick among competing
-- modifiers rather than the timestamps (Pawl.Engine.Dice.pickModifier), which is
-- why nothing here collapses the list.
--
-- Each modifier is TAGGED with the object that states it, `applying`'s own
-- answer, because CR 706.2a's cost is owed by the player that object's "you"
-- names rather than by `pid`: Wall of Fortune reaches every player's roll and
-- charges its own controller. Nothing for a stored carrier with no object behind
-- it, which no printing of this family has (CR 611.2a); Pawl.Engine.Dice is what
-- turns the tag into a seat.
rollModifiers :: PlayerId -> GameState -> [(Maybe ObjectId, ModifiedRoll.ModifiedRoll)]
rollModifiers pid gs =
  let modifies effect = case effect of
        PlayerEffect.ModifyDieRoll modifier -> Just modifier
        PlayerEffect.StateCoinFlip _ -> Nothing
        PlayerEffect.AdditionalVotes _ -> Nothing
        PlayerEffect.AdditionalSurveilCards _ -> Nothing
        PlayerEffect.CantGainLife -> Nothing
        PlayerEffect.CantLoseLife -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantActivateAbilities _ -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.ModifyCost {} -> Nothing
        PlayerEffect.AlternativeActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.SetMaximumHandSize _ -> Nothing
        PlayerEffect.IncreaseMaximumHandSize _ -> Nothing
        PlayerEffect.ReduceMaximumHandSize _ -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.LoseLifeForUnspentMana -> Nothing
        PlayerEffect.SpendManaAsThough _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.MayPlayAsThoughItHadFlash _ -> Nothing
        PlayerEffect.ActivateKeywordAtInstantSpeed _ -> Nothing
        PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.DamageCantBeRedirected _ -> Nothing
        PlayerEffect.CantSearchLibraries _ -> Nothing
        PlayerEffect.HasProtectionFrom _ -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
        PlayerEffect.CantSetSchemesInMotion -> Nothing
        PlayerEffect.CantAttackWithCreatures -> Nothing
        PlayerEffect.CantCastMatching _ -> Nothing
        PlayerEffect.CastOnlyAtSorcerySpeed -> Nothing
        PlayerEffect.CantPlayLands _ -> Nothing
        PlayerEffect.CastFrom _ -> Nothing
        PlayerEffect.PlayLandsFrom _ -> Nothing
        PlayerEffect.PlotFrom _ -> Nothing
        PlayerEffect.CastFromHandWithoutPayingManaCost _ -> Nothing
        PlayerEffect.CantGetCounters _ -> Nothing
   in Maybe.mapMaybe (\(source, effect) -> fmap ((,) (RowSource.object source)) (modifies effect)) (applying pid gs)

-- CR 611.2a / Quicken: the one-shot (Expiry.WhenUsed) stored rows `pid` would
-- SPEND by casting `oid` -- every row on castUse's axes that applies to them
-- (`applies`, the readers' own gate, so a row scoped to another seat is neither
-- widened for one player and spent by another) whose criterion matches. WotC's
-- ruling for both flash producers ends the effect "even if you cast it at a
-- time you normally could", so the timing loophole need not have been
-- exercised; and ALL matching rows go together, since two Quickens "will all
-- apply to the very next ... spell you cast".
--
-- Asked, not applied: Pawl.Engine.Cast.castSpellWith asks this of the
-- asProposed-stamped PRE-MOVE state -- so the criterion sees the chosen half and
-- CR 708.2a's face-down 2/2, the same view the cast's own gate read -- and hands
-- the rows to `consume` only once CR 601.2h's payment has succeeded, so a
-- rejected or reversed announcement (CR 601.2e, CR 733.1) spends nothing.
-- Pawl.PlayerEffectSpec's "a rejected cast leaves the grant standing" and "a
-- face-down cast spends the grant off the face the gate read" are the proofs.
spentByCast :: PlayerId -> ObjectId -> GameState -> [ActivePlayerEffect.ActivePlayerEffect]
spentByCast = spentGrants castUse

-- CR 611.2a: the criterion naming the spells whose CAST uses up a one-shot row
-- on this effect's axis, or Nothing where no cast does: castFlashGrant's axis,
-- and CR 601.2f's spell-cost modifier (Hardened Berserker's "the next spell you
-- cast this turn costs {1} less"), spent by the next spell its own Filter names
-- whatever that spell's cost leaves it to take off. The fallthrough is to
-- castFlashGrant, whose own is exhaustive. Pawl.EffectLintSpec reads it too: a
-- card may write Duration.UntilUsed only on an axis this answers for.
-- data/scenarios/expiry's "the Berserker's {1} off is used up by the first
-- spell" is the proof.
castUse :: PlayerEffect -> Maybe (Filter Keyword)
castUse effect = case effect of
  -- A row reading the spell's TARGETS answers Nothing, so Pawl.EffectLintSpec
  -- refuses it a one-shot duration: `matching` alone would spend it on a spell
  -- its target criteria never reached. No card in `data/cards/` writes one.
  PlayerEffect.ModifyCost modifier
    | CostModifier.subject modifier == CostSubject.Spells && Maybe.isNothing (CostModifier.whichTargets modifier) && Maybe.isNothing (CostModifier.perTarget modifier) -> Just (CostModifier.matching modifier)
  _ -> castFlashGrant effect

-- CR 611.2a / 601.1a's other half: the rows `pid` would spend by playing `oid`
-- as a land. Asked of the pre-move state by Pawl.Engine.Engine's Play arm for
-- spentByCast's reason, and consumed once the land has actually moved.
spentByLandPlay :: PlayerId -> ObjectId -> GameState -> [ActivePlayerEffect.ActivePlayerEffect]
spentByLandPlay = spentGrants landPlayFlashGrant

spentGrants :: (PlayerEffect -> Maybe (Filter Keyword)) -> PlayerId -> ObjectId -> GameState -> [ActivePlayerEffect.ActivePlayerEffect]
spentGrants useOf pid oid gs =
  let spent active =
        applies pid (ActivePlayerEffect.controller active) gs (ActivePlayerEffect.scope active)
          && Expiry.expiresWhenUsed (ActivePlayerEffect.expiry active)
          && maybe False (\criterion -> matchesObjectFrom (storedSource active) criterion oid gs) (useOf (ActivePlayerEffect.effect active))
   in filter spent (GameState.playerEffects gs)

-- Drop the rows spentByCast / spentByLandPlay named, once the play they were
-- asked about has happened.
consume :: [ActivePlayerEffect.ActivePlayerEffect] -> GameState -> GameState
consume rows gs = gs {GameState.playerEffects = filter (`notElem` rows) (GameState.playerEffects gs)}

-- Every PlayerRef this effect holds, traversed rather than read: CR 400.1's zone
-- references live inside the two permission payloads, and both a READER (the slot
-- report Pawl.Engine.Resolve.slotsOf owes) and a WRITER (the bake at CR 611.2b,
-- Pawl.Engine.Resolve's AffectPlayers arm) want the same walk.
--
-- Exhaustive with no wildcard, Pawl.Engine.QuantitySlot's posture: a new arm
-- carrying a reference has to say so here, and @-Werror@ is what asks. Const and
-- Identity are the two instances used.
overPlayerRefs :: (Applicative f) => (PlayerRef.PlayerRef -> f PlayerRef.PlayerRef) -> PlayerEffect -> f PlayerEffect
overPlayerRefs f effect = case effect of
  PlayerEffect.CantCastSpells -> pure effect
  PlayerEffect.CantActivateAbilities _ -> pure effect
  PlayerEffect.CantCastMoreThan _ -> pure effect
  PlayerEffect.ModifyCost {} -> pure effect
  PlayerEffect.AlternativeActivationCost _ -> pure effect
  PlayerEffect.PlayAdditionalLands _ -> pure effect
  PlayerEffect.NoMaximumHandSize -> pure effect
  PlayerEffect.SetMaximumHandSize _ -> pure effect
  PlayerEffect.IncreaseMaximumHandSize _ -> pure effect
  PlayerEffect.ReduceMaximumHandSize _ -> pure effect
  PlayerEffect.DontLoseUnspentMana _ -> pure effect
  PlayerEffect.LoseLifeForUnspentMana -> pure effect
  PlayerEffect.SpendManaAsThough _ -> pure effect
  PlayerEffect.CantBeTargetedBy _ -> pure effect
  PlayerEffect.HasProtectionFrom _ -> pure effect
  PlayerEffect.CastAsThoughItHadFlash _ -> pure effect
  PlayerEffect.MayPlayAsThoughItHadFlash _ -> pure effect
  PlayerEffect.ActivateKeywordAtInstantSpeed _ -> pure effect
  PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> pure effect
  PlayerEffect.CantBeCountered _ -> pure effect
  PlayerEffect.DamageCantBePrevented _ -> pure effect
  PlayerEffect.DamageCantBeRedirected _ -> pure effect
  PlayerEffect.CantSearchLibraries _ -> pure effect
  PlayerEffect.CantBecomeMonarch -> pure effect
  PlayerEffect.CantSetSchemesInMotion -> pure effect
  PlayerEffect.CantAttackWithCreatures -> pure effect
  PlayerEffect.CantCastMatching _ -> pure effect
  PlayerEffect.CastOnlyAtSorcerySpeed -> pure effect
  PlayerEffect.CantPlayLands _ -> pure effect
  PlayerEffect.CastFrom grant ->
    fmap (\ref -> PlayerEffect.CastFrom grant {CastFromZone.from = (CastFromZone.from grant) {InZone.player = ref}}) (f (InZone.player (CastFromZone.from grant)))
  PlayerEffect.PlayLandsFrom inZone ->
    fmap (\ref -> PlayerEffect.PlayLandsFrom inZone {InZone.player = ref}) (f (InZone.player inZone))
  PlayerEffect.PlotFrom grant ->
    fmap (\ref -> PlayerEffect.PlotFrom grant {PlotFromZone.from = (PlotFromZone.from grant) {InZone.player = ref}}) (f (InZone.player (PlotFromZone.from grant)))
  PlayerEffect.CastFromHandWithoutPayingManaCost _ -> pure effect
  PlayerEffect.CantGetCounters _ -> pure effect
  PlayerEffect.StateCoinFlip _ -> pure effect
  PlayerEffect.ModifyDieRoll _ -> pure effect
  PlayerEffect.AdditionalVotes _ -> pure effect
  PlayerEffect.AdditionalSurveilCards _ -> pure effect
  PlayerEffect.CantGainLife -> pure effect
  PlayerEffect.CantLoseLife -> pure effect

-- overPlayerRefs read, for a caller that only wants the references.
playerRefsIn :: PlayerEffect -> [PlayerRef.PlayerRef]
playerRefsIn = Functor.getConst . overPlayerRefs (Functor.Const . pure)

-- overPlayerRefs written, for a caller substituting each reference.
mapPlayerRefs :: (PlayerRef.PlayerRef -> PlayerRef.PlayerRef) -> PlayerEffect -> PlayerEffect
mapPlayerRefs f = Functor.runIdentity . overPlayerRefs (Functor.Identity . f)

-- Every Pawl.Types.DamagePattern this effect holds, traversed rather than read:
-- CR 615.12's prohibition and its CR 614.9 twin each carry one, and CR 601.2c's
-- recipient inside it is baked as the effect is stored (Pawl.Engine.Resolve's
-- AffectPlayers arm), exactly as overPlayerRefs above bakes a zone reference.
--
-- Applicative rather than a plain map, and the list instance is why: a pattern
-- whose slot named several recipients yields one effect per recipient, and one
-- whose slot named none yields no effect at all, which is CR 608.2b's answer.
--
-- Exhaustive with no wildcard, overPlayerRefs' posture: a new arm carrying a
-- pattern has to say so here, and @-Werror@ is what asks. This is a
-- CLASSIFICATION of the payload -- which arms narrow a damage event -- and never
-- a case on which card wrote one.
overDamagePatterns :: (Applicative f) => (DamagePattern.DamagePattern -> f DamagePattern.DamagePattern) -> PlayerEffect -> f PlayerEffect
overDamagePatterns f effect = case effect of
  PlayerEffect.CantCastSpells -> pure effect
  PlayerEffect.CantActivateAbilities _ -> pure effect
  PlayerEffect.CantCastMoreThan _ -> pure effect
  PlayerEffect.ModifyCost {} -> pure effect
  PlayerEffect.AlternativeActivationCost _ -> pure effect
  PlayerEffect.PlayAdditionalLands _ -> pure effect
  PlayerEffect.NoMaximumHandSize -> pure effect
  PlayerEffect.SetMaximumHandSize _ -> pure effect
  PlayerEffect.IncreaseMaximumHandSize _ -> pure effect
  PlayerEffect.ReduceMaximumHandSize _ -> pure effect
  PlayerEffect.DontLoseUnspentMana _ -> pure effect
  PlayerEffect.LoseLifeForUnspentMana -> pure effect
  PlayerEffect.SpendManaAsThough _ -> pure effect
  PlayerEffect.CantBeTargetedBy _ -> pure effect
  PlayerEffect.HasProtectionFrom _ -> pure effect
  PlayerEffect.CastAsThoughItHadFlash _ -> pure effect
  PlayerEffect.MayPlayAsThoughItHadFlash _ -> pure effect
  PlayerEffect.ActivateKeywordAtInstantSpeed _ -> pure effect
  PlayerEffect.ActivateLoyaltyAtInstantSpeed _ -> pure effect
  PlayerEffect.CantBeCountered _ -> pure effect
  PlayerEffect.DamageCantBePrevented pattern_ -> fmap PlayerEffect.DamageCantBePrevented (f pattern_)
  PlayerEffect.DamageCantBeRedirected pattern_ -> fmap PlayerEffect.DamageCantBeRedirected (f pattern_)
  PlayerEffect.CantSearchLibraries _ -> pure effect
  PlayerEffect.CantBecomeMonarch -> pure effect
  PlayerEffect.CantSetSchemesInMotion -> pure effect
  PlayerEffect.CantAttackWithCreatures -> pure effect
  PlayerEffect.CantCastMatching _ -> pure effect
  PlayerEffect.CastOnlyAtSorcerySpeed -> pure effect
  PlayerEffect.CantPlayLands _ -> pure effect
  PlayerEffect.CastFrom _ -> pure effect
  PlayerEffect.PlayLandsFrom _ -> pure effect
  PlayerEffect.PlotFrom _ -> pure effect
  PlayerEffect.CastFromHandWithoutPayingManaCost _ -> pure effect
  PlayerEffect.CantGetCounters _ -> pure effect
  PlayerEffect.StateCoinFlip _ -> pure effect
  PlayerEffect.ModifyDieRoll _ -> pure effect
  PlayerEffect.AdditionalVotes _ -> pure effect
  PlayerEffect.AdditionalSurveilCards _ -> pure effect
  PlayerEffect.CantGainLife -> pure effect
  PlayerEffect.CantLoseLife -> pure effect

-- overDamagePatterns read: the SLOTS this effect's patterns name as their
-- recipient (DamagePattern.boundRecipient), for the dataflow report
-- Pawl.Engine.Resolve.Slots.slotsOf owes -- playerRefsIn's shape, one payload
-- over.
boundRecipientSlots :: PlayerEffect -> [SlotName.SlotName]
boundRecipientSlots = Functor.getConst . overDamagePatterns (Functor.Const . Maybe.maybeToList . DamagePattern.boundRecipient)
