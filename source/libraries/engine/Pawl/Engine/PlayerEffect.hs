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
module Pawl.Engine.PlayerEffect where

import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaFilter as ManaFilter
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import Pawl.Types.CardName (CardName)
import Pawl.Types.CostAdjustments (CostAdjustments)
import qualified Pawl.Types.CostAdjustments as CostAdjustments
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.Face as Face
import Pawl.Types.Filter (Filter)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import Pawl.Types.Keyword (Keyword)
import Pawl.Types.ManaUnit (ManaUnit)
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerEffect (PlayerEffect)
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.PlayerScope (PlayerScope)
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import Pawl.Types.Subtype (Subtype)

-- CR 109.5: "you" on an object is its controller, and for a static ability the
-- CURRENT controller. `pid` is the player being asked about; `controller` is the
-- player the scope is anchored to. The argument order is (asked-about, anchor)
-- and the two are never interchangeable.
--
-- The anchor is CR 109.5's "you" for every caller but one: protectedFromTargeting
-- below passes the PROTECTED player, because CR 702.11c's "your opponents" are
-- the opponents of whoever has hexproof rather than of the effect's controller.
-- The parameter is named for the common case.
inScope :: PlayerId -> PlayerId -> PlayerScope -> Bool
inScope pid controller scope = case scope of
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
playersInScope :: Maybe PlayerId -> GameState -> PlayerScope -> Maybe [PlayerId]
playersInScope perspective gs scope =
  let everyone = Game.stillPlaying gs
      relative = fmap (\you -> filter (\pid -> inScope pid you scope) everyone) perspective
   in case scope of
        PlayerScope.You -> relative
        PlayerScope.Opponents -> relative
        PlayerScope.EachPlayer -> Just everyone

-- CR 604.2: every player effect applying to `pid` right now. Gathered LIVE from
-- the battlefield on every read and never captured, the same posture
-- Projection.gather takes for staticAbilities -- which is why Rule of Law
-- leaving the battlefield lifts its restriction with nothing to unwind.
--
-- The scope is resolved DYNAMICALLY (see Pawl.Types.PlayerScope): CR 611.2c
-- lets a rules-modifying effect reach objects that were not affected when it
-- began, so no set is ever frozen on this axis.
--
-- The (source, controller, scope, effect) rows are local: nothing outside this
-- function sees more than the (source, effect) pair it returns.
--
-- The SOURCE rides out alongside the effect, and only because CR 601.3a's
-- quality-bearing prohibitions read it: Null Chamber's "the chosen names" are
-- Object.chosenNames on the permanent that printed the ability, the same
-- direction Modification.AddChosenColor reads a colour. Nothing for a stored
-- CR 611.2c effect, which came from a resolved spell or ability and has no
-- permanent behind it -- and no such effect names a source-carried quality.
applying :: PlayerId -> GameState -> [(Maybe ObjectId, PlayerEffect)]
applying pid gs =
  let -- Hoisted out of the walk exactly as Projection.gather hoists it: an
      -- inlined call would recompute the whole game's SetLandSubtype list once
      -- per permanent.
      setEffs = Projection.setLandSubtypeEffects gs
      -- Hoisted for the same reason, and a thunk until a permanent that actually
      -- has a player ability forces it -- so the ordinary board pays nothing for
      -- the CR 604.2 question below.
      removed = Projection.abilityRemoval gs
      fromPermanent oid = case Game.faceOf oid gs of
        Nothing -> []
        Just face -> case Face.playerAbilities face of
          -- The overwhelming majority of permanents: no ability, so no
          -- controller projection and no CR 305.7 check is paid for.
          [] -> []
          abilities -> case Projection.controllerOf oid gs of
            Nothing -> []
            Just controller ->
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
                   in fmap (\ability -> (Just oid, controller, PlayerStaticAbility.scope ability, readAs (PlayerStaticAbility.effect ability))) abilities
                else []
      printed = concatMap fromPermanent (Set.toList (GameState.battlefield gs))
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
        ( Nothing,
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
      keep (source, controller, scope, _) =
        inScope pid controller scope
          && not (any (ignores source) (GameState.ignoredAbilities gs))
      ignores source ignored =
        IgnoredAbility.player ignored == pid
          && Just (IgnoredAbility.source ignored) == source
      effectOf (source, _, _, effect) = (source, effect)
   in fmap effectOf (filter keep (printed <> stored))

-- CR 612.1's subtype word swap over a PlayerEffect, the CR 613.10/613.11 axis's
-- answer to Pawl.Engine.Projection.rewriteModification. An Artificial Evolution
-- resolved at an Edgewalker moves "Cleric spells you cast cost {W}{B} less to
-- cast" onto the new word, because the word naming which spells the ability
-- discounts is text printed on the permanent like any other.
--
-- HERE and not beside rewriteModification, which is where the printed-text
-- rewrites for objects live: this module is the only one that may case on
-- PlayerEffect, and Pawl.Engine.Projection never sees the type at all. The
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
  -- The five arms carrying a Filter, which is the only place in this type a
  -- subtype word can hide. Thalia's "noncreature spells", Vedalken Orrery's
  -- "spells", Prowling Serpopard's "creature spells" and Heartstone's
  -- "activated abilities of creatures" name none today; Edgewalker's "Cleric
  -- spells" does.
  PlayerEffect.IncreaseSpellCost f n -> PlayerEffect.IncreaseSpellCost (Filter.rewrite pairs f) n
  PlayerEffect.ReduceSpellCost f cost -> PlayerEffect.ReduceSpellCost (Filter.rewrite pairs f) cost
  PlayerEffect.ReduceActivationCost f cost floor_ -> PlayerEffect.ReduceActivationCost (Filter.rewrite pairs f) cost floor_
  PlayerEffect.CastAsThoughItHadFlash f -> PlayerEffect.CastAsThoughItHadFlash (Filter.rewrite pairs f)
  PlayerEffect.CantBeCountered f -> PlayerEffect.CantBeCountered (Filter.rewrite pairs f)
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
  PlayerEffect.DontLoseUnspentMana _ -> effect
  PlayerEffect.CantBeTargetedBy _ -> effect
  PlayerEffect.DamageCantBePrevented _ -> effect
  PlayerEffect.CantSearchLibraries -> effect
  PlayerEffect.CantBecomeMonarch -> effect

-- CR 601.2i: how many spells this player has cast this turn. A fold over the
-- whole event log, which is exactly "this turn" because Engine.handoffTurn clears
-- it at the handoff and no reader ever drains it (scannedThrough is a watermark,
-- not a consumption). Rule of Law's ruling demands precisely this: the whole
-- turn, including spells cast before it was on the battlefield.
--
-- A Natural rather than an Integer, because a count of log entries cannot be
-- negative and CR 502.2's reader (GameState.spellsCastLastTurn, snapshotted by
-- Engine.beginTurnOf) holds one.
castsThisTurn :: PlayerId -> GameState -> Natural
castsThisTurn pid gs =
  let mine caster = caster == pid
   in Natural.length (filter mine (Maybe.mapMaybe (Game.castOf . snd) (Foldable.toList (GameState.events gs))))

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
-- card is asked this question once per half. Two of the three prohibitions are
-- quality-free -- "can't cast spells", "can't cast more than one spell" -- and
-- so ignore the name entirely; CR 601.3a's quality-bearing shape is what the
-- third needs it for (Null Chamber's "spells with the chosen names").
--
-- ONE name rather than the set CR 201.2a asks about ("at least one name in
-- common"). Every spell in this pool has exactly one name at this moment: the
-- proposal has already fixed the half, and nothing else has several names
-- (#650).
--
-- A NAME rather than the spell's object id, because the name is the whole of
-- what the three arms read ABOUT THE SPELL -- the other two read the player's
-- cast count and the source's own choice -- and the caller already has it: it
-- takes the name
-- off the chosen face, which is the only place it could come from, since pawl
-- projects nothing off the battlefield and the card is still in the zone it is
-- cast from (#160). A criterion over more of the spell than its name is what
-- would want the object back (#95).
--
-- CR 601.3a's LOOKAHEAD is still not implemented: the rule lets a player begin
-- casting when some choice made during the proposal could move the spell out of
-- the prohibited class, and nothing here searches choice space (#95).
--
-- Nothing in this pool reaches the LOOKAHEAD, and that rests on a missing
-- capability rather than on a claim about Magic. CR 601.2b names two choices
-- that are made BEFORE the announcement it governs and may restrict it --
-- "choosing to cast a spell with flashback from a graveyard or choosing to cast
-- a creature with morph face down" -- and both would move a spell across this
-- prohibition, since CR 702.37a gives a face-down spell "no name" at all.
--
-- Both of those choices are now MADE BEFORE this is asked rather than searched
-- for: each is its own Action, so the offer is per (half, facing) pair and this
-- predicate is asked once per pair with that pair's own name. CR 708.2a's empty
-- name is what a morph cast brings, which is why a Null Chamber naming the card
-- stops its face-up cast and not its face-down one -- Pawl.FaceDownSpec's
-- "CR 708.4 a prohibition naming the card stops the face-up cast and not the
-- morph one". What is still absent is the SEARCH: a player who must be told they
-- may begin casting because some choice not yet on the menu would escape the
-- prohibition gets no such offer (#95).
prohibitsCasting :: PlayerId -> CardName -> GameState -> Bool
prohibitsCasting pid name gs =
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
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.ReduceActivationCost {} -> False
        -- CR 305.2 raises how many LANDS may be played, and a land is never
        -- cast (CR 305.1), so this grant reaches nothing here.
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
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
   in any prohibits (applying pid gs)

-- CR 305.1: does any effect prohibit `pid` from PLAYING a land with this name?
-- The play-side twin of prohibitsCasting above, and a separate question rather
-- than a widening of it: CR 305.1 makes playing a land a special action that
-- never uses the stack, so a land is never a spell and none of the cast-side
-- prohibitions reaches it (Silence stops no land).
--
-- Takes a name for prohibitsCasting's reason, and one more of its own: the
-- caller has already asked whether the card is a land at all, off the same face
-- this name comes from. A land with several names would want the set CR 201.2a
-- asks about (#650), and none exists.
--
-- A DISJUNCTION for CR 101.2's reason.
prohibitsPlayingLand :: PlayerId -> CardName -> GameState -> Bool
prohibitsPlayingLand pid name gs =
  let prohibits (source, effect) = case effect of
        PlayerEffect.CantPlayLandChosenName -> Set.member name (chosenNamesOf source gs)
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
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.ReduceActivationCost {} -> False
        -- CR 305.2 raises HOW MANY lands may be played, never WHICH: a grant is
        -- no permission for a land this rule stops. landPlaysAllowed below is
        -- the gate that reads it.
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
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
   in any prohibits (applying pid gs)

-- CR 701.23: does any effect prohibit `pid` from searching a library?
--
-- Takes no library, unlike the two prohibitions above taking a name: no printed
-- effect narrows WHICH library, and Effect.Search searches only the resolving
-- controller's own (#1139). See Pawl.Types.PlayerEffect.CantSearchLibraries.
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
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantBecomeMonarch -> False
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
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
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
   in any (prohibits . snd) (applying pid gs)

-- CR 614.1c: the card names chosen as this effect's source entered
-- (Object.chosenNames). Empty for an effect with no source -- a stored CR 611.2c
-- row -- and for a permanent that chose nothing.
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
matchesObject :: Filter Keyword -> ObjectId -> GameState -> Bool
matchesObject filter_ oid gs =
  -- No source in scope at this site: `oid` is the AFFECTED object, not a source.
  Filter.matches (Filter.contextFor (Projection.controllerOf oid gs) Nothing) (Projection.viewOfObject oid gs) filter_

-- CR 613.11 / 601.2f: the cost increases and the cost reductions that apply to
-- `pid` CASTING `oid`.
--
-- The SPELL half of CR 601.2f, and the constructors it gathers are the whole of
-- the discriminator this module has: an arm read here is one whose sentence says
-- "spells", so Thalia's tax cannot reach an activation cost however her Filter
-- reads. activationCostAdjustments below is the other half.
--
-- No floor: no printed spell-cost reducer states Heartstone's sentence, so
-- CostAdjustments.minimumMana is zero here and CR 601.2f's own {0} is the only
-- floor a spell's total has.
--
-- matchesObject is called only from inside an arm that already matched a
-- cost-modifying constructor, so a board with no Thalia and no Medallion runs no
-- projections at all.
spellCostAdjustments :: PlayerId -> ObjectId -> GameState -> CostAdjustments
spellCostAdjustments pid oid gs =
  let matching :: Filter Keyword -> a -> Maybe a
      matching criterion amount = if matchesObject criterion oid gs then Just amount else Nothing
      increaseOf effect = case effect of
        PlayerEffect.IncreaseSpellCost criterion amount -> matching criterion amount
        PlayerEffect.ReduceSpellCost _ _ -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
      reductionOf effect = case effect of
        PlayerEffect.ReduceSpellCost criterion amount -> matching criterion amount
        PlayerEffect.IncreaseSpellCost _ _ -> Nothing
        -- The arm this whole split exists for: an ability's reduction is not a
        -- spell's, so it is gathered by activationCostAdjustments and never here.
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
      effects = fmap snd (applying pid gs)
   in CostAdjustments.MkCostAdjustments
        { CostAdjustments.increases = Maybe.mapMaybe increaseOf effects,
          CostAdjustments.reductions = Maybe.mapMaybe reductionOf effects,
          CostAdjustments.minimumMana = 0
        }

-- CR 613.11 / 601.2f: the cost reductions that apply to `pid` ACTIVATING an
-- ability of `srcId`, with the floor those reductions impose (CR 101.1 card
-- text). The ABILITY half of CR 601.2f, and the sibling of
-- spellCostAdjustments above.
--
-- The criterion is matched against `srcId`, the ability's SOURCE PERMANENT, which
-- is what Heartstone's "activated abilities of creatures" narrows -- so the same
-- matchesObject that reads a spell's characteristics for the caster reads a
-- permanent's here, and the permanent is projected rather than printed for the
-- same reason (an animated Vehicle's abilities are a creature's).
--
-- No INCREASES: nothing in pawl raises an activation cost, so the list is empty
-- rather than gathered (#1242). Kept in the record all the same, since CR 601.2f
-- orders increases before reductions whoever writes one.
--
-- The FLOOR is the MAXIMUM of the applying effects' floors: two effects each
-- forbidding a reduction below one mana forbid it once, and an effect that states
-- no floor cannot license another effect to ignore its own.
activationCostAdjustments :: PlayerId -> ObjectId -> GameState -> CostAdjustments
activationCostAdjustments pid srcId gs =
  let reductionOf effect = case effect of
        PlayerEffect.ReduceActivationCost criterion amount floor_ ->
          if matchesObject criterion srcId gs then Just (amount, floor_) else Nothing
        -- Thalia and Sapphire Medallion, turned away by the constructor and not
        -- by their Filters, which is exactly what keeps a noncreature permanent's
        -- activated ability untaxed (#90).
        PlayerEffect.IncreaseSpellCost _ _ -> Nothing
        PlayerEffect.ReduceSpellCost _ _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
      applicable = Maybe.mapMaybe (reductionOf . snd) (applying pid gs)
   in CostAdjustments.MkCostAdjustments
        { CostAdjustments.increases = [],
          CostAdjustments.reductions = fmap fst applicable,
          CostAdjustments.minimumMana = maximum (0 : fmap snd applicable)
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
-- Takes the OBJECT and not the half being proposed, so a split card's filter is
-- matched against CR 709.4's combined view rather than against the chosen half
-- (#656) -- the same seam spellCostAdjustments has, through the same matchesObject.
mayCastAsThoughItHadFlash :: PlayerId -> ObjectId -> GameState -> Bool
mayCastAsThoughItHadFlash pid oid gs =
  let allows effect = case effect of
        PlayerEffect.CastAsThoughItHadFlash criterion -> matchesObject criterion oid gs
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
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
          Just who -> inScope who pid scope
          Nothing -> case scope of
            PlayerScope.EachPlayer -> True
            PlayerScope.Opponents -> False
            PlayerScope.You -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        -- CR 701.6a grants no targeting immunity: Pawl.Types.Counterability
        -- says the same about CR 113.6g, and a Cancel at a spell Spider-Punk
        -- protects still targets it legally and still resolves.
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
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
        PlayerEffect.IncreaseSpellCost _ _ -> Nothing
        PlayerEffect.ReduceSpellCost _ _ -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
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
maximumHandSize :: PlayerId -> GameState -> Maybe Natural
maximumHandSize pid gs =
  let removes effect = case effect of
        PlayerEffect.NoMaximumHandSize -> True
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        PlayerEffect.CantBeCountered _ -> False
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
   in if any (removes . snd) (applying pid gs)
        then Nothing
        else Just defaultMaximumHandSize

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
-- Not the finest granularity the pool asks for. A card that keeps only the mana
-- it just added (Shizuko, Karn) needs the retention to leave this player-axis
-- carrier altogether (#352).
keepsUnspentMana :: PlayerId -> GameState -> ManaUnit -> Bool
keepsUnspentMana pid gs =
  let keeps effect = case effect of
        PlayerEffect.DontLoseUnspentMana f -> Just f
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.IncreaseSpellCost _ _ -> Nothing
        PlayerEffect.ReduceSpellCost _ _ -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.DamageCantBePrevented _ -> Nothing
        PlayerEffect.CantSearchLibraries -> Nothing
        PlayerEffect.CantBecomeMonarch -> Nothing
      filters = Maybe.mapMaybe (keeps . snd) (applying pid gs)
   in \unit -> any (\f -> ManaFilter.matches f unit) filters

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
  let stops effect = case effect of
        PlayerEffect.CantBeCountered criterion -> matchesObject criterion oid gs
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.ReduceActivationCost {} -> False
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
        -- Spider-Punk's OTHER sentence, and no part of this answer: CR 615.12
        -- is about a damage event and CR 701.6a about an object on the stack.
        -- The two travel together on one card and share nothing.
        PlayerEffect.DamageCantBePrevented _ -> False
        PlayerEffect.CantSearchLibraries -> False
        PlayerEffect.CantBecomeMonarch -> False
   in any (stops . snd) (applying pid gs)

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
-- The SOURCE rides out because SourceRelation.TheSource is resolved against it
-- -- Excruciator's clause names the permanent that prints it -- and it is the
-- Maybe `applying` already carries: Nothing for a stored CR 611.2c effect, which
-- has no permanent behind it, and no printing pairs one with a self-naming
-- pattern.
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
        PlayerEffect.CantBeCountered _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.IncreaseSpellCost _ _ -> Nothing
        PlayerEffect.ReduceSpellCost _ _ -> Nothing
        PlayerEffect.ReduceActivationCost {} -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
   in concatMap (\pid -> Maybe.mapMaybe says (applying pid gs)) (Game.stillPlaying gs)
