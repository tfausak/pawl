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
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaFilter as ManaFilter
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import Pawl.Types.CardName (CardName)
import qualified Pawl.Types.Face as Face
import Pawl.Types.Filter (Filter)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import Pawl.Types.ManaCost (ManaCost)
import Pawl.Types.ManaUnit (ManaUnit)
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerEffect (PlayerEffect)
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.PlayerScope (PlayerScope)
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility

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
              if (null setEffs || Projection.liveGiven setEffs Set.empty oid gs)
                && not (removed oid)
                then fmap (\ability -> (Just oid, controller, PlayerStaticAbility.scope ability, PlayerStaticAbility.effect ability)) abilities
                else []
      printed = concatMap fromPermanent (Set.toList (GameState.battlefield gs))
      -- CR 611.2c: the stored carrier. Its controller is read off the record and
      -- never re-derived -- see Pawl.Types.ActivePlayerEffect -- while its scope is
      -- resolved live, exactly as the printed carrier's is.
      --
      -- Neither gate above touches it, because it is not an ability for CR 613.1f
      -- to remove: CR 611.2a gives a resolved spell's continuous effect a duration
      -- of its own. Humility cannot take back a Silence that has already resolved.
      storedOne active =
        ( Nothing,
          ActivePlayerEffect.controller active,
          ActivePlayerEffect.scope active,
          ActivePlayerEffect.effect active
        )
      stored = fmap storedOne (GameState.playerEffects gs)
      keep (_, controller, scope, _) = inScope pid controller scope
      effectOf (source, _, _, effect) = (source, effect)
   in fmap effectOf (filter keep (printed <> stored))

-- CR 601.2i: how many spells this player has cast this turn. A fold over the
-- whole event log, which is exactly "this turn" because Engine.handoffTurn clears
-- it at the handoff and no reader ever drains it (scannedThrough is a watermark,
-- not a consumption). Rule of Law's ruling demands precisely this: the whole
-- turn, including spells cast before it was on the battlefield.
castsThisTurn :: PlayerId -> GameState -> Integer
castsThisTurn pid gs =
  let mine caster = caster == pid
   in toInteger (length (filter mine (Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events gs)))))

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
-- Nothing in this pool reaches it, and that rests on a missing capability rather
-- than on a claim about Magic. CR 601.2b names two choices that are made BEFORE
-- the announcement it governs and may restrict it -- "choosing to cast a spell
-- with flashback from a graveyard or choosing to cast a creature with morph face
-- down" -- and both would move a spell across this prohibition, since CR 702.37a
-- gives a face-down spell "no name" at all. CR 709.3's half is settled the same
-- way, before the card is put onto the stack, which is why each half is offered
-- as its own action here. Face-down casting is what pawl lacks: there is no
-- face-down state for anything to be in (#192).
prohibitsCasting :: PlayerId -> CardName -> GameState -> Bool
prohibitsCasting pid name gs =
  let cast = castsThisTurn pid gs
      prohibits (source, effect) = case effect of
        PlayerEffect.CantCastSpells -> True
        PlayerEffect.CantCastMoreThan limit -> cast >= toInteger limit
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
   in any prohibits (applying pid gs)

-- CR 614.1c: the card names chosen as this effect's source entered
-- (Object.chosenNames). Empty for an effect with no source -- a stored CR 611.2c
-- row -- and for a permanent that chose nothing.
--
-- The empty set is the answer that matches NO object rather than every object,
-- which is the shape CR 201.2a describes for an object with no name: having no
-- name is not sharing one.
chosenNamesOf :: Maybe ObjectId -> GameState -> Set.Set CardName
chosenNamesOf source gs = maybe Set.empty Object.chosenNames (source >>= \oid -> Game.lookupObject oid gs)

-- Does this spell match a player effect's Filter? Shared by the two questions
-- that carry one -- CR 601.2f's cost adjustments and CR 601.3b's timing
-- permission -- so that "which spells does this effect name?" has one reading.
--
-- Evaluated against the PROJECTED view (Projection.viewOfObject) -- a card type
-- is CR 613.1d layer 4 and a colour is CR 613.1e layer 5 -- never a printed
-- characteristic. The perspective is the spell's own controller (CR 109.5). Runs
-- through the identity-blind Filter.matches: this module never learns which
-- spell produced the Filter.
matchesSpell :: Filter Keyword -> ObjectId -> GameState -> Bool
matchesSpell filter_ oid gs =
  -- No source in scope at this site: `oid` is the AFFECTED object, not a source.
  Filter.matches (Filter.MkContext (Projection.controllerOf oid gs) Nothing) (Projection.viewOfObject oid gs) filter_

-- CR 613.11 / 601.2f: the cost increases and the cost reductions that apply to
-- `pid` casting `oid`, as two lists.
--
-- Kept APART, never summed into one signed delta: CR 601.2f applies every
-- increase before any reduction, and CR 118.7a gives a reduction a restriction
-- an increase does not have. The two halves do not even have the same shape --
-- an increase is an amount of generic mana and a reduction is a ManaCost.
-- Pawl.Engine.Cost.applyAdjustments consumes the pair; this only decides
-- membership.
--
-- matchesSpell is called only from inside an arm that already matched a
-- cost-modifying constructor, so a board with no Thalia and no Medallion runs no
-- projections at all.
costAdjustments :: PlayerId -> ObjectId -> GameState -> ([Natural], [ManaCost])
costAdjustments pid oid gs =
  let matching :: Filter Keyword -> a -> Maybe a
      matching criterion amount = if matchesSpell criterion oid gs then Just amount else Nothing
      increaseOf effect = case effect of
        PlayerEffect.IncreaseSpellCost criterion amount -> matching criterion amount
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
      reductionOf effect = case effect of
        PlayerEffect.ReduceSpellCost criterion amount -> matching criterion amount
        PlayerEffect.IncreaseSpellCost _ _ -> Nothing
        PlayerEffect.CantCastSpells -> Nothing
        PlayerEffect.CantCastMoreThan _ -> Nothing
        PlayerEffect.CantCastChosenName -> Nothing
        PlayerEffect.CantPlayLandChosenName -> Nothing
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
      effects = fmap snd (applying pid gs)
   in (Maybe.mapMaybe increaseOf effects, Maybe.mapMaybe reductionOf effects)

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
-- matchesSpell is called only from inside the arm that already matched, so a
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
-- (#656) -- the same seam costAdjustments has, through the same matchesSpell.
mayCastAsThoughItHadFlash :: PlayerId -> ObjectId -> GameState -> Bool
mayCastAsThoughItHadFlash pid oid gs =
  let allows effect = case effect of
        PlayerEffect.CastAsThoughItHadFlash criterion -> matchesSpell criterion oid gs
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.CantCastSpells -> False
        PlayerEffect.CantCastMoreThan _ -> False
        PlayerEffect.CantCastChosenName -> False
        PlayerEffect.CantPlayLandChosenName -> False
        PlayerEffect.IncreaseSpellCost _ _ -> False
        PlayerEffect.ReduceSpellCost _ _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
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
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.NoMaximumHandSize -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
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
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.DontLoseUnspentMana _ -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
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
        PlayerEffect.PlayAdditionalLands _ -> False
        PlayerEffect.DontLoseUnspentMana _ -> False
        PlayerEffect.CantBeTargetedBy _ -> False
        PlayerEffect.CastAsThoughItHadFlash _ -> False
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
        PlayerEffect.PlayAdditionalLands _ -> Nothing
        PlayerEffect.NoMaximumHandSize -> Nothing
        PlayerEffect.CantBeTargetedBy _ -> Nothing
        PlayerEffect.CastAsThoughItHadFlash _ -> Nothing
      filters = Maybe.mapMaybe (keeps . snd) (applying pid gs)
   in \unit -> any (\f -> ManaFilter.matches f unit) filters
