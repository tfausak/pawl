module Pawl.Engine.Quantity where

import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaCount as ManaCount
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaCount as ManaCount.Type
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Plus as Plus
import Pawl.Types.Quantity (Quantity)
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Rounding as Rounding
import qualified Pawl.Types.Scope as Scope
import Pawl.Types.SlotName (SlotName)

-- Nothing when the value cannot be determined.
--
-- `viewOf` and `context` are INJECTED rather than built here for module-cycle
-- reasons: Pawl.Engine.Projection imports this module, so nothing here can ask
-- the projection for a candidate's characteristics or the current perspective.
-- They flow through to Pawl.Engine.Count.evaluate for the Count arm; every
-- other arm ignores them.
--
-- CR 109.5 / 604.3a(3): whose "you" a quantity means is the CALLER's choice of
-- context. Projection.applyModification builds its context from the effect's
-- SOURCE's controller, Projection.applyCharacteristicPT from the OBJECT's own
-- controller, and Resolve from the resolving spell or ability's.
--
-- The ONE-OBJECT case, where CR 601.2b's announced X was stamped on the very
-- object every other arm reads -- true of a spell and of every caller outside a
-- resolution. evaluateFor below is where the two objects part company.
evaluate :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> Quantity -> Maybe Integer
evaluate viewOf context gs oid = evaluateFor viewOf context gs oid oid

-- The same fold with CR 601.2b's X taken from `announcedOn` instead of from
-- `oid`, because for an ACTIVATED ability they are different objects:
--
--   * `announcedOn` is the object ON THE STACK. That is where the announced
--     value is stamped -- Cast.castSpell stamps the spell's new incarnation,
--     Activate.activateAbility stamps the ability object -- so it is the only
--     place the value can be read back from.
--   * `oid` is the ability's SOURCE (CR 113.7), which every other arm reads and
--     which an activation cost may well have destroyed -- Cinder Elemental pays
--     with the very permanent the ability names, so by resolution CR 400.7 has
--     left its id naming nothing, and CR 113.7a is what lets the ability
--     resolve regardless (#544).
--
-- Quantity.InSlot asks `oid` FIRST and falls back to `announcedOn`, because it
-- has two writers: Resolve.bindAmountSlot writes to the effect's source
-- mid-resolution (Bane of Progress binds and reads one inside a TRIGGERED
-- ability, where the two ids differ), while Event.eventBindings writes to the
-- stack object as a trigger is gathered. See the arm itself.
evaluateFor :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> ObjectId -> Quantity -> Maybe Integer
evaluateFor viewOf context gs announcedOn oid = evaluateAgainst viewOf context gs announcedOn (Just oid) (viewOf oid)

-- The same fold aimed at a candidate that MAY NOT BE AN OBJECT. `mOid` is the
-- object the evaluation is aimed at and `mView` its characteristics; every
-- caller but one supplies both, through evaluateFor above.
--
-- The exception is a member of an Aggregation.Greatest over Scope.InHistory,
-- which has a view and no object: CR 608.2h's snapshot describes a past event
-- rather than anything on the battlefield now. So the view is passed
-- alongside rather than looked up from `mOid`, and the arms that read
-- characteristics read it; Quantity.InSlot, which reads BINDINGS rather than
-- characteristics, has only `announcedOn` to fall back on there, and that is
-- the object the fold's own resolution owns.
evaluateAgainst :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> Maybe ObjectId -> Maybe Filter.View -> Quantity -> Maybe Integer
evaluateAgainst viewOf context gs announcedOn mOid mView quantity = case quantity of
  Quantity.Literal n -> Just n
  -- CR 202.3 read through the injected view, exactly as the Power arm below is
  -- and for the extra reason that CR 707.2 makes mana cost copiable: layer 1
  -- replaces it, so a Clone entering as a copy of Darksteel Myr has mana value 3
  -- and not the 4 its own printed {3}{U} would give. Which face the printed cost
  -- came off (CR 712.8e, CR 708.2a) is settled at the projection's seed.
  --
  -- Nothing when the view cannot say: no object at all, or an object with no
  -- card behind it. WHICH view arrives is the caller's choice rather than the
  -- zone's, and every one of them answers CR 202.3 off the battlefield: the
  -- printed-card view for the readers that still take it
  -- (Projection.viewOfCardIn), and the CR 613 projection for the rest
  -- (Projection.fullView, Projection.viewUpTo), which project an object in any
  -- zone.
  Quantity.ManaValue -> mView >>= Filter.manaValue
  -- CR 208.1 read through the injected view, so this arm never learns whether
  -- it is looking at a live projection or a CR 608.2h snapshot -- the caller
  -- decides that by which ViewOf it supplies (Projection.fullView vs.
  -- Projection.viewWithLastKnown). Nothing when the object has no power: it is
  -- not a creature, or it is gone and no last known information was kept.
  Quantity.Power -> mView >>= Filter.power
  -- CR 208.1's other half, read off the same view and Nothing in the same places.
  Quantity.Toughness -> mView >>= Filter.toughness
  -- A value bound into the slot, read off the effect's SOURCE and then off the
  -- object on the stack. Nothing when neither holds an amount there: the
  -- producing effect has not run, or bound nothing.
  --
  -- TWO places because there are two writers, each of which binds where its value
  -- belongs:
  --
  --   * Resolve.bindAmountSlot writes to the SOURCE, mid-resolution -- Bane of
  --     Progress' "for each permanent destroyed this way".
  --   * Event.eventBindings writes to the object CR 603.3 put ON THE STACK, as
  --     the trigger was gathered -- Selfless Squire's "that many", the amount CR
  --     615.13's prevention supplied, and Sanguine Bond's and Exquisite Blood's
  --     "that much", the amount a life gain (CR 119.9) or a life loss (CR 119.3)
  --     supplied.
  --
  -- The source is asked first so the existing reading is untouched, and the two
  -- cannot collide over one name: a mid-resolution bind names a slot the CARD
  -- authored, and an event-supplied one names a reserved slot no card may name
  -- at all -- neither as a target slot (Pawl.CardSpec's reservedDeclarations)
  -- nor as an effect's bound SlotName (its reservedBindings). Both halves of
  -- that sweep are load-bearing HERE: the bind side is the one that could put a
  -- card's own write on the source, where this arm looks first (see
  -- Pawl.Engine.Binding.eventAmount).
  --
  -- CR 601.2b's X arrives here too, since #14 retired its dedicated arm. That arm
  -- read `announcedOn` ALONE, where this reads the source first and falls back --
  -- a difference only when the two ids differ AND the source carries an X binding
  -- of its own. It cannot: casting writes X to the object it announced on, and CR
  -- 400.7 mints a new object with no bindings on every zone change, so a
  -- permanent never carries the X its spell was cast for.
  Quantity.InSlot slot ->
    let boundOn holder = Game.lookupObject holder gs >>= Binding.amountOf slot . Object.bindings
     in fmap toInteger ((mOid >>= boundOn) <|> boundOn announcedOn)
  -- CR 208.2: a bare star has no value of its own. Both readers of a
  -- characteristic-defining P/T substitute the object's quantity for it first,
  -- through Projection.seedCharacteristicPT -- the projection at its seed
  -- (Projection.baseCharacteristics), and Projection.characteristicPowerIn off
  -- the battlefield -- so reaching this arm means the star was never resolved,
  -- honestly Nothing rather than a hole.
  Quantity.Star -> Nothing
  -- CR 608.2b: an effect may require information about a TARGET, which is not
  -- the ability's source (CR 113.7). Re-aim the fold at the object the slot
  -- names, so a payload can read the thing it points at. The CONTEXT rides
  -- through unchanged -- CR 109.5's "you" is still the resolving controller's --
  -- and only the object moves, which is what makes every object-reading arm
  -- (Power, ManaValue, ObjectCounters, HasDesignation) work under it at once.
  --
  -- `announcedOn` is fixed too, for the Count arm's reason: CR 601.2b's X belongs
  -- to the resolving object however the evaluation is aimed.
  --
  -- Nothing when the slot names no object. Filter.slotObjects is empty outside a
  -- resolution and omits an illegal slot (CR 608.2b) and a player recipient, so
  -- the three cases collapse onto the one answer -- unanswered, which every
  -- caller already treats as a no-op.
  --
  -- Terminating: the payload is a strictly smaller subterm.
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot inner) -> case Map.lookup slot (Filter.slotObjects context) of
    Nothing -> Nothing
    Just oid -> evaluateAgainst viewOf context gs announcedOn (Just oid) (viewOf oid) inner
  Quantity.Plus (Plus.MkPlus a b) -> case (recur a, recur b) of
    (Just x, Just y) -> Just (x + y)
    _ -> Nothing
  -- CR 107.1 / 107.1a: halve, then round the way the card printed. Nothing
  -- propagates from the payload, Plus' posture: half of a number nobody could
  -- determine is not a number either.
  Quantity.Halved (Halved.MkHalved rounding inner) -> fmap (halve rounding) (recur inner)
  -- CR 107.1b: the negated value, with no floor here -- a creature's power may be
  -- less than zero, and the readers that need a nonnegative count apply that
  -- rule's "zero is used instead" themselves. Unanswerable stays unanswerable:
  -- the negation of a value nothing could determine is not 0.
  Quantity.Negate a -> fmap negate (recur a)
  -- CR 208.2a / 608.2h: delegate to the general Count fold (Pawl.Engine.Count),
  -- which reads the CR 613 projection through the injected ViewOf. The second
  -- injection is this function itself, aimed at whichever CANDIDATE the fold is
  -- looking at, which is how Aggregation.Greatest reads a per-member quantity
  -- without Count importing this module. `announcedOn` stays FIXED across the
  -- candidates: CR 601.2b's X belongs to the resolving object. Terminating
  -- despite the mutual recursion -- a Greatest's payload is a strictly smaller
  -- subterm.
  Quantity.Count c -> Count.evaluate viewOf (\mOid' view -> evaluateAgainst viewOf context gs announcedOn mOid' (Just view)) context gs c
  -- CR 106.4: the mana-pool fold (Pawl.Engine.ManaCount). Takes neither
  -- injection the Count arm above does: a mana unit has no characteristics for
  -- the ViewOf to describe, and a ManaCount holds no inner Quantity for the
  -- reader to evaluate. It still needs the CONTEXT, which is what resolves its
  -- CR 109.5 "you" -- Omnath, Locus of Mana counts its own controller's pool.
  Quantity.ManaCount c -> ManaCount.evaluate context gs c
  -- CR 119.1: a player's life total, read STRAIGHT OFF GameState.players at the
  -- moment of the call for the reason the mana-pool arm above is -- CR 119.3
  -- adjusts a life total whenever an effect says so, with no state-based action
  -- and no priority pass owed in between, so a stored or sampled copy would go
  -- stale mid-resolution.
  --
  -- The PlayerRef is resolved by playersOf below -- Count.playersFor for every
  -- reference but the fold's own candidate -- which is what keeps one reference
  -- from meaning different players in different arms. Nothing for anything but
  -- EXACTLY ONE player: a life total is one player's scalar, so a reference
  -- naming several answers "whose?" rather than answering with a sum.
  --
  -- Maximising over several is a different shape and has its own spelling:
  -- Aggregation.Greatest over Scope.OverPlayers, with THIS arm reading each
  -- candidate through PlayerRef.Candidate. Malignus is that card, and it is why
  -- nothing here folds.
  Quantity.LifeTotal ref -> case playersOf ref of
    Just [pid] -> fmap Player.life (Map.lookup pid (GameState.players gs))
    _ -> Nothing
  -- CR 702.179e / 702.179f: a player's speed. LifeTotal's arm in every respect
  -- above -- read live, resolved through the same playersOf, and Nothing
  -- for a reference naming anything but exactly one player.
  --
  -- CR 702.179f is applied HERE and only here: "if that player has no speed,
  -- their speed is 0 for the purpose of an effect that refers to speed", and
  -- this arm IS such an effect's reading, so Player.speed of Nothing (CR
  -- 702.179b) answers Just 0. The outer Nothing means "which player?" went
  -- unanswered, which is a different claim -- a player the map does not hold at
  -- all is not a player with no speed.
  Quantity.Speed ref -> case playersOf ref of
    Just [pid] -> fmap (maybe 0 toInteger . Player.speed) (Map.lookup pid (GameState.players gs))
    _ -> Nothing
  -- CR 725.1: is that player the monarch? LifeTotal's and Speed's arm in what it
  -- reads and how the reference is resolved, but NOT in arity: CR 725.3 makes the
  -- monarch unique, so a disjunction over the named players and a sum over them
  -- agree on every board, and Queen Marchesa's "if an opponent is the monarch" is
  -- answerable at any number of seats. The siblings keep their one-player
  -- restriction, where the multi-player answer really is an aggregation choice
  -- (#681). EachPlayer therefore asks "is there a monarch?", and the empty list a
  -- departure (CR 800.4) can leave behind answers 0. Nothing stays reserved for a
  -- reference that could not be resolved at all.
  --
  -- CR 725.5 is applied HERE and only here: NO MONARCH answers Just 0, not
  -- Nothing. GameState.monarch of Nothing means CR 725.1's "there is no monarch
  -- in a game until an effect instructs a player to become the monarch", and a
  -- 0 on the measured side of a static ability's "as long as" clause makes that
  -- ability's continuous effect do nothing -- which is exactly what CR 725.5
  -- prescribes. Nothing would instead collapse to False through
  -- Condition.holds' undeterminable path, which reaches the same answer for
  -- this comparison by accident and would be the wrong claim about the rule.
  Quantity.IsMonarch ref -> case playersOf ref of
    Nothing -> Nothing
    Just pids -> Just (if any (\pid -> GameState.monarch gs == Just pid) pids then 1 else 0)
  -- CR 122.1: how many counters of a kind that player has. The third arm on
  -- LifeTotal's and Speed's terms -- live, one player only, through the same
  -- playersOf.
  --
  -- A kind the player's map does not hold answers 0 rather than Nothing, which
  -- is Player.counters' own convention and not this arm's invention: an absent
  -- key means the player has none of that counter, and "none" is a number. The
  -- outer Nothing is reserved for the reference, exactly as above.
  Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally ref kind) -> case playersOf ref of
    Just [pid] -> fmap (toInteger . Map.findWithDefault 0 kind . Player.counters) (Map.lookup pid (GameState.players gs))
    _ -> Nothing
  -- CR 122.1's OBJECT reading, through the injected view exactly as the Power
  -- arm above is -- so this arm never learns whether it is looking at a live
  -- projection or a CR 608.2h snapshot, and Projection.viewWithLastKnown is what
  -- answers Promising Duskmage's "if it had a +1/+1 counter on it" for a creature
  -- CR 400.7 has already deleted.
  --
  -- Filter.counters rather than Object.counters: reading the object directly
  -- would work while it lived and answer nothing at all once it died, which is
  -- the whole case this arm exists for.
  --
  -- A kind the map does not hold answers 0 rather than Nothing, the convention
  -- Object.counters and the PlayerCounters arm above both keep. The outer Nothing
  -- means the VIEW could not describe the object -- it is gone and nothing was
  -- filed under its id.
  Quantity.ObjectCounters kind -> fmap (toInteger . Map.findWithDefault 0 kind . Filter.counters) mView
  -- The designation as a 0/1, off the same view ObjectCounters reads -- so CR
  -- 608.2h's last known information answers for an object that is gone, which is
  -- what rule 702.112a's intervening "if" needs on resolution, and what CR 701.37a's
  -- and Repeat Offender's clause conditions need on theirs.
  --
  -- Nothing only where the view cannot describe the object at all, exactly as
  -- Power and ObjectCounters have it: an object nobody designated is not renowned,
  -- which is an answer.
  Quantity.HasDesignation d -> fmap (\view -> if Set.member d (Filter.designations view) then 1 else 0) mView
  -- CR 702.33d's designation as a 0/1, HasDesignation's arm in every respect. The
  -- object it reads is the RESOLVING SPELL, which is still on the stack while its
  -- own clause conditions are gated (Pawl.Engine.Resolve.gateHolds).
  Quantity.WasKicked -> fmap (\view -> if Filter.kicked view then 1 else 0) mView
  -- CR 508.3b: how many of that player's opponents were declared attacked this
  -- combat phase. LifeTotal's arm in shape -- live, one player only, resolved
  -- through the same playersOf, and Nothing for a reference naming
  -- anything but exactly one player, since "whose opponents?" has no sum.
  --
  -- Read off Combat.declaredAttacked and NOT Combat.attacked, which is that
  -- field's whole reason for existing: CR 508.4 says a creature put onto the
  -- battlefield attacking never "attacked", for trigger events AND effects, and
  -- rule 702.121a's is an effect.
  --
  -- NO liveness test on the players counted, deliberately: the record is what the
  -- rule asks about, so an opponent who has since left the game (CR 800.4) still
  -- counts, as does one whose attacker is no longer in combat. That is why this
  -- does not go through Count.playersFor's Opponent arm, which folds only
  -- Game.stillPlaying.
  --
  -- An EMPTY record answers 0 rather than Nothing: no attack declared is an
  -- answered question, and outside a combat phase the cleared record (CR 511.3)
  -- says the same thing. What is unanswered is only the reference.
  Quantity.OpponentsAttacked ref -> case playersOf ref of
    Just [pid] -> Just (toInteger (length (filter (attackedOpponent pid) (Set.toList (Combat.declaredAttacked (GameState.combat gs))))))
    _ -> Nothing
  -- CR 701.9a / 608.2i: how many cards that player has discarded this turn.
  -- OpponentsAttacked's arm in shape -- live, one player only, resolved through the
  -- same playersOf, and Nothing for a reference naming anything but exactly
  -- one player, since "whose discards?" has no sum.
  --
  -- A fold over GameState.events, which is cleared at turn handoff
  -- (Engine.beginTurnOf) -- so the log's extent IS "this turn" and nothing here
  -- names a window. Game.discardOf and not the Moved event the same discard also
  -- files; see that function for why the zone change is the wrong record.
  --
  -- BOTH DiscardCause values count, CR 702.29a making a cycled card a discarded
  -- one.
  --
  -- An EMPTY log answers 0 rather than Nothing, as OpponentsAttacked's empty
  -- record does: nobody having discarded is an answered question. What is
  -- unanswered is only the reference.
  Quantity.CardsDiscardedThisTurn ref -> case playersOf ref of
    Just [pid] -> Just (toInteger (length (filter ((== Just pid) . Game.discardOf . snd) (Foldable.toList (GameState.events gs)))))
    _ -> Nothing
  -- CR 509.1h's declaration, counted beyond the first: how many creatures are
  -- blocking the object this evaluation is aimed at, less one, floored at 0 for
  -- rule 702.23a's "beyond the first".
  --
  -- Read LIVE off Combat.blockers rather than through the injected view, combat
  -- being game state rather than a characteristic -- so an object CR 608.2h would
  -- answer for still answers here while the declaration stands. What fixes the
  -- number in time is the CALLER: Projection.freezeQuantities evaluates this as
  -- the ability resolves, which is CR 702.23b's "calculated only once per combat".
  --
  -- Nothing only for an evaluation aimed at no object -- a member of an
  -- Aggregation.Greatest over Scope.InHistory. An object nobody blocked is in no
  -- entry of the map and answers 0, which is a number and not a failure.
  Quantity.BlockersBeyondFirst ->
    fmap
      (\oid -> toInteger (max 0 (Set.size (Map.findWithDefault Set.empty oid (Combat.blockers (GameState.combat gs))) - 1)))
      mOid
  where
    recur = evaluateAgainst viewOf context gs announcedOn mOid mView
    -- CR 102.1's reference, resolved by Count.playersFor for every arm but the
    -- fold's own candidate. That one is answered HERE because this is where the
    -- candidate is: Count.evaluate's Scope.OverPlayers arm hands each candidate
    -- to this reader as a Pawl.Engine.Filter.playerView, whose identity IS the
    -- player, and Count.playersFor holds no view to read it from.
    --
    -- Nothing wherever the evaluation is not aimed at a player -- an object
    -- candidate's view carries no identity, and an evaluation outside a fold
    -- carries no candidate at all -- which is Pawl.Types.PlayerRef.Candidate's
    -- own stated answer there.
    playersOf ref = case ref of
      PlayerRef.Candidate -> fmap pure (mView >>= Filter.playerIdentity)
      -- The SECOND arm answered here rather than by Count.playersFor, and for a
      -- neighbouring reason: that function holds no view, and who controls a
      -- permanent is CR 613.1b's layer-2 question, which only a projection
      -- answers. The view is the caller's, which is how CR 608.2h reaches this --
      -- Spikeshell Harrier reads the speed of the player who controlled the
      -- permanent its own earlier clause has already bounced, and a last-known
      -- aware view is what still names them.
      --
      -- Nothing when the slot names no object or the view cannot describe it,
      -- Candidate's posture: the quantity is unanswered rather than answered off
      -- the resolving controller.
      PlayerRef.ControllerOfBound slot ->
        fmap pure (Map.lookup slot (Filter.slotObjects context) >>= viewOf >>= Filter.controller)
      PlayerRef.EachPlayer -> Count.playersFor context gs ref
      PlayerRef.Relative _ -> Count.playersFor context gs ref
      PlayerRef.InSlot _ -> Count.playersFor context gs ref
      PlayerRef.Specific _ -> Count.playersFor context gs ref

-- Is this declared attack an attack on one of that player's OPPONENTS? CR 506.3
-- gives three things a creature can attack and rule 702.121a counts only the
-- first: attacking an opponent's planeswalker or a battle they protect is not
-- attacking that opponent, which melee's own ruling states outright.
--
-- Every other player is an opponent (CR 102.2, CR 806.1), the reading
-- Pawl.Types.PlayerScope.Opponents and Count.playersFor already share; CR 102.3's
-- teammates are the one case it is wrong for, and pawl has no teams (#175).
attackedOpponent :: PlayerId.PlayerId -> AttackTarget.AttackTarget -> Bool
attackedOpponent pid target = case target of
  AttackTarget.OfPlayer other -> other /= pid
  AttackTarget.OfPlaneswalker _ -> False
  AttackTarget.OfBattle _ -> False

-- CR 107.1 / 107.1a: half of a number, as an integer, rounded the way the card
-- says. `div` floors and `negate . div (negate x)` is its ceiling, so both
-- directions mean the neighbouring integer rather than "away from zero" -- see
-- Pawl.Types.Rounding, which is where that reading is argued.
halve :: Rounding.Rounding -> Integer -> Integer
halve rounding n = case rounding of
  Rounding.Down -> div n 2
  Rounding.Up -> negate (div (negate n) 2)

-- CR 208.2a, last sentence: an undeterminable number is 0, including inside a
-- calculation. TOTAL where evaluate is partial -- an Integer, never a Maybe.
--
-- The recursion through Plus is what "inside a calculation" buys, and it is not
-- the same answer as substituting at the top: Tarmogoyf's printed 1+* is 1 when
-- its count cannot be determined, because it is the COUNT that becomes 0 and
-- not the sum. Plus, Halved and Negate are the calculations Pawl.Types.Quantity
-- has, and all three descend for that one reason -- but only Plus's descent
-- changes an answer. Half of CR 208.2a's substituted 0 is 0 whichever way it
-- rounds, so Malignus, whose whole CDA is a Halved, reads 0 with no opponents
-- either way; and no printed characteristic-defining P/T contains a Negate at
-- all. Both of those arms are consistency rather than a card's behaviour.
--
-- SCOPED TO THE CHARACTERISTIC-DEFINING ABILITY, as CR 208.2a is: the callers
-- are Projection.applyCharacteristicPT on the battlefield and
-- Projection.characteristicPowerIn off it (CR 604.3 makes the ability function
-- in all zones), and every other reader of a quantity must keep evaluate's
-- honest Nothing, since no rule tells those to invent a number.
--
-- It does NOT descend into a Count, and does not need to: an undeterminable
-- count IS the number CR 208.2a is talking about, so the 0 goes in whole here
-- and Pawl.Engine.Count.aggregate stays free to answer Nothing for non-CDA
-- readers.
determine :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> Quantity -> Integer
determine viewOf context gs oid quantity = case quantity of
  Quantity.Plus (Plus.MkPlus a b) -> determine viewOf context gs oid a + determine viewOf context gs oid b
  Quantity.Halved (Halved.MkHalved rounding inner) -> halve rounding (determine viewOf context gs oid inner)
  Quantity.Negate a -> negate (determine viewOf context gs oid a)
  _ -> Maybe.fromMaybe 0 (evaluate viewOf context gs oid quantity)

-- CR 208.2: resolve a printed star to the quantity a characteristic-defining
-- ability supplies, recursing through Plus so 1+* becomes 1+<the count>.
substituteStar :: Quantity -> Quantity -> Quantity
substituteStar star quantity = case quantity of
  Quantity.Star -> star
  Quantity.Plus (Plus.MkPlus a b) -> Quantity.Plus (Plus.MkPlus (substituteStar star a) (substituteStar star b))
  -- The same descent Plus takes, for CR 208.2's reason: a printed star inside a
  -- halving is still the value the CDA supplies. No card prints one there --
  -- Malignus' star is the whole P/T box and its CDA carries the halving.
  Quantity.Halved (Halved.MkHalved rounding inner) -> Quantity.Halved (Halved.MkHalved rounding (substituteStar star inner))
  -- Plus's descent, for Plus's reason: a star under a minus sign is still the
  -- star the characteristic-defining ability defines.
  Quantity.Negate a -> Quantity.Negate (substituteStar star a)
  Quantity.Literal _ -> quantity
  Quantity.ManaValue -> quantity
  Quantity.Power -> quantity
  Quantity.Toughness -> quantity
  Quantity.InSlot _ -> quantity
  Quantity.Count _ -> quantity
  Quantity.ManaCount _ -> quantity
  Quantity.LifeTotal _ -> quantity
  Quantity.Speed _ -> quantity
  Quantity.IsMonarch _ -> quantity
  Quantity.PlayerCounters {} -> quantity
  Quantity.ObjectCounters _ -> quantity
  Quantity.HasDesignation _ -> quantity
  Quantity.WasKicked -> quantity
  Quantity.OpponentsAttacked _ -> quantity
  Quantity.CardsDiscardedThisTurn _ -> quantity
  Quantity.BlockersBeyondFirst -> quantity
  -- No descent, for the Count arm's reason: CR 604.3 makes a CDA a static
  -- ability with no resolution and so no slots, and Pawl.CardSpec's
  -- powerToughnessSlots keeps a slot-naming quantity out of a printed P/T.
  Quantity.AgainstSlot {} -> quantity

-- The binding slots a quantity READS. The read half of the dataflow lint whose
-- write half is Resolve.definedSlots -- so a card whose "for each ... destroyed
-- this way" names a slot nothing binds is a failing test, not a silent no-op.
--
-- Binding.variableX is reported like any other slot, which is what #14 bought:
-- the "reads X iff the cost declares {X}" lint is then just the ordinary
-- available-slots comparison, because Pawl.CardSpec puts variableX on the
-- AVAILABLE side exactly when the cost prints an {X}. No arm here has to know
-- that X is special, and nothing subtracts it -- the fact lives where it belongs,
-- in what casting makes available.
slots :: Quantity -> Set SlotName
slots quantity = case quantity of
  Quantity.Literal _ -> Set.empty
  Quantity.ManaValue -> Set.empty
  Quantity.Power -> Set.empty
  Quantity.Toughness -> Set.empty
  Quantity.InSlot slot -> Set.singleton slot
  Quantity.Star -> Set.empty
  Quantity.Plus (Plus.MkPlus a b) -> Set.union (slots a) (slots b)
  -- Composition, as Plus is: the rounding names no slot and the payload may name
  -- any.
  Quantity.Halved (Halved.MkHalved _ inner) -> slots inner
  -- Whatever the payload reads, since a minus sign changes no slot: Toxic
  -- Deluge's -X is a Negate over the InSlot that names X. A REGRESSION FENCE
  -- rather than proven behaviour -- emptying this arm leaves the suite green,
  -- because the consumer that could tell (CR 603.3b's orderInert, through
  -- Resolve.modeSlots) is reached only by a TRIGGERED ability, and no card in
  -- the pool negates a slot read inside one.
  Quantity.Negate a -> slots a
  -- Terminating for the reason evaluate's Count arm is: a Greatest's payload is
  -- a strictly smaller subterm.
  Quantity.Count c -> Count.slots slots c
  -- Neither half of a ManaCount contributes an AMOUNT slot: a ManaFilter names
  -- no slot at all, and PlayerRef.InSlot names a TARGET slot, which is
  -- Resolve's half of the lint. Count's Scope is in the same position.
  --
  -- Resolve.slotsOf does NOT in fact recover such a nested ref, so no lint sees
  -- it (#1079). slotsAreExhaustive below reports these arms so the CR 603.3b
  -- elision cannot rest on the gap.
  Quantity.ManaCount _ -> Set.empty
  -- The same position a third time: this arm's PlayerRef.InSlot names a TARGET
  -- slot, not an amount one.
  Quantity.LifeTotal _ -> Set.empty
  -- And a fourth: LifeTotal's sibling carries a PlayerRef in the same position.
  Quantity.Speed _ -> Set.empty
  -- And a fifth, CR 725.1's designation -- a PlayerRef and nothing else.
  Quantity.IsMonarch _ -> Set.empty
  -- And a sixth. The PlayerCounterKind beside it names no slot either.
  Quantity.PlayerCounters {} -> Set.empty
  -- A bare CounterKind, which names no slot at all -- this arm carries no
  -- reference of any sort, the object being the one the evaluation is aimed at.
  Quantity.ObjectCounters _ -> Set.empty
  -- The designation, which carries no reference either -- ObjectCounters' position,
  -- with which designation in the kind's place.
  Quantity.HasDesignation _ -> Set.empty
  Quantity.WasKicked -> Set.empty
  -- And a seventh PlayerRef in that same position, CR 508.3b's record having
  -- nothing else on it.
  Quantity.OpponentsAttacked _ -> Set.empty
  -- And an eighth, CR 701.9a's tally having nothing beside its PlayerRef either.
  Quantity.CardsDiscardedThisTurn _ -> Set.empty
  -- And a nullary arm, which names nothing at all: CR 509.1h's declaration is
  -- read against the object the evaluation is aimed at, as ObjectCounters is.
  Quantity.BlockersBeyondFirst -> Set.empty
  -- The one arm that names a TARGET slot and reports it here anyway. Every other
  -- nested target slot is a PlayerRef this function leaves to Resolve.slotsOf,
  -- which cannot see it (#1079); reporting this one is what lets slotsOf recover
  -- it, and so what keeps Soul's Majesty's declared target on the read side of
  -- the D4 lint. The payload may hide slots of its own.
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot inner) -> Set.insert slot (slots inner)

-- CR 603.3b: is `slots` above the WHOLE of what evaluating this quantity reads
-- off the resolving object's bindings? It is not wherever a PlayerRef is nested
-- inside one: that names a TARGET slot, which `slots` leaves to
-- Resolve.slotsOf -- and slotsOf cannot see a reference buried in a quantity.
-- Resolve.slotsAreExhaustive is the sole caller and carries the whole account.
--
-- One arm per constructor, no wildcard, for that function's reason: a new
-- quantity arm carrying a reference must answer here rather than default to True
-- and license an unsound elision.
slotsAreExhaustive :: Quantity -> Bool
slotsAreExhaustive quantity = case quantity of
  Quantity.Literal _ -> True
  Quantity.ManaValue -> True
  Quantity.Power -> True
  Quantity.Toughness -> True
  Quantity.InSlot _ -> True
  Quantity.Star -> True
  Quantity.Plus (Plus.MkPlus a b) -> slotsAreExhaustive a && slotsAreExhaustive b
  -- Plus' answer: the rounding hides no reference, so what the payload hides is
  -- the whole question.
  Quantity.Halved (Halved.MkHalved _ inner) -> slotsAreExhaustive inner
  Quantity.Negate a -> slotsAreExhaustive a
  -- Both halves `slots` skips: the Scope's PlayerRef, and the per-member
  -- quantity of a Greatest, which may hide one of its own.
  Quantity.Count c ->
    scopeIsSlotless (Count.Type.scope c)
      && not (Count.anyQuantity (not . slotsAreExhaustive) c)
  Quantity.ManaCount c -> playerRefIsSlotless (ManaCount.Type.player c)
  Quantity.LifeTotal ref -> playerRefIsSlotless ref
  Quantity.Speed ref -> playerRefIsSlotless ref
  Quantity.IsMonarch ref -> playerRefIsSlotless ref
  Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally ref _) -> playerRefIsSlotless ref
  Quantity.ObjectCounters _ -> True
  Quantity.HasDesignation _ -> True
  Quantity.WasKicked -> True
  Quantity.OpponentsAttacked ref -> playerRefIsSlotless ref
  Quantity.CardsDiscardedThisTurn ref -> playerRefIsSlotless ref
  Quantity.BlockersBeyondFirst -> True
  -- True because `slots` above DOES report this arm's slot, unlike the nested
  -- PlayerRefs -- so the reported set is the whole of what evaluating it reads.
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot _ inner) -> slotsAreExhaustive inner

-- Only InSlot names a slot; the other three are answered from the evaluation
-- context alone (Resolve.playerRefSlots says the same thing as a set).
playerRefIsSlotless :: PlayerRef.PlayerRef -> Bool
playerRefIsSlotless ref = case ref of
  PlayerRef.EachPlayer -> True
  PlayerRef.Relative _ -> True
  PlayerRef.InSlot _ -> False
  -- The baked half names its player outright, so it reads no slot at all. A card
  -- cannot write one (Pawl.CardSpec's sweep), so this arm is only ever reached
  -- through a stored Expiry.While.
  PlayerRef.Specific _ -> True
  -- The fold's candidate names no slot either: it is read off the view the fold
  -- hands the evaluation, never off a binding.
  PlayerRef.Candidate -> True
  -- InSlot's answer, and for its reason: the slot is a TARGET slot, which
  -- Resolve.slotsOf is the half that reports -- and cannot see one buried in a
  -- quantity (#1079).
  PlayerRef.ControllerOfBound _ -> False

-- CR 611.2b: replace every PlayerRef.InSlot this quantity names with the baked
-- PlayerRef.Specific arm, off the players the resolution's bindings name. What
-- makes a "for as long as" condition that says "that player" answerable AFTER
-- its resolution: Pawl.Engine.Expiry.arm bakes as the duration begins, so the
-- stored condition names a seat rather than a slot on an object whose bindings
-- the sweep cannot reach. Pawl.Engine.Filter.bakeBound is the same move for a
-- target slot's atoms, and carries the argument for baking over threading.
--
-- The atom is LEFT STANDING when the environment names no player for the slot,
-- which is bakeBound's posture there too: Count.playersFor then answers Nothing
-- for it, Condition.holds collapses that to False, and CR 611.2b's duration
-- never starts -- rather than starting on a reference nothing can resolve.
--
-- Exhaustive, `slots`' posture: a new arm carrying a PlayerRef must fail to
-- compile here rather than silently keep an unbaked one.
bakeBound :: Map.Map SlotName PlayerId.PlayerId -> Quantity -> Quantity
bakeBound players quantity = case quantity of
  Quantity.LifeTotal ref -> Quantity.LifeTotal (bakePlayerRef players ref)
  Quantity.Speed ref -> Quantity.Speed (bakePlayerRef players ref)
  Quantity.IsMonarch ref -> Quantity.IsMonarch (bakePlayerRef players ref)
  Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally ref kind) -> Quantity.PlayerCounters (PlayerCounterTally.MkPlayerCounterTally (bakePlayerRef players ref) kind)
  Quantity.OpponentsAttacked ref -> Quantity.OpponentsAttacked (bakePlayerRef players ref)
  Quantity.CardsDiscardedThisTurn ref -> Quantity.CardsDiscardedThisTurn (bakePlayerRef players ref)
  Quantity.ManaCount c -> Quantity.ManaCount c {ManaCount.Type.player = bakePlayerRef players (ManaCount.Type.player c)}
  -- Both halves: the Scope says whose zone or which players, and an
  -- Aggregation.Greatest's per-member quantity may hide a reference of its own.
  -- Terminating for evaluate's reason -- a Greatest's payload is a strictly
  -- smaller subterm.
  Quantity.Count c ->
    let baked = Count.mapQuantity (bakeBound players) c
     in Quantity.Count baked {Count.Type.scope = bakeScope players (Count.Type.scope c)}
  Quantity.Plus (Plus.MkPlus a b) -> Quantity.Plus (Plus.MkPlus (bakeBound players a) (bakeBound players b))
  Quantity.Halved (Halved.MkHalved rounding inner) -> Quantity.Halved (Halved.MkHalved rounding (bakeBound players inner))
  Quantity.Negate a -> Quantity.Negate (bakeBound players a)
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot inner) -> Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot (bakeBound players inner))
  -- Every arm below holds no PlayerRef and no Quantity. InSlot names an AMOUNT
  -- slot rather than a player one, so nothing here substitutes it -- an amount an
  -- earlier effect bound is not a seat.
  Quantity.Literal _ -> quantity
  Quantity.ManaValue -> quantity
  Quantity.Power -> quantity
  Quantity.Toughness -> quantity
  Quantity.InSlot _ -> quantity
  Quantity.Star -> quantity
  Quantity.ObjectCounters _ -> quantity
  Quantity.HasDesignation _ -> quantity
  Quantity.WasKicked -> quantity
  Quantity.BlockersBeyondFirst -> quantity

-- One reference, baked. The whole of the substitution: every arm above funnels
-- through this, so what a slot means is stated once.
bakePlayerRef :: Map.Map SlotName PlayerId.PlayerId -> PlayerRef.PlayerRef -> PlayerRef.PlayerRef
bakePlayerRef players ref = case ref of
  PlayerRef.InSlot slot -> maybe ref PlayerRef.Specific (Map.lookup slot players)
  PlayerRef.EachPlayer -> ref
  PlayerRef.Relative _ -> ref
  PlayerRef.Specific _ -> ref
  -- Nothing to bake: the candidate is supplied by whichever fold is running when
  -- the quantity is evaluated, so a stored condition carrying one still resolves
  -- it the same way. What baking fixes is a reference to the RESOLUTION's
  -- bindings, which this is not.
  PlayerRef.Candidate -> ref
  -- LEFT STANDING, the posture Pawl.Engine.Filter.bakeBound takes for a slot its
  -- map cannot answer: this map holds the PLAYERS a resolution's slots name (CR
  -- 603.2), and this reference names a slot holding an OBJECT, whose controller
  -- only a projection gives. A stored CR 611.2b duration reading it therefore
  -- goes unanswered and ends, which is Pawl.Engine.Condition.holds' stated
  -- collapse; no card in the pool stores one (#1441).
  PlayerRef.ControllerOfBound _ -> ref

-- A scope's reference, baked. Both scopes that name players take one; CR 608.2i's
-- look-back names none.
bakeScope :: Map.Map SlotName PlayerId.PlayerId -> Scope.Scope -> Scope.Scope
bakeScope players scope = case scope of
  Scope.InZone (InZone.MkInZone zone ref) -> Scope.InZone (InZone.MkInZone zone (bakePlayerRef players ref))
  Scope.OverPlayers ref -> Scope.OverPlayers (bakePlayerRef players ref)
  Scope.InHistory _ -> scope

-- CR 608.2i's look-back names no player and no slot; a zone scope names whose
-- zone it is, and a player scope names the players themselves -- the same
-- reference either way, so the same question.
scopeIsSlotless :: Scope.Scope -> Bool
scopeIsSlotless scope = case scope of
  Scope.InZone (InZone.MkInZone _ ref) -> playerRefIsSlotless ref
  Scope.InHistory _ -> True
  Scope.OverPlayers ref -> playerRefIsSlotless ref

-- Does this quantity read CR 601.2b's announced X? Since #14 retired X's
-- dedicated constructor, that read is a Quantity.InSlot naming
-- Binding.variableX, and it can sit anywhere inside a quantity rather than only
-- at its root -- so answering needs the same recursion slots above has, and an
-- equality test against a bare X does not answer it at all.
--
-- Resolve.readsX is the one caller: it asks "does this card read X?" for the lint
-- that pairs a read against the cost's {X} (CR 107.3, CR 107.3a, CR 118.4).
-- Vitalizing Cascade's "X plus 3" is the card that distinguishes the two
-- answers.
--
-- Written out arm by arm rather than as a filter over slots, for the reason slots
-- itself is: a new Quantity constructor must make its author answer "does this
-- read X?" explicitly, rather than inherit whatever the other function decided.
readsX :: Quantity -> Bool
readsX quantity = case quantity of
  Quantity.InSlot slot -> slot == Binding.variableX
  -- The whole point of the recursion: Vitalizing Cascade's "X plus 3" is
  -- Plus X (Literal 3), which reads X without being equal to it.
  Quantity.Plus (Plus.MkPlus a b) -> readsX a || readsX b
  -- The same recursion: "half X, rounded down" would be a Halved over an X that
  -- is not equal to one. A REGRESSION FENCE rather than proven behaviour --
  -- neither producer halves an announced value, so answering False here leaves
  -- the suite green.
  Quantity.Halved (Halved.MkHalved _ inner) -> readsX inner
  -- Toxic Deluge's "-X" is Negate X, which reads X the same way. Without this
  -- arm the CR 107.3 lint would call the card an unannounced-X reader on one
  -- side and an unread announcement on the other.
  Quantity.Negate a -> readsX a
  -- Terminating for the reason slots' Count arm is: a Greatest's payload is a
  -- strictly smaller subterm.
  Quantity.Count c -> Count.anyQuantity readsX c
  -- Every remaining arm is a LEAF holding no Quantity, so none can hide an X.
  -- The five references below (ManaCount's, LifeTotal's, Speed's, IsMonarch's,
  -- PlayerCounters') are PlayerRefs, whose InSlot names a TARGET slot rather
  -- than an amount one, and X is only ever an amount.
  Quantity.Literal _ -> False
  Quantity.ManaValue -> False
  Quantity.Power -> False
  Quantity.Toughness -> False
  Quantity.Star -> False
  Quantity.ManaCount _ -> False
  Quantity.LifeTotal _ -> False
  Quantity.Speed _ -> False
  Quantity.IsMonarch _ -> False
  Quantity.PlayerCounters {} -> False
  Quantity.ObjectCounters _ -> False
  Quantity.HasDesignation _ -> False
  Quantity.WasKicked -> False
  Quantity.OpponentsAttacked _ -> False
  Quantity.CardsDiscardedThisTurn _ -> False
  Quantity.BlockersBeyondFirst -> False
  -- Not a leaf: its payload is a whole Quantity and may read X, the same recursion
  -- Plus above needs. Its own SlotName names a target rather than an amount, and X
  -- is only ever an amount.
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot _ inner) -> readsX inner

-- CR 202.3: each generic symbol contributes its number, each colored or
-- colorless symbol one, and each hybrid symbol its largest half (CR 202.3f). A
-- land has no mana cost (CR 202.1b), so its mana value is 0 (CR 202.3a).
manaValueOf :: Face.Face Card.Card -> Integer
manaValueOf face = case Face.manaCost face of
  Nothing -> 0
  Just (ManaCost.MkManaCost symbols) -> sum (fmap symbolValue symbols)

symbolValue :: ManaSymbol.ManaSymbol -> Integer
symbolValue symbol = case symbol of
  ManaSymbol.Generic n -> toInteger n
  ManaSymbol.OfType _ -> 1
  -- CR 202.3f: the largest component. Both halves of a colour/colour hybrid are
  -- one mana, so the largest is one.
  ManaSymbol.Hybrid {} -> 1
  -- CR 202.3f again, but here the halves differ: {2/B}'s generic half is the
  -- larger, so the symbol is worth 2 rather than every other typed symbol's 1.
  ManaSymbol.MonocoloredHybrid _ -> 2
  -- CR 202.3g, a rule of its own rather than CR 202.3f's largest component: the
  -- other half is 2 LIFE, not 2 mana, so there is no larger component to take.
  -- Mutagenic Growth ({G/P}) is 1, not 2.
  ManaSymbol.Phyrexian _ -> 1
  -- CR 202.3's own sentence, with no subrule: CR 107.4h makes {S} payable with
  -- one mana from a snow source, so Icehide Golem's mana value is 1.
  ManaSymbol.Snow -> 1
  -- CR 202.3e: off the stack a variable's contribution to mana value is 0.
  ManaSymbol.Variable -> 0
