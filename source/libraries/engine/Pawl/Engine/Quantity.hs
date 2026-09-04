module Pawl.Engine.Quantity where

import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaCount as ManaCount
import qualified Pawl.Engine.QuantitySlot as QuantitySlot
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.CompletedDungeon as CompletedDungeon
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.ManaCost as ManaCost
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
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

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
-- Quantity.InSlot asks `oid` FIRST, falls back to `announcedOn`, and then to
-- GameState.ambientAmounts, because it has three writers:
-- Resolve.bindAmountSlot writes to the effect's source mid-resolution (Bane of
-- Progress binds and reads one inside a TRIGGERED ability, where the two ids
-- differ), Event.eventBindings writes to the stack object as a trigger is
-- gathered, and Resolve.runPreventionRider writes the ambient channel, which
-- belongs to no object at all. See the arm itself.
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
  -- zone's, and the view a reader holding an OBJECT supplies is the CR 613
  -- projection (Projection.fullView, Projection.viewUpTo), which projects an
  -- object in any zone and so answers CR 202.3 off the battlefield too.
  Quantity.ManaValue -> mView >>= Filter.manaValue
  -- CR 208.1 read through the injected view, so this arm never learns whether
  -- it is looking at a live projection or a CR 608.2h snapshot -- the caller
  -- decides that by which ViewOf it supplies (Projection.fullView vs.
  -- Projection.viewWithLastKnown). Nothing when the object has no power: it is
  -- not a creature, or it is gone and no last known information was kept.
  Quantity.Power -> mView >>= Filter.power
  -- CR 208.1's other half, read off the same view and Nothing in the same places.
  Quantity.Toughness -> mView >>= Filter.toughness
  -- A value bound into the slot, read off the effect's SOURCE, then off the
  -- object on the stack, then out of the announcement the context carries, and
  -- last out of the ambient channel. Nothing when none of the four holds an
  -- amount: the producing effect has not run, or bound nothing.
  --
  -- FOUR places because there are four writers, each of which puts its value
  -- where that value belongs:
  --
  --   * Resolve.bindAmountSlot writes to the SOURCE, mid-resolution -- Bane of
  --     Progress' "for each permanent destroyed this way".
  --   * Event.eventBindings writes to the object CR 603.3 put ON THE STACK, as
  --     the trigger was gathered -- Selfless Squire's "that many", the amount CR
  --     615.13's prevention supplied; Sanguine Bond's and Exquisite Blood's
  --     "that much", the amount a life gain (CR 119.9) or a life loss (CR 119.3)
  --     supplied; and Shroofus Sproutsire's "that many", the combat damage CR
  --     510.2 dealt a player.
  --   * Target.slotContext writes Filter.Context's boundAmounts, for the span of
  --     one CR 202.3 computed bound on a target slot -- Venerable Warsinger's
  --     "mana value X or less ... where X is the amount of damage this creature
  --     dealt to that player", and Stir the Grave's "mana value X or less" off CR
  --     601.2b's announced X. THE ANNOUNCEMENT IS OFF-OBJECT ON TWO OF THIS
  --     CALLER'S THREE ROADS, which is why it needs a channel of its own: CR
  --     603.3d chooses a trigger's targets before the ability object carries any
  --     binding at all, and CR 601.2c chooses a spell's before CR 601.2i stamps
  --     the X onto it, so on neither can the two readings above reach what the
  --     announcement already holds, and the caller hands it over instead.
  --
  --     The third road is CR 707.10c's, and there the announcement IS on the
  --     object: Resolve.chooseNewTargetsFor re-chooses a copy's targets off a
  --     copy that CR 707.10 has already stamped with the value of X, and
  --     slotContext evaluates the bound with the copy's own id -- so the FIRST
  --     reading answers and this channel, which that caller also seeds, is never
  --     consulted for the X. It is seeded anyway because the same seed carries
  --     every other binding the copy kept.
  --
  --     Read after both object readings, so neither is disturbed, and it can
  --     collide with neither -- the map is empty except while a target slot's
  --     bound is being evaluated.
  --   * Resolve.runPreventionRider writes GameState.ambientAmounts, for the span
  --     of one CR 615.5 rider -- Inkshield's "for each 1 damage prevented this
  --     way". That writer has NO object to bind to: the shielded recipient can be
  --     a player, and CR 400.7 replaced the installing spell. Read LAST, so no
  --     reading above is disturbed, and it can collide with none of them: the
  --     map is empty except while a rider runs, and a rider's own effects are the
  --     only readers alive then.
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
     in fmap toInteger ((mOid >>= boundOn) <|> boundOn announcedOn <|> Map.lookup slot (Filter.boundAmounts context) <|> Map.lookup slot (GameState.ambientAmounts gs))
  -- CR 208.2: a bare star has no value of its own. Both readers of a
  -- characteristic-defining P/T substitute the object's quantity for it first,
  -- through Projection.seedCharacteristicPT at the projection's seed
  -- (Projection.baseCharacteristics), in every zone as CR 604.3 asks -- so
  -- reaching this arm means the star was never resolved, honestly Nothing rather
  -- than a hole.
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
  -- resolution and omits an illegal slot (CR 608.2b) and a player recipient, and
  -- Filter.slotOneObject declines a slot naming SEVERAL rather than picking one
  -- of them -- so the four cases collapse onto the one answer, unanswered, which
  -- every caller already treats as a no-op.
  --
  -- Terminating: the payload is a strictly smaller subterm.
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot inner) -> case Filter.slotOneObject slot context of
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
  -- CR 106.4: the mana-pool fold (Pawl.Engine.ManaCount). Takes the ViewOf but
  -- not the second injection the Count arm above does: a mana unit has no
  -- characteristics for a projection to describe, so the view is there only to
  -- resolve WHOSE pool (CR 613.1b's layer-2 controller), and a ManaCount holds no
  -- inner Quantity for the reader to evaluate. It still needs the CONTEXT, which
  -- is what resolves its CR 109.5 "you" -- Omnath, Locus of Mana counts its own
  -- controller's pool.
  Quantity.ManaCount c -> ManaCount.evaluate viewOf context gs c
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
  -- candidate through PlayerRef.Candidate -- Malignus is one such card, Daybreak
  -- Ranger // Nightfall Predator another -- and that shape is why nothing here
  -- folds.
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
  -- CR 103.1: is that player the starting player? IsMonarch's arm down to the
  -- arity argument -- there is exactly one starting player, so a disjunction over
  -- the named seats and a sum over them agree on every board.
  --
  -- The head of GameState.turnOrder, which CR 103.1's last sentence defines as
  -- the starting player's seat, and which that field is documented to be rotated
  -- to. It is never shortened by a departure, so a game whose starting player has
  -- left still answers with them -- the seat is the rule's subject, not who is
  -- still playing.
  --
  -- An EMPTY roster answers 0 rather than Nothing, IsMonarch's posture: there is
  -- no starting player, which is a number, and Nothing stays reserved for a
  -- reference that could not be resolved at all.
  Quantity.IsStartingPlayer ref -> case playersOf ref of
    Nothing -> Nothing
    Just pids -> Just (if any (\pid -> Maybe.listToMaybe (GameState.turnOrder gs) == Just pid) pids then 1 else 0)
  -- CR 102.1: is that player the active player? The two arms above's shape again,
  -- and the simplest of the three: GameState.activePlayer is a PlayerId rather
  -- than a Maybe, so there is no "no active player" board for this arm to have a
  -- posture about.
  Quantity.IsActivePlayer ref -> case playersOf ref of
    Nothing -> Nothing
    Just pids -> Just (if any (\pid -> GameState.activePlayer gs == pid) pids then 1 else 0)
  -- CR 122.1: how many counters of a kind that player has. The third arm on
  -- LifeTotal's and Speed's terms -- live, one player only, through the same
  -- playersOf.
  --
  -- A kind the player's map does not hold answers 0 rather than Nothing, which
  -- is Player.counters' own convention and not this arm's invention: an absent
  -- key means the player has none of that counter, and "none" is a number. The
  -- outer Nothing is reserved for the reference, exactly as above.
  --
  -- "AN opponent has three or more poison counters" is therefore NOT written
  -- here: it is an existential over the opponents, and it gets LifeTotal's
  -- spelling -- Aggregation.Greatest over Scope.OverPlayers, with this arm
  -- reading each candidate through PlayerRef.Candidate, since a maximum of at
  -- least three and a member of at least three are the same claim. Viral
  -- Spawning's Corrupted clause is that card, and CastSpec's three-seat
  -- GrantedFlashback case is what proves the reading (a two-seat board cannot:
  -- there "an opponent" and "your opponent" name one player).
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
  -- CR 122.1 without the kind: the SUM over every kind the object carries, off the
  -- same field and the same view the arm above reads, so CR 608.2h answers this one
  -- for a gone object too. An object with no counters at all sums to 0, which is the
  -- answer "it had no counters on it" wants rather than a Nothing.
  Quantity.ObjectCountersOfAnyKind -> fmap (toInteger . sum . Filter.counters) mView
  -- The designation as a 0/1, off the same view ObjectCounters reads -- so CR
  -- 608.2h's last known information answers for an object that is gone, which is
  -- what rule 702.112a's intervening "if" needs on resolution, and what CR 701.37a's
  -- and Repeat Offender's clause conditions need on theirs.
  --
  -- Nothing only where the view cannot describe the object at all, exactly as
  -- Power and ObjectCounters have it: an object nobody designated is not renowned,
  -- which is an answer.
  Quantity.HasDesignation d -> fmap (\view -> if Set.member d (Filter.designations view) then 1 else 0) mView
  -- CR 716.2d is applied HERE and nowhere else: a permanent with no level reads
  -- as level 1 for every rule and effect that asks, so the default belongs at the
  -- one read rather than in the field Filter.classLevel reports.
  Quantity.ClassLevel -> fmap (toInteger . ClassLevel.defaulted . Filter.classLevel) mView
  -- CR 702.33d's designation as a 0/1, HasDesignation's arm in every respect --
  -- rule 702.33d designating the spell for ANY of its kicker costs, so this asks
  -- the whole map. The object it reads is the RESOLVING SPELL, which is still on
  -- the stack while its own clause conditions are gated
  -- (Pawl.Engine.Resolve.gateHolds).
  Quantity.WasKicked -> fmap (\view -> if any (> 0) (Filter.kicked view) then 1 else 0) mView
  -- CR 702.33f's "kicked with its [A] kicker" and CR 702.33c's count, which are
  -- one read: how many times THIS cost was declared, zero for a cost the spell's
  -- controller declined and for one the card does not print.
  Quantity.TimesKickedWith cost -> fmap (toInteger . Map.findWithDefault 0 cost . Filter.kicked) mView
  -- CR 107.4h's third sentence as a 0/1, WasKicked's arm in every respect --
  -- including the object it reads, which for Berg Strider is the PERMANENT the
  -- spell became (CR 400.7d) and for Forsworn Paladin is the CR 602.2a ability
  -- object an AgainstSlot aimed it at.
  --
  -- A CLASSIFICATION of the mana and never an effect's identity: what the view
  -- reports is Pawl.Types.ProductionTag, the closed half of what a unit carries,
  -- and this arm asks it for one member.
  Quantity.TagWasSpent tag -> fmap (\view -> if Set.member tag (Filter.manaSpentTags view) then 1 else 0) mView
  -- CR 111.6's status as a 0/1, WasKicked's arm in every respect. Filter.token
  -- rather than Game.isToken: reading the object directly answers False for an
  -- id naming nothing, which is the whole case this arm exists for; see #1102.
  Quantity.WasToken -> fmap (\view -> if Filter.token view then 1 else 0) mView
  -- CR 509.1g's combat fact as a 0/1, WasToken's arm in every respect --
  -- including the reader, since a creature that has died is out of
  -- GameState.combat as well as out of GameState.objects; see #991, whose
  -- LastKnown blocking half is what makes this arm answer at all.
  Quantity.WasBlocking -> fmap (\view -> if Filter.blocking view then 1 else 0) mView
  -- CR 120.1's damage as a total, read off the event log for the object the
  -- quantity is aimed at (Game.damageDealtToThisTurn) rather than off its view:
  -- CR 608.2i is what makes the question answerable at all for a creature CR
  -- 400.7 has deleted, and the log survives the death where the object does not.
  --
  -- "This turn" and not "as it died", which is the design call this arm settles:
  -- the printed clause says this turn, and CR 120.6's regeneration and CR 120.3d's
  -- wither and infect are three boards where the damage that was dealt is no
  -- longer marked on the creature that took it.
  Quantity.DamageDealtToThisTurn -> fmap (toInteger . Game.damageDealtToThisTurn gs) mOid
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
  -- Nor Combat.declaredAttackedThisStep, its step-scoped twin: melee's words are
  -- "this combat", which CR 511.3's span matches and CR 500.1's does not. The two
  -- coincide for every melee trigger printed -- CR 508.1m puts the trigger on the
  -- stack in the step the declaration happened in -- so the fields are apart
  -- because the two rules ask different questions, not because a card tells them
  -- apart today.
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
    Just [pid] -> Just (toInteger (length (filter (attackedOpponent (Game.teams gs) pid) (Set.toList (Combat.declaredAttacked (GameState.combat gs))))))
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
    Just [pid] -> Just (toInteger (length (filter ((== Just pid) . Game.discardOf . LoggedEvent.event) (Foldable.toList (GameState.events gs)))))
    _ -> Nothing
  -- CR 119.3 / 608.2i: how much life that player has gained this turn.
  -- CardsDiscardedThisTurn's arm in footing -- a live fold over GameState.events,
  -- whose extent Engine.beginTurnOf's clearing makes "this turn" -- and in ARITY:
  -- Nothing for a reference naming anything but exactly one player, since "whose
  -- life?" has no sum.
  --
  -- The AMOUNTS are summed where the discard tally counts events, the printed
  -- sentence asking how much life rather than how many gains.
  -- Game.lifeGainedThisTurn is the fold, so a second reader of the same log cannot
  -- drift from this one.
  --
  -- An EMPTY log answers 0 rather than Nothing, as CardsDiscardedThisTurn's does.
  -- What is unanswered is only the reference.
  Quantity.LifeGainedThisTurn ref -> case playersOf ref of
    Just [pid] -> Just (toInteger (Game.lifeGainedThisTurn gs pid))
    _ -> Nothing
  -- CR 120.1 / 608.2i: how many of the players this reference names were dealt
  -- damage this turn. CardsDiscardedThisTurn's arm in footing -- a live fold over
  -- GameState.events, whose extent Engine.beginTurnOf makes "this turn" -- and
  -- IsMonarch's in arity: the question is asked of each named player separately, so
  -- a reference naming several is answered by counting them rather than by asking
  -- "whose?". Furious Spinesplitter's "for each opponent who" is that count, and
  -- rule 702.54a's bloodthirst is the same count compared against 1, which
  -- Pawl.Engine.Replacement.admitsEntry's Bloodthirst arm now asks.
  --
  -- The PLAYERS are counted and not the events, which is why this filters the
  -- player list rather than the log: two bolts at one opponent is one opponent.
  --
  -- NO liveness test on the players counted, OpponentsAttacked's posture and for
  -- its reason: the record is what the rule asks about, so an opponent who has
  -- since left the game (CR 800.4) still answers -- though playersFor's Opponent
  -- arm will already have dropped them from `pids`, so this only matters for a
  -- reference that names a player outright.
  --
  -- An EMPTY log answers 0 rather than Nothing, as CardsDiscardedThisTurn's does.
  -- What is unanswered is only the reference.
  Quantity.PlayersDealtDamageThisTurn ref -> case playersOf ref of
    Nothing -> Nothing
    Just pids -> Just (toInteger (length (filter wasDealtDamage pids)))
  -- CR 120.1 / 608.2i: how much damage in total the players this reference names
  -- were dealt this turn -- rule 702.54b's "the total damage your opponents have
  -- been dealt this turn", which Pawl.Engine.Event's Bloodthirst arm asks with
  -- PlayerRelation.Opponent.
  --
  -- SUMS the seats where the arm above counts them, which is the whole difference
  -- between rule 702.54a's threshold and rule 702.54b's X: three damage to one
  -- opponent and two to another is two opponents there and five here.
  --
  -- Same log, same CR 120.3a recipient and the same empty-log answer of 0; only
  -- the reference is ever unanswered.
  Quantity.DamageDealtToPlayersThisTurn ref -> case playersOf ref of
    Nothing -> Nothing
    Just pids -> Just (toInteger (sum (fmap (Game.damageDealtToPlayerThisTurn gs) pids)))
  -- CR 601.2i / 608.2i: how many spells that player cast during the turn just
  -- ended. LifeTotal's arm in ARITY -- one player's tally, so a reference naming
  -- several answers "whose?" rather than a sum -- and read STRAIGHT OFF the
  -- handoff snapshot for the reason the life total is read straight off
  -- GameState.players: the log it was folded from is cleared by that same handoff
  -- (Engine.beginTurnOf), so there is nothing live left to fold.
  --
  -- Both printed readings are about every player at once and both reach this arm
  -- through Aggregation.Greatest over Scope.OverPlayers, whose candidate arrives
  -- as PlayerRef.Candidate -- "no spells were cast last turn" is that maximum
  -- compared to 0 and "a player cast two or more spells last turn" is the same
  -- maximum compared to 2. Summing the seats would answer the second wrongly when
  -- two players cast one spell each.
  --
  -- An ABSENT entry answers 0 rather than Nothing, as CardsDiscardedThisTurn's
  -- empty log does: nobody having cast is an answered question. What is
  -- unanswered is only the reference.
  Quantity.SpellsCastLastTurn ref -> case playersOf ref of
    Just [pid] -> Just (toInteger (Map.findWithDefault 0 pid (GameState.castsLastTurn gs)))
    _ -> Nothing
  -- CR 309.7: how many dungeons that player has completed. LifeTotal's arm in
  -- ARITY -- one player's tally, so a reference naming several answers "whose?"
  -- rather than a sum -- and in SOURCE: read straight off the player, because
  -- Dungeon.remove writes it there and the log GameEvent.DungeonCompleted goes
  -- into is cleared at every turn handoff.
  --
  -- LIVE, which is what CR 604.2 needs: Gloom Stalker's "as long as you've
  -- completed a dungeon" is re-asked by Projection.conditionHolds on every
  -- projection, so the double strike appears in the same settle that CR 704.5t
  -- removed the dungeon in.
  --
  -- An ABSENT player answers 0 rather than Nothing, as SpellsCastLastTurn's absent
  -- entry does: having completed none is an answered question. What is unanswered
  -- is only the reference.
  Quantity.DungeonsCompleted ref -> case playersOf ref of
    Just [pid] -> Just (toInteger (maybe 0 Player.completedDungeons (Map.lookup pid (GameState.players gs))))
    _ -> Nothing
  -- CR 309.7 asked of ONE dungeon, the arm above's shape in arity, source and
  -- liveness: 1 if that player's completed names hold this one and 0 if not.
  --
  -- A 0\/1 rather than a Bool because Quantity is a number; the threshold that
  -- turns it into Acererak the Archlich's "if you haven't" is the Comparison's.
  Quantity.CompletedDungeon (CompletedDungeon.MkCompletedDungeon ref name) -> case playersOf ref of
    Just [pid] ->
      let completed = maybe Set.empty Player.completedDungeonNames (Map.lookup pid (GameState.players gs))
       in Just (if Set.member name completed then 1 else 0)
    _ -> Nothing
  -- CR 400.7 / 608.2i read as a 0/1: did the object this evaluation is aimed at
  -- enter the battlefield this turn?
  --
  -- BlockersBeyondFirst's arm in shape -- read LIVE off game state rather than
  -- through the injected view, an entry being an event and not a characteristic --
  -- and CardsDiscardedThisTurn's in extent: Engine.beginTurnOf clears the log at
  -- the turn handoff, so "this turn" is the log's own reach and nothing here names
  -- a window. LIVE matters for CR 604.2 -- Projection.conditionHolds re-asks this
  -- every time the projection is taken, so the hexproof goes away at the handoff
  -- rather than at whatever moment a snapshot had been captured.
  --
  -- Game.enteredBattlefield and not a Scope.InHistory count: that arm matches a
  -- Filter against the event's characteristic snapshot, where this asks whether one
  -- particular id is the entrant.
  --
  -- An object that entered TWICE this turn (blinked and returned) is still 1 rather
  -- than a tally: CR 400.7 makes the second arrival a new object with a new id, so
  -- only the current incarnation's own entry can match.
  --
  -- Nothing only for an evaluation aimed at no object -- a member of an
  -- Aggregation.Greatest over Scope.InHistory. A permanent that did not enter this
  -- turn is 0, which is a number and not a failure.
  Quantity.EnteredThisTurn ->
    fmap
      (\oid -> if any ((== Just oid) . Game.enteredBattlefield . LoggedEvent.event) (GameState.events gs) then 1 else 0)
      mOid
  -- CR 400.7 / 400.3 read as a 0/1: did the object this evaluation is aimed at
  -- enter the battlefield out of the named player's copy of the named zone?
  --
  -- EnteredThisTurn's arm with the origin zone tested as well, so its whole
  -- haddock carries over -- the live read off GameState.events, the log's own
  -- extent standing in for "this turn", and the keying on ZoneChange.object.
  --
  -- WHOSE copy is the entrant's OWNER: CR 400.3 sends a card to its owner's copy
  -- of a per-player zone, so the graveyard it left was its owner's. That is read
  -- through the INJECTED VIEW rather than off the board: CR 603.4 re-checks an
  -- intervening "if" on resolution, by which time the entrant may be gone, and
  -- Event.interveningHolds and Stack's re-check both inject
  -- Projection.viewWithLastKnownAnywhere so CR 608.2h still answers. A view that
  -- cannot describe the object at all is Nothing, which Condition.holds collapses
  -- to False. That view has to answer the OWNER as well as the characteristics,
  -- which is what LastKnown.owner is for.
  --
  -- CR 608.2i is why the log read is the rule and not a convenience: an entry is a
  -- completed action, and a check needing information about one finds its object
  -- wherever it now is so long as it takes no action on it -- which this clause,
  -- whose effect acts elsewhere, does not. So the second check can never answer
  -- differently from the first, and what a board can observe is that it reads the
  -- LOG. Breathless Knight proves it in Pawl.ConditionSpec: kill the entrant with
  -- the trigger on the stack and the +1/+1 counter still lands.
  --
  -- The clause is a printed FAMILY, not a card or two: Scryfall
  -- o:/(was|were) cast from|you cast it from/ o:entered, unique=cards,
  -- 2026-08-31, nine printings, of which eight state it as an intervening "if"
  -- (Fblthp, the Lost is the ninth, whose "if" opens a second sentence and so is
  -- ordinary English by CR 603.4's own parenthetical; nothing prints the older
  -- "entered the battlefield from" wording). The narrower o:"entered from"
  -- returns eight and misses Twilight Diviner, whose clause reads "entered or
  -- were cast from a graveyard".
  --
  -- Archfiend's Vessel is the one member whose clause and whose effect name the
  -- SAME object, which is exactly why it cannot observe this: a Vessel that left
  -- the battlefield fails CR 603.6's find and makes no Demon whichever way the
  -- re-check answered. Every other member reads the ENTRANT and acts elsewhere --
  -- Grist, Voracious Larva transforms Grist, Kotis, Sibsig Champion and
  -- Breathless Knight put counters on themselves, Celes, Rune Knight puts one on
  -- each creature you control, Extraordinary Journey draws you a card, Twilight
  -- Diviner copies one of the entrants, Prized Amalgam returns its own card -- so
  -- killing the entrant between the two checks tells a log read from a live-board
  -- one.
  Quantity.EnteredFrom inZone -> do
    oid <- mOid
    pids <- playersOf (InZone.player inZone)
    owner <- Filter.owner =<< viewOf oid
    let entered = any (\zc -> ZoneChange.from zc == InZone.zone inZone) (entriesOf oid)
    pure (if elem owner pids && entered then 1 else 0)
  -- CR 601.2a / 400.3 read as a 0/1: was the object this evaluation is aimed at
  -- cast by the named player out of that player's copy of the named zone?
  --
  -- Two hops rather than one, because CR 400.7 puts a whole object between the
  -- cast and the entry: the spell the card became is ZoneChange.departed of the
  -- stack-to-battlefield entry, and GameEvent.SpellCast files the zone under that
  -- id (Pawl.Types.SpellWasCast.zone). Nothing else records it -- the permanent
  -- has no memory of the spell's origin.
  --
  -- ONE reference answering THREE questions the rules distinguish: whose copy of
  -- the zone, who cast the spell, and who owns the card. The owner half is
  -- EnteredFrom's, for CR 400.3's reason, and is read the same way for CR 603.4's;
  -- the caster half is CR 601.2a's.
  --
  -- What makes the one reference exact is the POOL rather than anything about
  -- Magic. CR 400.3 puts a card only in its OWNER's library, hand or graveyard, so
  -- the zone's owner and the card's owner are always one seat; CR 400.1's shared
  -- zones can only take PlayerRef.EachPlayer (Pawl.Codec.InZone.undividedShared),
  -- where all three conjuncts are vacuous. Every card that READS this reference
  -- reads a graveyard or exile, and every CR 601.3 permission over a graveyard in
  -- data/cards/ names PlayerRef.Relative You, so wherever a spell those cards can
  -- see is cast, the caster is the owner too. Breathless Knight's "you cast it
  -- from A graveyard" is therefore PlayerRef.Relative You here -- an EachPlayer
  -- reference would read "anyone cast it", which is weaker -- and Fblthp, the
  -- Lost's agentless "was cast from your library" is that same seat named from the
  -- other side.
  --
  -- Not implemented: the caster and the zone's owner really can differ now, so the
  -- conjuncts are a regression fence rather than a proved trio -- mutating either
  -- away leaves the suite green. TWO roads reach it. Sen Triplets grants a cast
  -- out of an opponent's HAND (Pawl.Types.CastFromZone), and no card in
  -- data/cards/ reads a hand here; and CR 608.2g's offered cast asks no zone at
  -- all (Cast.castableWhenOffered), so an effect naming a card in an opponent's
  -- graveyard would cast it from there. The reference splits in three the day
  -- either gets a reader (#2689). Jetsam (Flotsam // Jetsam) is the printing on
  -- the second road; Havengul Lich would reach the first through the CR 601.3
  -- permission road, which still refuses at the one-object permission (#2795).
  --
  -- An object that reached the battlefield any OTHER way answers 0 rather than
  -- Nothing, `spells` coming up empty: a permanent put there by an effect was not
  -- cast at all, which is an answered question and the disjunct's other half
  -- (EnteredFrom) is what covers it.
  Quantity.WasCastFrom inZone -> do
    oid <- mOid
    pids <- playersOf (InZone.player inZone)
    owner <- Filter.owner =<< viewOf oid
    let spells = [ZoneChange.departed zc | zc <- entriesOf oid, ZoneChange.from zc == Zone.Stack]
        castFromZone cast =
          elem (SpellWasCast.spell cast) spells
            && SpellWasCast.zone cast == Just (InZone.zone inZone)
            && elem (SpellWasCast.player cast) pids
        wasCast = any (maybe False castFromZone . Game.castOf . LoggedEvent.event) (GameState.events gs)
    pure (if elem owner pids && wasCast then 1 else 0)
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
  -- CR 702.184c: Power's arm, with the substitution asked first. `perspective`
  -- is CR 109.5's "you" of the ability being resolved -- its controller (CR
  -- 113.8), unmoved by AgainstSlot's re-aim, which only ever repoints
  -- `mOid`/`mView`, and unmoved by whoever controls the stationing permanent
  -- NOW: Tapestry Warden's ruling gates on the station ability's controller
  -- controlling the Warden as it resolves. grantsStationToughnessFor walks the
  -- battlefield through the same `viewOf` every other arm reads, so a snapshot
  -- caller (CR 608.2h) answers False rather than reaching for a live board it was
  -- never handed.
  Quantity.StationMeasure ->
    let substitutes = maybe False (grantsStationToughnessFor viewOf gs) (Filter.perspective context)
        greater = case (mView >>= Filter.toughness, mView >>= Filter.power) of
          (Just t, Just p) -> t > p
          _ -> False
     in if substitutes && greater then mView >>= Filter.toughness else mView >>= Filter.power
  where
    recur = evaluateAgainst viewOf context gs announcedOn mOid mView
    -- Was this player dealt damage this turn? Game.wasDealtDamageThisTurn is the
    -- shared fold, so this count and the Filter.DealtDamageThisTurn atom a player
    -- candidate answers cannot come apart; see Game.damagedPlayer under it for CR
    -- 120.3a's recipient and for CR 120.8's zero.
    wasDealtDamage = Game.wasDealtDamageThisTurn gs
    -- CR 102.1's reference, resolved by Count.playersFor for every arm but the
    -- fold's own candidate. That one is answered HERE because this is where the
    -- candidate is: Count.evaluate's Scope.OverPlayers arm hands each candidate
    -- to this reader as a Pawl.Engine.Count.playerView, whose identity IS the
    -- player, and no id names it for that function's ViewOf to be asked with.
    --
    -- Nothing wherever the evaluation is not aimed at a player -- an object
    -- candidate's view carries no identity, and an evaluation outside a fold
    -- carries no candidate at all -- which is Pawl.Types.PlayerRef.Candidate's
    -- own stated answer there.
    playersOf ref = case ref of
      PlayerRef.Candidate -> fmap pure (mView >>= Filter.playerIdentity)
      -- CR 608.2h reaches Count.playersFor's arm through the view passed here --
      -- Spikeshell Harrier reads the speed of the player who controlled the
      -- permanent its own earlier clause has already bounced, and a last-known
      -- aware view is what still names them.
      PlayerRef.ControllerOfBound _ -> Count.playersFor viewOf context gs ref
      PlayerRef.EachPlayer -> Count.playersFor viewOf context gs ref
      PlayerRef.EachPlayerExcept _ -> Count.playersFor viewOf context gs ref
      PlayerRef.Relative _ -> Count.playersFor viewOf context gs ref
      PlayerRef.InSlot _ -> Count.playersFor viewOf context gs ref
      -- InSlot's plural, answered there too: off the resolution's own slots, or
      -- the source's bindings where the position supplies none.
      PlayerRef.EachInSlot _ -> Count.playersFor viewOf context gs ref
      PlayerRef.Specific _ -> Count.playersFor viewOf context gs ref
      -- CR 508.6's set, folded there off the live combat record. The scalar arms
      -- above still decline it, and not for want of an answer: each takes
      -- `Just [pid]` and no more (see the LifeTotal arm), so a reference naming a
      -- table's worth of players leaves them unanswered. Where it DOES read is a
      -- scope's fold -- Synthetic Toll of the Siege's "for each player attacking
      -- them" (Pawl.CountSpec).
      PlayerRef.Attacking _ -> Count.playersFor viewOf context gs ref

    -- Every entry onto the battlefield this log records for one id. A list and not
    -- a Maybe: CR 400.7 makes each arrival a new object, so at most one entry can
    -- name a given id, and folding over the log is what says so rather than
    -- assuming it.
    entriesOf oid =
      [ zc
      | ev <- Foldable.toList (GameState.events gs),
        Just zc <- [Game.enteredBattlefieldChange (LoggedEvent.event ev)],
        ZoneChange.object zc == oid
      ]

-- CR 702.184c: does `you` control ANY permanent that grants the toughness
-- substitution -- Modification.GrantsStationToughness's read, folded to the
-- per-player question Quantity.StationMeasure asks. False for a caller whose
-- `viewOf` cannot see the battlefield (a snapshot's own rebuild): rule 702.184c
-- asks about a LIVE grant, and that caller has no live board to walk.
grantsStationToughnessFor :: Count.ViewOf -> GameState -> PlayerId.PlayerId -> Bool
grantsStationToughnessFor viewOf gs you =
  any
    (maybe False (\v -> Filter.controller v == Just you && Filter.grantsStationToughness v) . viewOf)
    (Set.toList (GameState.battlefield gs))

-- Is this declared attack an attack on one of that player's OPPONENTS? CR 506.3
-- gives three things a creature can attack and rule 702.121a counts only the
-- first: attacking an opponent's planeswalker or a battle they protect is not
-- attacking that opponent, which melee's own ruling states outright.
--
-- CR 102.3's opponents, the reading Pawl.Types.PlayerScope.Opponents and
-- Count.playersFor already share: every player not on that player's team, which
-- at two seats (CR 102.2) and in a free-for-all (CR 806.1) is every other player.
attackedOpponent :: Teams.Teams -> PlayerId.PlayerId -> AttackTarget.AttackTarget -> Bool
attackedOpponent teams pid target = case target of
  AttackTarget.OfPlayer other -> Teams.areOpponents teams pid other
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
-- SCOPED TO THE CHARACTERISTIC-DEFINING ABILITY, as CR 208.2a is: the caller is
-- Projection.applyCharacteristicPT, which layer 7a runs for an object in any
-- zone (CR 604.3 makes the ability function in all zones), and every other reader
-- of a quantity must keep evaluate's honest Nothing, since no rule tells those to
-- invent a number.
--
-- It does NOT descend into a Count, and does not need to: an undeterminable
-- count IS the number CR 208.2a is talking about, so the 0 goes in whole here
-- and Pawl.Engine.Count.aggregate stays free to answer Nothing for non-CDA
-- readers.
determine :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> Quantity -> Integer
determine viewOf context gs oid = determineWith (evaluate viewOf context gs oid)

-- determine with the evaluator INJECTED, which is the whole of what determine
-- does with its four board arguments. The second caller is
-- Pawl.Engine.Resolve.Effect.bakeTokenCharacteristics, which has to evaluate a created
-- token's box against the CREATING object (Quantity.evaluateFor's two ids) and so
-- cannot reach the one-id evaluate above.
determineWith :: (Quantity -> Maybe Integer) -> Quantity -> Integer
determineWith eval quantity = case quantity of
  Quantity.Plus (Plus.MkPlus a b) -> determineWith eval a + determineWith eval b
  Quantity.Halved (Halved.MkHalved rounding inner) -> halve rounding (determineWith eval inner)
  Quantity.Negate a -> negate (determineWith eval a)
  _ -> Maybe.fromMaybe 0 (eval quantity)

-- CR 107.3m: substitute the value of X that was announced for the SPELL that
-- became this permanent into a quantity an enters-the-battlefield replacement
-- effect reads -- "if an object's enters-the-battlefield triggered ability or
-- replacement effect refers to X, and the spell that became that object as it
-- resolved had a value of X chosen for any of its costs, the value of X for that
-- ability is the same as the value of X for that spell". Protean Hydra's "this
-- creature enters with X +1/+1 counters on it" is the pool's reader.
--
-- A SUBSTITUTION at the entry site rather than a fallback inside
-- evaluateAgainst's InSlot arm, because rule 107.3m states the exception and its
-- limit in one sentence: "although the value of X for that permanent is 0". A
-- fallback would answer the announcement everywhere the permanent is asked --
-- for an activated ability of its own, where that same clause says 0.
--
-- Cost.substituteX's twin, one type over. The descent is Star.substituteStar's, for
-- the same reason: a composed quantity (1 + X) is still a reader of X. No
-- descent into Count -- its per-member quantity is read against ANOTHER object,
-- so an X inside it would be that object's and not this entry's -- nor into
-- AgainstSlot, which re-aims the fold the same way.
substituteAnnouncedX :: Natural -> Quantity -> Quantity
substituteAnnouncedX n quantity = case quantity of
  Quantity.InSlot slot | slot == Binding.variableX -> Quantity.Literal (toInteger n)
  Quantity.Plus (Plus.MkPlus a b) -> Quantity.Plus (Plus.MkPlus (substituteAnnouncedX n a) (substituteAnnouncedX n b))
  Quantity.Halved (Halved.MkHalved rounding inner) -> Quantity.Halved (Halved.MkHalved rounding (substituteAnnouncedX n inner))
  Quantity.Negate a -> Quantity.Negate (substituteAnnouncedX n a)
  _ -> quantity

-- QuantitySlot.slots split by WHICH HALF of the binding the read is of: the
-- subset it reports that names an OBJECT. Exactly the AgainstSlot arms, which aim an inner
-- number at the object a slot names and reach Filter.slotOneObject to find it --
-- and that declines a slot naming several rather than picking one of them
-- (Binding.onlyOne's doctrine), so such a read is damaged by a plural slot.
--
-- The difference from QuantitySlot.slots is the InSlot arm, which reads the
-- slot's AMOUNT (Binding.amountOf, see evaluateAgainst above) and reaches
-- slotOneObject on no
-- road at all. Pawl.Types.SlotName is one flat namespace, so a card MAY name an
-- amount slot what a plural target slot is named; classifying that read as a
-- singular OBJECT read is what would reject such a card for a reason that is not
-- about it; see #2774. Resolve.quantitySlots is where the two halves are rejoined,
-- as presence for the D4 dataflow lint and arity for the count lint.
--
-- Exhaustive with no fallthrough, slotsAreExhaustive's shape and for its reason:
-- a new arm naming a slot must answer here rather than inherit that function's
-- answer.
objectSlots :: Quantity -> Set SlotName
objectSlots quantity = case quantity of
  -- The amount reader, left out for the reason above.
  Quantity.InSlot _ -> Set.empty
  Quantity.Literal _ -> Set.empty
  Quantity.ManaValue -> Set.empty
  Quantity.Power -> Set.empty
  Quantity.Toughness -> Set.empty
  Quantity.Star -> Set.empty
  -- DESCENT: a composite's payload is card text like any other, and
  -- QuantitySlot.slots descends into each of these three the same way.
  Quantity.Plus (Plus.MkPlus a b) -> Set.union (objectSlots a) (objectSlots b)
  Quantity.Halved (Halved.MkHalved _ inner) -> objectSlots inner
  Quantity.Negate a -> objectSlots a
  -- DESCENT into a Greatest's per-member number, which may aim at a slot of its
  -- own; the other two aggregations carry no number to ask.
  Quantity.Count c -> QuantitySlot.foldCount objectSlots c
  -- Every remaining arm names no slot at all, QuantitySlot.slots saying the same
  -- of each:
  -- the references they carry are PlayerRefs, which name a target slot neither
  -- walk reports and Resolve.slotsOf cannot recover from here (#1079).
  Quantity.ManaCount _ -> Set.empty
  Quantity.LifeTotal _ -> Set.empty
  Quantity.Speed _ -> Set.empty
  Quantity.IsMonarch _ -> Set.empty
  Quantity.IsStartingPlayer _ -> Set.empty
  Quantity.IsActivePlayer _ -> Set.empty
  Quantity.PlayerCounters {} -> Set.empty
  Quantity.ObjectCounters _ -> Set.empty
  Quantity.ObjectCountersOfAnyKind -> Set.empty
  Quantity.HasDesignation _ -> Set.empty
  Quantity.ClassLevel -> Set.empty
  Quantity.WasKicked -> Set.empty
  -- CR 702.33f's read, WasKicked's arm above in every respect: the Cost it
  -- carries is the IDENTIFIER of one kicker ability, matched against the spell's
  -- own record by equality, never an instruction this traversal descends into.
  Quantity.TimesKickedWith _ -> Set.empty
  Quantity.TagWasSpent {} -> Set.empty
  Quantity.WasToken -> Set.empty
  Quantity.WasBlocking -> Set.empty
  Quantity.DamageDealtToThisTurn -> Set.empty
  Quantity.OpponentsAttacked _ -> Set.empty
  Quantity.CardsDiscardedThisTurn _ -> Set.empty
  Quantity.LifeGainedThisTurn _ -> Set.empty
  Quantity.PlayersDealtDamageThisTurn _ -> Set.empty
  Quantity.DamageDealtToPlayersThisTurn _ -> Set.empty
  Quantity.SpellsCastLastTurn _ -> Set.empty
  Quantity.DungeonsCompleted _ -> Set.empty
  Quantity.CompletedDungeon {} -> Set.empty
  Quantity.EnteredThisTurn -> Set.empty
  Quantity.EnteredFrom _ -> Set.empty
  Quantity.WasCastFrom _ -> Set.empty
  Quantity.BlockersBeyondFirst -> Set.empty
  -- Power's answer: it names no object at all and takes the one the evaluation
  -- is aimed at, so there is no slot here for AgainstSlot to have named.
  Quantity.StationMeasure -> Set.empty
  -- The one arm with an answer, and DESCENT beside it: the payload is evaluated
  -- against the named object and may aim at a further slot of its own.
  Quantity.AgainstSlot (AgainstSlot.MkAgainstSlot slot inner) -> Set.insert slot (objectSlots inner)

-- CR 603.3b: is QuantitySlot.slots the WHOLE of what evaluating this quantity
-- reads off the resolving object's bindings? It is not wherever
-- QuantitySlot.nestedRefs finds a read that names a slot -- a nested PlayerRef that reads one, or a scope that
-- names one outright. Resolve.slotsAreExhaustive is the sole caller and carries
-- the whole account.
slotsAreExhaustive :: Quantity -> Bool
slotsAreExhaustive = all (either playerRefIsSlotless (const False)) . QuantitySlot.nestedRefs

-- Only InSlot names a slot; the other three are answered from the evaluation
-- context alone (Resolve.playerRefSlots says the same thing as a set).
playerRefIsSlotless :: PlayerRef.PlayerRef -> Bool
playerRefIsSlotless ref = case ref of
  PlayerRef.EachPlayer -> True
  -- The exclusion names a slot, so this reads one -- InSlot's answer, even though
  -- the slot decides who is left OUT rather than who is in.
  PlayerRef.EachPlayerExcept _ -> False
  PlayerRef.Relative _ -> True
  PlayerRef.InSlot _ -> False
  -- InSlot's answer: this reads a slot too, and every one of them.
  PlayerRef.EachInSlot _ -> False
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
  -- InSlot's answer again: the player attacked is named by a slot.
  PlayerRef.Attacking _ -> False

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
-- Exhaustive, QuantitySlot.slots' posture: a new arm carrying a PlayerRef must
-- fail to compile here rather than silently keep an unbaked one -- which is what
-- Pawl.Engine.QuantitySlot.mapPlayerRefs is, and this is one instance of it.
bakeBound :: Map.Map SlotName PlayerId.PlayerId -> Quantity -> Quantity
bakeBound players =
  QuantitySlot.mapPlayerRefs
    (bakePlayerRef players)
    -- Both halves: the Scope says whose zone or which players, and an
    -- Aggregation.Greatest's per-member quantity may hide a reference of its own.
    -- Terminating for evaluate's reason -- a Greatest's payload is a strictly
    -- smaller subterm.
    (\c -> (QuantitySlot.mapCount (bakeBound players) c) {Count.Type.scope = QuantitySlot.mapScope (bakePlayerRef players) (Count.Type.scope c)})

-- The player a per-player instruction is CURRENTLY applying to, substituted for
-- Pawl.Types.PlayerRef.Candidate -- Shahrazad's "each player who doesn't win the
-- subgame loses half THEIR life", where the number is read against each payer's
-- own life total as the effect resolves rather than computed once and shared.
--
-- Candidate is the reference a card writes for "whichever player this reading is
-- aimed at", and Pawl.Engine.Count answers it inside a Scope.OverPlayers fold off
-- the candidate's view. An effect applying to a set of players is the same
-- question with no view to hand, so it is answered by substitution instead --
-- which is exactly bakeBound's move, one reference over.
--
-- A nested Count's SCOPE is substituted and its PER-MEMBER QUANTITY is not, which
-- is the point of the parameter. The two halves ask different questions:
--
--   * a scope names whose copy of a per-player zone the fold reads (CR 400.1) --
--     Nature's Resurgence's "each creature card in THEIR graveyard" is that
--     reference, and Count.playersFor answers a bare candidate with Nothing;
--   * a per-member quantity is read against the member the fold has reached, so
--     substituting this candidate inside would make Malignus' "the highest life
--     total among your opponents" read the recipient's life in every member.
--
-- Control is the question this reference cannot ask: the battlefield is shared
-- (CR 400.1) and Game.zoneMembers slices it by OWNER (see #161), so "each creature
-- THEY CONTROL" is Filter.ControlledByRecipient off Filter.Context's recipient
-- instead. Pawl.Engine.Resolve.Effect.evaluateForRecipient supplies both.
forCandidate :: PlayerId.PlayerId -> Quantity -> Quantity
forCandidate pid = QuantitySlot.mapPlayerRefs substitute (\c -> c {Count.Type.scope = QuantitySlot.mapScope substitute (Count.Type.scope c)})
  where
    substitute ref = case ref of
      PlayerRef.Candidate -> PlayerRef.Specific pid
      PlayerRef.EachPlayer -> ref
      PlayerRef.EachPlayerExcept _ -> ref
      PlayerRef.Relative _ -> ref
      PlayerRef.InSlot _ -> ref
      PlayerRef.EachInSlot _ -> ref
      PlayerRef.Specific _ -> ref
      PlayerRef.ControllerOfBound _ -> ref
      PlayerRef.Attacking _ -> ref

-- One reference, baked. The whole of the substitution: every arm above funnels
-- through this, so what a slot means is stated once.
bakePlayerRef :: Map.Map SlotName PlayerId.PlayerId -> PlayerRef.PlayerRef -> PlayerRef.PlayerRef
bakePlayerRef players ref = case ref of
  PlayerRef.InSlot slot -> maybe ref PlayerRef.Specific (Map.lookup slot players)
  -- LEFT STANDING, EachPlayerExcept's posture below and for its reason: this
  -- names a SET and PlayerRef.Specific names one seat, so there is nothing to
  -- bake to. Every scalar this function traverses reads exactly one player, so
  -- the reference answers Nothing baked or not.
  PlayerRef.EachInSlot _ -> ref
  PlayerRef.EachPlayer -> ref
  -- LEFT STANDING, ControllerOfBound's posture below, and here there is nothing
  -- to bake TO: PlayerRef.Specific names one seat and this names the rest of the
  -- table. It costs nothing either way, since every scalar this function
  -- traverses reads exactly one player (see the LifeTotal arm above) and so
  -- answers Nothing for this reference baked or not.
  PlayerRef.EachPlayerExcept _ -> ref
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
  -- collapse; no card in the pool stores one (#3058).
  PlayerRef.ControllerOfBound _ -> ref
  -- LEFT STANDING for ControllerOfBound's reason, plus one of its own: the slot
  -- this names holds a PLAYER, but what the reference reads is the live combat
  -- record, which no baking can fix in place.
  PlayerRef.Attacking _ -> ref

-- Does this quantity read CR 601.2b's announced X? Since #14 retired X's
-- dedicated constructor, that read is a Quantity.InSlot naming
-- Binding.variableX, and it can sit anywhere inside a quantity rather than only
-- at its root -- so answering needs the same recursion QuantitySlot.slots has,
-- and an equality test against a bare X does not answer it at all.
--
-- Resolve.readsX is the one caller: it asks "does this card read X?" for the lint
-- that pairs a read against the cost's {X} (CR 107.3, CR 107.3a, CR 118.4).
-- Vitalizing Cascade's "X plus 3" is the card that distinguishes the two
-- answers.
--
-- Written out arm by arm rather than as a filter over QuantitySlot.slots, for
-- that function's own reason: a new Quantity constructor must make its author
-- answer "does this read X?" explicitly, rather than inherit whatever the other
-- function decided.
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
  -- Terminating for the reason QuantitySlot.overSlots' Count arm is: a Greatest's
  -- payload is a
  -- strictly smaller subterm.
  Quantity.Count c -> QuantitySlot.anyCount readsX c
  -- Every remaining arm is a LEAF holding no Quantity, so none can hide an X.
  -- The seven references below (ManaCount's, LifeTotal's, Speed's, IsMonarch's,
  -- IsStartingPlayer's, IsActivePlayer's, PlayerCounters') are PlayerRefs, whose InSlot names a
  -- TARGET slot rather than an amount one, and X is only ever an amount.
  Quantity.Literal _ -> False
  Quantity.ManaValue -> False
  Quantity.Power -> False
  Quantity.Toughness -> False
  Quantity.Star -> False
  Quantity.ManaCount _ -> False
  Quantity.LifeTotal _ -> False
  Quantity.Speed _ -> False
  Quantity.IsMonarch _ -> False
  Quantity.IsStartingPlayer _ -> False
  Quantity.IsActivePlayer _ -> False
  Quantity.PlayerCounters {} -> False
  Quantity.ObjectCounters _ -> False
  Quantity.ObjectCountersOfAnyKind -> False
  Quantity.HasDesignation _ -> False
  Quantity.ClassLevel -> False
  Quantity.WasKicked -> False
  -- CR 702.33f's read, WasKicked's arm above in every respect: the Cost it
  -- carries is the IDENTIFIER of one kicker ability, matched against the spell's
  -- own record by equality, never an instruction this traversal descends into.
  Quantity.TimesKickedWith _ -> False
  Quantity.TagWasSpent {} -> False
  Quantity.WasToken -> False
  Quantity.WasBlocking -> False
  Quantity.DamageDealtToThisTurn -> False
  Quantity.OpponentsAttacked _ -> False
  Quantity.CardsDiscardedThisTurn _ -> False
  Quantity.LifeGainedThisTurn _ -> False
  Quantity.PlayersDealtDamageThisTurn _ -> False
  Quantity.DamageDealtToPlayersThisTurn _ -> False
  Quantity.SpellsCastLastTurn _ -> False
  Quantity.DungeonsCompleted _ -> False
  Quantity.CompletedDungeon {} -> False
  Quantity.EnteredThisTurn -> False
  Quantity.EnteredFrom _ -> False
  Quantity.WasCastFrom _ -> False
  Quantity.BlockersBeyondFirst -> False
  -- A leaf, Power's answer: it holds no Quantity of its own to hide X inside.
  Quantity.StationMeasure -> False
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
  -- CR 202.3g again: rule 202.3g says "each Phyrexian mana symbol", and CR
  -- 107.4f's hybrid Phyrexian symbols are Phyrexian mana symbols. One, the same
  -- as the arm above and for the same reason -- the symbol is one mana however
  -- it is paid.
  ManaSymbol.HybridPhyrexian _ -> 1
  -- CR 202.3's own sentence, with no subrule: CR 107.4h makes {S} payable with
  -- one mana from a snow source, so Icehide Golem's mana value is 1.
  ManaSymbol.Snow -> 1
  -- CR 202.3e: off the stack a variable's contribution to mana value is 0.
  ManaSymbol.Variable -> 0
