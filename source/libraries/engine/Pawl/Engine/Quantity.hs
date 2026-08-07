module Pawl.Engine.Quantity where

import Control.Applicative ((<|>))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.ManaCount as ManaCount
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.Quantity (Quantity)
import qualified Pawl.Types.Quantity as Quantity
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
evaluateFor viewOf context gs announcedOn oid quantity = case quantity of
  Quantity.Literal n -> Just n
  -- Game.manaCostFaceOf and not Game.faceOf: CR 712.8e reads a transformed
  -- permanent's mana value off its FRONT face's cost while every other
  -- characteristic comes off its back.
  Quantity.ManaValue -> fmap manaValueOf (Game.manaCostFaceOf oid gs)
  -- CR 208.1 read through the injected view, so this arm never learns whether
  -- it is looking at a live projection or a CR 608.2h snapshot -- the caller
  -- decides that by which ViewOf it supplies (Projection.fullView vs.
  -- Projection.viewWithLastKnown). Nothing when the object has no power: it is
  -- not a creature, or it is gone and no last known information was kept.
  Quantity.Power -> viewOf oid >>= Filter.power
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
  -- at all -- neither as a target spec (Pawl.CardSpec's reservedDeclarations)
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
     in fmap toInteger (boundOn oid <|> boundOn announcedOn)
  -- CR 208.2: a bare star has no value of its own. The projection substitutes
  -- the object's characteristic-defining quantity for it at the seed
  -- (Projection.baseCharacteristics), so reaching this arm means the star was
  -- never resolved -- honestly Nothing, not a hole.
  Quantity.Star -> Nothing
  Quantity.Plus a b -> case (evaluateFor viewOf context gs announcedOn oid a, evaluateFor viewOf context gs announcedOn oid b) of
    (Just x, Just y) -> Just (x + y)
    _ -> Nothing
  -- CR 208.2a / 608.2h: delegate to the general Count fold (Pawl.Engine.Count),
  -- which reads the CR 613 projection through the injected ViewOf. The second
  -- injection is this function itself, aimed at whichever CANDIDATE the fold is
  -- looking at, which is how Aggregation.Greatest reads a per-member quantity
  -- without Count importing this module. `announcedOn` stays FIXED across the
  -- candidates: CR 601.2b's X belongs to the resolving object. Terminating
  -- despite the mutual recursion -- a Greatest's payload is a strictly smaller
  -- subterm.
  Quantity.Count c -> Count.evaluate viewOf (evaluateFor viewOf context gs announcedOn) context gs c
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
  -- The PlayerRef is resolved by the same Count.playersFor the two arms above
  -- use, which is what keeps one reference from meaning different players in
  -- different arms. Nothing for anything but EXACTLY ONE player: a life total is
  -- one player's scalar, so a reference naming several answers "whose?" rather
  -- than answering with a sum. Summing or maximising over several is a different
  -- shape -- an aggregation, as Pawl.Types.Aggregation is for objects -- and no
  -- card in the pool asks for one (#681).
  Quantity.LifeTotal ref -> case Count.playersFor context gs ref of
    Just [pid] -> fmap Player.life (Map.lookup pid (GameState.players gs))
    _ -> Nothing
  -- CR 702.179e / 702.179f: a player's speed. LifeTotal's arm in every respect
  -- above -- read live, resolved through the same Count.playersFor, and Nothing
  -- for a reference naming anything but exactly one player.
  --
  -- CR 702.179f is applied HERE and only here: "if that player has no speed,
  -- their speed is 0 for the purpose of an effect that refers to speed", and
  -- this arm IS such an effect's reading, so Player.speed of Nothing (CR
  -- 702.179b) answers Just 0. The outer Nothing means "which player?" went
  -- unanswered, which is a different claim -- a player the map does not hold at
  -- all is not a player with no speed.
  Quantity.Speed ref -> case Count.playersFor context gs ref of
    Just [pid] -> fmap (maybe 0 toInteger . Player.speed) (Map.lookup pid (GameState.players gs))
    _ -> Nothing
  -- CR 122.1: how many counters of a kind that player has. The third arm on
  -- LifeTotal's and Speed's terms -- live, one player only, through the same
  -- Count.playersFor.
  --
  -- A kind the player's map does not hold answers 0 rather than Nothing, which
  -- is Player.counters' own convention and not this arm's invention: an absent
  -- key means the player has none of that counter, and "none" is a number. The
  -- outer Nothing is reserved for the reference, exactly as above.
  Quantity.PlayerCounters ref kind -> case Count.playersFor context gs ref of
    Just [pid] -> fmap (toInteger . Map.findWithDefault 0 kind . Player.counters) (Map.lookup pid (GameState.players gs))
    _ -> Nothing

-- CR 208.2a, last sentence: an undeterminable number is 0, including inside a
-- calculation. TOTAL where evaluate is partial -- an Integer, never a Maybe.
--
-- The recursion through Plus is what "inside a calculation" buys, and it is not
-- the same answer as substituting at the top: Tarmogoyf's printed 1+* is 1 when
-- its count cannot be determined, because it is the COUNT that becomes 0 and
-- not the sum. Plus is the only calculation Pawl.Types.Quantity has.
--
-- SCOPED TO THE CHARACTERISTIC-DEFINING ABILITY, as CR 208.2a is:
-- Projection.applyCharacteristicPT is the only caller, and every other reader
-- of a quantity must keep evaluate's honest Nothing, since no rule tells those
-- to invent a number.
--
-- It does NOT descend into a Count, and does not need to: an undeterminable
-- count IS the number CR 208.2a is talking about, so the 0 goes in whole here
-- and Pawl.Engine.Count.aggregate stays free to answer Nothing for non-CDA
-- readers.
determine :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> Quantity -> Integer
determine viewOf context gs oid quantity = case quantity of
  Quantity.Plus a b -> determine viewOf context gs oid a + determine viewOf context gs oid b
  _ -> Maybe.fromMaybe 0 (evaluate viewOf context gs oid quantity)

-- CR 208.2: resolve a printed star to the quantity a characteristic-defining
-- ability supplies, recursing through Plus so 1+* becomes 1+<the count>.
substituteStar :: Quantity -> Quantity -> Quantity
substituteStar star quantity = case quantity of
  Quantity.Star -> star
  Quantity.Plus a b -> Quantity.Plus (substituteStar star a) (substituteStar star b)
  Quantity.Literal _ -> quantity
  Quantity.ManaValue -> quantity
  Quantity.Power -> quantity
  Quantity.InSlot _ -> quantity
  Quantity.Count _ -> quantity
  Quantity.ManaCount _ -> quantity
  Quantity.LifeTotal _ -> quantity
  Quantity.Speed _ -> quantity
  Quantity.PlayerCounters _ _ -> quantity

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
  Quantity.InSlot slot -> Set.singleton slot
  Quantity.Star -> Set.empty
  Quantity.Plus a b -> Set.union (slots a) (slots b)
  -- Terminating for the reason evaluate's Count arm is: a Greatest's payload is
  -- a strictly smaller subterm.
  Quantity.Count c -> Count.slots slots c
  -- Neither half of a ManaCount contributes an AMOUNT slot: a ManaFilter names
  -- no slot at all, and PlayerRef.InSlot names a TARGET slot, which is
  -- Resolve's half of the lint. Count's Scope is in the same position.
  Quantity.ManaCount _ -> Set.empty
  -- The same position a third time: this arm's PlayerRef.InSlot names a TARGET
  -- slot, not an amount one.
  Quantity.LifeTotal _ -> Set.empty
  -- And a fourth: LifeTotal's sibling carries a PlayerRef in the same position.
  Quantity.Speed _ -> Set.empty
  -- And a fifth. The PlayerCounterKind beside it names no slot either.
  Quantity.PlayerCounters _ _ -> Set.empty

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
  ManaSymbol.Hybrid _ _ -> 1
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
