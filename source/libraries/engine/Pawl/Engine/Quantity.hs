module Pawl.Engine.Quantity where

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
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
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
-- Quantity.InSlot deliberately stays on `oid`: Resolve.bindAmountSlot writes
-- that value to the effect's source mid-resolution (Bane of Progress binds and
-- reads one inside a TRIGGERED ability, where the two ids differ), and each
-- amount is read where it was written.
evaluateFor :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> ObjectId -> Quantity -> Maybe Integer
evaluateFor viewOf context gs announcedOn oid quantity = case quantity of
  Quantity.Literal n -> Just n
  Quantity.ManaValue -> fmap manaValueOf (Game.faceOf oid gs)
  -- CR 208.1 read through the injected view, so this arm never learns whether
  -- it is looking at a live projection or a CR 608.2h snapshot -- the caller
  -- decides that by which ViewOf it supplies (Projection.fullView vs.
  -- Projection.viewWithLastKnown). Nothing when the object has no power: it is
  -- not a creature, or it is gone and no last known information was kept.
  Quantity.Power -> viewOf oid >>= Filter.power
  -- CR 601.2b: read the chosen X from the announcing object's binding
  -- environment.
  Quantity.X -> case Game.lookupObject announcedOn gs of
    Nothing -> Nothing
    Just obj -> fmap toInteger (Binding.amountOf Binding.variableX (Object.bindings obj))
  -- A value an earlier effect of this resolution bound into the slot, read off
  -- the effect's SOURCE. Nothing when the slot holds no amount: the producing
  -- effect has not run, or bound nothing.
  Quantity.InSlot slot -> case Game.lookupObject oid gs of
    Nothing -> Nothing
    Just obj -> fmap toInteger (Binding.amountOf slot (Object.bindings obj))
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
  Quantity.X -> quantity
  Quantity.InSlot _ -> quantity
  Quantity.Count _ -> quantity
  Quantity.ManaCount _ -> quantity

-- The binding slots a quantity READS. The read half of the dataflow lint whose
-- write half is Resolve.definedSlots -- so a card whose "for each ... destroyed
-- this way" names a slot nothing binds is a failing test, not a silent no-op.
--
-- X is NOT here, deliberately. It reads Binding.variableX, which casting fills
-- and no card declares as a target slot, so returning it would make the
-- equality lint demand a target spec for X on every card that reads one.
-- Resolve.readsX owns that half of the contract.
slots :: Quantity -> Set SlotName
slots quantity = case quantity of
  Quantity.Literal _ -> Set.empty
  Quantity.ManaValue -> Set.empty
  Quantity.Power -> Set.empty
  Quantity.X -> Set.empty
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
