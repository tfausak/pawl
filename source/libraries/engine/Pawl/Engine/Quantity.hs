module Pawl.Engine.Quantity where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.Card as Card
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
-- `viewOf` and `context` are INJECTED rather than built here, the same
-- module-cycle reason a player used to be: Pawl.Engine.Projection imports
-- Pawl.Engine.Quantity, so the arrow only points one way, and this module cannot ask
-- the projection what a candidate's characteristics are or who the current
-- perspective/source is. They flow straight through to Pawl.Engine.Count.evaluate for
-- the Count arm; every other arm ignores them.
--
-- CR 109.5 / 604.3a(3): the caller's choice of context is where the #34 fix
-- lives. Projection.applyModification builds its context from the effect's
-- SOURCE's controller (a static ability's continuous effect); Projection's
-- applyCharacteristicPT builds its from the OBJECT's own controller (a
-- characteristic-defining ability, which never affects another object);
-- Resolve builds its from the resolving spell/ability's controller and source.
--
-- The ONE-OBJECT case, where CR 601.2b's announced X was stamped on the very
-- object every other arm reads. True of a spell, whose stack incarnation holds
-- both, and of every caller outside a resolution; evaluateFor below is the case
-- where the two objects part company.
evaluate :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> Quantity -> Maybe Integer
evaluate viewOf context gs oid = evaluateFor viewOf context gs oid oid

-- The same fold with CR 601.2b's X taken from `announcedOn` instead of from
-- `oid`, because for an ACTIVATED ability they are different objects:
--
--   * `announcedOn` is the object ON THE STACK. That is where the announced value
--     is stamped -- Cast.castSpell stamps the spell's new incarnation,
--     Activate.activateAbility stamps the ability object -- so it is the only
--     place the value can be read back from.
--   * `oid` is the ability's SOURCE -- CR 113.7's "the object whose ability was
--     activated" -- which every other arm reads and which an activation cost may
--     well have destroyed: Cinder Elemental's "{X}{R}, {T}, Sacrifice this
--     creature: It deals X damage to any target" pays with the very permanent the
--     ability names, so by resolution CR 400.7 has left its id naming nothing at
--     all, and CR 113.7a is what lets the ability resolve regardless (#544).
--
-- Quantity.InSlot deliberately stays on `oid`: that value is written mid-
-- resolution by Resolve.bindAmountSlot, which writes it to the effect's source --
-- see its haddock, and Bane of Progress, which binds and reads one inside a
-- TRIGGERED ability where the two ids differ. Each amount is read where it was
-- written.
evaluateFor :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> ObjectId -> Quantity -> Maybe Integer
evaluateFor viewOf context gs announcedOn oid quantity = case quantity of
  Quantity.Literal n -> Just n
  Quantity.ManaValue -> fmap manaValueOf (Game.cardOf oid gs)
  -- CR 208.1 read through the injected view, so this arm never learns whether it
  -- is looking at a live projection or a CR 608.2h snapshot -- the caller decides
  -- that by which ViewOf it supplies (Projection.fullView vs.
  -- Projection.viewWithLastKnown). Nothing when the object has no power: it is
  -- not a creature, or it is gone and no last known information was kept.
  Quantity.Power -> viewOf oid >>= Filter.power
  -- CR 601.2b: read the chosen X from the announcing object's binding environment.
  Quantity.X -> case Game.lookupObject announcedOn gs of
    Nothing -> Nothing
    Just obj -> fmap toInteger (Binding.amountOf Binding.variableX (Object.bindings obj))
  -- A value an earlier effect of this resolution bound into the slot, read off
  -- the effect's SOURCE and the same binding field X is read from -- a different
  -- object from X's, for the reason this function's own haddock gives. Nothing
  -- when the slot holds no amount: the producing effect has not run, or bound
  -- nothing.
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
  -- which reads the CR 613 projection through the injected ViewOf.
  --
  -- The second injection is this function itself, aimed at whichever CANDIDATE
  -- the fold is looking at rather than at `oid`: that is how
  -- Aggregation.Greatest reads a per-member quantity without Pawl.Engine.Count
  -- importing this module. `announcedOn` stays FIXED across the candidates -- CR
  -- 601.2b's X belongs to the resolving object, not to whichever permanent the
  -- count is looking at. Terminating, though the two functions call each other: a
  -- Greatest's payload is a strictly smaller subterm of `quantity`, and the value
  -- came from finite card data.
  Quantity.Count c -> Count.evaluate viewOf (evaluateFor viewOf context gs announcedOn) context gs c

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

-- The binding slots a quantity READS. Part of the read half of the D4 dataflow
-- lint (Resolve.slotsOf calls this for every Quantity an opcode carries), whose
-- write half is Resolve.definedSlots -- so a card whose "for each ... destroyed
-- this way" names a slot nothing binds is a failing test, not a silent no-op.
--
-- X is NOT here, deliberately. It reads Binding.variableX, which casting fills
-- and no card declares as a target slot, so returning it would make the D4
-- equality lint ("a mode's slot reads equal its declared slots") demand a target
-- spec for X on every card that reads one. The {X} half of the same contract is
-- Resolve.readsX's, and stays there.
slots :: Quantity -> Set SlotName
slots quantity = case quantity of
  Quantity.Literal _ -> Set.empty
  Quantity.ManaValue -> Set.empty
  Quantity.Power -> Set.empty
  Quantity.X -> Set.empty
  Quantity.InSlot slot -> Set.singleton slot
  Quantity.Star -> Set.empty
  Quantity.Plus a b -> Set.union (slots a) (slots b)
  -- Terminating for the reason evaluate's Count arm is: a Greatest's payload is a
  -- strictly smaller subterm.
  Quantity.Count c -> Count.slots slots c

-- CR 202.3: the mana value is "the total amount of mana in its mana cost,
-- regardless of color" -- each generic symbol contributes its number, each
-- colored or colorless symbol one, and each hybrid symbol its largest half (CR
-- 202.3f). A land has no mana cost (CR 202.1b), so its mana value is 0 (CR
-- 202.3a).
manaValueOf :: Card.Card -> Integer
manaValueOf card = case Card.manaCost card of
  Nothing -> 0
  Just (ManaCost.MkManaCost symbols) -> sum (fmap symbolValue symbols)

symbolValue :: ManaSymbol.ManaSymbol -> Integer
symbolValue symbol = case symbol of
  ManaSymbol.Generic n -> toInteger n
  ManaSymbol.OfType _ -> 1
  -- CR 202.3f: "use the largest component of each hybrid symbol." Both halves of
  -- a colour/colour hybrid are one mana, so the largest is one -- whichever half
  -- is eventually paid, and its own example agrees: "{1}{W/U}{W/U} is 3".
  ManaSymbol.Hybrid _ _ -> 1
  -- CR 202.3f again, and here the halves differ: {2/B}'s generic half is the
  -- larger, so the symbol is worth 2 and not the 1 every other typed symbol is
  -- worth. Its own example is Flame Javelin's cost: "the mana value of a card
  -- with mana cost {2/B}{2/B}{2/B} is 6."
  ManaSymbol.MonocoloredHybrid _ -> 2
  -- CR 202.3g, a rule of its own rather than CR 202.3f's "largest component":
  -- "Each Phyrexian mana symbol in a card's mana cost contributes 1 to its mana
  -- value." The other half is 2 LIFE, not 2 mana, so there is no larger
  -- component to take, and its own example says so: "The mana value of a card
  -- with mana cost {1}{W/P}{W/P} is 3." Mutagenic Growth ({G/P}) is 1, not 2.
  ManaSymbol.Phyrexian _ -> 1
  -- CR 202.3's own sentence, with no subrule of its own: the mana value is "the
  -- total amount of mana in its mana cost, regardless of color", and CR 107.4h
  -- makes {S} "a cost that can be paid with one mana of any type produced by a
  -- snow source" -- one mana. Icehide Golem's mana value is 1.
  ManaSymbol.Snow -> 1
  -- CR 202.3e: off the stack a variable's contribution to mana value is 0.
  ManaSymbol.Variable -> 0
