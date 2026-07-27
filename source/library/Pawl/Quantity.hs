module Pawl.Quantity where

import qualified Pawl.Binding as Binding
import qualified Pawl.Count as Count
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Type.Card as Card
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.Quantity (Quantity)
import qualified Pawl.Type.Quantity as Quantity

-- Nothing when the value cannot be determined.
--
-- `viewOf` and `context` are INJECTED rather than built here, the same
-- module-cycle reason a player used to be: Pawl.Projection imports
-- Pawl.Quantity, so the arrow only points one way, and this module cannot ask
-- the projection what a candidate's characteristics are or who the current
-- perspective/source is. They flow straight through to Pawl.Count.evaluate for
-- the Count arm; every other arm ignores them.
--
-- CR 109.5 / 604.3a(3): the caller's choice of context is where the #34 fix
-- lives. Projection.applyModification builds its context from the effect's
-- SOURCE's controller (a static ability's continuous effect); Projection's
-- applyCharacteristicPT builds its from the OBJECT's own controller (a
-- characteristic-defining ability, which never affects another object);
-- Resolve builds its from the resolving spell/ability's controller and source.
evaluate :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> Quantity -> Maybe Integer
evaluate viewOf context gs oid quantity = case quantity of
  Quantity.Literal n -> Just n
  Quantity.ManaValue -> fmap manaValueOf (Game.cardOf oid gs)
  -- CR 208.1 read through the injected view, so this arm never learns whether it
  -- is looking at a live projection or a CR 608.2h snapshot -- the caller decides
  -- that by which ViewOf it supplies (Projection.fullView vs.
  -- Projection.viewWithLastKnown). Nothing when the object has no power: it is
  -- not a creature, or it is gone and no last known information was kept.
  Quantity.Power -> viewOf oid >>= Filter.power
  -- CR 601.2b: read the chosen X from the source object's binding environment.
  Quantity.X -> case Game.lookupObject oid gs of
    Nothing -> Nothing
    Just obj -> fmap toInteger (Binding.amountOf Binding.variableX (Object.bindings obj))
  -- CR 208.2: a bare star has no value of its own. The projection substitutes
  -- the object's characteristic-defining quantity for it at the seed
  -- (Projection.baseCharacteristics), so reaching this arm means the star was
  -- never resolved -- honestly Nothing, not a hole.
  Quantity.Star -> Nothing
  Quantity.Plus a b -> case (evaluate viewOf context gs oid a, evaluate viewOf context gs oid b) of
    (Just x, Just y) -> Just (x + y)
    _ -> Nothing
  -- CR 208.2a / 608.2h: delegate to the general Count fold (Pawl.Count),
  -- which reads the CR 613 projection through the injected ViewOf.
  Quantity.Count c -> Count.evaluate viewOf context gs c

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
  Quantity.Count _ -> quantity

-- CR 202.3: the mana value is the total amount of mana in the cost -- each
-- generic symbol contributes its number, each colored/typed symbol contributes
-- one. A land has no mana cost (CR 202.1), so its mana value is 0.
manaValueOf :: Card.Card -> Integer
manaValueOf card = case Card.manaCost card of
  Nothing -> 0
  Just (ManaCost.MkManaCost symbols) -> sum (fmap symbolValue symbols)

symbolValue :: ManaSymbol.ManaSymbol -> Integer
symbolValue symbol = case symbol of
  ManaSymbol.Generic n -> toInteger n
  ManaSymbol.OfType _ -> 1
  -- CR 202.3b: off the stack a variable's contribution to mana value is 0.
  ManaSymbol.Variable -> 0
