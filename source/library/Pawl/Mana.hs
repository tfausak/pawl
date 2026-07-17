module Pawl.Mana where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Game as Game
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.Color as Color
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.Mana (Mana)
import qualified Pawl.Type.Mana as Mana
import Pawl.Type.ManaCost (ManaCost)
import qualified Pawl.Type.ManaCost as ManaCost
import Pawl.Type.ManaSymbol (ManaSymbol)
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import Pawl.Type.ManaType (ManaType)
import qualified Pawl.Type.ManaType as ManaType
import Pawl.Type.ManaUnit (ManaUnit)
import qualified Pawl.Type.ManaUnit as ManaUnit
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import Pawl.Type.Subtype (Subtype)
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone

-- CR 305.6: a basic land's mana ability is granted intrinsically by its subtype,
-- not printed in its text box. This is a classification of the type line -- it
-- never reads the card's identity.
subtypeMana :: Subtype -> Maybe ManaType
subtypeMana subtype = case subtype of
  Subtype.Mountain -> Just (ManaType.Colored Color.Red)
  Subtype.Goblin -> Nothing
  Subtype.Warrior -> Nothing
  Subtype.Human -> Nothing
  Subtype.Bird -> Nothing
  Subtype.Ogre -> Nothing
  Subtype.Centaur -> Nothing
  Subtype.Cat -> Nothing
  Subtype.Dinosaur -> Nothing
  Subtype.Beast -> Nothing

-- Every mana type an object could produce, derived from its subtypes.
manaTypesOf :: ObjectId -> GameState -> [ManaType]
manaTypesOf oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Object.source obj of
    Source.OfCard printing ->
      Maybe.mapMaybe subtypeMana $
        Set.toList (TypeLine.subtypes (Card.typeLine (Printing.card printing)))

-- CR 106.4. Absent from the map means an empty pool.
poolOf :: PlayerId -> GameState -> Mana
poolOf pid gs = Map.findWithDefault (Mana.MkMana []) pid (GameState.manaPool gs)

setPool :: PlayerId -> Mana -> GameState -> GameState
setPool pid pool gs = gs {GameState.manaPool = Map.insert pid pool (GameState.manaPool gs)}

addMana :: PlayerId -> [ManaUnit] -> GameState -> GameState
addMana pid units gs =
  let Mana.MkMana existing = poolOf pid gs
   in setPool pid (Mana.MkMana (existing ++ units)) gs

-- CR 500.4: each player's mana pool empties at the end of every step and phase.
emptyManaPools :: GameState -> GameState
emptyManaPools gs = gs {GameState.manaPool = Map.empty}

-- Untapped permanents this player owns that can produce mana. Owner rather than
-- controller: M0 has no controller field, and nothing in M1a can change control.
manaSources :: PlayerId -> GameState -> [ObjectId]
manaSources pid gs =
  let isSource oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> Object.tapped obj == TapState.Untapped && not (null (manaTypesOf oid gs))
   in filter isSource (Game.zoneMembers Zone.Battlefield pid gs)

-- Activate an object's intrinsic mana ability: tap it, add its mana. CR 605.3:
-- a mana ability does not use the stack, so this is immediate.
--
-- Takes the first mana type the object can produce. A Mountain produces exactly
-- one, so there is no choice to make. EXPIRES when a source can produce more
-- than one type (any dual land): choosing between them is a real decision and
-- must become a Prompt.
tapForMana :: ObjectId -> GameState -> GameState
tapForMana oid gs = case Game.lookupObject oid gs of
  Nothing -> gs
  Just obj -> case manaTypesOf oid gs of
    [] -> gs
    produced : _ ->
      let tapped = obj {Object.tapped = TapState.Tapped}
          gs1 = gs {GameState.objects = Map.insert oid tapped (GameState.objects gs)}
       in addMana (Object.owner obj) [ManaUnit.MkManaUnit {ManaUnit.manaType = produced}] gs1

typedOf :: ManaSymbol -> Maybe ManaType
typedOf symbol = case symbol of
  ManaSymbol.OfType t -> Just t
  ManaSymbol.Generic _ -> Nothing

genericOf :: ManaSymbol -> Natural
genericOf symbol = case symbol of
  ManaSymbol.Generic n -> n
  ManaSymbol.OfType _ -> 0

takeTyped :: [ManaUnit] -> ManaType -> Maybe [ManaUnit]
takeTyped units wanted = case List.break (\u -> ManaUnit.manaType u == wanted) units of
  (before, _ : after) -> Just (before ++ after)
  (_, []) -> Nothing

takeAny :: [ManaUnit] -> a -> Maybe [ManaUnit]
takeAny units _ = case units of
  _ : rest -> Just rest
  [] -> Nothing

-- Spend a pool against a cost. Nothing when the pool cannot cover it.
--
-- Typed symbols are matched FIRST because they are the constrained ones: generic
-- takes any unit, so paying it first could consume the only red and strand a
-- {R} that nothing else can satisfy.
spend :: ManaCost -> Mana -> Maybe Mana
spend cost (Mana.MkMana units) =
  let ManaCost.MkManaCost symbols = cost
      typed = Maybe.mapMaybe typedOf symbols
      generic = sum (map genericOf symbols)
   in do
        afterTyped <- Monad.foldM takeTyped units typed
        afterGeneric <- Monad.foldM takeAny afterTyped [1 .. generic]
        pure (Mana.MkMana afterGeneric)

-- Produce mana by tapping sources front-of-list until the cost is covered, then
-- spend it. Nothing when it cannot be covered.
--
-- This elides a choice, and that is legitimate ONLY because the sources are
-- INDISTINGUISHABLE: every Mountain produces exactly one red unit, so picking
-- among them is canonicalization, not a decision -- there is no policy to have.
-- The engine makes no player choices; strategy belongs to the interpreter.
-- EXPIRES the moment mana sources differ in any way a player could care about,
-- at which point this must become a real Prompt.
payCost :: PlayerId -> ManaCost -> GameState -> Maybe GameState
payCost pid cost gs =
  let tapUntilPaid remaining state = case spend cost (poolOf pid state) of
        Just left -> Just (setPool pid left state)
        Nothing -> case remaining of
          [] -> Nothing
          oid : rest -> tapUntilPaid rest (tapForMana oid state)
   in tapUntilPaid (manaSources pid gs) gs

canPay :: PlayerId -> ManaCost -> GameState -> Bool
canPay pid cost gs = Maybe.isJust (payCost pid cost gs)
