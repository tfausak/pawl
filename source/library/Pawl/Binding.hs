module Pawl.Binding where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.Type.Binding (Binding)
import qualified Pawl.Type.Binding as Binding
import Pawl.Type.Recipient (Recipient)
import Pawl.Type.SlotName (SlotName)
import qualified Pawl.Type.SlotName as SlotName
import Pawl.Type.Subtype (Subtype)

-- CR 601.2b: the reserved slot under which a spell's single chosen X is stored.
-- No card's targetSpecs may name it (the D4 lint enforces this): X is not a
-- target, so it needs a key the target namespace cannot collide with.
variableX :: SlotName
variableX = SlotName.MkSlotName (Text.pack "X")

-- Project the chosen targets (CR 601.2c) out of a binding environment, dropping
-- slots with no target. Restores the pre-M4a Object.targets view for readers.
targetsOf :: Map SlotName Binding -> Map SlotName Recipient
targetsOf = Map.mapMaybe Binding.target

-- Project the chosen (from, to) land-type pairs (CR 612), dropping slots without.
subtypesOf :: Map SlotName Binding -> Map SlotName (Subtype, Subtype)
subtypesOf = Map.mapMaybe Binding.subtypes

-- The amount (X) bound at a slot, if any.
amountOf :: SlotName -> Map SlotName Binding -> Maybe Natural
amountOf slot m = Binding.amount =<< Map.lookup slot m

-- Build the binding environment stamped on a stack object at cast: the chosen
-- targets, the chosen land-type pairs, and (Just x) the chosen X under variableX.
-- A slot present in several inputs keeps every choice (Magical Hack's slot).
fromChoices ::
  Map SlotName Recipient ->
  Map SlotName (Subtype, Subtype) ->
  Maybe Natural ->
  Map SlotName Binding
fromChoices targets subtypes mAmount =
  let fromTargets = Map.map (\r -> Binding.empty {Binding.target = Just r}) targets
      fromSubtypes = Map.map (\p -> Binding.empty {Binding.subtypes = Just p}) subtypes
      merged = Map.unionWith mergeBinding fromTargets fromSubtypes
   in case mAmount of
        Nothing -> merged
        Just n ->
          Map.insertWith mergeBinding variableX (Binding.empty {Binding.amount = Just n}) merged

-- Combine two bindings for the same slot, preferring the left's present choice in
-- each field. Inputs are disjoint per field by construction, so this is a total,
-- order-independent merge.
mergeBinding :: Binding -> Binding -> Binding
mergeBinding a b =
  Binding.MkBinding
    { Binding.target = Binding.target a <|> Binding.target b,
      Binding.subtypes = Binding.subtypes a <|> Binding.subtypes b,
      Binding.amount = Binding.amount a <|> Binding.amount b
    }
