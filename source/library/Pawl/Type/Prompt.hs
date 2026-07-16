{-# LANGUAGE GADTs #-}

module Pawl.Type.Prompt where

import Numeric.Natural (Natural)
import Pawl.Type.Action (Action)
import Pawl.Type.Decider (Decider)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

data Prompt r where
  ChooseAction :: Decider -> PlayerId -> [Action] -> Prompt Action
  Shuffle :: [ObjectId] -> Prompt [ObjectId]
  -- CR 514.2. The [ObjectId] is the hand; the Natural is how many to discard.
  ChooseDiscard :: Decider -> PlayerId -> [ObjectId] -> Natural -> Prompt [ObjectId]
