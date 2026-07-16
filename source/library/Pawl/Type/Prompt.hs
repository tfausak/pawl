{-# LANGUAGE GADTs #-}

module Pawl.Type.Prompt where

import Pawl.Type.Action (Action)
import Pawl.Type.Decider (Decider)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

data Prompt r where
  ChooseAction :: Decider -> PlayerId -> [Action] -> Prompt Action
  Shuffle :: [ObjectId] -> Prompt [ObjectId]
