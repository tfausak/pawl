{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.Types.Program where

import Control.Monad ((>=>))

-- | An operational (free) monad over an instruction functor 'instr'.
-- 'Then i k' suspends on instruction 'i' and resumes with continuation 'k'.
data Program instr a where
  Return :: a -> Program instr a
  Then :: instr b -> (b -> Program instr a) -> Program instr a

instance Functor (Program instr) where
  fmap f program = case program of
    Return a -> Return (f a)
    Then i k -> Then i (fmap f . k)

instance Applicative (Program instr) where
  pure = Return
  pf <*> px = do
    f <- pf
    fmap f px

instance Monad (Program instr) where
  program >>= f = case program of
    Return a -> f a
    Then i k -> Then i (k >=> f)

prompt :: instr a -> Program instr a
prompt i = Then i Return

-- Rewrite every instruction a program suspends on, leaving the control flow
-- alone. A natural transformation of the instruction functor, so it cannot see
-- or change an answer -- which is what makes it safe to apply to a program that
-- is already half built.
--
-- The one caller is Engine.playSubgame, which uses it to name the game each
-- suspension came from (CR 729.1a): a subgame runs as a nested StateT over THIS
-- program, so its instructions pass outward through the parent's frame and can
-- be told there, from outside, which game raised them.
mapProgram :: (forall b. instr b -> instr2 b) -> Program instr a -> Program instr2 a
mapProgram f program = case program of
  Return a -> Return a
  Then i k -> Then (f i) (mapProgram f . k)

foldProgram :: (forall b. instr b -> b) -> Program instr a -> a
foldProgram answer program = case program of
  Return a -> a
  Then i k -> foldProgram answer (k (answer i))

foldProgramM :: (Monad m) => (forall b. instr b -> m b) -> Program instr a -> m a
foldProgramM answer program = case program of
  Return a -> pure a
  Then i k -> do
    b <- answer i
    foldProgramM answer (k b)
