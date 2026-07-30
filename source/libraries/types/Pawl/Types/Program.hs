{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.Types.Program where

import Control.Monad ((>=>))

-- An operational (free) monad over an instruction functor 'instr'.
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
