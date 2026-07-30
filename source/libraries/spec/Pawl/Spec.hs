{-# LANGUAGE RankNTypes #-}

-- | This module defines an abstract interface for writing tests.
module Pawl.Spec where

import qualified Control.Monad as Monad
import qualified GHC.Stack as Stack

-- | A test specification parameterized by two monads:
--
-- - @m@: assertions (individual test actions)
-- - @n@: test structure (grouping and registration)
data Spec m n = MkSpec
  { assertFailure :: forall x. (Stack.HasCallStack) => String -> m x,
    describe :: String -> n () -> n (),
    it :: String -> m () -> n ()
  }

-- | Asserts that two values are equal.
assertEq :: (Stack.HasCallStack, Applicative m, Eq a, Show a) => Spec m n -> a -> a -> m ()
assertEq s x y = Monad.unless (x == y) . assertFailure s $ "expected: " <> show x <> " == " <> show y

-- | Asserts that two values are not equal.
assertNe :: (Stack.HasCallStack, Applicative m, Eq a, Show a) => Spec m n -> a -> a -> m ()
assertNe s x y = Monad.unless (x /= y) . assertFailure s $ "expected: " <> show x <> " /= " <> show y

-- | Asserts that the first value is less than the second.
assertLt :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertLt s x y = Monad.unless (x < y) . assertFailure s $ "expected: " <> show x <> " < " <> show y

-- | Asserts that the first value is less than or equal to the second.
assertLe :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertLe s x y = Monad.unless (x <= y) . assertFailure s $ "expected: " <> show x <> " <= " <> show y

-- | Asserts that the first value is greater than the second.
assertGt :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertGt s x y = Monad.unless (x > y) . assertFailure s $ "expected: " <> show x <> " > " <> show y

-- | Asserts that the first value is greater than or equal to the second.
assertGe :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertGe s x y = Monad.unless (x >= y) . assertFailure s $ "expected: " <> show x <> " >= " <> show y
