{-# LANGUAGE RankNTypes #-}

module Pawl.Spec where

import qualified Control.Monad as Monad
import qualified GHC.Stack as Stack

-- | An abstract specification parameterized by two monads:
--
-- - @m@: assertions (individual test actions)
-- - @n@: test structure (grouping and registration)
data Spec m n = MkSpec
  { assertFailure :: forall x. (Stack.HasCallStack) => String -> m x,
    describe :: String -> n () -> n (),
    it :: String -> m () -> n ()
  }

-- | Asserts that a condition holds, failing with the given message. Every other
-- assertion is built on this one.
assertBool :: (Stack.HasCallStack, Applicative m) => Spec m n -> Bool -> String -> m ()
assertBool s b = Monad.unless b . assertFailure s

-- | Prefixes a failure with the caller's message, when there is one.
prefixed :: String -> String -> String
prefixed m t = if null m then t else m <> ": " <> t

-- | Asserts that a binary relation holds between two values.
assertOp :: (Stack.HasCallStack, Applicative m, Show a) => Spec m n -> String -> String -> (a -> a -> Bool) -> a -> a -> m ()
assertOp s m op f x y = assertBool s (f x y) . prefixed m $ "expected: " <> show x <> " " <> op <> " " <> show y

-- | Asserts that two values are equal.
assertEq :: (Stack.HasCallStack, Applicative m, Eq a, Show a) => Spec m n -> a -> a -> m ()
assertEq s = assertEqWith s ""

-- | 'assertEq' with a message.
assertEqWith :: (Stack.HasCallStack, Applicative m, Eq a, Show a) => Spec m n -> String -> a -> a -> m ()
assertEqWith s m = assertOp s m "==" (==)

-- | Asserts that two values are not equal.
assertNe :: (Stack.HasCallStack, Applicative m, Eq a, Show a) => Spec m n -> a -> a -> m ()
assertNe s = assertNeWith s ""

-- | 'assertNe' with a message.
assertNeWith :: (Stack.HasCallStack, Applicative m, Eq a, Show a) => Spec m n -> String -> a -> a -> m ()
assertNeWith s m = assertOp s m "/=" (/=)

-- | Asserts that the first value is less than the second.
assertLt :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertLt s = assertLtWith s ""

-- | 'assertLt' with a message.
assertLtWith :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> String -> a -> a -> m ()
assertLtWith s m = assertOp s m "<" (<)

-- | Asserts that the first value is less than or equal to the second.
assertLe :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertLe s = assertLeWith s ""

-- | 'assertLe' with a message.
assertLeWith :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> String -> a -> a -> m ()
assertLeWith s m = assertOp s m "<=" (<=)

-- | Asserts that the first value is greater than the second.
assertGt :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertGt s = assertGtWith s ""

-- | 'assertGt' with a message.
assertGtWith :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> String -> a -> a -> m ()
assertGtWith s m = assertOp s m ">" (>)

-- | Asserts that the first value is greater than or equal to the second.
assertGe :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> a -> a -> m ()
assertGe s = assertGeWith s ""

-- | 'assertGe' with a message.
assertGeWith :: (Stack.HasCallStack, Applicative m, Ord a, Show a) => Spec m n -> String -> a -> a -> m ()
assertGeWith s m = assertOp s m ">=" (>=)
