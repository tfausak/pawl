module Pawl.Codec.Common where

import qualified Data.Text as Text
import qualified GHC.Stack as Stack
import qualified Pawl.Decimal as Decimal
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Boolean as Boolean
import qualified Pawl.Json.Null as Null
import qualified Pawl.Json.Number as Number
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec

null :: Value.Value
null = Value.Null $ Null.MkNull ()

boolean :: Bool -> Value.Value
boolean = Value.Boolean . Boolean.MkBoolean

number :: Integer -> Integer -> Value.Value
number m = Value.Number . Number.MkNumber . Decimal.mkDecimal m

string :: String -> Value.Value
string = text . Text.pack

text :: Text.Text -> Value.Value
text = Value.String . String.MkString

array :: [Value.Value] -> Value.Value
array = Value.Array . Array.MkArray

pair :: String -> a -> Pair.Pair a
pair = Pair.MkPair . String.MkString . Text.pack

object :: [Pair.Pair Value.Value] -> Value.Value
object = Value.Object . Object.MkObject

asText :: Value.Value -> Either Text.Text Text.Text
asText v = case v of
  Value.String s -> Right $ String.unwrap s
  _ -> Left . Text.pack $ "expected string but got " <> show v

assertFromJson :: (Stack.HasCallStack, Monad m, Eq a, Eq b, Show a, Show b) => Spec.Spec m n -> (Value.Value -> Either a b) -> String -> b -> m ()
assertFromJson s f j x = do
  v <- assertJson s j
  Spec.assertEq s (f v) (Right x)

assertToJson :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> (a -> Value.Value) -> a -> String -> m ()
assertToJson s f x j = do
  v <- assertJson s j
  Spec.assertEq s (f x) v

assertJson :: (Stack.HasCallStack, Applicative m) => Spec.Spec m n -> String -> m Value.Value
assertJson s j = case Parsec.parseString Value.decode j of
  Nothing -> Spec.assertFailure s $ "invalid JSON: " <> show j
  Just v -> pure v
