module Pawl.JsonPointer.Evaluate where

import qualified Data.Function as Function
import Data.List ((!?))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonPointer.Pointer as Pointer
import qualified Pawl.JsonPointer.Token as Token
import qualified Text.Read as Read

-- | Evaluates a JSON Pointer against a JSON Value. Returns 'Nothing' if the
-- path does not exist or is invalid. Per RFC 6901:
--
-- - An empty pointer returns the document itself.
-- - For arrays, tokens must be valid non-negative integer indices.
-- - For objects, tokens are matched against member names.
evaluate :: Pointer.Pointer -> Value.Value -> Maybe Value.Value
evaluate = Function.fix $ \rec pointer value ->
  case Pointer.unwrap pointer of
    [] -> Just value
    token : rest -> do
      child <- step token value
      rec (Pointer.MkPointer rest) child

-- | Takes a single step in a JSON value using a reference token.
step :: Token.Token -> Value.Value -> Maybe Value.Value
step token value = case value of
  Value.Array array -> stepArray token array
  Value.Object object -> stepObject token object
  _ -> Nothing

-- | Steps into an array using a token as an index. Per RFC 6901, array indices
-- must be:
--
-- - Non-negative integers.
-- - Either "0" or not starting with "0" (no leading zeros).
stepArray :: Token.Token -> Array.Array a -> Maybe a
stepArray token array = do
  index <- case Text.unpack $ Token.unwrap token of
    '0' : _ : _ -> Nothing
    string -> Read.readMaybe string
  Array.unwrap array !? index

-- | Steps into an object using a token as a key.
stepObject :: Token.Token -> Object.Object a -> Maybe a
stepObject token =
  fmap Pair.value
    . List.find ((== Token.unwrap token) . String.unwrap . Pair.name)
    . Object.unwrap
