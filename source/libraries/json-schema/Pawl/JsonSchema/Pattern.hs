-- | Matching a string against a JSON Schema @pattern@.
--
-- Deliberately NOT a regular expression engine, and not the ECMA-262 dialect
-- the specification names. It implements exactly the constructs the patterns
-- this project emits use --- anchors, alternation, grouping, character classes
-- with ranges, literals, and postfix @*@ --- and answers 'Left' for anything
-- else, so a pattern outside that subset is reported as a schema defect rather
-- than silently treated as matching or as not matching. Not implemented:
-- escapes, negated classes, the other repetition operators, the wildcard, and
-- unanchored matching (gap #2108).
--
-- An unanchored pattern is one of the unsupported cases. JSON Schema's
-- @pattern@ is a partial match, this matcher is a whole-string one, and the
-- difference is invisible on the anchored patterns the project writes, so
-- refusing the unanchored ones is what keeps the two readings from diverging.
module Pawl.JsonSchema.Pattern where

import qualified Data.List as List
import qualified Data.Text as Text

-- | One element of a pattern. A group is an 'Alternation', since a group with
-- one branch and a group are the same thing.
data Node
  = Literal Char
  | -- | Inclusive ranges; a bare character is a range with itself.
    Class [(Char, Char)]
  | Alternation [[Node]]
  | Star Node
  deriving (Eq, Ord, Show)

-- | Whether the string matches the pattern. 'Left' means the PATTERN is
-- outside the supported subset, which is a defect in the schema rather than a
-- verdict about the string.
matches :: Text.Text -> Text.Text -> Either Text.Text Bool
matches source value = do
  nodes <- parse $ Text.unpack source
  pure . List.any null . matchNodes nodes $ Text.unpack value

-- Parsing --------------------------------------------------------------------

-- | Both anchors are required and are stripped here rather than modelled,
-- because the matcher is whole-string anyway.
parse :: String -> Either Text.Text [Node]
parse input = case input of
  '^' : rest | not (null rest) && last rest == '$' -> do
    (branches, remaining) <- alternation $ init rest
    if null remaining
      then Right $ flatten branches
      else Left $ unsupported remaining
  _ -> Left . Text.pack $ "expected an anchored pattern but got " <> show input

-- | A single branch stays a bare sequence; only a real choice becomes a node.
flatten :: [[Node]] -> [Node]
flatten branches = case branches of
  [branch] -> branch
  _ -> [Alternation branches]

-- | Stops at @)@ or at the end of input, so a group and the whole pattern share
-- it.
alternation :: String -> Either Text.Text ([[Node]], String)
alternation = go []
  where
    go acc input = do
      (branch, rest) <- sequenceOf input
      case rest of
        '|' : more -> go (branch : acc) more
        _ -> Right (reverse $ branch : acc, rest)

sequenceOf :: String -> Either Text.Text ([Node], String)
sequenceOf = go []
  where
    go acc input = case input of
      [] -> Right (reverse acc, input)
      c : _ | c == '|' || c == ')' -> Right (reverse acc, input)
      _ -> do
        (node, rest) <- atom input
        case rest of
          '*' : more -> go (Star node : acc) more
          _ -> go (node : acc) rest

atom :: String -> Either Text.Text (Node, String)
atom input = case input of
  '(' : rest -> do
    (branches, remaining) <- alternation rest
    case remaining of
      ')' : more -> Right (Alternation branches, more)
      _ -> Left $ unsupported input
  '[' : rest -> characterClass [] rest
  c : rest | List.notElem c metacharacters -> Right (Literal c, rest)
  _ -> Left $ unsupported input

-- | Positive classes only: a negated class, an escape and a trailing @-@ are
-- all unsupported rather than guessed at.
characterClass :: [(Char, Char)] -> String -> Either Text.Text (Node, String)
characterClass acc input = case input of
  ']' : rest | not (null acc) -> Right (Class $ reverse acc, rest)
  a : '-' : b : rest | ordinary a && ordinary b -> characterClass ((a, b) : acc) rest
  a : rest | ordinary a -> characterClass ((a, a) : acc) rest
  _ -> Left $ unsupported input

ordinary :: Char -> Bool
ordinary c = List.notElem c "[]-\\^"

metacharacters :: String
metacharacters = "\\^$.|?*+()[]{}"

unsupported :: String -> Text.Text
unsupported input = Text.pack $ "unsupported pattern syntax at " <> show input

-- Matching -------------------------------------------------------------------
--
-- Every matcher answers with EVERY remaining input the node could leave, so
-- backtracking falls out of the list rather than needing its own machinery.

matchNodes :: [Node] -> String -> [String]
matchNodes nodes input = case nodes of
  [] -> [input]
  node : rest -> concatMap (matchNodes rest) $ matchNode node input

matchNode :: Node -> String -> [String]
matchNode node input = case node of
  Literal c -> case input of
    x : xs | x == c -> [xs]
    _ -> []
  Class ranges -> case input of
    x : xs | List.any (\(a, b) -> a <= x && x <= b) ranges -> [xs]
    _ -> []
  Alternation branches -> concatMap (`matchNodes` input) branches
  Star inner -> star inner input

-- | Only a repetition that CONSUMED input is repeated, so a group that matches
-- the empty string cannot loop forever.
star :: Node -> String -> [String]
star node input =
  let n = length input
   in input : concatMap (star node) (filter ((< n) . length) $ matchNode node input)
