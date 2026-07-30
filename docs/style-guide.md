# Haskell style guide

> [Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/) ought to be enough for anybody.

This guide describes how I like to write [Haskell](https://www.haskell.org). It
focuses more on style and best practices than formatting and layout.

These are _recommendations_, not hard and fast rules. Deviating from the guide
is encouraged as long as you can defend your decision in code review.

In general, prefer clarity over cleverness. Try to write code that is easy to
understand, debug, and modify.

## Table of contents

- [Apply HLint suggestions](#apply-hlint-suggestions)
- [Avoid backtick operators](#avoid-backtick-operators)
- [Avoid big tuples](#avoid-big-tuples)
- [Avoid boolean blindness](#avoid-boolean-blindness)
- [Avoid compiler warnings](#avoid-compiler-warnings)
- [Avoid duplicate guards](#avoid-duplicate-guards)
- [Avoid excessive parentheses](#avoid-excessive-parentheses)
- [Avoid explicit export lists](#avoid-explicit-export-lists)
- [Avoid importing parents](#avoid-importing-parents)
- [Avoid language extensions](#avoid-language-extensions)
- [Avoid `List`](#avoid-list)
- [Avoid list comprehensions](#avoid-list-comprehensions)
- [Avoid mixing ADTs and records](#avoid-mixing-adts-and-records)
- [Avoid multi-layered nesting](#avoid-multi-layered-nesting)
- [Avoid multiple function declarations](#avoid-multiple-function-declarations)
- [Avoid multiple underscore suffixes](#avoid-multiple-underscore-suffixes)
- [Avoid out of order binders](#avoid-out-of-order-binders)
- [Avoid primes in names](#avoid-primes-in-names)
- [Avoid pure `do`](#avoid-pure-do)
- [Avoid separate `let`s](#avoid-separate-lets)
- [Avoid `String`](#avoid-string)
- [Avoid unnecessary eta reduction](#avoid-unnecessary-eta-reduction)
- [Avoid using partial functions](#avoid-using-partial-functions)
- [Avoid writing partial functions](#avoid-writing-partial-functions)
- [Derive at least `Eq` and `Show`](#derive-at-least-eq-and-show)
- [Expose record constructors and fields](#expose-record-constructors-and-fields)
- [Format with Ormolu](#format-with-ormolu)
- [Group all imports together](#group-all-imports-together)
- [Prefer arbitrary precision numbers](#prefer-arbitrary-precision-numbers)
- [Prefer ASCII](#prefer-ascii)
- [Prefer `case` expressions](#prefer-case-expressions)
- [Prefer composition to application](#prefer-composition-to-application)
- [Prefer `do` notation](#prefer-do-notation)
- [Prefer functions over operators](#prefer-functions-over-operators)
- [Prefer `let` over `where`](#prefer-let-over-where)
- [Prefer line comments](#prefer-line-comments)
- [Prefer monads for building records](#prefer-monads-for-building-records)
- [Prefer qualified imports](#prefer-qualified-imports)
- [Prefer separating re-exports and new declarations](#prefer-separating-re-exports-and-new-declarations)
- [Prefer short identifiers](#prefer-short-identifiers)
- [Prefer unqualified operators](#prefer-unqualified-operators)
- [Use camel case names](#use-camel-case-names)
- [Use descriptive conversion names](#use-descriptive-conversion-names)
- [Use `newtype` liberally](#use-newtype-liberally)
- [Use smart constructors](#use-smart-constructors)

## Avoid compiler warnings

Code should compile with `-Weverything` and `-Werror`. Exceptions are sometimes
allowed. For example:

- `-Wno-implicit-prelude`
- `-Wno-safe`
- `-Wno-unsafe`

<https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3>

## Avoid language extensions

GHC provides a large number of language extensions. Enabling them creates a
staggering number of custom sub-languages. Sticking to the defaults makes
things easier to learn.

``` hs
-- bad
{-# LANGUAGE LambdaCase #-}
\ case
  () -> ()

-- good
\ x -> case x of
  () -> ()
```

<https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#any-flavor-you-like>

## Apply HLint suggestions

Use [HLint](https://github.com/ndmitchell/hlint#readme) to lint all Haskell
code. It suggests many good ways to improve code.

<https://neilmitchell.blogspot.com/2009/09/how-i-use-hlint.html>

## Format with Ormolu

Use Ormolu to format all Haskell code. It may not format everything
perfectly, but it's better than arguing about layout.

<https://chrisdone.com/posts/hindent-5>

## Prefer `let` over `where`

`let` is an expression and follows the same rules as everything else. `where`
can only be used with declarations and can be awkward to use with `do`
notation, guards, and point-free code.

``` hs
-- bad
3 * kibi where kibi = 2 ^ 10

-- good
let kibi = 2 ^ 10 in 3 * kibi
```

<https://stackoverflow.com/questions/4362328/haskell-where-vs-let>

## Avoid out of order binders

Even though the values bound in a `let` expression can be given in any order,
they should be defined in a way that makes them easy to read from top to
bottom.

``` hs
-- bad
let
  x = y
  y = 1
in x

-- good
let
  y = 1
  x = y
in x
```

## Prefer `case` expressions

`case` expressions are generally easier to grok even though they're often
longer than the equivalent expression using functions.

``` hs
-- bad
putStrLn ("howdy " ++ maybe "stranger." (++ "!") maybeName)

-- good
putStrLn (case maybeName of
  Nothing -> "stranger."
  Just name -> name ++ "!")
```

Much like unnecessary eta reduction, this is sometimes a judgement call. Try to
avoid code that's too clever.

<https://www.yesodweb.com/blog/2015/10/beginner-friendly-code-and-apis>

## Avoid unnecessary eta reduction

Excessively point-free code can be difficult to puzzle out. In general prefer
explicit lambdas and function application.

``` hs
-- bad
((. f) . compare .)

-- good
\ x y -> compare (f x) (f y)
```

However sometimes eta expansion can make things harder to read. Keep things
point-free if it's easier to see the shape of an expression.

``` hs
-- bad
sum xs = foldr (\ x y -> x + y) 0 xs

-- good
sum = foldr (+) 0
```

<https://wiki.haskell.org/Pointfree>

## Avoid multiple function declarations

Multiple function declarations force you to repeat the function name and all
the arguments. Renaming any one of them requires changing every line. Also each
function declaration could use different argument names, which would be
confusing.

``` hs
-- bad
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- good
factorial n = case n of
  0 -> 1
  _ -> n * factorial (n - 1)
```

## Prefer `do` notation

`do` notation is generally easier to read because it's similar to imperative
code. It's also easier to modify, for example by adding logging. Compact
expressions using explicit monadic operators like `(>>=)` should be avoided.

``` hs
-- bad
getLine >>= print

-- good
do line <- getLine
   print line
```

## Avoid pure `do`

Normally `do` notation is used for monadic expressions. It is possible to use
it with pure expressions, but it's confusing.

``` hs
-- bad
double x = do
  let two = 2
  x * two

-- good
double x =
  let two = 2
  in x * two
```

## Avoid writing partial functions

Partial functions are difficult to work with because they can fail but their
type signatures don't express that. Avoid non-exhaustive pattern matching,
`undefined`, and `error`. Instead prefer types like `Maybe result` or
`Either failure success`.

``` hs
-- bad
first (x : _) = x

-- good
first xs = case xs of
  x : _ -> Just x
  _ -> Nothing
```

<https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html>

## Avoid using partial functions

Many libraries, including the standard library, come with partial functions
that throw exceptions at runtime. Wherever possible, avoid using these partial
functions and prefer their total (non-partial) versions instead.

``` hs
-- bad
head []

-- good
listToMaybe []
```

<https://begriffs.com/posts/2013-08-18-dont-be-partial-to-partial-functions.html>

## Group all imports together

Imports should not be split into groups.

``` hs
-- bad
import qualified Data.Aeson as Aeson

import qualified My.Internal.Secrets as Secrets

-- good
import qualified Data.Aeson as Aeson
import qualified My.Internal.Secrets as Secrets
```

## Avoid importing parents

A module like `A.B.C` shouldn't import anything from `A.B` or `A`. However it
is fine to import from `A.B.C.D` or `A.E`. This makes the module hierarchy
easier to understand.

## Prefer qualified imports

Qualifying imports makes it clear where things come from at the cost of making
things more verbose.

``` hs
-- bad
import Data.Aeson
import Data.Text (Text, pack, unpack)

-- good
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
```

When importing a module named `A.B.….Z`, generally prefer aliasing it as `Z`.

<https://www.parsonsmatt.org/2017/06/23/on_naming_things.html>

## Prefer unqualified operators

Qualified operators are visually noisy and can be hard to read. Import them
explicitly instead of using them from a qualified import.

``` hs
-- bad
import qualified Data.Aeson as Aeson
Aeson.object [ "successful" Aeson..= True ]

-- good
import Data.Aeson ((.=))
import qualified Data.Aeson
Aeson.object [ "successful" .= True ]
```

## Prefer functions over operators

When both a function and an operator are available, prefer the function over
the operator. Functions always follow the same rules, unlike operators which
force you to take precedence into consideration.

``` hs
-- bad
scooby & snack .~ True

-- good
set snack True scooby
```

## Prefer short identifiers

Exported identifiers only need to be unique in the module that they're defined
in. Modules should be used to separate namespaces.

``` hs
-- bad
data Company = Company { companyName :: Text }
data User = User { userName :: Text }

-- good
module Company where
  data Company = Company { name :: Text }
module User where
  data User = User { name :: Text }
```

Another way to say this is "design for qualified imports".

## Use camel case names

There's no good rationale for this, it's just convention. Avoid using
underscores to separate words in identifiers. Instead use upper case letters
for new words.

``` hs
-- bad
type LOUD_SNAKE = ()
quiet_snake = ()

-- good
type LoudSnake = ()
quietSnake = ()
```

In spite of this, some names like `IO` are written in all caps.

## Avoid primes in names

``` hs
-- bad
users' = filter isAdmin users

-- good
admins = filter isAdmin users

-- okay
users_ = filter isAdmin users
users2 = filter isAdmin users
```

Primes in names are confusing and should be avoided. Try to use descriptive
names instead. If there isn't a better name to use, adding an underscore or
number as a suffix is acceptable.

<http://elm-lang.org/blog/the-perfect-bug-report#less-syntax>

## Avoid multiple underscore suffixes

Using a single underscore is a fine way to distinguish values that would
otherwise have the same name. However if you find yourself having more than one
underscore, consider using a different suffix like a number instead.

``` hs
-- bad
users = ...
users_ = ...
users__ = ...

-- good
users1 = ...
users2 = ...
users3 = ...
```

## Avoid explicit export lists

Explicitly listing exports often causes a lot of busywork.

``` hs
-- bad
module Toppings ( pepperoni ) where

-- good
module Toppings where
```

## Avoid `String`

`String` is a linked list of characters, which is a remarkably bad data type
for almost all purposes. Whenever possible, prefer using `Text` instead.

``` hs
-- bad
"beans" :: String

-- good
"beans" :: Text
```

Sometimes `String` is convenient for a function argument when
`OverloadedStrings` is disabled.

<http://www.stephendiehl.com/posts/strings.html>

## Use `newtype` liberally

Type aliases (with `type`) don't add any type safety. Type wrappers (with
`newtype`) add type safety and don't have any runtime cost.

``` hs
-- bad
type Name = Text

-- good
newtype Name = MkName Text
```

<https://robots.thoughtbot.com/lessons-learned-avoiding-primitives-in-elm>

## Use smart constructors

Rather than trying to make everything correct by construction,
prefer simple `newtype` wrappers with smart constructors to check invariants.

``` hs
-- bad
data Email = Email
  { localPart :: Text
  , domain :: Text
  }

-- good
newtype Email = UnsafeEmail Text

fromText :: Text -> Maybe Email
fromText x = if isEmail x
  then Just $ UnsafeEmail x
  else Nothing

toText :: Email -> Text
toText (UnsafeEmail x) = x
```

<https://haskell-at-work.com/episodes/2018-02-26-validation-with-smart-constructors.html>

## Expose record constructors and fields

For records, hiding constructors and fields introduces too much boilerplate.
Unlike `newtype`s, you should simply export the constructors and fields.

``` hs
-- bad
data Person = MkPerson { name :: Text }
makePerson = MkPerson
getName = name
setName p n = p { name = n }

-- good
module Example where
data Person = MkPerson { name :: Text }
```

<https://www.yesodweb.com/book/settings-types>

## Use descriptive conversion names

For `newtype`s, using the name `unwrap` is fine. For other conversions,
consider using `fromX` or `toY` instead.

``` hs
-- bad
emailToText :: Email -> Text

-- good
Email.toText :: Email -> Text
Text.fromEmail :: Email -> Text

-- also good
newtype Name = MkName { unwrap :: String }
```

## Avoid list comprehensions

List comprehensions are cute, but they're not often used and can be difficult
to puzzle out. Prefer using regular functions or `do` notation instead.

``` hs
-- bad
[ x * 2 | x <- xs, x > 0 ]

-- good
map (* 2) (filter (> 0) xs)

-- okay
do
  x <- xs
  guard (x > 0)
  pure (x * 2)
```

## Prefer monads for building records

When building a record it is tempting to use the applicative operators `(<$>)`
and `(<*>)`. However they can pose a problem when the expression is polymorphic
because it's hard to tell if the right values are being put into the right
slots. Even though the monadic way of writing things is more verbose, it's a
lot harder to mess up.

``` hs
-- bad
instance FromJSON Episode where
  parseJSON = withObject "Episode" $ \ object -> Episode
    <$> object .: "title"
    <*> object .: "subtitle"

-- good
instance FromJSON Episode where
  parseJSON = withObject "Episode" $ \ object -> do
    title <- object .: "title"
    subtitle <- object .: "subtitle"
    pure Episode
      { episodeTitle = title
      , episodeSubtitle = subtitle
      }
```

## Avoid multi-layered nesting

When seeking values from a nested object, avoid creating nested case
statements. Instead seek to pull one value out at a time.

``` hs
-- bad
case foo of
  Just bar -> case Bar.maybeField bar of
    Just baz -> Baz.maybeField baz
    Nothing -> Nothing
  Nothing -> Nothing

-- good
maybeFieldBar <- case foo of
  Just bar -> Bar.maybeField bar
  Nothing -> Nothing
case maybeFieldBar of
  Just baz -> Baz.maybeField baz
  Nothing -> Nothing
```

## Derive at least `Eq` and `Show`

Both the `Eq` and `Show` type classes are frequently used for debugging. Be
sure to derive them for any custom types that you define.

``` hs
-- bad
data Switch
  = Off
  | On

-- good
data Switch
  = Off
  | On
  deriving (Eq, Show)
```

The only exception is for types that have sensitive information in them, like
passwords. In that case you may want no `Show` instance at all, or you may want
an instance that hides the information.

``` hs
newtype Password
  = MkPassword Text
  deriving Eq

instance Show Password where
  show = const "MkPassword \"REDACTED\""
```

## Avoid backtick operators

Turning a regular function into an infix operator using backticks is pretty
weird and easy to miss. Prefer calling functions the normal way.

```other
-- bad
apple `elem` fruits

-- good
elem apple fruits
```

## Avoid separate `let`s

``` hs
-- bad
do
  let x = 1
  let y = 2
  pure (x + y)

-- good
let
  x = 1
  y = 2
pure (x + y)
```

## Avoid mixing ADTs and records

In general a data type should either be an ADT like `data X = A Int | B String`
or a record like `data X = X { a :: Int, b :: String }`. Mixing both is a
recipe for disaster.

``` hs
-- bad
data T
  = C1 { f1 :: Int }
  | C2 { f2 :: Double }

-- good
data T
  = C1 T1
  | C2 T2

newtype T1
  = MkT1 Int

newtype T2
  = MkT2 Double
```

## Avoid excessive parentheses

When possible you should prefer using the `($)` operator to avoid parentheses.

``` hs
-- bad
g (f x)

-- good
g $ f x
```

## Prefer composition to application

Rather than have multiple `($)` operators in a row, change all but the last one
to `(.)`.

``` hs
-- bad
h $ g $ f x

-- good
h . g $ f x
```

## Avoid boolean blindness

Whenever possible you should define a custom type instead of using a `Bool`.

``` hs
-- bad
isActive :: Bool
isActive = someCondition

-- good
data State
  = Active
  | Inactive

state :: State
state = if someCondition
  then Active
  else Inactive
```

<https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/>

## Avoid big tuples

After about 3 or 4 elements, you should consider changing from a tuple to a
custom data type.

``` hs
-- bad
( 32, "Taylor", "Haskell" )

-- good
MkProgrammer
  { age = 32
  , name = "Taylor"
  , language = "Haskell"
  }
```

## Avoid duplicate guards

When writing declarations with guards, avoid re-stating the same bindings.

``` hs
-- bad
case f x of
  y | p y -> a
  y | q y -> b
  _ -> c

-- good
case f x of
  y | p y -> a
    | q y -> b
  _ -> c
```

## Prefer arbitrary precision numbers

Whenever possible you should prefer using arbitrary precision numbers.

``` hs
-- bad
1 :: Int

-- good
1 :: Integer
```

However it's important to keep in mind that many systems do not support
arbitrary precision. If you're working with a type that must be stored in a
database like PostgreSQL or sent over the wire to JavaScript, a limited
precision type is probably more appropriate.

## Prefer separating re-exports and new declarations

``` hs
-- bad
module A ( module Z, f ) where
import Z
f = undefined

-- good
module B ( f ) where
f = undefined

module A ( module Z, module B ) where
import Z
import B
```

If a module more or less re-exports other modules, that's all it should do. New
declarations should go into other modules that will then get pulled into the
re-exporter.

## Avoid `List`

Lists should only be used to model a stack or a stream (only iterated once). If
order is important, consider using a sequence (or array or vector) instead. If
order is not important, consider using a set instead.

``` hs
-- bad
numbers :: [Int]

-- good
import qualified Data.Sequence as Seq
numbers :: Seq.Seq Int

-- good
import qualified Data.Set as Set
numbers :: Set.Set Int
```

## Prefer line comments

Prefer line comments over block comments. Block comments can nest, which makes
them easy to unbalance, and commenting a region out with an editor keybinding
produces line comments anyway.

``` hs
-- bad
{- Convert a name into its slug. -}

-- good
-- Convert a name into its slug.
```

## Prefer ASCII

Generally prefer ASCII in source code, even inside of comments.

``` hs
-- bad
"fóo"

-- good
"f\xf3o"
```
