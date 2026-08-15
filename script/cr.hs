#!/usr/bin/env cabal
{- cabal:
build-depends:
  base,
  exceptions,
ghc-options:
  -Weverything
  -Wno-all-missed-specialisations
  -Wno-implicit-prelude
  -Wno-missing-deriving-strategies
  -Wno-missing-kind-signatures
  -Wno-missing-safe-haskell-mode
  -Wno-prepositive-qualified-module
  -Wno-safe
-}

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Numeric.Natural as Natural
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.Read as Read

main :: IO ()
main = do
  arguments <- Environment.getArgs
  let (flgs, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute optDescrs arguments
  mapM_ (Exception.throwM . MkUnexpectedArgument) args
  mapM_ (Exception.throwM . MkUnknownOption) opts
  mapM_ (Exception.throwM . MkInvalidOption) errs
  cfg <- Monad.foldM applyFlag defaultConfig flgs

  Monad.when (configHelp cfg) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo name optDescrs
    Exit.exitSuccess

  rules <- readFile . Maybe.fromMaybe "docs/rules.txt" $ configRules cfg
  let defined = definedRules rules

  if configList cfg
    then mapM_ (putStrLn . runShowS . numberS . fst) defined
    else do
      let number = Maybe.fromMaybe defaultNumber $ configNumber cfg
      case fmap snd $ filter ((== number) . fst) defined of
        [x] -> putStrLn x
        [] -> Exception.throwM $ MkNoResults number
        _ -> Exception.throwM $ MkTooManyResults number

-- | Every rule the document defines, in document order, paired with the line
-- that defines it. This is the one place that decides what @docs/rules.txt@
-- means: a rule is a line leading with its own number, taken from the
-- numbered-rules body --- which runs from the @Credits@ entry that ends the
-- table of contents to the @Glossary@ heading that follows the last rule.
--
-- Both @--number@ and @--list@ read this, and @script/check-citations.sh@
-- consumes the @--list@ rendering rather than parsing the document a second
-- way, so a revision that reshapes the file cannot move one answer without
-- moving the other.
definedRules :: String -> [(Number, String)]
definedRules =
  Maybe.mapMaybe (\line -> fmap (\number -> (number, line)) $ runReadP ruleLineP line)
    . takeWhile (/= "Glossary")
    . dropWhile (/= "Credits")
    . lines

optDescrs :: [GetOpt.OptDescr Flag]
optDescrs =
  [ GetOpt.Option ['h'] ["help"] (GetOpt.NoArg FlagHelp) "Shows this help message, then exits.",
    GetOpt.Option ['l'] ["list"] (GetOpt.NoArg FlagList) "Lists every rule number the document defines, one per line, instead of looking one up.",
    GetOpt.Option ['n'] ["number"] (GetOpt.ReqArg FlagNumber "NUMBER") "The rule number to look up. Defaults to '1'.",
    GetOpt.Option ['r'] ["rules"] (GetOpt.ReqArg FlagRules "FILE") "The comprehensive rules plain text document to use. Defaults to 'docs/rules.txt'."
  ]

data Flag
  = FlagHelp
  | FlagList
  | FlagNumber String
  | FlagRules String
  deriving (Eq, Show)

data Config = MkConfig
  { configHelp :: Bool,
    configList :: Bool,
    configNumber :: Maybe Number,
    configRules :: Maybe FilePath
  }
  deriving (Eq, Show)

defaultConfig :: Config
defaultConfig =
  MkConfig
    { configHelp = False,
      configList = False,
      configNumber = Nothing,
      configRules = Nothing
    }

applyFlag :: (Exception.MonadThrow m) => Config -> Flag -> m Config
applyFlag cfg flg = case flg of
  FlagHelp -> pure cfg {configHelp = True}
  FlagList -> pure cfg {configList = True}
  FlagNumber s -> case runReadP numberP s of
    Nothing -> Exception.throwM $ MkInvalidNumber s
    Just n -> pure cfg {configNumber = Just n}
  FlagRules s -> pure cfg {configRules = Just s}

-- | A rule number, like @702.21a@. The parts are unbounded because rule numbers
-- are not: a fixed-width primary silently wrapped 612 onto 100 and printed the
-- wrong rule. The tertiary is a string because CR 704.5aa exists.
data Number = MkNumber
  { numberPrimary :: Natural.Natural,
    numberSecondary :: Maybe Natural.Natural,
    numberTertiary :: Maybe String
  }
  deriving (Eq, Show)

defaultNumber :: Number
defaultNumber =
  MkNumber
    { numberPrimary = 1,
      numberSecondary = Nothing,
      numberTertiary = Nothing
    }

runReadP :: P.ReadP a -> String -> Maybe a
runReadP p = fmap fst . List.find (null . snd) . P.readP_to_S p

numberP :: P.ReadP Number
numberP = do
  primary <- do
    digits <- P.munch1 Char.isDigit
    maybe P.pfail pure $ Read.readMaybe digits
  secondary <- P.option Nothing . fmap Just $ do
    Monad.void $ P.char '.'
    digits <- P.munch1 Char.isDigit
    maybe P.pfail pure $ Read.readMaybe digits
  tertiary <- P.option Nothing . fmap Just $ P.munch1 Char.isAsciiLower
  pure
    MkNumber
      { numberPrimary = primary,
        numberSecondary = secondary,
        numberTertiary = tertiary
      }

-- | Parses the rule number off the front of a line that defines a rule, like
-- @702.21a Ward is ...@ or @100.1. These Magic rules ...@, then a space.
--
-- The trailing period is optional in both directions, because the document is
-- not consistent about it: the convention is a period on a section or a
-- numbered rule and none on a subrule, but this revision writes @119.1d.@ with
-- one and @606.5@ without. Requiring the convention silently loses those two.
ruleLineP :: P.ReadP Number
ruleLineP = do
  number <- numberP
  Monad.void . P.option '.' $ P.char '.'
  Monad.void $ P.char ' '
  Monad.void . P.munch $ const True
  pure number

runShowS :: ShowS -> String
runShowS = ($ "")

numberS :: Number -> ShowS
numberS n =
  shows (numberPrimary n)
    . maybe id (\s -> showChar '.' . shows s) (numberSecondary n)
    . maybe id showString (numberTertiary n)

newtype UnexpectedArgument
  = MkUnexpectedArgument String
  deriving (Eq, Show)

instance Exception.Exception UnexpectedArgument

newtype UnknownOption
  = MkUnknownOption String
  deriving (Eq, Show)

instance Exception.Exception UnknownOption

newtype InvalidOption
  = MkInvalidOption String
  deriving (Eq, Show)

instance Exception.Exception InvalidOption

newtype InvalidNumber
  = MkInvalidNumber String
  deriving (Eq, Show)

instance Exception.Exception InvalidNumber

newtype NoResults
  = MkNoResults Number
  deriving (Eq, Show)

instance Exception.Exception NoResults

newtype TooManyResults
  = MkTooManyResults Number
  deriving (Eq, Show)

instance Exception.Exception TooManyResults
