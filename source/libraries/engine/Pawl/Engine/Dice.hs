-- CR 706.6's ignored roll: "if a player is instructed to ignore a roll, that roll
-- is considered to have never happened". Rule 705's Pawl.Engine.Coin one rule
-- over, and the same division of labour -- the replaceable event itself is a
-- funnel in Pawl.Engine.Event (proposeDiceRoll), because CR 614's loop lives
-- there, and what is left of rule 706 that no game state is needed for lives
-- here.
module Pawl.Engine.Dice where

import qualified Data.List as List
import qualified Numeric.Natural as Natural

-- | CR 706.6: throw away the `n` lowest of these rolls, so that what comes back
-- is the rolls the instruction may still read. Rule 706.6 makes an ignored roll
-- one that HAPPENED -- the die was thrown and its face asked for -- and is then
-- treated as never having happened, so this runs on the results rather than on
-- the count of dice, and the caller has already asked for every face.
--
-- ONE AT A TIME, re-finding the minimum after each, because CR 706.6's sentence
-- is "the lowest roll" singular and two rows each ignore their own: over [1,1,5]
-- a single ignore leaves [1,5] and a second leaves [5], where a pass that took
-- every copy of the minimum at once would leave [5] for both.
--
-- A TIE is not asked about. Rule 706.6's second sentence gives the roller the
-- choice "if multiple results are tied for the lowest", and the tied rolls are
-- equal NUMBERS -- so dropping either leaves the same multiset of results for the
-- instruction to read, and no board can tell the answers apart. Where the rules
-- leave nothing to ask, don't prompt (Pawl.Engine.Event.flipOneCoin's agreed
-- faces, and for its reason). The first is taken, so a transcript replays.
--
-- Ignoring more rolls than were made leaves none rather than failing; the
-- instruction then binds nothing and reads an unbound slot. Unreachable from card
-- data -- every ignore comes from a row that added the die it ignores -- and so
-- defensive.
ignoreLowest :: Natural.Natural -> [Natural.Natural] -> [Natural.Natural]
ignoreLowest n rolls =
  if n == 0
    then rolls
    else case rolls of
      [] -> []
      _ -> ignoreLowest (n - 1) (dropOneLowest rolls)

-- | CR 706.6's singular "the lowest roll": the FIRST roll holding the least
-- number, dropped. Total on the empty list, which `ignoreLowest` above never
-- reaches it with.
dropOneLowest :: [Natural.Natural] -> [Natural.Natural]
dropOneLowest rolls = case rolls of
  [] -> []
  _ -> case List.elemIndex (minimum rolls) rolls of
    -- Unreachable: the minimum of a non-empty list is one of its members.
    -- Defensive, in the direction rule 706.6 leaves standing -- an ignore that
    -- found nothing to ignore leaves every roll readable.
    Nothing -> rolls
    Just i -> let (before, after) = splitAt i rolls in before <> drop 1 after
