-- CR 706 past the roll itself: rule 706.6's ignored roll, and rule 706.2's
-- modifiers that reach a roll from a source other than its own instruction.
-- Rule 705's Pawl.Engine.Coin one rule over, and the same division of labour --
-- the replaceable event itself is a funnel in Pawl.Engine.Event
-- (proposeDiceRoll), because CR 614's loop lives there, and what is left of rule
-- 706 lives here.
module Pawl.Engine.Dice where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import Pawl.Types.Game (Game)
import qualified Pawl.Types.ModifiedRoll as ModifiedRoll
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.RollModifier as RollModifier

-- | The CR 706.2 modifiers in force over the rolls `pid` is making, read fresh
-- for EACH die rather than once for the instruction: rule 706.2 words a modifier
-- against "the roll" singular, and a reroll of the first die can move the board
-- the second die's offer is read off.
--
-- Pawl.Engine.Coin.statementsFor's sibling, and deliberately NOT its
-- once-per-instruction posture: that one is Edgar, King of Figaro's printed
-- "the first time you flip one or more coins each turn", plain card text with no
-- rule behind it, and no printing of a die-roll modifier says anything like it.
modifiersFor :: PlayerId -> Game [ModifiedRoll.ModifiedRoll]
modifiersFor pid = fmap (PlayerEffect.rollModifiers pid) State.get

-- | CR 706.2b's first step: does anything in force offer a REROLL of a die of
-- `sides` that came up the natural result `natural`?
--
-- The NATURAL result, because rule 706.2b considers rerolls before any increase
-- or decrease -- so nothing has moved the number when this is asked, and an
-- implementation that read the instruction's own modifier in first would gate
-- Clam-I-Am's "if you roll a 3" on a number no die showed.
--
-- A Bool and not the matching modifiers, because CR 706.2a's offer is the same
-- offer however many sources make it: two Clam-I-Ams give the roller one reroll
-- to accept or decline, and accepting it throws one die. Rule 706.2b's pick
-- among COMPETING modifiers is what a second bucket would need (#3976), and
-- there is only one bucket.
--
-- An unstated `sides` or `natural` matches every roll -- Wall of Fortune's bare
-- "a die" -- rather than none.
offersReroll :: Natural.Natural -> Natural.Natural -> [ModifiedRoll.ModifiedRoll] -> Bool
offersReroll sides natural modifiers =
  let matches modifier =
        ModifiedRoll.modifier modifier == RollModifier.Reroll
          && all (== sides) (ModifiedRoll.sides modifier)
          && all (== natural) (ModifiedRoll.natural modifier)
   in any matches modifiers

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
