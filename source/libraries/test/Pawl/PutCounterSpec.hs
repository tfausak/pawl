{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Resolve's Effect.PutCountersFrom arm -- CR 122.8's putting
-- of the counters a permanent HAD onto a second permanent, which reads like the
-- CR 122.5 move Pawl.MoveCounterSpec covers and is not one.
module Pawl.PutCounterSpec where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

-- Iron Apprentice {1} Artifact Creature -- Construct 0/0 (Kamigawa: Neon
-- Dynasty; name, cost, type line, power, toughness and oracle text checked
-- against Scryfall 2026-08-30), data/cards/iron-apprentice.json:
--
--   This creature enters with a +1/+1 counter on it.
--   When this creature dies, if it had counters on it, put those counters on
--   target creature you control.
--
-- The second line is this module's subject, and the card is why the opcode
-- exists: "those counters" names neither a kind nor a count, so what crosses is
-- a whole per-kind tally, which Pawl.Types.PutCounters' one kind and one
-- Quantity cannot spell.
--
-- CR 122.8 is the rule, not CR 122.5: "the player doesn't move counters from one
-- object to the other. Rather, the player puts the same number of each kind of
-- counter the first object had onto the second object." CR 122.2 is why -- the
-- Apprentice's counters ceased to exist as it left the battlefield -- and rule
-- 122.5's fourth impossibility ("either object is no longer in the correct
-- zone") is what a move spelling would run into.
--
-- The tally is CR 608.2h last known information, and the Apprentice is the
-- ability's own SOURCE, so Pawl.Engine.Resolve's effectViewOf answers it through
-- Projection.viewWithLastKnown with no new binding: every printing of rule 122.8
-- that pawl can write today reads the source. Scryfall oracle:"put those
-- counters", 2026-08-30, over every printing ever released returns six cards --
-- Iron Apprentice, Scolding Administrator, Reluctant Role Model, Donatello
-- Mutant Mechanic, The Ozolith and Resourceful Defense -- of which the first two
-- are self-scoped and the rest read a departing BYSTANDER, which no reserved
-- slot names yet (#2694).
--
-- The entry line is Pawl.ReplacementSpec's business (CR 614.1c), and the boards
-- below stock the Apprentice's counters with S.addCounter instead: what this
-- module is about is what happens to a tally, not how one was built.
spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ putCountersFromSpec s registry

-- CR 603.3d's target, chosen as the trigger goes on the stack. FILTERS the
-- offered set rather than building a recipient, so CR 608.2b's re-read at
-- resolution still finds what was named -- Pawl.MoveCounterSpec's aimingTransfer
-- posture. alice controls a second Goblin Piker so the choice is a real one:
-- with one candidate the prompt short-circuits and the answer below would never
-- be consulted.
aiming :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aiming taker p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    fmap (\(_, offered) -> Set.filter ((==) (Just taker) . Recipient.objectOf) offered) asked
  _ -> S.identityAnswer p

-- The two kinds these boards use, read off one object: CR 122.1a's +1/+1 counter
-- and CR 122.1b's keyword counter. TWO, and in different counts, because "the
-- whole tally" and "the +1/+1 counters" are only different boards where the
-- object bears more than one kind.
pairOn :: ObjectId.ObjectId -> GameState.GameState -> Map.Map (CounterKind.CounterKind Keyword.Keyword) Integer
pairOn oid gs = fmap toInteger (maybe Map.empty Object.counters (Game.lookupObject oid gs))

-- What a creature IS, as the table sees it: CR 122.1a's power and toughness and
-- CR 122.1b's granted keyword, the two consequences the tally has once it lands.
bodyOf :: ObjectId.ObjectId -> GameState.GameState -> (Maybe Integer, Maybe Integer, Bool)
bodyOf oid gs = (Projection.powerOf oid gs, Projection.toughnessOf oid gs, Projection.hasKeyword Keyword.Flying oid gs)

putCountersFromSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
putCountersFromSpec s registry = Spec.describe s "CR 122.8 putting the counters a permanent had" $ do
  let -- alice: the Iron Apprentice, the Goblin Piker its trigger aims at, and a
      -- SECOND Goblin Piker so the target prompt has two candidates. bob holds a
      -- third, which "target creature you control" excludes -- so the offered set
      -- is wider than the legal one and wider still than the answer.
      --
      -- The trigger aims at the LATER of alice's two Pikers, so S.identityAnswer's
      -- least Recipient is the other one: a test that never reached `aiming` would
      -- grow the decoy instead, and the assertions below would say so.
      --
      -- `counters` is what a case puts on the Apprentice and is the ONLY
      -- difference between the boards below.
      board counters = do
        swamp <- S.printingOf s registry "Swamp"
        apprentice <- S.printingOf s registry "Iron Apprentice"
        piker <- S.printingOf s registry "Goblin Piker"
        let (apprenticeId, g1) = S.addCreature apprentice S.alice (S.landsInPlay swamp 1)
            (_, g2) = S.addCreature piker S.alice g1
            (takerId, g3) = S.addCreature piker S.alice g2
            (_, g4) = S.addCreature piker S.bob g3
            -- CR 104.3c: nothing here draws or advances a turn, and a stocked
            -- library keeps it that way if a later fixture change does.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g4 [1 .. 5 :: Int]
        pure (apprenticeId, takerId, counters apprenticeId stocked)
      -- CR 704.5g: the Apprentice is a 3/3 with its three +1/+1 counters, so
      -- three damage is lethal. Damage rather than a removal spell keeps this the
      -- NARROWEST path to the behaviour -- one trigger, one prompt, one
      -- resolution -- and a Murder would put a second Pool.Creatures target
      -- choice in front of the one this module is about.
      diesFrom damage (apprenticeId, takerId, before) =
        let dead = S.settleSba (if damage > 0 then S.markDamage apprenticeId damage before else before)
            settled = S.runPure (aiming takerId) dead Engine.settleForPriority
         in (settled, S.runPure (aiming takerId) settled Stack.resolveTop)
  -- THE CASE THIS UNIT EXISTS FOR. Two kinds in different counts, so "the whole
  -- tally" is a different board from "the +1/+1 counters" and from "one counter
  -- of each kind".
  Spec.it s "CR 122.8 every kind the Apprentice had lands on the target, in the counts it had" $ do
    built@(apprenticeId, takerId, before) <-
      board (\oid -> S.addCounter (CounterKind.Keyword Keyword.Flying) 2 oid . S.addCounter CounterKind.PlusOnePlusOne 3 oid)
    Spec.assertEqWith s "the Apprentice bears three +1/+1 counters and two flying counters" (pairOn apprenticeId before) (Map.fromList [(CounterKind.PlusOnePlusOne, 3), (CounterKind.Keyword Keyword.Flying, 2)])
    Spec.assertEqWith s "which CR 122.1a and CR 122.1b make a 3/3 with flying -- DIFFERENT facts from the tally" (bodyOf apprenticeId before) (Just 3, Just 3, True)
    Spec.assertEqWith s "alice's Piker is a plain 2/1 with no flying" (bodyOf takerId before) (Just 2, Just 1, False)
    let (settled, after) = diesFrom 3 built
    -- THE GAMEPLAY-LEVEL ASSERTION, and first: both kinds arrived in the counts
    -- the Apprentice had, so the Piker is a 5/4 (CR 122.1a) that flies (CR
    -- 122.1b). Either kind missing is a different triple.
    Spec.assertEqWith s "the Piker is a 5/4 with flying" (bodyOf takerId after) (Just 5, Just 4, True)
    Spec.assertEqWith s "which is the Apprentice's whole tally, kind for kind" (pairOn takerId after) (Map.fromList [(CounterKind.PlusOnePlusOne, 3), (CounterKind.Keyword Keyword.Flying, 2)])
    Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
    Spec.assertEqWith s "and the Apprentice is off the battlefield" (Set.member apprenticeId (GameState.battlefield after)) False
  -- The same board differing in exactly ONE thing: the flying counters are gone
  -- and nothing else moves. Without this pair the case above would pass on an
  -- implementation that put a flying counter of its own.
  Spec.it s "CR 122.8 a kind the Apprentice never had does not arrive" $ do
    built@(apprenticeId, takerId, before) <- board (S.addCounter CounterKind.PlusOnePlusOne 3)
    Spec.assertEqWith s "the Apprentice bears the +1/+1 counters alone" (pairOn apprenticeId before) (Map.singleton CounterKind.PlusOnePlusOne 3)
    let (_, after) = diesFrom 3 built
    Spec.assertEqWith s "the Piker is a 5/4 that does NOT fly" (bodyOf takerId after) (Just 5, Just 4, False)
    Spec.assertEqWith s "and holds only the kind the Apprentice had" (pairOn takerId after) (Map.singleton CounterKind.PlusOnePlusOne 3)
  -- CR 603.4's "otherwise it does nothing": an Apprentice that had no counters is
  -- a 0/0 and dies to CR 704.5f, and its own intervening "if" keeps the ability
  -- off the stack. A DIFFERENT death road from the pair above -- there is no
  -- board on which a counterless Apprentice survives to be damaged -- so this
  -- leg is stated as its own case rather than as the third of a triple.
  Spec.it s "CR 603.4 an Apprentice that had no counters puts none" $ do
    built@(apprenticeId, takerId, before) <- board (const id)
    Spec.assertEqWith s "no counters on it at all" (pairOn apprenticeId before) Map.empty
    let (settled, after) = diesFrom 0 built
    Spec.assertEqWith s "nothing reached the stack" (GameState.stack settled) []
    Spec.assertEqWith s "the Piker is the plain 2/1 it started as" (bodyOf takerId after) (Just 2, Just 1, False)
    Spec.assertEqWith s "and the Apprentice is off the battlefield just the same" (Set.member apprenticeId (GameState.battlefield after)) False
