-- Covers Pawl.Engine.Condition, Pawl.Types.Condition and Pawl.Types.Comparison,
-- including what Condition.holds makes of Pawl.Engine.Quantity's IsMonarch.
module Pawl.ConditionSpec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Count every battlefield object; the stub view decides how many match.
everyPermanent :: Count.Type.Count Quantity.Type.Quantity
everyPermanent =
  Count.Type.MkCount
    (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
    (Filter.Type.And [])
    Aggregation.Members

-- n Swamps on the battlefield, and a ViewOf (via S.stubView, Pawl.Support's
-- second consumer per Task 3) describing each of them.
boardOf :: Printing.Printing -> Integer -> (Count.ViewOf, GameState.GameState)
boardOf swamp n =
  let gs0 = Setup.emptyGame S.bothPlayers
      step (ids, g) _ =
        let (oid, g') = S.addCreature swamp S.alice g
         in (ids <> [oid], g')
      (oids, gs) = foldl step ([], gs0) [1 .. n]
      table = fmap (\oid -> (oid, Set.empty, Set.singleton Subtype.Swamp, Just S.alice)) oids
   in (S.stubView table, gs)

context :: Filter.Context
context = Filter.contextFor (Just S.alice) (Just (ObjectId.MkObjectId 0))

check :: Printing.Printing -> Integer -> Comparison.Comparison -> Integer -> Bool
check swamp n comparison threshold =
  let (viewOf, gs) = boardOf swamp n
   in Condition.holds
        viewOf
        context
        gs
        (ObjectId.MkObjectId 0)
        (Condition.Type.Compares (Quantity.Type.Count everyPermanent) comparison (Quantity.Type.Literal threshold))

-- Queen Marchesa's upkeep trigger: "if an opponent is the monarch" is
-- Quantity.IsMonarch (Relative Opponent), which names EVERY opponent -- one
-- player on two seats, two on three. CR 725.3 makes the monarch unique, so the
-- honest reading is a disjunction over them.
monarchSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
monarchSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan upkeep S.alice) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- Alice's upkeep with Queen Marchesa already out, on `seats`, after
      -- `crown` has settled the monarchy.
      upkeepWith marchesa seats crown =
        let (_, gs0) = S.addCreature marchesa S.alice (Setup.emptyGame seats)
         in resolveAll (settle (beginUpkeep (crown gs0)))
      noToken after = Spec.assertEqWith s "no token was created" (S.tokensOf after) []
      oneAssassin after = case S.tokensOf after of
        [tok] -> do
          Spec.assertEqWith s "1/1" (Projection.powerOf tok after, Projection.toughnessOf tok after) (Just 1, Just 1)
          Spec.assertEqWith s "black" (Projection.colorsOf tok after) (Set.singleton Color.Black)
          Spec.assertBool s (Set.member Subtype.Assassin (Projection.subtypesOf tok after)) "an Assassin"
          Spec.assertEqWith s "deathtouch and haste" (Map.keysSet (Projection.keywordsOf tok after)) (Set.fromList [Keyword.Deathtouch, Keyword.Haste])
          Spec.assertEqWith s "alice's" (Projection.controllerOf tok after) (Just S.alice)
        other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
   in Spec.describe s "IsMonarch" $ do
        -- Three seats: Relative Opponent names bob AND carol, which is the whole
        -- bug. On two seats the pre-change code already passed.
        Spec.it s "CR 725.3 an opponent is the monarch on three seats" $ do
          marchesa <- S.printingOf s registry "Queen Marchesa"
          oneAssassin (upkeepWith marchesa S.threePlayers (S.withMonarch S.carol))
        -- The same board with the crown moved to alice: this is what says the
        -- disjunction did not degenerate into "is there a monarch?".
        Spec.it s "CR 603.4 the controller holding the crown makes the clause false" $ do
          marchesa <- S.printingOf s registry "Queen Marchesa"
          noToken (upkeepWith marchesa S.threePlayers (S.withMonarch S.alice))
        -- Regression fence for the existing CR 725.5 arm: no monarch answers 0,
        -- not "undeterminable".
        Spec.it s "CR 725.5 no monarch at all makes the clause false" $ do
          marchesa <- S.printingOf s registry "Queen Marchesa"
          noToken (upkeepWith marchesa S.threePlayers id)
        -- Two seats, where the old arity restriction already answered: the fix
        -- must not move this.
        Spec.it s "CR 725.3 the two-seat answer is unchanged" $ do
          marchesa <- S.printingOf s registry "Queen Marchesa"
          oneAssassin (upkeepWith marchesa S.bothPlayers (S.withMonarch S.bob))
        -- The whole card, crowned by its own resolved ETB rather than by a
        -- fixture write.
        Spec.it s "CR 725.1 her enters trigger crowns her controller, so no token follows" $ do
          marchesa <- S.printingOf s registry "Queen Marchesa"
          let (oid, gs0) = S.addCreature marchesa S.alice (Setup.emptyGame S.threePlayers)
              entered = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
              crowned = resolveAll (settle (S.withEvents [GameEvent.Moved entered (Projection.project oid gs0)] gs0))
          Spec.assertEqWith s "alice is the monarch" (GameState.monarch crowned) (Just S.alice)
          noToken (resolveAll (settle (beginUpkeep crowned)))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Condition" $ do
  Spec.describe s "Exactly" $ do
    Spec.it s "CR 603.8 holds when the count equals the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (check swamp 0 Comparison.Exactly 0) "0 == 0"

    Spec.it s "CR 603.8 fails when the count differs" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (not (check swamp 1 Comparison.Exactly 0)) "1 /= 0"

  Spec.describe s "AtLeast" $ do
    Spec.it s "holds at the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (check swamp 3 Comparison.AtLeast 3) "3 >= 3"

    Spec.it s "fails below the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (not (check swamp 2 Comparison.AtLeast 3)) "2 < 3"

  Spec.describe s "AtMost" $ do
    Spec.it s "holds below the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (check swamp 0 Comparison.AtMost 1) "0 <= 1"

    Spec.it s "holds at the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (check swamp 1 Comparison.AtMost 1) "1 <= 1"

    Spec.it s "fails above the threshold" $ do
      swamp <- S.printingOf s registry "Swamp"
      Spec.assertBool s (not (check swamp 2 Comparison.AtMost 1)) "2 > 1"

  Spec.describe s "an undeterminable side is false, never true" $ do
    Spec.it s "when the MEASURED side cannot be evaluated" $ do
      -- Relative with no perspective: Count.evaluate is Nothing, and a
      -- total holds must collapse that to False (CR 611.2b's conservative
      -- reading), not to a vacuous True.
      swamp <- S.printingOf s registry "Swamp"
      let (viewOf, gs) = boardOf swamp 0
          count =
            Count.Type.MkCount
              (Scope.InZone Zone.Hand (PlayerRef.Relative PlayerRelation.You))
              (Filter.Type.And [])
              Aggregation.Members
      Spec.assertBool
        s
        ( not $
            Condition.holds
              viewOf
              (Filter.contextFor Nothing Nothing)
              gs
              (ObjectId.MkObjectId 0)
              (Condition.Type.Compares (Quantity.Type.Count count) Comparison.Exactly (Quantity.Type.Literal 0))
        )
        "false"

    Spec.it s "when the THRESHOLD side cannot be evaluated" $ do
      -- Quantity.X with no binding on the object: same collapse.
      swamp <- S.printingOf s registry "Swamp"
      let (viewOf, gs) = boardOf swamp 0
      Spec.assertBool
        s
        ( not $
            Condition.holds
              viewOf
              context
              gs
              (ObjectId.MkObjectId 0)
              (Condition.Type.Compares (Quantity.Type.Count everyPermanent) Comparison.Exactly (Quantity.Type.InSlot Binding.variableX))
        )
        "false"

  monarchSpec s registry
