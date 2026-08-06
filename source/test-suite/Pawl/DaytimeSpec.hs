{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Daytime (CR 731, "Day and Night", and CR 702.145's
-- daybound and nightbound), the CR 502.2 turn-based action Pawl.Engine.Engine
-- runs from it, the CR 117.5 settle arm beside it, Pawl.Types.Daytime, and
-- GameState's daytime and spellsCastLastTurn fields.
--
-- Gameplay-level throughout. Tovolar, Dire Overlord // Tovolar, the Midnight
-- Scourge is the whole fixture, and one card supplies every half of the rule: a
-- daybound front face (so a board with it on it becomes day, CR 702.145d), an
-- upkeep trigger that says "it becomes night" (CR 731.1), and a nightbound back
-- face for the night to turn it over onto (CR 702.145c). Russet Wolves is the
-- only other card here, and only to reach the trigger's "three or more Wolves
-- and/or Werewolves".
--
-- Two clauses of Tovolar's printed text are NOT modeled by the card file, and no
-- case here asserts on them: "Then transform any number of Human Werewolves you
-- control" needs a player-chosen subset of permanents, which pawl's ObjectRef
-- cannot express (#774), and "Whenever a Wolf or Werewolf you control deals
-- combat damage to a player, draw a card" needs a combat-damage trigger scoped
-- to a filter rather than to the bearer (#775).
module Pawl.DaytimeSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing

-- The face this permanent is showing, by name (CR 709.4a). What "transformed"
-- means to a reader: Object.face is the one field CR 701.27a writes, and every
-- characteristic follows it.
faceNameOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe CardName.CardName
faceNameOf oid gs = fmap Face.name (Game.faceOf oid gs)

frontName :: CardName.CardName
frontName = CardName.MkCardName (Text.pack "Tovolar, Dire Overlord")

backName :: CardName.CardName
backName = CardName.MkCardName (Text.pack "Tovolar, the Midnight Scourge")

-- CR 117.5's settle, which is where CR 702.145c/d/f/g are checked. Not
-- S.settleSba: that runs the state-based actions alone, and these rules are
-- explicitly not state-based actions.
settle :: GameState.GameState -> GameState.GameState
settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority

-- CR 502: the untap step's turn-based actions, which is where CR 502.2's
-- day/night check lives.
untapStep :: GameState.GameState -> GameState.GameState
untapStep gs = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))

-- Alice's board: Tovolar and `wolves` Russet Wolves, settled. Nothing has
-- happened yet, so the game still has neither designation (CR 731.1).
tovolarBoard :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
tovolarBoard tovolar wolf wolves =
  let (tovolarId, gs) = S.addCreature tovolar S.alice (Setup.emptyGame S.bothPlayers)
   in (tovolarId, foldr (\_ g -> snd (S.addCreature wolf S.alice g)) gs [1 .. wolves])

-- The upkeep step of alice's turn, ActivateSpec's augurUpkeep exactly: the
-- schedule loses its head so a runStep-driven case advances OUT of the upkeep
-- rather than back into it.
upkeep :: GameState.GameState -> GameState.GameState
upkeep gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.Beginning BeginningStep.Upkeep,
      GameState.priority = Just S.alice,
      GameState.remaining = Seq.drop 1 (GameState.remaining gs)
    }

-- How many spells the previous turn's active player cast during that turn, set
-- directly. Only the CR 502.2 cases use it, and what it stands in for is proved
-- through the rules by the handoff case at the foot of this module.
afterCasting :: Natural.Natural -> GameState.GameState -> GameState.GameState
afterCasting n gs = gs {GameState.spellsCastLastTurn = n}

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Daytime" $ do
  designationSpec s registry
  transformSpec s registry
  untapCheckSpec s registry

designationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
designationSpec s registry = Spec.describe s "Designation" $ do
  -- CR 731.1's first two sentences: the designation exists, and a game starts
  -- with neither. The falsifier for an engine that started every game at day.
  Spec.it s "CR 731.1 a game with no daybound permanent stays neither day nor night" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "neither, before anything settles" (GameState.daytime gs) Nothing
    Spec.assertEqWith s "and neither afterwards" (GameState.daytime (settle gs)) Nothing
  -- CR 702.145d: "any time a player controls a permanent with daybound, if it's
  -- neither day nor night, it becomes day." Controlling Tovolar is the whole
  -- input -- nothing is cast, nothing resolves.
  Spec.it s "CR 702.145d controlling a daybound permanent makes it day" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    let (tovolarId, gs) = tovolarBoard tovolar tovolar 0
        after = settle gs
    Spec.assertEqWith s "it is day" (GameState.daytime after) (Just Daytime.Day)
    -- CR 702.145c does NOT fire on it: the permanent is front face up and it is
    -- day, which is the designation daybound wants.
    Spec.assertEqWith s "and Tovolar is still front face up" (faceNameOf tovolarId after) (Just frontName)
    Spec.assertEqWith s "a 3/3" (S.powerToughnessOf tovolarId after) (Just (3, 3))

transformSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
transformSpec s registry = Spec.describe s "Transform" $ do
  -- The whole card, driven through the engine: alice's upkeep begins, CR 603.4's
  -- intervening "if" holds (Tovolar is himself a Werewolf, so two Wolves make
  -- three), the trigger resolves, CR 731.1 makes it night, and CR 702.145c turns
  -- the daybound permanent over as that happens.
  Spec.it s "CR 731.1/702.145c Tovolar's upkeep trigger makes it night and transforms him" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    wolf <- S.printingOf s registry "Russet Wolves"
    let (tovolarId, board) = tovolarBoard tovolar wolf 2
        started = settle board
        after = S.runPure S.identityAnswer (upkeep started) Engine.runStep
    Spec.assertEqWith s "the board was day before the upkeep" (GameState.daytime started) (Just Daytime.Day)
    Spec.assertEqWith s "and is night after it" (GameState.daytime after) (Just Daytime.Night)
    Spec.assertEqWith s "Tovolar shows his back face" (faceNameOf tovolarId after) (Just backName)
    Spec.assertEqWith s "which is a 4/4" (S.powerToughnessOf tovolarId after) (Just (4, 4))
  -- CR 603.4's intervening "if", with one Wolf too few: Tovolar plus one Russet
  -- Wolves is two, so nothing triggers and the designation does not move. The
  -- falsifier for a trigger that fired on the step alone.
  Spec.it s "CR 603.4 two Wolves are not three, so it stays day" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    wolf <- S.printingOf s registry "Russet Wolves"
    let (tovolarId, board) = tovolarBoard tovolar wolf 1
        after = S.runPure S.identityAnswer (upkeep (settle board)) Engine.runStep
    Spec.assertEqWith s "still day" (GameState.daytime after) (Just Daytime.Day)
    Spec.assertEqWith s "and still front face up" (faceNameOf tovolarId after) (Just frontName)

untapCheckSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
untapCheckSpec s registry = Spec.describe s "UntapCheck" $ do
  -- CR 502.2's first sentence: day, and the previous turn's active player cast
  -- no spells during it, so it becomes night -- and CR 702.145c turns Tovolar
  -- over as it does.
  Spec.it s "CR 502.2 day and no spells last turn becomes night" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    let (tovolarId, board) = tovolarBoard tovolar tovolar 0
        after = untapStep (afterCasting 0 (settle board))
    Spec.assertEqWith s "night" (GameState.daytime after) (Just Daytime.Night)
    Spec.assertEqWith s "and Tovolar turned over" (faceNameOf tovolarId after) (Just backName)
  -- The discriminator: one spell is not "didn't cast any spells", so the same
  -- step leaves it day. An implementation that changed the designation every
  -- untap step passes the case above and fails this one.
  Spec.it s "CR 502.2 one spell last turn keeps it day" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    let (tovolarId, board) = tovolarBoard tovolar tovolar 0
        after = untapStep (afterCasting 1 (settle board))
    Spec.assertEqWith s "still day" (GameState.daytime after) (Just Daytime.Day)
    Spec.assertEqWith s "and still front face up" (faceNameOf tovolarId after) (Just frontName)
  -- CR 502.2's second sentence, from a night reached the way the card reaches
  -- it: two spells cast during the previous turn make it day again, and CR
  -- 702.145f turns the now-nightbound permanent back.
  Spec.it s "CR 502.2/702.145f night and two spells last turn becomes day" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    wolf <- S.printingOf s registry "Russet Wolves"
    let (tovolarId, board) = tovolarBoard tovolar wolf 2
        night = S.runPure S.identityAnswer (upkeep (settle board)) Engine.runStep
        after = untapStep (afterCasting 2 night)
    Spec.assertEqWith s "it was night" (GameState.daytime night) (Just Daytime.Night)
    Spec.assertEqWith s "with Tovolar on his back face" (faceNameOf tovolarId night) (Just backName)
    Spec.assertEqWith s "and is day again" (GameState.daytime after) (Just Daytime.Day)
    Spec.assertEqWith s "with Tovolar back on his front face" (faceNameOf tovolarId after) (Just frontName)
  -- The other half's discriminator: ONE spell is not "two or more", so a night
  -- reached the same way survives the untap step. The falsifier for a check that
  -- flipped on any spell at all.
  Spec.it s "CR 502.2 night and only one spell last turn stays night" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    wolf <- S.printingOf s registry "Russet Wolves"
    let (tovolarId, board) = tovolarBoard tovolar wolf 2
        night = S.runPure S.identityAnswer (upkeep (settle board)) Engine.runStep
        after = untapStep (afterCasting 1 night)
    Spec.assertEqWith s "still night" (GameState.daytime after) (Just Daytime.Night)
    Spec.assertEqWith s "and Tovolar is still on his back face" (faceNameOf tovolarId after) (Just backName)
  -- CR 502.2's third sentence: "if it's neither day nor night, this check doesn't
  -- happen and it remains neither". A board with no daybound permanent never
  -- gained a designation, and the untap step does not give it one.
  Spec.it s "CR 502.2 neither day nor night: the check does not happen" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        after = untapStep (afterCasting 0 (settle board))
    Spec.assertEqWith s "still neither" (GameState.daytime after) Nothing
  -- CR 731.2's input, taken through the rules rather than written: alice casts a
  -- Lightning Bolt, the turn is handed on, and the count the next untap step will
  -- read is the one her turn ended with. The event log it is folded from is
  -- cleared by that same handoff, which is why the snapshot exists at all.
  Spec.it s "CR 731.2 the handoff records what the previous turn's active player cast" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, spellId) = S.handOne lightningBolt (S.landsInPlay mountain 1)
        cast = S.runPure S.castAnswer gs (S.cast S.alice spellId)
        handed = Engine.beginTurnOf S.bob cast
    Spec.assertEqWith s "nothing was on the books before the turn ended" (GameState.spellsCastLastTurn gs) 0
    Spec.assertEqWith s "one spell rode across the handoff" (GameState.spellsCastLastTurn handed) 1
    Spec.assertEqWith s "and the log it was folded from is gone" (Seq.null (GameState.events handed)) True
