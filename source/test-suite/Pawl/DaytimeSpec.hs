{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Daytime (CR 731, "Day and Night", and CR 702.145's
-- daybound and nightbound), the CR 502.2 turn-based action Pawl.Engine.Engine
-- runs from it, the CR 117.5 settle arm beside it, Pawl.Types.Daytime, and
-- GameState's daytime and spellsCastLastTurn fields.
--
-- Also CR 702.145b's FIRST static ability -- "if it is night and this permanent
-- is represented by a double-faced card, it enters transformed" -- which is CR
-- 712.13a's replacement effect, Pawl.Types.EntryRewrite's EntersTransformed,
-- minted by Pawl.Engine.Keyword.mintedReplacementsFor and applied by
-- Pawl.Engine.Event's arm under CR 616.1d's bucket. See entrySpec, whose fixture
-- is Infestation Expert // Infested Werewolf.
--
-- Also CR 702.145b's third static ability and CR 702.145e's second -- "this
-- permanent can't transform except due to its daybound/nightbound ability" --
-- which is Pawl.Engine.Daytime.restrictsTransform read by Pawl.Engine.Resolve's
-- `turnOver`. See restrictionSpec.
--
-- Gameplay-level throughout. Tovolar, Dire Overlord // Tovolar, the Midnight
-- Scourge is the whole fixture, and one card supplies every half of the rule: a
-- daybound front face (so a board with it on it becomes day, CR 702.145d), an
-- upkeep trigger that says "it becomes night" (CR 731.1), and a nightbound back
-- face for the night to turn it over onto (CR 702.145c). Russet Wolves is the
-- only other card here, and only to reach the trigger's "three or more Wolves
-- and/or Werewolves"; restrictionSpec adds Moonmist, Forest and Humility.
--
-- One clause of Tovolar's printed text is NOT modeled by the card file, and no
-- case here asserts on it: "Then transform any number of Human Werewolves you
-- control" needs a player-chosen subset of permanents, which pawl's ObjectRef
-- cannot express (#774). His combat-damage trigger IS modeled, and is read by
-- Pawl.TriggerSpec's `tovolarSpec` rather than here.
--
-- Moonmist's second sentence is not modeled by its card file either, and no case
-- here asserts on it: "prevent all combat damage that would be dealt this turn by
-- creatures other than Werewolves and Wolves" names its sources by
-- characteristic, and prevention can name a source only by identity (#588).
module Pawl.DaytimeSpec where

import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

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

-- alice controls Tovolar and two Forests, with Moonmist in her hand, settled.
-- `extra` is whatever the case adds to the board before that settle, which is
-- what decides the designation: with nothing added, CR 702.145d makes it day and
-- CR 702.145c then has nothing to do, and under Humility there is no daybound
-- permanent to make it anything, so it stays neither. Each case asserts the
-- designation it depends on rather than trusting this.
--
-- Two Forests and no more: Moonmist costs {1}{G}, and every land here is
-- untapped, so the cast can be paid without a decision.
--
-- ONE Human on the board, deliberately. Moonmist is targetless and board-wide,
-- so a second Human double-faced permanent would make the two cases below read
-- the same sweep rather than independent ones.
moonmistBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState -> GameState.GameState) ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
moonmistBoard tovolar forest moonmist extra =
  let (tovolarId, withTovolar) = S.addCreature tovolar S.alice (S.landsInPlay forest 2)
      (gs, spellId) = S.handOne moonmist (extra withTovolar)
   in (tovolarId, spellId, settle gs)

-- alice casts Moonmist and it resolves. Nothing advances a turn, so CR 104.3c
-- never comes up and Tovolar's upkeep trigger never gets a step to fire in.
resolveMoonmist :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
resolveMoonmist spellId gs =
  let cast = S.runPure S.castAnswer gs (S.cast S.alice spellId)
   in S.runPure S.castAnswer cast Stack.resolveTop

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Daytime" $ do
  designationSpec s registry
  transformSpec s registry
  restrictionSpec s registry
  untapCheckSpec s registry
  entrySpec s registry

expertFront :: CardName.CardName
expertFront = CardName.MkCardName (Text.pack "Infestation Expert")

expertBack :: CardName.CardName
expertBack = CardName.MkCardName (Text.pack "Infested Werewolf")

insectToken :: CardName.CardName
insectToken = CardName.MkCardName (Text.pack "Insect Token")

-- Every face alice's battlefield shows that belongs to Infestation Expert, found
-- by name rather than by an ObjectId: CR 400.7 makes the resolving spell a NEW
-- object, so the id the cast returned is not the permanent's.
--
-- A LIST rather than the first match, so a board that somehow grew two of them
-- fails loudly instead of answering about one.
expertFaces :: GameState.GameState -> [CardName.CardName]
expertFaces gs =
  [ name
  | oid <- Game.zoneMembers Zone.Battlefield S.alice gs,
    name <- Maybe.maybeToList (faceNameOf oid gs),
    name == expertFront || name == expertBack
  ]

-- alice controls Tovolar and five Forests, with Infestation Expert in hand.
-- `spells` is what the previous turn's active player cast, which is the whole of
-- what decides the designation the spell will enter under: the settle makes it
-- day (CR 702.145d), and CR 502.2's untap check then turns it to night on
-- nothing, or leaves it day on one.
--
-- S.handOne leaves the board in a main phase with alice holding priority, and
-- `untapStep` takes the phase it runs CR 502.2 for as an argument rather than
-- moving the board to it, so that stands. Nothing here is about timing anyway:
-- S.cast drives Pawl.Engine.Cast directly.
--
-- Tovolar is on the board only to give it a designation at all -- CR 702.145d
-- needs a permanent with daybound -- and five Forests is exactly Infestation
-- Expert's {4}{G}, so the cast can be paid without a decision.
expertBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Natural.Natural -> (ObjectId.ObjectId, GameState.GameState)
expertBoard tovolar forest expert spells =
  let (_, withTovolar) = S.addCreature tovolar S.alice (S.landsInPlay forest 5)
      (gs, spellId) = S.handOne expert withTovolar
   in (spellId, untapStep (afterCasting spells (settle gs)))

-- alice casts Infestation Expert and it resolves. Answered in two steps because
-- the assertion between them is the point: the permanent is on the battlefield
-- and its enters-the-battlefield trigger has not resolved yet.
castExpert :: ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, GameState.GameState)
castExpert spellId gs =
  let cast = S.runPure S.castAnswer gs (S.cast S.alice spellId)
      entered = S.runPure S.castAnswer cast Stack.resolveTop
      triggered = S.runPure S.castAnswer (settle entered) Stack.resolveTop
   in (entered, triggered)

entrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entrySpec s registry = Spec.describe s "EntersTransformed" $ do
  -- CR 712.13a through CR 702.145b's FIRST static ability: "if it is night and
  -- this permanent is represented by a double-faced card, it enters transformed."
  -- Infestation Expert is cast with its front face up (CR 712.11) and reaches the
  -- battlefield showing its back face.
  --
  -- THE FACE IS THE DISCRIMINATOR, and it is asserted at a moment that tells the
  -- competing readings apart. Three of them put a permanent on this board showing
  -- Infested Werewolf:
  --
  --   * it entered transformed -- CR 712.13a, the rule;
  --   * it entered front face up and the CR 702.145c sweep turned it over
  --     (Pawl.Engine.Daytime.turnDue, which is what this engine did before);
  --   * it was never front face up at all, a back-face cast (CR 712.11a) -- which
  --     this board rules out by casting the card from hand, where CR 712.8a shows
  --     only the front face.
  --
  -- The first two are separated by asserting on `entered`, the board the spell's
  -- resolution leaves, which is BEFORE the settle the sweep runs in.
  --
  -- THE TOKEN COUNT IS A FENCE, NOT A PROOF, and the reason is worth stating: the
  -- trigger scan reads the permanent's abilities off the live board at settle,
  -- and Daytime.settle runs earlier in that same settle, so the swept engine
  -- places the back face's trigger too (#1548). Two Insects here says the back
  -- face's ability is the live one; it does not say the permanent entered that
  -- way.
  Spec.it s "CR 712.13a/702.145b a daybound spell cast at night enters transformed" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    forest <- S.printingOf s registry "Forest"
    expert <- S.printingOf s registry "Infestation Expert"
    let (spellId, board) = expertBoard tovolar forest expert 0
        (entered, triggered) = castExpert spellId board
    Spec.assertEqWith s "it is night when the spell resolves" (GameState.daytime board) (Just Daytime.Night)
    Spec.assertEqWith s "the permanent is showing its back face already" (expertFaces entered) [expertBack]
    Spec.assertEqWith s "and its back face's trigger made two Insects" (S.countOnBattlefieldByName insectToken S.alice triggered) 2
  -- The negative, the same board with ONE spell cast during the previous turn
  -- instead of none: CR 502.2 leaves it day, CR 702.145b's condition fails, and
  -- the permanent enters front face up and stays there. The falsifier for a
  -- rewrite that applied on every entry.
  Spec.it s "CR 702.145b by day the same spell enters front face up" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    forest <- S.printingOf s registry "Forest"
    expert <- S.printingOf s registry "Infestation Expert"
    let (spellId, board) = expertBoard tovolar forest expert 1
        (entered, triggered) = castExpert spellId board
    Spec.assertEqWith s "it is day when the spell resolves" (GameState.daytime board) (Just Daytime.Day)
    Spec.assertEqWith s "the permanent is showing its front face" (expertFaces entered) [expertFront]
    Spec.assertEqWith s "and its front face's trigger made one Insect" (S.countOnBattlefieldByName insectToken S.alice triggered) 1

restrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
restrictionSpec s registry = Spec.describe s "TransformRestriction" $ do
  -- CR 702.145b's third static ability: "this permanent can't transform except
  -- due to its daybound ability." Moonmist says "transform all Humans" and
  -- Tovolar, Dire Overlord is a Human Werewolf, so the sweep names him -- and
  -- the rule refuses the turn anyway.
  --
  -- The face AND the projected name are asserted, not the power and toughness
  -- alone: 3/3 against 4/4 is a two-value difference an unrelated bug could
  -- reproduce, while Object.face and Projection.namesOf disagreeing about which
  -- face is up cannot be anything else. The P/T rides along as the reader's
  -- half.
  Spec.it s "CR 702.145b a daybound permanent refuses a spell's transform" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    forest <- S.printingOf s registry "Forest"
    moonmist <- S.printingOf s registry "Moonmist"
    let (tovolarId, spellId, board) = moonmistBoard tovolar forest moonmist id
        after = resolveMoonmist spellId board
    Spec.assertEqWith s "it was day before the spell" (GameState.daytime board) (Just Daytime.Day)
    Spec.assertEqWith s "and still is" (GameState.daytime after) (Just Daytime.Day)
    Spec.assertEqWith s "Tovolar is still front face up" (faceNameOf tovolarId after) (Just frontName)
    Spec.assertEqWith s "every reader still sees the front face's name" (Projection.namesOf tovolarId after) (Set.singleton frontName)
    Spec.assertEqWith s "a 3/3" (S.powerToughnessOf tovolarId after) (Just (3, 3))
  -- The positive control, and the reason the case above is not vacuous: the same
  -- board with Humility on it. CR 613.1f strips the daybound ability at layer 6
  -- and touches no subtype, so this permanent is still a Human that Moonmist's
  -- sweep names -- and with the ability gone, so is the restriction the ability
  -- carried. It transforms.
  --
  -- So this proves three things at once: the filter matches, the instruction
  -- reaches Pawl.Engine.Game.turnFaceOver, and the keyword is read off the
  -- PROJECTION rather than off the printed face. A gate reading printed keywords
  -- passes the case above and fails this one.
  --
  -- The face and the name, never the power and toughness: under Humility this
  -- permanent is 1/1 whichever face is up, so a P/T assertion here would prove
  -- nothing.
  Spec.it s "CR 613.1f Humility strips daybound, so the restriction goes with it" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    forest <- S.printingOf s registry "Forest"
    moonmist <- S.printingOf s registry "Moonmist"
    humility <- S.printingOf s registry "Humility"
    let (tovolarId, spellId, board) = moonmistBoard tovolar forest moonmist (S.withHumility humility)
        after = resolveMoonmist spellId board
    Spec.assertEqWith s "he is still a Human, so the sweep names him" (Set.member Subtype.Human (Projection.subtypesOf tovolarId board)) True
    Spec.assertEqWith s "and has no daybound left to forbid it" (Projection.hasKeyword Keyword.Daybound tovolarId board) False
    -- CR 702.145d needs "a permanent with daybound", and the projection has
    -- none, so the settle never gave this board a designation -- which is also
    -- why nothing here could have been turned over by CR 702.145c instead.
    Spec.assertEqWith s "so it is neither day nor night" (GameState.daytime board) Nothing
    Spec.assertEqWith s "so Moonmist turns him over" (faceNameOf tovolarId after) (Just backName)
    Spec.assertEqWith s "and every reader sees the back face's name" (Projection.namesOf tovolarId after) (Set.singleton backName)

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
