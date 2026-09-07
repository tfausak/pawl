{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 722 preparation cards end to end: Pawl.Types.Layout's Preparation
-- arm and the Pawl.Engine.Card functions that read it (CR 722.4's normal view
-- through `combined`, CR 722.3's offer through `castableFaces`, CR 722.2a's inset
-- frame through `prepareFace`), Pawl.Types.Designation's Prepared arm with CR
-- 722.3a's two gates on gaining it, Pawl.Engine.Prepare's mint of CR 722.3c's
-- copy into exile and its residency condition, Source.OfCardCopy and the CR
-- 704.5e sweep Pawl.Engine.Sba makes of it, the casting permission
-- Pawl.Engine.Cast.permitsCastPrepared reads, and CR 601.2i's unpreparing at the
-- moment the copy becomes cast.
--
-- Every case runs against the printed Encouraging Aviator // Jump: a {2}{U} 2/3
-- Bird Wizard with flying and "whenever this creature attacks, it becomes
-- prepared", whose prepare spell is the {U} instant Jump, "target creature gains
-- flying until end of turn". It was picked because it is the cheapest printing
-- whose prepare spell isolates the machinery: the copy's whole observable effect
-- is one keyword on one other creature, so an assertion about the copy cannot be
-- confused with an assertion about a board sweep.
--
-- Goblin Piker is on every board as Jump's target and never gains flying by any
-- other road, so "the Piker is flying" means the copy resolved. Aurelia, the
-- Warleader supplies CR 500.8's additional combat phase, which is how the
-- already-prepared case gets a SECOND attack out of one turn. Clone with
-- Concordant Crossroads is CR 722.2b's board -- a permanent that has a prepare
-- spell because of what it COPIED and not because of what is printed under it,
-- with the haste that lets it attack the turn it arrives -- and Reality Ripple is
-- CR 702.26b's.
--
-- Not implemented: CR 722.3c's "or phases in prepared" branch, so a permanent
-- that phases out prepared and back in mints no second copy, and CR 722.3d's "if
-- a prepare spell is copied, the copy is also a prepare spell" -- both #868's,
-- which this slice narrows rather than closes. CR 722.5's alternative name is
-- #679's.
module Pawl.PreparationSpec where

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Phasing as Phasing
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Zone as Zone

aviatorName, jumpName, cloneName :: CardName.CardName
aviatorName = CardName.MkCardName (Text.pack "Encouraging Aviator")
jumpName = CardName.MkCardName (Text.pack "Jump")
cloneName = CardName.MkCardName (Text.pack "Clone")

-- Every cast this player is offered, by the NAME of the half being offered --
-- Pawl.AdventureSpec's read of the same menu, and the shape CR 722.3's claim is
-- about: the inset frame is not among the things a player may cast.
namesOffered :: GameState.GameState -> [CardName.CardName]
namesOffered gs = [n | A.Cast _ n _ <- Action.legalActions S.alice gs]

-- The exiled objects that are CR 722.3c's copy, by id. Read off
-- Object.preparedCopyOf rather than off the name, so a case can tell "the copy is
-- gone" from "something else named Jump is in exile" -- and so the count is the
-- rule's own subject rather than a coincidence of naming.
prepareCopies :: GameState.GameState -> [ObjectId.ObjectId]
prepareCopies gs =
  [ oid
  | oid <- Set.toList (GameState.exile gs),
    Just obj <- [Game.lookupObject oid gs],
    Just _ <- [Object.preparedCopyOf obj]
  ]

-- The newest battlefield permanent whose PRINTED card is a Clone -- read off the
-- printing rather than off the projection, which is exactly the name a copy no
-- longer has (CR 707.2). Pawl.MoveCounterSpec's `newestNamed` reads the projected
-- face for the same job; this one must not, since the object under test is a copy.
newestClone :: GameState.GameState -> Maybe ObjectId.ObjectId
newestClone gs =
  let printed oid = fmap (S.nameOf . Printing.card) (Game.printingOfObject oid gs) == Just cloneName
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter printed (Set.toList (GameState.battlefield gs))))

isPrepared :: ObjectId.ObjectId -> GameState.GameState -> Bool
isPrepared oid gs = maybe False (Set.member Designation.Prepared . Object.designations) (Game.lookupObject oid gs)

-- Aim Jump at the Piker. FILTERED out of the offered set rather than built, so a
-- recipient the engine never offered cannot be smuggled in (#222) and CR 608.2b's
-- re-read at resolution finds the same recipient the choice named.
jumpAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
jumpAt victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature victim) . snd) sets
  _ -> S.identityAnswer p

-- CR 707.5's as-enters choice, pinned to ONE named permanent rather than
-- searched, which is Pawl.CopySpec's `copyNamed` posture and for its reason: an
-- answerer that looked for a legal creature would find the Aviator again after a
-- mutation and repair the assertion. Trigger batches are ordered as they arrive,
-- since the Clone's own entry and the attack put several on the stack.
copyNamed :: ObjectId.ObjectId -> Prompt.Prompt r -> r
copyNamed wanted p = case p of
  Prompt.ChooseCopyTarget {} -> Just wanted
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  _ -> S.identityAnswer p

-- Reality Ripple's one target, pinned to a named permanent for copyNamed's
-- reason, and everything else answered as `attackTo` would so the same answerer
-- can carry a combat.
rippleAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
rippleAt victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just victim) . Recipient.objectOf) . snd) sets
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  _ -> S.identityAnswer p

-- alice's Aviator and a Goblin Piker against bob, with one Island to pay the
-- copy's {U} and combat about to start. The Piker is Jump's target and is
-- otherwise inert: it never attacks, and nothing else on the board grants
-- anything.
aviatorDuel :: S.Board
aviatorDuel =
  S.duel
    S.beginningOfCombat
    [ S.settled "aviator" "Encouraging Aviator",
      S.settled "piker" "Goblin Piker",
      S.aliased "island" (S.permanent "Island")
    ]
    []

-- The same board with Aurelia, the Warleader beside the Aviator: her CR 500.8
-- additional combat phase is what gives one turn two declare-attackers steps, and
-- her untap (she is the only source of one here) is what lets the Aviator attack
-- in the second one.
--
-- bob starts at 60 so that two combats do not kill him: the three attackers deal
-- 7 a combat, and a dead defending player would end the game (CR 104.3b) before
-- the second declaration this case is about.
aureliaDuel :: S.Board
aureliaDuel =
  S.board
    ( S.battlefield
        S.alice
        [ S.settled "aviator" "Encouraging Aviator",
          S.settled "aurelia" "Aurelia, the Warleader",
          S.settled "piker" "Goblin Piker",
          S.aliased "island" (S.permanent "Island")
        ]
        NonEmpty.:| [(S.battlefield S.bob []) {S.setupLife = 60}]
    )
    S.alice
    S.beginningOfCombat

-- The Aviator attacks bob unblocked, and the whole combat phase runs -- so CR
-- 508.1's declaration, CR 603.3's trigger placement and CR 608's resolution all
-- happen inside the engine rather than being poked in.
attackScript :: Seq.Seq S.Timed
attackScript = S.turn 1 [S.on S.declareAttackers S.alice (S.attack [S.aliasRef "aviator"])]

-- CR 722.2b's board. The printed Aviator is here only to be COPIED: Concordant
-- Crossroads gives every creature haste (CR 702.10b), so the Clone can attack the
-- turn it enters. Six Islands, where the Clone's {2}{U} and then Jump's {U} come
-- to four: the spare mana is what keeps "alice may cast the copy" a claim about
-- CR 722.3c's permission rather than about an empty board.
--
-- The printed Aviator attacks too -- `attackTo` declares every creature -- and
-- mints a copy of its own, which is why every assertion below is keyed to the
-- permanent a copy NAMES rather than to a count of exile.
cloneDuel :: S.Board
cloneDuel =
  S.duel
    S.precombatMain
    [ S.settled "aviator" "Encouraging Aviator",
      S.settled "piker" "Goblin Piker",
      S.permanent "Concordant Crossroads",
      S.permanent "Island",
      S.permanent "Island",
      S.permanent "Island",
      S.permanent "Island",
      S.permanent "Island",
      S.permanent "Island"
    ]
    []

-- CR 702.26b's board: the proving board plus three Islands and Reality Ripple in
-- alice's own hand, so the phase-out happens after the mint with nothing else
-- changed.
rippleDuel :: S.Board
rippleDuel =
  S.board
    ( ( S.battlefield
          S.alice
          [ S.settled "aviator" "Encouraging Aviator",
            S.settled "piker" "Goblin Piker",
            S.permanent "Island",
            S.permanent "Island",
            S.permanent "Island"
          ]
      )
        { S.setupHand = Seq.fromList [S.aliased "ripple" (S.cardSetup "Reality Ripple")]
        }
        NonEmpty.:| [S.battlefield S.bob []]
    )
    S.alice
    S.beginningOfCombat

-- The exile copy minted FOR this permanent, by id. Keyed to the permanent rather
-- than to a count, so a board carrying two prepared permanents can name either.
copyFor :: ObjectId.ObjectId -> GameState.GameState -> [ObjectId.ObjectId]
copyFor permanentId gs =
  [ oid
  | oid <- prepareCopies gs,
    Just obj <- [Game.lookupObject oid gs],
    Object.preparedCopyOf obj == Just permanentId
  ]

aliasOrFail :: (Monad m) => Spec.Spec m n -> S.BuiltBoard -> String -> m ObjectId.ObjectId
aliasOrFail s built name = case Map.lookup (S.MkObjectAlias (Text.pack name)) (S.builtAliases built) of
  Nothing -> Spec.assertFailure s ("the board omitted the " <> name <> " alias")
  Just oid -> pure oid

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Preparation" $ do
  -- CR 722.4: "in every zone, a preparation card has only its normal
  -- characteristics." Stricter than either sibling layout -- CR 715.4 and CR
  -- 720.4 both carve the stack out and this rule carves nothing out -- so a hand
  -- and a library are asked, and the falsifier is CR 709.4's combined reading,
  -- which would name the card "Encouraging Aviator//Jump", make it an instant as
  -- well as a creature, and price it at the two halves' 4.
  Spec.it s "CR 722.4 a preparation card off the battlefield is only Encouraging Aviator" $ do
    aviator <- S.printingOf s registry "Encouraging Aviator"
    let inZone zone = S.addObjectIn zone aviator S.alice (Setup.emptyGame S.bothPlayers)
        (handId, inHand) = inZone Zone.Hand
        (libraryId, inLibrary) = inZone Zone.Library
    Spec.assertEqWith s "in a hand, named for the creature alone" (Projection.namesOf handId inHand) (Set.singleton aviatorName)
    Spec.assertEqWith s "and mana value 3, not the two halves' 4" (PC.manaValue (Projection.project handId inHand)) (Just 3)
    Spec.assertBool s (Set.member CardType.Creature (Projection.cardTypesOf handId inHand)) "a creature card"
    Spec.assertBool s (not (Set.member CardType.Instant (Projection.cardTypesOf handId inHand))) "and not an instant"
    Spec.assertEqWith s "in a library, the same" (Projection.namesOf libraryId inLibrary) (Set.singleton aviatorName)
    Spec.assertBool s (not (Set.member CardType.Instant (Projection.cardTypesOf libraryId inLibrary))) "and still not an instant"
  -- CR 722.3: "Preparation cards can't be cast using the alternative
  -- characteristics found within their inset frames." Where CR 715.3 and CR 720.3
  -- offer BOTH halves off the same card in the same hand -- Pawl.AdventureSpec's
  -- "CR 715.3 both halves are offered from a hand" is the contrast -- this layout
  -- offers one, ever.
  --
  -- Three Islands, so {2}{U} and {U} are both payable and an absent Jump offer is
  -- the layout rather than mana; and a Piker on the battlefield, so Jump would
  -- have a legal target if it were offered at all.
  Spec.it s "CR 722.3 the prepare spell is never offered from a hand" $ do
    aviator <- S.printingOf s registry "Encouraging Aviator"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, board) = S.addPermanent piker S.alice (S.landsInPlay island 3)
        (gs, _) = S.handOne aviator board
    Spec.assertEqWith s "only the creature half" (namesOffered gs) [aviatorName]
  -- THE proving case, and the whole mechanic in one board. CR 722.3a: the printed
  -- trigger makes the attacking Aviator prepared. CR 722.3c: "its controller
  -- creates a copy of that object in exile, except that copy has only the
  -- characteristics of that permanent's prepare spell ... for as long as the copy
  -- remains in exile, the prepared permanent's controller may cast the copy."
  -- CR 601.2i: "that permanent loses the prepared designation at the time the
  -- spell becomes cast."
  --
  -- The BEFORE assertions are what keep the after ones honest: a mint that fired
  -- unconditionally, or a permission that answered True for anything in exile,
  -- passes every post-attack assertion on its own. So the same board is asked for
  -- Jump's offer before the attack, when the Aviator is not prepared and exile is
  -- empty.
  Spec.it s "CR 722.3c attacking mints a castable Jump copy, and casting it unprepares the Aviator" $ do
    built <- S.buildBoardOrFail s registry aviatorDuel
    aviatorId <- aliasOrFail s built "aviator"
    pikerId <- aliasOrFail s built "piker"
    let start = S.builtState built
    Spec.assertBool s (not (isPrepared aviatorId start)) "before: the Aviator is not prepared"
    Spec.assertEqWith s "before: nothing in exile" (prepareCopies start) []
    Spec.assertBool s (notElem jumpName (namesOffered start)) "before: no Jump is offered"
    (_, attacked) <- S.runScriptOrFail s attackScript built S.combatGame
    Spec.assertBool s (isPrepared aviatorId attacked) "CR 722.3a: the Aviator is prepared"
    case prepareCopies attacked of
      [copyId] -> do
        -- CR 722.3c's "only the characteristics of that permanent's prepare
        -- spell", and "those characteristics become the copy's normal
        -- characteristics": the copy is Jump and nothing of the Aviator's.
        Spec.assertEqWith s "CR 722.3c: the copy is Jump" (Projection.namesOf copyId attacked) (Set.singleton jumpName)
        Spec.assertBool s (Set.member CardType.Instant (Projection.cardTypesOf copyId attacked)) "and an instant"
        Spec.assertBool s (not (Set.member CardType.Creature (Projection.cardTypesOf copyId attacked))) "and not a creature"
        Spec.assertEqWith s "and mana value 1, the inset frame's {U}" (PC.manaValue (Projection.project copyId attacked)) (Just 1)
        -- CR 722.3c's permission names ONE player, so it is not an offer to the
        -- table.
        Spec.assertBool s (Cast.castable S.alice copyId jumpName Facing.FaceUp attacked) "CR 722.3c: alice may cast the copy"
        Spec.assertBool s (not (Cast.castable S.bob copyId jumpName Facing.FaceUp attacked)) "and bob may not"
        Spec.assertBool s (elem jumpName (namesOffered attacked)) "after: Jump is offered"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying pikerId attacked)) "the Piker does not have flying yet"
        let cast = S.runPure (jumpAt pikerId) attacked (Cast.castSpell S.manaPerformer S.alice copyId jumpName Facing.FaceUp)
            resolved = S.runPure (jumpAt pikerId) cast Stack.resolveTop
        -- THE gameplay assertion, first so no proxy can absorb a mutation: the
        -- copy resolved and did what the prepare spell says.
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying pikerId resolved) "CR 722.3c: the cast copy gives the Piker flying"
        Spec.assertBool s (not (isPrepared aviatorId resolved)) "CR 601.2i: the Aviator is no longer prepared"
        Spec.assertEqWith s "and no copy is left in exile" (prepareCopies resolved) []
      other -> Spec.assertFailure s ("expected exactly one copy in exile, got " <> show (length other))
  -- CR 722.3c's residency condition, the half CR 704.5e's exception is scoped to:
  -- "this copy remains in exile for as long as the prepared permanent remains on
  -- the battlefield and has the prepared designation." The Aviator is put into its
  -- owner's hand, and the state-based check that follows removes the copy.
  Spec.it s "CR 722.3c the copy leaves exile when the Aviator leaves the battlefield" $ do
    built <- S.buildBoardOrFail s registry aviatorDuel
    aviatorId <- aliasOrFail s built "aviator"
    (_, attacked) <- S.runScriptOrFail s attackScript built S.combatGame
    Spec.assertEqWith s "the copy is there to lose" (length (prepareCopies attacked)) 1
    let bounced = S.runPure S.identityAnswer attacked (Event.changeZone aviatorId Zone.Hand)
        swept = S.runPure S.identityAnswer bounced Sba.checkStateBasedActions
    Spec.assertEqWith s "CR 704.5e: the copy has ceased to exist" (prepareCopies swept) []
    Spec.assertEqWith s "and exile is empty" (Foldable.toList (GameState.exile swept)) []
  -- CR 722.3a's second gate: "a permanent can't gain this designation if the
  -- permanent already has it." Aurelia's CR 500.8 additional combat phase gives
  -- the turn two declare-attackers steps and her untap lets the Aviator attack in
  -- both, so its printed trigger resolves TWICE with the copy still in exile.
  --
  -- The control is the first attack: exactly one copy after it, so the one copy
  -- after the second is a refused gain rather than a combat that never happened.
  Spec.it s "CR 722.3a a second attack while already prepared mints no second copy" $ do
    built <- S.buildBoardOrFail s registry aureliaDuel
    aviatorId <- aliasOrFail s built "aviator"
    let twice = S.runCombat (S.attackTo S.bob) (S.builtState built)
    -- The CONTROL, and what keeps the count below from passing vacuously: two
    -- combats of 7 took bob from 60 to 46, where one would have left him at 53.
    -- So the Aviator really did attack a second time while prepared.
    Spec.assertEqWith s "CR 500.8: two combat phases happened" (S.lifeOf S.bob twice) (Just 46)
    Spec.assertBool s (isPrepared aviatorId twice) "the Aviator is still prepared"
    Spec.assertEqWith s "CR 722.3a: still exactly one copy in exile" (length (prepareCopies twice)) 1
  -- CR 722.2b: "the existence and values of these alternative characteristics are
  -- part of the object's copiable values." So a permanent has a prepare spell
  -- because of what it COPIES, not because of the card printed underneath it --
  -- and a Clone that entered as a copy of the Aviator (CR 707.5's as-enters road)
  -- becomes prepared and mints a Jump copy of its own.
  --
  -- The falsifier is reading the printing behind Object.source, which is what the
  -- first slice did: the Clone's own card is a {2}{U} shapeshifter with no inset
  -- frame, so CR 722.3a's gate refused the designation and exile stayed empty.
  --
  -- The printed Aviator attacks alongside and mints its own copy, which is the
  -- control: a mint that fired for neither, or one that fired only for the printed
  -- card, is told apart by WHICH permanent each copy names.
  Spec.it s "CR 722.2b a Clone of the Aviator becomes prepared and mints a Jump copy" $ do
    built <- S.buildBoardOrFail s registry cloneDuel
    aviatorId <- aliasOrFail s built "aviator"
    pikerId <- aliasOrFail s built "piker"
    clone <- S.printingOf s registry "Clone"
    let (cloneHandId, ready) = S.addHandCard clone S.alice (S.builtState built)
        entered = S.runPure (copyNamed aviatorId) ready (S.cast S.alice cloneHandId *> Stack.resolveTop *> Engine.settleForPriority)
    case newestClone entered of
      Nothing -> Spec.assertFailure s "the Clone did not reach the battlefield"
      Just cloneId -> do
        -- Without this the mint below could be the printed Aviator's: the Clone
        -- has to be a copy at all before CR 722.2b has anything to say.
        Spec.assertEqWith s "CR 707.5: the Clone entered as a copy of the Aviator" (Projection.namesOf cloneId entered) (Set.singleton aviatorName)
        Spec.assertBool s (not (isPrepared cloneId entered)) "before: the Clone is not prepared"
        -- Six steps carry the turn from its precombat main phase, where CR
        -- 302.1's sorcery timing let the Clone be cast, into and through combat.
        -- S.runCombat cannot: `combatGame` stops at once when the phase is not a
        -- combat one, so it would run nothing here and the case would assert
        -- against a board that never fought.
        let fought = S.runPure (S.attackTo S.bob) entered (Monad.replicateM_ 6 Engine.runStep)
        -- THE gameplay assertion, first so no proxy can absorb a mutation.
        Spec.assertBool s (isPrepared cloneId fought) "CR 722.2b: the Clone became prepared"
        case copyFor cloneId fought of
          [copyId] -> do
            Spec.assertEqWith s "CR 722.3c: the Clone's copy is Jump" (Projection.namesOf copyId fought) (Set.singleton jumpName)
            Spec.assertBool s (Cast.castable S.alice copyId jumpName Facing.FaceUp fought) "and alice may cast it"
            let resolved = S.runPure (jumpAt pikerId) fought (Cast.castSpell S.manaPerformer S.alice copyId jumpName Facing.FaceUp *> Stack.resolveTop)
            Spec.assertBool s (Projection.hasKeyword Keyword.Flying pikerId resolved) "and casting it gives the Piker flying"
            Spec.assertBool s (not (isPrepared cloneId resolved)) "CR 601.2i: the Clone is no longer prepared"
          other -> Spec.assertFailure s ("expected exactly one copy for the Clone, got " <> show (length other))
        -- The control: the printed Aviator attacked too and minted its own, so a
        -- mint that never ran at all is caught here rather than passing above.
        Spec.assertEqWith s "and the printed Aviator minted one of its own" (length (copyFor aviatorId fought)) 1
  -- CR 702.26b: a phased-out permanent "is treated as though it does not exist",
  -- and CR 722.3c keeps the copy only "for as long as the prepared permanent
  -- remains on the battlefield". So phasing the Aviator out ends the copy at the
  -- next state-based check, which is also what that rule's own "or phases in
  -- prepared" branch presupposes -- phasing in would otherwise mint a second copy
  -- beside a first that never left.
  --
  -- The falsifier is reading Object.zone, which is what the first slice did: CR
  -- 702.26d leaves a phased-out permanent's zone at Zone.Battlefield, so the copy
  -- stayed in exile and stayed castable while the permanent did not exist.
  Spec.it s "CR 702.26b Reality Ripple phases the Aviator out and the copy ceases to exist" $ do
    built <- S.buildBoardOrFail s registry rippleDuel
    aviatorId <- aliasOrFail s built "aviator"
    rippleId <- aliasOrFail s built "ripple"
    (_, attacked) <- S.runScriptOrFail s attackScript built S.combatGame
    -- The control, and what keeps the assertions below from passing vacuously.
    Spec.assertEqWith s "the copy is there to lose" (length (prepareCopies attacked)) 1
    Spec.assertBool s (any (\oid -> Cast.castable S.alice oid jumpName Facing.FaceUp attacked) (prepareCopies attacked)) "and alice may cast it"
    let phased = S.runPure (rippleAt aviatorId) attacked (S.cast S.alice rippleId *> Stack.resolveTop *> Sba.checkStateBasedActions)
    -- Without this the copy's absence could be a Reality Ripple that fizzled.
    Spec.assertEqWith s "CR 702.26b: the Aviator left GameState.battlefield, phased out" (Set.member aviatorId (GameState.battlefield phased), Phasing.isPhasedOut aviatorId phased) (False, True)
    -- THE gameplay assertion.
    Spec.assertEqWith s "CR 704.5e: the copy has ceased to exist" (prepareCopies phased) []
    Spec.assertEqWith s "and exile is empty" (Foldable.toList (GameState.exile phased)) []
    Spec.assertBool s (notElem jumpName (namesOffered phased)) "so no Jump is offered any more"
