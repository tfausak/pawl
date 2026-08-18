{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Resolve over investigate (CR 701.30), suspect (CR 701.58), the
-- token-making effects around them, and the effects that reveal or cast at
-- random. The machinery is Pawl.ResolveSpec.
module Pawl.InvestigateSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Answers Prompt.ChooseSacrifices with `wanted`, when it is on offer. A pair of
-- tests differing only in this argument proves the ANSWER decides which permanent
-- is sacrificed, rather than the order the candidates are enumerated in.
sacrifices :: ObjectId.ObjectId -> Prompt.Prompt r -> r
sacrifices wanted p = case p of
  Prompt.ChooseSacrifices _ _ _ candidates _ ->
    if elem wanted candidates then Set.singleton wanted else Set.fromList (take 1 candidates)
  Prompt.ChooseAnyNumberToSacrifice {} -> Set.empty
  Prompt.ChooseTapsForTotalPower _ _ _ candidates _ -> Set.fromList candidates
  _ -> S.identityAnswer p

-- CR 701.16a: "'Investigate' means 'Create a Clue token.' See rule 111.10f."
-- The keyword action is pure shorthand for a Create, which is why Thraben
-- Inspector needs no opcode of its own: the card data spells CR 111.10f's
-- predefined Clue out literally ("a colorless Clue artifact token with '{2},
-- Sacrifice this token: Draw a card.'"), which is the "given, not derived" side
-- of Effect.Create's own doc comment rather than a lookup of the predefined
-- definition.
--
-- Gameplay level throughout: the Inspector is cast from hand for {W}, its
-- CR 603.6a enters trigger is placed by the settle and resolved off the stack,
-- and the Clue's own ability is then activated and resolved.
--
-- Alice keeps THREE Plains so that after the {W} exactly two stay untapped --
-- the Clue's {2} -- and a Goblin Piker sits in her library so the draw has
-- something to find (CR 104.3c would otherwise decide the game first) and so
-- the drawn card is identifiable by name rather than by a count that the
-- Inspector's own 1/2 could coincide with.
investigateBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
investigateBoard s registry = do
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  inspector <- S.printingOf s registry "Thraben Inspector"
  let (gs0, spellId) = S.handOne inspector (S.landsInPlay plains 3)
      (_, gs1) = S.addLibraryCard piker S.alice gs0
      cast = S.runPure S.identityAnswer gs1 (S.cast S.alice spellId)
      -- The settle is what places the CR 603.6a trigger; the second resolveTop
      -- is the trigger itself.
      entered = S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
  pure (S.runPure S.identityAnswer entered (Stack.resolveTop >> Engine.settleForPriority))

-- The one Clue on the board, by the fact that it is the only token there.
clueOf :: GameState.GameState -> Maybe ObjectId.ObjectId
clueOf gs = case S.tokensOf gs of
  [oid] -> Just oid
  _ -> Nothing

-- The untapped lands on the board -- on this board, alice's Plains and nothing
-- else. Used to build the one-mana board from the two-mana board by tapping one
-- more land and changing nothing else.
untappedPlains :: GameState.GameState -> [ObjectId.ObjectId]
untappedPlains gs =
  [ oid
  | oid <- Set.toList (GameState.battlefield gs),
    Set.member CardType.Land (Projection.cardTypesOf oid gs),
    fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Untapped
  ]

investigateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
investigateSpec s registry = Spec.describe s "Investigate" $ do
  Spec.it s "CR 701.16a Thraben Inspector's ETB creates one colorless Clue artifact token" $ do
    after <- investigateBoard s registry
    -- Three Plains, the Inspector and exactly one more permanent. Stated as a
    -- total rather than as "one token" so that a Create minting two fails here
    -- as well as at clueOf below.
    Spec.assertEqWith s "five permanents: three Plains, the Inspector and one more" (Set.size (GameState.battlefield after)) 5
    Spec.assertEqWith s "the Inspector resolved" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Thraben Inspector") S.alice after) 1
    case clueOf after of
      Nothing -> Spec.assertFailure s "expected exactly one token on the battlefield"
      Just clueId -> do
        -- CR 111.4: investigate does not name its token, so the name is the
        -- subtype plus the word "Token".
        Spec.assertEqWith s "the token is named Clue Token" (fmap Face.name (Game.faceOf clueId after)) (Just . CardName.MkCardName $ Text.pack "Clue Token")
        Spec.assertEqWith s "CR 111.10f: an artifact" (Projection.cardTypesOf clueId after) (Set.singleton CardType.Artifact)
        Spec.assertEqWith s "CR 111.10f: with subtype Clue" (Projection.subtypesOf clueId after) (Set.singleton Subtype.Clue)
        -- CR 202.2b ("objects with no colored mana symbols in their mana costs
        -- are colorless") plus CR 202.2e (a color indicator is the other way an
        -- object gets a color): the token face carries neither, which is how
        -- the card data spells "colorless". The falsifier for the clause being
        -- asserted rather than assumed -- a token face given colorIndicator
        -- White fails here and nowhere else.
        Spec.assertEqWith s "CR 111.10f: and colorless" (Projection.colorsOf clueId after) Set.empty
        -- CR 111.2: the player who creates a token controls it.
        Spec.assertEqWith s "CR 111.2: alice created it, so alice controls it" (Projection.controllerOf clueId after) (Just S.alice)
  Spec.it s "CR 111.10f the Clue's {2} is real: one untapped Plains cannot pay it" $ do
    -- The negative board differs from the positive one ONLY in how many lands
    -- are untapped: same permanents, same phase, same empty stack. Without
    -- that, "not activatable" would pass for any of the reasons a cost check
    -- can fail.
    twoMana <- investigateBoard s registry
    case (clueOf twoMana, untappedPlains twoMana) of
      (Just clueId, first : _) -> do
        let oneMana = S.tapObject first twoMana
        Spec.assertEqWith s "two Plains untapped after the {W}" (length (untappedPlains twoMana)) 2
        Spec.assertEqWith s "one on the negative board" (length (untappedPlains oneMana)) 1
        case Activate.abilitiesFor clueId twoMana of
          [ability] -> do
            Spec.assertBool s (Activate.activatable S.alice clueId ability twoMana) "two mana pays {2}"
            Spec.assertBool s (not (Activate.activatable S.alice clueId ability oneMana)) "one does not"
          other -> Spec.assertFailure s ("expected exactly one activated ability on the Clue, got " <> show (length other))
      _ -> Spec.assertFailure s "expected one token and at least one untapped Plains"
  Spec.it s "CR 111.10f cracking the Clue draws a card, and the token ceases to exist (CR 111.7)" $ do
    before <- investigateBoard s registry
    case clueOf before of
      Nothing -> Spec.assertFailure s "expected exactly one token on the battlefield"
      Just clueId -> case Activate.abilitiesFor clueId before of
        [ability] -> do
          let activated = S.runPure S.identityAnswer before (Activate.activateAbility S.alice clueId ability)
              after = S.runPure S.identityAnswer activated (Stack.resolveTop >> Engine.settleForPriority)
          Spec.assertEqWith s "alice's hand was empty before" (S.handSize S.alice before) 0
          -- Named, not counted -- and NOT through S.countByName, which spans
          -- hand and library together and so cannot tell "drawn" from "still
          -- in the library". The hand's own member is what says the Piker
          -- moved, and the emptied library is the other half of it.
          Spec.assertEqWith s "alice drew the Goblin Piker" (fmap (`S.soleFaceName` after) (Game.zoneMembers Zone.Hand S.alice after)) [CardName.MkCardName $ Text.pack "Goblin Piker"]
          Spec.assertEqWith s "and her library is empty" (length (Game.zoneMembers Zone.Library S.alice after)) 0
          Spec.assertBool s (not (S.onBattlefield clueId after)) "the Clue was sacrificed as a cost"
          -- CR 111.7: a token in any zone other than the battlefield ceases to
          -- exist, so the honest assertion is that it exists nowhere -- NOT
          -- that it reached the graveyard.
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject clueId after)) "CR 111.7: and no longer exists in any zone"
          Spec.assertEqWith s "alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
          Spec.assertEqWith s "three Plains are now tapped: the {W} and the {2}" (S.tappedCount S.alice after) 3
        other -> Spec.assertFailure s ("expected exactly one activated ability on the Clue, got " <> show (length other))

-- CR 701.60, proved by Person of Interest {3}{R} Creature -- Human Rogue 2/2,
-- "When this creature enters, suspect it. Create a 2/2 white and blue Detective
-- creature token."
--
-- The Detective is what makes every case below a PAIR on one board: it is
-- alice's (or bob's) other creature, created by the same resolution, and the only
-- thing that differs between the two is the designation. A fixture that read the
-- card wrong, or a menace grant aimed at the wrong object, fails the second half
-- of each assertion rather than passing for a board-shaped reason.
--
-- CR 701.60c has two halves in two different subsystems, so both are asserted at
-- gameplay level: menace goes through Pawl.Engine.Projection's layer-6 grant and
-- is read by a block declaration, "can't block" goes through
-- Pawl.Engine.CombatRestriction and is read by another.
personOfInterestSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
personOfInterestSpec s registry = Spec.describe s "PersonOfInterest" $ do
  let -- The board after the CR 603.6a enters trigger has resolved: `pid` gets the
      -- Person of Interest and its Detective, and the Pikers fill out whichever
      -- side the case needs. S.entersWithTrigger rather than a cast, because
      -- S.combatBoardOf starts in the declare attackers step, where no spell can
      -- be cast.
      board mine theirs pid = do
        poi <- S.printingOf s registry "Person of Interest"
        piker <- S.printingOf s registry "Goblin Piker"
        let (gs0, ours, yours) = S.combatBoardOf (replicate mine piker) (replicate theirs piker)
            (poiId, gs1) = S.entersWithTrigger poi pid gs0
            settled = S.runPure S.identityAnswer gs1 (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
        pure (settled, poiId, ours, yours)
  Spec.it s "CR 701.60a the enters trigger suspects the Person and not the Detective it makes" $ do
    (gs, poiId, _, _) <- board 0 0 S.alice
    case S.tokensOf gs of
      [tokenId] -> do
        Spec.assertEqWith s "the Person is suspected, the Detective is not" (suspectedOf poiId gs, suspectedOf tokenId gs) (Just True, Just False)
        Spec.assertEqWith s "the token is a 2/2" (S.powerToughnessOf tokenId gs) (Just (2, 2))
        Spec.assertEqWith s "a Detective creature" (Projection.cardTypesOf tokenId gs, Projection.subtypesOf tokenId gs) (Set.singleton CardType.Creature, Set.singleton Subtype.Detective)
        Spec.assertEqWith s "white and blue" (Projection.colorsOf tokenId gs) (Set.fromList [Color.White, Color.Blue])
        Spec.assertEqWith s "CR 111.2: alice created it, so alice controls it" (Projection.controllerOf tokenId gs) (Just S.alice)
      other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
  -- CR 701.60a's "until it leaves the battlefield", asserted on
  -- Object.newIncarnation directly for the reason Pawl.TriggerSpec's renown case
  -- gives: nothing writes the field on an entry, and Pawl.SetupSpec's CR 400.7
  -- case is blind to a field the forgetting never touches.
  Spec.it s "CR 701.60a the designation does not survive CR 400.7" $ do
    (gs, poiId, _, _) <- board 0 0 S.alice
    case Game.lookupObject poiId gs of
      Nothing -> Spec.assertFailure s "expected to find the Person"
      Just obj -> Spec.assertEqWith s "this incarnation is suspected, the next one is not" (isSuspected obj, isSuspected (Object.newIncarnation obj)) (True, False)
  Spec.it s "CR 701.60c a suspected creature has menace, so one blocker cannot block it" $ do
    -- bob's two Pikers are the falsifier for reading rule 701.60c as "can't be
    -- blocked": the very creature that cannot block the Person alone can block it
    -- alongside the other, and the block survives a real declare blockers step.
    (entered, poiId, _, blockers) <- board 0 2 S.alice
    let gs = S.runPure S.aggressiveAnswer entered (Combat.declareAttackers S.alice)
    case (S.tokensOf gs, blockers) of
      ([tokenId], [first, second]) -> do
        Spec.assertEqWith s "the Person has menace and the Detective does not" (Projection.hasKeyword Keyword.Menace poiId gs, Projection.hasKeyword Keyword.Menace tokenId gs) (True, False)
        Spec.assertEqWith s "the Person is attacking" (S.attackerDeclarationsOf gs) [poiId]
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton first (Set.singleton poiId)) gs)) "one blocker is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, Set.singleton poiId), (second, Set.singleton poiId)]) gs) "two are legal"
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
        Spec.assertEqWith s "and both block" (Combat.blockersOf poiId after) (Set.fromList [first, second])
      other -> Spec.assertFailure s ("expected one token and two blockers, got " <> show other)
  Spec.it s "CR 701.60c a suspected creature can't block, where the Detective beside it can" $ do
    -- The designation on the DEFENDING side, so rule 701.60c's second half is the
    -- only thing separating the two creatures bob could block with. Both are his,
    -- both entered this turn, and only one is suspected.
    (entered, poiId, attackers, _) <- board 1 0 S.bob
    let gs = S.runPure S.aggressiveAnswer entered (Combat.declareAttackers S.alice)
    case (S.tokensOf gs, attackers) of
      ([tokenId], [attackerId]) -> do
        Spec.assertEqWith s "alice's Piker is attacking" (S.attackerDeclarationsOf gs) [attackerId]
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton poiId (Set.singleton attackerId)) gs)) "the Person cannot block"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton tokenId (Set.singleton attackerId)) gs) "the Detective can"
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
        Spec.assertEqWith s "so only the Detective blocks" (Combat.blockersOf attackerId after) (Set.singleton tokenId)
      other -> Spec.assertFailure s ("expected one token and one attacker, got " <> show other)

-- CR 701.60a's second ending, "until a spell or ability causes it to no longer be
-- suspected", proved by Eliminate the Impossible {1}{U} Instant, "Investigate.
-- Creatures your opponents control get -2/-0 until end of turn. If any of them
-- are suspected, they're no longer suspected."
--
-- One board carries the whole case: bob's Person of Interest is suspected and his
-- Detective is not, so the -2/-0 lands on both while only one designation ends.
-- The two things rule 701.60c hangs off the designation are then asserted gone at
-- gameplay level, each through its own subsystem -- menace through the layer-6
-- grant, "can't block" through the combat restriction.
eliminateTheImpossibleSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
eliminateTheImpossibleSpec s registry = Spec.describe s "EliminateTheImpossible" $ do
  Spec.it s "CR 701.60a a spell ends the designation, and CR 701.60c's menace and can't-block end with it" $ do
    poi <- S.printingOf s registry "Person of Interest"
    piker <- S.printingOf s registry "Goblin Piker"
    island <- S.printingOf s registry "Island"
    eliminate <- S.printingOf s registry "Eliminate the Impossible"
    let (gs0, attackers, _) = S.combatBoardOf [piker] []
        (poiId, gs1) = S.entersWithTrigger poi S.bob gs0
        entered = S.runPure S.identityAnswer gs1 (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
        -- S.addCreature places any permanent; these two are the {1}{U}.
        (_, gs2) = S.addCreature island S.alice entered
        (_, gs3) = S.addCreature island S.alice gs2
        (spellId, gs4) = S.addHandCard eliminate S.alice gs3
        declared = S.runPure S.aggressiveAnswer gs4 (Combat.declareAttackers S.alice)
    case (S.tokensOf declared, attackers) of
      ([detectiveId], [attackerId]) -> do
        let after = S.runPure S.identityAnswer declared (S.cast S.alice spellId >> Stack.resolveTop >> Engine.settleForPriority)
        -- The before half, on the very board the spell is cast from: without it
        -- every "after" assertion could be passing because the fixture never
        -- suspected anything.
        Spec.assertEqWith s "before: the Person is suspected and the Detective is not" (suspectedOf poiId declared, suspectedOf detectiveId declared) (Just True, Just False)
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton poiId (Set.singleton attackerId)) declared)) "before: the Person cannot block"
        Spec.assertEqWith s "after: neither is suspected" (suspectedOf poiId after, suspectedOf detectiveId after) (Just False, Just False)
        Spec.assertEqWith s "CR 701.60c: and the menace it had is gone" (Projection.hasKeyword Keyword.Menace poiId after, Projection.hasKeyword Keyword.Menace detectiveId after) (False, False)
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton poiId (Set.singleton attackerId)) after) "CR 701.60c: so the Person can block"
        let blocked = S.runPure S.aggressiveAnswer after Combat.declareBlockers
        Spec.assertBool s (Set.member poiId (Combat.blockersOf attackerId blocked)) "and it does block"
        -- The two clauses either side of the ending, so a card file that dropped
        -- one fails here: the -2/-0 reaches both of bob's creatures, and the
        -- Clue is alice's.
        Spec.assertEqWith s "both of bob's creatures took -2/-0" (S.powerToughnessOf poiId after, S.powerToughnessOf detectiveId after) (Just (0, 2), Just (0, 2))
        Spec.assertEqWith s "and alice's own attacker did not, so the sweep is opponents-only" (S.powerToughnessOf attackerId after) (Just (2, 1))
        Spec.assertEqWith s "and alice investigated" (fmap (`S.soleFaceName` after) (filter (/= detectiveId) (S.tokensOf after))) [CardName.MkCardName $ Text.pack "Clue Token"]
      other -> Spec.assertFailure s ("expected one token and one attacker, got " <> show other)

-- CR 701.60b read as a number, proved by Repeat Offender {1}{B} Creature -- Human
-- Assassin 2/1, "{2}{B}: If this creature is suspected, put a +1/+1 counter on
-- it. Otherwise, suspect it."
--
-- The card is its own pair: the same activation on the same board takes the other
-- branch once the designation is there, so the two clause conditions are the only
-- thing that can separate the two outcomes.
repeatOffenderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
repeatOffenderSpec s registry = Spec.describe s "RepeatOffender" $ do
  Spec.it s "CR 701.60b the first activation suspects and the second adds a counter" $ do
    swamp <- S.printingOf s registry "Swamp"
    offender <- S.printingOf s registry "Repeat Offender"
    let (offenderId, board) = S.addCreature offender S.alice (S.landsInPlay swamp 6)
        activate gs = case Activate.abilitiesFor offenderId gs of
          [ability] -> Right (S.runPure S.identityAnswer gs (Activate.activateAbility S.alice offenderId ability >> Stack.resolveTop >> Engine.settleForPriority))
          other -> Left (length other)
        state gs = (suspectedOf offenderId gs, S.counterOf CounterKind.PlusOnePlusOne offenderId gs, S.powerToughnessOf offenderId gs)
    case activate board of
      Left n -> Spec.assertFailure s ("expected exactly one activated ability, got " <> show n)
      Right once -> do
        Spec.assertEqWith s "it starts unsuspected, with no counter" (state board) (Just False, 0, Just (2, 1))
        Spec.assertEqWith s "the first activation suspects it and places NOTHING" (state once) (Just True, 0, Just (2, 1))
        case activate once of
          Left n -> Spec.assertFailure s ("expected exactly one activated ability, got " <> show n)
          Right twice -> Spec.assertEqWith s "the second finds it suspected and places a counter" (state twice) (Just True, 1, Just (3, 2))

-- CR 701.60b read as a CRITERION, proved by Rune-Brand Juggler {B}{R} Creature --
-- Human Shaman 2/2 (data/cards/rune-brand-juggler.json): "When this creature
-- enters, suspect up to one target creature you control. {3}{B}{R}, Sacrifice a
-- suspected creature: Target creature gets -5/-5 until end of turn."
--
-- The Filter atom rides a CR 701.21a sacrifice cost, so the designation decides
-- both whether the ability can be activated at all and which permanent pays for
-- it. Boggart Brute is on the board in both cases below, and it is what separates
-- the designation from what CR 701.60c hangs off it: its menace is PRINTED and it
-- is never suspected, so a criterion reading the menace grant rather than the
-- designation would offer it as fodder.
runeBrandJugglerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
runeBrandJugglerSpec s registry = Spec.describe s "RuneBrandJuggler" $ do
  Spec.it s "CR 701.60b the cost takes the suspected creature, and the menace one is not a candidate" $ do
    (jugglerId, pikerId, bruteId, wallId, gs0) <- jugglerBoard s registry
    let entered = S.runPure (takingTargets 1 [pikerId]) gs0 (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
    -- The board the activation happens on: exactly one of alice's three creatures
    -- is suspected, so every assertion below has a same-board counterexample.
    Spec.assertEqWith s "the Piker is suspected, the Brute and the Juggler are not" (fmap (`suspectedOf` entered) [pikerId, bruteId, jugglerId]) [Just True, Just False, Just False]
    Spec.assertEqWith s "and the Brute's menace is printed rather than the designation's" (Projection.hasKeyword Keyword.Menace bruteId entered, suspectedOf bruteId entered) (True, Just False)
    case Activate.abilitiesFor jugglerId entered of
      [ability] -> do
        Spec.assertBool s (Activate.activatable S.alice jugglerId ability entered) "a suspected creature to sacrifice makes it activatable"
        let after = S.runPure (jugglerAnswer wallId bruteId) entered (Activate.activateAbility S.alice jugglerId ability >> Stack.resolveTop >> Engine.settleForPriority)
        -- The interpreter asks for the BRUTE whenever a sacrifice is on offer, and
        -- CR 701.21a's prompt is raised only above one candidate -- so a criterion
        -- that dropped the designation would sacrifice the Brute here, and a
        -- criterion that read menace would sacrifice it instead of the Piker.
        Spec.assertBool s (not (S.onBattlefield pikerId after)) "the suspected creature paid the cost"
        Spec.assertEqWith s "and reached alice's graveyard (CR 701.21a)" (fmap (`S.soleFaceName` after) (Game.zoneMembers Zone.Graveyard S.alice after)) [CardName.MkCardName $ Text.pack "Goblin Piker"]
        Spec.assertBool s (S.onBattlefield bruteId after) "the creature with menace and no designation did not"
        Spec.assertBool s (S.onBattlefield jugglerId after) "and neither did the Juggler"
        Spec.assertEqWith s "before: the Wall is a 0/8" (S.powerToughnessOf wallId entered) (Just (0, 8))
        Spec.assertEqWith s "after: the ability resolved for -5/-5" (S.powerToughnessOf wallId after) (Just (-5, 3))
      other -> Spec.assertFailure s ("expected exactly one activated ability on the Juggler, got " <> show (length other))
  -- The same board, the same lands and the same three creatures; the one
  -- difference is CR 115.6's announcement, which leaves nothing suspected.
  Spec.it s "CR 701.60b with nothing suspected the cost cannot be paid, though the creatures and the mana are the same" $ do
    (jugglerId, pikerId, bruteId, _, gs0) <- jugglerBoard s registry
    let entered = S.runPure decliningTargets gs0 (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
    Spec.assertEqWith s "the ETB was declined, so no creature is suspected" (fmap (`suspectedOf` entered) [pikerId, bruteId, jugglerId]) [Just False, Just False, Just False]
    case Activate.abilitiesFor jugglerId entered of
      [ability] -> do
        -- NOT a mana or a timing failure: the case above answers True for the same
        -- ability, on the same five lands, with the same three creatures and the
        -- same target available -- the designation is the only thing that moved.
        Spec.assertBool s (not (Activate.activatable S.alice jugglerId ability entered)) "three unsuspected creatures are not candidates"
      other -> Spec.assertFailure s ("expected exactly one activated ability on the Juggler, got " <> show (length other))

-- Rune-Brand Juggler entering under alice, who also controls the Goblin Piker its
-- trigger will suspect and a Boggart Brute it will not, plus exactly the {3}{B}{R}
-- the activated ability costs (four Swamps and a Mountain). bob's Wall of Stone is
-- the ability's target, a 0/8 so that -5/-5 is legible without CR 704.5f taking it
-- away.
jugglerBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
jugglerBoard s registry = do
  swamp <- S.printingOf s registry "Swamp"
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  brute <- S.printingOf s registry "Boggart Brute"
  wall <- S.printingOf s registry "Wall of Stone"
  juggler <- S.printingOf s registry "Rune-Brand Juggler"
  let (_, g1) = S.addCreature mountain S.alice (S.landsInPlay swamp 4)
      (pikerId, g2) = S.addCreature piker S.alice g1
      (bruteId, g3) = S.addCreature brute S.alice g2
      (wallId, g4) = S.addCreature wall S.bob g3
      (jugglerId, g5) = S.entersWithTrigger juggler S.alice g4
      gs =
        g5
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
  pure (jugglerId, pikerId, bruteId, wallId, gs)

-- CR 701.60b's designation, read off the object.
suspectedOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Bool
suspectedOf oid gs = fmap isSuspected (Game.lookupObject oid gs)

-- CR 701.60b asked of one object, which is Set membership rather than a field.
isSuspected :: Object.Object -> Bool
isSuspected = Set.member Designation.Suspected . Object.designations

-- Aims the ability at `victim` and asks for `fodder` whenever CR 701.21a offers a
-- sacrifice choice. The fodder is deliberately the permanent the criterion must
-- NOT offer: at one candidate Prompt.ChooseSacrifices is elided, so this half of
-- the interpreter can only ever fire on a criterion that is too wide.
jugglerAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
jugglerAnswer victim fodder p = case p of
  Prompt.ChooseSacrifices {} -> sacrifices fodder p
  _ -> takingTargets 1 [victim] p

-- The library alice searches, the ids of whichever of its cards the ability
-- names, and every id in it. The last is what lets an assertion say which SUBSET
-- the search offered rather than how many.
data CookbookBoard = MkCookbookBoard
  { cookbookState :: GameState.GameState,
    cookbookIds :: [ObjectId.ObjectId],
    cookbookLibrary :: [ObjectId.ObjectId]
  }

-- Asmoranomardicadaistinaculdacar on the battlefield with its CR 603.6a enters
-- trigger pending, over a four-card library. Passing False for `withCookbooks`
-- swaps the two copies of the named card for two more Golden Eggs and changes
-- nothing else -- same library size, same card types, same seat, same phase --
-- so the pair of boards differs in the NAME and in nothing that could make a
-- search fail for another reason.
--
-- Two copies of the named card, not one: a set assertion over a single member
-- cannot tell a filter that matched by name from one that admitted every card in
-- the library. The Golden Egg is an ARTIFACT, as the Cookbook is, so the two are
-- separated by the name alone; the Mountain is the card of another type the
-- filter is also asked of.
cookbookBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m CookbookBoard
cookbookBoard s registry withCookbooks = do
  mountain <- S.printingOf s registry "Mountain"
  egg <- S.printingOf s registry "Golden Egg"
  cookbook <- S.printingOf s registry "The Underworld Cookbook"
  asmor <- S.printingOf s registry "Asmoranomardicadaistinaculdacar"
  let named = if withCookbooks then cookbook else egg
      (mountainId, g1) = S.addLibraryCard mountain S.alice (Setup.emptyGame S.bothPlayers)
      (eggId, g2) = S.addLibraryCard egg S.alice g1
      (firstId, g3) = S.addLibraryCard named S.alice g2
      (secondId, g4) = S.addLibraryCard named S.alice g3
      (_, g5) = S.entersWithTrigger asmor S.alice g4
  pure
    MkCookbookBoard
      { cookbookState = g5,
        cookbookIds = if withCookbooks then [firstId, secondId] else [],
        cookbookLibrary = [mountainId, eggId, firstId, secondId]
      }

-- Takes CR 603.5's "may", records every candidate set the search offered, and
-- finds `wanted` -- PINNED, so an answerer cannot repair the assertion by going
-- looking for a legal pick after a mutation. Finds nothing when nothing is
-- pinned, which is the negative board's answer.
cookbookAnswer :: [ObjectId.ObjectId] -> Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
cookbookAnswer wanted p = case p of
  Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
  Prompt.SearchLibrary _ _ matches _ -> do
    State.modify' (\searches -> searches <> [matches])
    pure wanted
  Prompt.Shuffle library -> pure library
  _ -> pure (S.identityAnswer p)

-- Settle the pending enters trigger onto the stack and resolve it, keeping both
-- the candidate sets the search offered and the board it left behind.
runCookbook :: [ObjectId.ObjectId] -> CookbookBoard -> ([[ObjectId.ObjectId]], GameState.GameState)
runCookbook wanted board =
  let ((_, gs), searches) =
        State.runState
          (Engine.runGame (cookbookAnswer wanted) (cookbookState board) Engine.priorityLoop)
          []
   in (searches, gs)

-- CR 201.2 makes a name a characteristic like any other, and CR 709.4a fixes the
-- test as membership. Proved by Asmoranomardicadaistinaculdacar
-- (data/cards/asmoranomardicadaistinaculdacar.json): "When
-- Asmoranomardicadaistinaculdacar enters, you may search your library for a card
-- named The Underworld Cookbook, reveal it, put it into your hand, then shuffle."
-- The Underworld Cookbook (data/cards/the-underworld-cookbook.json) is the other
-- half of the pair.
cookbookSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cookbookSpec s registry = Spec.describe s "TheUnderworldCookbook" $ do
  Spec.it s "CR 201.2 the search offers exactly the cards with the printed name" $ do
    board <- cookbookBoard s registry True
    let (searches, _) = runCookbook (take 1 (cookbookIds board)) board
    Spec.assertEqWith s "the library holds four cards" (length (cookbookLibrary board)) 4
    -- Identity, not count: BOTH Cookbooks and neither the Golden Egg nor the
    -- Mountain. A filter that admitted every artifact, or every card, fails here
    -- rather than at the find below.
    Spec.assertEqWith s "one search, offering both Cookbooks and nothing else" (fmap Set.fromList searches) [Set.fromList (cookbookIds board)]
  Spec.it s "CR 701.23e the found card is revealed and put into its owner's hand" $ do
    board <- cookbookBoard s registry True
    let (_, after) = runCookbook (take 1 (cookbookIds board)) board
        cookbookName = CardName.MkCardName $ Text.pack "The Underworld Cookbook"
    -- By NAME rather than by the id that was pinned: CR 400.7 mints a new object
    -- when the card leaves the library, so the card in the hand is not the id the
    -- search was answered with. The identity assertion is the candidate-set case
    -- above; this one is about where the card ended up.
    Spec.assertEqWith s "the named card, and only it, is in alice's hand" (namesIn Zone.Hand S.alice after) [Just cookbookName]
    -- One Cookbook was taken and the other was not, which a "found everything the
    -- filter admitted" reading would fail.
    Spec.assertEqWith s "the other three cards stayed in the library" (length (namesIn Zone.Library S.alice after)) 3
    Spec.assertEqWith s "one of them still the second Cookbook" (length (filter (== Just cookbookName) (namesIn Zone.Library S.alice after))) 1
  Spec.it s "CR 701.23b nothing is offered when no card in the library has the name" $ do
    -- The same board with the two Cookbooks swapped for Golden Eggs. The search
    -- still happens -- the trigger resolves and the library is still read -- so
    -- an empty offer is the filter's answer rather than a step that never ran.
    board <- cookbookBoard s registry False
    let (searches, after) = runCookbook [] board
    Spec.assertEqWith s "the library holds four cards here too" (length (cookbookLibrary board)) 4
    Spec.assertEqWith s "one search, offering nothing" searches [[]]
    Spec.assertEqWith s "and alice's hand is empty" (length (Game.zoneMembers Zone.Hand S.alice after)) 0

-- Merfolk Spy is the card: {U} Creature -- Merfolk Rogue 1/1, "Islandwalk /
-- Whenever this creature deals combat damage to a player, that player reveals a
-- card at random from their hand." It is the pool's first Effect.Reveal inside a
-- triggered ability, and the first over somebody else's hand.
--
-- "At random" is not a property of any one outcome, since the engine does not
-- roll: it is "asked Prompt.RandomObject over the whole hand and honoured the
-- answer". So no single board can tell a random pick from a fixed one, and the
-- PAIR of boards below -- identical but for the index the interpreter answered
-- with -- is the whole proof. Revealing the head unasked passes the second and
-- fails the first.
randomRevealSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
randomRevealSpec s registry =
  let -- alice attacks with an unblocked Spy; bob holds `cards` and no blocker.
      -- THREE distinct printings in the hand, stocked in this order, so index 0
      -- is the first and index 2 the last (Game.zoneMembers hands back the
      -- zone's own order, CR 400.5). Three rather than one because a one-card
      -- hand elides the ask entirely, and rather than two because two would make
      -- the pinned index a coin flip.
      board spy cards = case S.combatBoardOf [spy] [] of
        (base, ours, yours) -> (ours, yours, List.foldl' (\g p -> snd (S.addHandCard p S.bob g)) base cards)
      -- Answers Prompt.RandomObject by INDEX into the offer, attacking with
      -- everything otherwise. Pinned by index rather than read off the prompt's
      -- fields: an answerer that hunted for "a legal card" would go on answering
      -- legally after a mutation broke which card the engine honours, and these
      -- cases would stay green over it.
      rolling :: Int -> Prompt.Prompt r -> r
      rolling i p = case p of
        Prompt.RandomObject offered -> case List.drop (min i (length (NonEmpty.toList offered) - 1)) (NonEmpty.toList offered) of
          h : _ -> h
          [] -> NonEmpty.head offered
        _ -> S.aggressiveAnswer p
      -- Who showed what, by name. The SEAT is asserted alongside the name in
      -- every leg: rule 701.20a's shower is the player carrying out the
      -- instruction, which here is bob and not the trigger's controller alice,
      -- and two seats are what tell those two readings apart.
      revealed gs = fmap (fmap (List.sort . fmap (Text.unpack . CardName.unwrap) . Set.toList)) (S.revealsOf gs)
      spyCards = ["Merfolk Spy", "Goblin Piker", "Bog Wraith", "Bird Maiden"]
   in Spec.describe s "RandomReveal" $ do
        Spec.it s "CR 701.20a a random reveal shows the card randomness named, not the first in hand" $ do
          ps <- traverse (S.printingOf s registry) spyCards
          case ps of
            [spy, piker, wraith, maiden] -> case board spy [piker, wraith, maiden] of
              ([attacker], [], gs) -> do
                let after = S.runCombat (rolling 2) gs
                Spec.assertBool s (S.onBattlefield attacker after) "the unblocked Spy survived combat"
                Spec.assertEqWith s "CR 510.1b: its one damage reached bob" (S.lifeOf S.bob after) (Just 19)
                Spec.assertEqWith s "the LAST card in bob's hand was revealed, by bob" (revealed after) [(S.bob, ["Goblin Piker"])]
                Spec.assertEqWith s "CR 701.20b: revealing moved nothing" (S.handSize S.bob after) 3
              _ -> Spec.assertFailure s "fixture should give alice one attacker and bob none"
            _ -> Spec.assertFailure s "four printings"
        -- The other half of the pair: the same board, the same everything, one
        -- different answer.
        Spec.it s "CR 701.20a the same board with a different roll reveals a different card" $ do
          ps <- traverse (S.printingOf s registry) spyCards
          case ps of
            [spy, piker, wraith, maiden] -> case board spy [piker, wraith, maiden] of
              ([_], [], gs) -> do
                let after = S.runCombat (rolling 0) gs
                Spec.assertEqWith s "the FIRST card this time" (revealed after) [(S.bob, ["Bird Maiden"])]
                Spec.assertEqWith s "and nothing moved either way" (S.handSize S.bob after) 3
              _ -> Spec.assertFailure s "fixture should give alice one attacker and bob none"
            _ -> Spec.assertFailure s "four printings"
        Spec.it s "CR 701.20a and the middle card, which no fixed reading of the hand reaches" $ do
          ps <- traverse (S.printingOf s registry) spyCards
          case ps of
            [spy, piker, wraith, maiden] -> case board spy [piker, wraith, maiden] of
              ([_], [], gs) -> do
                let after = S.runCombat (rolling 1) gs
                Spec.assertEqWith s "bob showed the Wraith" (revealed after) [(S.bob, ["Bog Wraith"])]
              _ -> Spec.assertFailure s "fixture should give alice one attacker and bob none"
            _ -> Spec.assertFailure s "four printings"
        -- CR 101.3 and CR 609.3 at both ends. S.aggressiveAnswer is deliberate
        -- here and nowhere above: it bottoms out in Replay.defaultAnswer, which
        -- answers a RandomObject with the head of the offer -- so this leg
        -- asserts only that a reveal happened, never which card it was.
        Spec.it s "CR 609.3 a one-card hand is revealed with nothing to determine, and an empty hand reveals nothing" $ do
          spy <- S.printingOf s registry "Merfolk Spy"
          piker <- S.printingOf s registry "Goblin Piker"
          case board spy [piker] of
            ([_], [], one) -> case board spy [] of
              ([_], [], none) -> do
                Spec.assertEqWith s "the lone card was revealed" (revealed (S.runCombat S.aggressiveAnswer one)) [(S.bob, ["Goblin Piker"])]
                Spec.assertEqWith s "an empty hand reveals nothing" (revealed (S.runCombat S.aggressiveAnswer none)) []
              _ -> Spec.assertFailure s "fixture should give alice one attacker and bob none"
            _ -> Spec.assertFailure s "fixture should give alice one attacker and bob none"
        -- CR 104.4b, GameSpec's lastChoiceSpec one effect over: being asked for
        -- randomness is not being offered a CHOICE, so the ask must go through
        -- Game.ask and leave GameState.lastChoice alone -- otherwise a loop
        -- containing a random reveal would look interruptible and
        -- Engine.checkMandatoryLoop could never call it a draw. Driven through
        -- Resolve.applyEffect rather than combat because declaring attackers is
        -- itself a choice and would stamp the field either way.
        Spec.it s "CR 104.4b a random reveal is not an optional action" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          wraith <- S.printingOf s registry "Bog Wraith"
          maiden <- S.printingOf s registry "Bird Maiden"
          let base = List.foldl' (\g p -> snd (S.addHandCard p S.bob g)) (Setup.emptyGame S.bothPlayers) [piker, wraith, maiden]
              gs = base {GameState.lastChoice = Timestamp.MkTimestamp 0}
              effect = Effect.Reveal (Reveal.MkReveal (ObjectRef.RandomCardInHand (PlayerRef.Relative PlayerRelation.Opponent)) Nothing)
              after = S.runPure (rolling 1) gs (Resolve.applyEffect S.noSource S.noSource S.alice Map.empty Map.empty effect)
          Spec.assertEqWith s "the roll was honoured here too" (revealed after) [(S.bob, ["Bog Wraith"])]
          Spec.assertEqWith s "and nobody was recorded as having been offered a choice" (GameState.lastChoice after) (Timestamp.MkTimestamp 0)

-- Wild Evocation is the card: {5}{R} Enchantment, "At the beginning of each
-- player's upkeep, that player reveals a card at random from their hand. If it's
-- a land card, the player puts it onto the battlefield. Otherwise, the player
-- casts it without paying its mana cost if able."
--
-- The pool's only producer of CR 608.2g's "INSTRUCTS" half, and its only
-- OfferCast whose caster is somebody other than the resolving controller. Both
-- readings need two seats to separate, so ALICE controls the enchantment and BOB
-- takes the upkeep: a one-seat board would make "that player casts it" and "the
-- resolving controller casts it" agree, and would make "each player's upkeep"
-- indistinguishable from "your upkeep".
--
-- The revealed card is bound and read back twice -- once by Filter.IsBound in
-- each branch's condition, once by ObjectRef.InSlot / OfferCast's slot -- which
-- is what makes Effect.Reveal's slot load-bearing rather than decoration.
--
-- "If it's a land card" is counted over EVERY player's hand rather than over the
-- upkeep player's, and the two are the same question: the Filter.IsBound
-- conjunct already names exactly one card, so the scope only has to reach it. A
-- Count over a slot-named player's zone is unanswerable from a trigger today
-- (gap #1783), which is why it is not spelled the other way.
wildEvocationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wildEvocationSpec s registry =
  let -- alice controls the enchantment; bob holds `cards` in the order given, so
      -- index 0 is the first (CR 400.5, through Game.zoneMembers). S.addHandCard
      -- puts its card at the FRONT of the hand, so the last named is stocked
      -- first.
      board evocation cards =
        let (_, withEvocation) = S.addCreature evocation S.alice (Setup.emptyGame S.bothPlayers)
         in List.foldl' (\g p -> snd (S.addHandCard p S.bob g)) withEvocation (reverse cards)
      -- BOB's upkeep, stamped and recorded: the half TurnScope.EachTurn buys,
      -- since under ControllersTurn the trigger would not fire here at all.
      atBobsUpkeep gs =
        let upkeep = Phase.Beginning BeginningStep.Upkeep
         in Event.recordEvent
              (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.bob))
              (gs {GameState.phase = upkeep, GameState.activePlayer = S.bob})
      runBobsUpkeep :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      runBobsUpkeep answer gs =
        let began = atBobsUpkeep gs
            settled = S.runPure answer began Engine.settleForPriority
         in S.runPure answer settled Engine.priorityLoop
      -- Bob's library, which `board` leaves empty. LOAD-BEARING for every leg
      -- whose cast resolves a draw: CR 704.5b makes a draw from an empty library
      -- lose bob the game at the next SBA check, which on a two-seat board hands
      -- alice the game (CR 104.2a) and leaves the assertions reading a finished
      -- one.
      withLibrary ps gs = List.foldl' (\g p -> snd (S.addLibraryCard p S.bob g)) gs ps
      -- randomRevealSpec's answerer, pinned by INDEX for its reason: one that
      -- hunted the offer for a castable card would go on answering legally after
      -- a mutation broke which card the engine honours.
      rolling :: Int -> Prompt.Prompt r -> r
      rolling i p = case p of
        Prompt.RandomObject offered -> case List.drop (min i (length (NonEmpty.toList offered) - 1)) (NonEmpty.toList offered) of
          h : _ -> h
          [] -> NonEmpty.head offered
        _ -> S.identityAnswer p
      -- The same, counting every question the offer puts to a player: CR 608.2g's
      -- "allows" half, and CR 709.3's half-choice beside it. BOTH, so that "nobody
      -- was asked" means what it says on a board where the offered card has two
      -- castable halves.
      --
      -- Threaded through State rather than pinned, since two OfferedCast prompts
      -- over one board are structurally identical and a pure answerer could not
      -- tell them apart.
      offersUnder :: Int -> GameState.GameState -> Int
      offersUnder i gs =
        let counting :: Prompt.Prompt r -> State.State Int r
            counting p = case p of
              Prompt.OfferedCast {} -> do
                State.modify (+ 1)
                pure (S.identityAnswer p)
              Prompt.ChooseOfferedCastFace {} -> do
                State.modify (+ 1)
                pure (S.identityAnswer p)
              _ -> pure (rolling i p)
         in State.execState
              (Engine.runGame counting (atBobsUpkeep gs) (Engine.settleForPriority >> Engine.priorityLoop))
              0
      -- The halves CR 709.3's prompt actually offered, one entry per prompt
      -- raised, in the order the prompt carried them.
      halvesOffered :: GameState.GameState -> [[CardName.CardName]]
      halvesOffered gs =
        let recording :: Prompt.Prompt r -> State.State [[CardName.CardName]] r
            recording p = case p of
              Prompt.ChooseOfferedCastFace _ _ _ options -> do
                State.modify (<> [NonEmpty.toList options])
                pure (NonEmpty.head options)
              _ -> pure (rolling 0 p)
         in State.execState
              (Engine.runGame recording (atBobsUpkeep gs) (Engine.settleForPriority >> Engine.priorityLoop))
              []
      -- Test B's answerer, pinning CR 709.3's half BY NAME. Returns the wanted
      -- name whether or not it was offered: Resolve.offerCast rejects rather than
      -- repairs, so a leg whose half stopped being offered goes red instead of
      -- quietly casting the other one.
      choosingHalf :: CardName.CardName -> Prompt.Prompt r -> r
      choosingHalf want p = case p of
        Prompt.ChooseOfferedCastFace {} -> want
        _ -> rolling 0 p
      -- The same again, TAKING the offer. S.identityAnswer bottoms out in
      -- Replay.defaultAnswer, whose Prompt.OfferedCast arm declines, so a leg
      -- asserting a cast HAPPENED on an excused branch would otherwise pass
      -- because nothing happens either way.
      exercising :: Int -> Prompt.Prompt r -> r
      exercising i p = case p of
        Prompt.OfferedCast {} -> OptionalDecision.Exercises
        _ -> rolling i p
      named n = CardName.MkCardName (Text.pack n)
      -- WHO controls the permanent bob's card became, which is the one reading
      -- that separates "that player casts it" from "the resolving controller
      -- casts it". NOT the owner: S.countOnBattlefieldByName indexes the
      -- battlefield by owner (CR 108.3), and the card is bob's whoever cast it,
      -- so a count alone is green under either reading.
      controllerOfNamed n gs =
        Maybe.listToMaybe
          [ ctrl
          | oid <- Game.zoneMembers Zone.Battlefield S.bob gs,
            fmap Face.name (Game.faceOf oid gs) == Just (named n),
            ctrl <- Maybe.maybeToList (Projection.controllerOf oid gs)
          ]
      zoneOf zone gs = fmap (\oid -> maybe "?" (Text.unpack . CardName.unwrap . Face.name) (Game.faceOf oid gs)) (Game.zoneMembers zone S.bob gs)
      bobsHand = zoneOf Zone.Hand
      bobsGraveyard = zoneOf Zone.Graveyard
      revealed gs = fmap (fmap (List.sort . fmap (Text.unpack . CardName.unwrap) . Set.toList)) (S.revealsOf gs)
      -- Three distinct printings, none of them a land, so every index is a
      -- different card and the "otherwise" branch is the one taken.
      spells = ["Goblin Piker", "Bog Wraith", "Bird Maiden"]
   in Spec.describe s "WildEvocation" $ do
        -- The unit's whole point. Rule 608.2g's "instructs" is not a decision, so
        -- the cast happens with no Prompt.OfferedCast raised at all -- and it is
        -- BOB's cast, which the seat the permanent lands under is what says.
        Spec.it s "CR 608.2g a mandatory offer casts without asking, and the caster is the named player" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          ps <- traverse (S.printingOf s registry) spells
          let gs = board evocation ps
              after = runBobsUpkeep (rolling 0) gs
          Spec.assertEqWith s "CR 608.2g: BOB controls the resulting permanent, not alice who controls the enchantment" (controllerOfNamed "Goblin Piker" after) (Just S.bob)
          Spec.assertEqWith s "and it really did resolve" (S.countOnBattlefieldByName (named "Goblin Piker") S.bob after) 1
          Spec.assertEqWith s "the other two are still in hand" (bobsHand after) ["Bog Wraith", "Bird Maiden"]
          Spec.assertEqWith s "bob showed the card he cast" (revealed after) [(S.bob, ["Goblin Piker"])]
          Spec.assertEqWith s "stack empty: the trigger and the spell both resolved" (length (GameState.stack after)) 0
          Spec.assertEqWith s "and nobody was asked whether to cast" (offersUnder 0 gs) 0
        -- The same board, one different roll: the cast is of the card the reveal
        -- NAMED rather than of whatever the hand happens to hold. Without this
        -- leg an implementation casting the head of the hand passes the case
        -- above.
        Spec.it s "CR 608.2g the card cast is the one the reveal bound" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          ps <- traverse (S.printingOf s registry) spells
          let after = runBobsUpkeep (rolling 2) (board evocation ps)
          Spec.assertEqWith s "the Maiden resolved onto bob's battlefield" (S.countOnBattlefieldByName (named "Bird Maiden") S.bob after) 1
          Spec.assertEqWith s "and the Piker never left bob's hand" (bobsHand after) ["Goblin Piker", "Bog Wraith"]
        -- The land branch, and the pair that proves the branch reads the BOUND
        -- card rather than the zone. One Forest sits in the hand in both legs,
        -- so an implementation counting lands in the hand passes the first and
        -- fails the second.
        --
        -- The nonland is a Lightning Bolt rather than a creature deliberately:
        -- a creature is on the battlefield either way, so it cannot tell a CAST
        -- from the land branch's MoveToZone. The Bolt separates them -- cast, it
        -- resolves, deals its damage and reaches a graveyard (CR 608.2n); moved,
        -- it would sit on the battlefield with alice's life untouched.
        Spec.it s "CR 701.20a a revealed land is put onto the battlefield instead of cast" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          forest <- S.printingOf s registry "Forest"
          bolt <- S.printingOf s registry "Lightning Bolt"
          maiden <- S.printingOf s registry "Bird Maiden"
          let gs = board evocation [forest, bolt, maiden]
              after = runBobsUpkeep (rolling 0) gs
          Spec.assertEqWith s "the Forest is on bob's battlefield" (S.countOnBattlefieldByName (named "Forest") S.bob after) 1
          Spec.assertEqWith s "nothing was cast: the Bolt is still in hand and nobody was burned" (bobsHand after, S.lifeOf S.alice after) (["Lightning Bolt", "Bird Maiden"], Just 20)
          Spec.assertEqWith s "and no cast was offered either" (offersUnder 0 gs) 0
        Spec.it s "CR 608.2g the same hand with a nonland revealed casts it and leaves the land alone" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          forest <- S.printingOf s registry "Forest"
          bolt <- S.printingOf s registry "Lightning Bolt"
          maiden <- S.printingOf s registry "Bird Maiden"
          let after = runBobsUpkeep (rolling 1) (board evocation [forest, bolt, maiden])
          Spec.assertEqWith s "the Bolt was CAST: it resolved and dealt its damage" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "so it reached a graveyard rather than the battlefield (CR 608.2n)" (bobsGraveyard after, S.countOnBattlefieldByName (named "Lightning Bolt") S.bob after) (["Lightning Bolt"], 0)
          Spec.assertEqWith s "and the Forest is still in hand, unplayed" (bobsHand after) ["Forest", "Bird Maiden"]
          Spec.assertEqWith s "so nothing entered the battlefield as a land" (S.countOnBattlefieldByName (named "Forest") S.bob after) 0
        -- CR 118.8's "if able", read through Cast.castableWhenOffered: a Plummet
        -- with no flier anywhere to target cannot be cast, so a MANDATORY offer
        -- is simply not made. Bob's hand is otherwise identical to the first
        -- case's, and the board carries no creature with flying.
        Spec.it s "CR 601.3 an uncastable card is not cast and raises no prompt" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          plummet <- S.printingOf s registry "Plummet"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          let gs = board evocation [plummet, piker, maiden]
              after = runBobsUpkeep (rolling 0) gs
          Spec.assertEqWith s "the Plummet is still in bob's hand" (bobsHand after) ["Plummet", "Goblin Piker", "Bird Maiden"]
          Spec.assertEqWith s "bob still showed it, though (CR 701.20a)" (revealed after) [(S.bob, ["Plummet"])]
          Spec.assertEqWith s "stack empty: the trigger resolved and did nothing" (length (GameState.stack after)) 0
          Spec.assertEqWith s "and no question was put on the wire" (offersUnder 0 gs) 0
        -- CR 118.8c, and the pair below it. Magmatic Insight's mandatory
        -- additional cost is "discard a land card": an action involving cards
        -- with a STATED QUALITY (CR 701.23b's phrase) in the HAND, CR 400.2's
        -- hidden zone. So the "if able" instruction stops being an instruction
        -- and becomes a may -- one Prompt.OfferedCast, which the stock answerer
        -- declines.
        --
        -- The Forest is in hand throughout, so the cost is payable: this is CR
        -- 118.8c's "even if those cards are present in that zone" and not the CR
        -- 601.3 guard the Plummet leg above exercises. The leg after next proves
        -- payability by taking the offer on this very board.
        Spec.it s "CR 118.8c a mandatory cast whose cost names a card of a stated quality in hand is offered, not forced" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          insight <- S.printingOf s registry "Magmatic Insight"
          forest <- S.printingOf s registry "Forest"
          piker <- S.printingOf s registry "Goblin Piker"
          wraith <- S.printingOf s registry "Bog Wraith"
          let gs = withLibrary [wraith] (board evocation [insight, forest, piker])
              after = runBobsUpkeep (rolling 0) gs
          Spec.assertEqWith s "bob was ASKED, which is the whole of CR 118.8c" (offersUnder 0 gs) 1
          Spec.assertEqWith s "and declining left the Insight in hand with the land undiscarded" (bobsHand after) ["Magmatic Insight", "Forest", "Goblin Piker"]
          Spec.assertEqWith s "so nothing was discarded and nothing resolved" (bobsGraveyard after) []
          Spec.assertEqWith s "bob still showed the card (CR 701.20a runs before the offer)" (revealed after) [(S.bob, ["Magmatic Insight"])]
          Spec.assertEqWith s "stack empty: the trigger resolved and the cast was declined" (length (GameState.stack after)) 0
          Spec.assertEqWith s "and no card was drawn, so the library is untouched" (length (Game.zoneMembers Zone.Library S.bob after)) 1
        -- The discriminating negative, one Filter apart. Cathartic Reunion's cost
        -- is "discard two cards" -- the same component, the same hidden zone, but
        -- a bare QUANTITY (CR 701.23d), which states no quality. So CR 118.8c does
        -- not reach it and the cast stays mandatory. This is what proves the
        -- classification reads the component's own criterion rather than which
        -- card is being cast.
        Spec.it s "CR 118.8c a cost naming a bare quantity of cards is not excused" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          reunion <- S.printingOf s registry "Cathartic Reunion"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          wraith <- S.printingOf s registry "Bog Wraith"
          bolt <- S.printingOf s registry "Lightning Bolt"
          plummet <- S.printingOf s registry "Plummet"
          let gs = withLibrary [wraith, bolt, plummet] (board evocation [reunion, piker, maiden])
              after = runBobsUpkeep (rolling 0) gs
          Spec.assertEqWith s "nobody was asked: this cast is still an instruction" (offersUnder 0 gs) 0
          Spec.assertEqWith s "the two discards joined the Reunion in the graveyard (CR 608.2n)" (List.sort (bobsGraveyard after)) ["Bird Maiden", "Cathartic Reunion", "Goblin Piker"]
          Spec.assertEqWith s "and the three drawn cards are what bob holds, so he did not deck himself (CR 704.5b)" (List.sort (bobsHand after)) ["Bog Wraith", "Lightning Bolt", "Plummet"]
          Spec.assertEqWith s "stack empty: the trigger and the Reunion both resolved" (length (GameState.stack after)) 0
        -- The payability witness for the first leg: the SAME board, the offer
        -- taken. Without it "bob was asked" would be consistent with an excuse
        -- gated on the cost being unpayable, which is a different rule (CR 601.3,
        -- the Plummet leg above) that raises no prompt at all.
        Spec.it s "CR 118.8c the excused cast can still be taken, and the stated-quality cost paid" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          insight <- S.printingOf s registry "Magmatic Insight"
          forest <- S.printingOf s registry "Forest"
          piker <- S.printingOf s registry "Goblin Piker"
          wraith <- S.printingOf s registry "Bog Wraith"
          bolt <- S.printingOf s registry "Lightning Bolt"
          let gs = withLibrary [wraith, bolt] (board evocation [insight, forest, piker])
              after = runBobsUpkeep (exercising 0) gs
          Spec.assertEqWith s "the Forest was discarded to pay the cost, and the Insight resolved after it" (List.sort (bobsGraveyard after)) ["Forest", "Magmatic Insight"]
          Spec.assertEqWith s "bob drew two, so the spell really resolved" (List.sort (bobsHand after)) ["Bog Wraith", "Goblin Piker", "Lightning Bolt"]
          Spec.assertEqWith s "and nothing was put onto the battlefield as a land" (S.countOnBattlefieldByName (named "Forest") S.bob after) 0
          Spec.assertEqWith s "stack empty: the trigger and the Insight both resolved" (length (GameState.stack after)) 0
        -- CR 609.3 at the empty end: the reveal names nothing, so the slot goes
        -- unbound, Resolve.slotOne answers Nothing and the "otherwise" clause --
        -- whose AtMost 0 count DOES hold over an empty hand -- offers nothing.
        -- A regression fence rather than a proof: no mutation of this change
        -- makes it fail on its own.
        Spec.it s "CR 609.3 an empty hand reveals nothing and casts nothing" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          let after = runBobsUpkeep (rolling 0) (board evocation [])
          Spec.assertEqWith s "nothing was revealed" (revealed after) []
          Spec.assertEqWith s "bob's hand is still empty" (bobsHand after) []
          Spec.assertEqWith s "stack empty: the trigger resolved" (length (GameState.stack after)) 0
        -- CR 709.3a / 715.3a: the offer is evaluated PER HALF, so a prohibition
        -- that stops one half leaves the other on offer. Void Winnower forbids
        -- bob casting a spell with an even mana value; Embereth Shieldbreaker's
        -- {1}{R} is 2 and its Adventure half Battle Display's {R} is 1, so exactly
        -- one half survives -- and one legal option is one outcome, so CR 709.3's
        -- choice is not put to anyone.
        --
        -- The Bonesplitter is alice's, and it is what makes Battle Display's
        -- "destroy target artifact" fillable (CR 601.2c): without it the Adventure
        -- half would be gated out too and the case would prove nothing.
        Spec.it s "CR 709.3a an offered cast reaches the half the front face's prohibition leaves alone" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          winnower <- S.printingOf s registry "Void Winnower"
          bonesplitter <- S.printingOf s registry "Bonesplitter"
          shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          let (_, withWinnower) = S.addCreature winnower S.alice (board evocation [shieldbreaker, piker, maiden])
              (bonesplitterId, gs) = S.addCreature bonesplitter S.alice withWinnower
              after = runBobsUpkeep (rolling 0) gs
          Spec.assertEqWith s "the Adventure half was cast, and it destroyed the Bonesplitter" (S.onBattlefield bonesplitterId gs, S.onBattlefield bonesplitterId after) (True, False)
          -- BY NAME, and CR 715.4 is why the exiled card answers to the creature's
          -- name. Transcribed from Pawl.AdventureSpec's CR 715.3d case, with bob's
          -- exile because bob owns the card (CR 108.3).
          Spec.assertEqWith
            s
            "CR 715.3d: the adventurer card was exiled rather than put into a graveyard"
            (fmap (\o -> Projection.namesOf o after) (Game.zoneMembers Zone.Exile S.bob after))
            [Set.singleton (named "Embereth Shieldbreaker")]
          Spec.assertEqWith s "the front half was never cast, so no creature entered" (S.countOnBattlefieldByName (named "Embereth Shieldbreaker") S.bob after) 0
          Spec.assertEqWith s "bob's other two cards never moved" (bobsHand after) ["Goblin Piker", "Bird Maiden"]
          Spec.assertEqWith s "one surviving half is one outcome, so nothing was asked" (offersUnder 0 gs) 0
          Spec.assertEqWith s "stack empty: the trigger and the spell both resolved" (length (GameState.stack after)) 0
        -- The same board minus the Winnower, which is the pair that proves the
        -- gate above is doing the choosing rather than the fan-out always taking
        -- the last half: both halves are castable here, so CR 709.3's choice is
        -- real and is put to BOB, the player the offer names.
        --
        -- Two legs off one board differing only in the answer, so neither
        -- direction of a hard-coded pick survives.
        Spec.it s "CR 709.3 two castable halves are offered to the caster, and the answer decides which is cast" $ do
          evocation <- S.printingOf s registry "Wild Evocation"
          bonesplitter <- S.printingOf s registry "Bonesplitter"
          shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          let (bonesplitterId, gs) = S.addCreature bonesplitter S.alice (board evocation [shieldbreaker, piker, maiden])
              adventureLeg = runBobsUpkeep (choosingHalf (named "Battle Display")) gs
              creatureLeg = runBobsUpkeep (choosingHalf (named "Embereth Shieldbreaker")) gs
          Spec.assertEqWith s "the Adventure answer destroys the Bonesplitter and the creature answer leaves it" (S.onBattlefield bonesplitterId adventureLeg, S.onBattlefield bonesplitterId creatureLeg) (False, True)
          Spec.assertEqWith s "CR 608.2g: the creature half resolved under BOB's control, not alice's" (controllerOfNamed "Embereth Shieldbreaker" creatureLeg) (Just S.bob)
          Spec.assertEqWith
            s
            "CR 715.3d: only the leg that cast the Adventure exiles the card"
            ( fmap (\o -> Projection.namesOf o adventureLeg) (Game.zoneMembers Zone.Exile S.bob adventureLeg),
              Game.zoneMembers Zone.Exile S.bob creatureLeg
            )
            ([Set.singleton (named "Embereth Shieldbreaker")], [])
          Spec.assertEqWith s "exactly one half-choice, carrying both halves in printed order" (halvesOffered gs) [[named "Embereth Shieldbreaker", named "Battle Display"]]
          Spec.assertEqWith s "and that is the only question: CR 608.2g's may is not asked of a mandatory offer" (offersUnder 0 gs) 1
          Spec.assertEqWith s "stack empty in both legs" (length (GameState.stack adventureLeg), length (GameState.stack creatureLeg)) (0, 0)

-- CR 601.2c's announcement, answered with a stated number for every variable
-- slot -- where S.identityAnswer announces as many as the board allows.
announcingCount :: Natural -> Prompt.Prompt r -> r
announcingCount n p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const n) offers
  _ -> S.identityAnswer p

-- Announces `n` targets per slot and aims them at `wanted`, in that order of
-- preference. S.identityAnswer would take the least Recipients instead, which on
-- these boards is not what the assertions are about.
takingTargets :: Natural -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
takingTargets n wanted p = case p of
  Prompt.AnnounceTargets {} -> announcingCount n p
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (maybe False (\oid -> elem oid wanted) . Recipient.objectOf) sets
  _ -> S.identityAnswer p

-- CR 115.6: declines every optional slot, announcing zero targets. Everything
-- else is S.identityAnswer's answer, which for ChooseTargets fills what it is
-- offered -- so the two answerers differ in exactly one decision.
decliningTargets :: Prompt.Prompt r -> r
decliningTargets p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const 0) offers
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  cookbookSpec s registry
  investigateSpec s registry
  personOfInterestSpec s registry
  eliminateTheImpossibleSpec s registry
  repeatOffenderSpec s registry
  runeBrandJugglerSpec s registry
  randomRevealSpec s registry
  wildEvocationSpec s registry
