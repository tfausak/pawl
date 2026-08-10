{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Replacement's EntryR AsCopy arm (the CR 614.12a copy choice, run
-- from inside Pawl.Engine.Event's changeZone), the P2 copy gate (Clone), and
-- Pawl.Engine.Resolve's CreateCopy arm (CR 707.2's token copy, Cackling
-- Counterpart). Gameplay-level: Clone enters via the zone-change funnel and the
-- Counterpart is cast and resolved, and their projected characteristics are
-- asserted.
module Pawl.CopySpec where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Regenerability as Regenerability

-- The battlefield objects whose printed card is named "Clone" (their source is
-- unchanged by copying -- only their projected characteristics change).
clonesOnBattlefield :: GameState.GameState -> [ObjectId]
clonesOnBattlefield gs = filter isClone (Set.toList (GameState.battlefield gs))
  where
    isClone oid = maybe False (\f -> Face.name f == CardName.MkCardName (Text.pack "Clone")) (Game.faceOf oid gs)

cloneOnBattlefield :: GameState.GameState -> Maybe ObjectId
cloneOnBattlefield = Maybe.listToMaybe . clonesOnBattlefield

-- The highest-id (most recently entered) object in a list. Total (no partial
-- `maximum`): sort descending by Down, take the head via listToMaybe.
newest :: [ObjectId] -> Maybe ObjectId
newest = Maybe.listToMaybe . List.sortOn Ord.Down

-- Answers the as-enters copy choice with the highest-id legal target (the most
-- recently entered creature), declining only when none is legal. Delegates every
-- other prompt to S.identityAnswer.
-- Names a copy target that was never offered -- the lying interpreter #222 is
-- about. legalCopyTargets is the ONLY thing enforcing CR 614.12a's same-batch
-- exclusion, so an unchecked answer would let a Clone copy something it may not.
copyForbidden :: ObjectId -> Prompt.Prompt r -> r
copyForbidden wanted p = case p of
  Prompt.ChooseCopyTarget {} -> Just wanted
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

copyNewest :: Prompt.Prompt r -> r
copyNewest p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> newest legal
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

-- copyNewest's opposite: decline every as-enters copy choice. The token-copy
-- tests answer with this so that a token which wrongly kept its base card's own
-- `EntryR AsCopy` (Clone's) copies NOTHING and dies as a 0/0, rather than being
-- repaired into the right answer by the answerer.
declineCopy :: Prompt.Prompt r -> r
declineCopy p = case p of
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

-- The tokens on the battlefield (CR 111.6), newest first.
tokensOnBattlefield :: GameState.GameState -> [ObjectId]
tokensOnBattlefield gs = List.sortOn Ord.Down (filter (`Game.isToken` gs) (Set.toList (GameState.battlefield gs)))

-- alice casts `spell` (paying from lands already in play) and the stack top
-- resolves, then the board settles. Cast and resolution run under the same
-- answerer, so a prompt either side of the boundary is answered alike.
castAndResolve :: (forall r. Prompt.Prompt r -> r) -> Printing.Printing -> GameState.GameState -> GameState.GameState
castAndResolve answer spell board =
  let (staged, oid) = S.handOne spell board
      afterCast = S.runPure answer staged (S.cast S.alice oid)
   in resolveAndSettle answer afterCast

-- Resolve the stack top (a permanent enters -- the copy choice is now made INSIDE
-- that resolution, CR 614.12a) AND run the settle boundary (so a 0/0 Clone with
-- nothing to copy dies to the CR 704.5f state-based action), under the given
-- answerer.
resolveAndSettle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAndSettle answer gs =
  snd (Engine.runGamePure answer gs (Stack.resolveTop >> Engine.settleForPriority))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  Spec.it s "Clone copies a creature and projects its P/T (CR 707.2)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board) = S.addCreature piker S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone left the battlefield unexpectedly"
      Just cloneId -> do
        Spec.assertEqWith s "Clone's power is the Piker's" (Projection.powerOf cloneId resolved) $ Just 2
        Spec.assertEqWith s "Clone's toughness is the Piker's" (Projection.toughnessOf cloneId resolved) $ Just 1
        Spec.assertBool s (Projection.isCreatureOf cloneId resolved) "Clone is a creature"
        Spec.assertBool s (Projection.powerOf pikerId resolved == Just 2) "the copied Piker is untouched"

  Spec.it s "Clone with no creature to copy enters as a 0/0 and dies (CR 704.5f)" $ do
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, staged) = S.spellOnStack clone S.alice gs0
        resolved = resolveAndSettle copyNewest staged
    Spec.assertEqWith s "the 0/0 Clone is gone (state-based action)" (cloneOnBattlefield resolved) Nothing

  -- #222: with no creature on the battlefield there are no legal copy
  -- targets at all, so an interpreter naming one must be refused -- the Clone
  -- enters as a 0/0 and dies exactly as it does when it declines. Same
  -- fixture as the "no creature to copy" test above, so the only variable is
  -- the answer.
  Spec.it s "#222 a copy target that was never offered is refused" $ do
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, staged) = S.spellOnStack clone S.alice gs0
        phantom = ObjectId.MkObjectId 9999
        resolved = resolveAndSettle (copyForbidden phantom) staged
    Spec.assertEqWith s "the Clone copied nothing and died as a 0/0" (cloneOnBattlefield resolved) Nothing

  Spec.it s "Clone copies base P/T, not a counter-boosted P/T (CR 707.2 falsifier)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board0) = S.addCreature piker S.alice gs0
        -- Put a +1/+1 counter on the Piker: projected 3/2, base 2/1.
        board = S.addCounter CounterKind.PlusOnePlusOne 1 pikerId board0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone left the battlefield unexpectedly"
      Just cloneId -> do
        Spec.assertEqWith s "source is boosted to 3/2" (Projection.powerOf pikerId resolved) $ Just 3
        Spec.assertEqWith s "Clone copies the base 2, not 3" (Projection.powerOf cloneId resolved) $ Just 2
        Spec.assertEqWith s "Clone copies the base 1, not 2" (Projection.toughnessOf cloneId resolved) $ Just 1

  Spec.it s "Clone copies a creature's activated abilities (CR 707.2)" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, board) = S.addCreature prodigalSorcerer S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone left the battlefield unexpectedly"
      Just cloneId ->
        Spec.assertBool
          s
          (not (null (Projection.abilitiesOf cloneId resolved)))
          "Clone has the copied activated ability"

  Spec.it s "a copy of a copy resolves to the underlying creature (self-reference)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, board) = S.addCreature piker S.alice gs0
        (_, stagedA) = S.spellOnStack clone S.alice board
        afterA = resolveAndSettle copyNewest stagedA
        (_, stagedB) = S.spellOnStack clone S.alice afterA
        afterB = resolveAndSettle copyNewest stagedB
        -- Both Clones now name "Clone"; the newest (highest id) is B.
        afterBId = newest (clonesOnBattlefield afterB)
    case afterBId of
      Nothing -> Spec.assertFailure s "no Clones on the battlefield"
      Just bId -> do
        Spec.assertEqWith s "the copy-of-a-copy is a 2/1" (Projection.powerOf bId afterB) $ Just 2
        Spec.assertBool s (Projection.isCreatureOf bId afterB) "the copy-of-a-copy is a creature"

  Spec.it s "a copy survives its source leaving the battlefield (CR 707.5 lock)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board) = S.addCreature piker S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
        afterKill = S.runPure S.identityAnswer resolved (Event.destroy Regenerability.Regenerable [pikerId])
    case cloneOnBattlefield afterKill of
      Nothing -> Spec.assertFailure s "Clone should survive the source's death"
      Just cloneId -> do
        Spec.assertEqWith s "the source is gone" (Set.member pikerId (GameState.battlefield afterKill)) False
        Spec.assertEqWith s "the Clone is still a 2/1" (Projection.powerOf cloneId afterKill) $ Just 2
        Spec.assertEqWith s "the Clone is still 1 toughness" (Projection.toughnessOf cloneId afterKill) $ Just 1

  Spec.it s "Clone of Tarmogoyf copies the ABILITY, so both recompute (CR 707.2a)" $ do
    -- THE FALSIFIER for snapshotting the NUMBER: CR 707.2a says a copy
    -- acquires the abilities of the object it copies, because those values are
    -- derived from its rules text. Seeding the CDA as an evaluated integer
    -- would freeze the Clone at the graveyards' contents at the moment it
    -- entered -- P2's deferred bill, paid here.
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withBolt) = S.addGraveyardCard lightningBolt S.alice gs0
        (goyfId, board) = S.addCreature tarmogoyf S.alice withBolt
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
        -- A second card type reaches a graveyard AFTER the Clone entered.
        (_, later) = S.addGraveyardCard piker S.bob resolved
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone did not reach the battlefield"
      Just cloneId -> do
        Spec.assertEqWith s "at entry the Clone is the Goyf's 1/2" (Projection.powerOf cloneId resolved) $ Just 1
        Spec.assertEqWith s "at entry, toughness 1+1" (Projection.toughnessOf cloneId resolved) $ Just 2
        Spec.assertEqWith s "the source moves to 2" (Projection.powerOf goyfId later) $ Just 2
        Spec.assertEqWith s "and so does the COPY" (Projection.powerOf cloneId later) $ Just 2
        Spec.assertEqWith s "the copy's toughness moves too" (Projection.toughnessOf cloneId later) $ Just 3

  Spec.it s "Cackling Counterpart mints a token copy of the targeted creature (CR 707.2, CR 111.3)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    counterpart <- S.printingOf s registry "Cackling Counterpart"
    let (_, board) = S.addCreature piker S.alice (S.landsInPlay island 3)
        resolved = castAndResolve declineCopy counterpart board
    case tokensOnBattlefield resolved of
      [tokenId] -> do
        Spec.assertEqWith s "the token's name is the copied creature's" (PC.name (Projection.project tokenId resolved)) $ CardName.MkCardName (Text.pack "Goblin Piker")
        Spec.assertEqWith s "the token's power is the copied creature's" (Projection.powerOf tokenId resolved) $ Just 2
        Spec.assertEqWith s "the token's toughness is the copied creature's" (Projection.toughnessOf tokenId resolved) $ Just 1
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  -- THE PROVING TEST for the copiable stamp. The target is itself a copy, so
  -- its printed card (Clone, a 0/0 with an as-enters copy ability) and its
  -- copiable values (the Piker's) disagree -- and CR 707.2's "as modified by
  -- other copy effects" says the token takes the latter. Under declineCopy a
  -- token that fell back on the printed card is a 0/0 that CR 704.5f buries.
  Spec.it s "a token copy of a Clone copies what the Clone copies (CR 707.2)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    counterpart <- S.printingOf s registry "Cackling Counterpart"
    let (_, board) = S.addCreature piker S.bob (S.landsInPlay island 3)
        (_, staged) = S.spellOnStack clone S.alice board
        -- alice's Clone is now her only creature, so it is the Counterpart's
        -- only legal target ("target creature you control").
        withClone = resolveAndSettle copyNewest staged
        resolved = castAndResolve declineCopy counterpart withClone
    case tokensOnBattlefield resolved of
      [tokenId] -> do
        Spec.assertEqWith s "the token is named for the Piker, not the Clone" (PC.name (Projection.project tokenId resolved)) $ CardName.MkCardName (Text.pack "Goblin Piker")
        Spec.assertEqWith s "the token is a 2, not a 0" (Projection.powerOf tokenId resolved) $ Just 2
        Spec.assertEqWith s "the token is a 1, not a 0" (Projection.toughnessOf tokenId resolved) $ Just 1
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  -- CR 707.2's exclusion: counters are not copied. The falsifier for stamping
  -- the PROJECTION rather than the copiable values.
  Spec.it s "a token copy of a counter-boosted creature is the base P/T (CR 707.2)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    counterpart <- S.printingOf s registry "Cackling Counterpart"
    let (pikerId, board0) = S.addCreature piker S.alice (S.landsInPlay island 3)
        board = S.addCounter CounterKind.PlusOnePlusOne 1 pikerId board0
        resolved = castAndResolve declineCopy counterpart board
    Spec.assertEqWith s "the source is boosted to 3/2" (Projection.powerOf pikerId resolved) $ Just 3
    case tokensOnBattlefield resolved of
      [tokenId] -> do
        Spec.assertEqWith s "the token copies the base 2, not 3" (Projection.powerOf tokenId resolved) $ Just 2
        Spec.assertEqWith s "the token copies the base 1, not 2" (Projection.toughnessOf tokenId resolved) $ Just 1
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))
