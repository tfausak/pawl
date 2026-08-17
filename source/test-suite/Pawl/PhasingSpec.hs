{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Phasing (CR 702.26, "Phasing", and the CR 502.1 / 703.4a
-- turn-based action it is read from), the GameState.phasedOut field that carries
-- CR 702.26b's status, and the arm of Pawl.Engine.Engine.runTurnBasedActions that
-- runs the action ahead of CR 502.2's day/night check and CR 502.3's untap.
--
-- Gameplay-level throughout. Sandbar Crocodile is the whole fixture: its entire
-- printed text is "Phasing", so every assertion here is about rule 702.26 and not
-- about a card. Goblin Piker stands beside it in the cases that need a permanent
-- WITHOUT phasing, as the falsifier for an action that phased out the board.
--
-- Pacifism joins them for CR 702.26g's indirect half, which needs an Aura and a
-- host: it is the pool's minimal pair with the Crocodile, since its enchant pool
-- is creatures.
--
-- Reality Ripple is the second fixture, and the Effect group is everything the
-- untap step cannot reach on its own. Its whole printed text is one clause
-- ("target artifact, creature, or land phases out"), so those cases are about
-- rule 702.26 too. Bonesplitter rides with it for CR 702.26i, an Equipment being
-- an artifact (CR 301.5) and so a legal target -- which is how a DIRECTLY
-- phased-out attachment becomes reachable without the Aura-with-phasing that has
-- never been printed. Goblin Piker is its host, and its own phase-out is what
-- distinguishes CR 702.26a's "the keyword decides who leaves, never who returns".
--
-- What is NOT asserted, because no card in the pool can reach it: CR 702.26e/f's
-- continuous-effect consequences (#930), and CR 702.26n's schedule for a
-- permanent phased out under a player who has since left the game (#931) -- CR
-- 702.26k's own clause is asserted below.
module Pawl.PhasingSpec where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Phasing as Phasing
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasedOut as PhasedOut
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 502: the untap step's turn-based actions, run for `pid`. DaytimeSpec's
-- helper of the same shape, with the active player made explicit because the
-- whole of rule 502.1 turns on whose step it is.
untapStep :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
untapStep pid gs =
  S.runPure
    S.identityAnswer
    gs {GameState.activePlayer = pid}
    (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))

-- Is this permanent among the ones the game currently treats as existing? The
-- battlefield membership every other reader in the engine consults, asked
-- directly, because CR 702.26b's "treated as though it does not exist" is
-- exactly this bit.
onBattlefield :: ObjectId.ObjectId -> GameState.GameState -> Bool
onBattlefield oid gs = Set.member oid (GameState.battlefield gs)

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

zoneOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Zone.Zone
zoneOf oid gs = fmap Object.zone (Game.lookupObject oid gs)

tap :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
tap oid gs =
  gs
    { GameState.objects =
        Map.adjust (\obj -> obj {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)
    }

-- Alice controls one Sandbar Crocodile and nothing else has happened.
crocodileBoard :: Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
crocodileBoard crocodile = S.addCreature crocodile S.alice (Setup.emptyGame S.bothPlayers)

-- `owner` controls a Sandbar Crocodile, with an Aura `enchanter` controls
-- attached to it. Attached directly, which is Pawl.CombatSpec's `pacifying`
-- posture and for its reason: both printings are real, and a state fixture is
-- the only way to reach an untap step with the Aura already on.
enchantedCrocodile ::
  Printing.Printing ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  PlayerId.PlayerId ->
  GameState.GameState ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
enchantedCrocodile crocodile pacifism owner enchanter gs =
  let (crocId, withCroc) = S.addCreature crocodile owner gs
      (auraId, withAura) = S.addCreature pacifism enchanter withCroc
   in (crocId, auraId, S.attach auraId crocId withAura)

attachedHostOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Recipient.Recipient
attachedHostOf oid gs = Game.lookupObject oid gs >>= Object.attachedTo

-- Aim every target slot at `oid`, and otherwise answer as S.aggressiveAnswer does
-- -- so one answerer serves both the quiet boards and the combat ones.
--
-- FILTERS the offered set rather than building a Recipient, which is AuraSpec's
-- aimAt posture and for its reason: Reality Ripple's slot pools permanents, and a
-- hand-built recipient of a different shape than the pool offers is dropped by CR
-- 608.2b's re-read at resolution with no error to see. S.preferring takes the
-- slot's announced number, preferring the wanted candidate.
aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just oid) . Recipient.objectOf) sets
  _ -> S.aggressiveAnswer p

-- alice casts `spell` at `victim` through the real cast path and resolves it.
rippleAt :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
rippleAt victim spell gs =
  S.runPure (aimedAt victim) gs $ do
    S.cast S.alice spell
    Monad.void Stack.resolveTop

-- alice, with two Islands and a Reality Ripple in hand, plus whatever `stock`
-- adds. Two Islands is EXACTLY {1}{U}: a board that could not pay would leave the
-- spell on the stack or in hand and every assertion downstream would pass for the
-- wrong reason, which is why each case asserts the phased-in setup first.
rippleBoard ::
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState -> (a, GameState.GameState)) ->
  (a, ObjectId.ObjectId, GameState.GameState)
rippleBoard island ripple stock =
  let (ids, stocked) = stock (S.landsFor island S.alice 2 (Setup.emptyGame S.bothPlayers))
      (board, spell) = S.handOne ripple stocked
   in (ids, spell, board)

-- alice's Goblin Piker with a Bonesplitter equipping it, on a rippleBoard.
-- Attached directly rather than through CR 702.6's equip ability, which is
-- enchantedCrocodile's posture above and for its reason: both printings are real,
-- and the attachment is this fixture's premise rather than anything it asserts.
equippedBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  ((ObjectId.ObjectId, ObjectId.ObjectId), ObjectId.ObjectId, GameState.GameState)
equippedBoard island piker bonesplitter ripple =
  rippleBoard island ripple $ \gs ->
    let (host, withHost) = S.addCreature piker S.alice gs
        (equip, withEquip) = S.addCreature bonesplitter S.alice withHost
     in ((host, equip), S.attach equip host withEquip)

-- Is `oid` in CR 508.1's attacking-creatures record? Membership in Combat.attackers
-- is what Pawl.Engine.Projection reads for Filter.IsAttacking, so it is the same
-- question CR 506.4's "stops being an attacking creature" asks.
isAttacking :: ObjectId.ObjectId -> GameState.GameState -> Bool
isAttacking oid gs = Map.member oid (Combat.Type.attackers (GameState.combat gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Phasing" $ do
  phaseOutSpec s registry
  phaseInSpec s registry
  controllerSpec s registry
  indirectSpec s registry
  untapOrderSpec s registry
  nonexistenceSpec s registry
  departureSpec s registry
  effectSpec s registry
  attachedSpec s registry

-- CR 702.26b's other door: an effect that says a permanent phases out. Reality
-- Ripple throughout, whose whole printed text is "target artifact, creature, or
-- land phases out" -- so nothing here is about a card either.
effectSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
effectSpec s registry = Spec.describe s "Effect" $ do
  -- The capability itself: GameState.phasedOut written by something other than CR
  -- 502.1's turn-based action, with no phasing ability anywhere on the board.
  --
  -- The row says bob and not alice, which is CR 702.26a's "phased out under that
  -- player's control" -- alice cast the spell, bob controls the creature.
  Spec.it s "CR 702.26b Reality Ripple phases out a creature with no phasing" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    ripple <- S.printingOf s registry "Reality Ripple"
    let ((victim, bystander), spell, board) =
          rippleBoard island ripple $ \gs ->
            let (a, g1) = S.addCreature piker S.bob gs
                (b, g2) = S.addCreature piker S.bob g1
             in ((a, b), g2)
        after = rippleAt victim spell board
    Spec.assertEqWith s "setup: both of bob's Pikers are on the battlefield" (fmap (`onBattlefield` board) [victim, bystander]) [True, True]
    Spec.assertEqWith s "setup: and neither has phasing" (fmap (`Phasing.isPhasedOut` board) [victim, bystander]) [False, False]
    Spec.assertEqWith s "the one it targeted is phased out" (Phasing.isPhasedOut victim after) True
    Spec.assertEqWith s "directly, under BOB, who controls it" (Phasing.phasedOutStatus victim after) (Just (PhasedOut.Directly S.bob))
    Spec.assertEqWith s "and gone from the battlefield" (onBattlefield victim after) False
    -- The same board's falsifier: the second Piker nobody aimed at is untouched,
    -- so this is not an effect that swept the board.
    Spec.assertEqWith s "the other Piker is not" (Phasing.isPhasedOut bystander after) False
    Spec.assertEqWith s "and is still on the battlefield" (onBattlefield bystander after) True
    -- CR 702.26d: no zone change, so Object.zone is untouched and the object still
    -- exists. Both are what CR 702.26i's host test later reads.
    Spec.assertEqWith s "its zone still says battlefield" (zoneOf victim after) (Just Zone.Battlefield)
  -- CR 702.26b's "can't be affected by anything else in the game", at the one
  -- reader a spell goes through: CR 115.10's legality. A second Reality Ripple
  -- cannot aim at the permanent the first one sent away, though it can still aim
  -- at the one beside it.
  Spec.it s "CR 702.26b a phased-out permanent is not a legal target" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    ripple <- S.printingOf s registry "Reality Ripple"
    let ((victim, bystander), spell, board) =
          rippleBoard island ripple $ \gs ->
            let (a, g1) = S.addCreature piker S.bob gs
                (b, g2) = S.addCreature piker S.bob g1
             in ((a, b), g2)
        after = rippleAt victim spell board
        legalIn gs = case S.spellTargetSlot ripple of
          Nothing -> Set.empty
          Just slot -> Set.map Recipient.objectOf (Target.legalRecipients Nothing S.noSource slot gs)
    Spec.assertEqWith s "setup: it was a legal target while phased in" (Set.member (Just victim) (legalIn board)) True
    Spec.assertEqWith s "and is not once phased out" (Set.member (Just victim) (legalIn after)) False
    Spec.assertEqWith s "while the Piker beside it still is" (Set.member (Just bystander) (legalIn after)) True
  -- CR 702.26a's phase-in half applied to a permanent with NO phasing, which is
  -- the reading Pawl.Engine.Phasing.phasingIn implements -- "the keyword decides
  -- who leaves, never who returns" -- and which no board could distinguish from
  -- "only a permanent with phasing phases in" until an effect could phase one out.
  --
  -- And the schedule is BOB's, not the caster's: alice's untap step comes and goes
  -- with the Piker still away.
  Spec.it s "CR 702.26a it phases back in at its own controller's next untap step" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    ripple <- S.printingOf s registry "Reality Ripple"
    let ((victim, _), spell, board) =
          rippleBoard island ripple $ \gs ->
            let (a, g1) = S.addCreature piker S.bob gs
                (b, g2) = S.addCreature piker S.bob g1
             in ((a, b), g2)
        gone = rippleAt victim spell board
        alices = untapStep S.alice gone
        bobs = untapStep S.bob alices
    Spec.assertEqWith s "setup: it is phased out" (Phasing.isPhasedOut victim gone) True
    Spec.assertEqWith s "alice's untap step brings it back" (onBattlefield victim alices) False
    Spec.assertEqWith s "bob's does" (onBattlefield victim bobs) True
    Spec.assertEqWith s "with no row left" (Phasing.isPhasedOut victim bobs) False
    -- CR 702.26p's other side: it never had phasing, so bob's untap step does not
    -- send it straight back out again.
    Spec.assertEqWith s "and it does not phase out again at once" (Phasing.phasedOutStatus victim bobs) Nothing
  -- CR 506.4, restated as CR 702.26b's last sentence: "a permanent that phases out
  -- is removed from combat." Reality Ripple is the only route to it -- CR 502.1's
  -- action runs in the untap step, the turn's first, so nothing is ever in combat
  -- when it fires.
  --
  -- alice phases out one of her OWN two attackers, so bob's life total counts the
  -- clause: 2 instead of 4. The control is the same declaration with no Ripple
  -- cast, which is what keeps "took no damage from it" from passing because the
  -- board was never in combat at all.
  Spec.it s "CR 506.4 an attacking creature that phases out mid-combat deals no combat damage" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    ripple <- S.printingOf s registry "Reality Ripple"
    let (base, mine, _) = S.combatBoardOf [piker, piker] []
        withMana = S.landsFor island S.alice 2 base
        (spell, board) = S.addHandCard ripple S.alice withMana
    case mine of
      victim : _ -> do
        let declared = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
            rippled = S.settleSba (rippleAt victim spell declared)
            damaged = S.runPure S.aggressiveAnswer rippled (Monad.void Damage.dealCombatDamage)
            bothConnect = S.runPure S.aggressiveAnswer (S.settleSba declared) (Monad.void Damage.dealCombatDamage)
        Spec.assertEqWith s "setup: both Pikers are attacking" (fmap (`isAttacking` declared) mine) [True, True]
        Spec.assertEqWith s "the Ripple phased its target out" (Phasing.isPhasedOut victim rippled) True
        Spec.assertEqWith s "and CR 506.4 took it out of the record" (isAttacking victim rippled) False
        Spec.assertEqWith s "the other attacker is still in it" (fmap (`isAttacking` rippled) (drop 1 mine)) [True]
        Spec.assertEqWith s "so bob takes 2 rather than 4" (S.lifeOf S.bob damaged) (Just 18)
        Spec.assertEqWith s "which is the same board with no Ripple cast" (S.lifeOf S.bob bothConnect) (Just 16)
      [] -> Spec.assertFailure s "the combat board should have given alice two attackers"

indirectSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
indirectSpec s registry = Spec.describe s "Indirect" $ do
  -- CR 702.26g's first two sentences: "when a permanent phases out, any Auras,
  -- Equipment, or Fortifications attached to that permanent phase out at the
  -- same time. This alternate way of phasing out is known as phasing out
  -- 'indirectly'."
  --
  -- alice controls both halves, which is the case that keeps CR 702.26a's "that
  -- player" and the Aura's own controller from having to be told apart; the
  -- split board is the labelled case below.
  Spec.it s "CR 702.26g an Aura attached to a permanent that phases out phases out with it" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    pacifism <- S.printingOf s registry "Pacifism"
    let (crocId, auraId, board) = enchantedCrocodile crocodile pacifism S.alice S.alice (Setup.emptyGame S.bothPlayers)
        after = untapStep S.alice board
    Spec.assertEqWith s "both were on the battlefield before the step" (fmap (`onBattlefield` board) [crocId, auraId]) [True, True]
    Spec.assertEqWith s "and neither is afterwards" (fmap (`onBattlefield` after) [crocId, auraId]) [False, False]
    -- The Crocodile phased out on CR 702.26a's own schedule; the Aura, which has
    -- no phasing of its own, phased out because rule 702.26g dragged it.
    Spec.assertEqWith s "the Crocodile phased out directly" (Phasing.phasedOutStatus crocId after) (Just (PhasedOut.Directly S.alice))
    Spec.assertEqWith s "and the Aura indirectly" (Phasing.phasedOutStatus auraId after) (Just (PhasedOut.Indirectly S.alice))
  -- CR 704.5m -- "if an Aura is attached to an illegal object or player, or is
  -- not attached to an object or player, that Aura is put into its owner's
  -- graveyard" -- is what CR 702.26g exists to keep off this Aura. Without the
  -- indirect half the Aura is left on the battlefield with a host the game no
  -- longer treats as existing, and the next state-based check buries it.
  --
  -- The second Pacifism is the POSITIVE CONTROL for a negative assertion: it is
  -- attached to nothing, so rule 704.5m names it, and its arrival in the
  -- graveyard is what proves the settle pass actually ran. Without it, an
  -- assertion that the first Aura is not in the graveyard would pass just as
  -- happily against a settle that never happened.
  --
  -- Counted rather than named, because CR 400.7 mints a fresh incarnation on the
  -- way to the graveyard and the id that arrives is not the id that left. One
  -- card there is the loose Aura; two would be rule 704.5m taking the phased-out
  -- one as well, which is what this test exists to catch.
  Spec.it s "CR 704.5m does not bury an Aura that phased out with its host" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    pacifism <- S.printingOf s registry "Pacifism"
    let (_, auraId, enchanted) = enchantedCrocodile crocodile pacifism S.alice S.alice (Setup.emptyGame S.bothPlayers)
        (looseId, board) = S.addCreature pacifism S.alice enchanted
        settled = S.settleSba (untapStep S.alice board)
    Spec.assertEqWith s "alice's graveyard was empty before" (length (Game.zoneMembers Zone.Graveyard S.alice board)) 0
    Spec.assertEqWith s "and holds exactly one card after, so the pass ran" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 1
    Spec.assertEqWith s "the one it buried is the unattached Aura" (onBattlefield looseId settled) False
    Spec.assertEqWith s "the phased-out Aura is still phased out" (Phasing.isPhasedOut auraId settled) True
    Spec.assertEqWith s "still exists" (Maybe.isJust (Game.lookupObject auraId settled)) True
    Spec.assertEqWith s "and is still attached to its host" (attachedHostOf auraId settled) (attachedHostOf auraId board)
  -- CR 702.26g's last sentence: "an Aura, Equipment, or Fortification that phased
  -- out indirectly won't phase in by itself, but instead phases in along with the
  -- permanent it's attached to."
  Spec.it s "CR 702.26g the Aura phases in with its host, still attached" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    pacifism <- S.printingOf s registry "Pacifism"
    let (crocId, auraId, board) = enchantedCrocodile crocodile pacifism S.alice S.alice (Setup.emptyGame S.bothPlayers)
        gone = untapStep S.alice board
        back = untapStep S.alice gone
    Spec.assertEqWith s "both are back on the battlefield" (fmap (`onBattlefield` back) [crocId, auraId]) [True, True]
    Spec.assertEqWith s "neither has a phased-out row left" (fmap (`Phasing.isPhasedOut` back) [crocId, auraId]) [False, False]
    Spec.assertEqWith s "and the Aura is still attached to the Crocodile" (attachedHostOf auraId back) (Just (Recipient.ToCreature crocId))
    -- And CR 704.5m has nothing to say about it once it is back, which is the
    -- second half of "phases in ATTACHED".
    Spec.assertEqWith s "which a settle pass leaves alone" (elem auraId (Game.zoneMembers Zone.Graveyard S.alice (S.settleSba back))) False
  -- "Won't phase in by itself", as a board where by itself and with its host are
  -- different answers: alice's Pacifism on BOB's Crocodile. The Aura phased out
  -- under alice, so a schedule read off the stored player would bring it back at
  -- HER untap step, alone, with its host still gone.
  --
  -- Labelled separately because the split changes who CR 702.26a's "that player"
  -- is: bob controls the permanent with phasing, so it is bob's untap step that
  -- moves anything at all here.
  Spec.it s "CR 702.26g an indirectly phased-out Aura does not phase in on its own controller's schedule" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    pacifism <- S.printingOf s registry "Pacifism"
    let (crocId, auraId, board) = enchantedCrocodile crocodile pacifism S.bob S.alice (Setup.emptyGame S.bothPlayers)
        gone = untapStep S.bob board
        alices = untapStep S.alice gone
        bobs = untapStep S.bob alices
    Spec.assertEqWith s "bob's untap step took both" (fmap (`onBattlefield` gone) [crocId, auraId]) [False, False]
    Spec.assertEqWith s "the Aura phased out under alice, who controls it" (Phasing.phasedOutStatus auraId gone) (Just (PhasedOut.Indirectly S.alice))
    Spec.assertEqWith s "alice's untap step brings back neither" (fmap (`onBattlefield` alices) [crocId, auraId]) [False, False]
    Spec.assertEqWith s "and bob's brings back both" (fmap (`onBattlefield` bobs) [crocId, auraId]) [True, True]
    Spec.assertEqWith s "with the Aura still on the Crocodile" (attachedHostOf auraId bobs) (Just (Recipient.ToCreature crocId))
  -- CR 702.26j: "abilities that trigger when a permanent becomes attached or
  -- unattached from an object or player don't trigger when that permanent phases
  -- in or out." Asserted as the stronger fact that the phasing event appends
  -- NOTHING to CR 608.2i's log across either transition -- which also restates CR
  -- 702.26d's "zone-change triggers don't trigger".
  --
  -- NOT a proof of a rule 702.26j guard, because there is none to guard: pawl has
  -- no attach or unattach event and no trigger condition keyed to one (#1033), so
  -- this assertion would pass against an engine that had never heard of the rule.
  -- It is a regression fence for the day one exists.
  Spec.it s "CR 702.26j/702.26d neither transition emits an event" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    pacifism <- S.printingOf s registry "Pacifism"
    let (_, _, board) = enchantedCrocodile crocodile pacifism S.alice S.alice (Setup.emptyGame S.bothPlayers)
        gone = untapStep S.alice board
        back = untapStep S.alice gone
        events = length . GameState.events
    Spec.assertEqWith s "phasing out logged nothing" (events gone) (events board)
    Spec.assertEqWith s "and phasing in logged nothing" (events back) (events gone)

phaseOutSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phaseOutSpec s registry = Spec.describe s "PhaseOut" $ do
  -- CR 702.26a's first half and CR 702.26b's first two sentences, driven through
  -- the turn-based action: alice's untap step begins, her phased-in permanent
  -- with phasing phases out, and the game stops treating it as existing.
  Spec.it s "CR 702.26a/702.26b a permanent with phasing phases out during its controller's untap step" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        after = untapStep S.alice board
    Spec.assertEqWith s "it was on the battlefield before the step" (onBattlefield crocId board) True
    Spec.assertEqWith s "and is not afterwards" (onBattlefield crocId after) False
    Spec.assertEqWith s "its status is phased out" (Phasing.isPhasedOut crocId after) True
    Spec.assertEqWith s "under alice" (Phasing.phasedOutUnder crocId after) (Just S.alice)
    -- CR 702.26b as every OTHER reader sees it: a phased-out creature is not a
    -- creature alice controls, because the battlefield walk no longer finds it.
    Spec.assertEqWith s "alice controls no creature" (S.creaturesInPlay S.alice after) 0
  -- CR 702.26d's first sentence: "the phasing event doesn't actually cause a
  -- permanent to change zones". The object is the SAME object, still on the
  -- battlefield as far as its own zone says, and no zone change happened -- which
  -- is what keeps CR 603.6's zone-change triggers silent and stops CR 400.7
  -- minting a new incarnation.
  Spec.it s "CR 702.26d phasing out changes no zone" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        after = untapStep S.alice board
    Spec.assertEqWith s "the object still exists" (Maybe.isJust (Game.lookupObject crocId after)) True
    Spec.assertEqWith s "and its zone is still the battlefield" (zoneOf crocId after) (Just Zone.Battlefield)
    Spec.assertEqWith s "no object was created or destroyed" (Map.size (GameState.objects after)) (Map.size (GameState.objects board))
  -- The falsifier for an action that emptied the battlefield rather than reading
  -- rule 702.26a's "with phasing". Goblin Piker has no phasing and stays.
  Spec.it s "CR 702.26a a permanent without phasing does not phase out" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, board) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        after = untapStep S.alice board
    Spec.assertEqWith s "the Piker is still on the battlefield" (onBattlefield pikerId after) True
    Spec.assertEqWith s "and is not phased out" (Phasing.isPhasedOut pikerId after) False

phaseInSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phaseInSpec s registry = Spec.describe s "PhaseIn" $ do
  -- CR 702.26a's second half and CR 702.26c: the next untap step of the player it
  -- phased out under brings it back. Two of alice's untap steps in a row, which is
  -- the whole cycle the card is printed to do.
  Spec.it s "CR 702.26a/702.26c a phased-out permanent phases in at its controller's next untap step" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        gone = untapStep S.alice board
        back = untapStep S.alice gone
    Spec.assertEqWith s "it was phased out" (onBattlefield crocId gone) False
    Spec.assertEqWith s "and is back on the battlefield" (onBattlefield crocId back) True
    Spec.assertEqWith s "with no phased-out status left" (Phasing.isPhasedOut crocId back) False
    Spec.assertEqWith s "and alice controls a creature again" (S.creaturesInPlay S.alice back) 1
  -- "This all happens simultaneously" (CR 702.26a's third sentence). Both halves
  -- read the state before either writes, so the permanent this step phased OUT is
  -- not also a permanent this step phases IN. The falsifier for a sequential
  -- implementation, in which a creature with phasing would never leave at all.
  Spec.it s "CR 702.26a phasing out and phasing in happen simultaneously" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        after = Phasing.phasingEvent S.alice board
    Spec.assertEqWith s "one step out, and it stays out" (onBattlefield crocId after) False
  -- A third untap step phases it back out: the card alternates for as long as it
  -- is on the battlefield, which is what makes rule 702.26a a schedule rather than
  -- a one-shot.
  Spec.it s "CR 702.26a the cycle repeats every untap step" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        states = iterate (untapStep S.alice) board
    Spec.assertEqWith
      s
      "in, out, in, out, in"
      (fmap (onBattlefield crocId) (take 5 states))
      [True, False, True, False, True]

controllerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
controllerSpec s registry = Spec.describe s "Controller" $ do
  -- CR 702.26a scopes both halves to the player whose untap step it is: "that
  -- player controls" and "under that player's control". Bob's untap step does
  -- nothing to alice's Crocodile.
  Spec.it s "CR 702.26a another player's untap step phases nothing out" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        after = untapStep S.bob board
    Spec.assertEqWith s "alice's Crocodile is still on the battlefield" (onBattlefield crocId after) True
  -- The other direction, and the case a `phasedOut` set without the player in it
  -- would get wrong: once it has phased out under alice, BOB's untap step must
  -- leave it out. Only alice's brings it back.
  Spec.it s "CR 702.26a it phases in only under the player it phased out under" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        gone = untapStep S.alice board
        bobsTurn = untapStep S.bob gone
        alicesTurn = untapStep S.alice bobsTurn
    Spec.assertEqWith s "bob's untap step leaves it phased out" (onBattlefield crocId bobsTurn) False
    Spec.assertEqWith s "still under alice" (Phasing.phasedOutUnder crocId bobsTurn) (Just S.alice)
    Spec.assertEqWith s "and alice's brings it back" (onBattlefield crocId alicesTurn) True

untapOrderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
untapOrderSpec s registry = Spec.describe s "UntapOrder" $ do
  -- CR 502.1 / 703.4a put phasing FIRST, "before the active player untaps
  -- permanents" (CR 702.26a). A tapped Crocodile is therefore gone by the time CR
  -- 502.3's untap runs and does not untap; it phases in tapped on the following
  -- untap step, and THAT step's untap -- which the phase-in precedes -- untaps it.
  --
  -- This is the ordering falsifier: run the phasing action after the untap and the
  -- first assertion below flips to Untapped.
  Spec.it s "CR 502.1/702.26a a tapped permanent phases out before it would untap" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        tapped = tap crocId board
        gone = untapStep S.alice tapped
        back = untapStep S.alice gone
    Spec.assertEqWith s "it phased out still tapped" (tapStateOf crocId gone) (Just TapState.Tapped)
    Spec.assertEqWith s "having left before the untap" (onBattlefield crocId gone) False
    Spec.assertEqWith s "and phases in, then untaps, next time" (tapStateOf crocId back) (Just TapState.Untapped)
    Spec.assertEqWith s "back on the battlefield" (onBattlefield crocId back) True

nonexistenceSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
nonexistenceSpec s registry = Spec.describe s "Nonexistence" $ do
  -- CR 702.26b at the three predicates that ask "is this a permanent" WITHOUT
  -- walking the battlefield to find their candidates. Each takes an id and answers
  -- about it directly, so each has to carry the test itself; a phased-out
  -- permanent's Object.zone still reads Zone.Battlefield (CR 702.26d), and a
  -- predicate reading that field instead of the set would call a creature the game
  -- treats as nonexistent a legal attacker.
  --
  -- Asked off-menu, which is the only way to ask: Combat.legalAttackers and
  -- legalBlockers filter Projection.controlsGiven, which walks the battlefield
  -- set, so no menu ever offers one. The Effect group below is where a creature
  -- ALREADY in the combat record phases out, which is CR 506.4 rather than these
  -- three predicates.
  Spec.it s "CR 702.26b a phased-out creature is not a legal attacker, blocker or combat damage source" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = crocodileBoard crocodile
        attacking = board {GameState.activePlayer = S.alice}
        gone = untapStep S.alice attacking
    Spec.assertEqWith s "it could attack while phased in" (Combat.canAttack S.alice crocId attacking) True
    Spec.assertEqWith s "and cannot once phased out" (Combat.canAttack S.alice crocId gone) False
    Spec.assertEqWith s "nor block" (Combat.canBlock S.alice crocId gone) False
    Spec.assertEqWith s "and CR 506.4's liveness test says it has left" (Damage.onBattlefield crocId gone) False
    -- The falsifier for the two predicates reading Object.zone: that field is
    -- unchanged, so a version that consulted it would answer True to all three.
    Spec.assertEqWith s "even though its zone still says battlefield" (zoneOf crocId gone) (Just Zone.Battlefield)

departureSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
departureSpec s registry = Spec.describe s "Departure" $ do
  -- CR 702.26k: "phased-out permanents owned by a player who leaves the game also
  -- leave the game." One of the two rules on the far side of CR 702.26b's
  -- "except", so CR 800.4a's sweep has to name GameState.phasedOut and not only
  -- the battlefield. Three seats, because CR 800.1 ends a two-player game the
  -- moment one of them leaves and CR 800.4a never runs.
  --
  -- The falsifier for the row being left behind: without the deletion in
  -- Game.removeFromZones the object is gone from GameState.objects while its id
  -- still sits in `phasedOut`, keyed to a player who will never have another
  -- untap step.
  --
  -- Rule 702.26k's SECOND sentence -- that this causes no zone-change ability to
  -- trigger -- is asserted in Pawl.DepartureSpec, where it is the paired
  -- negative for CR 603.6c's phased-in half; the Crocodile prints no ability to
  -- observe it with.
  Spec.it s "CR 702.26k a phased-out permanent leaves the game with its owner" $ do
    crocodile <- S.printingOf s registry "Sandbar Crocodile"
    let (crocId, board) = S.addCreature crocodile S.alice (Setup.emptyGame S.threePlayers)
        gone = untapStep S.alice board
        after = S.runPure S.identityAnswer gone (Departure.leaveGame Departure.Type.Conceded S.alice)
    Spec.assertEqWith s "it was phased out under alice" (Phasing.phasedOutUnder crocId gone) (Just S.alice)
    Spec.assertEqWith s "the game continued without her" (GameState.result after) Nothing
    Spec.assertEqWith s "the object is gone" (Maybe.isJust (Game.lookupObject crocId after)) False
    Spec.assertEqWith s "and so is its phased-out row" (Phasing.isPhasedOut crocId after) False

-- CR 702.26i, the DIRECTLY phased-out attachment. Bonesplitter is the fixture and
-- Reality Ripple is what reaches it: an Equipment is an artifact (CR 301.5), so
-- rule 702.26i needs no Aura or Equipment carrying the phasing keyword -- which is
-- as well, since none has been printed.
attachedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attachedSpec s registry = Spec.describe s "Attached" $ do
  -- CR 702.26b's "treated as though it does not exist", read off the equipped
  -- creature rather than off a flag: while the Equipment is away its static ability
  -- generates nothing, so CR 613.1c's layer has no +2/+0 to apply. The projection
  -- gets that from GameState.battlefield membership without knowing phasing exists.
  Spec.it s "CR 702.26b a phased-out Equipment's static ability stops applying" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    ripple <- S.printingOf s registry "Reality Ripple"
    let ((host, equip), spell, board) = equippedBoard island piker bonesplitter ripple
        gone = rippleAt equip spell board
        back = untapStep S.alice gone
    Spec.assertEqWith s "setup: equipped, the Piker is 4/1" (Projection.powerOf host board) (Just 4)
    Spec.assertEqWith s "the Equipment phased out" (Phasing.isPhasedOut equip gone) True
    Spec.assertEqWith s "and the Piker is 2/1 again" (Projection.powerOf host gone) (Just 2)
    Spec.assertEqWith s "CR 702.26c: 4/1 once more when it phases in" (Projection.powerOf host back) (Just 4)
  -- CR 702.26h: "if an object would simultaneously phase out directly and
  -- indirectly, it just phases out indirectly." The pair that proves the tie-break
  -- is one board aimed two ways -- at the Equipment, which is CR 702.26a's direct
  -- row, and at its HOST, where CR 702.26g drags the Equipment along instead.
  Spec.it s "CR 702.26h the same Equipment phases out directly or indirectly by what was targeted" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    ripple <- S.printingOf s registry "Reality Ripple"
    let ((host, equip), spell, board) = equippedBoard island piker bonesplitter ripple
        atEquip = rippleAt equip spell board
        atHost = rippleAt host spell board
    Spec.assertEqWith s "aimed at the Equipment, it phased out directly" (Phasing.phasedOutStatus equip atEquip) (Just (PhasedOut.Directly S.alice))
    Spec.assertEqWith s "and its host stayed" (onBattlefield host atEquip) True
    Spec.assertEqWith s "aimed at the host, the Equipment phased out INDIRECTLY" (Phasing.phasedOutStatus equip atHost) (Just (PhasedOut.Indirectly S.alice))
    Spec.assertEqWith s "the host itself directly" (Phasing.phasedOutStatus host atHost) (Just (PhasedOut.Directly S.alice))
    Spec.assertEqWith s "and both are gone" (fmap (`onBattlefield` atHost) [host, equip]) [False, False]
  -- CR 702.26i's first sentence: a directly phased-out Equipment "will phase in
  -- attached to the object ... it was attached to when it phased out, if that
  -- object is still in the same zone".
  Spec.it s "CR 702.26i it phases in still attached when its host is still there" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    ripple <- S.printingOf s registry "Reality Ripple"
    let ((host, equip), spell, board) = equippedBoard island piker bonesplitter ripple
        gone = rippleAt equip spell board
        back = untapStep S.alice gone
    -- The attachment survives the trip untouched, which is what makes rule 702.26i
    -- answerable at all: nothing clears Object.attachedTo while the permanent is away.
    Spec.assertEqWith s "it is still attached while phased out" (attachedHostOf equip gone) (Just (Recipient.ToCreature host))
    Spec.assertEqWith s "it phases in" (onBattlefield equip back) True
    Spec.assertEqWith s "attached to the same Piker" (attachedHostOf equip back) (Just (Recipient.ToCreature host))
  -- CR 702.26i's second sentence: "if not, that Aura, Equipment, or Fortification
  -- phases in unattached." The host leaves the battlefield while the Equipment is
  -- away, which it can only do because CR 702.26b does not protect it -- it never
  -- phased out.
  --
  -- The paired positive is the case above, on the same board through the same
  -- helper: without it this one passes for a version that drops every attachment on
  -- the way back in.
  Spec.it s "CR 702.26i it phases in UNATTACHED when its host has left the battlefield" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    ripple <- S.printingOf s registry "Reality Ripple"
    let ((host, equip), spell, board) = equippedBoard island piker bonesplitter ripple
        gone = rippleAt equip spell board
        dead = S.runPure S.identityAnswer gone (Event.changeZone host Zone.Graveyard)
        back = untapStep S.alice dead
    Spec.assertEqWith s "setup: the Equipment phased out DIRECTLY" (Phasing.phasedOutStatus equip gone) (Just (PhasedOut.Directly S.alice))
    -- CR 400.7 mints a new object for the graveyard, so the id the attachment names
    -- stops existing -- which is what hostRemains reads as "not in the same zone".
    Spec.assertEqWith s "its host then left the battlefield" (onBattlefield host dead) False
    Spec.assertEqWith s "and stopped existing under that id" (Maybe.isJust (Game.lookupObject host dead)) False
    Spec.assertEqWith s "the Equipment still phased out meanwhile" (Phasing.isPhasedOut equip dead) True
    Spec.assertEqWith s "it phases in" (onBattlefield equip back) True
    Spec.assertEqWith s "unattached, its host being gone" (attachedHostOf equip back) Nothing
