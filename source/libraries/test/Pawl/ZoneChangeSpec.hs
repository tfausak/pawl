{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Resolve over the effects that move an object between zones or
-- change a life total: library position, drawing, losing life, and the
-- life-total exchanges. The machinery is Pawl.ResolveSpec.
module Pawl.ZoneChangeSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- alice controls `n` Swamps and holds `printing` in a main phase with priority;
-- bob controls one `foe`. Returns (foe's id, post-cast-and-resolve state).
castBlackRemovalAt :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
castBlackRemovalAt swamp printing foe =
  let base = S.landsInPlay swamp 3
      (foeId, withFoe) = S.addPermanent foe S.bob base
      (gs, spellId) = S.handOne printing withFoe
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
      resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (foeId, resolved)

-- Answers ChooseTargets by pointing every slot at bob (the opponent), otherwise
-- behaves like identityAnswer. Used to aim a player-targeting spell at bob.
atBobAnswer :: Prompt.Prompt r -> r
atBobAnswer p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> S.identityAnswer p

-- atBobAnswer with CR 701.9a's choice pinned by INDEX rather than left to the
-- identity answerer, so a discard of two takes two distinct cards.
discardAtBob :: Prompt.Prompt r -> r
discardAtBob p = case p of
  Prompt.ChooseDiscard _ _ ids n -> take (Natural.toIntSaturating n) ids
  _ -> atBobAnswer p

-- Add k cards of a printing to pid's hand (each a fresh Hand-zone object).
handCards :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
handCards printing pid k gs =
  let addOne g =
        let (printingId, gP) = Game.intern printing g
            (oid, g1) = Game.freshObjectId gP
            obj = Object.MkObject pid Nothing (Source.OfCard printingId) Zone.Hand TapState.Untapped Facing.FaceUp False False Set.empty 0 (Sickness.Settled pid) Map.empty Map.empty Map.empty Map.empty Nothing Nothing Nothing Set.empty Nothing (Timestamp.MkTimestamp 0) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Set.empty Set.empty Map.empty Map.empty False False False False False Seq.empty 0 (Mana.MkMana []) Nothing Nothing Nothing Nothing Set.empty Set.empty False Set.empty Set.empty Nothing Nothing
         in g1
              { GameState.objects = Map.insert oid obj (GameState.objects g1),
                GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand g1)
              }
   in List.foldl' (\g _ -> addOne g) gs [1 .. k]

-- Put k cards of a printing into pid's library, each on top of the last, for a
-- draw to find.
stockLibrary :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
stockLibrary printing pid k gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. k]

-- alice's upkeep begins, settled to the point where any trigger it woke is on
-- the stack (CR 603.3b) waiting to resolve.
settleAtAlicesUpkeep :: GameState.GameState -> GameState.GameState
settleAtAlicesUpkeep gs =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
   in snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)

-- Who drew, in the order they drew, read off the turn-scoped event log. CR
-- 121.1 makes a draw one library-to-hand move, and a library and a hand each
-- belong to one player, so the moved card's owner is the drawer. Any OTHER route
-- from library to hand would count here too; no fixture below has one.
drawersOf :: GameState.GameState -> [PlayerId.PlayerId]
drawersOf gs =
  let drawer zc =
        if ZoneChange.from zc == Zone.Library && ZoneChange.to zc == Zone.Hand
          then fmap Object.owner (Game.lookupObject (ZoneChange.object zc) gs)
          else Nothing
   in Maybe.mapMaybe drawer (S.zoneChangesOf gs)

-- Shahrazad and Sindbad on alice's battlefield, untapped and settled so its {T}
-- is payable (CR 302.6), over a library whose TOP card is `top` and a hand
-- holding `handLand` and `handSpell` -- two cards named neither `top` nor each
-- other, so any of the three is identifiable in the result by name alone.
--
-- `filler` sits under the drawn card twice over, which is what keeps CR 104.3c
-- out of the fixture: alice still has a library after the draw.
sindbadBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, GameState.GameState)
sindbadBoard sindbad top filler handLand handSpell =
  let (sindbadId, onBoard) = S.addPermanent sindbad S.alice (Setup.emptyGame S.bothPlayers)
      -- S.addLibraryCard puts each card ON TOP of the last, so `top` goes in
      -- after the filler.
      stocked = snd (S.addLibraryCard top S.alice (stockLibrary filler S.alice 2 onBoard))
   in (sindbadId, snd (S.addHandCard handSpell S.alice (snd (S.addHandCard handLand S.alice stocked))))

-- Activate the permanent's one activated ability for alice and resolve it. LOUD
-- rather than quiet when the card offers a different number of them: a helper
-- that silently did nothing is how this test would stay green while activating
-- no ability at all.
activateSole :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activateSole oid gs = case Activatable.abilitiesFor oid gs of
  [ability] ->
    let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability)
     in S.runPure S.identityAnswer activated Stack.resolveTop
  other -> error ("Pawl.ZoneChangeSpec: expected exactly one activated ability, got " <> show (length other))

-- alice casts the spell from her hand at the only legal target the board offers
-- and it resolves, then state-based actions run. The shared half of the two
-- Flicker of Fate cases below, so the token board and the card board differ in
-- the victim and in nothing else.
castAndSettle :: Printing.Printing -> GameState.GameState -> GameState.GameState
castAndSettle spell board =
  let (gs, spellId) = S.handOne spell board
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
   in S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))

zoneChangeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
zoneChangeSpec s registry = Spec.describe s "ZoneChange" $ do
  Spec.it s "CR 701.8 Murder destroys a normal creature into its owner's graveyard" $ do
    swamp <- S.printingOf s registry "Swamp"
    murder <- S.printingOf s registry "Murder"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, after) = castBlackRemovalAt swamp murder piker
    Spec.assertEqWith s "no creature survives" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "Piker in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
  Spec.it s "CR 700.4 Murder does nothing to an indestructible creature (destroy /= move)" $ do
    swamp <- S.printingOf s registry "Swamp"
    murder <- S.printingOf s registry "Murder"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (_, after) = castBlackRemovalAt swamp murder darksteelMyr
    -- The falsifier: modelling Destroy as MoveToZone slot Graveyard would
    -- bury the Myr. It stays; the spell still resolved and was buried.
    Spec.assertEqWith s "Myr still on the battlefield" (S.creaturesInPlay S.bob after) 1
    Spec.assertEqWith s "bob's graveyard empty" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "Murder in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  Spec.it s "CR 701.19a Murder is replaced by regeneration" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    murder <- S.printingOf s registry "Murder"
    let base = S.landsInPlay swamp 3
        (victim, withFoe) = S.addPermanent piker S.bob base
        shielded = S.addRegenShield victim withFoe
        (gs, spellId) = S.handOne murder shielded
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = S.settleSba (snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop))
    Spec.assertEqWith s "the shielded creature survived Murder" (S.creaturesInPlay S.bob after) 1
  Spec.it s "CR 400.7 Unsummon returns a creature to its owner's hand" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    let base = S.landsInPlay island 1
        (_, withPiker) = S.addPermanent piker S.bob base
        (gs, spellId) = S.handOne unsummon withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "no creature on the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "a card in bob's hand (its owner)" (S.handSize S.bob after) 1
    Spec.assertEqWith s "Unsummon in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  Spec.it s "CR 701.19a regeneration does not save a bounced creature" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    let base = S.landsInPlay island 1
        (victim, withFoe) = S.addPermanent piker S.bob base
        shielded = S.addRegenShield victim withFoe
        (gs, spellId) = S.handOne unsummon shielded
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the creature left the battlefield (bounce is not a destruction)" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "it is in bob's hand" (length (Game.zoneMembers Zone.Hand S.bob after)) 1
  Spec.it s "CR 701.13 Angelic Edict exiles a target creature" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    let base = S.landsInPlay plains 5
        (_, withPiker) = S.addPermanent piker S.bob base
        (gs, spellId) = S.handOne angelicEdict withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "no creature on the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "one card in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
  Spec.it s "CR 115 Angelic Edict may exile an enchantment (non-creature permanent)" $ do
    plains <- S.printingOf s registry "Plains"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    let base = S.landsInPlay plains 5
        -- bob controls only Rest in Peace (an enchantment, not a creature), so
        -- it is the single legal CreatureOrEnchantmentTarget.
        (ripId, withRip) = S.addPermanent restInPeace S.bob base
        (gs, spellId) = S.handOne angelicEdict withRip
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the enchantment left the battlefield" (Game.lookupObject ripId after) Nothing
    Spec.assertEqWith s "one card in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
  -- Flicker of Fate's two clauses in one resolution: the exile binds its arrival
  -- into a slot (CR 400.7j) and the return names that slot, which is what "then
  -- return IT" means. The card twin of the CR 111.8 case below, and it is what
  -- makes that case discriminating: a board where nothing comes back proves
  -- nothing on its own, since a return clause that found no object to move reads
  -- exactly the same. Here the same wire path returns the creature, so the slot
  -- binding and the second move are both known to work.
  Spec.it s "CR 400.7 Flicker of Fate returns the creature card it exiled, as a new object" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    flickerOfFate <- S.printingOf s registry "Flicker of Fate"
    let (victim, board) = S.addPermanent piker S.bob (S.landsInPlay plains 2)
        after = castAndSettle flickerOfFate board
    Spec.assertEqWith s "the creature came back onto the battlefield" (S.creaturesInPlay S.bob after) 1
    -- alice cast the spell, so a return that ignored the card's "under its
    -- owner's control" would hand bob's creature to her (CR 110.2a).
    Spec.assertEqWith s "CR 110.2a it came back under its owner's control, not the caster's" (Maybe.mapMaybe (`Projection.controllerOf` after) (Game.zoneMembers Zone.Battlefield S.bob after)) [S.bob]
    Spec.assertEqWith s "CR 400.7 and as a new object: neither the permanent that left nor the exiled incarnation is there" (Game.lookupObject victim after) Nothing
    Spec.assertEqWith s "nothing stayed in exile" (Game.zoneMembers Zone.Exile S.bob after) []
  -- CR 111.8: "A token that has left the battlefield can't move to another zone
  -- or come back onto the battlefield." Only a within-one-resolution window
  -- reaches the rule -- CR 704.5d removes such a token at the next state-based
  -- check -- and Flicker of Fate is the pool's one shape that opens it, since
  -- its second clause names the object its first clause exiled rather than "that
  -- card" (CR 111.6 would keep a token out of the latter on the card's own
  -- wording).
  --
  -- This board differs from the card twin above in exactly one thing: the victim
  -- is a token of the same printing. So "no creature came back" here cannot be
  -- the return clause missing its reference, which is what that twin rules out;
  -- it is the rule refusing the move.
  Spec.it s "CR 111.8 Flicker of Fate cannot bring back a token it exiled" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    flickerOfFate <- S.printingOf s registry "Flicker of Fate"
    let (victim, board) = S.addToken (Printing.card piker) S.bob (S.landsInPlay plains 2)
        after = castAndSettle flickerOfFate board
    Spec.assertEqWith s "no token came back onto the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "CR 111.7 it ceased to exist rather than waiting in exile" (Game.zoneMembers Zone.Exile S.bob after) []
    Spec.assertEqWith s "the token that was exiled is gone" (Game.lookupObject victim after) Nothing
  Spec.it s "CR 121.1 Divination draws its controller two cards" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    divination <- S.printingOf s registry "Divination"
    let base = S.landsInPlay island 3
        (_, g1) = S.addLibraryCard piker S.alice base
        (_, g2) = S.addLibraryCard piker S.alice g1
        (gs, spellId) = S.handOne divination g2
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "two cards drawn to hand" (S.handSize S.alice after) 2
    Spec.assertEqWith s "library emptied" (Game.zoneMembers Zone.Library S.alice after) []
  Spec.it s "CR 121.4 a Draw that outruns the library records the loss" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    divination <- S.printingOf s registry "Divination"
    let base = S.landsInPlay island 3
        (_, g1) = S.addLibraryCard piker S.alice base
        (gs, spellId) = S.handOne divination g1
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty after)) "drewFromEmpty marked"
  -- The card that proves Effect.Draw's recipient (#272): CR 121.1 says who
  -- draws, and here that is the player the spell TARGETS (CR 601.2c), not
  -- the controller who paid for it. Divination above is the same opcode
  -- pointed at `Relative You`; the two together are the falsifier for a
  -- Draw that always drew for its controller.
  Spec.it s "CR 121.1 Ancestral Recall draws three cards for the player it targets, not its controller" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    ancestralRecall <- S.printingOf s registry "Ancestral Recall"
    let base = S.landsInPlay island 1
        withLib = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.bob g)) base [1 .. (4 :: Int)]
        (gs, spellId) = S.handOne ancestralRecall withLib
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "three cards drawn to bob's hand" (S.handSize S.bob after) 3
    Spec.assertEqWith s "one card left in bob's library" (length (Game.zoneMembers Zone.Library S.bob after)) 1
    Spec.assertEqWith s "alice drew nothing" (S.handSize S.alice after) 0
  -- The card that proves Effect.Draw's `EachPlayer` arm (#276). Divination
  -- above draws for the controller alone and Ancestral Recall for one named
  -- player; Vision Skeins is the first Draw in the pool that reaches the
  -- whole table at once.
  Spec.it s "CR 121.1 Vision Skeins draws two cards for each player, its caster included" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    visionSkeins <- S.printingOf s registry "Vision Skeins"
    let base = S.landsInPlay island 2
        withLibs = stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 base)
        (gs, spellId) = S.handOne visionSkeins withLibs
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew two" (S.handSize S.alice after) 2
    Spec.assertEqWith s "bob drew two as well" (S.handSize S.bob after) 2
    Spec.assertEqWith s "no draw outran a library" (GameState.drewFromEmpty after) Set.empty
  -- CR 121.2c: "If more than one player is instructed to draw cards, the
  -- active player performs all of their draws first, then each other player
  -- in turn order does the same." The seat order the players map answers in
  -- is not that order, so this needs an active player who is not the first
  -- seat: alice casts an INSTANT on BOB's turn, which makes seat order
  -- [alice, bob, carol] and turn order [bob, carol, alice] disagree.
  --
  -- The draws are read back off the turn-scoped event log -- the same log a
  -- trigger scans (CR 603.2) -- because that is where the order of the
  -- individual draws is observable; the hand sizes alone are order-blind.
  Spec.it s "CR 121.2c Vision Skeins draws for the active player first, then in turn order" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    visionSkeins <- S.printingOf s registry "Vision Skeins"
    let -- S.landsInPlay builds its own two-seat game, so the {1}{U} goes on
        -- a three-seat board one Island at a time.
        withMana = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
        withLibs = stockLibrary piker S.carol 2 (stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 withMana))
        (gs0, spellId) = S.handOne visionSkeins withLibs
        -- handOne hands alice the turn along with the card, so bob takes the
        -- turn back. Cast.castSpell gates neither timing nor priority, but
        -- the fixture is a legal board regardless: Vision Skeins is an
        -- INSTANT, which alice may cast on bob's turn.
        gs = gs0 {GameState.activePlayer = S.bob}
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith
      s
      "bob (active) draws both of his, then carol, then the caster"
      (drawersOf after)
      [S.bob, S.bob, S.carol, S.carol, S.alice, S.alice]
    Spec.assertEqWith s "and everyone holds two" (fmap (\pid -> S.handSize pid after) [S.alice, S.bob, S.carol]) [2, 2, 2]
  -- The card that proves Effect.Draw's `Relative Opponent` arm (#276), and
  -- the one shape no "you draw" card can stand in for: Master of the Feast's
  -- trigger is a DRAWBACK, drawing for everyone except the player who
  -- controls it (CR 109.5 makes "your upkeep" that controller's).
  Spec.it s "CR 121.1 Master of the Feast's upkeep trigger draws for the opponent, not its controller" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    masterOfTheFeast <- S.printingOf s registry "Master of the Feast"
    let (_, board) = S.addPermanent masterOfTheFeast S.alice (Setup.emptyGame S.bothPlayers)
        withLibs = stockLibrary piker S.bob 1 (stockLibrary piker S.alice 1 board)
        onStack = settleAtAlicesUpkeep withLibs
        after = snd (Engine.runGamePure S.identityAnswer onStack Stack.resolveTop)
    Spec.assertBool s (not (null (GameState.stack onStack))) "the upkeep trigger really reached the stack"
    Spec.assertEqWith s "bob drew" (S.handSize S.bob after) 1
    Spec.assertEqWith s "alice, who controls it, did not" (S.handSize S.alice after) 0
    Spec.assertEqWith s "and alice's library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 1
  -- The discriminator, and it needs a THIRD seat: at two players an
  -- `Opponent` arm that reached only ONE opponent is indistinguishable from
  -- one that reaches them all. CR 806.1: in a Free-for-All the players
  -- compete as individuals, so every other player is an opponent -- CR 102.3's
  -- teammates are the one exception, and this board is played between no teams
  -- -- and both of them draw.
  Spec.it s "CR 806.1 at three seats each opponent draws off Master of the Feast, and only opponents" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    masterOfTheFeast <- S.printingOf s registry "Master of the Feast"
    let (_, board) = S.addPermanent masterOfTheFeast S.alice S.threePlayerGame
        withLibs = stockLibrary piker S.carol 1 (stockLibrary piker S.bob 1 (stockLibrary piker S.alice 1 board))
        after = snd (Engine.runGamePure S.identityAnswer (settleAtAlicesUpkeep withLibs) Stack.resolveTop)
    -- A drawer whose library was empty would draw no card and so record no
    -- zone change; this is what keeps the list below honest about that.
    Spec.assertEqWith s "no draw outran a library" (GameState.drewFromEmpty after) Set.empty
    Spec.assertEqWith s "both opponents drew, and the controller did not" (drawersOf after) [S.bob, S.carol]
  -- CR 102.1: "A player is one of the people in the game", so once CR 800.4a
  -- takes carol out, `EachPlayer` stops naming her (#279). It needs three
  -- seats twice over: CR 800.4 says only a multiplayer game -- CR 800.1's,
  -- one that BEGAN with more than two players -- continues after a
  -- departure, and a two-seat game would already have ended under CR 104.2a
  -- with nothing left to resolve.
  --
  -- drewFromEmpty is what makes this observable rather than merely tidy.
  -- CR 800.4a took carol's library out of the game with her, so a draw aimed
  -- at her finds it empty and Event.drawCard writes her seat into that set --
  -- engine state recorded for someone who is not in the game.
  Spec.it s "CR 800.4a Vision Skeins does not draw for a player who has left the game" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    visionSkeins <- S.printingOf s registry "Vision Skeins"
    let withMana = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
        withLibs = stockLibrary piker S.carol 2 (stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 withMana))
        (gs0, spellId) = S.handOne visionSkeins withLibs
        gs = S.departs Departure.Type.Conceded S.carol gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the two players still in the game drew, in APNAP order" (drawersOf after) [S.alice, S.alice, S.bob, S.bob]
    Spec.assertEqWith s "and nothing was drawn against carol's departed library" (GameState.drewFromEmpty after) Set.empty
  -- The card that proves Effect.Draw's SLOT (#1899): Shahrazad and Sindbad's
  -- "{T}: Draw a card and reveal it. If it isn't a land card, discard it."
  -- (Unknown Event; Oracle text and type line checked against
  -- api.scryfall.com). Every draw above answers "who" and "how many"; this one
  -- is the first that has to answer WHICH CARD, because the reveal and the
  -- discard both say "it" -- CR 121.1 puts the card in the hand, and the two
  -- later instructions name that card there.
  --
  -- The pair below differ in EXACTLY ONE THING, the top card of the library, so
  -- what the second board shows is the condition reading the bound card rather
  -- than a board assembled to pass. A binding that named the wrong card would
  -- reverse both: the nonland board would bury a card the draw never touched,
  -- and the land board -- whose hand holds a nonland the whole time -- would
  -- discard THAT instead of leaving the graveyard empty. A binding that named
  -- nothing empties the first board's graveyard.
  Spec.it s "CR 121.1 / 701.20a Shahrazad and Sindbad discards the nonland card it just drew, and only that card" $ do
    sindbad <- S.printingOf s registry "Shahrazad and Sindbad"
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    divination <- S.printingOf s registry "Divination"
    let (sindbadId, board) = sindbadBoard sindbad piker plains island divination
        after = activateSole sindbadId board
    Spec.assertEqWith
      s
      "the Goblin Piker it drew is what went to the graveyard"
      (namesIn Zone.Graveyard S.alice after)
      [Just (S.printingName piker)]
    Spec.assertEqWith
      s
      "and the two cards the hand already held, one of them a nonland, are untouched"
      (List.sort (namesIn Zone.Hand S.alice after))
      (List.sort [Just (S.printingName island), Just (S.printingName divination)])
    Spec.assertEqWith
      s
      "CR 701.20a it was shown to the table as it was drawn"
      (S.revealsOf after)
      [(S.alice, Set.singleton (S.printingName piker))]
  Spec.it s "CR 121.1 the same ability keeps a LAND it drew, so the clause condition reads the bound card" $ do
    sindbad <- S.printingOf s registry "Shahrazad and Sindbad"
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    divination <- S.printingOf s registry "Divination"
    let (sindbadId, board) = sindbadBoard sindbad mountain plains island divination
        after = activateSole sindbadId board
    Spec.assertEqWith
      s
      "the Mountain it drew joined the hand and stayed there"
      (List.sort (namesIn Zone.Hand S.alice after))
      (List.sort [Just (S.printingName mountain), Just (S.printingName island), Just (S.printingName divination)])
    Spec.assertEqWith s "and nothing was discarded" (namesIn Zone.Graveyard S.alice after) []
    Spec.assertEqWith
      s
      "CR 701.20a the reveal happens either way"
      (S.revealsOf after)
      [(S.alice, Set.singleton (S.printingName mountain))]
  Spec.it s "CR 701.17 Tome Scour mills five from a target player's library" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    tomeScour <- S.printingOf s registry "Tome Scour"
    let base = S.landsInPlay island 1
        withLib = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.bob g)) base [1 .. (6 :: Int)]
        (gs, spellId) = S.handOne tomeScour withLib
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "five milled to graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 5
    Spec.assertEqWith s "one card left in library" (length (Game.zoneMembers Zone.Library S.bob after)) 1
  Spec.it s "CR 701.17b milling a short library mills fewer with no loss" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    tomeScour <- S.printingOf s registry "Tome Scour"
    let base = S.landsInPlay island 1
        (_, g1) = S.addLibraryCard piker S.bob base
        (_, g2) = S.addLibraryCard piker S.bob g1
        (gs, spellId) = S.handOne tomeScour g2
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = S.settleSba (snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop))
    Spec.assertEqWith s "two milled" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
    Spec.assertBool s (not (Set.member S.bob (GameState.drewFromEmpty after))) "bob did not lose (milling is not drawing)"
  Spec.it s "CR 701.9 Mind Rot discards two chosen cards from a hand of three" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 3 base
        (gs, spellId) = S.handOne mindRot withHand
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "one card left in bob's hand" (S.handSize S.bob after) 1
    Spec.assertEqWith s "two cards in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
  Spec.it s "CR 609.3 a forced full-hand discard is not prompted" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 2 base
        (gs, spellId) = S.handOne mindRot withHand
        -- Answer ChooseDiscard with [] so a prompt would discard nothing;
        -- aim the spell at bob.
        noDiscard q = case q of
          Prompt.ChooseDiscard {} -> []
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure noDiscard gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure noDiscard cast Stack.resolveTop)
    -- Elision (hand == count): the whole hand is discarded without asking (#63).
    Spec.assertEqWith s "bob's hand emptied" (S.handSize S.bob after) 0
    Spec.assertEqWith s "both cards discarded" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
  -- The three below are about the PROMPTED branch -- hand of three, discard
  -- two -- where the elision above does not apply and the answer is a real
  -- choice. Mind Rot is not "may", and CR 609.3's "as much as possible" caps
  -- nothing here (the hand is larger than the count), so every card an answer
  -- omits is one the player could have discarded.
  Spec.it s "CR 701.9b an empty ChooseDiscard answer still discards the full count" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 3 base
        (gs, spellId) = S.handOne mindRot withHand
        noDiscard q = case q of
          Prompt.ChooseDiscard {} -> []
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure noDiscard gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure noDiscard cast Stack.resolveTop)
    Spec.assertEqWith s "two discarded despite the answer naming none" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
    Spec.assertEqWith s "one card left in bob's hand" (S.handSize S.bob after) 1
  Spec.it s "CR 701.9b a valid pick is honoured and only the shortfall is completed" $ do
    -- Discriminating against "ignore the answer and take the first n": the
    -- answer names the LAST card in hand, which a first-n completion would
    -- leave behind.
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 3 base
        (gs, spellId) = S.handOne mindRot withHand
        onlyLast q = case q of
          Prompt.ChooseDiscard _ _ ids _ -> take 1 (reverse ids)
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure onlyLast gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure onlyLast cast Stack.resolveTop)
    case reverse (Game.zoneMembers Zone.Hand S.bob cast) of
      [] -> Spec.assertFailure s "fixture should leave bob a hand to discard from"
      lastCard : _ -> do
        Spec.assertEqWith s "two discarded" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
        Spec.assertBool s (List.notElem lastCard (Game.zoneMembers Zone.Hand S.bob after)) "and the card the answer named is one of them"
  Spec.it s "CR 701.9b naming the same card twice fills one slot, not two" $ do
    -- ChooseDiscard is answered with a LIST, so unlike ChooseSacrifices'
    -- Set the duplicate has to be removed here or it discards one card short.
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 3 base
        (gs, spellId) = S.handOne mindRot withHand
        sameTwice q = case q of
          Prompt.ChooseDiscard _ _ ids _ -> concat (replicate 2 (take 1 ids))
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
          _ -> S.identityAnswer q
        cast = snd (Engine.runGamePure sameTwice gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure sameTwice cast Stack.resolveTop)
    Spec.assertEqWith s "two distinct cards discarded" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
    Spec.assertEqWith s "one card left in bob's hand" (S.handSize S.bob after) 1
  -- CR 701.9a's discard BINDS what it moved, so the same resolution's next clause
  -- can look back at it: Psychic Miasma's "if a land card is discarded this way,
  -- return Psychic Miasma to its owner's hand". CR 701.70a's "if you discarded a
  -- nonland card this way" is the same rider read from the other side; no printing
  -- carries that literal wording outside connive and recruit reminder text
  -- (Scryfall o:"nonland card is discarded this way", 2026-08-22, no hit).
  --
  -- TWO legs, and neither is redundant. The asserted quantity is Psychic Miasma's
  -- own zone after resolution, and three implementations disagree about it:
  -- binding the CR 400.7 incarnation gives hand / graveyard; binding the pre-move
  -- HAND id -- which compiles, round-trips and loads -- makes Filter.IsBound false
  -- against every graveyard card and gives graveyard / graveyard, so only leg A
  -- separates it; dropping the card's Land conjunct gives hand / hand, so only leg
  -- B separates that.
  --
  -- bob holds exactly ONE card, so CR 609.3 makes the discard forced and no
  -- Prompt.ChooseDiscard answer stands between the board and the assertion. Two
  -- seats, so the discarding hand and the returning card are never the same zone.
  Spec.it s "CR 701.9a a land discarded this way returns Psychic Miasma to its owner's hand" $ do
    swamp <- S.printingOf s registry "Swamp"
    miasma <- S.printingOf s registry "Psychic Miasma"
    let base = S.landsInPlay swamp 3
        withHand = handCards swamp S.bob 1 base
        (gs, spellId) = S.handOne miasma withHand
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "psychic miasma returned to alice's hand" (namesIn Zone.Hand S.alice after) [Just (S.printingName miasma)]
    Spec.assertEqWith s "and did not also reach alice's graveyard" (namesIn Zone.Graveyard S.alice after) []
    Spec.assertEqWith s "bob's hand emptied" (S.handSize S.bob after) 0
    Spec.assertEqWith s "bob's graveyard holds the swamp" (namesIn Zone.Graveyard S.bob after) [Just (S.printingName swamp)]
  Spec.it s "CR 701.9a a nonland discarded this way leaves Psychic Miasma in its owner's graveyard" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    miasma <- S.printingOf s registry "Psychic Miasma"
    let base = S.landsInPlay swamp 3
        withHand = handCards piker S.bob 1 base
        (gs, spellId) = S.handOne miasma withHand
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "psychic miasma stayed in alice's graveyard" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName miasma)]
    Spec.assertEqWith s "and reached no hand" (S.handSize S.alice after) 0
    Spec.assertEqWith s "bob's hand emptied" (S.handSize S.bob after) 0
    Spec.assertEqWith s "bob's graveyard holds the piker" (namesIn Zone.Graveyard S.bob after) [Just (S.printingName piker)]
  -- CR 701.9a's per-turn TALLY, the same log read as a number rather than as the
  -- set one resolution moved: Dream Salvage's "draw cards equal to the number of
  -- cards target opponent discarded this turn". The declared target is read ONLY
  -- through that number -- the draw itself is CR 109.5's caster -- which is the
  -- shape the D4 dataflow lint could not see until Resolve.Slots.quantitySlots
  -- folded QuantitySlot.nestedRefs; Pawl.AbilitySlotLintSpec's "the lint itself
  -- catches a slot read only from inside a number" is the rejecting half.
  --
  -- TWO boards differing in exactly one thing, the size of bob's hand, so the
  -- draw cannot be a literal that happens to agree: Mind Rot names two cards and
  -- CR 609.3 makes a one-card hand give what it has, so the tallies are two and
  -- one. Alice's library is stocked past the larger draw, and bob's hand is
  -- filled with a printing alice's library does not hold, so a drawn card cannot
  -- be mistaken for a discarded one.
  Spec.it s "CR 701.9a Dream Salvage draws as many cards as the target discarded this turn" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    salvage <- S.printingOf s registry "Dream Salvage"
    let drawnAfterDiscarding held =
          let base = stockLibrary piker S.alice 3 (S.landsInPlay swamp 4)
              withHand = handCards swamp S.bob held base
              (gs, rotId) = S.handOne mindRot withHand
              (staged, salvageId) = S.handOne salvage gs
              rotCast = snd (Engine.runGamePure discardAtBob staged (S.cast S.alice rotId))
              rotted = snd (Engine.runGamePure discardAtBob rotCast Stack.resolveTop)
              salvageCast = snd (Engine.runGamePure discardAtBob rotted (S.cast S.alice salvageId))
           in snd (Engine.runGamePure discardAtBob salvageCast Stack.resolveTop)
        two = drawnAfterDiscarding 3
        one = drawnAfterDiscarding 1
    Spec.assertEqWith s "two discarded, two drawn" (namesIn Zone.Hand S.alice two) [Just (S.printingName piker), Just (S.printingName piker)]
    Spec.assertEqWith s "and bob really discarded two" (namesIn Zone.Graveyard S.bob two) [Just (S.printingName swamp), Just (S.printingName swamp)]
    Spec.assertEqWith s "one discarded, one drawn" (namesIn Zone.Hand S.alice one) [Just (S.printingName piker)]
    Spec.assertEqWith s "and bob really discarded one" (namesIn Zone.Graveyard S.bob one) [Just (S.printingName swamp)]
  -- The same rider read through a CR 614 redirect. Rest in Peace replaces the
  -- discard's destination with exile, which CR 400.2 lists among the public
  -- zones, so CR 400.7j keeps the discarded card findable by a later part of the
  -- same effect and the rider must still fire. The land leg above is this same
  -- board without the one permanent.
  --
  -- The asserted quantity is again Psychic Miasma's own zone, and it separates
  -- the two implementations: a rider reading the target's GRAVEYARD counts zero,
  -- leaves the return unrun, and lets Psychic Miasma head for alice's graveyard
  -- -- where Rest in Peace exiles it too -- while a rider folding the bound set
  -- finds the swamp in exile and returns the spell to alice's hand.
  --
  -- Rest in Peace goes under ALICE so bob's side is byte-identical to the legs
  -- above; its replacement is symmetric, so the controller does not matter. It is
  -- PLACED rather than cast, which is what keeps its own "exile all graveyards"
  -- trigger out of the way -- and both graveyards are empty at setup regardless.
  Spec.it s "CR 400.7j a land discarded into exile still returns Psychic Miasma to its owner's hand" $ do
    swamp <- S.printingOf s registry "Swamp"
    miasma <- S.printingOf s registry "Psychic Miasma"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    let base = S.landsInPlay swamp 3
        (_, withRip) = S.addPermanent restInPeace S.alice base
        withHand = handCards swamp S.bob 1 withRip
        (gs, spellId) = S.handOne miasma withHand
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "psychic miasma returned to alice's hand" (namesIn Zone.Hand S.alice after) [Just (S.printingName miasma)]
    Spec.assertEqWith s "and was not itself exiled on the way" (namesIn Zone.Exile S.alice after) []
    -- The guard against a fix that reaches the right answer by suppressing the
    -- redirect: the discard really did land somewhere other than a graveyard.
    Spec.assertEqWith s "bob's discarded swamp is in exile" (namesIn Zone.Exile S.bob after) [Just (S.printingName swamp)]
    Spec.assertEqWith s "and bob's graveyard is empty" (namesIn Zone.Graveyard S.bob after) []
    Spec.assertEqWith s "bob's hand emptied" (S.handSize S.bob after) 0
  -- The same rider through a redirect into a HIDDEN zone (CR 400.2) that
  -- REVEALS the card on the way: CR 701.9c undefines a discarded card's
  -- characteristics only when it is hidden "without being revealed", so the
  -- revealed land is still a land and the rider must fire. Wheel of Sun and
  -- Moon, {G/W}{G/W} Enchantment -- Aura: "Enchant player. If a card would be
  -- put into enchanted player's graveyard from anywhere, instead that card is
  -- revealed and put on the bottom of that player's library."
  -- Pawl.Engine.Count's findableAfterMove is the funnel, and reads the reveal
  -- off the log.
  --
  -- The Rest in Peace leg above with the one permanent swapped: the Wheel is
  -- alice's, enchanting bob, so the discard lands in bob's library while Psychic
  -- Miasma -- alice's card -- returns to alice's hand. Bob's library is stocked
  -- first, so "the bottom" is a position and not the whole library. Dropping the
  -- reveal read in findableAfterMove leaves the spell in alice's graveyard; the
  -- first assertion is what reddens. The unrevealed side -- a discard hidden
  -- without a reveal -- has no producer in the pool (#2230).
  Spec.it s "CR 701.9c a land discarded into a library revealed still returns Psychic Miasma" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    miasma <- S.printingOf s registry "Psychic Miasma"
    wheel <- S.printingOf s registry "Wheel of Sun and Moon"
    let base = S.landsInPlay swamp 3
        (wheelId, withWheel) = S.addPermanent wheel S.alice base
        enchanting = S.attachTo wheelId (Recipient.ToPlayer S.bob) withWheel
        (_, stocked) = S.addLibraryCard piker S.bob enchanting
        withHand = handCards swamp S.bob 1 stocked
        (gs, spellId) = S.handOne miasma withHand
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "psychic miasma returned to alice's hand" (namesIn Zone.Hand S.alice after) [Just (S.printingName miasma)]
    Spec.assertEqWith s "and did not also reach alice's graveyard" (namesIn Zone.Graveyard S.alice after) []
    -- The guard against a fix that reaches the right answer by suppressing the
    -- redirect: the discard really did land in the hidden zone.
    Spec.assertEqWith s "bob's discarded swamp is on the bottom of his library" (namesIn Zone.Library S.bob after) [Just (S.printingName piker), Just (S.printingName swamp)]
    Spec.assertEqWith s "and bob's graveyard is empty" (namesIn Zone.Graveyard S.bob after) []
    Spec.assertEqWith s "bob's hand emptied" (S.handSize S.bob after) 0
  -- The Wheel's own relation, CR 303.4b's enchanted player
  -- (ControllerRelation.EnchantedPlayers, judged by Replacement.relationHolds),
  -- as the whole card: cast at bob, then one card of each seat's headed for its
  -- owner's graveyard. THREE seats, because two cannot part the relation from
  -- its siblings: alice controls the Wheel, so Yours would take her card, and
  -- carol is her opponent, so Opponents would take carol's. Bob's library is
  -- stocked so the bottom is a position; the reveal (CR 701.20a) is asserted off
  -- the log, and is bob's, who holds the card.
  Spec.it s "CR 303.4b / 614.1a Wheel of Sun and Moon reroutes only the enchanted player's cards" $ do
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    wheel <- S.printingOf s registry "Wheel of Sun and Moon"
    let lands = S.landsFor forest S.alice 2 S.threePlayerGame
        (staged, spellId) = S.handOne wheel lands
        cast = snd (Engine.runGamePure atBobAnswer staged (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        (_, stocked) = S.addLibraryCard piker S.bob resolved
        (bobs, g1) = S.addHandCard swamp S.bob stocked
        (alices, g2) = S.addHandCard swamp S.alice g1
        (carols, g3) = S.addHandCard swamp S.carol g2
        after = S.runPure S.identityAnswer g3 (mapM_ (\oid -> Event.changeZone oid Zone.Graveyard) [bobs, alices, carols])
        enchanting = fmap Object.attachedTo (filter (\o -> Object.zone o == Zone.Battlefield && Maybe.isJust (Object.attachedTo o)) (Map.elems (GameState.objects after)))
    Spec.assertEqWith s "bob's card went to the bottom of bob's library" (namesIn Zone.Library S.bob after) [Just (S.printingName piker), Just (S.printingName swamp)]
    Spec.assertEqWith s "and not to bob's graveyard" (namesIn Zone.Graveyard S.bob after) []
    Spec.assertEqWith s "alice's own card reached her graveyard -- she is not the enchanted player" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName swamp)]
    Spec.assertEqWith s "carol's card reached her graveyard -- an opponent is not the enchanted player" (namesIn Zone.Graveyard S.carol after) [Just (S.printingName swamp)]
    Spec.assertEqWith s "bob revealed the card on its way" (S.revealsOf after) [(S.bob, Set.singleton (S.printingName swamp))]
    Spec.assertEqWith s "the Wheel is attached to bob" enchanting [Just (Recipient.ToPlayer S.bob)]
  -- CR 701.9's OTHER arity: Tinybones Joins Up's "any number of target players
  -- each discard a card", where the slot names three seats rather than Mind
  -- Rot's one. CR 101.4 is the ordering rule -- its own worked example is a
  -- table-wide edict -- so every seat is asked before any card moves, in turn
  -- order from the active player.
  --
  -- THREE seats and each hand a distinct printing, so both readings of "each"
  -- are separated: a fold over the controller's hand would empty alice's and
  -- leave the other two, and legalOne's Nothing would leave all three at two.
  Spec.it s "CR 701.9 / 101.4 Tinybones Joins Up has every targeted player discard, in APNAP order" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    sentry <- S.printingOf s registry "Ogre Sentry"
    rats <- S.printingOf s registry "Typhoid Rats"
    tinybones <- S.printingOf s registry "Tinybones Joins Up"
    let base = S.landsFor swamp S.alice 1 S.threePlayerGame
        (withSpell, spellId) = S.handOne tinybones base
        -- The hands are stocked AFTER S.handOne, which sets alice's hand rather
        -- than adding to it.
        gs = handCards rats S.carol 2 (handCards sentry S.bob 2 (handCards piker S.alice 2 withSpell))
        -- CR 601.2c: "any number" announces zero unless the answer says
        -- otherwise, and a zero-target announcement is satisfied by BOTH
        -- readings. So announce every recipient the board offers, and take the
        -- whole offered set rather than building recipients by hand.
        --
        -- The state is the seats ChooseDiscard was raised for, in the order they
        -- were asked, which is what makes CR 101.4's ordering observable.
        answer :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
        answer p = case p of
          Prompt.AnnounceTargets _ _ _ slots -> pure (fmap (\(_, candidates) -> Natural.length candidates) slots)
          Prompt.ChooseTargets _ _ _ sets -> pure (fmap snd sets)
          Prompt.ChooseDiscard _ victim held _ -> do
            State.modify' (<> [victim])
            pure (take 1 held)
          _ -> pure (S.identityAnswer p)
        (after, asked) =
          flip State.runState [] $ do
            (_, castGs) <- Engine.runGame answer gs (S.cast S.alice spellId)
            -- The enchantment resolves, then settling puts its CR 603.3d enters
            -- trigger on the stack with its targets announced.
            (_, entered) <- Engine.runGame answer castGs (Stack.resolveTop >> Engine.settleForPriority)
            (_, resolved) <- Engine.runGame answer entered Stack.resolveTop
            pure resolved
    Spec.assertEqWith s "alice discarded one of her two" (S.handSize S.alice after) 1
    Spec.assertEqWith s "bob discarded one of his two" (S.handSize S.bob after) 1
    Spec.assertEqWith s "carol discarded one of her two" (S.handSize S.carol after) 1
    -- WHOSE card left whose hand: a fold reading the controller's hand three
    -- times would put three pikers in one graveyard.
    Spec.assertEqWith s "alice's graveyard holds her own piker" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName piker)]
    Spec.assertEqWith s "bob's graveyard holds his own sentry" (namesIn Zone.Graveyard S.bob after) [Just (S.printingName sentry)]
    Spec.assertEqWith s "carol's graveyard holds her own rats" (namesIn Zone.Graveyard S.carol after) [Just (S.printingName rats)]
    Spec.assertEqWith s "CR 101.4: asked in turn order from the active player" asked [S.alice, S.bob, S.carol]

-- Griptide is "Put target creature on top of its owner's library", the pool's
-- producer for a library arrival that is NOT the bottom (#989). Everything the
-- group asserts is one card's worth of rules: CR 400.3 picks the library (its
-- OWNER's, not the caster's), CR 400.7 mints the incarnation that lands in it,
-- and CR 401.2 keeps the order a thing only the position can decide.
libraryPositionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
libraryPositionSpec s registry = Spec.describe s "LibraryPosition" $ do
  -- Three cards deep, top to bottom, because with ONE card the top and the
  -- bottom are the same index and the two positions are indistinguishable; and
  -- aimed at BOB's creature, because against her own "its owner's library" and
  -- "the caster's library" would name the same library.
  --
  -- Three is also deep enough that the draw below is an ordinary draw rather
  -- than CR 104.3c's loss: bob draws one of four.
  Spec.it s "CR 400.3 / 401.2 whole card: Griptide puts the creature on TOP of its OWNER's library, and its owner draws it" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    griptide <- S.printingOf s registry "Griptide"
    -- S.addLibraryCard puts each card ON TOP, so the LAST seeded is the head.
    let (pikerId, g1) = S.addPermanent piker S.bob (S.landsInPlay island 4)
        (deepId, g2) = S.addLibraryCard bolt S.bob g1
        (middleId, g3) = S.addLibraryCard bolt S.bob g2
        (oldTopId, g4) = S.addLibraryCard bolt S.bob g3
        (gs, spellId) = S.handOne griptide g4
        aimAtPiker :: Prompt.Prompt r -> r
        aimAtPiker p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature pikerId))) sets
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure aimAtPiker gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure aimAtPiker cast Stack.resolveTop)
        -- CR 400.7 mints a FRESH id at the destination, so asserting the
        -- battlefield id is gone is not the same claim as asserting the new one
        -- is on top. Both are made.
        bobsLibrary = Game.zoneMembers Zone.Library S.bob after
    Spec.assertBool s (not (S.onBattlefield pikerId after)) "the creature left the battlefield"
    Spec.assertEqWith s "bob's library grew by exactly one" (length bobsLibrary) 4
    Spec.assertEqWith s "alice's library is untouched" (Game.zoneMembers Zone.Library S.alice after) []
    case bobsLibrary of
      arrived : rest -> do
        Spec.assertEqWith
          s
          "and the card at INDEX 0 is the returned creature"
          (fmap S.nameOf (Game.cardOf arrived after))
          (Just (CardName.MkCardName (Text.pack "Goblin Piker")))
        Spec.assertEqWith s "with the previous top card now at index 1" rest [oldTopId, middleId, deepId]
        -- What makes the position OBSERVABLE rather than an internal detail:
        -- CR 121.1's draw puts "the top card of their library" into the hand, so
        -- a Piker in bob's hand is the rule and a Bolt is the bottom-of-library
        -- behaviour this closes.
        let drawn =
              S.runPure aimAtPiker after $ do
                State.modify' $ \g -> g {GameState.activePlayer = S.bob, GameState.turnNumber = 2}
                S.drawStep
        -- By NAME, not by id: the draw is itself a zone change, so CR 400.7
        -- mints a second incarnation and the card in hand is not `arrived`
        -- either. bob's library holds nothing but Bolts, so the name is what
        -- tells the returned creature from the card that was on top before it.
        Spec.assertEqWith
          s
          "bob draws it in his draw step"
          (fmap (fmap S.nameOf . (`Game.cardOf` drawn)) (Game.zoneMembers Zone.Hand S.bob drawn))
          [Just (CardName.MkCardName (Text.pack "Goblin Piker"))]
        Spec.assertEqWith s "leaving the three he started with" (Game.zoneMembers Zone.Library S.bob drawn) [oldTopId, middleId, deepId]
      [] -> Spec.assertFailure s "bob's library should hold the seeded cards"
  -- The elision LibraryPlacement.Stated buys: a card that NAMES the end asks
  -- nobody for it. Paired with the Aetherspouts group's positive, which requires
  -- the prompt on an owner-chosen board -- without the pair either alone would
  -- pass an implementation that never asks at all.
  Spec.it s "CR 401.2 a STATED end raises no ChooseLibraryEnd" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    griptide <- S.printingOf s registry "Griptide"
    let (pikerId, g1) = S.addPermanent piker S.bob (S.landsInPlay island 4)
        (_, g2) = S.addLibraryCard bolt S.bob g1
        (gs, spellId) = S.handOne griptide g2
        countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseLibraryEnd {} -> do
            State.modify (+ 1)
            pure (S.identityAnswer p)
          Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Set.singleton (Recipient.ToCreature pikerId))) sets)
          _ -> pure (S.identityAnswer p)
        (after, asked) =
          State.runState
            ( fmap snd . Engine.runGame countingAnswer gs $ do
                S.cast S.alice spellId
                Stack.resolveTop
            )
            0
    Spec.assertEqWith s "nobody was asked which end" asked 0
    Spec.assertEqWith s "and the creature still went to the TOP" (fmap (fmap S.nameOf . (`Game.cardOf` after)) (take 1 (Game.zoneMembers Zone.Library S.bob after))) [Just (CardName.MkCardName (Text.pack "Goblin Piker"))]
  -- The control: Unsummon states no library position at all, so its bounce must
  -- be exactly what it was before the field existed.
  Spec.it s "CR 400.3 the control: Unsummon on the same board still returns the creature to its owner's HAND" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    unsummon <- S.printingOf s registry "Unsummon"
    let (pikerId, g1) = S.addPermanent piker S.bob (S.landsInPlay island 4)
        (_, g2) = S.addLibraryCard bolt S.bob g1
        (gs, spellId) = S.handOne unsummon g2
        aimAtPiker :: Prompt.Prompt r -> r
        aimAtPiker p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature pikerId))) sets
          _ -> S.identityAnswer p
        cast = snd (Engine.runGamePure aimAtPiker gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure aimAtPiker cast Stack.resolveTop)
    Spec.assertEqWith s "one card in bob's hand" (S.handSize S.bob after) 1
    Spec.assertEqWith s "and his library is the one card it was" (length (Game.zoneMembers Zone.Library S.bob after)) 1

-- Every question Aetherspouts raises: which object each owner was asked to place
-- (CR 401.2), and which batch each owner was asked to arrange (CR 401.4).
type SpoutsLog = ([(PlayerId.PlayerId, ObjectId.ObjectId)], [(PlayerId.PlayerId, LibraryPosition.LibraryPosition, [ObjectId.ObjectId])])

-- alice is mid-combat attacking with `mine` creatures she owns and `stolen`
-- creatures BOB owns under her control, holds an Aetherspouts and the five
-- Islands that pay for it, and both libraries are two cards deep so an arrival
-- at either end is distinguishable from one at the other.
--
-- The stolen creature is what makes a ONE-COMBAT board hold two owners at all:
-- CR 508.1a says "the active player chooses which creatures THAT THEY CONTROL
-- ... will attack", so every attacker in one combat shares a controller and only
-- separating owner from controller can put two owners' cards in the batch.
-- S.giveControl also settles it under alice, which is that rule's second
-- sentence ("controlled by the active player continuously since the turn
-- began").
spoutsBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
spoutsBoard island spouts mine stolen =
  let addAll pid ps gs = List.foldl' (\(ids, g) p -> let (oid, g1) = S.addPermanent p pid g in (ids <> [oid], g1)) ([], gs) ps
      (gs0, ours, _) = S.combatBoardOf mine []
      (theirs, gs1) = addAll S.bob stolen gs0
      gs2 = List.foldl' (\g oid -> S.giveControl oid S.alice g) gs1 theirs
      withLands = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) gs2 [1 :: Int .. 5]
      stocked = List.foldl' (\g pid -> snd (S.addLibraryCard island pid (snd (S.addLibraryCard island pid g)))) withLands [S.alice, S.bob]
      (withCard, spell) = S.handOne spouts stocked
   in ( -- handOne parks its state in a precombat main phase; this board is
        -- mid-combat, the way Pawl.MassEffectSpec's trumpetBoard restores it.
        withCard
          { GameState.phase = GameState.phase gs0,
            GameState.priority = GameState.priority gs0
          },
        spell,
        ours,
        theirs
      )

-- Declare alice's attack, then cast and resolve the Aetherspouts under an
-- answerer that records every question it is asked.
--
-- `end` picks each owner's answer BY WHO IS ASKED, which is the whole point of
-- the two-owner board: an implementation that raised the prompt with the
-- resolving CONTROLLER would hand both cards the same end.
castSpouts :: (PlayerId.PlayerId -> LibraryPosition.LibraryPosition) -> [Natural] -> GameState.GameState -> ObjectId.ObjectId -> (GameState.GameState, SpoutsLog)
castSpouts end arrangement board spell =
  let attacking = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
      answerer :: Prompt.Prompt r -> State.State SpoutsLog r
      answerer p = case p of
        Prompt.ChooseLibraryEnd _ pid oid -> do
          State.modify (\(ends, arrs) -> (ends <> [(pid, oid)], arrs))
          pure (end pid)
        Prompt.ArrangeLibraryArrivals _ pid position oids -> do
          State.modify (\(ends, arrs) -> (ends, arrs <> [(pid, position, oids)]))
          pure arrangement
        _ -> pure (S.identityAnswer p)
   in State.runState
        ( fmap snd . Engine.runGame answerer attacking $ do
            S.cast S.alice spell
            Stack.resolveTop
        )
        ([], [])

-- Aetherspouts ({3}{U}{U} instant, "For each attacking creature, its owner puts
-- it on their choice of the top or bottom of their library"): the pool's
-- producer for a library end the OWNER picks (CR 401.2, #1035), and a producer
-- for CR 401.4's arrangement of two or more cards reaching one end at once
-- (#990) -- Surging Dementia's ripple states its end and still hands over the
-- arrangement, so it reaches that half too. WotC's own 2014-07-18 ruling on the
-- card states both halves.
--
-- Nothing here bears on the order SoulfireEruption's group asks about: CR 608.2f's
-- secondary sentence is guarded by "if
-- the action can't be processed simultaneously", and CR 401.4 gives a library
-- destination its own rule with its own decider -- so a correct Aetherspouts
-- SCREENS the sweep order off rather than exposing it.
aetherspoutsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aetherspoutsSpec s registry = Spec.describe s "Aetherspouts" $ do
  -- The claim: the end each creature goes to is decided by that creature's
  -- OWNER, not by the spell's controller and not by the engine. alice controls
  -- both attackers; bob owns one of them, and only he can send it to the top.
  Spec.it s "CR 401.2 each attacking creature's OWNER picks the end, not the resolving controller" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    spouts <- S.printingOf s registry "Aetherspouts"
    let (board, spell, ours, theirs) = spoutsBoard island spouts [piker] [giant]
        (after, (ends, arrangements)) = castSpouts (\pid -> if pid == S.bob then LibraryPosition.Top else LibraryPosition.Bottom) [] board spell
        -- By NAME, through `namesIn`: CR 400.7 mints a fresh id at the
        -- destination, so a library arrival is never the id it had on the
        -- battlefield. That is also why the two attackers are two different
        -- printings -- two Pikers would be indistinguishable at either end.
        bobs = namesIn Zone.Library S.bob after
        alices = namesIn Zone.Library S.alice after
    -- Without this the sweep could have found nothing and every assertion below
    -- would pass vacuously.
    Spec.assertEqWith s "both attackers left the battlefield" (filter (`S.onBattlefield` after) (ours <> theirs)) []
    Spec.assertEqWith s "each creature's own owner was asked, once, in the sweep's APNAP order" ends (fmap ((,) S.alice) ours <> fmap ((,) S.bob) theirs)
    -- One card per (owner, end) group, so CR 401.4 has nothing to arrange. The
    -- negative half of the elision pair; the positive is the next test.
    Spec.assertEqWith s "and nobody was asked to arrange a batch of one" arrangements []
    Spec.assertEqWith s "each library grew by exactly its owner's card" (length bobs, length alices) (3, 3)
    -- The discriminating half. An implementation that ignored the answers would
    -- fall back on LibraryPosition.defaultValue and put everything on the
    -- BOTTOM, so it is bob's Giant at the TOP that catches it -- a
    -- both-cards-to-the-bottom board would pass for the wrong reason.
    Spec.assertEqWith
      s
      "bob answered Top so his Giant heads his library; alice answered Bottom so her Piker is last in hers"
      (take 1 bobs <> drop 2 alices)
      [Just (S.printingName giant), Just (S.printingName piker)]
  -- CR 401.4's positive, at the TOP. Two of alice's own creatures, both sent to
  -- the top of her library, arranged with the NON-CANONICAL answer [1, 0] --
  -- answering [0, 1] is the sweep order, so every assertion would pass under an
  -- implementation that never asked.
  Spec.it s "CR 401.4 two cards reaching the TOP at once are arranged by their owner" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    spouts <- S.printingOf s registry "Aetherspouts"
    let (board, spell, ours, _) = spoutsBoard island spouts [piker, giant] []
        (after, (ends, arrangements)) = castSpouts (const LibraryPosition.Top) [1, 0] board spell
        alices = namesIn Zone.Library S.alice after
    Spec.assertEqWith s "both attackers left the battlefield" (filter (`S.onBattlefield` after) ours) []
    Spec.assertEqWith s "alice was asked about each of her two creatures" (length ends) 2
    Spec.assertEqWith s "and asked ONCE to arrange the pair, at the top of her library" arrangements [(S.alice, LibraryPosition.Top, ours)]
    -- The answer names the cards from the chosen end inward, so the creature the
    -- sweep offered SECOND finishes on top.
    Spec.assertEqWith
      s
      "read from the top inward, the library is the order she gave"
      (take 2 alices)
      [Just (S.printingName giant), Just (S.printingName piker)]
  -- The same claim at the other end. Both ends want the SAME traversal, because
  -- the Sequence grows from opposite ends for them -- the answer's head is the
  -- last card moved either way -- so this is the leg that catches a moves-in-
  -- answer-order implementation just as the one above does.
  Spec.it s "CR 401.4 two cards reaching the BOTTOM at once are arranged by their owner" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    spouts <- S.printingOf s registry "Aetherspouts"
    let (board, spell, ours, _) = spoutsBoard island spouts [piker, giant] []
        (after, (_, arrangements)) = castSpouts (const LibraryPosition.Bottom) [1, 0] board spell
        alices = namesIn Zone.Library S.alice after
    Spec.assertEqWith s "asked once, at the bottom of her library" arrangements [(S.alice, LibraryPosition.Bottom, ours)]
    Spec.assertEqWith
      s
      "read from the bottom inward, the library is the order she gave"
      (reverse (drop 2 alices))
      [Just (S.printingName giant), Just (S.printingName piker)]

drawCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
drawCardSpec s registry = Spec.describe s "DrawCard" $ do
  Spec.it s "CR 121.2 drawCard moves the top library card to hand" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (_, withCard) = S.addLibraryCard piker S.alice base
        after = S.runPure S.identityAnswer withCard (Event.drawCard S.alice)
    Spec.assertEqWith s "one card in hand" (S.handSize S.alice after) 1
    Spec.assertEqWith s "library empty" (Game.zoneMembers Zone.Library S.alice after) []
  Spec.it s "CR 121.3 drawing from an empty library records the failed draw" $ do
    let after = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Event.drawCard S.alice)
    Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty after)) "drewFromEmpty marked"

loseLifeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
loseLifeSpec s registry = Spec.describe s "LoseLife" $ do
  -- Both cases are Sign in Blood, the card that proves the opcode (#273): its
  -- two clauses share one target slot, so the player who draws is the player
  -- who loses life, and neither is aimed at the caster.
  -- The last assertion is the falsifier for a life loss spelled as damage.
  -- CR 119.2 makes damage a CAUSE of life loss, not a synonym for it, so
  -- this records no damage event for CR 614/615's replacement and
  -- prevention, infect's CR 120.3b diversion or CR 704.5h's deathtouch scan
  -- to read.
  Spec.it s "CR 119.3 Sign in Blood makes the player it targets draw two and lose two life" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let base = S.landsInPlay swamp 2
        withLib = stockLibrary piker S.bob 3 base
        (gs, spellId) = S.handOne signInBlood withLib
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        isDamage ev = case ev of
          GameEvent.DamageDealt _ -> True
          _ -> False
    Spec.assertEqWith s "bob drew two" (S.handSize S.bob after) 2
    Spec.assertEqWith s "and lost two life" (S.lifeOf S.bob after) (fmap (subtract 2) (S.lifeOf S.bob gs))
    Spec.assertEqWith s "alice, who cast it, lost none" (S.lifeOf S.alice after) (S.lifeOf S.alice gs)
    Spec.assertBool s (not (any isDamage (S.eventsOf after))) "no damage was dealt (CR 119.2)"
  -- CR 704.5a: life lost without damage still reaches the state-based
  -- action -- the same check a CR 119.4 pay-life cost answers to. Bob is at
  -- two, so the second clause is lethal though nothing dealt damage.
  Spec.it s "CR 704.5a Sign in Blood's life loss can take a player to 0 and lose them the game" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let base = S.landsInPlay swamp 2
        withLib = stockLibrary piker S.bob 3 base
        (gs0, spellId) = S.handOne signInBlood withLib
        gs = gs0 {GameState.players = Map.adjust (\pl -> pl {Player.life = 2}) S.bob (GameState.players gs0)}
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "bob is at 0" (S.lifeOf S.bob after) (Just 0)
    Spec.assertEqWith s "and alice wins" (GameState.result (S.settleSba after)) (Just (Result.Won S.alice))

-- A per-player amount that is a number of the RECIPIENT'S OWN, on three opcodes
-- and through the two spellings of that reading:
--
--   * Stronghold Discipline, {2}{B}{B} Sorcery: "Each player loses 1 life for
--     each creature they control." Effect.LoseLife over a count filtered by
--     Filter.ControlledByRecipient -- CR 110.2's CONTROL, read over the shared
--     battlefield (CR 400.1), which no per-seat scope can express (see #161).
--   * Nature's Resurgence, {2}{G}{G} Sorcery: "Each player draws a card for each
--     creature card in their graveyard." Effect.Draw over a count whose SCOPE is
--     the recipient's own graveyard -- PlayerRef.Candidate, substituted by
--     Quantity.forCandidate, which is the half a nested Count used to be left out
--     of.
--   * Acidic Soil, {2}{R} Sorcery: "Acidic Soil deals damage to each player equal
--     to the number of lands they control." Effect.DealDamage over Stronghold
--     Discipline's spelling, and the case for CR 608.2f: the amount is read once
--     per recipient and the damage is still dealt as ONE batch.
--
-- Three seats taking three DIFFERENT amounts in each case, because a board where
-- two of them take the same number cannot tell a per-recipient reading from one
-- evaluation shared by the table. alice, the CONTROLLER, is one of the three:
-- "each player" reaches her, and handing everyone the controller's number is
-- exactly the error being excluded.
--
-- All three cards are mandatory and targetless, so no prompt is raised during
-- any of the resolutions and no answerer can repair a mutated reading.
perRecipientAmountSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
perRecipientAmountSpec s registry =
  let castAndResolve spellId gs =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
         in S.runPure S.identityAnswer cast Stack.resolveTop
      addPikers piker pid n gs = List.foldl' (\board _ -> snd (S.addPermanent piker pid board)) gs [1 .. n :: Int]
      addGraves printing pid n gs = List.foldl' (\board _ -> snd (S.addGraveyardCard printing pid board)) gs [1 .. n :: Int]
      at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
      -- alice controls 3 Pikers, bob 2 and carol 1, on life totals 20, 17 and 13
      -- -- distinct counts against distinct totals, so no seat's answer is any
      -- other seat's and none of them is a total either. The Swamps are alice's
      -- {2}{B}{B}; a land is not a creature, so they do not enter the count.
      disciplineBoard = do
        swamp <- S.printingOf s registry "Swamp"
        piker <- S.printingOf s registry "Goblin Piker"
        discipline <- S.printingOf s registry "Stronghold Discipline"
        let withLands = S.landsFor swamp S.alice 4 S.threePlayerGame
            withCreatures = addPikers piker S.bob 2 (addPikers piker S.alice 3 withLands)
            (carolPiker, withCarol) = S.addPermanent piker S.carol withCreatures
            lifed = withCarol {GameState.players = at S.alice 20 (at S.bob 17 (at S.carol 13 (GameState.players withCarol)))}
            (gs, spellId) = S.handOne discipline lifed
        pure (carolPiker, spellId, gs)
      -- `aliceCreatures` is the one dial. bob's graveyard holds 2 creature cards
      -- and carol's 3; alice's holds a FOREST as well, which is a card in the
      -- graveyard and not a creature card, so a count that dropped the filter
      -- would give her one too many. Every library is stocked well past the
      -- deepest draw, so CR 104.3c takes nobody.
      resurgenceBoard aliceCreatures = do
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        resurgence <- S.printingOf s registry "Nature's Resurgence"
        let withLands = S.landsFor forest S.alice 4 S.threePlayerGame
            graved = addGraves piker S.carol 3 (addGraves piker S.bob 2 (addGraves piker S.alice aliceCreatures withLands))
            withFiller = snd (S.addGraveyardCard forest S.alice graved)
            stocked = List.foldl' (\board pid -> stockLibrary piker pid 8 board) withFiller [S.alice, S.bob, S.carol]
            (gs, spellId) = S.handOne resurgence stocked
        pure (spellId, gs)
      -- alice controls 3 Mountains, bob 2 and carol 1, on life totals 20, 17 and
      -- 13. alice's three are also the {2}{R}, and they are TAPPED by the time
      -- the spell resolves -- "lands they control" counts a tapped land, so her
      -- own answer is still 3.
      acidicBoard = do
        mountain <- S.printingOf s registry "Mountain"
        soil <- S.printingOf s registry "Acidic Soil"
        let withAlice = S.landsFor mountain S.alice 3 S.threePlayerGame
            withBob = S.landsFor mountain S.bob 2 withAlice
            (carolMountain, withCarol) = S.addPermanent mountain S.carol withBob
            lifed = withCarol {GameState.players = at S.alice 20 (at S.bob 17 (at S.carol 13 (GameState.players withCarol)))}
            (gs, spellId) = S.handOne soil lifed
        pure (carolMountain, spellId, gs)
      damages gs = fmap (\ev -> (DamageEvent.target ev, DamageEvent.amount ev)) (S.damageEventsOf gs)
   in Spec.describe s "PerRecipientAmount" $ do
        Spec.it s "CR 119.3 Stronghold Discipline charges each player for their OWN creatures" $ do
          (_carolPiker, spellId, gs) <- disciplineBoard
          let after = castAndResolve spellId gs
          Spec.assertEqWith s "alice, controlling 3, went 20 -> 17" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "bob, controlling 2, went 17 -> 15 -- not alice's 3" (S.lifeOf S.bob after) (Just 15)
          Spec.assertEqWith s "carol, controlling 1, went 13 -> 12" (S.lifeOf S.carol after) (Just 12)
          Spec.assertEqWith s "three losses, each the payer's own count" (lifeLosses after) [(S.alice, 3), (S.bob, 2), (S.carol, 1)]
        -- The control twin, differing in ONE thing: carol's Piker is under bob's
        -- control. She still OWNS it, so an amount read off the owner-sliced
        -- battlefield would leave both their answers where they were; CR 110.2's
        -- control is what moves the 1 from carol to bob.
        Spec.it s "CR 110.2 the control: a creature carol owns but bob controls is charged to BOB" $ do
          (carolPiker, spellId, gs0) <- disciplineBoard
          let gs = S.giveControl carolPiker S.bob gs0
              after = castAndResolve spellId gs
          Spec.assertEqWith s "alice is unmoved at 17" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "bob, now controlling 3, went 17 -> 14" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "carol, controlling nothing, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "and CR 119.9 leaves her out of the log entirely" (lifeLosses after) [(S.alice, 3), (S.bob, 3)]
          Spec.assertEqWith s "the Piker is still carol's card" (fmap Object.owner (Game.lookupObject carolPiker after)) (Just S.carol)
        Spec.it s "CR 121.2 Nature's Resurgence draws each player their OWN graveyard's creatures" $ do
          (spellId, gs) <- resurgenceBoard 1
          let after = castAndResolve spellId gs
          Spec.assertEqWith s "alice drew 1: one creature card, and the Forest is not one" (S.handSize S.alice after) 1
          Spec.assertEqWith s "bob drew 2 -- not alice's 1" (S.handSize S.bob after) 2
          Spec.assertEqWith s "carol drew 3" (S.handSize S.carol after) 3
          Spec.assertEqWith s "and each library is shorter by exactly that many" (fmap (\pid -> length (Game.zoneMembers Zone.Library pid after)) [S.alice, S.bob, S.carol]) [7, 6, 5]
        -- The control twin, differing in ONE thing: alice's graveyard holds four
        -- creature cards instead of one. Only her draw moves -- a reading that
        -- evaluated once from the controller's graveyard would move all three
        -- from 1/1/1 to 4/4/4.
        Spec.it s "CR 400.1 the control: alice's own graveyard grows, and nobody else's draw moves" $ do
          (spellId, gs) <- resurgenceBoard 4
          let after = castAndResolve spellId gs
          Spec.assertEqWith s "alice drew 4" (S.handSize S.alice after) 4
          Spec.assertEqWith s "bob still drew 2" (S.handSize S.bob after) 2
          Spec.assertEqWith s "carol still drew 3" (S.handSize S.carol after) 3
          Spec.assertEqWith s "and the libraries agree" (fmap (\pid -> length (Game.zoneMembers Zone.Library pid after)) [S.alice, S.bob, S.carol]) [4, 6, 5]
        Spec.it s "CR 120.3a Acidic Soil deals each player their OWN land count" $ do
          (_carolMountain, spellId, gs) <- acidicBoard
          let after = castAndResolve spellId gs
          Spec.assertEqWith s "alice, controlling 3, went 20 -> 17" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "bob, controlling 2, went 17 -> 15 -- not alice's 3" (S.lifeOf S.bob after) (Just 15)
          Spec.assertEqWith s "carol, controlling 1, went 13 -> 12" (S.lifeOf S.carol after) (Just 12)
          -- Three events, and CR 608.2f's simultaneity is what the ONE
          -- applyDamage call keeps: the amounts differ per seat while the batch
          -- does not become three batches.
          Spec.assertEqWith s "one batch of three, each the recipient's own count" (damages after) [(Recipient.ToPlayer S.alice, 3), (Recipient.ToPlayer S.bob, 2), (Recipient.ToPlayer S.carol, 1)]
        -- The control twin, differing in ONE thing: carol's Mountain is under
        -- bob's control. She still OWNS it, so an amount read off an
        -- owner-sliced battlefield would leave both their answers where they
        -- were; CR 110.2's control is what moves the 1 from carol to bob.
        Spec.it s "CR 110.2 the control: a land carol owns but bob controls is charged to BOB" $ do
          (carolMountain, spellId, gs0) <- acidicBoard
          let gs = S.giveControl carolMountain S.bob gs0
              after = castAndResolve spellId gs
          Spec.assertEqWith s "alice is unmoved at 17" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "bob, now controlling 3, went 17 -> 14" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "carol, controlling nothing, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "and CR 120.8 drops her recipient without dropping the batch" (damages after) [(Recipient.ToPlayer S.alice, 3), (Recipient.ToPlayer S.bob, 3)]
          Spec.assertEqWith s "the Mountain is still carol's card" (fmap Object.owner (Game.lookupObject carolMountain after)) (Just S.carol)

-- Mirror Universe (Legends) on alice's battlefield, in her own upkeep, with the
-- three seats at three DIFFERENT life totals: "{T}, Sacrifice this artifact:
-- Exchange life totals with target opponent. Activate only during your upkeep."
--
-- Three seats because a two-player board cannot tell the exchange's TARGET from
-- "the other player". carol is a second legal target the interpreter can pick,
-- and the totals are distinct so that no pair of them coincides.
--
-- The schedule loses its head for augurBoard's reason (ActivateSpec): emptyGame's
-- `remaining` still begins with the upkeep step, so a runStep-driven test would
-- otherwise advance out of the step the card names.
mirrorBoard :: Printing.Printing -> Integer -> Integer -> Integer -> (ObjectId.ObjectId, GameState.GameState)
mirrorBoard mirror aliceLife bobLife carolLife =
  let (mirrorId, gs1) = S.addPermanent mirror S.alice S.threePlayerGame
      at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
   in ( mirrorId,
        gs1
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.Beginning BeginningStep.Upkeep,
            GameState.priority = Just S.alice,
            GameState.remaining = Seq.drop 1 (GameState.remaining gs1),
            GameState.players = at S.alice aliceLife (at S.bob bobLife (at S.carol carolLife (GameState.players gs1)))
          }
      )

-- Soul Conduit (Eldritch Moon) on alice's battlefield over six untapped Islands,
-- with the three seats at three DIFFERENT life totals: "{6}, {T}: Two target
-- players exchange life totals."
--
-- The same three seats and the same schedule surgery as mirrorBoard, and for the
-- same reasons -- but here the point of the third seat is that the two sides of
-- the exchange can BOTH be players other than the controller, which two seats
-- cannot express.
soulConduitBoard :: Printing.Printing -> Printing.Printing -> Integer -> Integer -> Integer -> (ObjectId.ObjectId, GameState.GameState)
soulConduitBoard conduit island aliceLife bobLife carolLife =
  let withLands = List.foldl' (\gs _ -> snd (S.addPermanent island S.alice gs)) S.threePlayerGame [1 .. 6 :: Int]
      (conduitId, gs1) = S.addPermanent conduit S.alice withLands
      at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
   in ( conduitId,
        gs1
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.Beginning BeginningStep.Upkeep,
            GameState.priority = Just S.alice,
            GameState.remaining = Seq.drop 1 (GameState.remaining gs1),
            GameState.players = at S.alice aliceLife (at S.bob bobLife (at S.carol carolLife (GameState.players gs1)))
          }
      )

-- Takes the first activation offered, taps whatever the payment asks for, and
-- fills the target slot with `sides` -- S.preferring rather than a fixed set, so
-- the announced count (CR 601.2c) is what decides how many are named.
conduitAnswer :: [PlayerId.PlayerId] -> Prompt.Prompt r -> r
conduitAnswer sides p =
  let wanted r = case r of
        Recipient.ToPlayer pid -> elem pid sides
        _ -> False
   in case p of
        Prompt.ChooseAction _ _ options -> case filter isActivation options of
          a : _ -> a
          [] -> A.Pass
        Prompt.ChooseManaSource _ _ candidates -> Just (NonEmpty.head candidates)
        Prompt.ChooseTargets _ _ _ sets -> S.preferring wanted sets
        _ -> S.identityAnswer p

-- Takes the first activation offered and aims every target slot at `who`.
exchangeAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
exchangeAnswer who p = case p of
  Prompt.ChooseAction _ _ options -> case filter isActivation options of
    a : _ -> a
    [] -> A.Pass
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
  _ -> S.identityAnswer p

isActivation :: A.Action -> Bool
isActivation a = case a of
  A.Activate {} -> True
  A.Pass -> False
  A.Play {} -> False
  A.Cast {} -> False
  A.TurnFaceUp {} -> False
  A.Unlock _ _ -> False
  A.DiscardFromHand _ -> False
  A.Plot _ -> False
  A.Foretell _ -> False
  A.Suspend _ -> False
  A.PutCompanionIntoHand -> False
  A.ActivateManaAbility _ -> False
  A.Ignore _ _ -> False
  A.EndEffect _ -> False

-- The life events the whole step logged, by player and amount. CR 701.12c makes
-- the exchange a GAIN and a LOSS rather than two assignments, so this is what a
-- "whenever you gain life" trigger would have to read.
lifeGains :: GameState.GameState -> [(PlayerId.PlayerId, Natural)]
lifeGains gs = Maybe.mapMaybe (\ev -> case ev of GameEvent.LifeGained (LifeChange.MkLifeChange pid n) -> Just (pid, n); _ -> Nothing) (S.eventsOf gs)

lifeLosses :: GameState.GameState -> [(PlayerId.PlayerId, Natural)]
lifeLosses gs = Maybe.mapMaybe (\ev -> case ev of GameEvent.LifeLost (LifeChange.MkLifeChange pid n) -> Just (pid, n); _ -> Nothing) (S.eventsOf gs)

exchangeLifeTotalsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exchangeLifeTotalsSpec s registry = Spec.describe s "ExchangeLifeTotals" $ do
  -- The gameplay-level proof (design.md section 4), driven through
  -- Engine.runStep and the priority loop: alice at 4 and bob at 27 swap, and
  -- each reaches the other's PREVIOUS total -- an implementation that wrote one
  -- side before reading the other would leave both on one number.
  Spec.it s "CR 701.12c whole card: Mirror Universe swaps its controller's total with the target's" $ do
    mirror <- S.printingOf s registry "Mirror Universe"
    let (mirrorId, board) = mirrorBoard mirror 4 27 13
        after = S.runPure (exchangeAnswer S.bob) board Engine.runStep
    Spec.assertEqWith s "alice took bob's 27" (S.lifeOf S.alice after) (Just 27)
    Spec.assertEqWith s "bob took alice's 4" (S.lifeOf S.bob after) (Just 4)
    Spec.assertEqWith s "carol, untargeted, is untouched" (S.lifeOf S.carol after) (Just 13)
    Spec.assertBool s (not (Set.member mirrorId (GameState.battlefield after))) "the Universe paid itself"
    -- CR 701.12c's "gains or loses the amount of life necessary", as events:
    -- 27 - 4 either way, and nothing else moved a life total this step.
    Spec.assertEqWith s "alice gained 23" (lifeGains after) [(S.alice, 23)]
    Spec.assertEqWith s "bob lost 23" (lifeLosses after) [(S.bob, 23)]
    -- The printed "target opponent" is the card's own filter, read from the
    -- perspective of the player activating it (CR 109.5), so alice is never a
    -- candidate. Paired with the outcome above rather than asserted alone, since
    -- a candidate list nothing consumed proves nothing.
    let candidates = case Activatable.abilitiesFor mirrorId board of
          [ability] -> case Seq.lookup 0 (Modal.modes (ActivatedAbility.modal ability)) of
            Just mode -> Map.elems (Target.legalSets (Just S.alice) False Map.empty mirrorId (Mode.targetSlots mode) board)
            Nothing -> []
          _ -> []
    Spec.assertEqWith s "both opponents are candidates, alice is not" candidates [Set.fromList [Recipient.ToPlayer S.bob, Recipient.ToPlayer S.carol]]

  -- The same board and the same interpreter, aimed at carol: the slot is READ
  -- rather than the exchange running against a fixed second seat.
  Spec.it s "CR 601.2c the other side is the slot's target, not simply the opponent" $ do
    mirror <- S.printingOf s registry "Mirror Universe"
    let (_, board) = mirrorBoard mirror 4 27 13
        after = S.runPure (exchangeAnswer S.carol) board Engine.runStep
    Spec.assertEqWith s "alice took carol's 13" (S.lifeOf S.alice after) (Just 13)
    Spec.assertEqWith s "carol took alice's 4" (S.lifeOf S.carol after) (Just 4)
    Spec.assertEqWith s "bob, untargeted, is untouched" (S.lifeOf S.bob after) (Just 27)

  -- CR 119.9: equal totals are an exchange that moves nobody, and a gain of 0 is
  -- no life gain event at all -- so "whenever you gain life" must not fire on it.
  Spec.it s "CR 119.9 an exchange between equal totals logs no life event" $ do
    mirror <- S.printingOf s registry "Mirror Universe"
    let (mirrorId, board) = mirrorBoard mirror 15 15 13
        after = S.runPure (exchangeAnswer S.bob) board Engine.runStep
    Spec.assertEqWith s "alice is still at 15" (S.lifeOf S.alice after) (Just 15)
    Spec.assertEqWith s "bob is still at 15" (S.lifeOf S.bob after) (Just 15)
    Spec.assertBool s (not (Set.member mirrorId (GameState.battlefield after))) "and the ability was activated: its sacrifice was paid"
    Spec.assertEqWith s "no gain" (lifeGains after) []
    Spec.assertEqWith s "no loss" (lifeLosses after) []

  -- CR 701.12c's other shape: BOTH sides come out of one instance of the word
  -- "target" (CR 601.2c), and neither of them need be the controller. bob at 27
  -- and carol at 13 swap while alice, who activated it, keeps her 4 -- so the
  -- reading in which the controller is always one side gets a different answer
  -- for every seat. The four numbers (4, 13, 27 and the 14 that moves) are
  -- distinct, so no pair of readings coincides.
  Spec.it s "CR 701.12c Soul Conduit exchanges the totals of two players, neither of them its controller" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    let (conduitId, board) = soulConduitBoard conduit island 4 27 13
        after = S.runPure (conduitAnswer [S.bob, S.carol]) board Engine.runStep
    Spec.assertEqWith s "bob took carol's 13" (S.lifeOf S.bob after) (Just 13)
    Spec.assertEqWith s "carol took bob's 27" (S.lifeOf S.carol after) (Just 27)
    Spec.assertEqWith s "alice, who activated it, is untouched" (S.lifeOf S.alice after) (Just 4)
    Spec.assertEqWith s "carol gained 14" (lifeGains after) [(S.carol, 14)]
    Spec.assertEqWith s "bob lost 14" (lifeLosses after) [(S.bob, 14)]
    -- The ability was really activated, so an exchange that did nothing cannot
    -- pass the assertions above by leaving the board alone.
    Spec.assertEqWith s "and the Conduit paid its own {T}" (fmap Object.tapped (Game.lookupObject conduitId after)) (Just TapState.Tapped)

  -- The same board and the same interpreter, differing only in which two players
  -- the answer names: the controller is a side when she is TARGETED, and the slot
  -- is what decides.
  Spec.it s "CR 601.2c both sides are read from the slot, the controller included when named" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    let (_, board) = soulConduitBoard conduit island 4 27 13
        after = S.runPure (conduitAnswer [S.alice, S.bob]) board Engine.runStep
    Spec.assertEqWith s "alice took bob's 27" (S.lifeOf S.alice after) (Just 27)
    Spec.assertEqWith s "bob took alice's 4" (S.lifeOf S.bob after) (Just 4)
    Spec.assertEqWith s "carol, whom nobody named, is untouched" (S.lifeOf S.carol after) (Just 13)

  -- CR 701.12a: "if the entire exchange can't be completed, no part of the
  -- exchange occurs." One of the two targets leaves the game after the ability is
  -- on the stack, so CR 608.2b drops her (a departed player is no longer in CR
  -- 115's pool) and the ability resolves with one side and no exchange -- rather
  -- than falling back on the controller, which is the reading this discriminates.
  Spec.it s "CR 701.12a an exchange left with one side does nothing at all" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    let (conduitId, board) = soulConduitBoard conduit island 4 27 13
        answer :: Prompt.Prompt r -> r
        answer = conduitAnswer [S.bob, S.carol]
    case Activatable.abilitiesFor conduitId board of
      [ability] -> do
        let activated = S.runPure answer board (Activate.activateAbility S.alice conduitId ability)
            gone = S.runPure answer activated (Departure.leaveGame Departure.Type.Conceded S.carol)
            after = S.runPure answer gone Stack.resolveTop
        Spec.assertEqWith s "bob, the surviving target, keeps his 27" (S.lifeOf S.bob after) (Just 27)
        Spec.assertEqWith s "and alice, who is no side of it, keeps her 4" (S.lifeOf S.alice after) (Just 4)
        Spec.assertEqWith s "no gain" (lifeGains after) []
        Spec.assertEqWith s "no loss" (lifeLosses after) []
      other -> Spec.assertFailure s ("expected exactly one activated ability on the Conduit, got " <> show (length other))

  -- CR 701.12c's deferral to CR 119.7: bob is at 27 and carol at 13, so the
  -- exchange would RAISE carol -- and carol can't gain life, because alice
  -- controls a Giant Cindermaw ("Players can't gain life"). CR 119.7 says the
  -- exchange won't happen, and the WHOLE exchange: bob's loss does not happen
  -- either, which is the reading a per-side gate would get wrong.
  Spec.it s "CR 119.7 an exchange that would raise a player who can't gain life doesn't happen at all" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    cindermaw <- S.printingOf s registry "Giant Cindermaw"
    let (conduitId, plain) = soulConduitBoard conduit island 4 27 13
        board = snd (S.addPermanent cindermaw S.alice plain)
        after = S.runPure (conduitAnswer [S.bob, S.carol]) board Engine.runStep
    Spec.assertEqWith s "carol, whom it would have raised, keeps her 13" (S.lifeOf S.carol after) (Just 13)
    Spec.assertEqWith s "and bob, whom it would have lowered, keeps his 27" (S.lifeOf S.bob after) (Just 27)
    Spec.assertEqWith s "no gain" (lifeGains after) []
    Spec.assertEqWith s "no loss" (lifeLosses after) []
    -- The ability was really activated, so the assertions above cannot pass
    -- because nothing happened at all.
    Spec.assertEqWith s "and the Conduit paid its own {T}" (fmap Object.tapped (Game.lookupObject conduitId after)) (Just TapState.Tapped)

  -- The paired control on the same board: the Cindermaw is the only difference,
  -- so the case above is not passing for want of mana or a target.
  Spec.it s "CR 701.12c without the Cindermaw the same exchange happens" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    let (_, board) = soulConduitBoard conduit island 4 27 13
        after = S.runPure (conduitAnswer [S.bob, S.carol]) board Engine.runStep
    Spec.assertEqWith s "carol took bob's 27" (S.lifeOf S.carol after) (Just 27)

-- CR 701.12g on alice's upkeep, three seats, the exchanger carrying a +1/+1
-- counter so that CR 613.4b's layer 7b and CR 613.4c's layer 7c tell apart:
-- the life side reaches the PROJECTED value, and the creature side's new base
-- still takes the counter on top (Tree of Redemption's 2018-03-16 ruling).
exchangeValuesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exchangeValuesSpec s registry = Spec.describe s "ExchangeValues" $ do
  -- Tree of Redemption, a 0/13 made 1/14, with alice at 7: she reaches 14 and
  -- its base toughness becomes 7, so it is a 1/8.
  Spec.it s "CR 701.12g Tree of Redemption exchanges its controller's life with its toughness" $ do
    tree <- S.printingOf s registry "Tree of Redemption"
    let (treeId, board) = exchangerBoard tree Nothing 7
        after = S.runPure (conduitAnswer []) board Engine.runStep
    Spec.assertEqWith s "alice took the Tree's previous toughness, counter included" (S.lifeOf S.alice after) (Just 14)
    Spec.assertEqWith s "the Tree's base toughness became 7, the counter on top, its power untouched" (S.powerToughnessOf treeId after) (Just (1, 8))
    Spec.assertEqWith s "and the Tree paid its own {T}" (fmap Object.tapped (Game.lookupObject treeId after)) (Just TapState.Tapped)

  -- CR 119.7 through CR 701.12a: the board above with bob's Giant Cindermaw
  -- ("Players can't gain life"). alice's side is barred, so the Tree's side
  -- does not happen either.
  Spec.it s "CR 119.7 a player who can't gain life doesn't exchange, and neither does the toughness" $ do
    tree <- S.printingOf s registry "Tree of Redemption"
    cindermaw <- S.printingOf s registry "Giant Cindermaw"
    let (treeId, board) = exchangerBoard tree (Just cindermaw) 7
        after = S.runPure (conduitAnswer []) board Engine.runStep
    Spec.assertEqWith s "alice keeps her 7" (S.lifeOf S.alice after) (Just 7)
    Spec.assertEqWith s "and the Tree is still a 1/14" (S.powerToughnessOf treeId after) (Just (1, 14))
    Spec.assertEqWith s "though the Tree paid its own {T}" (fmap Object.tapped (Game.lookupObject treeId after)) (Just TapState.Tapped)

  -- Evra, Halcyon Witness, a 4/4 made 5/5, with alice at 9: the power side.
  -- She reaches 5 and Evra's base power becomes 9, so it is a 10/5.
  Spec.it s "CR 701.12g Evra, Halcyon Witness exchanges its controller's life with its power" $ do
    evra <- S.printingOf s registry "Evra, Halcyon Witness"
    island <- S.printingOf s registry "Island"
    let (evraId, plain) = exchangerBoard evra Nothing 9
        board = List.foldl' (\gs _ -> snd (S.addPermanent island S.alice gs)) plain [1 .. 4 :: Int]
        after = S.runPure (conduitAnswer []) board Engine.runStep
    Spec.assertEqWith s "alice took Evra's previous power, counter included" (S.lifeOf S.alice after) (Just 5)
    Spec.assertEqWith s "Evra's base power became 9, the counter on top, its toughness untouched" (S.powerToughnessOf evraId after) (Just (10, 5))

  -- Serene Master, a 0/2, blocks alice's Hill Giant, a 3/3, and its trigger
  -- targets the Giant (CR 509.1g's IsBlockedBySource): the powers swap until
  -- end of combat (CR 511.2), so the Master deals 3 and takes 0.
  Spec.it s "CR 701.12g Serene Master swaps powers with the creature it blocks until end of combat" $ do
    master <- S.printingOf s registry "Serene Master"
    giant <- S.printingOf s registry "Hill Giant"
    let (board, giants, masters) = S.combatBoardOf [giant] [master]
        -- Aims AWAY from the Giant whenever anything else is offered, so a filter
        -- letting the Master itself in is caught aiming there.
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseTargets _ _ _ sets -> S.preferring (\r -> notElem r (fmap Recipient.ToCreature giants)) sets
          _ -> S.aggressiveAnswer p
        after = S.runCombat answer board
    case (giants, masters) of
      ([giantId], [masterId]) -> do
        Spec.assertBool s (not (Set.member giantId (GameState.battlefield after))) "the Giant died to the Master's borrowed 3 power"
        Spec.assertEqWith s "CR 511.2 the Master survived the Giant's borrowed 0 power and is a 0/2 again after combat" (S.powerToughnessOf masterId after) (Just (0, 2))
      _ -> Spec.assertFailure s "expected one creature a side"

-- `exchanger` on alice's battlefield with one +1/+1 counter, alice at `life`,
-- bob at 20 and optionally controlling `bobs`, in alice's upkeep with priority.
-- The schedule surgery is mirrorBoard's.
exchangerBoard :: Printing.Printing -> Maybe Printing.Printing -> Integer -> (ObjectId.ObjectId, GameState.GameState)
exchangerBoard exchanger bobs life =
  let (exchangerId, gs0) = S.addPermanent exchanger S.alice S.threePlayerGame
      gs1 = maybe gs0 (\printing -> snd (S.addPermanent printing S.bob gs0)) bobs
      gs2 = S.addCounter CounterKind.PlusOnePlusOne 1 exchangerId gs1
   in ( exchangerId,
        gs2
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.Beginning BeginningStep.Upkeep,
            GameState.priority = Just S.alice,
            GameState.remaining = Seq.drop 1 (GameState.remaining gs2),
            GameState.players = Map.adjust (\pl -> pl {Player.life = life}) S.alice (GameState.players gs2)
          }
      )

-- CR 119.5: "If an effect sets a player's life total to a specific number, the
-- player gains or loses the necessary amount of life to end up with the new
-- total." So a set is NOT a third kind of life event: it is a gain or a loss,
-- whichever the arithmetic makes it, and everything that watches gaining or
-- losing life sees it. The whole group exists to hold that reading in place.
--
-- Two cards prove it, and it takes two because neither reaches both directions:
--
--   * Magister Sphinx, {4}{W}{U}{B} Artifact Creature -- Sphinx 5/5 with flying,
--     "When this creature enters, target player's life total becomes 10." The
--     literal, so the SAME card is a gain at one seat and a loss at another --
--     and the first two cases below are one board differing in nothing but which
--     seat the trigger names.
--   * Arbiter of Knollridge, {6}{W} Creature -- Giant Wizard 5/5 with vigilance,
--     "When this creature enters, each player's life total becomes the highest
--     life total among all players." The fold, and the several-recipients shape a
--     targeted card cannot reach.
--
-- Three seats at 4, 27 and 13 -- distinct, and one above 10 and two below, so the
-- Sphinx cases tell a gain from a loss. 27 is not the sum (44), the count (3) or
-- the least (4), so Arbiter's one number falsifies every other fold. Only the
-- CR 119.9 case changes a starting total, moving carol to 10 so that the seat the
-- trigger names is already there.
--
-- The watchers are what make the claim about EVENTS rather than about totals.
-- Ajani's Pridemate ("whenever you gain life, put a +1/+1 counter on this
-- creature") is on the board for the gain side and Mindcrank ("whenever an
-- opponent loses life, that player mills that many cards") for the loss side, so
-- every case asserts which of the two fired -- and a set that wrote Player.life
-- directly would leave both silent while every life total still came out right.
setLifeTotalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
setLifeTotalSpec s registry =
  let -- Cast, then settle-and-resolve until the stack runs dry: the spell, then
      -- CR 603.6a's entry trigger, then whatever the life change itself
      -- triggered. Deliberately NOT Engine.priorityLoop, which advances the turn
      -- and clears GameState.events out from under lifeGains and lifeLosses. Six
      -- passes is more than the deepest case needs, and settling or resolving an
      -- empty stack is a no-op.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step board = S.runPure answer (S.runPure answer board Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\board _ -> step board) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- Pinned, not searched: the trigger's target slot takes `who` and nothing
      -- else, so a mutation cannot be repaired by an answerer that goes looking
      -- for a legal option. Three players and a count of one, so there is a real
      -- choice to pin.
      aimedAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer who) sets
        _ -> S.identityAnswer p
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      graveyardSize pid gs = Seq.length (Map.findWithDefault Seq.empty pid (GameState.graveyard gs))
      -- The three seats, alice holding the mana and both watchers, and every
      -- library stocked so Mindcrank has something to mill and CR 104.3c never
      -- fires. `lands` is the mana base the spell needs; everything else is
      -- shared by all four cases.
      setBoard lands pridemate mindcrank filler spell aliceLife bobLife carolLife =
        let withLands = List.foldl' (\board (printing, n) -> S.landsFor printing S.alice n board) S.threePlayerGame lands
            (aliceMate, withAliceMate) = S.addPermanent pridemate S.alice withLands
            (bobMate, withBobMate) = S.addPermanent pridemate S.bob withAliceMate
            (_, withCrank) = S.addPermanent mindcrank S.alice withBobMate
            stocked = List.foldl' (\board pid -> stockLibrary filler pid 30 board) withCrank [S.alice, S.bob, S.carol]
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = stocked {GameState.players = at S.alice aliceLife (at S.bob bobLife (at S.carol carolLife (GameState.players stocked)))}
            (gs, spellId) = S.handOne spell lifed
         in (aliceMate, bobMate, spellId, gs)
      addPikers piker pid n gs = List.foldl' (\board _ -> snd (S.addPermanent piker pid board)) gs [1 .. n :: Int]
      -- Biorhythm's board, where what differs between the seats is their CREATURE
      -- COUNT rather than their life total. setBoard leaves alice a Pridemate and
      -- a Mindcrank -- an ARTIFACT, so not one of her creatures -- and bob a
      -- Pridemate, so the Pikers below take alice to 4 creatures and bob to 3.
      -- `carolPikers` is the one knob the pair of cases turns.
      --
      -- Life totals 2, 3 and 9 against counts 4, 3 and 0: every seat's answer is
      -- distinct, no seat's answer is any other seat's, and only bob's happens to
      -- equal his own starting total -- which is what the CR 119.9 half of the
      -- pair needs. bob's count and bob's life coinciding is why the other two
      -- seats are there: alice's 2 against 4 and carol's 9 against 0 tell "counts
      -- creatures" from "reads a life total".
      biorhythmBoard carolPikers = do
        forest <- S.printingOf s registry "Forest"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        biorhythm <- S.printingOf s registry "Biorhythm"
        let (aliceMate, bobMate, spellId, gs) = setBoard [(forest, 8)] pridemate mindcrank piker biorhythm 2 3 9
        pure (aliceMate, bobMate, spellId, addPikers piker S.carol carolPikers (addPikers piker S.bob 2 (addPikers piker S.alice 3 gs)))
      sphinxBoard aliceLife bobLife carolLife = do
        plains <- S.printingOf s registry "Plains"
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        sphinx <- S.printingOf s registry "Magister Sphinx"
        pure (setBoard [(plains, 5), (island, 1), (swamp, 1)] pridemate mindcrank piker sphinx aliceLife bobLife carolLife)
   in Spec.describe s "SetLifeTotal" $ do
        -- The gain direction. alice is BELOW 10, so reaching it is a gain of 6 --
        -- and her Pridemate sees it, which is the whole CR 119.5 claim.
        Spec.it s "CR 119.5 Magister Sphinx sets a total UPWARD, and that is a life gain" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 13
          let after = castAndTrigger (aimedAt S.alice) spellId gs
          Spec.assertEqWith s "alice reached 10" (S.lifeOf S.alice after) (Just 10)
          Spec.assertEqWith s "bob, untargeted, keeps his 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol, untargeted, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a gain of exactly 6" (lifeGains after) [(S.alice, 6)]
          Spec.assertEqWith s "and as no loss at all" (lifeLosses after) []
          Spec.assertEqWith s "alice's Pridemate saw the gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's did not: it was not his life" (countersOn bobMate after) (Just 0)
          Spec.assertEqWith s "and Mindcrank stayed silent: nobody lost life" (graveyardSize S.bob after) 0
        -- The control twin, differing in ONE thing: the seat the trigger names.
        -- bob is ABOVE 10, so the identical card is a LOSS of 17 -- Mindcrank
        -- fires and the Pridemate does not, which is the pair that tells a set
        -- from a gain.
        Spec.it s "CR 119.5 the control: the same card set DOWNWARD is a life loss" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 13
          let after = castAndTrigger (aimedAt S.bob) spellId gs
          Spec.assertEqWith s "bob came down to 10" (S.lifeOf S.bob after) (Just 10)
          Spec.assertEqWith s "alice, untargeted, keeps her 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "carol, untargeted, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a loss of exactly 17" (lifeLosses after) [(S.bob, 17)]
          Spec.assertEqWith s "and as no gain at all" (lifeGains after) []
          Spec.assertEqWith s "Mindcrank milled bob for exactly 17" (graveyardSize S.bob after) 17
          Spec.assertEqWith s "alice's Pridemate stayed silent" (countersOn aliceMate after) (Just 0)
          Spec.assertEqWith s "and so did bob's: losing life is not gaining it" (countersOn bobMate after) (Just 0)
        -- CR 119.9's own last sentence, on the set: carol is ALREADY at 10, so
        -- the necessary amount is 0, no life event occurs, and neither watcher
        -- may fire. The Sphinx really entered, so a spell that did nothing cannot
        -- pass this by leaving the board alone.
        Spec.it s "CR 119.9 setting a total to the number it already holds is neither a gain nor a loss" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 10
          let after = castAndTrigger (aimedAt S.carol) spellId gs
          Spec.assertEqWith s "carol is still at 10" (S.lifeOf S.carol after) (Just 10)
          Spec.assertEqWith s "no gain" (lifeGains after) []
          Spec.assertEqWith s "no loss" (lifeLosses after) []
          Spec.assertEqWith s "no Pridemate counter anywhere" (fmap (\oid -> countersOn oid after) [aliceMate, bobMate]) [Just 0, Just 0]
          Spec.assertEqWith s "and Mindcrank milled nobody" (graveyardSize S.carol after) 0
          Spec.assertEqWith s "and the Sphinx really entered, so a spell that never resolved cannot pass this" (Set.size (GameState.battlefield after)) (Set.size (GameState.battlefield gs) + 1)
        -- Arbiter of Knollridge: SEVERAL recipients from one instruction, and a
        -- folded number rather than a literal. 27 is the highest, and it is not
        -- the sum (44), the count (3), the least (4) or any seat's own total, so
        -- one set of three assertions falsifies every other reading.
        --
        -- bob is the seat that is ALREADY highest, and his own Pridemate is the
        -- point of the case: he ends on the number he started on, so CR 119.9
        -- says no life gain event happened to him even though the effect named
        -- him. That is the assertion a raw "write the total to every player"
        -- implementation fails.
        Spec.it s "CR 119.5 Arbiter of Knollridge raises every seat to the HIGHEST total, gaining only where the total moves" $ do
          plains <- S.printingOf s registry "Plains"
          pridemate <- S.printingOf s registry "Ajani's Pridemate"
          mindcrank <- S.printingOf s registry "Mindcrank"
          piker <- S.printingOf s registry "Goblin Piker"
          arbiter <- S.printingOf s registry "Arbiter of Knollridge"
          let (aliceMate, bobMate, spellId, gs) = setBoard [(plains, 7)] pridemate mindcrank piker arbiter 4 27 13
              after = castAndTrigger S.identityAnswer spellId gs
          Spec.assertEqWith s "alice rose from 4 to 27" (S.lifeOf S.alice after) (Just 27)
          Spec.assertEqWith s "bob, already highest, stayed at 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol rose from 13 to 27" (S.lifeOf S.carol after) (Just 27)
          Spec.assertEqWith s "exactly the two seats that moved gained, by exactly their deltas" (lifeGains after) [(S.alice, 23), (S.carol, 14)]
          Spec.assertEqWith s "nobody lost life" (lifeLosses after) []
          Spec.assertEqWith s "alice's Pridemate saw her gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's did not: a total set to itself is no gain (CR 119.9)" (countersOn bobMate after) (Just 0)
          Spec.assertEqWith s "and Mindcrank milled nobody" (graveyardSize S.carol after) 0
        -- Biorhythm, {6}{G}{G} Sorcery: "Each player's life total becomes the
        -- number of creatures they control." The number is EACH RECIPIENT'S OWN,
        -- which neither producer above can tell from a single evaluation: the
        -- Sphinx names one seat and Arbiter names one number for the whole table.
        --
        -- Three seats, three different counts -- alice 4, bob 3, carol 0 -- so no
        -- seat's answer can stand in for another's, and a reading that evaluated
        -- once from the CONTROLLER's perspective would hand bob and carol alice's
        -- 4. Each seat carries one half of the rule besides:
        --
        --   * alice gains (2 -> 4), and her Pridemate sees it;
        --   * bob's count is his current total, so CR 119.9 leaves him with no
        --     life event at all and his Pridemate silent;
        --   * carol controls nothing, so her total becomes 0 and CR 104.3b takes
        --     her out of the game.
        Spec.it s "CR 119.5 Biorhythm sets EACH seat to its OWN creature count" $ do
          (aliceMate, bobMate, spellId, gs) <- biorhythmBoard 0
          let after = castAndTrigger S.identityAnswer spellId gs
          Spec.assertEqWith s "alice, controlling 4 creatures, rose from 2 to 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob, controlling 3, is at 3 -- not alice's 4" (S.lifeOf S.bob after) (Just 3)
          Spec.assertEqWith s "carol, controlling none, fell from 9 to 0" (S.lifeOf S.carol after) (Just 0)
          Spec.assertEqWith s "only alice gained, and by her own delta" (lifeGains after) [(S.alice, 2)]
          Spec.assertEqWith s "only carol lost, and by hers" (lifeLosses after) [(S.carol, 9)]
          Spec.assertEqWith s "alice's Pridemate saw her gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's did not: his total was already his count (CR 119.9)" (countersOn bobMate after) (Just 0)
          Spec.assertBool s (notElem S.carol (Game.stillPlaying after)) "CR 104.3b took carol, at 0 life, out of the game"
          Spec.assertEqWith s "and left the other two in it" (filter (`elem` Game.stillPlaying after) [S.alice, S.bob]) [S.alice, S.bob]
        -- The control twin, differing in ONE thing: carol controls a single Piker.
        -- Her answer moves 0 -> 1 while alice's and bob's do not move at all,
        -- which is the pair that shows the count is read per seat; and a total of
        -- 1 is a total CR 104.3b has no quarrel with, so the state-based action
        -- above fired on carol's number rather than on her being named.
        Spec.it s "CR 104.3b the control: one creature is one life, and carol stays in the game" $ do
          (aliceMate, bobMate, spellId, gs) <- biorhythmBoard 1
          let after = castAndTrigger S.identityAnswer spellId gs
          Spec.assertEqWith s "alice is unmoved at 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob is unmoved at 3" (S.lifeOf S.bob after) (Just 3)
          Spec.assertEqWith s "carol, controlling one creature, fell from 9 to 1" (S.lifeOf S.carol after) (Just 1)
          Spec.assertEqWith s "alice still gained 2" (lifeGains after) [(S.alice, 2)]
          Spec.assertEqWith s "carol lost 8 rather than 9" (lifeLosses after) [(S.carol, 8)]
          Spec.assertBool s (elem S.carol (Game.stillPlaying after)) "and stayed in the game"
          Spec.assertEqWith s "Mindcrank milled carol for exactly her loss" (graveyardSize S.carol after) 8
          Spec.assertEqWith s "alice's Pridemate saw her gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's stayed silent" (countersOn bobMate after) (Just 0)

-- Beacon of Immortality ({5}{W} Instant -- "Double target player's life total.
-- Shuffle Beacon of Immortality into its owner's library.", Oracle text verified
-- against Scryfall 2026-09-19).
--
-- CR 701.10d states doubling a life total as an arithmetic rather than a game
-- action: the player "gains or loses an amount of life such that their new life
-- total is twice its current value". So it needs no opcode of its own, CR
-- 701.10b's reason in Pawl.PowerToughnessSpec's Unleash Fury group. It is CR
-- 119.5's set -- setLifeTotalSpec above -- over Quantity.Times of 2 and the
-- TARGET's own Quantity.LifeTotal, and the gain or loss falls out of that set.
--
-- Three seats at 4, 27 and 13: distinct, and no seat's double (8, 54, 26) is any
-- seat's total or any other seat's double. So the totals falsify a reading that
-- doubles the CONTROLLER's total, one that doubles a literal, and one that
-- doubles every seat -- and the two cases are ONE board differing in nothing but
-- the seat the target names.
--
-- bob's Ajani's Pridemate ("whenever you gain life, put a +1\/+1 counter on this
-- creature") is what makes the claim about the life EVENT rule 701.10d names
-- rather than about the total: a doubling written as a raw Player.life write
-- leaves it silent while 54 still appears.
--
-- Only the UPWARD direction is reachable, and that is a fact about the rules
-- rather than a gap: doubling lowers a total only from one below 0, and CR
-- 104.3b takes a player at 0 or less out of the game the next time a player
-- would receive priority. A total of exactly 0 doubles to itself, which CR 119.9
-- makes no life gain event, and setLifeTotalSpec's CR 119.9 case holds that
-- reading already.
--
-- Written against S.runPure rather than the Board harness, and that is the
-- shuffle clause's doing: the harness has no vocabulary for Prompt.Shuffle,
-- which randomness rather than any player answers (S.promptDecider), so a card
-- that shuffles cannot be scripted through it.
doubleLifeTotalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
doubleLifeTotalSpec s registry =
  let -- setLifeTotalSpec's driver: cast, then settle-and-resolve until the stack
      -- runs dry, so the spell and the life gain's trigger both resolve.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step g = S.runPure answer (S.runPure answer g Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\g _ -> step g) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- PINNED to a seat rather than searched for, so a mutation cannot be
      -- repaired by an answerer that goes hunting for a legal target; three seats
      -- against a count of one leaves a real choice to pin.
      aimedAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer who) sets
        _ -> S.identityAnswer p
      -- alice holds six Plains for {5}{W} and nothing in her library, so the
      -- shuffled Beacon is the whole of it. Nothing here draws a card, so an
      -- empty library never reaches CR 104.3c.
      beaconBoard = do
        plains <- S.printingOf s registry "Plains"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        beacon <- S.printingOf s registry "Beacon of Immortality"
        let withLands = S.landsFor plains S.alice 6 S.threePlayerGame
            (mate, withMate) = S.addPermanent pridemate S.bob withLands
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = withMate {GameState.players = at S.alice 4 (at S.bob 27 (at S.carol 13 (GameState.players withMate)))}
            (gs, spellId) = S.handOne beacon lifed
        pure (mate, spellId, gs)
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
   in Spec.describe s "Beacon of Immortality" $ do
        Spec.it s "CR 701.10d doubling a life total leaves the target at twice its OWN total, as a life gain" $ do
          (mate, spellId, gs) <- beaconBoard
          let after = castAndTrigger (aimedAt S.bob) spellId gs
          Spec.assertEqWith s "bob, the target, went from 27 to 54" (S.lifeOf S.bob after) (Just 54)
          Spec.assertEqWith s "alice, who cast it, keeps her 4: the total doubled is the TARGET's" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "carol, untargeted, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a gain of exactly bob's own 27" (lifeGains after) [(S.bob, 27)]
          Spec.assertEqWith s "and as no loss at all" (lifeLosses after) []
          Spec.assertEqWith s "bob's Pridemate saw the gain, so CR 119.5's gain is what happened" (countersOn mate after) (Just 1)
          Spec.assertEqWith s "and the Beacon shuffled ITSELF into its owner's library" (namesIn Zone.Library S.alice after) [Just (CardName.MkCardName (Text.pack "Beacon of Immortality"))]
          Spec.assertEqWith s "rather than resolving into her graveyard" (namesIn Zone.Graveyard S.alice after) []
        -- The control twin, differing in ONE thing: the seat the target names.
        Spec.it s "CR 701.10d the control: the same card aimed at another seat doubles THAT seat's total" $ do
          (mate, spellId, gs) <- beaconBoard
          let after = castAndTrigger (aimedAt S.alice) spellId gs
          Spec.assertEqWith s "alice, now the target, went from 4 to 8" (S.lifeOf S.alice after) (Just 8)
          Spec.assertEqWith s "bob is unmoved at 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol is unmoved at 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a gain of exactly alice's own 4" (lifeGains after) [(S.alice, 4)]
          Spec.assertEqWith s "and bob's Pridemate stayed silent: it was not his life" (countersOn mate after) (Just 0)

-- Reverse the Sands, {6}{W}{W} Sorcery: "Redistribute any number of players'
-- life totals. (Each of those players gets one life total back.)" CR 119.7 and
-- CR 119.8 name the action; every seat's new total is CR 119.5's gain or loss,
-- which setLifeTotalSpec above holds in place for one recipient and this group
-- holds for a whole permutation.
--
-- The choice is the resolving controller's (CR 608.2c-d, and the card's own
-- ruling "you choose which player gets which life total when the spell
-- resolves"), so the engine may never pick it. Three seats at 27, 4 and 13 --
-- distinct, so no two assignments produce the same board -- and the two positive
-- cases are ONE board answered two ways, differing in nothing but the
-- permutation. They disagree at every seat, which is what no fixed permutation
-- the engine could have chosen for itself can do.
--
-- Neither permutation is the identity and neither is a rotation: both are
-- transpositions, so each has a seat that is chosen and mapped to ITSELF. That
-- seat is the one that tells "kept its own total" from "was never chosen" --
-- both leave the total alone, and only the watchers agree with CR 119.9 that
-- neither is a life event.
--
-- The watchers are what make the claim about EVENTS rather than totals. A
-- Pridemate under each seat is the gain side ("whenever you gain life") and
-- bob's Mindcrank is the loss side ("whenever an opponent loses life, that
-- player mills that many cards"), so each case pins which of the two fired at
-- which seat. A redistribution written as three raw Player.life writes would
-- leave all four silent while every total still came out right.
redistributeLifeTotalsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
redistributeLifeTotalsSpec s registry =
  let -- setLifeTotalSpec's driver: cast, then settle-and-resolve until the stack
      -- runs dry, so the spell and everything its life changes triggered all
      -- resolve. Deliberately NOT Engine.priorityLoop, which advances the turn
      -- and clears GameState.events out from under lifeGains and lifeLosses.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step board = S.runPure answer (S.runPure answer board Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\board _ -> step board) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- PINNED, not searched: the answer is exactly these pairs whatever the
      -- prompt offers, so a mutation to the engine's own handling cannot be
      -- repaired by an answerer that goes hunting for a legal permutation.
      assigning :: [(PlayerId.PlayerId, PlayerId.PlayerId)] -> Prompt.Prompt r -> r
      assigning pairs p = case p of
        Prompt.ChooseRedistribution {} -> Map.fromList pairs
        _ -> S.identityAnswer p
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      graveyardSize pid gs = Seq.length (Map.findWithDefault Seq.empty pid (GameState.graveyard gs))
      -- alice holds the mana and casts; every seat has a Pridemate, bob has the
      -- Mindcrank, and every library is stocked deep enough that a 23-card mill
      -- never reaches CR 104.3c.
      sandsBoard = do
        plains <- S.printingOf s registry "Plains"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        sands <- S.printingOf s registry "Reverse the Sands"
        let withLands = S.landsFor plains S.alice 8 S.threePlayerGame
            (aliceMate, g1) = S.addPermanent pridemate S.alice withLands
            (bobMate, g2) = S.addPermanent pridemate S.bob g1
            (carolMate, g3) = S.addPermanent pridemate S.carol g2
            (_, g4) = S.addPermanent mindcrank S.bob g3
            stocked = List.foldl' (\board pid -> stockLibrary piker pid 40 board) g4 [S.alice, S.bob, S.carol]
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = stocked {GameState.players = at S.alice 27 (at S.bob 4 (at S.carol 13 (GameState.players stocked)))}
            (gs, spellId) = S.handOne sands lifed
        pure (aliceMate, bobMate, carolMate, spellId, gs)
      -- The board every refused answer is checked against: nothing moved, and
      -- nothing was logged. Shared so the three #222 cases differ in exactly one
      -- thing -- the answer.
      assertUntouched after = do
        Spec.assertEqWith s "alice keeps her 27" (S.lifeOf S.alice after) (Just 27)
        Spec.assertEqWith s "bob keeps his 4" (S.lifeOf S.bob after) (Just 4)
        Spec.assertEqWith s "carol keeps her 13" (S.lifeOf S.carol after) (Just 13)
        Spec.assertEqWith s "no gain" (lifeGains after) []
        Spec.assertEqWith s "no loss" (lifeLosses after) []
        -- Load-bearing: the spent sorcery and nothing else is in alice's
        -- graveyard, so the spell really RESOLVED and no Mindcrank mill followed.
        -- Without it every case below would pass just as well on a spell that
        -- fizzled.
        Spec.assertEqWith s "the spell resolved, and milled nobody doing it" (graveyardSize S.alice after) 1
   in Spec.describe s "RedistributeLifeTotals" $ do
        -- alice hands her 27 to carol and takes carol's 13; bob is CHOSEN and
        -- mapped to himself. One seat gains, one loses, one is chosen and stays
        -- put -- and 13, 4 and 27 are three different numbers, so this one set of
        -- assertions falsifies every other permutation of the three.
        Spec.it s "Reverse the Sands whole card: the controller's permutation is what happens, seat by seat" $ do
          (aliceMate, bobMate, carolMate, spellId, gs) <- sandsBoard
          let after = castAndTrigger (assigning [(S.alice, S.carol), (S.bob, S.bob), (S.carol, S.alice)]) spellId gs
          Spec.assertEqWith s "alice took carol's 13" (S.lifeOf S.alice after) (Just 13)
          Spec.assertEqWith s "bob, mapped to himself, is still on 4" (S.lifeOf S.bob after) (Just 4)
          Spec.assertEqWith s "carol took alice's 27" (S.lifeOf S.carol after) (Just 27)
          -- CR 119.5, per seat: the necessary amount, and its own sign.
          Spec.assertEqWith s "carol gained exactly the difference" (lifeGains after) [(S.carol, 14)]
          Spec.assertEqWith s "alice lost exactly the difference" (lifeLosses after) [(S.alice, 14)]
          Spec.assertEqWith s "carol's Pridemate saw her gain" (countersOn carolMate after) (Just 1)
          Spec.assertEqWith s "alice's did not: losing life is not gaining it" (countersOn aliceMate after) (Just 0)
          -- CR 119.9 on the fixed point: bob was chosen, so a redistribution
          -- that handed out totals blindly would still have "given" him one.
          -- Taking his own is a delta of 0 and therefore no life event at all.
          Spec.assertEqWith s "bob's Pridemate stayed silent: his own total back is no gain" (countersOn bobMate after) (Just 0)
          -- 14 milled, plus the spent sorcery itself (CR 608.2n), which is alice's
          -- card and lands in her graveyard -- so this also witnesses that the
          -- spell really resolved rather than fizzling quietly.
          Spec.assertEqWith s "bob's Mindcrank milled alice for exactly what she lost" (graveyardSize S.alice after) 15
          Spec.assertEqWith s "and milled carol for nothing: she gained" (graveyardSize S.carol after) 0
        -- The control twin: the SAME board, differing in nothing but the
        -- permutation the controller names. Every seat lands somewhere else than
        -- it did above, so no permutation the engine picked for itself can
        -- satisfy both cases -- which is the whole second-invariant claim.
        Spec.it s "the same board answered differently redistributes differently at every seat" $ do
          (aliceMate, bobMate, carolMate, spellId, gs) <- sandsBoard
          let after = castAndTrigger (assigning [(S.alice, S.bob), (S.bob, S.alice), (S.carol, S.carol)]) spellId gs
          Spec.assertEqWith s "alice took bob's 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob took alice's 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol, mapped to herself, is still on 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "bob gained exactly the difference" (lifeGains after) [(S.bob, 23)]
          Spec.assertEqWith s "alice lost exactly the difference" (lifeLosses after) [(S.alice, 23)]
          Spec.assertEqWith s "bob's Pridemate saw his gain" (countersOn bobMate after) (Just 1)
          Spec.assertEqWith s "alice's stayed silent" (countersOn aliceMate after) (Just 0)
          Spec.assertEqWith s "carol's stayed silent: her own total back is no gain" (countersOn carolMate after) (Just 0)
          -- 23 milled plus the spent sorcery, as in the case above.
          Spec.assertEqWith s "bob's Mindcrank milled alice for exactly what she lost" (graveyardSize S.alice after) 24
        -- CR 119.7's own clause on a redistribution: "a player can't receive a
        -- new life total such that the player's life total would become
        -- higher". alice's Giant Cindermaw ("Players can't gain life") makes
        -- carol's rise to 27 a total she can't receive, so the permutation is
        -- not a legal answer and nothing happens -- assertUntouched's board,
        -- the same one an ill-formed answer lands on. The assignment is the
        -- first positive case's, so the Cindermaw is the only difference
        -- between the two.
        Spec.it s "CR 119.7 an assignment that would raise a player who can't gain life is not a legal answer" $ do
          cindermaw <- S.printingOf s registry "Giant Cindermaw"
          (_, _, _, spellId, gs) <- sandsBoard
          let after = castAndTrigger (assigning [(S.alice, S.carol), (S.bob, S.bob), (S.carol, S.alice)]) spellId (snd (S.addPermanent cindermaw S.alice gs))
          assertUntouched after
        -- A ROTATION, and the reason the two transpositions above are not enough
        -- on their own: a transposition is its own inverse, so reading the answer
        -- backwards -- giving each named player's total AWAY instead of handing it
        -- TO them -- lands on the very same board. This assignment's inverse is
        -- the other rotation, which lands on a different total at all three
        -- seats, so this is the case that pins the direction of the map.
        Spec.it s "a rotation moves every seat, and in the direction the answer names" $ do
          (aliceMate, bobMate, carolMate, spellId, gs) <- sandsBoard
          let after = castAndTrigger (assigning [(S.alice, S.bob), (S.bob, S.carol), (S.carol, S.alice)]) spellId gs
          Spec.assertEqWith s "alice took bob's 4, not carol's 13" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob took carol's 13, not alice's 27" (S.lifeOf S.bob after) (Just 13)
          Spec.assertEqWith s "carol took alice's 27, not bob's 4" (S.lifeOf S.carol after) (Just 27)
          Spec.assertEqWith s "the two seats that rose gained their own differences" (lifeGains after) [(S.bob, 9), (S.carol, 14)]
          Spec.assertEqWith s "and the one that fell lost hers" (lifeLosses after) [(S.alice, 23)]
          Spec.assertEqWith s "bob's Pridemate saw his gain" (countersOn bobMate after) (Just 1)
          Spec.assertEqWith s "carol's saw hers" (countersOn carolMate after) (Just 1)
          Spec.assertEqWith s "alice's stayed silent" (countersOn aliceMate after) (Just 0)
          -- The rotation is also what proves the totals are read from ONE
          -- snapshot: an implementation that set each seat in turn against the
          -- live board would hand bob the 4 alice had just taken.
          Spec.assertEqWith s "23 milled plus the spent sorcery" (graveyardSize S.alice after) 24
        -- The ruling's option (a), "leave the life totals as they are": "any
        -- number of players" includes none, so the empty answer is legal and
        -- quiet rather than refused.
        Spec.it s "redistributing among nobody is a legal answer and moves nothing" $ do
          (_, _, _, spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning []) spellId gs)
        -- #222, splitting: alice takes carol's 13 while carol keeps it, so one
        -- life total would end up on two seats -- exactly what "you can't split
        -- up a life total when you redistribute it" forbids. Refused ENTIRE,
        -- because there is no honest partial permutation to keep.
        Spec.it s "#222 an answer that hands out a total its owner keeps is refused" $ do
          (_, _, _, spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning [(S.alice, S.carol)]) spellId gs)
        -- #222, duplication: alice and bob both take carol's 13. The keys are all
        -- three seats but the totals handed out are only two, so it is not a
        -- bijection and alice's 27 would simply vanish.
        Spec.it s "#222 an answer giving two players the same life total is refused" $ do
          (_, _, _, spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning [(S.alice, S.carol), (S.bob, S.carol), (S.carol, S.alice)]) spellId gs)
        -- #222, an outsider: dave is not in this game. This answer IS a
        -- permutation -- its keys and its values are the same two seats -- so
        -- only the candidate check refuses it, which is what makes this case
        -- discriminating rather than a second copy of the one above.
        Spec.it s "#222 an answer naming a player who is not in the game is refused" $ do
          (_, _, _, spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning [(S.alice, S.dave), (S.dave, S.alice)]) spellId gs)
        -- CR 102.1: the offer is the players IN the game, not the keys of
        -- GameState.players, which keep a departed seat's row. Driven through
        -- Resolve.applyEffect rather than a cast, the narrowest path that raises
        -- the prompt at all.
        Spec.it s "CR 102.1 every player in the game is offered, beside the total they hold, and a departed seat is not" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          let (src, g0) = S.addPermanent piker S.alice S.threePlayerGame
              at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
              gs = g0 {GameState.players = at S.alice 27 (at S.bob 4 (at S.carol 13 (GameState.players g0)))}
              recording :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, Integer)] r
              recording p = case p of
                Prompt.ChooseRedistribution _ _ offered -> do
                  State.put offered
                  pure (S.identityAnswer p)
                _ -> pure (S.identityAnswer p)
              offerOf g = List.sort (State.execState (Engine.runGame recording g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.RedistributeLifeTotals)) [])
              gone = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.carol)
          Spec.assertEqWith s "all three seats, each beside its own total" (offerOf gs) [(S.alice, 27), (S.bob, 4), (S.carol, 13)]
          Spec.assertEqWith s "carol conceded, so she is no longer a candidate" (offerOf gone) [(S.alice, 27), (S.bob, 4)]
        -- Where the rules leave nothing to ask, do not ask. One candidate admits
        -- only the identity and no candidate not even that, so both are the same
        -- assignment however they are answered; two candidates is a real choice.
        Spec.it s "one remaining player leaves only the identity, so no prompt is raised" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          let (src, gs) = S.addPermanent piker S.alice S.threePlayerGame
              countingAnswer :: Prompt.Prompt r -> State.State Int r
              countingAnswer p = case p of
                Prompt.ChooseRedistribution {} -> do
                  State.modify (+ 1)
                  pure (S.identityAnswer p)
                _ -> pure (S.identityAnswer p)
              asks g = State.execState (Engine.runGame countingAnswer g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.RedistributeLifeTotals)) 0
              leaves pid g = S.runPure S.identityAnswer g (Departure.leaveGame Departure.Type.Conceded pid)
              two = leaves S.carol gs
              one = leaves S.bob two
          Spec.assertEqWith s "three seats: a real decision" (asks gs) 1
          Spec.assertEqWith s "two seats: still a real decision, the swap being legal" (asks two) 1
          Spec.assertEqWith s "one seat: only the identity, so nothing to ask" (asks one) 0

-- One with the Machine, the card that proves Aggregation.Greatest (#254):
-- "Draw cards equal to the greatest mana value among artifacts you control."
-- Nothing but the fold is new -- the effect is the existing Draw, the scope and
-- the filter were both already expressible, and the per-member quantity is the
-- existing Quantity.ManaValue (CR 202.3), the same read Karn, Legacy Reforged
-- wants.
--
-- Alice's board is Bonesplitter ({1}), Serum Powder ({3}) and Mindslaver ({6}),
-- chosen so that greatest (6), count (3), sum (10) and least (1) are four
-- DIFFERENT numbers: one hand-size assertion falsifies every other fold.
greatestSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
greatestSpec s registry = Spec.describe s "Greatest" $ do
  Spec.it s "CR 202.3 One with the Machine draws the GREATEST mana value, not the count, the sum or the least" $ do
    island <- S.printingOf s registry "Island"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    serumPowder <- S.printingOf s registry "Serum Powder"
    mindslaver <- S.printingOf s registry "Mindslaver"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    let base = S.landsInPlay island 4
        (_, withOne) = S.addPermanent bonesplitter S.alice base
        (_, withTwo) = S.addPermanent serumPowder S.alice withOne
        (_, withThree) = S.addPermanent mindslaver S.alice withTwo
        withLib = stockLibrary piker S.alice 10 withThree
        (gs, spellId) = S.handOne oneWithTheMachine withLib
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    -- The spell left the hand as it was cast, so the hand size IS the draw.
    Spec.assertEqWith s "alice drew six" (S.handSize S.alice after) 6
  Spec.it s "CR 109.5 an opponent's larger artifact does not raise \"artifacts YOU control\"" $ do
    -- Bob's Mindslaver ({6}) is on the same battlefield and is the largest
    -- artifact in the game; Alice's own Bonesplitter ({1}) is the answer.
    island <- S.printingOf s registry "Island"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    mindslaver <- S.printingOf s registry "Mindslaver"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    let base = S.landsInPlay island 4
        (_, withMine) = S.addPermanent bonesplitter S.alice base
        (_, withTheirs) = S.addPermanent mindslaver S.bob withMine
        withLib = stockLibrary piker S.alice 10 withTheirs
        (gs, spellId) = S.handOne oneWithTheMachine withLib
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew one, not six" (S.handSize S.alice after) 1
  Spec.it s "CR 205.2a a larger NONARTIFACT permanent does not raise \"ARTIFACTS you control\"" $ do
    -- Panglacial Wurm is {5}{G}{G} -- mana value 7, larger than any artifact
    -- in the pool -- and Alice controls it, so only the card-type conjunct
    -- keeps it out of the fold. Her four Islands are the same falsifier at
    -- mana value 0 (CR 202.1b / 202.3a), which no maximum could ever show.
    island <- S.printingOf s registry "Island"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    let base = S.landsInPlay island 4
        (_, withArtifact) = S.addPermanent bonesplitter S.alice base
        (_, withWurm) = S.addPermanent panglacialWurm S.alice withArtifact
        withLib = stockLibrary piker S.alice 10 withWurm
        (gs, spellId) = S.handOne oneWithTheMachine withLib
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew one, not seven" (S.handSize S.alice after) 1
  -- The empty matched set. No rule in the CR gives a maximum over nothing a
  -- value: CR 208.2a's "use 0 instead of that number" is scoped to a
  -- characteristic-defining ability, which One with the Machine's draw is not,
  -- and where the CR does want an empty maximum to be 0 it says so card-by-card
  -- (CR 714.2d, a Saga with no chapter abilities). So the fold answers Nothing
  -- -- undeterminable, the posture this codebase propagates everywhere -- and
  -- Resolve's Draw arm draws nothing for it.
  --
  -- OBSERVATIONALLY, Nothing and 0 are the same here, and the Gatherer
  -- ruling on Rishkar's Expertise ("if you control no creatures with power
  -- greater than 0 ... you draw no cards") is what this matches either way.
  -- Pawl.CountSpec pins the distinction where it IS visible, at the fold.
  Spec.it s "CR 208.2a controlling no artifacts draws nothing rather than substituting 0" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    let base = S.landsInPlay island 4
        withLib = stockLibrary piker S.alice 10 base
        (gs, spellId) = S.handOne oneWithTheMachine withLib
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew nothing" (S.handSize S.alice after) 0
    Spec.assertEqWith s "and her library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 10
  -- CR 707.2 / 613.2a: a copy's mana value is the COPIED object's, because the
  -- mana cost is one of the copiable values layer 1 replaces. The two numbers
  -- cannot coincide here: Clone is printed {3}{U} (mana value 4) and Darksteel
  -- Myr is printed {3} (mana value 3), so reading the printed card gives 4 and
  -- reading the copy gives 3.
  --
  -- The Myr is BOB's, which is what leaves the Clone alone among "artifacts YOU
  -- control" -- the maximum is then a single member and the assertion is about
  -- that member's mana value and nothing else.
  Spec.it s "CR 707.2 a Clone copying Darksteel Myr counts as mana value 3, not its own printed 4" $ do
    island <- S.printingOf s registry "Island"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    let base = S.landsInPlay island 4
        (_, withMyr) = S.addPermanent darksteelMyr S.bob base
        (_, staged) = S.spellOnStack clone S.alice withMyr
        -- CR 614.12a: the copy choice happens inside the Clone's own entry, so
        -- the answerer takes the one legal target -- bob's Myr is the only
        -- creature on the battlefield.
        entered = snd (Engine.runGamePure copyTheOnlyTarget staged (Stack.resolveTop >> Engine.settleForPriority))
        -- CR 104.3c: ten cards is far more than the three this draws, so alice
        -- cannot deck herself before the assertion.
        withLib = stockLibrary piker S.alice 10 entered
        (gs, spellId) = S.handOne oneWithTheMachine withLib
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew three, not four" (S.handSize S.alice after) 3
  -- CR 202.3b, second sentence: "If a permanent or spell is a copy of the back
  -- face of a nonmodal double-faced object (even if the card representing that
  -- copy is itself a double-faced card), the mana value of the copy is 0."
  --
  -- The NUMBER is the whole of what this adds to the Darksteel Myr case above.
  -- CR 707.2 already makes the Clone read the copied object's mana value rather
  -- than its own printed {3}{U}, and CR 712.8e already makes the copied object
  -- -- a transformed Thraben Gargoyle // Stonewing Antagonizer -- read its FRONT
  -- face's {1}. Without this rule the copy inherits that 1; with it the source
  -- and the copy report DIFFERENT mana values off the same copiable snapshot,
  -- and CR 202.3b is the only thing separating them.
  --
  -- The Gargoyle is BOB's, for the Darksteel Myr case's reason: it leaves the
  -- Clone alone among "artifacts YOU control", so the maximum folds over one
  -- member and the hand size is that member's mana value and nothing else. Both
  -- faces are Artifact Creature, so the copy is in the fold whichever face was
  -- copied and the card type is not what changes.
  --
  -- 0 is a dangerous number to assert: an empty fold, a copy that never
  -- happened and a Clone left as its printed 0/0 self all draw nothing too. So
  -- the copy is IDENTIFIED before its mana value is read -- its name, card types
  -- and 4/2 body say it really is Stonewing Antagonizer under alice's control --
  -- and bob's source is asserted at 1 in the same breath, which is what shows
  -- the 0 is the copy's own answer rather than a mana value reader that broke.
  Spec.it s "CR 202.3b a Clone copying a TRANSFORMED Stonewing Antagonizer has mana value 0, not the front face's 1" $ do
    island <- S.printingOf s registry "Island"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    case cloneOfGargoyle True island gargoyle clone piker oneWithTheMachine of
      (_, [], _) -> Spec.assertFailure s "no copy on alice's battlefield"
      (source, copy : _, after) -> do
        Spec.assertEqWith s "the copy is Stonewing Antagonizer" (Projection.namesOf copy after) (Set.singleton antagonizerName)
        Spec.assertEqWith s "an artifact creature alice controls" (Projection.cardTypesOf copy after, Projection.controllerOf copy after) (Set.fromList [CardType.Artifact, CardType.Creature], Just S.alice)
        Spec.assertEqWith s "with the back face's 4/2 body" (S.powerToughnessOf copy after) (Just (4, 2))
        Spec.assertEqWith s "CR 712.8e: bob's transformed permanent still reads its front face's 1" (Filter.manaValue (Projection.viewOfObject source after)) (Just 1)
        Spec.assertEqWith s "CR 202.3b: the copy of that back face reads 0" (Filter.manaValue (Projection.viewOfObject copy after)) (Just 0)
        Spec.assertEqWith s "so alice drew nothing" (S.handSize S.alice after) 0
        Spec.assertEqWith s "and her library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 10
  -- The control, and the reason the case above is not passed by an engine that
  -- answers 0 for every copy: the SAME fixture with the Gargoyle left front-face
  -- up. CR 202.3b's second sentence is about a copy of the BACK face, so a copy
  -- of the front face keeps CR 707.2's ordinary answer -- the copied object's
  -- {1} -- and alice draws one card rather than none.
  Spec.it s "CR 707.2 a Clone copying the UNTRANSFORMED Thraben Gargoyle keeps that face's mana value" $ do
    island <- S.printingOf s registry "Island"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    case cloneOfGargoyle False island gargoyle clone piker oneWithTheMachine of
      (_, [], _) -> Spec.assertFailure s "no copy on alice's battlefield"
      (source, copy : _, after) -> do
        Spec.assertEqWith s "the copy is Thraben Gargoyle" (Projection.namesOf copy after) (Set.singleton gargoyleName)
        Spec.assertEqWith s "an artifact creature alice controls" (Projection.cardTypesOf copy after, Projection.controllerOf copy after) (Set.fromList [CardType.Artifact, CardType.Creature], Just S.alice)
        Spec.assertEqWith s "with the front face's 2/2 body" (S.powerToughnessOf copy after) (Just (2, 2))
        Spec.assertEqWith s "bob's permanent reads 1" (Filter.manaValue (Projection.viewOfObject source after)) (Just 1)
        Spec.assertEqWith s "and so does the copy of it" (Filter.manaValue (Projection.viewOfObject copy after)) (Just 1)
        Spec.assertEqWith s "so alice drew one" (S.handSize S.alice after) 1

-- The two names Thraben Gargoyle // Stonewing Antagonizer prints, for the CR
-- 202.3b pair above.
gargoyleName, antagonizerName :: CardName.CardName
gargoyleName = CardName.MkCardName (Text.pack "Thraben Gargoyle")
antagonizerName = CardName.MkCardName (Text.pack "Stonewing Antagonizer")

-- The board the two CR 202.3b cases share, differing only in `turnOver`: bob's
-- Thraben Gargoyle, turned over or not; alice's Clone copying it; then alice
-- casting One with the Machine and its draw resolving. Answers with bob's
-- permanent, the creatures alice controls, and the state after the draw.
--
-- The printings come in the order a case fetches them: Island, Thraben
-- Gargoyle, Clone, Goblin Piker, One with the Machine.
cloneOfGargoyle ::
  Bool ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
cloneOfGargoyle turnOver island gargoyle clone piker oneWithTheMachine =
  let base = S.landsInPlay island 4
      (source, withGargoyle) = S.addPermanent gargoyle S.bob base
      turned = if turnOver then transformEveryCreature withGargoyle else withGargoyle
      (_, staged) = S.spellOnStack clone S.alice turned
      -- CR 614.12a: the copy choice happens inside the Clone's own entry, and
      -- bob's Gargoyle is the only creature on the battlefield to offer.
      entered = snd (Engine.runGamePure copyTheOnlyTarget staged (Stack.resolveTop >> Engine.settleForPriority))
      -- CR 400.7 minted a new id when the Clone left the stack, so the copy is
      -- found by what it IS: the only creature alice controls, her other four
      -- permanents being Islands.
      copies = filter (\oid -> Projection.isCreatureOf oid entered) (Game.zoneMembers Zone.Battlefield S.alice entered)
      -- CR 104.3c: ten cards is far more than the one this draws at most, so
      -- alice cannot deck herself before the assertion.
      withLib = stockLibrary piker S.alice 10 entered
      (gs, spellId) = S.handOne oneWithTheMachine withLib
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
      after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (source, copies, after)

-- "Transform each creature" applied straight, which is Moonmist's shape (CR
-- 701.27a) with a wider filter. Stonewing Antagonizer prints no way back and the
-- Gargoyle here is BOB's, so paying its own {6} would need a board of bob's
-- lands that says nothing about CR 202.3b.
transformEveryCreature :: GameState.GameState -> GameState.GameState
transformEveryCreature gs =
  S.runPure
    S.identityAnswer
    gs
    (Resolve.applyEffect S.noSource S.noSource S.alice Map.empty Map.empty (Effect.Transform (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature))))

-- Answers the CR 614.12a copy choice with the first legal target and delegates
-- everything else, for a fixture where exactly one creature is legal.
copyTheOnlyTarget :: Prompt.Prompt r -> r
copyTheOnlyTarget p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> Maybe.listToMaybe legal
  _ -> S.identityAnswer p

-- Aims every target slot at one creature, for a fixture with several legal ones.
targetingCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
targetingCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- Soul's Majesty, the card that proves Quantity.AgainstSlot (#1171): "Draw cards
-- equal to the power of target creature you control." The power read is the
-- TARGET's, where every other object-reading quantity is aimed at the effect's
-- SOURCE (CR 113.7) -- here a sorcery, which has no power at all, so the source
-- reading answers Nothing and draws nothing.
--
-- Alice's Thragtusk is 5/3 and her Giant Spider 2/4; bob's Panglacial Wurm is
-- 9/5. Targeting each of hers in turn separates the slot's power (5, then 2)
-- from that creature's toughness (3, then 4), from the other creature's power
-- (2, then 5), from the count of her creatures (2), from the greatest power in
-- the game (9), and from the source's nothing (0).
soulsMajestySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulsMajestySpec s registry = Spec.describe s "SoulsMajesty" $ do
  Spec.it s "CR 113.7 draws the power of the TARGET rather than of the sorcery" $ do
    forest <- S.printingOf s registry "Forest"
    thragtusk <- S.printingOf s registry "Thragtusk"
    giantSpider <- S.printingOf s registry "Giant Spider"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    piker <- S.printingOf s registry "Goblin Piker"
    soulsMajesty <- S.printingOf s registry "Soul's Majesty"
    let base = S.landsInPlay forest 5
        (tusk, withTusk) = S.addPermanent thragtusk S.alice base
        (_, withSpider) = S.addPermanent giantSpider S.alice withTusk
        (_, withWurm) = S.addPermanent panglacialWurm S.bob withSpider
        -- CR 104.3c: twelve is far more than any reading here draws.
        withLib = stockLibrary piker S.alice 12 withWurm
        (gs, spellId) = S.handOne soulsMajesty withLib
        cast = snd (Engine.runGamePure (targetingCreature tusk) gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure (targetingCreature tusk) cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew five" (S.handSize S.alice after) 5
  Spec.it s "CR 601.2c the SAME board draws two when the other creature is the target" $ do
    forest <- S.printingOf s registry "Forest"
    thragtusk <- S.printingOf s registry "Thragtusk"
    giantSpider <- S.printingOf s registry "Giant Spider"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    piker <- S.printingOf s registry "Goblin Piker"
    soulsMajesty <- S.printingOf s registry "Soul's Majesty"
    let base = S.landsInPlay forest 5
        (_, withTusk) = S.addPermanent thragtusk S.alice base
        (spider, withSpider) = S.addPermanent giantSpider S.alice withTusk
        (_, withWurm) = S.addPermanent panglacialWurm S.bob withSpider
        withLib = stockLibrary piker S.alice 12 withWurm
        (gs, spellId) = S.handOne soulsMajesty withLib
        cast = snd (Engine.runGamePure (targetingCreature spider) gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure (targetingCreature spider) cast Stack.resolveTop)
    Spec.assertEqWith s "alice drew two" (S.handSize S.alice after) 2

-- Randomness under CR 400.7's zone change: Elkin Lair {3}{R} World
-- Enchantment (Homelands; Oracle text checked against api.scryfall.com,
-- 2026-09-18) -- "At the beginning of each player's upkeep, that player exiles a
-- card at random from their hand. The player may play that card this turn. At
-- the beginning of the next end step, if the player hasn't played the card, they
-- put it into their graveyard."
--
-- The pool's only card moving a card at random OUT of a hand (Scryfall
-- o:/(exiles?|puts?|returns?|shuffles?) .{0,20}cards? at random from (your|their|his or her|that player.s|target player.s|each player.s) hand/,
-- 2026-09-11, one hit), so Effect.MoveToZone's gather asking randomCardsInHand
-- is what this card is here to exercise.
--
-- TWO SEATS, and ALICE controls the enchantment while BOB takes the upkeep:
-- "that player" and "the resolving controller" are the same seat on a one-seat
-- board, and alice holds a card of her own so a gather reading the controller's
-- hand exiles the wrong one rather than nothing.
--
-- THREE distinct printings in bob's hand, and the PAIR of legs is the proof: "at
-- random" is not a property of one outcome, since the engine does not roll, so
-- the two legs differ in the index the interpreter answered with and in nothing
-- else. Exiling the head of the hand unasked passes the second and fails the
-- first.
elkinLairSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
elkinLairSpec s registry =
  let -- S.addHandCard puts its card at the FRONT of the hand, so the list is
      -- stocked in reverse and index 0 is the first card named.
      board lair mine theirs =
        let (_, withLair) = S.addPermanent lair S.alice (Setup.emptyGame S.bothPlayers)
            withAlices = List.foldl' (\g q -> snd (S.addHandCard q S.alice g)) withLair (reverse mine)
         in List.foldl' (\g q -> snd (S.addHandCard q S.bob g)) withAlices (reverse theirs)
      -- BOB's upkeep, stamped and recorded -- the half TurnScope.EachTurn buys,
      -- since under ControllersTurn the trigger would not fire here at all.
      runBobsUpkeep :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      runBobsUpkeep answer gs = S.runPure answer (bobsStepBegins (Phase.Beginning BeginningStep.Upkeep) answer gs) Engine.priorityLoop
      -- A step of bob's begun and its triggers put on the stack, none resolved.
      bobsStepBegins :: Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      bobsStepBegins step answer gs =
        let began =
              Event.recordEvent
                (GameEvent.StepBegan (StepBegan.MkStepBegan step S.bob))
                (gs {GameState.phase = step, GameState.activePlayer = S.bob})
         in S.runPure answer began Engine.settleForPriority
      endStep = Phase.Ending EndingStep.EndStep
      -- Bob's main phase after that upkeep, where S.castAnswer plays whatever
      -- he may: the exiled card is the only card he could play.
      bobsMainPlaying :: GameState.GameState -> GameState.GameState
      bobsMainPlaying gs = S.runPure S.castAnswer gs {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.bob} Engine.priorityLoop
      -- Pinned by INDEX into the offer rather than read off the prompt's fields:
      -- an answerer that hunted for "a legal card" would go on answering legally
      -- after a mutation broke which card the engine honours.
      rolling :: Int -> Prompt.Prompt r -> r
      rolling i p = case p of
        Prompt.RandomObject offered -> case List.drop (min i (length (NonEmpty.toList offered) - 1)) (NonEmpty.toList offered) of
          h : _ -> h
          [] -> NonEmpty.head offered
        _ -> S.identityAnswer p
      named n = Just (CardName.MkCardName (Text.pack n))
      bobsCards = ["Goblin Piker", "Bog Wraith", "Bird Maiden"]
   in Spec.describe s "ElkinLair" $ do
        -- The proving case.
        Spec.it s "CR 400.7 the card exiled is the one randomness named, not the first in hand" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          bolt <- S.printingOf s registry "Lightning Bolt"
          ps <- traverse (S.printingOf s registry) bobsCards
          let after = runBobsUpkeep (rolling 2) (board lair [bolt] ps)
          Spec.assertEqWith s "the LAST card of bob's hand is in exile" (namesIn Zone.Exile S.bob after) [named "Bird Maiden"]
          Spec.assertEqWith s "and the two randomness passed over stay in hand" (namesIn Zone.Hand S.bob after) [named "Goblin Piker", named "Bog Wraith"]
          Spec.assertEqWith s "CR 603.2's \"that player\" is bob: alice's own card is untouched" (namesIn Zone.Hand S.alice after) [named "Lightning Bolt"]
          Spec.assertEqWith s "and the trigger resolved" (length (GameState.stack after)) 0
        -- The other half of the pair: the same board, the same everything, one
        -- different answer.
        Spec.it s "CR 400.7 the same board with a different roll exiles a different card" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          bolt <- S.printingOf s registry "Lightning Bolt"
          ps <- traverse (S.printingOf s registry) bobsCards
          let after = runBobsUpkeep (rolling 0) (board lair [bolt] ps)
          Spec.assertEqWith s "the FIRST card this time" (namesIn Zone.Exile S.bob after) [named "Goblin Piker"]
          Spec.assertEqWith s "and the other two stay in hand" (namesIn Zone.Hand S.bob after) [named "Bog Wraith", named "Bird Maiden"]
        -- The SECOND clause, and WHOSE it is: "THE PLAYER may play that card
        -- this turn" is the upkeep player the trigger bound, not CR 109.5's
        -- "you". An instant is what the roll names, so CR 601.3's window is open
        -- during the upkeep for both seats alike.
        --
        -- ONE MOUNTAIN EACH, which is what makes the negative about the
        -- PERMISSION rather than about mana: alice has exactly the {R} the
        -- Lightning Bolt asks for and still may not cast it, where before this
        -- clause was transcribed the permission was hers and not bob's.
        Spec.it s "CR 601.3 the upkeep player may cast the card the trigger exiled, and the Lair's controller may not" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          ps <- traverse (S.printingOf s registry) ["Lightning Bolt", "Bog Wraith", "Bird Maiden"]
          let withMana g = snd (S.addPermanent mountain S.bob (snd (S.addPermanent mountain S.alice g)))
              after = runBobsUpkeep (rolling 0) (withMana (board lair [piker] ps))
          case Game.zoneMembers Zone.Exile S.bob after of
            [exiledId] -> do
              Spec.assertBool s (S.castable S.bob exiledId after) "bob, whom the trigger bound, may cast the Lightning Bolt it exiled from his hand"
              Spec.assertBool s (not (S.castable S.alice exiledId after)) "alice controls the Lair and holds the same untapped Mountain, and may not"
              -- Proxies, AFTER both behavioural assertions so neither can absorb
              -- a mutation: the card in exile is the one the roll named, and
              -- alice's Mountain is still untapped and so still able to pay.
              Spec.assertEqWith s "the Lightning Bolt is the card in exile" (namesIn Zone.Exile S.bob after) [named "Lightning Bolt"]
              Spec.assertEqWith s "and alice has tapped nothing" (S.tappedCount S.alice after) 0
            _ -> Spec.assertFailure s "bob's upkeep should exile exactly one card"
        -- The THIRD clause: "at the beginning of the next end step, if the player
        -- hasn't played the card, they put it into their graveyard". Three boards
        -- differing only in what bob did with the card between: nothing, cast it
        -- (CR 601.2a), or played it as a land (CR 305.1). Two Mountains each, so
        -- the unplayed Goblin Piker was affordable and stayed unplayed.
        Spec.it s "CR 603.4 an unplayed card goes to the graveyard at the next end step" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let withMana g = snd (S.addPermanent mountain S.bob (snd (S.addPermanent mountain S.bob g)))
              upkept = runBobsUpkeep (rolling 0) (withMana (board lair [] [piker]))
              after = S.runPure S.identityAnswer (bobsStepBegins endStep S.identityAnswer upkept) Engine.priorityLoop
          Spec.assertEqWith s "the Goblin Piker bob never played is in his graveyard" (namesIn Zone.Graveyard S.bob after) [named "Goblin Piker"]
          Spec.assertEqWith s "and no longer in exile" (namesIn Zone.Exile S.bob upkept, namesIn Zone.Exile S.bob after) ([named "Goblin Piker"], [])
        -- CR 603.4: the condition is false as the end step begins, so the ability
        -- never triggers -- not merely finds nothing (CR 603.7c) on resolution.
        Spec.it s "CR 603.4 a card cast from exile was played, so the end step trigger does not trigger" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let withMana g = snd (S.addPermanent mountain S.bob (snd (S.addPermanent mountain S.bob g)))
              played = bobsMainPlaying (runBobsUpkeep (rolling 0) (withMana (board lair [] [piker])))
              ending = bobsStepBegins endStep S.identityAnswer played
          Spec.assertEqWith s "nothing triggered at bob's end step" (length (GameState.stack ending)) 0
          Spec.assertEqWith s "bob cast the Goblin Piker" (namesIn Zone.Battlefield S.bob played) [named "Mountain", named "Mountain", named "Goblin Piker"]
        Spec.it s "CR 603.4 a land card played from exile was played, so the end step trigger does not trigger" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          mountain <- S.printingOf s registry "Mountain"
          let withMana g = snd (S.addPermanent mountain S.bob (snd (S.addPermanent mountain S.bob g)))
              played = bobsMainPlaying (runBobsUpkeep (rolling 0) (withMana (board lair [] [mountain])))
              ending = bobsStepBegins endStep S.identityAnswer played
          Spec.assertEqWith s "nothing triggered at bob's end step" (length (GameState.stack ending)) 0
          Spec.assertEqWith s "bob played the Mountain" (namesIn Zone.Battlefield S.bob played) [named "Mountain", named "Mountain", named "Mountain"]

-- Randomness over CR 400.2's PUBLIC zone, the pair of cards that exercise
-- Pawl.Types.ObjectRef.RandomCardInGraveyard. Ghoulraiser {1}{B}{B} Creature --
-- Zombie 2/2 (Jumpstart) -- "When this creature enters, return a Zombie card at
-- random from your graveyard to your hand." Make a Wish {3}{G} Sorcery
-- (Innistrad) -- "Return two cards at random from your graveyard to your hand."
-- Both checked against api.scryfall.com, 2026-09-19. Neither card elides
-- anything.
--
-- The two halves the ref carries, one card each: Ghoulraiser is the FILTER
-- ("a Zombie card") and Make a Wish is the COUNT ("two cards"), which is why
-- both are here rather than one.
--
-- TWO SEATS, and BOB's graveyard is stocked with Zombies of his own: the scope
-- is "your graveyard", so a scope reading every player's would pick a second
-- card out of his pile, which the assertions on his graveyard and on alice's
-- hand size read directly.
ghoulraiserSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ghoulraiserSpec s registry =
  let -- alice controls four Swamps -- slack over the creature's {1}{B}{B}, so no
      -- payment order can fail for reasons of its own -- and holds the Zombie.
      -- `buried` goes into the named graveyards in the order given, which is the
      -- order the candidates are offered in.
      board ghoulraiser swamp buried =
        let mana = S.landsInPlay swamp 4
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) mana buried
            (withCard, handId) = S.handOne ghoulraiser withGraves
         in (handId, withCard {GameState.priority = Just S.alice})
      -- Cast, let the creature enter, let CR 603.3b put the enters trigger on the
      -- stack, then resolve it.
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      run answer handId gs = S.runPure answer gs (S.cast S.alice handId *> Stack.resolveTop *> Engine.settleForPriority *> Stack.resolveTop)
      -- Pinned by INDEX into the offer rather than read off the prompt's fields:
      -- an answerer that hunted for "a Zombie" would go on answering legally
      -- after a mutation broke which card the engine honours.
      rolling :: Int -> Prompt.Prompt r -> r
      rolling i p = case p of
        Prompt.RandomObject offered -> case List.drop (min i (length (NonEmpty.toList offered) - 1)) (NonEmpty.toList offered) of
          h : _ -> h
          [] -> NonEmpty.head offered
        _ -> S.identityAnswer p
      named n = Just (CardName.MkCardName (Text.pack n))
      -- bob's three first, so a scope reading every graveyard reaches a pile whose
      -- cards no assertion below can confuse with alice's; then alice's, with the
      -- two non-Zombies INTERLEAVED, so the third candidate of the filtered offer
      -- and the third of an unfiltered one are different cards.
      stock =
        [ ("Putrid Goblin", S.bob),
          ("Headless Skaab", S.bob),
          ("Khenra Eternal", S.bob),
          ("Whipstitched Zombie", S.alice),
          ("Lightning Bolt", S.alice),
          ("Highborn Ghoul", S.alice),
          ("Murder", S.alice),
          ("Dregscape Zombie", S.alice)
        ]
      boardOf ghoulraiser swamp printings = board ghoulraiser swamp (zip printings (fmap snd stock))
   in Spec.describe s "Ghoulraiser" $ do
        -- The proving case.
        Spec.it s "CR 404.1 the card returned is the Zombie randomness named, not the first card buried" $ do
          ghoulraiser <- S.printingOf s registry "Ghoulraiser"
          swamp <- S.printingOf s registry "Swamp"
          printings <- traverse (S.printingOf s registry . fst) stock
          let (handId, gs) = boardOf ghoulraiser swamp printings
              after = run (rolling 2) handId gs
          Spec.assertEqWith s "the THIRD Zombie of alice's graveyard is in her hand" (namesIn Zone.Hand S.alice after) [named "Dregscape Zombie"]
          Spec.assertEqWith
            s
            "the two Zombies randomness passed over stay buried, and so do both non-Zombies"
            (namesIn Zone.Graveyard S.alice after)
            [named "Whipstitched Zombie", named "Lightning Bolt", named "Highborn Ghoul", named "Murder"]
          Spec.assertEqWith
            s
            "CR 400.1 \"your graveyard\" is alice's: bob's Zombies are untouched"
            (namesIn Zone.Graveyard S.bob after)
            [named "Putrid Goblin", named "Headless Skaab", named "Khenra Eternal"]
          Spec.assertEqWith s "and the trigger resolved" (length (GameState.stack after)) 0
        -- The other half of the pair: the same board, the same everything, one
        -- different answer. "At random" is not a property of one outcome, since
        -- the engine does not roll, so the two legs differ in the index the
        -- interpreter answered with and in nothing else.
        Spec.it s "CR 404.1 the same board with a different roll returns a different card" $ do
          ghoulraiser <- S.printingOf s registry "Ghoulraiser"
          swamp <- S.printingOf s registry "Swamp"
          printings <- traverse (S.printingOf s registry . fst) stock
          let (handId, gs) = boardOf ghoulraiser swamp printings
              after = run (rolling 0) handId gs
          Spec.assertEqWith s "the FIRST Zombie this time" (namesIn Zone.Hand S.alice after) [named "Whipstitched Zombie"]
          Spec.assertEqWith
            s
            "and the other four cards stay buried"
            (namesIn Zone.Graveyard S.alice after)
            [named "Lightning Bolt", named "Highborn Ghoul", named "Murder", named "Dregscape Zombie"]
        -- The COUNT, and it counts DISTINCT cards: the answerer names the FIRST
        -- candidate of every offer, so a second ask made over the whole pile
        -- again would name the card already taken and only one card would move.
        Spec.it s "CR 608.2c Make a Wish returns two DISTINCT cards, the same answer given twice" $ do
          wish <- S.printingOf s registry "Make a Wish"
          forest <- S.printingOf s registry "Forest"
          printings <- traverse (S.printingOf s registry) ["Whipstitched Zombie", "Lightning Bolt", "Highborn Ghoul", "Murder"]
          let mana = S.landsInPlay forest 4
              withGraves = List.foldl' (\g p -> snd (S.addGraveyardCard p S.alice g)) mana printings
              (ready, spell) = S.handOne wish withGraves
              after = S.runPure (rolling 0) (ready {GameState.priority = Just S.alice}) (S.cast S.alice spell *> Stack.resolveTop)
          Spec.assertEqWith
            s
            "the first candidate of each offer, and the second offer no longer holds the first"
            (namesIn Zone.Hand S.alice after)
            [named "Whipstitched Zombie", named "Lightning Bolt"]
          Spec.assertEqWith
            s
            "the other two stay buried, with the sorcery on top of them (CR 608.2n)"
            (namesIn Zone.Graveyard S.alice after)
            [named "Highborn Ghoul", named "Murder", named "Make a Wish"]

-- CR 701.9b's two exceptions to the discarding player's own choice, both over
-- Discard.These. bob holds four distinct cards, so every assertion reads
-- identity, and the expectations are read off his hand's own order rather than
-- assumed. Each answerer pins its pick to the LAST card offered -- the one
-- answer a fallback to the head of the offer, or CR 701.9b's default choice
-- answered with the first cards, would not produce.
--
-- Hymn to Tourach {B}{B} Sorcery -- "Target player discards two cards at
-- random." Duress {B} Sorcery -- "Target opponent reveals their hand. You choose
-- a noncreature, nonland card from it. That player discards that card." (both
-- checked against api.scryfall.com, 2026-09-11).
discardExceptionsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
discardExceptionsSpec s registry = Spec.describe s "CR 701.9b discard exceptions" $ do
  Spec.it s "CR 701.9b Hymn to Tourach discards the two cards randomness named" $ do
    hymn <- S.printingOf s registry "Hymn to Tourach"
    swamp <- S.printingOf s registry "Swamp"
    cards <- traverse (S.printingOf s registry) ["Goblin Piker", "Forest", "Lightning Bolt", "Murder"]
    let (withSpell, spell) = S.handOne hymn (S.landsInPlay swamp 2)
        ready = List.foldl' (\g p -> snd (S.addHandCard p S.bob g)) withSpell cards
        hand = namesIn Zone.Hand S.bob ready
        -- Randomness answers the last card of each offer; bob, were he asked
        -- CR 701.9b's default question, would answer the first two.
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.RandomObject offered -> NonEmpty.last offered
          Prompt.ChooseDiscard _ _ ids n -> take (Natural.toIntSaturating n) ids
          _ -> atBobAnswer p
        after = S.runPure answer (S.runPure answer ready (S.cast S.alice spell)) Stack.resolveTop
    Spec.assertEqWith
      s
      "the last card of each random offer is discarded, not the first two"
      (List.sort (namesIn Zone.Graveyard S.bob after))
      (List.sort (drop 2 hand))
    Spec.assertEqWith
      s
      "and the two randomness passed over stay in bob's hand"
      (List.sort (namesIn Zone.Hand S.bob after))
      (List.sort (take 2 hand))
  Spec.it s "CR 701.9b Duress discards the card its caster chose from the revealed hand" $ do
    duress <- S.printingOf s registry "Duress"
    swamp <- S.printingOf s registry "Swamp"
    cards <- traverse (S.printingOf s registry) ["Goblin Piker", "Lightning Bolt", "Forest", "Murder"]
    let (withSpell, spell) = S.handOne duress (S.landsInPlay swamp 1)
        ready = List.foldl' (\g p -> snd (S.addHandCard p S.bob g)) withSpell cards
        hand = Game.zoneMembers Zone.Hand S.bob ready
        nameOf oid = fmap S.nameOf (Game.cardOf oid ready)
        spells = [oid | oid <- hand, nameOf oid `elem` fmap (Just . CardName.MkCardName . Text.pack) ["Lightning Bolt", "Murder"]]
        -- Records who was asked over which cards, and answers the last one.
        answer :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, [ObjectId.ObjectId])] r
        answer p = case p of
          Prompt.ChooseCardFromAmong _ asked _ offered -> do
            State.modify' (<> [(asked, NonEmpty.toList offered)])
            pure (NonEmpty.last offered)
          _ -> pure (atBobAnswer p)
        (after, asks) = State.runState (fmap snd (Engine.runGame answer ready (S.cast S.alice spell *> Stack.resolveTop))) []
    Spec.assertEqWith s "the card alice chose is the one bob discarded" (namesIn Zone.Graveyard S.bob after) (fmap nameOf (take 1 (reverse spells)))
    Spec.assertEqWith s "CR 608.2d alice was asked, over the noncreature, nonland cards alone" asks [(S.alice, spells)]
    Spec.assertEqWith
      s
      "CR 701.20a bob revealed his whole hand"
      (List.sort (S.revealsOf after))
      (List.sort (fmap (\p -> (S.bob, Set.singleton (S.printingName p))) cards))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  zoneChangeSpec s registry
  discardExceptionsSpec s registry
  elkinLairSpec s registry
  ghoulraiserSpec s registry
  libraryPositionSpec s registry
  aetherspoutsSpec s registry
  drawCardSpec s registry
  loseLifeSpec s registry
  perRecipientAmountSpec s registry
  exchangeLifeTotalsSpec s registry
  exchangeValuesSpec s registry
  setLifeTotalSpec s registry
  doubleLifeTotalSpec s registry
  redistributeLifeTotalsSpec s registry
  greatestSpec s registry
  soulsMajestySpec s registry
