{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Target's per-player slots: CR 601.2c's "for each opponent,
-- ... up to one target ... that player controls", announced once per opponent
-- (Target.announcedSlots) and re-judged per copy at CR 608.2b
-- (Target.boundCopies). Riptide Gearhulk and Enigma Thief take the triggered
-- road, Dismantling Wave and Blatant Thievery the spell's, The Theorist, Jace
-- Beleren the activation's. Every board has three seats, so "each
-- opponent" and "that player" are never the same seat by accident.
module Pawl.TargetPerPlayerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural.Type
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Target" . Spec.describe s "PerPlayer" $ do
  let -- alice holds `creature`; bob controls a Goblin Piker and a Forest, carol
      -- a Glorious Anthem, alice an Ornithopter. bob's and carol's libraries
      -- hold three cards each (the LAST named is the top).
      board creature = do
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        anthem <- S.printingOf s registry "Glorious Anthem"
        ornithopter <- S.printingOf s registry "Ornithopter"
        card <- S.printingOf s registry creature
        seeded <- traverse (S.printingOf s registry) ["Lightning Bolt", "Unsummon", "Griptide"]
        let (pikerId, g1) = S.addPermanent piker S.bob S.threePlayerGame
            (_, g2) = S.addPermanent forest S.bob g1
            (anthemId, g3) = S.addPermanent anthem S.carol g2
            (_, g4) = S.addPermanent ornithopter S.alice g3
            stock pid g = List.foldl' (\g' p -> snd (S.addLibraryCard p pid g')) g seeded
            (gs, cardId) = S.handOne card (stock S.carol (stock S.bob g4))
        pure (gs, cardId, pikerId, anthemId)
      -- The trigger's announcement, recorded: every AnnounceTargets offer as
      -- the object ids each slot offers.
      enter :: Set.Set ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [[Set.Set ObjectId.ObjectId]])
      enter picks cardId gs =
        State.runState (fmap snd (Engine.runGame (aiming picks) gs (Event.changeZone cardId Zone.Battlefield >> Engine.placePendingTriggers))) []
      resolve :: GameState.GameState -> GameState.GameState
      resolve gs = State.evalState (fmap snd (Engine.runGame (aiming Set.empty) gs Stack.resolveTop)) []
      named = Just . CardName.MkCardName . Text.pack
      onBattlefield name = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack name))
  -- CR 601.2c: one slot per opponent, each offering only what THAT opponent
  -- controls -- not bob's Forest (nonland), not alice's own Ornithopter, and
  -- not the other opponent's permanent.
  Spec.it s "CR 601.2c Riptide Gearhulk asks each opponent's slot for that player's nonland permanents only" $ do
    (gs, cardId, pikerId, anthemId) <- board "Riptide Gearhulk"
    let (placed, offers) = enter (Set.fromList [pikerId, anthemId]) cardId gs
        after = resolve placed
    -- THE GAMEPLAY ASSERTIONS, ahead of the offer proxy.
    Spec.assertEqWith s "bob's Piker went third from the top of bob's library" (namesIn Zone.Library S.bob after) (fmap named ["Griptide", "Unsummon", "Goblin Piker", "Lightning Bolt"])
    Spec.assertEqWith s "carol's Anthem went third from the top of carol's library" (namesIn Zone.Library S.carol after) (fmap named ["Griptide", "Unsummon", "Glorious Anthem", "Lightning Bolt"])
    Spec.assertEqWith s "alice's own Ornithopter stayed" (onBattlefield "Ornithopter" S.alice after) 1
    Spec.assertEqWith s "one announcement, one slot per opponent, each its own player's" offers [[Set.singleton pikerId, Set.singleton anthemId]]
  -- CR 115.6: "up to one" per opponent, so one copy may be left empty while the
  -- other is filled.
  Spec.it s "CR 115.6 Riptide Gearhulk may leave carol's slot empty and still take bob's target" $ do
    (gs, cardId, pikerId, _) <- board "Riptide Gearhulk"
    let after = resolve (fst (enter (Set.singleton pikerId) cardId gs))
    Spec.assertEqWith s "bob's Piker went into his library" (onBattlefield "Goblin Piker" S.bob after) 0
    Spec.assertEqWith s "carol's Anthem stayed on the battlefield" (onBattlefield "Glorious Anthem" S.carol after) 1
  -- CR 608.2b per copy: the Piker was chosen for BOB's slot. Once carol
  -- controls it, it is still an opponent's nonland permanent -- so only the
  -- copy's own "that player" can make it illegal.
  Spec.it s "CR 608.2b the Piker changing hands to carol is illegal for bob's slot, and carol's target still resolves" $ do
    (gs, cardId, pikerId, anthemId) <- board "Riptide Gearhulk"
    let placed = fst (enter (Set.fromList [pikerId, anthemId]) cardId gs)
        after = resolve (S.giveControl pikerId S.carol placed)
    Spec.assertEqWith s "the Piker (bob owns it) stayed on the battlefield" (onBattlefield "Goblin Piker" S.bob after) 1
    Spec.assertEqWith s "carol's Anthem went third from the top of her library" (namesIn Zone.Library S.carol after) (fmap named ["Griptide", "Unsummon", "Glorious Anthem", "Lightning Bolt"])
  Spec.it s "CR 601.2c Enigma Thief returns each opponent's chosen permanent to its owner's hand" $ do
    (gs, cardId, pikerId, anthemId) <- board "Enigma Thief"
    let after = resolve (fst (enter (Set.fromList [pikerId, anthemId]) cardId gs))
    Spec.assertBool s (elem (named "Goblin Piker") (namesIn Zone.Hand S.bob after)) "the Piker is in bob's hand"
    Spec.assertBool s (elem (named "Glorious Anthem") (namesIn Zone.Hand S.carol after)) "the Anthem is in carol's hand"
    Spec.assertEqWith s "alice's Ornithopter stayed" (onBattlefield "Ornithopter" S.alice after) 1
  -- The spell's road: CR 601.2c's announcement as the spell is cast, and CR
  -- 608.2b's re-check as it resolves.
  Spec.it s "CR 601.2c Dismantling Wave destroys up to one artifact or enchantment of each opponent's" $ do
    plains <- S.printingOf s registry "Plains"
    ornithopter <- S.printingOf s registry "Ornithopter"
    anthem <- S.printingOf s registry "Glorious Anthem"
    wave <- S.printingOf s registry "Dismantling Wave"
    let (bobsId, g1) = S.addPermanent ornithopter S.bob (S.landsFor plains S.alice 3 S.threePlayerGame)
        (carolsId, g2) = S.addPermanent anthem S.carol g1
        (_, g3) = S.addPermanent ornithopter S.alice g2
        (gs, waveId) = S.handOne wave g3
        (after, offers) = State.runState (fmap snd (Engine.runGame (aiming (Set.fromList [bobsId, carolsId])) gs (S.cast S.alice waveId >> Stack.resolveTop))) []
    Spec.assertEqWith s "bob's Ornithopter was destroyed" (onBattlefield "Ornithopter" S.bob after) 0
    Spec.assertEqWith s "carol's Anthem was destroyed" (onBattlefield "Glorious Anthem" S.carol after) 0
    Spec.assertEqWith s "alice's own Ornithopter stayed" (onBattlefield "Ornithopter" S.alice after) 1
    Spec.assertEqWith s "one slot per opponent, each its own player's" offers [[Set.singleton bobsId, Set.singleton carolsId]]

  -- Exactly one target per opponent. alice holds seven Islands; bob controls a
  -- Goblin Piker and a Forest, carol an Ornithopter unless `carolEmpty`.
  let thievery carolEmpty = do
        island <- S.printingOf s registry "Island"
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        ornithopter <- S.printingOf s registry "Ornithopter"
        card <- S.printingOf s registry "Blatant Thievery"
        let (pikerId, g1) = S.addPermanent piker S.bob (S.landsFor island S.alice 7 S.threePlayerGame)
            (_, g2) = S.addPermanent forest S.bob g1
            (thopterId, g3) = if carolEmpty then (pikerId, g2) else S.addPermanent ornithopter S.carol g2
            (gs, spellId) = S.handOne card g3
        pure (gs, spellId, pikerId, thopterId)
      steal picks gs spellId = State.evalState (fmap snd (Engine.runGame (aiming picks) gs (S.cast S.alice spellId))) []
  Spec.it s "CR 601.2c Blatant Thievery gains control of one permanent from each opponent" $ do
    (gs, spellId, pikerId, thopterId) <- thievery False
    let after = resolve (steal (Set.fromList [pikerId, thopterId]) gs spellId)
    Spec.assertEqWith s "alice controls bob's Piker" (Projection.controllerOf pikerId after) (Just S.alice)
    Spec.assertEqWith s "alice controls carol's Ornithopter" (Projection.controllerOf thopterId after) (Just S.alice)
  -- Sylvan Primordial's ruling for the same template: a player with nothing to
  -- target gets no target, so carol controlling nothing does not stop the cast.
  -- The pair's other half: with NO opponent controlling anything the slot
  -- cannot be filled at all.
  Spec.it s "CR 601.2c Blatant Thievery with carol controlling nothing still takes bob's Piker" $ do
    (gs, spellId, pikerId, _) <- thievery True
    Spec.assertBool s (S.castable S.alice spellId gs) "castable with carol controlling nothing"
    Spec.assertEqWith s "alice controls bob's Piker" (Projection.controllerOf pikerId (resolve (steal (Set.singleton pikerId) gs spellId))) (Just S.alice)
    island <- S.printingOf s registry "Island"
    card <- S.printingOf s registry "Blatant Thievery"
    let (bare, bareSpellId) = S.handOne card (S.landsFor island S.alice 7 S.threePlayerGame)
    Spec.assertBool s (not (S.castable S.alice bareSpellId bare)) "and not castable with no opponent controlling anything"
  -- Blatant Thievery's own ruling: "If a permanent changes controller after
  -- being targeted but before this spell resolves, you won't gain control of
  -- that permanent."
  Spec.it s "CR 608.2b Blatant Thievery does not take the Piker once carol controls it" $ do
    (gs, spellId, pikerId, thopterId) <- thievery False
    let after = resolve (S.giveControl pikerId S.carol (steal (Set.fromList [pikerId, thopterId]) gs spellId))
    Spec.assertEqWith s "carol still controls the Piker" (Projection.controllerOf pikerId after) (Just S.carol)
    Spec.assertEqWith s "alice controls carol's Ornithopter" (Projection.controllerOf thopterId after) (Just S.alice)
  -- The activation's road (CR 602.2b's announcement), on a loyalty ability.
  Spec.it s "CR 602.2b The Theorist, Jace Beleren's -2 returns each opponent's chosen artifact or creature" $ do
    jace <- S.printingOf s registry "The Theorist, Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    ornithopter <- S.printingOf s registry "Ornithopter"
    anthem <- S.printingOf s registry "Glorious Anthem"
    case Face.activatedAbilities (S.combinedFace jace) of
      [_, minusTwo, _] -> do
        let (jaceId, g1) = S.addPermanent jace S.alice S.threePlayerGame
            (pikerId, g2) = S.addPermanent piker S.bob (S.addCounter CounterKind.Loyalty 3 jaceId g1)
            (thopterId, g3) = S.addPermanent ornithopter S.carol g2
            -- carol's Anthem is neither an artifact nor a creature: not offered.
            (_, g4) = S.addPermanent anthem S.carol g3
            (_, gs) = S.addPermanent piker S.alice g4
            (after, offers) = State.runState (fmap snd (Engine.runGame (aiming (Set.fromList [pikerId, thopterId])) gs (Activate.activateAbility S.alice jaceId minusTwo >> Stack.resolveTop))) []
        Spec.assertBool s (elem (named "Goblin Piker") (namesIn Zone.Hand S.bob after)) "bob's Piker is in his hand"
        Spec.assertBool s (elem (named "Ornithopter") (namesIn Zone.Hand S.carol after)) "carol's Ornithopter is in her hand"
        Spec.assertEqWith s "carol's Anthem stayed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Glorious Anthem")) S.carol after) 1
        Spec.assertEqWith s "alice's own Piker stayed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 1
        Spec.assertEqWith s "one slot per opponent, each its own player's" offers [[Set.singleton pikerId, Set.singleton thopterId]]
      _ -> Spec.assertFailure s "The Theorist has three loyalty abilities"

-- Announce one target for every slot offering one of `picks`, none elsewhere,
-- and take `picks` out of each offer -- FILTERED from the offer, so the
-- recipient is the one the pool tagged. Every AnnounceTargets offer is
-- recorded, in slot order, as object ids.
aiming :: Set.Set ObjectId.ObjectId -> Prompt.Prompt r -> State.State [[Set.Set ObjectId.ObjectId]] r
aiming picks p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> do
    State.modify (<> [fmap (objectsOf . snd) (Map.elems offers)])
    pure (fmap (\(_, legal) -> if Set.null (Set.intersection picks (objectsOf legal)) then 0 else 1 :: Natural.Type.Natural) offers)
  Prompt.ChooseTargets _ _ _ asked -> pure (fmap (Set.filter (maybe False (`Set.member` picks) . Recipient.objectOf) . snd) asked)
  _ -> pure (S.identityAnswer p)

objectsOf :: Set.Set Recipient.Recipient -> Set.Set ObjectId.ObjectId
objectsOf = Set.fromList . Maybe.mapMaybe Recipient.objectOf . Set.toList

-- Every card in a zone, by name, top first for a library.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (fmap S.nameOf . (`Game.cardOf` gs)) (Game.zoneMembers zone pid gs)
