{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Resolve over effects that act on many objects at once, from
-- Exhume to Golgothian Sylex: reanimation, library reveals, sweepers and the
-- tokens they leave. Split out of Pawl.MassEffectSpec, which keeps the
-- machinery.
module Pawl.BoardEffectSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Phasing as Phasing
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve.Slots as Resolve
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import Pawl.MassEffectSpec (namesIn, soleActivatedAbility, statusOf)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AfterTurn as AfterTurn
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhasedOut as PhasedOut
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.Zone as Zone

-- The same arm with a CHOOSER other than the resolving controller:
-- Chooser.EachInScope, where portOfKarfellSpec above is Chooser.TheController.
--
-- Exhume {1}{B} Sorcery -- "Each player puts a creature card from their graveyard
-- onto the battlefield." (name, cost, type line and Oracle text checked against
-- api.scryfall.com). Its whole text is that one sentence, so nothing else on the
-- card can be what these assertions read.
--
-- CR 608.2d has the player an effect instructs announce the choices it offers,
-- and this sentence instructs EACH PLAYER -- so each of them chooses, out of
-- their own graveyard alone, in APNAP order (CR 608.2e, CR 101.4). CR 110.2a then
-- gives each arrival to the player who put it there, which is the graveyard's own
-- player: a graveyard is filed under the card's owner (CR 400.3), so the card
-- writes EntryRiders' underOwner and every returning creature enters under its
-- owner rather than under the caster's control. That is the whole difference from
-- Rise of the Dark Realms' "under your control".
--
-- THREE SEATS, with a board built so that the readings are told apart:
--
--   * EACH PLAYER as chooser versus the CONTROLLER as chooser. The answerer
--     replies by WHICH PLAYER the prompt names -- alice and carol take their
--     second candidate, bob his LAST, and bob's graveyard holds three so that
--     his three readings come apart: an engine that asked alice about every
--     graveyard would take bob's second card, one that chose for the player
--     would take his first, and only the right one takes his third. An engine
--     that asked one player about the union of the graveyards would return one
--     card rather than three besides.
--   * THE CHOSEN card versus the FIRST matching one. The paired leg below runs
--     the same board through Replay.defaultAnswer, which takes every first.
--   * EACH PLAYER'S OWN graveyard versus the union. No creature card ever
--     crosses seats, which the per-owner control assertion is what pins.
--   * A CREATURE CARD versus the whole zone. Each graveyard also holds a
--     noncreature card, and each must stay buried.
--   * ONE EACH versus a sweep. Two match per graveyard and one comes back.
exhumeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exhumeSpec s registry =
  let -- alice controls four Swamps -- twice what the spell costs, so a payment
      -- that taps one source at a time cannot fail for reasons of its own -- and
      -- holds an Exhume; `buried` goes into the named graveyards in the order
      -- given. Returns the spell's id.
      board exhume swamp buried =
        let mana = S.landsFor swamp S.alice 4 S.threePlayerGame
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) mana buried
            (withSpell, spell) = S.handOne exhume withGraves
         in (spell, withSpell {GameState.priority = Just S.alice})
      -- Cast and resolve, keeping the RESPONSES beside the board so the same call
      -- answers both "what came back" and "how many players were asked".
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [Response.Response])
      run answer spell gs =
        let ((_, after), responses) = Replay.record answer gs (S.cast S.alice spell >> Stack.resolveTop)
         in (after, responses)
      named = Just . CardName.MkCardName . Text.pack
      -- The whole battlefield minus alice's four Swamps, by NAME and CONTROLLER:
      -- CR 400.7 mints a fresh id at the destination, so a returned card cannot
      -- be found by the id it was buried under, and CR 110.2a is what decides
      -- whose the arrival is.
      arrivals gs =
        List.sort
          ( fmap
              (\oid -> (fmap S.nameOf (Game.cardOf oid gs), Projection.controllerOf oid gs))
              (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) /= named "Swamp") (Set.toList (GameState.battlefield gs)))
          )
      choices responses =
        length
          (filter (\r -> case r of Response.ChoseCardInGraveyard _ -> True; _ -> False) responses)
      -- The prompt's candidates in the order it offers them, which is the order
      -- Resolve.graveyardCardsOf sorts each graveyard into.
      secondOf offered = case offered of
        _ NonEmpty.:| (second : _) -> second
        only NonEmpty.:| [] -> only
   in Spec.describe s "Exhume" $ do
        -- The headline: three players, three separate choices, three creatures
        -- back under three different controllers.
        Spec.it s "CR 608.2d each player chooses in their own graveyard, and keeps what they choose" $ do
          exhume <- S.printingOf s registry "Exhume"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          murder <- S.printingOf s registry "Murder"
          sentry <- S.printingOf s registry "Ogre Sentry"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          judgment <- S.printingOf s registry "Day of Judgment"
          wraith <- S.printingOf s registry "Bog Wraith"
          hero <- S.printingOf s registry "Benalish Hero"
          berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
          forest <- S.printingOf s registry "Forest"
          let buried =
                [ (piker, S.alice),
                  (murder, S.alice),
                  (maiden, S.alice),
                  (sentry, S.bob),
                  (judgment, S.bob),
                  (cavalry, S.bob),
                  (wraith, S.bob),
                  (hero, S.carol),
                  (forest, S.carol),
                  (berserkers, S.carol)
                ]
              (spell, gs) = board exhume swamp buried
              -- BY THE PLAYER THE PROMPT NAMES, which is the whole assertion:
              -- alice and carol take their second candidate and bob his last, so
              -- no one answer can stand in for another's.
              choosing p = case p of
                Prompt.ChooseCardInGraveyard _ pid _ offered _ ->
                  if pid == S.bob then NonEmpty.last offered else secondOf offered
                _ -> S.identityAnswer p
              (after, responses) = run choosing spell gs
          Spec.assertEqWith s "all three players were asked" (choices responses) 3
          Spec.assertEqWith
            s
            "each player's own choice is on the battlefield under their own control"
            (arrivals after)
            ( List.sort
                [ (named "Bird Maiden", Just S.alice),
                  (named "Bog Wraith", Just S.bob),
                  (named "Berserkers of Blood Ridge", Just S.carol)
                ]
            )
          Spec.assertEqWith
            s
            "the unchosen creature cards and the noncreature stay buried in every graveyard, and the spent sorcery joins alice's (CR 608.2n)"
            ( List.sort (namesIn Zone.Graveyard S.alice after),
              List.sort (namesIn Zone.Graveyard S.bob after),
              List.sort (namesIn Zone.Graveyard S.carol after)
            )
            ( List.sort [named "Goblin Piker", named "Murder", named "Exhume"],
              List.sort [named "Ogre Sentry", named "Benalish Cavalry", named "Day of Judgment"],
              List.sort [named "Benalish Hero", named "Forest"]
            )
        -- CR 101.4b: a pair of casts differing only in alice's pick. bob takes his
        -- LAST candidate when his prompt says alice took her Bird Maiden and his
        -- first otherwise, so what comes back for him is what he was told.
        Spec.it s "CR 101.4b a later player knows the cards the earlier players took" $ do
          exhume <- S.printingOf s registry "Exhume"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          wraith <- S.printingOf s registry "Bog Wraith"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(piker, S.alice), (maiden, S.alice), (sentry, S.bob), (wraith, S.bob), (hero, S.carol)]
              (spell, gs) = board exhume swamp buried
              maidens = filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == named "Bird Maiden") (Game.zoneMembers Zone.Graveyard S.alice gs)
              choosing :: (NonEmpty.NonEmpty ObjectId.ObjectId -> ObjectId.ObjectId) -> Prompt.Prompt r -> r
              choosing hers p = case p of
                Prompt.ChooseCardInGraveyard _ pid _ offered earlier
                  | pid == S.alice -> hers offered
                  | pid == S.bob ->
                      if any (\(who, oid) -> who == S.alice && elem oid maidens) earlier
                        then NonEmpty.last offered
                        else NonEmpty.head offered
                _ -> S.identityAnswer p
              bobs after = filter ((== Just S.bob) . snd) (arrivals after)
              (toldMaiden, _) = run (choosing secondOf) spell gs
              (toldPiker, _) = run (choosing NonEmpty.head) spell gs
          Spec.assertEqWith s "CR 101.4b told alice took her Bird Maiden, bob took his last candidate" (bobs toldMaiden) [(named "Bog Wraith", Just S.bob)]
          Spec.assertEqWith s "CR 101.4b told alice took her Goblin Piker, bob took his first" (bobs toldPiker) [(named "Ogre Sentry", Just S.bob)]
          Spec.assertEqWith s "and alice's own pick came back each time" (fmap (filter ((== Just S.alice) . snd) . arrivals) [toldMaiden, toldPiker]) [[(named "Bird Maiden", Just S.alice)], [(named "Goblin Piker", Just S.alice)]]
        -- The paired control, and the whole reason each graveyard buries TWO
        -- creature cards: the same cast on the same board with the DEFAULT
        -- answerer brings back the other one in every seat. If the engine were
        -- picking, both legs would name the same three cards.
        Spec.it s "CR 608.2d the engine does not pick: another answer returns the other card in every seat" $ do
          exhume <- S.printingOf s registry "Exhume"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          murder <- S.printingOf s registry "Murder"
          sentry <- S.printingOf s registry "Ogre Sentry"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          judgment <- S.printingOf s registry "Day of Judgment"
          wraith <- S.printingOf s registry "Bog Wraith"
          hero <- S.printingOf s registry "Benalish Hero"
          berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
          forest <- S.printingOf s registry "Forest"
          let buried =
                [ (piker, S.alice),
                  (murder, S.alice),
                  (maiden, S.alice),
                  (sentry, S.bob),
                  (judgment, S.bob),
                  (cavalry, S.bob),
                  (wraith, S.bob),
                  (hero, S.carol),
                  (forest, S.carol),
                  (berserkers, S.carol)
                ]
              (spell, gs) = board exhume swamp buried
              (after, _) = run S.identityAnswer spell gs
          Spec.assertEqWith
            s
            "each seat's first candidate comes back instead"
            (arrivals after)
            ( List.sort
                [ (named "Goblin Piker", Just S.alice),
                  (named "Ogre Sentry", Just S.bob),
                  (named "Benalish Hero", Just S.carol)
                ]
            )
        -- CR 101.3 and CR 609.3 applied PER PLAYER: a graveyard with nothing
        -- matching drops that player out of the batch rather than the
        -- instruction out of the effect, and a graveyard with exactly one
        -- matching card leaves them nothing to decide, so they are not asked.
        Spec.it s "CR 101.3 an empty share is skipped and a forced one is not asked" $ do
          exhume <- S.printingOf s registry "Exhume"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          forest <- S.printingOf s registry "Forest"
          let buried = [(piker, S.alice), (maiden, S.alice), (sentry, S.bob), (forest, S.carol)]
              (spell, gs) = board exhume swamp buried
              choosing p = case p of
                Prompt.ChooseCardInGraveyard _ _ _ offered _ -> secondOf offered
                _ -> S.identityAnswer p
              (after, responses) = run choosing spell gs
          Spec.assertEqWith s "only alice, who had two candidates, was asked" (choices responses) 1
          Spec.assertEqWith
            s
            "alice's chosen card and bob's forced one came back; carol had no creature card and nothing happened for her"
            (arrivals after)
            (List.sort [(named "Bird Maiden", Just S.alice), (named "Ogre Sentry", Just S.bob)])
          Spec.assertEqWith s "and carol's noncreature card is still buried" (namesIn Zone.Graveyard S.carol after) [named "Forest"]
        -- Every graveyard empty: the spell resolves, asks nobody and returns
        -- nothing. The spent sorcery in alice's graveyard is what keeps this from
        -- passing because the spell never resolved at all.
        Spec.it s "CR 609.3 empty graveyards are a no-op rather than a failure" $ do
          exhume <- S.printingOf s registry "Exhume"
          swamp <- S.printingOf s registry "Swamp"
          let (spell, gs) = board exhume swamp []
              (after, responses) = run S.identityAnswer spell gs
          Spec.assertEqWith s "nobody was asked" (choices responses) 0
          Spec.assertEqWith s "nothing arrived" (arrivals after) []
          Spec.assertEqWith s "and the spell really did resolve" (namesIn Zone.Graveyard S.alice after) [named "Exhume"]

-- TWO chosen graveyard cards in ONE resolution, where the second must not be the
-- first: Blood for Bones, the card that made #1433 look like a missing exclusion.
--
-- Blood for Bones {3}{B} Sorcery -- "As an additional cost to cast this spell,
-- sacrifice a creature. Return a creature card from your graveyard to the
-- battlefield, then return another creature card from your graveyard to your
-- hand." (name, cost, type line and Oracle text checked against
-- api.scryfall.com). The whole card is transcribed.
--
-- "ANOTHER" NEEDS NO EXCLUSION HERE: the two returns are two effects of one
-- resolution, each gathering its own candidates from the state it runs in (CR
-- 608.2c), and the first return has already taken its card out of the graveyard
-- -- CR 400.7 mints a new object at the destination and retires the old id --
-- before the second is offered. So the second choice cannot see the first, and
-- what the printed word forbids is already impossible.
--
-- The ANSWERER BELOW ASKS FOR IT ANYWAY, naming the first return's card at both
-- prompts, which is what keeps that from being an assumption: a second gather
-- that read a PRE-MOVE snapshot would put that id back on offer, the answerer
-- would take it, and the move of an id that no longer resolves would leave the
-- hand empty rather than holding the card these assertions name. No mutation
-- makes the engine offer it, because nothing in the engine can -- so this leg is
-- a regression fence for that property rather than a proof of an exclusion rule
-- pawl does not have.
--
-- The additional cost is load-bearing beside that: the sacrificed creature is in
-- the graveyard by the time the spell resolves, so it is a candidate for both
-- returns.
bloodForBonesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bloodForBonesSpec s registry =
  let -- alice controls six Swamps -- slack over the spell's four, for
      -- portOfKarfellSpec's reason -- and ONE creature, so the additional cost's
      -- victim is forced and no prompt of its own can be mistaken for the
      -- returns' prompts. `buried` goes into the named graveyards in the order
      -- given.
      board blood swamp victim buried =
        let mana = S.landsFor swamp S.alice 6 S.threePlayerGame
            (_, withVictim) = S.addPermanent victim S.alice mana
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) withVictim buried
            (withSpell, spell) = S.handOne blood withGraves
         in (spell, withSpell {GameState.priority = Just S.alice})
      named = Just . CardName.MkCardName . Text.pack
      arrivals gs =
        List.sort
          ( fmap
              (\oid -> (fmap S.nameOf (Game.cardOf oid gs), Projection.controllerOf oid gs))
              (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) /= named "Swamp") (Set.toList (GameState.battlefield gs)))
          )
      choices responses =
        length
          (filter (\r -> case r of Response.ChoseCardInGraveyard _ -> True; _ -> False) responses)
   in Spec.describe s "BloodForBones" $ do
        -- The headline: the card alice chose is on the battlefield, a DIFFERENT
        -- one she chose is in her hand, and the answerer asked for the first one
        -- both times.
        Spec.it s "CR 608.2c the second return cannot take the card the first one took" $ do
          blood <- S.printingOf s registry "Blood for Bones"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          sentry <- S.printingOf s registry "Ogre Sentry"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(maiden, S.alice), (murder, S.alice), (cavalry, S.alice), (sentry, S.alice), (hero, S.bob)]
              (spell, gs) = board blood swamp piker buried
              -- Pinned BY NAME rather than by position: the Ogre Sentry is the
              -- third of the four creature cards alice's graveyard holds once the
              -- Piker has paid the cost, so it is neither the first candidate nor
              -- next to it, and the Piker is the last.
              wantedBy name gs1 =
                List.find (\oid -> fmap S.nameOf (Game.cardOf oid gs1) == named name) (Game.zoneMembers Zone.Graveyard S.alice gs1)
              -- FIRST the Sentry, and then the Sentry AGAIN -- which the second
              -- return cannot grant, so the fallback is the pinned Piker.
              choosing :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
              choosing sentryId pikerId p = case p of
                Prompt.ChooseCardInGraveyard _ _ _ offered _ ->
                  if List.elem sentryId (NonEmpty.toList offered) then sentryId else pikerId
                _ -> S.identityAnswer p
              afterCast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
          case (wantedBy "Ogre Sentry" afterCast, wantedBy "Goblin Piker" afterCast) of
            (Just sentryId, Just pikerId) -> do
              let answer :: Prompt.Prompt r -> r
                  answer = choosing sentryId pikerId
                  ((_, after), responses) = Replay.record answer afterCast Stack.resolveTop
              Spec.assertEqWith s "alice was asked twice" (choices responses) 2
              Spec.assertEqWith
                s
                "the Ogre Sentry she chose first is on the battlefield under her control"
                (arrivals after)
                [(named "Ogre Sentry", Just S.alice)]
              Spec.assertEqWith
                s
                "and the Goblin Piker -- not the Sentry the answerer asked for twice -- is the card in her hand"
                (namesIn Zone.Hand S.alice after)
                [named "Goblin Piker"]
              Spec.assertEqWith
                s
                "the two cards neither return took, the noncreature and the spent sorcery stay in her graveyard"
                (List.sort (namesIn Zone.Graveyard S.alice after))
                (List.sort [named "Bird Maiden", named "Benalish Cavalry", named "Murder", named "Blood for Bones"])
              Spec.assertEqWith s "and bob's creature card was never a candidate" (namesIn Zone.Graveyard S.bob after) [named "Benalish Hero"]
            _ -> Spec.assertBool s False "expected the sacrificed Piker and the buried Sentry in alice's graveyard after the cast"
        -- Where "another" and "a creature card" come apart: ONE creature card in
        -- the whole graveyard. The first return takes it, the second has nothing
        -- left to name and is ignored (CR 101.3, CR 609.3), and nobody is asked
        -- at either step -- one candidate is not a choice.
        Spec.it s "CR 101.3 a lone creature card returns to the battlefield and nothing goes to hand" $ do
          blood <- S.printingOf s registry "Blood for Bones"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          murder <- S.printingOf s registry "Murder"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(murder, S.alice), (hero, S.bob)]
              (spell, gs) = board blood swamp piker buried
              ((_, after), responses) = Replay.record S.identityAnswer gs (S.cast S.alice spell >> Stack.resolveTop)
          Spec.assertEqWith s "neither return had anything to ask" (choices responses) 0
          Spec.assertEqWith
            s
            "the sacrificed Piker is the lone candidate and it comes back"
            (arrivals after)
            [(named "Goblin Piker", Just S.alice)]
          Spec.assertEqWith s "and alice's hand is empty" (namesIn Zone.Hand S.alice after) []
          Spec.assertEqWith
            s
            "the noncreature card and the spent sorcery stay buried"
            (List.sort (namesIn Zone.Graveyard S.alice after))
            (List.sort [named "Murder", named "Blood for Bones"])

-- The third CHOOSER, and the effect that fills it: Chooser.BoundInSlot over a
-- slot Effect.ChoosePlayer bound as the same resolution ran.
--
-- Skullwinder {2}{G} Creature -- Snake 1/3: "Deathtouch. When this creature
-- enters, return target card from your graveyard to your hand, then choose an
-- opponent. That player returns a card from their graveyard to their hand."
-- (name, cost, type line, power, toughness and Oracle text checked against
-- api.scryfall.com). The whole card is transcribed; nothing is elided.
--
-- TWO CHOICES AND AN ORDER BETWEEN THEM, which is the unit:
--
--   * WHICH OPPONENT, announced by the RESOLVING CONTROLLER as the effect is
--     applied (CR 608.2c, CR 608.2d). Not a target -- CR 115.10a makes a player
--     a target only where the text identifies them with the word, and this
--     sentence does not -- so no slot was announced at CR 601.2c and CR 608.2b
--     re-validates nothing.
--   * WHICH CARD, announced by THAT PLAYER, out of their own graveyard (CR
--     608.2d again: the player an effect instructs is the one who announces its
--     choices). "That player ... their graveyard" is the possessive Exhume's
--     "each player ... their graveyard" is, over one seat instead of every seat.
--
-- The order is the printed one (CR 608.2c "in the order written"): the opponent
-- must be chosen before there is a player for the second sentence to instruct.
--
-- CR 603.3d is why every leg stocks ALICE's graveyard: the first sentence has a
-- required target, and a trigger that can choose no legal target is removed from
-- the stack, taking the sentences after it along.
--
-- THREE SEATS, with the board built so the readings come apart -- a duel would
-- collapse "an opponent" onto the only other player and prove nothing:
--
--   * SOMEBODY ELSE CHOSE versus THE CONTROLLER CHOSE, twice over. The opponent
--     answer is pinned to CAROL, the LAST candidate, where the default answerer
--     takes bob; and the card answer is pinned to carol's THIRD, where the
--     default takes her first. The paired leg below runs the same board through
--     that default answerer and lands two different cards in a different hand.
--   * THE CHOSEN PLAYER was asked versus THE CONTROLLER was asked about their
--     graveyard. The answerer replies by the player the prompt NAMES: only a
--     prompt aimed at carol gets the third candidate, and one aimed at anybody
--     else gets the first.
--   * THEIR OWN graveyard versus the union of all of them. Alice's remaining
--     card and bob's three come before carol's in the union's ascending order,
--     so the THIRD candidate of a union is one of BOB's -- a different card, in a
--     different hand, than the third of carol's own.
--   * A GRAVEYARD RETURN at all versus a battlefield sweep: bob's graveyard must
--     be untouched in every leg.
skullwinderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
skullwinderSpec s registry =
  let -- alice controls four Forests -- slack over the creature's {2}{G}, so no
      -- payment order can fail for reasons of its own -- and holds a
      -- Skullwinder. `buried` goes into the named graveyards in the order given,
      -- which is also the ascending-id order the prompts offer them in.
      board skullwinder forest buried =
        let mana = S.landsFor forest S.alice 4 S.threePlayerGame
            withGraves = List.foldl' (\g (printing, pid) -> snd (S.addGraveyardCard printing pid g)) mana buried
            (withCard, handId) = S.handOne skullwinder withGraves
         in (handId, withCard {GameState.priority = Just S.alice})
      -- Cast, let the creature enter, let CR 603.3b put the enters trigger on the
      -- stack (which is where CR 603.3d picks its target), then resolve it.
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [Response.Response])
      run answer handId gs =
        let ((_, after), responses) =
              Replay.record answer gs $ do
                S.cast S.alice handId
                Stack.resolveTop
                Engine.settleForPriority
                Stack.resolveTop
         in (after, responses)
      named = Just . CardName.MkCardName . Text.pack
      opponentChoices responses = length (filter (\r -> case r of Response.ChoseOpponent _ -> True; _ -> False) responses)
      playerChoices responses = length (filter (\r -> case r of Response.ChosePlayer _ -> True; _ -> False) responses)
      cardChoices responses = length (filter (\r -> case r of Response.ChoseCardInGraveyard _ -> True; _ -> False) responses)
      -- The third candidate the prompt offers, by POSITION rather than by name:
      -- a name would be found again in a union of every graveyard, and the
      -- position is what tells the two offers apart.
      thirdOf offered = Maybe.fromMaybe (NonEmpty.head offered) (Maybe.listToMaybe (drop 2 (NonEmpty.toList offered)))
      -- Pinned to CAROL and to her THIRD card. Keyed on the player the prompt
      -- NAMES, so a prompt put to anybody else takes the first candidate and
      -- lands a different card in a different hand.
      choosing p = case p of
        Prompt.ChooseOpponent _ _ _ offered -> NonEmpty.last offered
        Prompt.ChooseCardInGraveyard _ pid _ offered _ ->
          if pid == S.carol then thirdOf offered else NonEmpty.head offered
        _ -> S.identityAnswer p
      -- alice's two, bob's three and carol's three, all distinct names so no
      -- assertion can read one seat's card as another's.
      stock hero cavalry berserkers murder maiden sentry judgment wraith =
        [ (murder, S.alice),
          (maiden, S.alice),
          (sentry, S.bob),
          (judgment, S.bob),
          (wraith, S.bob),
          (hero, S.carol),
          (cavalry, S.carol),
          (berserkers, S.carol)
        ]
   in Spec.describe s "Skullwinder" $ do
        -- The headline: alice picks the opponent, carol picks the card, and the
        -- card that moves is the one CAROL named out of CAROL's graveyard.
        Spec.it s "CR 608.2d the chosen opponent chooses in their own graveyard, and keeps what they choose" $ do
          skullwinder <- S.printingOf s registry "Skullwinder"
          forest <- S.printingOf s registry "Forest"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          judgment <- S.printingOf s registry "Day of Judgment"
          wraith <- S.printingOf s registry "Bog Wraith"
          hero <- S.printingOf s registry "Benalish Hero"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
          let (handId, gs) = board skullwinder forest (stock hero cavalry berserkers murder maiden sentry judgment wraith)
              (after, responses) = run choosing handId gs
          Spec.assertEqWith s "one opponent was chosen" (opponentChoices responses) 1
          Spec.assertEqWith s "and exactly one graveyard card choice was put, not one per seat" (cardChoices responses) 1
          Spec.assertEqWith
            s
            "carol's THIRD card is in carol's hand"
            (namesIn Zone.Hand S.carol after)
            [named "Berserkers of Blood Ridge"]
          Spec.assertEqWith
            s
            "her other two stay buried"
            (List.sort (namesIn Zone.Graveyard S.carol after))
            (List.sort [named "Benalish Hero", named "Benalish Cavalry"])
          Spec.assertEqWith
            s
            "bob was not the opponent chosen, so his graveyard is whole and his hand empty"
            (List.sort (namesIn Zone.Graveyard S.bob after), namesIn Zone.Hand S.bob after)
            (List.sort [named "Ogre Sentry", named "Day of Judgment", named "Bog Wraith"], [])
          Spec.assertEqWith
            s
            "alice got her own targeted card back and nothing else"
            (namesIn Zone.Hand S.alice after, namesIn Zone.Graveyard S.alice after)
            ([named "Murder"], [named "Bird Maiden"])
          Spec.assertEqWith
            s
            "and the Snake itself is on the battlefield"
            (List.sort (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (filter (\oid -> fmap S.nameOf (Game.cardOf oid after) /= named "Forest") (Set.toList (GameState.battlefield after)))))
            [named "Skullwinder"]
        -- CR 102.2 / 608.2d: PlayerScope.Opponents leaves the CONTROLLER out of
        -- the offer, so an answerer naming alice is answering a question she was
        -- never put -- the offer is filtered and the first opponent takes it.
        -- The board is the headline's, one answerer apart. A scope that admitted
        -- her would let her raid her own graveyard a second time, which the hand
        -- and graveyard assertions read directly; the recorded response is the
        -- other half, since the engine raises ChoosePlayer rather than
        -- ChooseOpponent exactly when the offer holds the chooser.
        Spec.it s "CR 102.2 the controller is not among the opponents offered" $ do
          skullwinder <- S.printingOf s registry "Skullwinder"
          forest <- S.printingOf s registry "Forest"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          judgment <- S.printingOf s registry "Day of Judgment"
          wraith <- S.printingOf s registry "Bog Wraith"
          hero <- S.printingOf s registry "Benalish Hero"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
          let namingAlice p = case p of
                Prompt.ChooseOpponent {} -> S.alice
                _ -> S.identityAnswer p
              (handId, gs) = board skullwinder forest (stock hero cavalry berserkers murder maiden sentry judgment wraith)
              (after, responses) = run namingAlice handId gs
          Spec.assertEqWith s "alice keeps only her own targeted card, her second staying buried" (namesIn Zone.Hand S.alice after, namesIn Zone.Graveyard S.alice after) ([named "Murder"], [named "Bird Maiden"])
          Spec.assertEqWith s "bob, the first opponent, is the one the fallback named" (namesIn Zone.Hand S.bob after) [named "Ogre Sentry"]
          Spec.assertEqWith s "and the prompt put was an opponent's, never a player's" (opponentChoices responses, playerChoices responses) (1, 0)
        -- The paired control, on the SAME board with only the answerer changed:
        -- the default takes the first opponent and the first card, so a different
        -- seat is asked and a different card moves. Two seats' worth of
        -- difference from one answerer swap is what tells "the engine chose" from
        -- "the players chose".
        Spec.it s "CR 608.2d the engine picks neither choice: another answer names another seat and another card" $ do
          skullwinder <- S.printingOf s registry "Skullwinder"
          forest <- S.printingOf s registry "Forest"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          judgment <- S.printingOf s registry "Day of Judgment"
          wraith <- S.printingOf s registry "Bog Wraith"
          hero <- S.printingOf s registry "Benalish Hero"
          cavalry <- S.printingOf s registry "Benalish Cavalry"
          berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
          let (handId, gs) = board skullwinder forest (stock hero cavalry berserkers murder maiden sentry judgment wraith)
              (after, _) = run S.identityAnswer handId gs
          Spec.assertEqWith s "bob is the first opponent, and his first card comes back" (namesIn Zone.Hand S.bob after) [named "Ogre Sentry"]
          Spec.assertEqWith s "carol is untouched" (namesIn Zone.Hand S.carol after, length (namesIn Zone.Graveyard S.carol after)) ([], 3)
        -- CR 101.3 / CR 609.3 for the chosen player: an empty graveyard leaves
        -- nothing to name, so that share of the instruction is ignored rather
        -- than the trigger failing. The first sentence's return is what proves
        -- the ability really did resolve.
        Spec.it s "CR 101.3 a chosen opponent with an empty graveyard is a no-op rather than a failure" $ do
          skullwinder <- S.printingOf s registry "Skullwinder"
          forest <- S.printingOf s registry "Forest"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          let buried = [(murder, S.alice), (maiden, S.alice), (sentry, S.bob)]
              (handId, gs) = board skullwinder forest buried
              (after, responses) = run choosing handId gs
          Spec.assertEqWith s "carol was chosen and had nothing to be asked about" (opponentChoices responses, cardChoices responses) (1, 0)
          Spec.assertEqWith s "nothing came out of any graveyard but alice's own target" (namesIn Zone.Hand S.carol after, namesIn Zone.Hand S.bob after) ([], [])
          Spec.assertEqWith s "bob's graveyard is whole" (namesIn Zone.Graveyard S.bob after) [named "Ogre Sentry"]
          Spec.assertEqWith s "and the trigger really did resolve" (namesIn Zone.Hand S.alice after) [named "Murder"]
        -- One candidate is not a choice: the chosen player is NOT asked, and the
        -- lone card still comes back. Paired with the leg above on the same
        -- board, one card apart.
        Spec.it s "CR 608.2d a lone card in the chosen player's graveyard is not put to them" $ do
          skullwinder <- S.printingOf s registry "Skullwinder"
          forest <- S.printingOf s registry "Forest"
          murder <- S.printingOf s registry "Murder"
          maiden <- S.printingOf s registry "Bird Maiden"
          sentry <- S.printingOf s registry "Ogre Sentry"
          hero <- S.printingOf s registry "Benalish Hero"
          let buried = [(murder, S.alice), (maiden, S.alice), (sentry, S.bob), (hero, S.carol)]
              (handId, gs) = board skullwinder forest buried
              (after, responses) = run choosing handId gs
          Spec.assertEqWith s "the opponent was chosen, the card was not asked about" (opponentChoices responses, cardChoices responses) (1, 0)
          Spec.assertEqWith s "and carol's only card came back anyway" (namesIn Zone.Hand S.carol after, namesIn Zone.Graveyard S.carol after) ([named "Benalish Hero"], [])

-- The FILTER on ObjectRef.ChosenCardInHand: which cards in a hand the choice is
-- offered over. CR 402.3 is what licenses one over a zone CR 400.2 makes hidden
-- -- the chooser is the hand's own owner, so narrowing what they are offered
-- reveals nothing to anybody -- and Karn Liberated's unfiltered "a card from
-- their hand" cannot tell the two readings apart.
--
-- Elvish Piper {3}{G} Creature -- Elf Shaman 1/1 -- "{G}, {T}: You may put a
-- creature card from your hand onto the battlefield." (name, cost, type line, P/T
-- and Oracle text checked against api.scryfall.com). Its whole printed text is
-- that one ability, so nothing else on the card can be what these assertions
-- read.
--
-- The proof is the CANDIDATE SET rather than the answer, which is what the
-- response count reads: the prompt is raised only at two or more candidates, so
-- "how many matched" is observable without trusting an answerer that could pick
-- the right card under either reading. The pinned answer names a card the filter
-- excludes, so an unfiltered gather would offer it and put it onto the
-- battlefield.
--
-- The board tells apart the readings a wrong or missing filter would take:
--
--   * CREATURE CARDS versus every card. alice's hand holds one creature card
--     beside a land and an instant, all three distinct printings.
--   * YOUR hand versus each player's. bob holds a creature card of his own, which
--     must stay in his hand -- and carol is the third seat, so "an opponent" is
--     not collapsed onto the only other player.
--   * A HAND at all versus a graveyard or a library. Nothing is in either.
elvishPiperSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
elvishPiperSpec s registry =
  let -- alice controls three Forests and one untapped Elvish Piper, and holds
      -- `mine`; bob holds `theirs`. Returns the Piper's id and each of alice's
      -- hand cards, in the order given.
      board piper forest mine theirs =
        let mana = S.landsFor forest S.alice 3 S.threePlayerGame
            (piperId, withPiper) = S.addPermanent piper S.alice mana
            (withMine, mineIds) =
              List.foldl'
                (\(g, ids) printing -> let (oid, g2) = S.addHandCard printing S.alice g in (g2, ids <> [oid]))
                (withPiper, [])
                mine
            withTheirs = List.foldl' (\g printing -> snd (S.addHandCard printing S.bob g)) withMine theirs
         in (piperId, mineIds, withTheirs {GameState.priority = Just S.alice})
      -- Activate the Piper's one ability and resolve it, keeping the RESPONSES
      -- beside the board -- portOfKarfellSpec's run, one card over.
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> Maybe (GameState.GameState, [Response.Response])
      run answer piperId gs = case Activatable.abilitiesFor piperId gs of
        [ability] ->
          let ((_, after), responses) = Replay.record answer gs (Activate.activateAbility S.alice piperId ability >> Stack.resolveTop)
           in Just (after, responses)
        _ -> Nothing
      -- How many times a player was asked which card in their hand to take. ZERO
      -- is the observation that pins the candidate set: one matching card is
      -- elided (CR 101.3), so a gather that offered the whole hand would ask.
      handChoices responses = length (filter (\r -> case r of Response.ChoseCardInHand _ -> True; _ -> False) responses)
      -- How many times CR 603.5's printed "may" was put.
      mays responses = length (filter (\r -> case r of Response.ChoseOptional _ -> True; _ -> False) responses)
      named = Just . CardName.MkCardName . Text.pack
      -- alice's battlefield minus the Forests: the Piper, plus whatever the
      -- ability put there. By NAME, since CR 400.7 mints a fresh id at the
      -- destination.
      arrivals gs =
        List.sort
          ( filter
              (/= named "Forest")
              ( fmap
                  (\oid -> fmap S.nameOf (Game.cardOf oid gs))
                  (filter (\oid -> Projection.controllerOf oid gs == Just S.alice) (Set.toList (GameState.battlefield gs)))
              )
          )
      -- Says yes to the printed "may" and names `wanted` when a hand choice is
      -- put. Answering the "may" is what makes the ability do anything at all --
      -- S.identityAnswer declines it.
      taking :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      taking wanted p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseCardInHand {} -> wanted
        _ -> S.identityAnswer p
   in Spec.describe s "ElvishPiper" $ do
        -- The headline. The pinned answer is the MOUNTAIN, which the filter
        -- excludes: an unfiltered gather offers it, asks, and puts a land onto the
        -- battlefield, where the filtered one offers only the Piker, asks nothing
        -- and puts the Piker there.
        Spec.it s "CR 402.3 only the creature cards in your own hand are offered" $ do
          piper <- S.printingOf s registry "Elvish Piper"
          forest <- S.printingOf s registry "Forest"
          piker <- S.printingOf s registry "Goblin Piker"
          mountain <- S.printingOf s registry "Mountain"
          growth <- S.printingOf s registry "Giant Growth"
          wolves <- S.printingOf s registry "Russet Wolves"
          let (piperId, mineIds, gs) = board piper forest [piker, mountain, growth] [wolves]
          case mineIds of
            [_, mountainId, _] -> case run (taking mountainId) piperId gs of
              Just (after, responses) -> do
                Spec.assertEqWith s "CR 608.2d a creature card to put makes the may a real option: it was put" (mays responses) 1
                Spec.assertEqWith s "one candidate matched, so nothing was asked" (handChoices responses) 0
                Spec.assertEqWith
                  s
                  "the Goblin Piker is on the battlefield beside the Piper, and the Mountain the answer named is not"
                  (arrivals after)
                  (List.sort [named "Elvish Piper", named "Goblin Piker"])
                Spec.assertEqWith
                  s
                  "the two cards the filter excluded are still in her hand"
                  (List.sort (namesIn Zone.Hand S.alice after))
                  (List.sort [named "Mountain", named "Giant Growth"])
                Spec.assertEqWith s "bob's creature card stayed in bob's hand" (namesIn Zone.Hand S.bob after) [named "Russet Wolves"]
                Spec.assertEqWith s "and carol was not asked to give anything up" (S.handSize S.carol after) 0
              Nothing -> Spec.assertFailure s "Elvish Piper's ability did not resolve"
            _ -> Spec.assertFailure s "the fixture did not put three cards in alice's hand"
        -- Narrowing the candidates must not turn the choice into the engine's.
        -- TWO creature cards match, so the prompt is real, and the pair of legs
        -- differs only in the answer: the pinned one takes the Wolves, the default
        -- takes the Piker.
        Spec.it s "CR 608.2d the player still chooses when two creature cards match" $ do
          piper <- S.printingOf s registry "Elvish Piper"
          forest <- S.printingOf s registry "Forest"
          piker <- S.printingOf s registry "Goblin Piker"
          wolves <- S.printingOf s registry "Russet Wolves"
          mountain <- S.printingOf s registry "Mountain"
          let (piperId, mineIds, gs) = board piper forest [piker, wolves, mountain] []
              -- The control leg keeps the "may" and drops only the hand answer,
              -- so the default takes the first candidate offered instead -- which
              -- is the Russet Wolves, the pinned leg's answer being the Piker.
              defaulting p = case p of
                Prompt.ChooseOptional {} -> OptionalDecision.Exercises
                _ -> S.identityAnswer p
          case mineIds of
            [pikerId, _, _] -> case (run (taking pikerId) piperId gs, run defaulting piperId gs) of
              (Just (after, responses), Just (control, controlResponses)) -> do
                Spec.assertEqWith s "two candidates matched, so alice was asked" (handChoices responses, handChoices controlResponses) (1, 1)
                Spec.assertEqWith
                  s
                  "the Goblin Piker she named is what entered"
                  (arrivals after)
                  (List.sort [named "Elvish Piper", named "Goblin Piker"])
                Spec.assertEqWith s "and the Wolves she did not name is still in hand" (List.sort (namesIn Zone.Hand S.alice after)) (List.sort [named "Russet Wolves", named "Mountain"])
                Spec.assertEqWith
                  s
                  "the engine does not pick: the default answer brings the OTHER creature card in"
                  (arrivals control)
                  (List.sort [named "Elvish Piper", named "Russet Wolves"])
              _ -> Spec.assertFailure s "Elvish Piper's ability did not resolve"
            _ -> Spec.assertFailure s "the fixture did not put three cards in alice's hand"
        -- No candidate at all, so the option is impossible and CR 608.2d does not
        -- put the "may". An unfiltered gather would offer the land and the
        -- instant and put one of them onto the battlefield.
        Spec.it s "CR 608.2d a hand holding no creature card offers nothing" $ do
          piper <- S.printingOf s registry "Elvish Piper"
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          growth <- S.printingOf s registry "Giant Growth"
          let (piperId, mineIds, gs) = board piper forest [mountain, growth] []
          case mineIds of
            [mountainId, _] -> case run (taking mountainId) piperId gs of
              Just (after, responses) -> do
                Spec.assertEqWith s "CR 608.2d the may was never put" (mays responses) 0
                Spec.assertEqWith s "nothing was asked" (handChoices responses) 0
                Spec.assertEqWith s "only the Piper is on her battlefield" (arrivals after) [named "Elvish Piper"]
                Spec.assertEqWith s "and both cards are still in her hand" (List.sort (namesIn Zone.Hand S.alice after)) (List.sort [named "Mountain", named "Giant Growth"])
              Nothing -> Spec.assertFailure s "Elvish Piper's ability did not resolve"
            _ -> Spec.assertFailure s "the fixture did not put two cards in alice's hand"

-- CR 401.2's ordered pile named by POSITION rather than by characteristics:
-- ObjectRef.TopOfLibrary, the arm no Filter could stand in for.
--
-- Count on Luck {R}{R}{R} Enchantment -- "At the beginning of your upkeep, exile
-- the top card of your library. You may play that card this turn." (name, cost,
-- type line and Oracle text checked against api.scryfall.com). Its whole text is
-- the one trigger, so nothing else on the card can be what these assertions read.
--
-- The board is built so that three readings of "the top card of your library"
-- are told apart, since a board that cannot distinguish them proves nothing:
--
--   * TOP versus any other card. alice's library holds three distinct cards, so
--     an arm reading the bottom or an arbitrary member names a different one.
--   * YOUR library versus each player's. bob's library is stocked too, with a
--     printing that appears nowhere in alice's, and it must be untouched -- which
--     also covers the "target opponent's" reading.
--   * A LIBRARY read at all versus a battlefield sweep. The enchantment itself
--     and nothing else is on the battlefield, and it stays there.
--
-- The permission the second sentence grants is asserted only as far as CR 400.7's
-- binding goes: the exiled card carries one, which is what proves the MoveToZone's
-- slot bound the incarnation this arm minted rather than some other object.
countOnLuckSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
countOnLuckSpec s registry =
  let -- alice controls Count on Luck and her library holds `stock`, DEEPEST
      -- FIRST -- S.addLibraryCard puts each card on top, so the last name given
      -- is the top card. bob's library holds one Ogre Sentry, a printing alice
      -- never has. Her upkeep then begins, the trigger goes on the stack and
      -- resolves.
      board stock = do
        countOnLuck <- S.printingOf s registry "Count on Luck"
        sentry <- S.printingOf s registry "Ogre Sentry"
        stocked <- mapM (S.printingOf s registry) stock
        let (luckId, g1) = S.addPermanent countOnLuck S.alice (Setup.emptyGame S.bothPlayers)
            g2 = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) g1 stocked
            g3 = snd (S.addLibraryCard sentry S.bob g2)
            upkeep = Phase.Beginning BeginningStep.Upkeep
            begun =
              Event.recordEvent
                (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
                (g3 {GameState.phase = upkeep, GameState.activePlayer = S.alice})
            onStack = S.runPure S.identityAnswer begun Engine.settleForPriority
        pure (luckId, S.runPure S.identityAnswer onStack Engine.priorityLoop)
      -- What the top-level namesIn answers with. It reports a zone in its own
      -- order, which for a library is top first -- Pawl.Engine.Game.zoneMembers
      -- hands the Seq back as stored.
      named = Just . CardName.MkCardName . Text.pack
      permissionsIn pid gs = fmap Object.playableFromExile (Maybe.mapMaybe (\oid -> Game.lookupObject oid gs) (Game.zoneMembers Zone.Exile pid gs))
   in Spec.describe s "CountOnLuck" $ do
        Spec.it s "CR 401.2 the top card of your library, and only it, is exiled" $ do
          (luckId, after) <- board ["Goblin Piker", "Bird Maiden", "Benalish Hero"]
          Spec.assertEqWith
            s
            "the Benalish Hero on top is in exile and the two under it are still in the library, in order"
            (namesIn Zone.Exile S.alice after, namesIn Zone.Library S.alice after)
            ([named "Benalish Hero"], [named "Bird Maiden", named "Goblin Piker"])
          Spec.assertEqWith
            s
            "bob's library is untouched, so this is not each player's library and not an opponent's"
            (namesIn Zone.Library S.bob after, namesIn Zone.Exile S.bob after)
            ([named "Ogre Sentry"], [])
          Spec.assertBool s (S.onBattlefield luckId after) "the enchantment is still on the battlefield, so nothing swept it"
          Spec.assertEqWith
            s
            "the one exiled card carries the play permission, so the move bound the incarnation IT minted"
            (fmap Maybe.isJust (permissionsIn S.alice after))
            [True]
        -- The empty-library case, which is the same board minus the stock alone.
        -- CR 104.3c takes nobody out of the game here: an empty library only
        -- loses when its owner would DRAW from it, and the trigger draws nothing.
        Spec.it s "CR 401.2 an empty library has no top card, so the exile does nothing" $ do
          (luckId, after) <- board []
          Spec.assertEqWith
            s
            "nothing at all was exiled"
            (namesIn Zone.Exile S.alice after, namesIn Zone.Exile S.bob after)
            ([], [])
          Spec.assertEqWith s "bob's library is still untouched" (namesIn Zone.Library S.bob after) [named "Ogre Sentry"]
          Spec.assertBool s (S.onBattlefield luckId after) "and alice is still in the game with her enchantment"
          Spec.assertEqWith s "the game has no result: an empty library is not itself a loss" (GameState.result after) Nothing

-- The DEPTH on ObjectRef.TopOfLibrary, and the group binding a move of several
-- cards owes its second sentence.
--
-- Act on Impulse {2}{R} Sorcery -- "Exile the top three cards of your library.
-- Until end of turn, you may play those cards." (name, cost, type line and Oracle
-- text checked against api.scryfall.com). Its whole printed text is those two
-- sentences, so nothing else on the card can be what these assertions read.
--
-- alice casts it off three Mountains and the priority loop resolves it, which is
-- what makes this gameplay-level rather than an applyEffect call.
--
-- The board tells the readings apart that a wrong depth or a wrong binding would
-- take:
--
--   * THREE versus one, and versus all of them. Her library holds FIVE distinct
--     cards, so "the top card" leaves four behind and "her library" leaves none;
--     both the exiled three and the two left are asserted, in the pile's order
--     for the two that stay (CR 401.2).
--   * THE TOP three versus the bottom three. The five are distinct printings, so
--     the two answers name disjoint sets.
--   * YOUR library versus each player's. bob's library is stocked with a
--     printing alice never has, and it must be untouched.
--   * THOSE CARDS versus one of them. All three exiled cards must carry the play
--     permission: a MoveToZone that bound only the last incarnation it minted
--     leaves two of the three without one.
actOnImpulseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
actOnImpulseSpec s registry =
  let -- alice's library holds `stock`, DEEPEST FIRST -- S.addLibraryCard puts
      -- each card on top, so the last name given is the top card. bob's library
      -- holds one Ogre Sentry, a printing alice never has.
      board stock = do
        mountain <- S.printingOf s registry "Mountain"
        actOnImpulse <- S.printingOf s registry "Act on Impulse"
        sentry <- S.printingOf s registry "Ogre Sentry"
        stocked <- mapM (S.printingOf s registry) stock
        let g1 = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) (S.landsInPlay mountain 3) stocked
            g2 = snd (S.addLibraryCard sentry S.bob g1)
            (withSpell, spell) = S.handOne actOnImpulse g2
            afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
        pure (S.runPure S.identityAnswer afterCast Engine.priorityLoop)
      named = Just . CardName.MkCardName . Text.pack
      -- SORTED, because exile is a holding area with no order of its own (CR
      -- 406.1) -- unlike the library below, whose order CR 401.2 fixes.
      exiledNames pid = List.sort . namesIn Zone.Exile pid
      permissionsIn pid gs = fmap (Maybe.isJust . Object.playableFromExile) (Maybe.mapMaybe (\oid -> Game.lookupObject oid gs) (Game.zoneMembers Zone.Exile pid gs))
   in Spec.describe s "ActOnImpulse" $ do
        Spec.it s "CR 401.2 the top three cards of your library are exiled, and the rest stay put" $ do
          after <- board ["Goblin Piker", "Bird Maiden", "Benalish Hero", "Hill Giant", "Sabretooth Tiger"]
          Spec.assertEqWith
            s
            "the top three are in exile and the two under them are still in the library, in order"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            ( List.sort [named "Sabretooth Tiger", named "Hill Giant", named "Benalish Hero"],
              [named "Bird Maiden", named "Goblin Piker"]
            )
          Spec.assertEqWith
            s
            "bob's library is untouched, so this is not each player's library"
            (namesIn Zone.Library S.bob after, namesIn Zone.Exile S.bob after)
            ([named "Ogre Sentry"], [])
          Spec.assertEqWith
            s
            "ALL THREE exiled cards carry the play permission, so the move bound the whole group and not one incarnation of it"
            (permissionsIn S.alice after)
            [True, True, True]
        -- Fewer cards than the depth: CR 609.3 does only as much as possible, and
        -- CR 104.3c takes nobody out of the game for it -- an empty library only
        -- loses when its owner would DRAW from it, and this spell draws nothing.
        Spec.it s "CR 609.3 a library shorter than the depth gives up what it has" $ do
          after <- board ["Goblin Piker", "Bird Maiden"]
          Spec.assertEqWith
            s
            "both cards were exiled and the library is empty"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            (List.sort [named "Goblin Piker", named "Bird Maiden"], [])
          Spec.assertEqWith s "and both carry the permission" (permissionsIn S.alice after) [True, True]
          Spec.assertEqWith s "the game has no result: an empty library is not itself a loss" (GameState.result after) Nothing
        -- ONE card, which is the other binding shape: a single arrival binds the
        -- singular slot, and the permission still reaches it.
        Spec.it s "CR 609.3 a one-card library gives up its one card" $ do
          after <- board ["Goblin Piker"]
          Spec.assertEqWith s "the one card is exiled" (exiledNames S.alice after) [named "Goblin Piker"]
          Spec.assertEqWith s "and carries the permission" (permissionsIn S.alice after) [True]

-- CR 611.2a: a duration that names a WINDOW rather than a deadline, on the PLAY
-- PERMISSION carrier -- Galvanic Relay {2}{R} Sorcery, "Exile the top card of
-- your library. During your next turn, you may play that card. / Storm" (name,
-- cost, type line and Oracle text checked against api.scryfall.com 2026-09-22).
-- Storm is printed and works; with no spell cast before it the trigger makes no
-- copy, so nothing on the card but those two sentences reaches these assertions.
--
-- The permission lands on Object.playableFromExile under Expiry.DuringTurnOf and
-- Pawl.Engine.Cast.permitsPlayFromExile is the gate that keeps it inert until
-- that turn -- the second carrier to ask Pawl.Engine.Expiry.begun.
--
-- THE PAIR is Commune with Lava {X}{R}{R} Instant, announced at X = 1: the same
-- one card off the same library under a permission for the same player, differing
-- only in that its duration states an END and no beginning (Duration.UntilEndOfYourNextTurn).
-- A leg that is green because the exiled card was unaffordable, uncastable by
-- timing or never exiled at all would take the pair down with it.
--
-- EIGHT Mountains, so the exiled Goblin Piker's {2}{R} is payable on every turn
-- asked about, the turn the Relay itself was cast included (cast-gate vacuity).
galvanicRelaySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
galvanicRelaySpec s registry =
  let -- alice's turn 3 and turn 5, reached through Engine.handoffTurn so CR
      -- 514.2's sweep runs ahead of each handoff: the row must survive the
      -- cleanup of the turn it was stored on, which is the whole difference from
      -- "this turn".
      handoff gs =
        S.runPure
          S.identityAnswer
          (Expiry.dropAtCleanup gs)
          (Engine.handoffTurn >> Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
      -- The active player's precombat main phase with priority, which is where a
      -- sorcery-speed play is offered at all (CR 307.1).
      mainPhase gs = gs {GameState.phase = Phase.PrecombatMain, GameState.priority = Just (GameState.activePlayer gs)}
      offeredTo pid oid gs = any (S.isCastOf oid) (Action.legalActions pid (mainPhase gs))
      permissionFor oid gs = Game.lookupObject oid gs >>= Object.playableFromExile
   in Spec.describe s "GalvanicRelay" $ do
        -- THE PROVING TEST: the window has not begun on the turn the Relay
        -- resolved, and the pair's end-only duration is live on that same turn.
        Spec.it s "CR 611.2a the permission is inert on the turn it was stored, where an end-only duration is not" $ do
          (exiled, relayed) <- relayBoard s registry "Galvanic Relay"
          (openExiled, communed) <- relayBoard s registry "Commune with Lava"
          Spec.assertBool s (not (offeredTo S.alice exiled relayed)) "CR 611.2a: alice's next turn has not begun, so the exiled card is not offered"
          Spec.assertBool s (offeredTo S.alice openExiled communed) "the pair: under a duration that states only an end, the same card is offered at once"
          -- Ordered BEHIND the offers so a permission that was never stored
          -- reddens the pair rather than being absorbed here.
          Spec.assertEqWith s "the Relay stored a window naming alice's turn after turn 1" (fmap ExilePlayPermission.expiry (permissionFor exiled relayed)) (Just (Expiry.Type.DuringTurnOf (AfterTurn.MkAfterTurn S.alice 1)))
          Spec.assertEqWith s "and the pair a deadline off the same pair of samples" (fmap ExilePlayPermission.expiry (permissionFor openExiled communed)) (Just (Expiry.Type.AtEndOfTurnOf (AfterTurn.MkAfterTurn S.alice 1)))
        Spec.it s "CR 611.2a and the card is playable once that turn has begun" $ do
          (exiled, relayed) <- relayBoard s registry "Galvanic Relay"
          let bobsTurn = handoff relayed
              alicesTurn = handoff bobsTurn
          Spec.assertEqWith s "alice is active on turn 3" (GameState.activePlayer alicesTurn, GameState.turnNumber alicesTurn) (S.alice, 3)
          Spec.assertBool s (offeredTo S.alice exiled alicesTurn) "CR 611.2a: the window is open, so alice may play the exiled card"
          -- Not swept on bob's turn, just not begun -- which is the whole claim.
          Spec.assertBool s (Maybe.isJust (permissionFor exiled bobsTurn)) "and the permission was still stored through bob's turn"
        Spec.it s "CR 514.2 the window closes at the end of the turn it named" $ do
          (exiled, relayed) <- relayBoard s registry "Galvanic Relay"
          let afterwards = handoff (handoff (handoff relayed))
              later = handoff afterwards
          Spec.assertEqWith s "alice is active again on turn 5" (GameState.activePlayer later, GameState.turnNumber later) (S.alice, 5)
          Spec.assertBool s (not (offeredTo S.alice exiled later)) "the card is no longer playable on the turn after the one the window named"
          -- Ordered BEHIND the offer so a sweep that kept the permission reddens
          -- the gameplay assertion rather than an absence here.
          Spec.assertEqWith s "with the permission cleared once that turn's cleanup has run" (permissionFor exiled afterwards) Nothing

-- alice's precombat main phase on turn 1, eight Mountains out and one Goblin
-- Piker as the whole of her library, with `name` in hand and cast. Returns the
-- id the exile MINTED (CR 400.7) rather than the library card's, and the board
-- with the stack resolved down to empty -- which is what settles Galvanic
-- Relay's storm trigger along with the spell.
relayBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m (ObjectId.ObjectId, GameState.GameState)
relayBoard s registry name = do
  mountain <- S.printingOf s registry "Mountain"
  spell <- S.printingOf s registry name
  piker <- S.printingOf s registry "Goblin Piker"
  let g1 = snd (S.addLibraryCard piker S.alice (S.landsFor mountain S.alice 8 (Setup.emptyGame S.bothPlayers)))
      (spellId, g2) = S.addHandCard spell S.alice g1
      board =
        g2
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      -- Commune with Lava's X, which Galvanic Relay never asks: its depth is a
      -- literal one.
      announcing :: Prompt.Prompt r -> r
      announcing p = case p of
        Prompt.ChooseX {} -> 1
        _ -> S.identityAnswer p
      step gs game = S.runPure announcing gs (game >> Engine.settleForPriority)
      resolveAll gs = if null (GameState.stack gs) then gs else resolveAll (step gs Stack.resolveTop)
      after = resolveAll (step board (S.cast S.alice spellId))
  Spec.assertEqWith s "one card is in exile" (length (Game.zoneMembers Zone.Exile S.alice after)) 1
  case Game.zoneMembers Zone.Exile S.alice after of
    [exiled] -> pure (exiled, after)
    _ -> pure (ObjectId.MkObjectId 0, after)

-- A COMPUTED depth on ObjectRef.TopOfLibrary: CR 601.2b's announced X, read as
-- how deep into a library a move reaches. Act on Impulse's literal three above
-- cannot tell "the depth is a number" from "the depth is a number the card
-- computed"; Commune with Lava's X is the first printing that can.
--
-- Commune with Lava {X}{R}{R} Instant -- "Exile the top X cards of your library.
-- Until the end of your next turn, you may play those cards." (name, cost, type
-- line and Oracle text checked against api.scryfall.com). Its whole printed text
-- is those two sentences.
--
-- The board tells the readings apart:
--
--   * The ANNOUNCED X versus any fixed number. Two legs on the same library and
--     the same six Mountains announce X = 1 and X = 3 and must exile one card and
--     three; a depth read as a literal, or as zero because nothing saw the X,
--     gives both legs the same answer.
--   * SIX Mountains on both legs, so the X = 3 leg is not proving something about
--     affordability (cast-gate vacuity): X = 1 leaves four Mountains unspent.
--   * YOUR library versus each player's. bob's library holds a printing alice
--     never has, and it must be untouched.
--   * CR 609.3: X = 3 against a two-card library exiles two rather than failing.
communeWithLavaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
communeWithLavaSpec s registry =
  let -- alice holds Commune with Lava over six Mountains, her library stocked with
      -- `stock` DEEPEST FIRST -- S.addLibraryCard puts each card on top, so the
      -- last name given is the top card. bob's library holds one Ogre Sentry, a
      -- printing alice never has. `x` is what she announces.
      board x stock = do
        mountain <- S.printingOf s registry "Mountain"
        commune <- S.printingOf s registry "Commune with Lava"
        sentry <- S.printingOf s registry "Ogre Sentry"
        stocked <- mapM (S.printingOf s registry) stock
        let g1 = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) (S.landsInPlay mountain 6) stocked
            g2 = snd (S.addLibraryCard sentry S.bob g1)
            (withSpell, spell) = S.handOne commune g2
            announced = x :: Natural
            announcing :: Prompt.Prompt r -> r
            announcing p = case p of
              Prompt.ChooseX {} -> announced
              _ -> S.identityAnswer p
            afterCast = S.runPure announcing withSpell (S.cast S.alice spell)
        pure (S.runPure announcing afterCast Engine.priorityLoop)
      named = Just . CardName.MkCardName . Text.pack
      -- SORTED, because exile is a holding area with no order of its own (CR
      -- 406.1) -- actOnImpulseSpec's reason.
      exiledNames pid = List.sort . namesIn Zone.Exile pid
      permissionsIn pid gs = fmap (Maybe.isJust . Object.playableFromExile) (Maybe.mapMaybe (\oid -> Game.lookupObject oid gs) (Game.zoneMembers Zone.Exile pid gs))
      fiveCards = ["Goblin Piker", "Bird Maiden", "Benalish Hero", "Hill Giant", "Sabretooth Tiger"]
   in Spec.describe s "CommuneWithLava" $ do
        -- The pair, and the whole proof: one board, two announced values, two
        -- depths.
        Spec.it s "CR 601.2b the announced X is how deep the exile reaches" $ do
          one <- board 1 fiveCards
          three <- board 3 fiveCards
          Spec.assertEqWith
            s
            "X=1 takes the top card alone and leaves the four under it, in order"
            (exiledNames S.alice one, namesIn Zone.Library S.alice one)
            ( [named "Sabretooth Tiger"],
              [named "Hill Giant", named "Benalish Hero", named "Bird Maiden", named "Goblin Piker"]
            )
          Spec.assertEqWith
            s
            "X=3 takes the top three and leaves the two under them, in order"
            (exiledNames S.alice three, namesIn Zone.Library S.alice three)
            ( List.sort [named "Sabretooth Tiger", named "Hill Giant", named "Benalish Hero"],
              [named "Bird Maiden", named "Goblin Piker"]
            )
          Spec.assertEqWith
            s
            "bob's library is untouched on both legs, so this is not each player's library"
            (namesIn Zone.Library S.bob one, namesIn Zone.Library S.bob three)
            ([named "Ogre Sentry"], [named "Ogre Sentry"])
          Spec.assertEqWith
            s
            "every exiled card carries the play permission on both legs, so the move bound the whole group"
            (permissionsIn S.alice one, permissionsIn S.alice three)
            ([True], [True, True, True])
        -- CR 609.3: fewer cards than the announced depth gives up what there is.
        -- CR 104.3c takes nobody out of the game for it -- an empty library only
        -- loses when its owner would DRAW from it, and this spell draws nothing.
        Spec.it s "CR 609.3 X above the library's size exiles what it has" $ do
          after <- board 3 ["Goblin Piker", "Bird Maiden"]
          Spec.assertEqWith
            s
            "both cards were exiled and the library is empty"
            (exiledNames S.alice after, namesIn Zone.Library S.alice after)
            (List.sort [named "Goblin Piker", named "Bird Maiden"], [])
          Spec.assertEqWith s "and both carry the permission" (permissionsIn S.alice after) [True, True]
          Spec.assertEqWith s "the game has no result: an empty library is not itself a loss" (GameState.result after) Nothing
        -- The STATIC-ANALYSIS half, planted rather than read off a card, because no
        -- printing puts a TARGET slot in a library's depth and the gameplay cases
        -- above pass whatever these two answer. Both are the seam a nested Quantity
        -- slips through: objectRefSlots and readsX reach it only via
        -- objectRefQuantities, and Effect.Reveal is the cheapest of the
        -- ObjectRef-taking opcodes to plant it under. A damage clause's ref is the
        -- same seam and answers the same way, which the last assertion pins:
        -- CR 120.1a admits no card in a library as a damage recipient, so nothing
        -- printed can reach it and only a planted effect can.
        Spec.it s "CR 603.3b a depth nested in an ObjectRef is a slot read and an X read" $ do
          let slot = SlotName.MkSlotName (Text.pack "victim")
              -- A slotless reveal: the planted Quantity is in the ref's depth,
              -- and the bind slot is a separate position this case says nothing
              -- about.
              depthOf q = Effect.Reveal (Reveal.MkReveal (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) q)) Nothing)
          -- SlotArity.Amount and not One: the depth reads the slot's amount
          -- rather than an object, so the entry is a read with no arity claim on
          -- it (Pawl.Engine.Resolve.Slots.quantitySlots).
          Spec.assertEqWith
            s
            "a depth naming a slot is reported, so the CR 603.3b dataflow lint sees it"
            (Resolve.slotsOf (depthOf (Quantity.InSlot slot)))
            (Map.singleton slot SlotArity.Amount)
          Spec.assertEqWith
            s
            "and a literal depth names none, so the report is the depth's and not the arm's"
            (Resolve.slotsOf (depthOf (Quantity.Literal 3)))
            Map.empty
          Spec.assertEqWith
            s
            "CR 107.3: a depth reading X makes the effect an X reader"
            (Resolve.readsX [depthOf (Quantity.InSlot Binding.variableX)], Resolve.readsX [depthOf (Quantity.Literal 3)])
            (True, False)
          -- The third reader of the same seam, and the one CR 603.3b's elision
          -- rests on: a PlayerRef nested in the depth is a TARGET slot that
          -- QuantitySlot.slots leaves to this module, so the effect must stop claiming
          -- its reads are fully stated. A LifeTotal over a slot rather than a bare
          -- Quantity.InSlot, since that arm is slotless-exhaustive on its own.
          Spec.assertEqWith
            s
            "CR 603.3b: a depth hiding a target slot is not exhaustively reported"
            ( Resolve.slotsAreExhaustive (depthOf (Quantity.LifeTotal (PlayerRef.InSlot slot))),
              Resolve.slotsAreExhaustive (depthOf (Quantity.Literal 3))
            )
            (False, True)
          -- The same three answers off Effect.DealDamage, whose clause carries a
          -- ref of its own: its amount is a Literal throughout, so every answer
          -- here is the REF's depth and not the clause's amount.
          let damageDepthOf q =
                Effect.DealDamage
                  ( DealDamage.MkDealDamage
                      (Seq.singleton (DamagePart.MkDamagePart (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) q)) (Quantity.Literal 1)))
                      Nothing
                      Nothing
                  )
          Spec.assertEqWith
            s
            "a damage clause's own ref reports its depth's slot, X read and exhaustiveness alike"
            ( Resolve.slotsOf (damageDepthOf (Quantity.InSlot slot)),
              Resolve.readsX [damageDepthOf (Quantity.InSlot Binding.variableX)],
              Resolve.slotsAreExhaustive (damageDepthOf (Quantity.LifeTotal (PlayerRef.InSlot slot)))
            )
            (Map.singleton slot SlotArity.Amount, True, False)
          -- And the same three off CR 122.5's GIVER, which is the seam's newest
          -- reader: `from` was a bare SlotName until it was widened to a whole
          -- ObjectRef, and two of the three arms above kept compiling while
          -- reading only the moved kinds' count -- so a depth nested in the giver
          -- was invisible to both (#2729). The moved kinds are EveryOfKind, which
          -- writes no count of its own, so every answer here is the ref's depth.
          -- The destination reports SlotArity.Many beside it, both sides being
          -- ObjectRefs since the second one was widened to a group too.
          let giverDepthOf q =
                Effect.MoveCounters
                  ( MoveCounters.MkMoveCounters
                      (ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary (PlayerRef.Relative PlayerRelation.You) q))
                      (MovedKinds.EveryOfKind CounterKind.PlusOnePlusOne)
                      Nothing
                      (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "recipient")))
                  )
          Spec.assertEqWith
            s
            "a counter move's giver reports its depth's slot, X read and exhaustiveness alike"
            ( Resolve.slotsOf (giverDepthOf (Quantity.InSlot slot)),
              Resolve.readsX [giverDepthOf (Quantity.InSlot Binding.variableX)],
              Resolve.slotsAreExhaustive (giverDepthOf (Quantity.LifeTotal (PlayerRef.InSlot slot)))
            )
            (Map.fromList [(slot, SlotArity.Amount), (SlotName.MkSlotName (Text.pack "recipient"), SlotArity.Many)], True, False)
          Spec.assertEqWith
            s
            "and a literal depth reads no X and states its slots whole, so the answers are the depth's"
            ( Resolve.readsX [giverDepthOf (Quantity.Literal 3)],
              Resolve.slotsAreExhaustive (giverDepthOf (Quantity.Literal 3))
            )
            (False, True)

-- alice is mid-combat with one creature per printing in `mine`, bob defends with
-- one per printing in `theirs`, and alice holds a Trumpet Blast plus exactly the
-- three Mountains that pay for it. The board sits at the declare attackers step
-- like every combatBoardOf board, so the ENGINE declares the attack and the
-- combat record every test below reads is its own, never hand-written.
-- S.addPermanent is what puts the Mountains out: the "any printing, on the
-- battlefield, untapped and Settled" helper its haddock says it is.
trumpetBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
trumpetBoard mountain trumpetBlast mine theirs =
  let (gs0, ours, yours) = S.combatBoardOf mine theirs
      withLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.alice g)) gs0 [1 :: Int .. 3]
      (withCard, _) = S.handOne trumpetBlast withLands
   in ( -- handOne parks its state in a precombat main phase; this board is
        -- mid-combat.
        withCard
          { GameState.phase = GameState.phase gs0,
            GameState.priority = GameState.priority gs0
          },
        ours,
        yours
      )

-- Attack with everything, cast whenever a cast is offered, and never block.
-- Blocks are DECLINED so the attacker survives into the postcombat main phase,
-- which is where the "the set does not shrink either" leg reads it.
attackAndCast :: Prompt.Prompt r -> r
attackAndCast p = case p of
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- Run whole steps until `step` is the current phase, WITHOUT running it. Bounded
-- so a bug cannot loop forever.
runToStep :: Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToStep step answer gs0 =
  let go n g =
        if n <= (0 :: Int) || GameState.phase g == step
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 8 gs0

-- Every stored continuous effect's affected set. CR 611.2c is a claim about
-- exactly this field, so the tests below read it directly as well as through the
-- projection: a filter stored here and re-evaluated would pass a naive
-- power-is-4 assertion.
affectedSets :: GameState.GameState -> [Affected.Affected]
affectedSets = fmap ContinuousEffect.affected . GameState.continuousEffects

-- The attacking creatures, by id, in the engine's own combat record.
attackerIds :: GameState.GameState -> [ObjectId.ObjectId]
attackerIds = Map.keys . Combat.Type.attackers . GameState.combat

-- ObjectRef.AnyNumberMatching under Effect.MoveToZone: the CR 608.2d subset a
-- MOVE names, where Tovolar's is the subset a CR 701.27a turn names.
--
-- Glorious Protector {2}{W}{W} Creature -- Angel Cleric 3/4 (flash, flying,
-- foretell {2}{W}) is the producer: "when this creature enters, you may exile any
-- number of non-Angel creatures you control until this creature leaves the
-- battlefield".
--
-- TWO things are new here. The gather is one. The other is CR 610.3's duration:
-- the printed "until" makes this one one-shot effect with an end, so the exile is
-- declined outright once the Protector has gone (CR 610.3b) and the return is a
-- one-shot effect rather than an ability anybody can respond to. NOT the printed
-- pair Savior of Ollenbock has (Pawl.KeywordTriggerSpec) -- that card really does
-- print a second triggered ability, and its Oracle text says so.
--
-- The board makes every conjunct of the ref's Filter load-bearing, and each is a
-- different way a wrong gather would over-reach: three non-Angel creatures alice
-- controls are the candidates, of which the chooser names ONE, so "the chosen
-- ones" and "all of them" differ; alice's Angel of Finality is a creature she
-- controls that the printed "non-Angel" excludes; bob's Hill Giant is a non-Angel
-- creature that "you control" excludes; and the Protector itself is an Angel, so
-- the card never sweeps itself up without needing a printed "other".
gloriousProtectorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gloriousProtectorSpec s registry =
  let named = CardName.MkCardName . Text.pack
      -- The names a seat CONTROLS, which is the question CR 110.2a answers for an
      -- arrival and Game.zoneMembers -- indexed by OWNER (CR 108.3) -- cannot.
      controlledNames pid gs =
        List.sort
          ( fmap
              (\oid -> fmap S.nameOf (Game.cardOf oid gs))
              (filter (\oid -> Projection.controllerOf oid gs == Just pid) (Set.toList (GameState.battlefield gs)))
          )
      exiledNames gs = List.sort (namesIn Zone.Exile S.alice gs <> namesIn Zone.Exile S.bob gs)
      -- alice: four Plains, three non-Angel creatures, an Angel, and the Protector
      -- in hand. bob: one non-Angel creature. Nothing is tapped and nothing has
      -- entered this way, so the only trigger any leg places is the Protector's.
      board protector plains piker maiden sentry angel giant =
        let g0 = List.foldl' (\g _ -> snd (S.addPermanent plains S.alice g)) (Setup.emptyGame S.bothPlayers) [1 :: Int .. 4]
            (_, g1) = S.addPermanent piker S.alice g0
            (_, g2) = S.addPermanent maiden S.alice g1
            (_, g3) = S.addPermanent sentry S.alice g2
            (_, g4) = S.addPermanent angel S.alice g3
            (_, g5) = S.addPermanent giant S.bob g4
         in S.handOne protector g5
      -- Cast the Protector and run the priority loop out, so its enters trigger is
      -- placed and resolved under the same answerer.
      cast :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> GameState.GameState
      cast answer (withSpell, spell) =
        let afterCast = S.runPure answer withSpell (S.cast S.alice spell)
         in S.runPure answer afterCast Engine.priorityLoop
      -- Accepts the printed "may" -- without which no leg below reaches the
      -- gather at all, CR 603.5's default being to decline.
      accepting :: Prompt.Prompt r -> r
      accepting p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- Accepts the may and names EXACTLY these permanents, offered or not: the
      -- fixture has to say what the chooser said, since a fallback that happened
      -- to agree would prove nothing.
      namingExactly :: Set.Set ObjectId.ObjectId -> Prompt.Prompt r -> r
      namingExactly wanted p = case p of
        Prompt.ChooseAnyNumberOfPermanents {} -> wanted
        _ -> accepting p
      -- Was the subset actually put to a player? Read off the recorded responses
      -- rather than off the board, so "nobody was asked" and "asked and answered
      -- with nothing" stay distinguishable.
      wasAsked responses =
        let isChoice r = case r of
              Response.ChoseAnyNumberOfPermanents _ -> True
              _ -> False
         in any isChoice responses
      recording :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, [Response.Response])
      recording answer (withSpell, spell) =
        let ((_, afterCast), castResponses) = Replay.record answer withSpell (S.cast S.alice spell)
            ((_, after), loopResponses) = Replay.record answer afterCast Engine.priorityLoop
         in (after, castResponses <> loopResponses)
      printings = do
        protector <- S.printingOf s registry "Glorious Protector"
        plains <- S.printingOf s registry "Plains"
        piker <- S.printingOf s registry "Goblin Piker"
        maiden <- S.printingOf s registry "Bird Maiden"
        sentry <- S.printingOf s registry "Ogre Sentry"
        angel <- S.printingOf s registry "Angel of Finality"
        giant <- S.printingOf s registry "Hill Giant"
        pure (board protector plains piker maiden sentry angel giant)
      -- The Bird Maiden on alice's battlefield, which is the one permanent every
      -- leg below names or declines to name.
      maidenId = permanentNamed "Bird Maiden"
      permanentNamed name gs =
        List.find
          (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (named name))
          (Set.toList (GameState.battlefield gs))
   in Spec.describe s "GloriousProtector" $ do
        -- The empty answer CR 608.2d admits, which is a different thing from the
        -- may being declined below: the player WAS asked and named nobody.
        Spec.it s "CR 608.2d naming nobody is a legal answer, and the player was still asked" $ do
          staged <- printings
          let (after, responses) = recording (namingExactly Set.empty) staged
          Spec.assertBool s (wasAsked responses) "the subset was put to the controller"
          Spec.assertEqWith s "and nothing left the battlefield" (exiledNames after) []
        -- CR 603.5's printed "may", declined: no exile, and no subset question --
        -- the clause's instructions never run, so the gather is never reached.
        Spec.it s "CR 603.5 declining the may exiles nothing and asks no subset" $ do
          staged <- printings
          let (after, responses) = recording S.identityAnswer staged
          Spec.assertBool s (not (wasAsked responses)) "no subset was put to the controller"
          Spec.assertEqWith s "exile is empty" (exiledNames after) []
          Spec.assertEqWith
            s
            "and every creature alice controls is still hers"
            (controlledNames S.alice after)
            (List.sort (fmap (Just . named) ["Angel of Finality", "Bird Maiden", "Glorious Protector", "Goblin Piker", "Ogre Sentry", "Plains", "Plains", "Plains", "Plains"]))
        -- The same board with the one thing changed: the Protector PHASES OUT
        -- instead of dying. CR 702.26d makes that no zone change, so CR 610.3's
        -- specified event has not happened and the creature stays in exile --
        -- which is why the watch asks the source's zone rather than its membership
        -- of the battlefield set, the one question rule 702.26b makes answer
        -- differently.
        Spec.it s "CR 702.26d phasing the source out is not leaving, so nothing returns" $ do
          staged <- printings
          case maidenId (fst staged) of
            Nothing -> Spec.assertFailure s "fixture should give alice a Bird Maiden"
            Just maiden -> do
              let exiled = cast (namingExactly (Set.singleton maiden)) staged
              case permanentNamed "Glorious Protector" exiled of
                Nothing -> Spec.assertFailure s "the Protector should be on the battlefield"
                Just oid -> do
                  let phased = Phasing.phaseOut (PhasedOut.Directly S.alice) oid exiled
                      settled = S.runPure S.identityAnswer phased Engine.settleForPriority
                  Spec.assertEqWith s "the Bird Maiden is still in exile" (exiledNames settled) [Just (named "Bird Maiden")]
                  Spec.assertEqWith s "and the watch still stands" (Map.size (GameState.movedUntil settled)) 1

-- The Oblivion Ring family's opponent-facing half, which Glorious Protector above
-- structurally cannot reach: its gather is "creatures YOU control", so every card
-- it exiles is one alice both owns and controls, and CR 610.3c -- "an object
-- returned to the battlefield this way returns under its owner's control" -- says
-- nothing observable on that board.
--
-- Banisher Priest {1}{W}{W} Creature -- Human Cleric 2/2 is the producer: "when
-- this creature enters, exile target creature an opponent controls until this
-- creature leaves the battlefield". It is the corpus's first TARGETED move
-- carrying a CR 610.3 duration, and the first whose victim belongs to another
-- seat.
--
-- Three seats, because "an opponent" and "the other player" are one player on
-- two: bob's Hill Giant is the target, and carol's Goblin Piker is a creature an
-- opponent controls that was not named, so "the target" and "every opponent's
-- creature" differ. Two candidates also make the announcement a real choice
-- rather than a short-circuit.
--
-- CR 610.3c is a REGRESSION FENCE here and not a proof: the return runs through
-- the zone-change door that names no controller, so CR 110.2a's default makes it
-- the owner by construction and no mutation of Pawl.Engine.MoveDuration can spell
-- the other reading -- the watch remembers the source and the zone, and the
-- exiling ability's controller is not recorded. What the leg does separate is
-- "returns to whoever exiled it", which is what the OUTBOUND move does (Resolve's
-- MoveToZone arm hands the funnel `Just controller`): an engine reusing that on
-- the way back puts the Giant on alice's battlefield.
banisherPriestSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
banisherPriestSpec s registry =
  let named = CardName.MkCardName . Text.pack
      -- The names a seat CONTROLS (CR 110.2a), which Game.zoneMembers -- indexed
      -- by OWNER (CR 108.3) -- cannot answer.
      controlledNames pid gs =
        List.sort
          ( fmap
              (\oid -> fmap S.nameOf (Game.cardOf oid gs))
              (filter (\oid -> Projection.controllerOf oid gs == Just pid) (Set.toList (GameState.battlefield gs)))
          )
      exiledNames gs = List.sort (concatMap (\pid -> namesIn Zone.Exile pid gs) [S.alice, S.bob, S.carol])
      permanentNamed name gs =
        List.find
          (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (named name))
          (Set.toList (GameState.battlefield gs))
      -- alice: three Plains and the Priest in hand. bob: a Hill Giant. carol: a
      -- Goblin Piker. Nothing else triggers.
      board priest plains giant piker =
        let g0 = S.landsFor plains S.alice 3 S.threePlayerGame
            (_, g1) = S.addPermanent giant S.bob g0
            (_, g2) = S.addPermanent piker S.carol g1
         in S.handOne priest g2
      -- Name bob's Giant out of the OFFERED set rather than building a recipient:
      -- CR 608.2b re-reads the announcement at resolution, and a hand-built
      -- Recipient.ToObject of the same permanent is a different recipient from the
      -- ToCreature a Creatures pool offers.
      targeting :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      targeting oid p = case p of
        Prompt.ChooseTargets _ _ _ sets ->
          fmap (\(_, legal) -> Set.filter ((== Just oid) . Recipient.objectOf) legal) sets
        _ -> S.identityAnswer p
      cast :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> GameState.GameState
      cast answer (withSpell, spell) =
        let afterCast = S.runPure answer withSpell (S.cast S.alice spell)
         in S.runPure answer afterCast Engine.priorityLoop
      printings = do
        priest <- S.printingOf s registry "Banisher Priest"
        plains <- S.printingOf s registry "Plains"
        giant <- S.printingOf s registry "Hill Giant"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (board priest plains giant piker)
      -- Cast the Priest naming bob's Giant, and hand back the board with the
      -- Giant in exile and the Priest still alive.
      exiledBoard = do
        staged <- printings
        pure (fmap (\giant -> cast (targeting giant) staged) (permanentNamed "Hill Giant" (fst staged)))
   in Spec.describe s "BanisherPriest" $ do
        -- CR 603.3d: the trigger's announcement named ONE of the two creatures
        -- alice's opponents control, and the duration holds it there while the
        -- Priest lives.
        Spec.it s "CR 610.3 only the named creature is exiled, and it stays there while the Priest lives" $ do
          mExiled <- exiledBoard
          case mExiled of
            Nothing -> Spec.assertFailure s "fixture should give bob a Hill Giant"
            Just exiled -> do
              Spec.assertEqWith s "bob's Giant, and only it, is in exile" (exiledNames exiled) [Just (named "Hill Giant")]
              Spec.assertEqWith s "and carol's Piker, an opponent's creature that was not named, stays put" (controlledNames S.carol exiled) [Just (named "Goblin Piker")]
        -- The headline: CR 610.3c's "under its owner's control", read on the one
        -- board Glorious Protector cannot build.
        Spec.it s "CR 610.3c the Priest's death returns the Giant to bob, its owner, and not to alice who exiled it" $ do
          mExiled <- exiledBoard
          case mExiled >>= (\exiled -> fmap ((,) exiled) (permanentNamed "Banisher Priest" exiled)) of
            Nothing -> Spec.assertFailure s "the Priest should be on the battlefield with the Giant exiled"
            Just (exiled, priest) -> do
              let killed = S.runPure S.identityAnswer exiled (Event.destroy Regenerability.Regenerable [priest])
                  after = S.runPure S.identityAnswer killed Engine.priorityLoop
              Spec.assertEqWith s "the Giant is back on the battlefield under bob's control" (controlledNames S.bob after) [Just (named "Hill Giant")]
              Spec.assertEqWith s "and alice, who exiled it, controls only her three Plains" (controlledNames S.alice after) (List.sort (fmap (Just . named) ["Plains", "Plains", "Plains"]))

-- ObjectRef.EachCardInYourLibrary under Effect.MoveToZone: CR 400.12's
-- whole-zone instruction over CR 400.1's other hidden per-player zone, where
-- Ignorant Bliss' EachCardInYourHand takes the first.
--
-- Leveler {5} Artifact Creature -- Juggernaut 10/10 is the producer: "when this
-- creature enters, exile all cards from your library". One ability, one clause,
-- no shuffle and no reveal, so the sweep is the whole of what it does.
--
-- NOT A SEARCH, which is the rules question the arm rests on. CR 701.23a's
-- search FINDS a card matching a description; Leveler states no description and
-- offers no choice, so CR 701.23b's "isn't required to find" has nothing to
-- reach and CR 701.23f's search triggers do not fire. Nor does anything shuffle:
-- CR 701.24 gives a merely-swept library no shuffle. The third leg below reads
-- the recorded responses for exactly those two, since a board cannot otherwise
-- tell "swept" from "searched and found everything".
--
-- The board makes each reading of the sweep distinguishable: alice's library
-- holds THREE distinct cards, so "all of them", "one of them" and "none of them"
-- are three different exiles; bob's library holds two more, so "your library"
-- and "each library" differ; and a Plains sits in alice's hand, so the other
-- hidden zone would show if the arm reached the wrong one. CR 704.5b is the
-- consequence Leveler is printed for, and the last pair below is the same draw
-- on two boards differing only in whether the Leveler was cast.
levelerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
levelerSpec s registry =
  let named = CardName.MkCardName . Text.pack
      sortedNames zone pid gs = List.sort (namesIn zone pid gs)
      -- alice: five Plains to pay {5}, a three-card library, a Plains in hand and
      -- the Leveler in hand. bob: a two-card library and nothing else, so the
      -- only trigger any leg places is the Leveler's.
      board leveler plains piker maiden sentry giant angel =
        let g0 = S.landsFor plains S.alice 5 (Setup.emptyGame S.bothPlayers)
            (_, g1) = S.addLibraryCard piker S.alice g0
            (_, g2) = S.addLibraryCard maiden S.alice g1
            (_, g3) = S.addLibraryCard sentry S.alice g2
            (_, g4) = S.addLibraryCard giant S.bob g3
            (_, g5) = S.addLibraryCard angel S.bob g4
            -- handOne REPLACES alice's hand, so the Plains that proves the sweep
            -- found the library rather than the other hidden zone goes in after.
            (g6, spell) = S.handOne leveler g5
            (_, g7) = S.addHandCard plains S.alice g6
         in (g7, spell)
      -- Cast the Leveler and run the priority loop out, so its enters trigger is
      -- placed and resolved.
      cast (withSpell, spell) =
        let afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
         in S.runPure S.identityAnswer afterCast Engine.priorityLoop
      printings = do
        leveler <- S.printingOf s registry "Leveler"
        plains <- S.printingOf s registry "Plains"
        piker <- S.printingOf s registry "Goblin Piker"
        maiden <- S.printingOf s registry "Bird Maiden"
        sentry <- S.printingOf s registry "Ogre Sentry"
        giant <- S.printingOf s registry "Hill Giant"
        angel <- S.printingOf s registry "Angel of Finality"
        pure (board leveler plains piker maiden sentry giant angel)
   in Spec.describe s "Leveler" $ do
        -- CR 109.5's "you" is one seat: bob's library is not swept and his exile
        -- stays empty, which is what parts "your library" from "each library".
        Spec.it s "CR 109.5 no other player's library is touched" $ do
          staged <- printings
          let after = cast staged
          Spec.assertEqWith
            s
            "bob's library still holds both his cards"
            (sortedNames Zone.Library S.bob after)
            (List.sort (fmap (Just . named) ["Angel of Finality", "Hill Giant"]))
          Spec.assertEqWith s "and nothing of his is in exile" (sortedNames Zone.Exile S.bob after) []
        -- CR 701.23 and CR 701.24, read off the recorded responses: the sweep asks
        -- nobody to find anything and shuffles nothing. A board cannot show this,
        -- because a search that found every card would leave the same exile.
        Spec.it s "CR 701.23a a sweep is not a search, and CR 701.24 shuffles nothing" $ do
          staged <- printings
          let (withSpell, spell) = staged
              ((_, afterCast), castResponses) = Replay.record S.identityAnswer withSpell (S.cast S.alice spell)
              ((_, after), loopResponses) = Replay.record S.identityAnswer afterCast Engine.priorityLoop
              responses = castResponses <> loopResponses
              searched r = case r of
                Response.Searched _ -> True
                Response.ChoseSearchZones _ -> True
                _ -> False
              shuffled r = case r of
                Response.Shuffled _ -> True
                _ -> False
          -- The two negatives are over a REAL response log, not an empty one:
          -- the cast and the loop both recorded, so "no search" is a statement
          -- about what this resolution asked rather than about a recorder that
          -- never ran.
          Spec.assertBool s (not (null responses)) "the run recorded responses"
          Spec.assertBool s (not (any searched responses)) "no search was put to a player"
          Spec.assertBool s (not (any shuffled responses)) "and no library was shuffled"
          -- Anti-vacuity: the sweep really did run under the same answerer, so
          -- the two negatives above are about a resolved Leveler.
          Spec.assertEqWith s "and the sweep still happened" (sortedNames Zone.Library S.alice after) []
        -- The paired boards, differing in exactly one thing: whether the Leveler
        -- was cast. CR 704.5b then takes alice out on the same draw that leaves
        -- her playing on the board where her library survived.
        Spec.it s "CR 704.5b drawing from the swept library loses the game" $ do
          staged <- printings
          let emptied = cast staged
              intact = fst staged
              drawn gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (Event.drawCard S.alice)) Sba.checkStateBasedActions
          Spec.assertEqWith s "she is still playing the moment the sweep finishes" (statusOf S.alice emptied) (Just Status.Playing)
          Spec.assertEqWith s "and drawing from the emptied library loses her the game" (statusOf S.alice (drawn emptied)) (Just (Status.Departed Departure.Type.Lost))
          Spec.assertEqWith s "where the same draw on the board she never cast it on leaves her playing" (statusOf S.alice (drawn intact)) (Just Status.Playing)
          Spec.assertEqWith
            s
            "which is the anti-vacuity: that board's library is the one the sweep would have taken"
            (sortedNames Zone.Library S.alice intact)
            (List.sort (fmap (Just . named) ["Bird Maiden", "Goblin Piker", "Ogre Sentry"]))

-- Aim Caldera Breaker's reflexive at anything BUT bob's two creatures, falling
-- back to the Angel when -- as the card's own words require -- there is nothing
-- else. FILTERED rather than replaced, so a pool that wrongly offered alice's own
-- Breaker or one of bob's Mountains would take the damage and leave the Angel at
-- zero: "an opponent controls" and "creature or planeswalker" are read rather
-- than assumed.
calderaBreakerAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
calderaBreakerAnswer angel giant p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    fmap
      ( \(_, legal) ->
          let others = Set.filter (\r -> Recipient.objectOf r /= Just angel && Recipient.objectOf r /= Just giant) legal
           in if Set.null others then Set.filter ((== Just angel) . Recipient.objectOf) legal else others
      )
      sets
  _ -> S.identityAnswer p

-- ObjectRef.EachCardInYourLibrary with a STATED filter, the sweep levelerSpec
-- above takes bare: CR 109.2a's "card" beside the name of a zone, where the bare
-- form is CR 400.12's instruction to the zone itself.
--
-- Caldera Breaker {3}{R}{R}{R} Artifact Creature -- Golem 6/6 (Alchemy: Ixalan,
-- Oracle text fetched from Scryfall 2026-09-02) is the producer: "When Caldera
-- Breaker enters, exile all Mountain cards from your library. When you do,
-- Caldera Breaker deals that much damage to target creature or planeswalker an
-- opponent controls."
--
-- NOT A SEARCH, which is the rules question this arm rests on and which a stated
-- characteristic does not change. CR 701.23a's search LOOKS AT a zone and FINDS
-- cards matching a description; this text says neither word, so CR 701.23b's
-- "isn't required to find" governs nobody here -- that rule is about a player who
-- is searching -- CR 701.23f's search triggers do not fire, and CR 701.24
-- shuffles nothing.
--
-- The board tells the readings apart. alice's library INTERLEAVES three Mountains
-- with three distinct nonland cards, so "the matches", "the whole zone" and "a
-- prefix" are three different exiles and the survivors' ORDER is readable -- which
-- is what a shuffle would destroy. A Mountain sits in her hand and two more in
-- bob's library, so a sweep that reached the other hidden zone or another seat
-- would show. bob controls two creatures, so the reflexive's target slot has more
-- candidates than its count and cannot short-circuit.
--
-- "That much" is CR 400.7j: exile is a public zone (CR 400.2), so a later part of
-- the same effect can find the cards it put there, and the count is read off the
-- slot the move bound rather than off the library that no longer holds them.
calderaBreakerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
calderaBreakerSpec s registry =
  let named = CardName.MkCardName . Text.pack
      sortedNames zone pid gs = List.sort (namesIn zone pid gs)
      -- alice: six Mountains to pay {3}{R}{R}{R}, a six-card library alternating
      -- Mountain and not, a Mountain in hand and the Breaker in hand. bob: two
      -- creatures and two Mountains in his own library.
      board breaker mountain piker maiden sentry angel giant =
        let g0 = S.landsFor mountain S.alice 6 (Setup.emptyGame S.bothPlayers)
            -- addLibraryCard puts each card on TOP, so the library reads
            -- Mountain, Piker, Mountain, Maiden, Mountain, Sentry from the top.
            (_, g1) = S.addLibraryCard sentry S.alice g0
            (_, g2) = S.addLibraryCard mountain S.alice g1
            (_, g3) = S.addLibraryCard maiden S.alice g2
            (_, g4) = S.addLibraryCard mountain S.alice g3
            (_, g5) = S.addLibraryCard piker S.alice g4
            (_, g6) = S.addLibraryCard mountain S.alice g5
            (_, g7) = S.addLibraryCard mountain S.bob g6
            (_, g8) = S.addLibraryCard mountain S.bob g7
            (angelId, g9) = S.addPermanent angel S.bob g8
            (giantId, g10) = S.addPermanent giant S.bob g9
            -- handOne REPLACES alice's hand, so the Mountain that proves the
            -- sweep found the library rather than the other hidden zone goes in
            -- after it.
            (g11, spell) = S.handOne breaker g10
            (_, g12) = S.addHandCard mountain S.alice g11
         in (g12, spell, angelId, giantId)
      cast (withSpell, spell, angel, giant) =
        let afterCast = S.runPure (calderaBreakerAnswer angel giant) withSpell (S.cast S.alice spell)
         in S.runPure (calderaBreakerAnswer angel giant) afterCast Engine.priorityLoop
      printings = do
        breaker <- S.printingOf s registry "Caldera Breaker"
        mountain <- S.printingOf s registry "Mountain"
        piker <- S.printingOf s registry "Goblin Piker"
        maiden <- S.printingOf s registry "Bird Maiden"
        sentry <- S.printingOf s registry "Ogre Sentry"
        angel <- S.printingOf s registry "Angel of Finality"
        giant <- S.printingOf s registry "Hill Giant"
        pure (board breaker mountain piker maiden sentry angel giant)
   in Spec.describe s "Caldera Breaker" $ do
        -- The headline, gameplay-level first: the three Mountains leave and the
        -- three nonland cards stay, IN THE ORDER THEY WERE IN.
        Spec.it s "CR 109.2a only the matching cards leave the library, in place" $ do
          staged <- printings
          let after = cast staged
          Spec.assertEqWith
            s
            "the three cards left in alice's library are the nonland ones, top to bottom"
            (namesIn Zone.Library S.alice after)
            (fmap (Just . named) ["Goblin Piker", "Bird Maiden", "Ogre Sentry"])
          Spec.assertEqWith
            s
            "and exactly the three Mountains that were in it are in exile"
            (sortedNames Zone.Exile S.alice after)
            (replicate 3 (Just (named "Mountain")))
          Spec.assertEqWith s "the Mountain in her hand is untouched, so the sweep found the library and not the other hidden zone" (sortedNames Zone.Hand S.alice after) [Just (named "Mountain")]
        -- CR 109.5's "you" is one seat, and the filter is not a licence to reach
        -- another library that also holds matches.
        Spec.it s "CR 109.5 no other player's library is touched" $ do
          staged <- printings
          let after = cast staged
          Spec.assertEqWith
            s
            "bob's library still holds both his Mountains"
            (sortedNames Zone.Library S.bob after)
            (replicate 2 (Just (named "Mountain")))
          Spec.assertEqWith s "and nothing of his is in exile" (sortedNames Zone.Exile S.bob after) []
        -- CR 603.12 and CR 400.7j: the reflexive fires once and reads "that much"
        -- off the group the move bound, not off the library it emptied of matches.
        Spec.it s "CR 603.12 / 400.7j the reflexive deals damage equal to the cards exiled" $ do
          staged <- printings
          let (_, _, angel, giant) = staged
              after = cast staged
          Spec.assertEqWith s "the targeted creature took three, one per exiled Mountain" (S.damageOf angel after) (Just 3)
          Spec.assertEqWith s "and the other candidate took none, so the damage went to one target" (S.damageOf giant after) (Just 0)
          Spec.assertEqWith s "which is the anti-vacuity: three Mountains really were exiled" (sortedNames Zone.Exile S.alice after) (replicate 3 (Just (named "Mountain")))
        -- CR 701.23 and CR 701.24, read off the recorded responses: a sweep that
        -- states a characteristic still asks nobody to find anything and shuffles
        -- nothing. A board cannot show this on its own, because a search that
        -- found every Mountain would leave the same exile.
        Spec.it s "CR 701.23a a filtered sweep is not a search, and CR 701.24 shuffles nothing" $ do
          staged <- printings
          let (withSpell, spell, angel, giant) = staged
              ((_, afterCast), castResponses) = Replay.record (calderaBreakerAnswer angel giant) withSpell (S.cast S.alice spell)
              ((_, after), loopResponses) = Replay.record (calderaBreakerAnswer angel giant) afterCast Engine.priorityLoop
              responses = castResponses <> loopResponses
              searched r = case r of
                Response.Searched _ -> True
                Response.ChoseSearchZones _ -> True
                _ -> False
              shuffled r = case r of
                Response.Shuffled _ -> True
                _ -> False
          Spec.assertBool s (not (null responses)) "the run recorded responses"
          Spec.assertBool s (not (any searched responses)) "no search was put to a player"
          Spec.assertBool s (not (any shuffled responses)) "and no library was shuffled"
          Spec.assertEqWith
            s
            "and the survivors kept the order a shuffle would have destroyed"
            (namesIn Zone.Library S.alice after)
            (fmap (Just . named) ["Goblin Piker", "Bird Maiden", "Ogre Sentry"])

-- Trumpet Blast ({2}{R} instant, "Attacking creatures get +2/+0 until end of
-- turn") is the pool's first card whose CONTINUOUS effect names a filter-selected
-- set rather than a target. Day of Judgment's EachMatching feeds a ONE-SHOT, so
-- CR 608.2c/608.2f are the whole of its story; this one is stored and keeps
-- applying, which puts it under CR 611.2c as well:
--
--   "If a continuous effect generated by the resolution of a spell or ability
--   modifies the characteristics or changes the controller of any objects, the
--   set of objects it affects is determined when that continuous effect begins.
--   After that point, the set won't change."
--
-- So the sweep happens ONCE, at resolution, and its RESULT is frozen into the
-- stored effect as Affected.TheseObjects. The three legs below are the ones a
-- stored-and-re-evaluated Filter would fail: it would pump a creature that
-- became attacking later, and drop the pump from one that left combat.
--
-- The modification is layer 7c (CR 613.4c: "effects and counters that modify
-- power and/or toughness"), the same layer Giant Growth's already lands in --
-- what is new here is the affected set, not the modification.
trumpetBlastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trumpetBlastSpec s registry = Spec.describe s "TrumpetBlast" $ do
  -- The structural half of CR 611.2c, read off the stored effect rather than
  -- through the projection: what is stored is an ID SET, not the Filter that
  -- found it. Every behavioural leg below follows from this one field, and an
  -- implementation that stored Affected.Matching would fail here first.
  Spec.it s "CR 611.2c the stored effect holds the swept ids, not the filter that swept them" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    trumpetBlast <- S.printingOf s registry "Trumpet Blast"
    let (board, ours, _) = trumpetBoard mountain trumpetBlast [piker, piker] [piker]
        after = runToStep (Phase.Combat CombatStep.DeclareBlockers) attackAndCast board
    Spec.assertEqWith s "one stored effect, over exactly the two attackers" (affectedSets after) [Affected.TheseObjects (Set.fromList ours)]
  -- CR 611.2c's own sentence, in the direction it is usually quoted: the set
  -- is fixed when the effect BEGINS, so a creature that becomes attacking
  -- afterwards is not in it.
  --
  -- Hanweir Garrison is the pool's producer for "becomes attacking later":
  -- its CR 508.3a attack trigger creates two 1/1 Humans "that are tapped and
  -- attacking". The trigger is put on the stack as attackers are declared,
  -- alice casts Trumpet Blast on top of it, and the spell therefore resolves
  -- FIRST -- so the tokens are minted, already attacking, after the continuous
  -- effect began. They are attacking, which is exactly what makes this
  -- discriminating: a stored Filter re-evaluated each projection would find
  -- them and pump them to 3/1.
  Spec.it s "CR 611.2c a creature that becomes attacking after the spell resolves is not in the set" $ do
    mountain <- S.printingOf s registry "Mountain"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    trumpetBlast <- S.printingOf s registry "Trumpet Blast"
    let (board, ours, _) = trumpetBoard mountain trumpetBlast [garrison] []
        after = runToStep (Phase.Combat CombatStep.DeclareBlockers) attackAndCast board
        tokens = filter (`List.notElem` ours) (attackerIds after)
    Spec.assertEqWith s "the stack is empty: both the spell and the trigger resolved" (length (GameState.stack after)) 0
    Spec.assertEqWith s "the trigger made two tokens" (length tokens) 2
    Spec.assertEqWith s "the Garrison was attacking when the spell resolved, so it is a 4/3" (fmap (`Projection.powerOf` after) ours) (fmap (const (Just 4)) ours)
    Spec.assertEqWith s "the tokens ARE attacking" (length (filter (`List.elem` attackerIds after) tokens)) 2
    Spec.assertEqWith s "and are 1/1 all the same: they were not in the set when it was determined" (fmap (`Projection.powerOf` after) tokens) (fmap (const (Just 1)) tokens)
    Spec.assertEqWith s "the stored set still names only the Garrison" (affectedSets after) [Affected.TheseObjects (Set.fromList ours)]
  -- CR 400.7: "An object that moves from one zone to another becomes a new
  -- object with no memory of, or relation to, its previous existence." A
  -- frozen set is a set of ObjectIds, so the creature that comes back is
  -- simply not in it -- which is the reason CR 611.2c can be implemented as an
  -- id set at all.
  Spec.it s "CR 400.7 a creature that leaves the battlefield and returns is a new object outside the frozen set" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    trumpetBlast <- S.printingOf s registry "Trumpet Blast"
    let (board, ours, _) = trumpetBoard mountain trumpetBlast [piker] []
        after = runToStep (Phase.Combat CombatStep.DeclareBlockers) attackAndCast board
    case ours of
      [attacker] -> do
        let bounced = S.runPure S.identityAnswer after (Event.changeZone attacker Zone.Hand)
            (returned, back) = S.addPermanent piker S.alice bounced
        Spec.assertEqWith s "it was a 4/1 before it left" (Projection.powerOf attacker after) (Just 4)
        Spec.assertBool s (returned /= attacker) "what came back is a different object"
        Spec.assertEqWith s "and it is a plain 2/1" (Projection.powerOf returned back) (Just 2)
        Spec.assertEqWith s "the stored set still names the incarnation that left" (affectedSets back) [Affected.TheseObjects (Set.singleton attacker)]
      _ -> Spec.assertFailure s "fixture should have exactly one attacker"

-- Aura Thief ({3}{U} 2/2 Creature -- Illusion, "Flying / When this creature
-- dies, you gain control of all enchantments") is the CONTROL-side twin of
-- Trumpet Blast, and the other half of what CR 611.2c names: that rule fixes the
-- affected set of a resolution effect that "modifies the characteristics OR
-- CHANGES THE CONTROLLER of any objects". The layer differs (CR 613.1b's layer 2
-- rather than 613.4c's 7c) and the opcode differs, but the freeze is the same
-- one, and these tests are the proof that GainControl performs it too.
--
-- The trigger is a dies trigger, so the whole card runs the way Doomed
-- Traveler's does in Pawl.TriggerSpec: a Lightning Bolt kills the 2/2, CR
-- 704.5g's state-based action puts it in the graveyard, the CR 603.10a look-back
-- trigger reaches the stack in that same settle, and resolving it is what
-- steals the enchantments. Nothing here hand-builds a continuous effect.
--
-- The printed reminder "(You don't get to move Auras.)" is not a rule this
-- opcode has to implement: nothing in GainControl moves an attachment, and CR
-- 701.3 is the only thing that does.
auraThiefSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
auraThiefSpec s registry =
  let -- alice: one Mountain (the Bolt's {R}), an Aura Thief, and a Greed of her
      -- own; bob: a Bad Moon and a Hardened Scales. All four enchantments are
      -- inert on this board -- no black creature, no +1/+1 counter, no activation
      -- -- so the only thing any test here reads off them is who controls them.
      -- S.identityAnswer targets the least Recipient and Recipient.ToCreature
      -- sorts before Recipient.ToPlayer, so the Thief, the only creature on the
      -- board, is the Bolt's target without a bespoke interpreter.
      thiefBoard = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        auraThief <- S.printingOf s registry "Aura Thief"
        greed <- S.printingOf s registry "Greed"
        badMoon <- S.printingOf s registry "Bad Moon"
        hardenedScales <- S.printingOf s registry "Hardened Scales"
        let (thief, g1) = S.addPermanent auraThief S.alice (S.landsInPlay mountain 1)
            (hers, g2) = S.addPermanent greed S.alice g1
            (moon, g3) = S.addPermanent badMoon S.bob g2
            (scales, g4) = S.addPermanent hardenedScales S.bob g3
            (withBolt, spell) = S.handOne lightningBolt g4
        pure (withBolt, spell, thief, [hers], [moon, scales])
      -- Cast the Bolt, resolve it, settle (CR 704.5g destroys the damaged 2/2 and
      -- the same settle places its CR 603.10a look-back trigger), then resolve
      -- the trigger.
      boltIt (gs, spell) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spell)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
   in Spec.describe s "AuraThief" $ do
        -- The structural half of CR 611.2c, on the control side: what is stored
        -- is the swept id set, not the Filter that found it.
        Spec.it s "CR 611.2c the stored control effect holds the swept ids, not the filter that swept them" $ do
          (board, spell, _, hers, theirs) <- thiefBoard
          let (_, after) = boltIt (board, spell)
          Spec.assertEqWith
            s
            "one stored effect, over all three enchantments"
            (affectedSets after)
            [Affected.TheseObjects (Set.fromList (hers <> theirs))]
        -- "After that point, the set won't change." An enchantment that arrives
        -- after the trigger has resolved is not in the set, so its controller
        -- keeps it -- the control-side twin of the Hanweir Garrison tokens.
        Spec.it s "CR 611.2c an enchantment that enters after the trigger resolves is not stolen" $ do
          (board, spell, _, _, theirs) <- thiefBoard
          greed <- S.printingOf s registry "Greed"
          let (_, after) = boltIt (board, spell)
              (latecomer, later) = S.addPermanent greed S.bob after
          Spec.assertEqWith s "the ones that were there are alice's" (fmap (`Projection.controllerOf` later) theirs) (fmap (const (Just S.alice)) theirs)
          Spec.assertEqWith s "the one that arrived afterwards is still bob's" (Projection.controllerOf latecomer later) (Just S.bob)
        -- CR 302.6: "A creature's activated ability with the tap symbol ... in
        -- its activation cost can't be activated unless the creature has been
        -- under its controller's control continuously since their most recent
        -- turn began." Gaining control interrupts that continuity, and gaining
        -- control of something you already control does not -- so the sweep has
        -- to ask per object rather than re-Sicking everything it names.
        Spec.it s "CR 302.6 the newly gained enchantments are re-Sicked and the one alice already controlled is not" $ do
          (board, spell, _, hers, theirs) <- thiefBoard
          let (_, after) = boltIt (board, spell)
              sicknessOf oid = fmap Object.sickness (Game.lookupObject oid after)
          Spec.assertEqWith s "bob's, taken from him, start their clock over" (fmap sicknessOf theirs) (fmap (const (Just Sickness.Sick)) theirs)
          Spec.assertEqWith s "alice's own was never interrupted" (fmap sicknessOf hers) (fmap (const (Just (Sickness.Settled S.alice))) hers)

-- The one battlefield permanent whose card carries this name. Bane's printed
-- incarnation is gone by the time the trigger resolves (CR 400.7), so the test
-- cannot hold the id it was cast under.
namedOnBattlefield :: String -> GameState.GameState -> Maybe ObjectId.ObjectId
namedOnBattlefield name gs =
  List.find
    (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack name))
    (Set.toList (GameState.battlefield gs))

-- The SPELL counterings recorded so far, in stack-sweep order; a countered
-- ability records GameEvent.AbilityCountered and is not in this. The local
-- sibling of Pawl.EventSpec's own: Event.counter is the only funnel that appends
-- one, and what it appends is exactly the set the opcode ACTUALLY countered.
counteredSpells :: GameState.GameState -> [ObjectId.ObjectId]
counteredSpells gs =
  let counteringOf event = case event of
        GameEvent.SpellCountered c -> Just (Countering.countered c)
        _ -> Nothing
   in Maybe.mapMaybe counteringOf (S.eventsOf gs)

-- ONE board for Swift Silence, built once and branched. bob has five untapped
-- lands -- two Plains and three Islands, exactly {2}{W}{U}{U} and no more, so no
-- assertion below can turn on spare mana -- and the Swift Silence in hand.
-- Waiting under it are four objects, and each is on the stack for a reason:
--
--   * alice's Divination and bob's own Goblin Piker are the counterable
--     victims, one per seat, so the sweep is not one player's;
--   * alice's Blurred Mongoose prints "can't be countered" (CR 113.6g), which
--     is what tells the swept set apart from the countered one;
--   * alice's Prodigal Sorcerer's activated {T} is an ABILITY, which CR 113.9
--     says is not a spell -- so CR 109.2b's "all other spells" must leave it
--     alone however the sweep is written.
--
-- Both libraries are stocked, since the rider draws and CR 104.3c would
-- otherwise decide the game before an assertion ran.
--
-- Swift Silence is CAST rather than placed: Support.spellOnStack leaves
-- Object.bindings empty, and CR 601.2b's mode choice is one of the bindings a
-- cast writes, so a placed modal spell resolves into nothing. The victims are
-- placed, since none of them resolves.
--
-- `mongoose` is a Maybe so the twin below can drop the uncounterable spell and
-- change NOTHING else -- same seats, same mana, same victims, same ability, same
-- stack order.
--
-- Nothing where the Sorcerer stopped declaring exactly one activated ability,
-- which is stifleBoard's posture for the same fixture.
--
-- Returns the two counterable victims, the uncounterable one, the ability, the
-- Swift Silence in hand and the board.
swiftSilenceBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Maybe Printing.Printing ->
  Maybe (ObjectId.ObjectId, ObjectId.ObjectId, Maybe ObjectId.ObjectId, [ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
swiftSilenceBoard plains island swiftSilence divination piker sorcerer mMongoose = case soleActivatedAbility sorcerer of
  Nothing -> Nothing
  Just ability ->
    let lands = S.landsFor island S.bob 3 (S.landsFor plains S.bob 2 (Setup.emptyGame S.bothPlayers))
        stock pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard divination pid g)) gs [1 :: Int .. 5]
        stocked = stock S.bob (stock S.alice lands)
        (sorcererId, withSorcerer) = S.addPermanent sorcerer S.alice stocked
        -- CR 302.6: the Sorcerer must have settled under alice before its {T}
        -- may be activated at all.
        settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.alice)
        (hers, withHers) = S.spellOnStack divination S.alice settled
        (his, withHis) = S.spellOnStack piker S.bob withHers
        (mUncounterable, withMongoose) = case mMongoose of
          Nothing -> (Nothing, withHis)
          Just mongoose -> let (oid, g) = S.spellOnStack mongoose S.alice withHis in (Just oid, g)
        onStack = GameState.stack withMongoose
        atAlice :: Prompt.Prompt r -> r
        atAlice p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
          _ -> S.identityAnswer p
        activated = S.runPure atAlice (withMongoose {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice sorcererId ability)
        abilityIds = filter (`notElem` onStack) (GameState.stack activated)
        (silence, board) = S.addHandCard swiftSilence S.bob activated
     in Just (hers, his, mUncounterable, abilityIds, silence, board)

-- bob casts his Swift Silence over the waiting stack and lets it resolve.
swiftSilenceRun :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
swiftSilenceRun silence gs =
  let cast = S.runPure S.identityAnswer gs (S.cast S.bob silence)
   in S.runPure S.identityAnswer cast Stack.resolveTop

-- Swift Silence {2}{W}{U}{U} Instant: "Counter all other spells. Draw a card for
-- each spell countered this way."
--
-- The proving case for #1507: the first opcode to counter a SET rather than a
-- targeted slot. CR 109.2b is what puts the set on the stack -- a description
-- carrying the word "spell" "means a spell matching that description on the
-- stack" -- and CR 115.10a is what keeps it off the target list, so nothing here
-- is announced at CR 601.2c and CR 608.2b has nothing to fizzle.
swiftSilenceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
swiftSilenceSpec s registry = Spec.describe s "SwiftSilence" $ do
  -- Four spells on the stack, and each reading of "all other spells" gives a
  -- different number of cards drawn, so the board tells them apart:
  --
  --   * "one other spell" draws 1;
  --   * "all spells", with the source not excluded, counters Swift Silence too
  --     -- CR 608.2m has it finish resolving anyway -- and draws 3;
  --   * "everything the sweep named" draws 3 as well, since the Mongoose is
  --     named and CR 113.6g keeps it from being countered;
  --   * what was actually countered this way is 2.
  Spec.it s "CR 109.2b/701.6a counters every other spell on the stack and draws for what it countered" $ do
    swiftSilence <- S.printingOf s registry "Swift Silence"
    divination <- S.printingOf s registry "Divination"
    piker <- S.printingOf s registry "Goblin Piker"
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    case swiftSilenceBoard plains island swiftSilence divination piker sorcerer (Just mongoose) of
      Nothing -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      Just (hers, his, mUncounterable, abilityIds, silence, board) -> do
        let resolved = swiftSilenceRun silence board
        Spec.assertEqWith s "the activation put exactly one ability on the stack" (length abilityIds) 1
        Spec.assertEqWith
          s
          "exactly the two counterable spells were countered: `Not IsSource` spared Swift Silence itself, CR 113.6g the Mongoose, and CR 113.9 the ability"
          (List.sort (counteredSpells resolved))
          (List.sort [hers, his])
        Spec.assertEqWith
          s
          "the ability and the uncounterable spell are still on the stack under their original ids, and they are all that is left"
          (GameState.stack resolved)
          (abilityIds <> Maybe.maybeToList mUncounterable)
        -- CR 608.2n: an ability that HAD been countered would have ceased to
        -- exist, so a live object here is the sweep having spared it.
        Spec.assertBool s (all (\oid -> Maybe.isJust (Game.lookupObject oid resolved)) abilityIds) "CR 113.9 the ability object still exists"
        Spec.assertBool s (not (S.onBattlefield his resolved)) "the countered creature spell never became a permanent"
        Spec.assertEqWith s "CR 701.6a alice's countered spell reached her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
        -- Two cards: bob's own countered Piker, and CR 608.2n's Swift Silence,
        -- put there as the last part of its own resolution rather than by any
        -- countering.
        Spec.assertEqWith s "bob's holds his countered spell and the resolved Swift Silence" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 2
        Spec.assertEqWith s "two countered this way, so two cards drawn" (S.handSize S.bob resolved) 2
        Spec.assertEqWith s "and nobody else drew" (S.handSize S.alice resolved) 0
  -- The discriminating twin: the SAME board with the uncounterable spell
  -- removed and nothing else changed. The sweep now names two rather than three
  -- and the draw is unchanged at two, so the two above were the COUNTERED set
  -- and not the swept one.
  Spec.it s "CR 113.6g removing the uncounterable spell leaves the count unchanged" $ do
    swiftSilence <- S.printingOf s registry "Swift Silence"
    divination <- S.printingOf s registry "Divination"
    piker <- S.printingOf s registry "Goblin Piker"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    case swiftSilenceBoard plains island swiftSilence divination piker sorcerer Nothing of
      Nothing -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      Just (hers, his, _, abilityIds, silence, board) -> do
        let resolved = swiftSilenceRun silence board
        Spec.assertEqWith s "still the same two" (List.sort (counteredSpells resolved)) (List.sort [hers, his])
        Spec.assertEqWith s "and only the untouched ability is left on the stack" (GameState.stack resolved) abilityIds
        Spec.assertEqWith s "still two cards drawn" (S.handSize S.bob resolved) 2

-- The Faerie tokens ONE player controls, by name and by CR 108.4's controller
-- rather than by Support.countOnBattlefieldByName, which slices the battlefield
-- by OWNER (CR 108.3) and so cannot say who controls anything.
faerieTokensUnder :: PlayerId.PlayerId -> GameState.GameState -> Int
faerieTokensUnder pid gs =
  length
    ( filter
        ( \oid ->
            fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Faerie Token"))
              && Projection.controllerOf oid gs == Just pid
        )
        (Set.toList (GameState.battlefield gs))
    )

-- ONE board for Glen Elendra's Answer, built once and branched, and
-- swiftSilenceBoard's with a third seat and both stack populations. THREE SEATS,
-- because "your opponents control" and "you don't control" are the same set at
-- two: carol holds the second opponent's ability, so a sweep keyed to CR 102.1
-- and one keyed to "not mine" still agree, while a sweep keyed to the ACTIVE
-- player would not.
--
-- bob casts, and waiting under his spell are five objects, each on the stack for
-- a reason:
--
--   * alice's Divination is the counterable opponent SPELL, present only when
--     `mCounterable` is;
--   * alice's Blurred Mongoose prints "can't be countered" (CR 113.6g), which is
--     what tells the swept set apart from the countered one;
--   * alice's and carol's Prodigal Sorcerer activations are the opponent
--     ABILITIES (CR 113.9), which is what this card reaches and Swift Silence's
--     "all other spells" does not;
--   * bob's own Sorcerer activation and his own Goblin Piker are the controls:
--     one ability and one spell that "your opponents control" must spare.
--
-- bob also has a Baral, Chief of Compliance, whose "whenever a spell or ability
-- you control counters A SPELL" is the fence keeping GameEvent.SpellCountered and
-- GameEvent.AbilityCountered apart: a countered ability records the sibling event
-- and not his, so he must fire once here and not three times.
--
-- Four Islands, one more than Baral's CR 601.2f reduction leaves {1}{U}{U}
-- needing, so no assertion below turns on the reduction having applied. Every
-- library is stocked, since a Baral trigger that resolves draws and CR 104.3c
-- would otherwise decide the game.
--
-- Glen Elendra's Answer is CAST rather than placed, swiftSilenceBoard's reason:
-- CR 601.2b's mode choice is a binding only a cast writes. The victims are
-- placed, since none of them resolves.
--
-- Nothing where the Sorcerer stopped declaring exactly one activated ability.
--
-- Returns the counterable opponent spell, the uncounterable one, the two
-- opponent abilities, bob's own two objects, the Answer in hand, and the board.
glenElendraBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Maybe Printing.Printing ->
  Maybe (Maybe ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
glenElendraBoard island glenElendra baral piker mongoose sorcerer mCounterable = case soleActivatedAbility sorcerer of
  Nothing -> Nothing
  Just ability ->
    let lands = S.landsFor island S.bob 4 S.threePlayerGame
        stock pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard island pid g)) gs [1 :: Int .. 5]
        stocked = List.foldl' (flip stock) lands [S.alice, S.bob, S.carol]
        (_, withBaral) = S.addPermanent baral S.bob stocked
        -- CR 302.6: each Sorcerer must have settled under its own controller
        -- before its {T} may be activated at all.
        addSorcerer pid (ids, gs) = let (oid, g) = S.addPermanent sorcerer pid gs in (ids <> [(pid, oid)], g)
        (sorcerers, withSorcerers) = List.foldl' (flip addSorcerer) ([], withBaral) [S.alice, S.bob, S.carol]
        settled = List.foldl' (\g pid -> S.runPure S.identityAnswer g (Engine.settleAll pid)) withSorcerers [S.alice, S.bob, S.carol]
        (mHers, withHers) = case mCounterable of
          Nothing -> (Nothing, settled)
          Just counterable -> let (oid, g) = S.spellOnStack counterable S.alice settled in (Just oid, g)
        (uncounterable, withMongoose) = S.spellOnStack mongoose S.alice withHers
        (his, withHis) = S.spellOnStack piker S.bob withMongoose
        -- Each activation names its own controller as the damage recipient (CR
        -- 120.3a), which no assertion reads: none of these abilities resolves.
        atSelf :: PlayerId.PlayerId -> Prompt.Prompt r -> r
        atSelf pid p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer pid))) sets
          _ -> S.identityAnswer p
        activate (ids, gs) (pid, oid) =
          let before = GameState.stack gs
              after = S.runPure (atSelf pid) (gs {GameState.priority = Just pid}) (Activate.activateAbility pid oid ability)
           in (ids <> fmap ((,) pid) (filter (`notElem` before) (GameState.stack after)), after)
        (abilityIds, activated) = List.foldl' activate ([], withHis) sorcerers
        (answer, board) = S.addHandCard glenElendra S.bob activated
        theirs = fmap snd (filter ((/= S.bob) . fst) abilityIds)
        mine = fmap snd (filter ((== S.bob) . fst) abilityIds) <> [his]
     in Just (mHers, uncounterable, theirs, mine, answer, board)

-- bob casts his Glen Elendra's Answer over the waiting stack, lets it resolve,
-- and settles so CR 603.3's triggers reach the stack. Answers the board and the
-- objects the settle ADDED, which is how many times Baral fired.
glenElendraRun :: ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [ObjectId.ObjectId])
glenElendraRun answer gs =
  let cast = S.runPure S.identityAnswer gs (S.cast S.bob answer)
      resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
   in (settled, filter (`notElem` GameState.stack resolved) (GameState.stack settled))

-- Glen Elendra's Answer {2}{U}{U} Instant: "This spell can't be countered. /
-- Counter all spells your opponents control and all abilities your opponents
-- control. Create a 1/1 blue and black Faerie creature token with flying for
-- each spell and ability countered this way." Oracle text checked against
-- api.scryfall.com, 2026-08-21.
--
-- The proving case for ObjectRef.EachOnStack: Swift Silence's sweep with CR
-- 109.2b's word "spell" switched off, so CR 405.1's whole zone is in and the
-- Filter alone narrows it. CR 115.10a keeps the set off the target list, so
-- nothing is announced at CR 601.2c and CR 608.2b has nothing to fizzle.
glenElendrasAnswerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
glenElendrasAnswerSpec s registry = Spec.describe s "GlenElendrasAnswer" $ do
  -- Six objects wait on the stack, and each reading of the sentence makes a
  -- different number of Faeries, so the board tells them apart:
  --
  --   * "all other SPELLS", CR 109.2b's reading, makes 1;
  --   * everything the sweep NAMED, counting the Mongoose CR 113.6g spared,
  --     makes 4;
  --   * everything on the stack, with CR 102.1's relation dropped, makes 5 --
  --     bob's own ability and his own spell as well;
  --   * what was actually countered this way is 3.
  Spec.it s "CR 405.1/701.6a counters an opponent's abilities beside their spells, and makes a Faerie for each" $ do
    island <- S.printingOf s registry "Island"
    glenElendra <- S.printingOf s registry "Glen Elendra's Answer"
    baral <- S.printingOf s registry "Baral, Chief of Compliance"
    piker <- S.printingOf s registry "Goblin Piker"
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    divination <- S.printingOf s registry "Divination"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    case glenElendraBoard island glenElendra baral piker mongoose sorcerer (Just divination) of
      Nothing -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      Just (mHers, uncounterable, theirs, mine, answer, board) -> do
        Spec.assertEqWith s "setup: the two opponents each activated one ability" (length theirs) 2
        let (resolved, triggers) = glenElendraRun answer board
        -- THE gameplay assertion, and it comes first: three objects were
        -- countered this way -- one spell and two abilities -- so three Faeries.
        Spec.assertEqWith s "one spell and two abilities countered this way, so three Faeries under bob" (faerieTokensUnder S.bob resolved) 3
        -- The fence. Baral's "counters A SPELL" saw one countering, not three:
        -- the other two are GameEvent.AbilityCountered, which his condition does
        -- not read.
        Spec.assertEqWith s "CR 113.9 Baral fired once, for the spell alone" (length triggers) 1
        -- CR 608.2n: a countered ability ceases to exist, so a live object is
        -- one the sweep spared.
        Spec.assertBool s (all (\oid -> Maybe.isNothing (Game.lookupObject oid resolved)) theirs) "CR 608.2n both opponent abilities ceased to exist"
        Spec.assertBool s (all (\oid -> Maybe.isJust (Game.lookupObject oid resolved)) mine) "bob's own ability and his own spell were spared"
        Spec.assertEqWith
          s
          "what is left on the stack is bob's own two objects and CR 113.6g's uncounterable spell, under their original ids"
          (List.sort (filter (`notElem` triggers) (GameState.stack resolved)))
          (List.sort (uncounterable : mine))
        Spec.assertEqWith s "CR 701.6a alice's countered spell reached her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
        Spec.assertBool s (all (\oid -> not (S.onBattlefield oid resolved)) (Maybe.maybeToList mHers)) "the countered spell never resolved"
        -- CR 608.2n again, from the other side: the Answer put ITSELF into bob's
        -- graveyard as the last part of its own resolution, and nothing
        -- countered it (CR 113.6g).
        Spec.assertEqWith s "bob's graveyard holds the spent Answer alone" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 1
  -- The discriminating twin: the SAME board with the counterable opponent SPELL
  -- removed and nothing else changed. Two Faeries rather than three, and Baral
  -- goes silent -- so the third Faerie above was that spell and the two here are
  -- the abilities, which is the whole of what this unit adds.
  Spec.it s "CR 113.9 with only abilities countered, the Faeries still come and Baral stays silent" $ do
    island <- S.printingOf s registry "Island"
    glenElendra <- S.printingOf s registry "Glen Elendra's Answer"
    baral <- S.printingOf s registry "Baral, Chief of Compliance"
    piker <- S.printingOf s registry "Goblin Piker"
    mongoose <- S.printingOf s registry "Blurred Mongoose"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    case glenElendraBoard island glenElendra baral piker mongoose sorcerer Nothing of
      Nothing -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
      Just (_, _, theirs, _, answer, board) -> do
        let (resolved, triggers) = glenElendraRun answer board
        Spec.assertEqWith s "two abilities countered this way, so two Faeries" (faerieTokensUnder S.bob resolved) 2
        Spec.assertEqWith s "and Baral, whose trigger says A SPELL, did not fire at all" (length triggers) 0
        Spec.assertBool s (all (\oid -> Maybe.isNothing (Game.lookupObject oid resolved)) theirs) "CR 608.2n both opponent abilities ceased to exist"
        Spec.assertEqWith s "and nothing reached alice's graveyard: an ability has none to reach (CR 608.2n)" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 0

-- Rampage of the Clans {3}{G} Instant: "Destroy all artifacts and enchantments.
-- For each permanent destroyed this way, its controller creates a 3/3 green
-- Centaur creature token." Oracle text checked against api.scryfall.com,
-- 2026-08-21.
--
-- Bane of Progress' sweep with the OTHER rider: the count half
-- (data/scenarios/board-effect) reads how many died, and this reads WHICH --
-- Destroy's `permanents` slot, walked by Effect.ForEach with each member's
-- controller supplying CR 111.2's creator through CR 608.2h last known
-- information.
--
-- THREE SEATS, and the destroyed permanents are split between two of them:
-- alice's artifact and bob's enchantment. A rider keyed to the CASTER rather
-- than to each permanent's controller would put both Centaurs under alice, so
-- the split is what makes the reading observable. carol holds the controls.
--
-- Cast off four Forests through the PRIORITY LOOP: the spell resolves inside
-- the loop and the tokens are on the board when it settles. Answers the
-- finished board and the TRANSCRIPT, since what CR 608.2f did or did not ask
-- alice is half of what these cases claim.
castRampage :: Printing.Printing -> Printing.Printing -> GameState.GameState -> (GameState.GameState, [Response.Response])
castRampage forest rampage board =
  let (withSpell, spell) = S.handOne rampage (S.landsFor forest S.alice 4 board)
      ((_, resolved), transcript) = Replay.record S.identityAnswer withSpell (S.cast S.alice spell >> Engine.priorityLoop)
   in (resolved, transcript)

-- The CR 608.2f intra-seat orderings alice was asked for, in order. VariableEffectSpec's
-- own reader, one spec over.
orderAnswersIn :: [Response.Response] -> [[Natural]]
orderAnswersIn = Maybe.mapMaybe (\r -> case r of Response.OrderedForEach o -> Just o; _ -> Nothing)

-- The permanents on the battlefield named "Centaur Token" that this player
-- CONTROLS. Projection.controllerOf and not Support.countOnBattlefieldByName,
-- which indexes the battlefield by OWNER (CR 108.3) and so cannot see control at
-- all -- and CR 111.2 makes the creator both here, which is exactly why the
-- weaker question would pass under either reading.
centaursControlledBy :: PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
centaursControlledBy pid gs =
  filter
    ( \oid ->
        fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Centaur Token"))
          && Projection.controllerOf oid gs == Just pid
    )
    (Set.toList (GameState.battlefield gs))

-- The card names in a player's graveyard, sorted. CR 701.8a moves a destroyed
-- permanent to its OWNER's graveyard, which is the half of "its controller" this
-- group has to tell apart.
graveyardNames :: PlayerId.PlayerId -> GameState.GameState -> [String]
graveyardNames pid gs =
  List.sort
    (Maybe.mapMaybe (\oid -> fmap (Text.unpack . CardName.unwrap . Face.name) (Game.faceOf oid gs)) (Game.zoneMembers Zone.Graveyard pid gs))

rampageOfTheClansSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rampageOfTheClansSpec s registry = Spec.describe s "RampageOfTheClans" $ do
  -- The proving case for #463: a mass destruction whose rider acts on EACH
  -- permanent it destroyed rather than on how many there were.
  --
  -- The board separates every reading that could be taken:
  --
  --   * alice's Bonesplitter and bob's Bad Moon are destroyed, one per seat, so
  --     "its controller" and "you" give different answers;
  --   * carol's Darksteel Myr is an artifact the filter MATCHES and CR 702.12b
  --     will not let be destroyed, so a rider walking the swept set rather than
  --     the destroyed set would hand carol a Centaur;
  --   * carol's Goblin Piker is neither, and is the control.
  Spec.it s "CR 111.2 each destroyed permanent's own controller creates its Centaur" $ do
    forest <- S.printingOf s registry "Forest"
    rampage <- S.printingOf s registry "Rampage of the Clans"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    badMoon <- S.printingOf s registry "Bad Moon"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    piker <- S.printingOf s registry "Goblin Piker"
    let (equipment, g1) = S.addPermanent bonesplitter S.alice (Setup.emptyGame S.threePlayers)
        (moon, g2) = S.addPermanent badMoon S.bob g1
        (myr, g3) = S.addPermanent darksteelMyr S.carol g2
        (bystander, board) = S.addPermanent piker S.carol g3
        (resolved, transcript) = castRampage forest rampage board
    -- The gameplay-level assertion, and FIRST: a rider keyed to the caster puts
    -- this at 0 and alice's at 2, so neither reading is vacuous here.
    Spec.assertEqWith s "bob controlled the destroyed enchantment, so bob controls a Centaur" (length (centaursControlledBy S.bob resolved)) 1
    Spec.assertEqWith s "alice controlled the destroyed artifact, so alice controls one and not two" (length (centaursControlledBy S.alice resolved)) 1
    Spec.assertEqWith s "CR 702.12b carol's indestructible artifact was matched and not destroyed, so carol gets nothing" (length (centaursControlledBy S.carol resolved)) 0
    Spec.assertEqWith s "stack empty: the spell resolved" (length (GameState.stack resolved)) 0
    Spec.assertBool s (not (S.onBattlefield equipment resolved)) "the artifact died"
    Spec.assertBool s (not (S.onBattlefield moon resolved)) "the enchantment died"
    Spec.assertBool s (S.onBattlefield myr resolved) "the indestructible artifact creature stands"
    -- CR 608.2f's secondary sentence gives away a relative order only "on
    -- multiple objects controlled by the same player", and these two are not --
    -- so each seat is a group of one, APNAP settles the whole order, and there
    -- is nothing to ask. Read off the destroyed PERMANENTS through CR 608.2h,
    -- since neither is on the battlefield by the time the loop runs.
    Spec.assertEqWith s "and no order was asked for: the two dead permanents were two seats' " (orderAnswersIn transcript) []
    Spec.assertBool s (S.onBattlefield bystander resolved) "the creature that is neither was never named"
    -- CR 111.1: the token is what the card says it is, not a blank permanent.
    case centaursControlledBy S.bob resolved of
      [centaur] -> do
        Spec.assertEqWith s "a 3/3" (Projection.powerOf centaur resolved) (Just 3)
        Spec.assertEqWith s "with 3 toughness" (Projection.toughnessOf centaur resolved) (Just 3)
        Spec.assertEqWith s "and green" (Set.toList (Projection.colorsOf centaur resolved)) [Color.Green]
      other -> Spec.assertEqWith s "exactly one Centaur under bob" (length other) 1
  -- The discriminating twin: the SAME spell over a board where ONE seat holds
  -- both doomed permanents. Two Centaurs, both under bob -- so the case above's
  -- one-each is the rider following each permanent rather than dealing one token
  -- per seat that lost anything.
  Spec.it s "two permanents of one player's make two Centaurs for that player" $ do
    forest <- S.printingOf s registry "Forest"
    rampage <- S.printingOf s registry "Rampage of the Clans"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    badMoon <- S.printingOf s registry "Bad Moon"
    let (_, g1) = S.addPermanent bonesplitter S.bob (Setup.emptyGame S.threePlayers)
        (_, board) = S.addPermanent badMoon S.bob g1
        (resolved, transcript) = castRampage forest rampage board
    Spec.assertEqWith s "both destroyed permanents were bob's, so bob makes two Centaurs" (length (centaursControlledBy S.bob resolved)) 2
    Spec.assertEqWith s "and the caster, who lost nothing, makes none" (length (centaursControlledBy S.alice resolved)) 0
    -- The case above's negative from the other side: two objects of ONE seat's
    -- are what CR 608.2f's secondary sentence is about, so alice is asked once.
    Spec.assertEqWith s "alice ordered bob's two, having none of her own" (orderAnswersIn transcript) [[0, 1]]
  -- CR 608.2h with CR 701.8a: "its controller" is the permanent's LAST KNOWN
  -- controller, and CR 701.8a moves the card to its OWNER's graveyard. The two
  -- differ here and nowhere else in this group, which is what makes this the
  -- case that says which of them the rider reads: bob's Control Magic has taken
  -- alice's Bonded Construct, and the sweep destroys the Construct and the Aura
  -- both. bob controlled each of them, so bob makes both Centaurs -- while the
  -- Construct's card lands in alice's graveyard, where a rider reading the
  -- CARDS rather than the permanents would have found it.
  Spec.it s "CR 608.2h the Centaur goes to the permanent's last controller, not to its owner" $ do
    forest <- S.printingOf s registry "Forest"
    rampage <- S.printingOf s registry "Rampage of the Clans"
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    controlMagic <- S.printingOf s registry "Control Magic"
    let (construct, g1) = S.addPermanent bondedConstruct S.alice (Setup.emptyGame S.threePlayers)
        (aura, g2) = S.addPermanent controlMagic S.bob g1
        board = S.attach aura construct g2
        (resolved, transcript) = castRampage forest rampage board
    Spec.assertEqWith s "setup: bob's Control Magic has taken alice's artifact creature" (Projection.controllerOf construct board) (Just S.bob)
    Spec.assertEqWith s "bob controlled both destroyed permanents, so bob makes both Centaurs" (length (centaursControlledBy S.bob resolved)) 2
    Spec.assertEqWith s "and its OWNER makes none" (length (centaursControlledBy S.alice resolved)) 0
    Spec.assertEqWith s "CR 701.8a while the Construct's card went to alice's graveyard, next to the spell itself" (graveyardNames S.alice resolved) ["Bonded Construct", "Rampage of the Clans"]
    Spec.assertEqWith s "and the Aura's card to bob's" (graveyardNames S.bob resolved) ["Control Magic"]
    Spec.assertEqWith s "alice ordered the two, both being bob's" (orderAnswersIn transcript) [[0, 1]]
  -- CR 608.2c with CR 101.3: the sweep destroys nothing, so the slot is unbound,
  -- the loop has no members and nobody creates anything.
  Spec.it s "an empty sweep leaves the loop no members, so no Centaur is created" $ do
    forest <- S.printingOf s registry "Forest"
    rampage <- S.printingOf s registry "Rampage of the Clans"
    piker <- S.printingOf s registry "Goblin Piker"
    let (bystander, board) = S.addPermanent piker S.bob (Setup.emptyGame S.threePlayers)
        (resolved, _) = castRampage forest rampage board
    Spec.assertEqWith s "no Centaur for the caster" (length (centaursControlledBy S.alice resolved)) 0
    Spec.assertEqWith s "none for bob either" (length (centaursControlledBy S.bob resolved)) 0
    Spec.assertBool s (S.onBattlefield bystander resolved) "the creature that is neither an artifact nor an enchantment stands"

-- Phyrexian Rebirth {4}{W}{W} Sorcery: "Destroy all creatures, then create an
-- X/X colorless Phyrexian Horror artifact creature token, where X is the number
-- of creatures destroyed this way." Oracle text checked against
-- api.scryfall.com, 2026-09-06.
--
-- Rampage of the Clans' sweep with Destroy's COUNT rider instead of its
-- permanents one, read where no card had read it before: inside the token's own
-- printed box. CR 111.3 makes the value the creating effect defines part of the
-- token's TEXT, which is why Resolve.bakeTokenCharacteristics stamps it as a
-- literal at the mint rather than leaving a board-reading box on a permanent.
--
-- THREE SEATS, and the doomed creatures are split across two of them, so a
-- count keyed to the caster's own board would read 1 rather than 3. carol's
-- Darksteel Myr is a creature the filter MATCHES and CR 702.12b will not let be
-- destroyed, so a box counting the SWEPT set would read 4.
castRebirth :: Printing.Printing -> Printing.Printing -> GameState.GameState -> GameState.GameState
castRebirth plains rebirth board =
  let (withSpell, spell) = S.handOne rebirth (S.landsFor plains S.alice 6 board)
   in S.settleSba (S.runPure S.identityAnswer withSpell (S.cast S.alice spell >> Engine.priorityLoop))

-- The Horror's printed power AS TEXT, which is CR 111.3's "this becomes the
-- token's text" made observable: a stamped literal cannot be re-read, and
-- the spell that bound the count is in a graveyard under a fresh id by now
-- (CR 400.7), so a box left standing would answer nothing at all.
horrorPrintedPower :: ObjectId.ObjectId -> GameState.GameState -> Maybe Quantity.Quantity
horrorPrintedPower oid gs = fmap Power.unwrap (Game.faceOf oid gs >>= Face.power)

phyrexianRebirthSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
phyrexianRebirthSpec s registry = Spec.describe s "PhyrexianRebirth" $ do
  -- The case that PROVES a token's printed box may be a Quantity.InSlot naming
  -- an amount an EARLIER effect of the same resolution bound, which
  -- Resolve.bakeTokenCharacteristics reads against the resolution rather than
  -- against the token.
  --
  -- Every other reading gives a different number here, which is the whole point
  -- of the board: 4 for the swept set, 1 for the caster's own creatures, 2 and 1
  -- for the Pikers' printed box, and 0 for a box that reads the token's own
  -- (empty) bindings.
  Spec.it s "CR 111.3 the Horror is an X/X where X is the number of creatures DESTROYED" $ do
    plains <- S.printingOf s registry "Plains"
    rebirth <- S.printingOf s registry "Phyrexian Rebirth"
    piker <- S.printingOf s registry "Goblin Piker"
    evangel <- S.printingOf s registry "Cabal Evangel"
    myr <- S.printingOf s registry "Darksteel Myr"
    let (alicePiker, g1) = S.addPermanent piker S.alice (Setup.emptyGame S.threePlayers)
        (bobPiker, g2) = S.addPermanent piker S.bob g1
        (bobEvangel, g3) = S.addPermanent evangel S.bob g2
        (carolMyr, board) = S.addPermanent myr S.carol g3
        resolved = castRebirth plains rebirth board
        horrors = fmap (\oid -> S.powerToughnessOf oid resolved) (S.tokensOf resolved)
    Spec.assertEqWith s "a 3/3 Horror: three creatures were destroyed, four were swept, one was alice's" horrors [Just (3, 3)]
    Spec.assertEqWith s "CR 111.3 and 3 is its printed power, stamped as text at the mint" (fmap (`horrorPrintedPower` resolved) (S.tokensOf resolved)) [Just (Quantity.Literal 3)]
    Spec.assertBool s (not (S.onBattlefield alicePiker resolved)) "alice's Piker died"
    Spec.assertBool s (not (S.onBattlefield bobPiker resolved)) "bob's Piker died"
    Spec.assertBool s (not (S.onBattlefield bobEvangel resolved)) "bob's Evangel died"
    Spec.assertBool s (S.onBattlefield carolMyr resolved) "CR 702.12b carol's indestructible Myr was swept and stands"
    Spec.assertEqWith s "stack empty: the spell resolved" (length (GameState.stack resolved)) 0
  -- The STATIC-ANALYSIS half. The gameplay cases above pass whatever the walkers
  -- answer, because Pawl.Engine.Resolve.Effect.bakeTokenCharacteristics evaluates
  -- the box whether or not anything reported it -- so only this case says that
  -- CR 603.3b's dataflow lint can SEE the read, which is what stops a token box
  -- from naming a slot nothing binds.
  --
  -- Off the printed card and not a planted effect: the whole face's reads are
  -- asserted, so the entry can only have come from the token's box. The
  -- Destroy that binds it reports nothing here -- a bind is a definition, and
  -- boundSlots is what reports those -- which is why the two halves of the
  -- lint's subtraction are asserted together.
  Spec.it s "CR 111.3 the token's printed box reports its slot read, and the Destroy defines it" $ do
    rebirth <- S.cardOf s registry "Phyrexian Rebirth"
    let face = NonEmpty.head (Card.Type.faces rebirth)
        destroyed = SlotName.MkSlotName (Text.pack "destroyed")
    Spec.assertEqWith
      s
      "the face reads `destroyed` as an amount, and reads it only through the Horror's X/X"
      (foldMap Resolve.slotsOf (Card.allEffects face))
      (Map.singleton destroyed SlotArity.Amount)
    Spec.assertEqWith
      s
      "and the sweep defines it, so the read dangles nowhere"
      (foldMap Resolve.boundSlots (Card.allEffects face))
      (Set.singleton destroyed)
    -- The other two walkers over the same position -- slotsAreExhaustive and
    -- readsX -- take the widening as REGRESSION FENCES rather than proven
    -- behaviour, so nothing here asserts them: a token box naming a slot is
    -- exhaustive either way, and no card in data/cards/ writes one that reads CR
    -- 601.2b's X, so mutating the box out of both leaves the suite green
    -- (2026-09-06). They are kept because CR 111.3 makes the box the creating
    -- effect's number for all three readers alike.
    Spec.assertBool
      s
      (all Resolve.slotsAreExhaustive (Card.allEffects face))
      "CR 603.3b nothing this face reads is a slot the walkers cannot report"

-- Plummet ({1}{G} Instant, "Destroy target creature with flying"), the pool's
-- first card whose Filter names a KEYWORD (Filter.HasKeyword, CR 702.9).
--
-- The negative half of every pair here is the one that carries the claim: a
-- Filter that admitted everything would pass the positive assertions unchanged.
plummetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
plummetSpec s registry = Spec.describe s "Plummet" $ do
  -- CR 702.9b: "A creature with flying can't be blocked except by creatures with
  -- flying and/or reach" -- the ability Bird Maiden prints and Goblin Piker does
  -- not. Nothing else separates the two here, so only the keyword can be what
  -- decides the offer.
  Spec.it s "CR 702.9 HasKeyword Flying admits the flier and rejects the ground creature" $ do
    plummet <- S.printingOf s registry "Plummet"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    piker <- S.printingOf s registry "Goblin Piker"
    case S.spellTargetSlot plummet of
      Nothing -> Spec.assertFailure s "Plummet's printing carries no 'target' slot"
      Just theSlot -> do
        let (flierId, gs1) = S.addPermanent birdMaiden S.bob (Setup.emptyGame S.bothPlayers)
            (groundId, gs) = S.addPermanent piker S.bob gs1
            legal = Target.legalRecipients Nothing S.noSource theSlot gs
        Spec.assertBool s (Set.member (Recipient.ToCreature flierId) legal) "the flier is a legal target"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature groundId) legal)) "the creature without flying is not"
  -- The other direction, and the one that proves the read is not of the printed
  -- card: Humility (CR 613.1f, "all creatures lose all abilities") takes the
  -- flying off a creature that PRINTS it, and the offer goes with it.
  Spec.it s "CR 613.1f Humility strips the printed flying, and the offer goes with it" $ do
    plummet <- S.printingOf s registry "Plummet"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    humility <- S.printingOf s registry "Humility"
    case S.spellTargetSlot plummet of
      Nothing -> Spec.assertFailure s "Plummet's printing carries no 'target' slot"
      Just theSlot -> do
        let (flierId, before) = S.addPermanent birdMaiden S.bob (Setup.emptyGame S.bothPlayers)
            after = S.withHumility humility before
        Spec.assertBool s (Set.member (Recipient.ToCreature flierId) (Target.legalRecipients Nothing S.noSource theSlot before)) "legal while it flies"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying flierId after)) "Humility took the flying"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature flierId) (Target.legalRecipients Nothing S.noSource theSlot after))) "so it is no longer a legal target"

-- Announces X=2 and takes the identity fallback everywhere else -- which answers
-- CR 601.2b's Phyrexian question with the FIRST offer, the mana route, so the
-- {G/P} is paid with a Forest rather than with life.
answerXTwo :: Prompt.Prompt r -> r
answerXTwo p = case p of
  Prompt.ChooseX {} -> 2
  _ -> S.identityAnswer p

-- The damage marked on a permanent (CR 120.3e), or Nothing if it is gone.
markedOn :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural
markedOn oid gs = fmap Object.damage (Game.lookupObject oid gs)

-- Corrosive Gale ({X}{G/P} Sorcery, "Corrosive Gale deals X damage to each
-- creature with flying") -- the pool's first Effect.DealDamage over a SET rather
-- than a slot, and the first producer of ObjectRef.EachMatching at all whose
-- filter names a keyword.
--
-- One board throughout: bob's Bird Maiden (1/2, prints flying), alice's
-- Narcomoeba (1/1, prints flying) and bob's Goblin Piker (2/1, prints none),
-- beside three of alice's Forests. The fliers are split between the two players
-- on purpose: "each creature with flying" is not "each creature your opponents
-- control", and alice burning her own Narcomoeba is what says so. The Piker is
-- the other half of the claim: CR 109.2 hands an EachMatching the WHOLE
-- battlefield, so a filter missing its HasKeyword half would burn it too.
--
-- The Forests are not a third control and could not be: CR 120.1a takes a land
-- out of the batch at Damage.damageRecipient whatever the filter said. The
-- HasCardType half of the filter is pinned by CardsSpec instead.
corrosiveGaleSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
corrosiveGaleSpec s registry = Spec.describe s "CorrosiveGale" $ do
  Spec.it s "CR 109.2 Corrosive Gale deals X to each creature with flying, and none to the one without" $ do
    gale <- S.printingOf s registry "Corrosive Gale"
    forest <- S.printingOf s registry "Forest"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    narcomoeba <- S.printingOf s registry "Narcomoeba"
    piker <- S.printingOf s registry "Goblin Piker"
    let (maidenId, g1) = S.addPermanent birdMaiden S.bob (S.landsInPlay forest 3)
        (moebaId, g2) = S.addPermanent narcomoeba S.alice g1
        (pikerId, g3) = S.addPermanent piker S.bob g2
        (gs, spellId) = S.handOne gale g3
        cast = snd (Engine.runGamePure answerXTwo gs (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure answerXTwo cast Stack.resolveTop)
        after = S.settleSba resolved
    Spec.assertEqWith s "three Forests paid {2}{G}" (S.tappedCount S.alice after) 3
    Spec.assertEqWith s "CR 120.3e: 2 marked on the Bird Maiden" (markedOn maidenId resolved) (Just 2)
    Spec.assertEqWith s "CR 120.3e: 2 marked on the Narcomoeba, an opponent's flier is no different" (markedOn moebaId resolved) (Just 2)
    Spec.assertEqWith s "and nothing at all on the Goblin Piker" (markedOn pikerId resolved) (Just 0)
    Spec.assertBool s (not (S.onBattlefield maidenId after)) "CR 704.5g buried the 1/2"
    Spec.assertBool s (not (S.onBattlefield moebaId after)) "and the 1/1"
    Spec.assertBool s (S.onBattlefield pikerId after) "the creature without flying was never in the set"

-- Teferi, Hero of Dominaria {3}{W}{U} Legendary Planeswalker -- Teferi, loyalty
-- 4: "+1: Draw a card. At the beginning of the next end step, untap up to two
-- lands." (Oracle text checked against api.scryfall.com 2026-09-27.) CR 608.2d's
-- "up to two": ObjectRef.AnyNumberMatching with a ceiling, chosen as the delayed
-- ability resolves rather than targeted.
--
-- Five tapped lands, three alice's Islands and two bob's Mountains -- the
-- printed "lands" is not "lands you control" -- so more are eligible than the
-- ceiling admits, and the choice is put to a player however many it names.
teferiUntapSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
teferiUntapSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      -- Records every ChooseAnyNumberOfPermanents as (candidates, ceiling) and
      -- answers it with `pick` of the candidates -- offered or not, so an
      -- engine that trusted the answer would act on it.
      answering :: ([ObjectId.ObjectId] -> [ObjectId.ObjectId]) -> Prompt.Prompt r -> State.State [([ObjectId.ObjectId], Maybe Natural)] r
      answering pick p = case p of
        Prompt.ChooseAnyNumberOfPermanents _ _ _ candidates atMost -> do
          State.modify (<> [(candidates, atMost)])
          pure (Set.fromList (pick candidates))
        _ -> pure (S.identityAnswer p)
      tapped gs oid = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Tapped
      -- alice's Teferi at its printed loyalty, the five lands tapped in APNAP
      -- order, and a stocked library for the +1's draw. Activates the +1,
      -- resolves it, then begins the end step and resolves the delayed ability
      -- under `answering pick`.
      run pick = do
        teferi <- S.printingOf s registry "Teferi, Hero of Dominaria"
        island <- S.printingOf s registry "Island"
        mountain <- S.printingOf s registry "Mountain"
        piker <- S.printingOf s registry "Goblin Piker"
        let (teferiId, g0) = S.addPermanent teferi S.alice (Setup.emptyGame S.bothPlayers)
            withLoyalty = S.addCounter CounterKind.Loyalty 4 teferiId g0
            addLand (ids, g) (p, pid) = let (oid, g') = S.addPermanent p pid g in (ids <> [oid], S.tapObject oid g')
            (lands, g1) = List.foldl' addLand ([], withLoyalty) [(island, S.alice), (island, S.alice), (island, S.alice), (mountain, S.bob), (mountain, S.bob)]
            g2 = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) g1 [1 .. (3 :: Int)]
            gs = g2 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice, GameState.turnNumber = 2}
        pure $ case Face.activatedAbilities (S.combinedFace teferi) of
          plus : _ ->
            let armed = S.runPure S.identityAnswer gs (do Activate.activateAbility S.alice teferiId plus; Stack.resolveTop)
                settled = S.runPure S.identityAnswer (beginEndStep armed) Engine.settleForPriority
                (after, asked) = State.runState (fmap snd (Engine.runGame (answering pick) settled Engine.priorityLoop)) []
             in Just (lands, armed, after, asked)
          [] -> Nothing
   in Spec.describe s "TeferiHeroOfDominaria" $ do
        -- One of each seat's lands named: exactly those two untap, and the prompt
        -- offered all five with a ceiling of two.
        Spec.it s "CR 608.2d the delayed +1 untaps the two lands named, of five offered" $ do
          let chosen cs = case cs of
                a : _ : _ : m : _ -> [a, m]
                _ -> []
          result <- run chosen
          case result of
            Just (lands, armed, after, asked) -> do
              Spec.assertEqWith s "the +1 armed one delayed ability" (length (GameState.delayedTriggers armed)) 1
              Spec.assertEqWith s "alice's first Island and bob's first Mountain untapped, the other three did not" (fmap (tapped after) lands) [False, True, True, False, True]
              Spec.assertEqWith s "asked once, offered every land, at most two" asked [(lands, Just 2)]
            Nothing -> Spec.assertFailure s "Teferi prints a +1"

-- Archfiend of Depravity {3}{B}{B} Creature -- Demon 5/4: "Flying / At the
-- beginning of each opponent's end step, that player chooses up to two creatures
-- they control, then sacrifices the rest." (Oracle text checked against
-- api.scryfall.com 2026-09-30.) CR 608.2d's plural choice made by a seat other
-- than the ability's controller: Effect.ChoosePermanents.
--
-- Three seats, so "that player" is not merely "an opponent": alice controls the
-- Archfiend, it is bob's end step, and carol's three creatures are neither
-- offered nor sacrificed. Bob has three, one more than the ceiling.
archfiendOfDepravitySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
archfiendOfDepravitySpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      -- Records every ChooseAnyNumberOfPermanents as (decider, candidates,
      -- ceiling) and answers with the first and third candidates.
      answering :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, [ObjectId.ObjectId], Maybe Natural)] r
      answering p = case p of
        Prompt.ChooseAnyNumberOfPermanents decider _ _ candidates atMost -> do
          State.modify (<> [(Decider.unwrap decider, candidates, atMost)])
          pure (Set.fromList [c | (i, c) <- zip [0 :: Int ..] candidates, i /= 1])
        _ -> pure (S.identityAnswer p)
   in Spec.describe s "ArchfiendOfDepravity"
        . Spec.it s "CR 608.2d that player chooses the two they keep and sacrifices the rest"
        $ do
          archfiend <- S.printingOf s registry "Archfiend of Depravity"
          piker <- S.printingOf s registry "Goblin Piker"
          let (archfiendId, g0) = S.addPermanent archfiend S.alice (Setup.emptyGame S.threePlayers)
              addCreature (ids, g) pid = let (oid, g') = S.addPermanent piker pid g in (ids <> [oid], g')
              (bobs, g1) = List.foldl' addCreature ([], g0) [S.bob, S.bob, S.bob]
              (carols, g2) = List.foldl' addCreature ([], g1) [S.carol, S.carol, S.carol]
              gs = g2 {GameState.phase = endStep, GameState.activePlayer = S.bob, GameState.turnNumber = 2}
              began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.bob)) gs
              settled = S.runPure S.identityAnswer began Engine.settleForPriority
              (after, asked) = State.runState (fmap snd (Engine.runGame answering settled Engine.priorityLoop)) []
          Spec.assertEqWith
            s
            "bob's first and third creatures, carol's three and the Archfiend are left; bob's second is sacrificed"
            (fmap (`S.onBattlefield` after) (bobs <> carols <> [archfiendId]))
            [True, False, True, True, True, True, True]
          Spec.assertEqWith s "bob alone was asked, offered his three, at most two" asked [(S.bob, bobs, Just 2)]

-- Covetous Elegy {4}{W}{B} Sorcery: "Each player chooses up to two creatures
-- they control, then sacrifices the rest. Then you create a tapped Treasure token
-- for each creature your opponents control." (Oracle text checked against
-- api.scryfall.com 2026-09-30.) Effect.ChoosePermanents in an Effect.ForEach
-- over the players, the loop's union of the picks then spared by one Sacrifice.
--
-- Three seats, each answering with every candidate but the second: alice's lone
-- creature is kept, bob and carol each lose their second of three.
covetousElegySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
covetousElegySpec s registry =
  let answering :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, [ObjectId.ObjectId])] r
      answering p = case p of
        Prompt.ChooseAnyNumberOfPermanents decider _ _ candidates _ -> do
          State.modify (<> [(Decider.unwrap decider, candidates)])
          pure (Set.fromList [c | (i, c) <- zip [0 :: Int ..] candidates, i /= 1])
        _ -> pure (S.identityAnswer p)
      tappedTokens gs = length [t | t <- S.tokensOf gs, fmap Object.tapped (Game.lookupObject t gs) == Just TapState.Tapped]
   in Spec.describe s "CovetousElegy"
        . Spec.it s "CR 101.4 each player keeps the two they chose; alice gets a Treasure per opposing survivor"
        $ do
          elegy <- S.printingOf s registry "Covetous Elegy"
          piker <- S.printingOf s registry "Goblin Piker"
          plains <- S.printingOf s registry "Plains"
          swamp <- S.printingOf s registry "Swamp"
          let addCreature (ids, g) pid = let (oid, g') = S.addPermanent piker pid g in (ids <> [oid], g')
              lands = S.landsFor swamp S.alice 3 (S.landsFor plains S.alice 3 S.threePlayerGame)
              (alices, g0) = addCreature ([], lands) S.alice
              (bobs, g1) = List.foldl' addCreature ([], g0) [S.bob, S.bob, S.bob]
              (carols, g2) = List.foldl' addCreature ([], g1) [S.carol, S.carol, S.carol]
              (withSpell, spell) = S.handOne elegy g2
              gs = withSpell {GameState.priority = Just S.alice}
              (after, asked) = State.runState (fmap snd (Engine.runGame answering gs (S.cast S.alice spell >> Stack.resolveTop))) []
          Spec.assertEqWith
            s
            "survivors, then who was asked over what, then alice's tapped Treasures"
            (fmap (`S.onBattlefield` after) (alices <> bobs <> carols), asked, tappedTokens after)
            ([True, True, False, True, True, False, True], [(S.alice, alices), (S.bob, bobs), (S.carol, carols)], 4)

-- Come Back Wrong {2}{B} Sorcery (DSK 86): "Destroy target creature. If a
-- creature card is put into a graveyard this way, return it to the battlefield
-- under your control. Sacrifice it at the beginning of your next end step."
--
-- The pool's first card to NAME what a destruction buried, where Bane of
-- Progress above only COUNTS it. The two are different questions about the same
-- printed phrase, and the difference is CR 400.7: the permanent that was
-- destroyed does not exist by the time the second sentence runs, so the only
-- object left to name is the incarnation the graveyard move minted -- a
-- different id, in a different zone, which is why Effect.Destroy's `buried` slot
-- is bound from the move's answer rather than from its own target slot.
--
-- One board throughout: bob's lone creature, alice's three Swamps, and Come Back
-- Wrong in alice's hand. The creature is bob's on purpose -- "under YOUR
-- control" is a change of controller (CR 110.2a), which one seat could not show.
comeBackWrongSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
comeBackWrongSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      creaturesOnBattlefield gs = filter (`Projection.isCreatureOf` gs) (Set.toList (GameState.battlefield gs))
      -- alice casts Come Back Wrong at `victim` off three Swamps and lets it
      -- resolve, then settles state-based actions.
      castAt spell base =
        let (withSpell, spellId) = S.handOne spell base
            afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spellId)
         in S.settleSba (S.runPure S.identityAnswer afterCast Stack.resolveTop)
   in Spec.describe s "ComeBackWrong" $ do
        -- The card's last sentence, and the reason the MoveToZone binds a slot of
        -- its own: "it" is the BATTLEFIELD incarnation, a third object again (CR
        -- 400.7). "Your next end step" is TurnScope.ControllersTurn, so alice's.
        Spec.it s "CR 603.7 the delayed ability sacrifices what came back at the caster's next end step" $ do
          comeBackWrong <- S.printingOf s registry "Come Back Wrong"
          swamp <- S.printingOf s registry "Swamp"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, board) = S.addPermanent piker S.bob (S.landsInPlay swamp 3)
              armed = castAt comeBackWrong board
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "one creature was on the battlefield to sacrifice" (length (creaturesOnBattlefield armed)) 1
          Spec.assertEqWith s "and none is left" (creaturesOnBattlefield after) []
          Spec.assertEqWith s "CR 701.21a a sacrifice puts it in its OWNER's graveyard, not the caster's" (namesIn Zone.Graveyard S.bob after) [Just (S.nameOf (Printing.card piker))]
          Spec.assertEqWith s "the delayed store is spent" (length (GameState.delayedTriggers after)) 0

-- Apocalypse Chime {2} Artifact -- "{2}, {T}, Sacrifice this artifact: Destroy
-- all nontoken permanents with a name originally printed in the Homelands
-- expansion. They can't be regenerated." (name, cost, type line and Oracle text
-- checked against api.scryfall.com, 2026-09-02.)
--
-- CR 206.3 is what makes this a name question rather than a question about
-- sets: the errata reads "a name originally printed in", and CR 206.3c DEFINES
-- the list of names. The card names the expansion symbolically --
-- Filter.HasNameOriginallyPrintedIn "HML" -- and the rule's own list is
-- Pawl.Types.Expansion.catalog, so a name is still a characteristic (CR 109.3)
-- and the card still enumerates nothing.
--
-- The board tells apart the readings a wrong list or a dropped conjunct takes:
--
--   * A LISTED name versus every permanent. carol's Serra Inquisitors is on CR
--     206.3c's list and alice's Goblin Piker is not.
--   * NONTOKEN versus every permanent (CR 111.1). bob's token is built from the
--     same Serra Inquisitors card, so its name matches too and only that
--     conjunct tells the two apart.
--   * CR 701.19c versus an ordinary destruction. alice's Aether Storm carries a
--     regeneration shield and is destroyed through it.
--   * EVERY controller versus the activator's. Three seats, one of the listed
--     permanents each.
--
-- Aether Storm rather than a second Serra Inquisitors for the shielded
-- permanent, so that no one mutation can redden two of these assertions at
-- once: it is an enchantment, which is also "all nontoken PERMANENTS" being
-- read wider than creatures. Neither its "creature spells can't be cast" nor
-- its pay-life ability is reachable here -- nothing is cast and no other
-- ability is activated.
apocalypseChimeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
apocalypseChimeSpec s registry =
  Spec.describe s "ApocalypseChime" $ do
    Spec.it s "CR 206.3c destroys the listed nontoken permanents, through a shield, and nothing else" $ do
      chime <- S.printingOf s registry "Apocalypse Chime"
      plains <- S.printingOf s registry "Plains"
      piker <- S.printingOf s registry "Goblin Piker"
      storm <- S.printingOf s registry "Aether Storm"
      inquisitors <- S.printingOf s registry "Serra Inquisitors"
      let (chimeId, g1) = S.addPermanent chime S.alice (S.landsFor plains S.alice 2 S.threePlayerGame)
          (pikerId, g2) = S.addPermanent piker S.alice g1
          (stormId, g3) = S.addPermanent storm S.alice g2
          (tokenId, g4) = S.addToken (Printing.card inquisitors) S.bob (S.addRegenShield stormId g3)
          (carolsId, g5) = S.addPermanent inquisitors S.carol g4
          board = g5 {GameState.priority = Just S.alice}
      case soleActivatedAbility chime of
        Nothing -> Spec.assertFailure s "Apocalypse Chime should print exactly one activated ability"
        Just ability -> do
          let after = S.runPure S.identityAnswer board (Activate.activateAbility S.alice chimeId ability >> Stack.resolveTop)
          Spec.assertBool s (not (null (GameState.replacements board))) "the fixture really armed a regeneration shield"
          Spec.assertBool s (not (S.onBattlefield carolsId after)) "CR 206.3c carol's Serra Inquisitors, a listed nontoken permanent, was destroyed"
          Spec.assertBool s (not (S.onBattlefield stormId after)) "CR 701.19c alice's Aether Storm was destroyed through its regeneration shield"
          Spec.assertBool s (S.onBattlefield tokenId after) "CR 111.1 bob's token of that same listed card survives"
          Spec.assertBool s (S.onBattlefield pikerId after) "and alice's Goblin Piker, whose name is not on the list, survives"

-- Switcheroo ({4}{U} Sorcery, "Exchange control of two target creatures.")
-- against CR 701.12b, whose two sentences are the two tests below: different
-- controllers swap simultaneously, one controller does nothing at all.
--
-- The two boards differ in EXACTLY one thing -- who controls the second targeted
-- creature -- so the negative is not assembled on a board of its own. Three
-- seats, and carol's Bog Wraith is a third candidate the slot's count of two
-- cannot take, so the targeting is a real choice rather than a forced one; the
-- answerer FILTERS the offered set (S.preferring) rather than building
-- recipients, so CR 608.2b's re-read at resolution still finds them.
switcherooBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  PlayerId.PlayerId ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
switcherooBoard s registry secondController = do
  switcheroo <- S.printingOf s registry "Switcheroo"
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  evangel <- S.printingOf s registry "Cabal Evangel"
  wraith <- S.printingOf s registry "Bog Wraith"
  let withLands = List.foldl' (\gs _ -> snd (S.addPermanent island S.alice gs)) S.threePlayerGame [1 .. 5 :: Int]
      (alicePiker, g1) = S.addPermanent piker S.alice withLands
      (second, g2) = S.addPermanent evangel secondController g1
      (carolWraith, g3) = S.addPermanent wraith S.carol g2
      (g4, spellId) = S.handOne switcheroo g3
  pure
    ( alicePiker,
      second,
      carolWraith,
      spellId,
      g4
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Casts nothing itself -- S.cast drives that -- but pays the mana and aims the
-- one target slot at the two creatures `wanted` names.
switcherooAnswer :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
switcherooAnswer wanted p =
  let isWanted r = case r of
        Recipient.ToCreature oid -> elem oid wanted
        _ -> False
   in case p of
        Prompt.ChooseManaSource _ _ candidates -> Just (NonEmpty.head candidates)
        Prompt.ChooseTargets _ _ _ sets -> S.preferring isWanted sets
        _ -> S.identityAnswer p

switcherooSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
switcherooSpec s registry = Spec.describe s "Switcheroo" $ do
  -- CR 701.12b's first sentence. BOTH halves are asserted, in that order: an
  -- implementation that moved one creature and left the other is the reading
  -- this discriminates, and "the permanent that was controlled by the other
  -- player" is not satisfied by either half alone.
  Spec.it s "CR 701.12b two creatures of different controllers swap controllers" $ do
    (alicePiker, bobEvangel, carolWraith, spellId, board) <- switcherooBoard s registry S.bob
    let answer :: Prompt.Prompt r -> r
        answer = switcherooAnswer [alicePiker, bobEvangel]
        cast = S.runPure answer board (S.cast S.alice spellId)
        after = S.runPure answer cast Stack.resolveTop
    Spec.assertEqWith s "bob controls what was alice's Goblin Piker" (Projection.controllerOf alicePiker after) (Just S.bob)
    Spec.assertEqWith s "alice controls what was bob's Cabal Evangel" (Projection.controllerOf bobEvangel after) (Just S.alice)
    Spec.assertEqWith s "carol's Bog Wraith, whom nobody targeted, is untouched" (Projection.controllerOf carolWraith after) (Just S.carol)
    -- CR 302.6: neither creature has been under its new controller's control
    -- since that player's most recent turn began, so neither may attack now. The
    -- Piker could attack for alice on the board this one came from.
    Spec.assertBool s (alicePiker `notElem` Combat.legalAttackers S.alice after) "CR 302.6 alice can no longer attack with the Piker she gave away"
    Spec.assertBool s (bobEvangel `notElem` Combat.legalAttackers S.alice after) "CR 302.6 nor with the Evangel she just took"

-- Spawnbroker {2}{U} 1/1 (Magic Origins; Oracle checked against Scryfall
-- 2026-10-08): "When this creature enters, you may exchange control of target
-- creature you control and target creature with power less than or equal to that
-- creature's power an opponent controls." CR 701.12b between two SLOTS, the
-- second's CR 208.1 bound read off the one creature the first names.
--
-- alice controls the Spawnbroker (power 1), a Goblin Piker (2) and a Bog Wraith
-- (3), so the `yours` slot's candidates are plural and the `theirs` slot can only
-- be offered bob's Wraith by measuring each of them alone: against all three at
-- once the bound reads no power (Binding.onlyOne), the offer is empty, and
-- placement removes the trigger as though no legal choice existed. bob's
-- Typhoid Rats keep the `theirs` choice a real one. The two runs differ in
-- exactly one thing, which creature fills `yours`.
spawnbrokerRun :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, GameState.GameState)
spawnbrokerRun s registry measuredByWraith = do
  spawnbroker <- S.printingOf s registry "Spawnbroker"
  piker <- S.printingOf s registry "Goblin Piker"
  wraith <- S.printingOf s registry "Bog Wraith"
  rats <- S.printingOf s registry "Typhoid Rats"
  let (alicePiker, g1) = S.addPermanent piker S.alice S.threePlayerGame
      (aliceWraith, g2) = S.addPermanent wraith S.alice g1
      (bobWraith, g3) = S.addPermanent wraith S.bob g2
      (_, g4) = S.addPermanent rats S.bob g3
      (_, entered) = S.entersWithTrigger spawnbroker S.alice g4
      yours = if measuredByWraith then aliceWraith else alicePiker
      wanted = [yours, bobWraith]
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter (maybe False (`elem` wanted) . Recipient.objectOf) offered) sets
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      placed = S.runPure answer entered Engine.placePendingTriggers
  pure (alicePiker, aliceWraith, bobWraith, placed, S.runPure answer placed Stack.resolveTop)

spawnbrokerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
spawnbrokerSpec s registry = Spec.describe s "Spawnbroker" $ do
  Spec.it s "CR 601.2c a bound read off one sibling target is offered against each candidate alone" $ do
    (alicePiker, aliceWraith, bobWraith, measuredPlaced, measured) <- spawnbrokerRun s registry True
    (_, _, _, refusedPlaced, refused) <- spawnbrokerRun s registry False
    Spec.assertEqWith s "alice controls bob's Wraith, whose power 3 her own Wraith's power admits" (Projection.controllerOf bobWraith measured) (Just S.alice)
    Spec.assertEqWith s "and bob controls alice's Wraith" (Projection.controllerOf aliceWraith measured) (Just S.bob)
    -- CR 601.2c's joint check, the other run: measured against the power 2
    -- Piker, bob's Wraith is not a legal target, so the announcement is refused
    -- at placement rather than put on the stack to fail at CR 608.2b. Legal
    -- choices exist, so this is not CR 603.3d: the decider repeats the refused
    -- answer and Engine.placeBorne's `attempt` gives up and ceases the trigger.
    Spec.assertEqWith s "measured against her Piker instead, the trigger never reaches the stack" (length (GameState.stack refusedPlaced)) 0
    Spec.assertEqWith s "where measured against her Wraith it did" (length (GameState.stack measuredPlaced)) 1
    Spec.assertEqWith s "and bob keeps his Wraith" (Projection.controllerOf bobWraith refused) (Just S.bob)
    Spec.assertEqWith s "and alice her Piker" (Projection.controllerOf alicePiker refused) (Just S.alice)

-- Avarice Totem ({1} Artifact, "{5}: Exchange control of this artifact and
-- target nonland permanent.") against CR 701.12b's OTHER printed shape: one side
-- is CR 113.7's source object rather than a second target.
--
-- Switcheroo's board one type over, and for its reasons: three seats, the two
-- boards differing in EXACTLY one thing -- who controls the targeted Evangel --
-- and carol's Bog Wraith a candidate nobody names, so the slot's one target is a
-- real choice. The five Islands are lands and so are not candidates; the Totem
-- itself is one, which is why the answerer FILTERS rather than building a
-- recipient.
avariceTotemBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  PlayerId.PlayerId ->
  m (Printing.Printing, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
avariceTotemBoard s registry evangelController = do
  totem <- S.printingOf s registry "Avarice Totem"
  island <- S.printingOf s registry "Island"
  evangel <- S.printingOf s registry "Cabal Evangel"
  wraith <- S.printingOf s registry "Bog Wraith"
  let withLands = List.foldl' (\gs _ -> snd (S.addPermanent island S.alice gs)) S.threePlayerGame [1 .. 5 :: Int]
      (totemId, g1) = S.addPermanent totem S.alice withLands
      (evangelId, g2) = S.addPermanent evangel evangelController g1
      (wraithId, g3) = S.addPermanent wraith S.carol g2
  pure
    ( totem,
      totemId,
      evangelId,
      wraithId,
      g3
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Pays the {5} off whatever mana source comes first and aims the one target slot
-- at `wanted`. A Permanents pool offers Recipient.ToObject, so that is the tag
-- filtered on.
avariceTotemAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
avariceTotemAnswer wanted p =
  let isWanted r = case r of
        Recipient.ToObject oid -> oid == wanted
        _ -> False
   in case p of
        Prompt.ChooseManaSource _ _ candidates -> Just (NonEmpty.head candidates)
        Prompt.ChooseTargets _ _ _ sets -> S.preferring isWanted sets
        _ -> S.identityAnswer p

avariceTotemSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
avariceTotemSpec s registry = Spec.describe s "AvariceTotem" $ do
  -- CR 701.12b's first sentence, with the source as one side. Both halves are
  -- asserted, in that order: the reading this discriminates is one that moved the
  -- targeted permanent and left the source where it was.
  Spec.it s "CR 701.12b the source and its one target swap controllers" $ do
    (totem, totemId, bobEvangel, carolWraith, board) <- avariceTotemBoard s registry S.bob
    case soleActivatedAbility totem of
      Nothing -> Spec.assertFailure s "Avarice Totem should print exactly one activated ability"
      Just ability -> do
        let answer :: Prompt.Prompt r -> r
            answer = avariceTotemAnswer bobEvangel
            after = S.runPure answer board (Activate.activateAbility S.alice totemId ability >> Stack.resolveTop)
        Spec.assertEqWith s "bob controls the Totem alice activated" (Projection.controllerOf totemId after) (Just S.bob)
        Spec.assertEqWith s "alice controls what was bob's Cabal Evangel" (Projection.controllerOf bobEvangel after) (Just S.alice)
        Spec.assertEqWith s "carol's Bog Wraith, whom nobody targeted, is untouched" (Projection.controllerOf carolWraith after) (Just S.carol)
        -- CR 302.6 re-Sicks the Evangel under its new controller, so alice cannot
        -- attack with what she just took. It could attack for bob on the board
        -- this one came from.
        Spec.assertBool s (bobEvangel `notElem` Combat.legalAttackers S.alice after) "CR 302.6 alice cannot attack with the Evangel she just took"

-- God-Eternal Bontu {3}{B}{B} 5/6 menace: "When God-Eternal Bontu enters,
-- sacrifice any number of other permanents, then draw that many cards."
-- (Scryfall, 2026-09-29.)
--
-- alice holds five Swamps, a Goblin Piker, an Ogre Sentry and a Bird Maiden;
-- bob a Hill Giant. She names the Piker and the Sentry, so "that many" is two,
-- where the offer is eight and every other permanent on the battlefield nine.
godEternalBontuSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
godEternalBontuSpec s registry =
  let -- Records each ChooseAnyNumberOfPermanents' candidates and answers with
      -- exactly `wanted`, so the offer is read off the engine.
      answering :: Set.Set ObjectId.ObjectId -> Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
      answering wanted p = case p of
        Prompt.ChooseAnyNumberOfPermanents _ _ _ candidates _ -> do
          State.modify (<> [candidates])
          pure wanted
        _ -> pure (S.identityAnswer p)
      run withPeace = do
        bontu <- S.printingOf s registry "God-Eternal Bontu"
        swamp <- S.printingOf s registry "Swamp"
        piker <- S.printingOf s registry "Goblin Piker"
        sentry <- S.printingOf s registry "Ogre Sentry"
        maiden <- S.printingOf s registry "Bird Maiden"
        giant <- S.printingOf s registry "Hill Giant"
        peace <- S.printingOf s registry "Rest in Peace"
        let addLand (ids, g) _ = let (oid, g') = S.addPermanent swamp S.alice g in (ids <> [oid], g')
            (swamps, g0) = List.foldl' addLand ([], Setup.emptyGame S.bothPlayers) [1 .. (5 :: Int)]
            (pikerId, g1) = S.addPermanent piker S.alice g0
            (sentryId, g2) = S.addPermanent sentry S.alice g1
            (maidenId, g3) = S.addPermanent maiden S.alice g2
            (giantId, g4) = S.addPermanent giant S.bob g3
            (peaceIds, g5) = if withPeace then let (oid, g') = S.addPermanent peace S.alice g4 in ([oid], g') else ([], g4)
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) g5 [1 .. (6 :: Int)]
            (withSpell, spell) = S.handOne bontu stocked {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
            afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
            (after, asked) = State.runState (fmap snd (Engine.runGame (answering (Set.fromList [pikerId, sentryId])) afterCast Engine.priorityLoop)) []
            offered = Set.fromList (swamps <> [pikerId, sentryId, maidenId] <> peaceIds)
        pure (after, asked, offered, (pikerId, sentryId, maidenId, giantId))
      drew gs = length (Game.zoneMembers Zone.Hand S.alice gs)
   in Spec.describe s "GodEternalBontu" $ do
        Spec.it s "CR 701.21a draws one card for each permanent she named and sacrificed" $ do
          (after, asked, offered, (pikerId, sentryId, maidenId, giantId)) <- run False
          Spec.assertEqWith s "alice drew two cards, the two she sacrificed" (drew after) 2
          Spec.assertEqWith s "CR 701.21a the Piker and the Sentry went to her graveyard" (List.sort (namesIn Zone.Graveyard S.alice after)) (List.sort [Just (named "Goblin Piker"), Just (named "Ogre Sentry")])
          Spec.assertBool s (not (S.onBattlefield pikerId after || S.onBattlefield sentryId after)) "neither named creature is on the battlefield"
          Spec.assertBool s (S.onBattlefield maidenId after && S.onBattlefield giantId after) "the Maiden she passed over and bob's Giant stay"
          Spec.assertEqWith s "asked once, offered her eight other permanents and neither Bontu nor bob's Giant" (fmap Set.fromList asked) [offered]
          Spec.assertBool s (Maybe.isJust (namedOnBattlefield "God-Eternal Bontu" after)) "Bontu herself stays"
  where
    named = CardName.MkCardName . Text.pack

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  godEternalBontuSpec s registry
  switcherooSpec s registry
  spawnbrokerSpec s registry
  avariceTotemSpec s registry
  plummetSpec s registry
  corrosiveGaleSpec s registry
  exhumeSpec s registry
  bloodForBonesSpec s registry
  skullwinderSpec s registry
  elvishPiperSpec s registry
  gloriousProtectorSpec s registry
  teferiUntapSpec s registry
  archfiendOfDepravitySpec s registry
  covetousElegySpec s registry
  banisherPriestSpec s registry
  levelerSpec s registry
  calderaBreakerSpec s registry
  trumpetBlastSpec s registry
  auraThiefSpec s registry
  rampageOfTheClansSpec s registry
  phyrexianRebirthSpec s registry
  comeBackWrongSpec s registry
  swiftSilenceSpec s registry
  glenElendrasAnswerSpec s registry
  countOnLuckSpec s registry
  actOnImpulseSpec s registry
  galvanicRelaySpec s registry
  communeWithLavaSpec s registry
  apocalypseChimeSpec s registry
