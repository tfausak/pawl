{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- The far side of Pawl.Engine.Engine.runGameAsked: what an answerer needs that
-- the rules engine deliberately cannot supply itself.
--
-- The engine's dependency list has no pawl:registry edge, so nothing inside it
-- can answer "what card is this name?"; see #3047. An interpreter holds a
-- registry by construction, which is why a question about the Oracle card
-- reference is settled here rather than there: this module depends on both
-- halves, and neither half depends on it.
module Pawl.Interpreter where

import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt

-- CR 201.4: is this a name in the Oracle card reference, and -- CR 201.4a --
-- does the card it names match the characteristics the effect asked for?
--
-- The registry IS the reference the rule speaks of: a name it does not answer to
-- is not a name a player may choose. That makes the answer a claim about the
-- pool a caller pointed the registry at, the same posture Pawl.Registry's own
-- header takes about what a registry is for.
--
-- Answered by the registry AND by the card it hands back, which is not one
-- question but two: a file registry looks a name up by its SLUG
-- (Pawl.Registry.slugFor), so "goblin-piker" and "GOBLIN PIKER" both fetch the
-- Goblin Piker, and neither is a name in the reference. Card.faceNamed is the
-- exact comparison that refuses them -- the same comparison
-- Pawl.Engine.Filter's HasChosenName makes later, so a name this admits is one
-- the rest of the engine can act on rather than one it silently ignores.
--
-- That face IS the judgement's subject, which is CR 201.4b and CR 201.4d in one
-- step: a split card's half and a double-faced card's back face are each keyed
-- in Pawl.Registry.index under their own name, and rule 201.4a is put to "only
-- that half's" or "only the back face's" characteristics. The alternative names
-- CR 201.4c and CR 201.4f allow are judged the same way. Nothing here reaches CR
-- 709.4a's combined view, which is a name the card has without being any one
-- face's and so is not choosable at all (CR 201.4b's "but not both").
--
-- Matched through Pawl.Engine.Projection.View.viewOfCard off the printed face,
-- Pawl.Engine.Companion.fulfilled's posture and for its reason: a card outside
-- the game has no object to project (CR 400.11), and rule 201.4a asks about
-- Oracle text rather than about anything on the board. The chooser is the
-- context's perspective, since they are the player the restriction is put to.
legalCardName ::
  (Monad m) =>
  Registry.Registry m ->
  GameState.GameState ->
  PlayerId.PlayerId ->
  Filter.Type.Filter Keyword.Keyword ->
  CardName.CardName ->
  m Bool
legalCardName registry gs chooser restriction name = do
  found <- Registry.fetchCard registry name
  pure $ case found of
    Nothing -> False
    Just card -> case Card.faceNamed name card of
      Nothing -> False
      Just face ->
        Filter.matches
          (Filter.contextFor (Game.teams gs) (Just chooser) Nothing)
          (Projection.viewOfCard face)
          restriction

-- CR 201.4's "the player must choose the name of a card", enforced over an
-- interpreter's own answerer: an illegal name is not recorded, the same question
-- is put again, and only a legal answer reaches the engine.
--
-- ASKED AGAIN rather than replaced, which is the one posture available here.
-- Prompt.ChooseCardName offers no candidate list -- rule 201.4's offer is every
-- card in the reference -- so there is nothing to fall back to the way
-- Pawl.Engine.Target.drawFromPiles falls back to the first card of a pile, and
-- picking a legal name on the answerer's behalf would be making a player's
-- choice. An answerer that never answers legally never returns, which is the
-- contract a client's own prompt loop already has --
-- Pawl.Engine.Replay.defaultAnswer is the one answerer in the tree that must not
-- be wrapped, its own arm saying why it answers rule 201.4 illegally on purpose.
--
-- A wrapper an interpreter installs rather than a check inside the engine: the
-- engine cannot resolve a name at all. Both roads to a chosen name --
-- Pawl.Engine.Resolve's Effect.ChooseCardName arm (CR 608.2c) and
-- Pawl.Engine.Event's EntryRewrite.ChooseCardNames arm (CR 614.1c) -- raise this
-- one Prompt, so covering the Prompt covers both.
--
-- Over Asked rather than Prompt because rule 201.4a's match needs the asking
-- game's teams for its Filter context, and Asked is where that state is.
--
-- Pawl.CastProhibitionSpec's "CR 201.4 a name no card has is refused and the
-- chooser is asked again", "CR 201.4 a slug the registry answers to is not a
-- card's name" and "CR 201.4a a real card the restriction forbids is refused and
-- the chooser is asked again" are what prove it.
policingCardNames ::
  (Monad m) =>
  Registry.Registry m ->
  (forall r. Asked.Asked r -> m r) ->
  Asked.Asked a ->
  m a
policingCardNames registry answer asked = case Asked.prompt asked of
  Prompt.ChooseCardName _ chooser _ restriction ->
    let again = do
          name <- answer asked
          legal <- legalCardName registry (Asked.game asked) chooser restriction name
          if legal then pure name else again
     in again
  _ -> answer asked
