{-# LANGUAGE GADTs #-}

-- Covers CR 720 end to end: Pawl.Types.Layout's Omen arm, the Pawl.Engine.Card
-- arms that read it (CR 720.4's normal-characteristics view and CR 720.3's
-- castable halves) plus Card.isOmen, and Pawl.Engine.Resolve.finishSpell, where
-- CR 720.3d's shuffle-back is the one clause that is a consequence of the layout
-- rather than of the card's printed opcodes.
--
-- Every case runs against the printed Riling Dawnbreaker // Signaling Roar: a
-- {4}{W} 3/4 Dragon whose Omen is a {1}{W} sorcery, "Create a 2/2 white Soldier
-- creature token". Its halves are named rather than inferred, since S.cast
-- routes through S.soleFaceName and errors on a card with more than one castable
-- half.
module Pawl.OmenSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- The two names the card prints (CR 720.2). The first is the card's own name
-- under CR 720.4; the second names only the alternative characteristics, and CR
-- 720.5 is what lets a player choose it where a name is asked for -- the open
-- half of that rule is #679.
dawnbreakerName, roarName :: CardName.CardName
dawnbreakerName = CardName.MkCardName (Text.pack "Riling Dawnbreaker")
roarName = CardName.MkCardName (Text.pack "Signaling Roar")

soldierName :: CardName.CardName
soldierName = CardName.MkCardName (Text.pack "Soldier Token")

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Omen" $ do
  -- CR 720.4: "In every zone except the stack, and while on the stack not as an
  -- Omen, an omen card has only its normal characteristics."
  --
  -- The falsifier is CR 709.4's reading, which is what the pool's split cards
  -- take: a combined view would name this card
  -- "Riling Dawnbreaker//Signaling Roar", give it both card types, and price it
  -- at the concatenated {4}{W}{1}{W} for mana value 7.
  Spec.it s "CR 720.4 in a hand the card is only its normal half" $ do
    dawnbreaker <- S.printingOf s registry "Riling Dawnbreaker"
    let (gs, oid) = S.handOne dawnbreaker (Setup.emptyGame S.bothPlayers)
    case Game.faceOf oid gs of
      Nothing -> Spec.assertFailure s "expected a card in hand"
      Just face -> do
        Spec.assertEqWith s "named for the creature alone" (Projection.namesOf oid gs) (Set.singleton dawnbreakerName)
        Spec.assertEqWith s "mana value 5, not the two halves' 7" (Quantity.manaValueOf face) 5
        Spec.assertBool s (Set.member CardType.Creature (Projection.cardTypesOf oid gs)) "a creature card"
        Spec.assertBool s (not (Set.member CardType.Sorcery (Projection.cardTypesOf oid gs))) "and not a sorcery"
  -- CR 720.3: "As a player casts an omen card, the player chooses whether they
  -- cast the card normally or as an Omen." Both halves offered, and the choice
  -- left to the player.
  --
  -- Asked of Pawl.Engine.Card.castableFaces DIRECTLY as well as through the
  -- board below, because that list is what every road to a cast reads -- the
  -- priority actions here, and Pawl.Engine.Resolve.offerCast, which builds its
  -- own candidates over the same call.
  Spec.it s "CR 720.3 both halves are castable faces" $ do
    dawnbreaker <- S.printingOf s registry "Riling Dawnbreaker"
    Spec.assertEqWith
      s
      "the creature and the Omen, in printed order"
      (fmap Face.name (Card.castableFaces (Printing.card dawnbreaker)))
      [dawnbreakerName, roarName]
  Spec.it s "CR 720.3 both halves are offered from a hand" $ do
    dawnbreaker <- S.printingOf s registry "Riling Dawnbreaker"
    plains <- S.printingOf s registry "Plains"
    let namesOffered gs = [n | A.Cast _ n _ <- Action.legalActions S.alice gs]
        (both, _) = S.handOne dawnbreaker (S.landsInPlay plains 5)
        (one, _) = S.handOne dawnbreaker (S.landsInPlay plains 2)
    Spec.assertEqWith s "five Plains: both halves" (namesOffered both) [dawnbreakerName, roarName]
    -- CR 720.3a: "When casting an omen card as an Omen, only the alternative
    -- characteristics are evaluated to see if it can be cast." Two Plains pay
    -- Signaling Roar's {1}{W} and cannot pay the creature's {4}{W}, so an engine
    -- pricing either half off the other offers the wrong one -- or, if it priced
    -- both off a combined cost, neither.
    Spec.assertEqWith s "two Plains: only the Omen" (namesOffered one) [roarName]
  -- CR 720.3d: "As an Omen spell resolves, its controller shuffles it into its
  -- owner's library instead of putting it into its owner's graveyard as it
  -- resolves." The clause this whole layout exists for -- CR 715.3d exiles an
  -- Adventure instead, and nothing the card prints says either.
  --
  -- alice's library starts EMPTY (S.landsInPlay), so the library reading below is
  -- the omen card or nothing, and the graveyard reading distinguishes "shuffled
  -- back" from "went nowhere at all".
  Spec.it s "CR 720.3d the resolved Omen creates the token and is shuffled into its owner's library" $ do
    dawnbreaker <- S.printingOf s registry "Riling Dawnbreaker"
    plains <- S.printingOf s registry "Plains"
    let (gs, oid) = S.handOne dawnbreaker (S.landsInPlay plains 2)
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid roarName Facing.FaceUp))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        namesIn zone = fmap (\o -> Projection.namesOf o resolved) (Game.zoneMembers zone S.alice resolved)
    Spec.assertEqWith s "the omen card is in its owner's library" (namesIn Zone.Library) [Set.singleton dawnbreakerName]
    Spec.assertEqWith s "and not in the graveyard" (namesIn Zone.Graveyard) []
    -- The Omen's own printed effect, which is what makes the case a resolution
    -- rather than a fizzle: an Omen that never resolved would leave no token and
    -- would pass the two assertions above by being still on the stack.
    Spec.assertEqWith s "the Soldier token is on the battlefield" (length (filter (\o -> Set.member soldierName (Projection.namesOf o resolved)) (Set.toList (GameState.battlefield resolved)))) 1
    Spec.assertEqWith s "and the stack is empty" (length (GameState.stack resolved)) 0
  -- CR 701.24a is the other half of rule 720.3d's "shuffles": the move alone
  -- would put the card back in a known position. Counted through the prompt,
  -- since Pawl.Engine.Event.shuffleLibrary raises exactly one Prompt.Shuffle per
  -- library it randomizes and a deterministic answerer makes the ORDER
  -- unobservable.
  --
  -- The creature half is the discriminating pair's other board: it resolves onto
  -- the battlefield and shuffles nothing, so a `Prompt.Shuffle` raised on every
  -- resolution would fail here rather than pass twice.
  Spec.it s "CR 701.24a the Omen's owner shuffles, and the creature half does not" $ do
    dawnbreaker <- S.printingOf s registry "Riling Dawnbreaker"
    plains <- S.printingOf s registry "Plains"
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.Shuffle {} -> do
            State.modify' (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (S.identityAnswer p)
        shufflesFor name lands =
          let (gs, oid) = S.handOne dawnbreaker (S.landsInPlay plains lands)
              cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid name Facing.FaceUp))
           in State.execState (Engine.runGame countingAnswer cast Stack.resolveTop) 0
    Spec.assertEqWith s "the Omen shuffles one library" (shufflesFor roarName 2) 1
    Spec.assertEqWith s "the creature half shuffles none" (shufflesFor dawnbreakerName 5) 0
