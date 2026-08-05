-- Covers CR 715 end to end: Pawl.Types.Layout's Adventure arm, the two
-- Pawl.Engine.Card arms that read it (CR 715.4's combined view and CR 715.3's
-- castable halves) plus Card.isAdventure, Pawl.Engine.Resolve.finishSpell (CR
-- 715.3d's exile), and Pawl.Engine.Cast.permitsCastFromExile -- the pool's only
-- route to playing a card from exile at all.
--
-- Every case runs against the printed Embereth Shieldbreaker // Battle Display:
-- a {1}{R} 2/1 Human Knight whose Adventure is a {R} sorcery, "Destroy target
-- artifact". Its halves are named rather than inferred, since S.cast routes
-- through S.soleFaceName and errors on a card with more than one castable half.
module Pawl.AdventureSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
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
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

-- The two names the card prints (CR 715.2). The first is the card's own name
-- under CR 715.4; the second names only the alternative characteristics, and CR
-- 715.5 is what lets a player choose it where a name is asked for (#650).
shieldbreakerName, battleDisplayName :: CardName.CardName
shieldbreakerName = CardName.MkCardName (Text.pack "Embereth Shieldbreaker")
battleDisplayName = CardName.MkCardName (Text.pack "Battle Display")

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Adventure" $ do
  -- CR 715.4: "In every zone except the stack, and while on the stack not as an
  -- Adventure, an adventurer card has only its normal characteristics."
  --
  -- The falsifier is CR 709.4's reading, which is what the pool's OTHER
  -- multi-face layout takes: a combined view would name this card
  -- "Embereth Shieldbreaker//Battle Display", give it both card types, and
  -- price it at the concatenated {1}{R}{R} for mana value 3.
  Spec.it s "CR 715.4 in a hand the card is only its normal half" $ do
    shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
    let (gs, oid) = S.handOne shieldbreaker (Setup.emptyGame S.bothPlayers)
    case Game.faceOf oid gs of
      Nothing -> Spec.assertFailure s "expected a card in hand"
      Just face -> do
        Spec.assertEqWith s "named for the creature alone" (Projection.nameOf oid gs) shieldbreakerName
        Spec.assertEqWith s "mana value 2, not the two halves' 3" (Quantity.manaValueOf face) 2
        Spec.assertBool s (Set.member CardType.Creature (Projection.cardTypesOf oid gs)) "a creature card"
        Spec.assertBool s (not (Set.member CardType.Sorcery (Projection.cardTypesOf oid gs))) "and not a sorcery"
  -- CR 715.3: "As a player plays an adventurer card, the player chooses whether
  -- they play the card normally or as an Adventure." Both halves offered, and
  -- the choice left to the player.
  Spec.it s "CR 715.3 both halves are offered from a hand" $ do
    shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let namesOffered gs = [n | A.Cast _ n <- Action.legalActions S.alice gs]
        -- An artifact on the battlefield, or Battle Display has no legal target
        -- and is gated out for a reason that has nothing to do with the layout.
        board = snd (S.addCreature bonesplitter S.alice (S.landsInPlay mountain 2))
        (both, _) = S.handOne shieldbreaker board
        (one, _) = S.handOne shieldbreaker (snd (S.addCreature bonesplitter S.alice (S.landsInPlay mountain 1)))
    Spec.assertEqWith s "two Mountains: both halves" (namesOffered both) [shieldbreakerName, battleDisplayName]
    -- CR 715.3a: "only the alternative characteristics are evaluated to see if
    -- it can be cast". One Mountain pays Battle Display's {R} and cannot pay the
    -- creature's {1}{R}, so an engine pricing either half off the other offers
    -- the wrong one -- or, if it priced both off a combined cost, neither.
    Spec.assertEqWith s "one Mountain: only the Adventure" (namesOffered one) [battleDisplayName]
  -- CR 715.3d: "Instead of putting a spell that was cast as an Adventure into
  -- its owner's graveyard as it resolves, its controller exiles it."
  Spec.it s "CR 715.3d the resolved Adventure destroys the artifact and is exiled" $ do
    shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let (bonesplitterId, board) = S.addCreature bonesplitter S.alice (S.landsInPlay mountain 1)
        (gs, oid) = S.handOne shieldbreaker board
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid battleDisplayName))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertBool s (S.onBattlefield bonesplitterId gs) "the Bonesplitter starts on the battlefield"
    Spec.assertBool s (not (S.onBattlefield bonesplitterId resolved)) "and Battle Display destroys it"
    -- BY NAME, since the destroyed Bonesplitter is in the graveyard too: what
    -- this asserts is which of the two cards went where, and CR 715.4 is what
    -- makes the exiled one answer to the creature's name.
    Spec.assertEqWith
      s
      "the adventurer card is the one in exile"
      (fmap (\o -> Projection.nameOf o resolved) (Game.zoneMembers Zone.Exile S.alice resolved))
      [shieldbreakerName]
    Spec.assertEqWith
      s
      "and the graveyard holds only its target"
      (fmap (\o -> Projection.nameOf o resolved) (Game.zoneMembers Zone.Graveyard S.alice resolved))
      [CardName.MkCardName (Text.pack "Bonesplitter")]
  -- CR 715.3d's second and third sentences, which are one question asked of the
  -- same exiled card: "For as long as that card remains exiled, that player may
  -- play it. It can't be cast as an Adventure this way."
  --
  -- The contrast with the hand case above is the whole point: the same card
  -- offers TWO halves from a hand and exactly ONE from exile.
  Spec.it s "CR 715.3d from exile the creature is castable and the Adventure is not" $ do
    shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    -- TWO artifacts, and the Adventure destroys only one. Without the survivor
    -- this case proves nothing: Battle Display would be gated out for having no
    -- legal target, and an engine that had forgotten CR 715.3d's "it can't be
    -- cast as an Adventure this way" entirely would still pass.
    let (_, oneArtifact) = S.addCreature bonesplitter S.alice (S.landsInPlay mountain 3)
        (_, board) = S.addCreature bonesplitter S.alice oneArtifact
        (gs, oid) = S.handOne shieldbreaker board
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid battleDisplayName))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        namesOffered g = [n | A.Cast _ n <- Action.legalActions S.alice g]
    Spec.assertEqWith
      s
      "an artifact survives, so the Adventure has a legal target"
      (length (filter (\o -> Set.member CardType.Artifact (Projection.cardTypesOf o resolved)) (Set.toList (GameState.battlefield resolved))))
      1
    -- Two Mountains are still untapped, so the creature's {1}{R} is payable and
    -- an absent offer is about the permission rather than about mana.
    Spec.assertEqWith s "only the creature half" (namesOffered resolved) [shieldbreakerName]
    case Game.zoneMembers Zone.Exile S.alice resolved of
      [exiledId] -> do
        Spec.assertBool s (Cast.castable S.alice exiledId shieldbreakerName resolved) "the creature is castable from exile"
        Spec.assertBool s (not (Cast.castable S.alice exiledId battleDisplayName resolved)) "the Adventure is not"
        -- The permission names its player (CR 715.3d's "that player"), so it is
        -- not an offer to the table.
        Spec.assertBool s (not (Cast.castable S.bob exiledId shieldbreakerName resolved)) "and bob may not cast it"
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))
  -- The whole loop, which is what the mechanic IS: the Adventure resolves, and
  -- the creature it left in exile is cast from there onto the battlefield.
  Spec.it s "CR 715.3d the creature is cast from exile onto the battlefield" $ do
    shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let (_, board) = S.addCreature bonesplitter S.alice (S.landsInPlay mountain 3)
        (gs, oid) = S.handOne shieldbreaker board
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid battleDisplayName))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    case Game.zoneMembers Zone.Exile S.alice resolved of
      [exiledId] -> do
        let recast = snd (Engine.runGamePure S.identityAnswer resolved (Cast.castSpell S.alice exiledId shieldbreakerName))
            entered = snd (Engine.runGamePure S.identityAnswer recast Stack.resolveTop)
            knights =
              [ o
              | o <- Set.toList (GameState.battlefield entered),
                Set.member Subtype.Knight (Projection.subtypesOf o entered)
              ]
        Spec.assertEqWith s "exile is empty again" (Game.zoneMembers Zone.Exile S.alice entered) []
        case knights of
          [knightId] -> do
            -- CR 715.4 again, on the far side of the loop: what entered is the
            -- 2/1 creature and not the sorcery it was cast through.
            Spec.assertEqWith s "a 2/1 on the battlefield" (S.powerToughnessOf knightId entered) (Just (2, 1))
            Spec.assertEqWith s "named for the creature" (Projection.nameOf knightId entered) shieldbreakerName
          other -> Spec.assertFailure s ("expected exactly one Knight, got " <> show (length other))
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))
  -- The mechanic's own ruling: "If an adventurer card ends up in exile for any
  -- other reason than by exiling itself while resolving, it won't give you
  -- permission to cast it as a permanent spell."
  --
  -- Without this the permission would be indistinguishable from "an adventurer
  -- card in exile is castable", which is a different and wrong rule.
  Spec.it s "an adventurer card exiled any other way permits nothing" $ do
    shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    -- Through handOne, which is what puts alice in a main phase holding
    -- priority: without it every cast below is refused on TIMING, and this case
    -- would pass against an engine that had no permission check at all. The card
    -- in hand is a Mountain, whose only action is a land drop, and the artifact
    -- is there so the Adventure half is not gated on targeting either.
    let (withArtifact, _) = S.handOne mountain (snd (S.addCreature bonesplitter S.alice (S.landsInPlay mountain 3)))
        (exiledId, gs) = S.addExiledCard shieldbreaker S.alice withArtifact
    Spec.assertEqWith s "no permission on the card" (fmap Object.playableFromExileBy (Game.lookupObject exiledId gs)) (Just Nothing)
    Spec.assertBool s (not (Cast.castable S.alice exiledId shieldbreakerName gs)) "the creature is not castable"
    Spec.assertBool s (not (Cast.castable S.alice exiledId battleDisplayName gs)) "nor is the Adventure"
  -- The same ruling's other half: an Adventure spell that leaves the stack "by
  -- failing to resolve because its targets have all become illegal" is NOT
  -- exiled. CR 608.2b's fizzle is the path, and CR 715.3d's "as it resolves"
  -- never reaches it.
  Spec.it s "CR 608.2b a fizzled Adventure goes to the graveyard, not exile" $ do
    shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let (bonesplitterId, board) = S.addCreature bonesplitter S.alice (S.landsInPlay mountain 3)
        (gs, oid) = S.handOne shieldbreaker board
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid battleDisplayName))
        -- The only target leaves the battlefield while the spell is on the
        -- stack, so every filled slot is illegal at CR 608.2b.
        gone = snd (Engine.runGamePure S.identityAnswer cast (Event.changeZone bonesplitterId Zone.Graveyard))
        fizzled = snd (Engine.runGamePure S.identityAnswer gone Stack.resolveTop)
    Spec.assertEqWith s "nothing in exile" (Game.zoneMembers Zone.Exile S.alice fizzled) []
    Spec.assertEqWith s "the card and its target are both in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice fizzled)) 2
