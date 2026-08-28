{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 712.4's meld cards: Pawl.Types.Layout's Meld arm and the
-- Pawl.Engine.Card functions that read it. A meld card carries its front face
-- alone (CR 712.4b), so what this spec pins first is that the front face plays
-- exactly as a one-faced card's does -- the mana ability and the targeted grant
-- printed on Hanweir Battlements, reached through Pawl.Engine.Cost.tapForMana
-- and Pawl.Engine.Activate.activateAbility.
--
-- Hanweir Battlements and Hanweir Garrison are the pool's only meld pair, and
-- the Battlements is the half CR 712.4a puts the melding ability on.
module Pawl.MeldSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Meld as Meld
import qualified Pawl.Types.MeldSource as MeldSource
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- The offered set FILTERED rather than a hand-built recipient, so CR 608.2b's
-- re-read at resolution cannot drop a target the engine never offered.
aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    S.preferring (\r -> Recipient.objectOf r == Just oid) sets
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Meld" $ do
  -- CR 712.8's second sentence gives a meld card's front face "its own set of
  -- characteristics", and CR 712.8d makes those the live ones here: "While a
  -- double-faced permanent has its front face up, it has only the characteristics
  -- of its front face." CR 712.4b is why there is nothing else to show -- the
  -- back face determines nothing off a melded permanent, so pawl prints each half
  -- of the pair as its front face alone. The printed "{T}: Add {C}" is an
  -- ordinary activated mana ability (CR 605.1a).
  Spec.it s "CR 712.8d a meld card on the battlefield has its front face's characteristics: Hanweir Battlements taps for {C}" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    let (battlementsId, gs) = S.addCreature battlements S.alice (Setup.emptyGame S.bothPlayers)
        after = S.runPure S.identityAnswer gs (Cost.tapForMana battlementsId)
    Spec.assertEqWith
      s
      "pool"
      (Game.poolOf S.alice after)
      (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colorless, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}])
    Spec.assertEqWith s "and the land is tapped" (S.tappedCount S.alice after) 1
  -- The same front face's second printed ability, which targets: CR 712.8d again,
  -- and the case that shows the front face's TEXT is live rather than just its
  -- type line. The Mountain is what pays the {R}.
  Spec.it s "CR 712.8d Hanweir Battlements' {R}, {T} grants haste to the creature it targets" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (battlementsId, g1) = S.addCreature battlements S.alice base
        (_, g2) = S.addCreature mountain S.alice g1
        (pikerId, g3) = S.addCreature piker S.alice g2
        ready = g3 {GameState.priority = Just S.alice}
    case Face.activatedAbilities (S.combinedFace battlements) of
      _ : haste : _ -> do
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste pikerId ready)) "the Piker starts without haste"
        let after = S.runPure (aimedAt pikerId) ready (do Activate.activateAbility S.alice battlementsId haste; Stack.resolveTop)
        Spec.assertBool s (Projection.hasKeyword Keyword.Haste pikerId after) "CR 702.10: the targeted creature gains haste"
      _ -> Spec.assertFailure s "Hanweir Battlements should print two activated abilities"
  -- CR 701.42a: "the resulting permanent is a single object represented by two
  -- cards", and Game.componentsOf is the one reader that can name them. Pure and
  -- over a hand-built Source rather than a board, because the classifier is the
  -- shared thing CR 202.3c, CR 712.21 and CR 701.27g each read: the components in
  -- the order the meld recorded them, and nothing at all for a source that one
  -- card represents.
  Spec.it s "CR 701.42a the cards representing a melded permanent, and none for anything else" $ do
    let garrison = PrintingId.MkPrintingId 9
        battlements = PrintingId.MkPrintingId 10
        township =
          Source.OfMeld
            MeldSource.MkMeldSource
              { MeldSource.result = PrintingId.MkPrintingId 8,
                MeldSource.components = garrison NonEmpty.:| [battlements]
              }
    Spec.assertEqWith s "the two cards, in order" (Game.componentsOf township) (Seq.fromList [garrison, battlements])
    -- CR 712.8g: the combined back face is NOT one of them, so a reader summing
    -- the components under CR 202.3c cannot pick up the result's own cost.
    Spec.assertBool s (Seq.null (Seq.filter (== PrintingId.MkPrintingId 8) (Game.componentsOf township))) "the result is not a component"
    Spec.assertEqWith s "CR 108.2 an ordinary card represents only itself" (Game.componentsOf (Source.OfCard garrison)) Seq.empty
  -- CR 701.42a's keyword action, driven straight rather than through the card:
  -- the melding ability lands in a later change, and what is proven here is the
  -- opcode. Both halves of the pool's only meld pair sit in exile, where a card's
  -- own "exile them" would have put them, and the slot naming them is the one that
  -- exile would have bound (CR 400.7j).
  --
  -- The combined back face is card DATA the opcode carries, so any card stands in
  -- for it here; the Goblin Piker is a creature, which is what lets the entry cases
  -- below observe an entering CREATURE.
  Spec.it s "CR 701.42a two meld cards become one permanent, and stop being two cards" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    piker <- S.printingOf s registry "Goblin Piker"
    let (bId, gId, after) = melded battlements garrison piker
    case Set.toList (GameState.battlefield after) of
      [meldedId] -> do
        -- CR 712.8g: "the object represented by those cards has only the
        -- characteristics of the combined back face."
        Spec.assertEqWith s "the melded permanent shows the combined back face" (Game.cardOf meldedId after) (Just (Printing.card piker))
        -- CR 701.42a's "single object represented by two cards", read back through
        -- the classifier CR 202.3c, CR 712.21 and CR 701.27g share. Two DISTINCT
        -- printings, in the order the objects were named.
        Spec.assertEqWith
          s
          "both cards represent it"
          (fmap (Maybe.mapMaybe (\pid -> fmap Printing.card (Game.printingOf pid after)) . Foldable.toList . Game.componentsOf . Object.source) (Game.lookupObject meldedId after))
          (Just [Printing.card battlements, Printing.card garrison])
        Spec.assertEqWith s "owned by the shared owner (CR 701.42b)" (fmap Object.owner (Game.lookupObject meldedId after)) (Just S.alice)
        Spec.assertEqWith s "and controlled by them (CR 109.4c)" (Projection.controllerOf meldedId after) (Just S.alice)
      other -> Spec.assertFailure s ("expected exactly one permanent, got " <> show (length other))
    -- The cards stop being objects: one permanent, not two cards beside it.
    Spec.assertEqWith s "nothing is left in exile" (Game.zoneMembers Zone.Exile S.alice after) []
    Spec.assertEqWith s "the Battlements' own id is gone" (fmap Object.owner (Game.lookupObject bId after)) Nothing
    Spec.assertEqWith s "and the Garrison's" (fmap Object.owner (Game.lookupObject gId after)) Nothing
  -- CR 701.42b: "tokens, cards that aren't meld cards, or meld cards that don't
  -- form a meld pair can't be melded", and CR 701.42c: "if an effect instructs a
  -- player to meld objects that can't be melded, they stay in their current zone"
  -- -- rule 701.42c's own Graf Rats example. The board differs from the case above
  -- in ONE thing: the counterpart is a Goblin Piker, whose layout is not Meld.
  Spec.it s "CR 701.42b/701.42c a card that is not a meld card melds nothing, and both stay put" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    piker <- S.printingOf s registry "Goblin Piker"
    let (bId, pId, after) = melded battlements piker piker
    Spec.assertEqWith s "nothing entered the battlefield" (Set.size (GameState.battlefield after)) 0
    Spec.assertEqWith s "both cards are still in exile" (List.sort (Game.zoneMembers Zone.Exile S.alice after)) (List.sort [bId, pId])
  -- CR 712.14c: "those cards enter the battlefield as a single permanent with
  -- their back faces up" -- an ENTRY, so CR 616.1's entry replacement loop must
  -- run over it. bob's Kismet rewrites an opponent's entering creature to tapped
  -- (CR 614.1d), and it is the only thing separating this board from the untapped
  -- control beside it.
  Spec.it s "CR 616.1 the melded permanent runs the entry replacement loop: Kismet taps it" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    piker <- S.printingOf s registry "Goblin Piker"
    kismet <- S.printingOf s registry "Kismet"
    let tappedIn base =
          let after = thirdOf (meldedOn base battlements garrison piker)
           in case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield base)) of
                -- The permanent that was not there before: Kismet is on the
                -- battlefield too, and only the arrival is under test.
                [arrived] -> fmap Object.tapped (Game.lookupObject arrived after)
                _ -> Nothing
        withKismet = snd (S.addCreature kismet S.bob (Setup.emptyGame S.bothPlayers))
    Spec.assertEqWith s "Kismet's opponent-entry rewrite reached it" (tappedIn withKismet) (Just TapState.Tapped)
    Spec.assertEqWith s "and without Kismet the same meld enters untapped (CR 110.5b)" (tappedIn (Setup.emptyGame S.bothPlayers)) (Just TapState.Untapped)
  -- The other half of an entry, one rule over: CR 603.6a's enters-the-battlefield
  -- triggers scan the event the meld records. alice's Soul Warden gains her 1 life
  -- when a creature that is not itself enters, and the trigger is taken all the way
  -- to resolution rather than read off the log.
  Spec.it s "CR 603.6a an enters-the-battlefield trigger sees the melded permanent" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    piker <- S.printingOf s registry "Goblin Piker"
    warden <- S.printingOf s registry "Soul Warden"
    let base = snd (S.addCreature warden S.alice (Setup.emptyGame S.bothPlayers))
        (_, _, after) = meldedOn base battlements garrison piker
        settled = S.runPure S.identityAnswer after (do Engine.settleForPriority; Stack.resolveTop)
    Spec.assertEqWith s "alice gained exactly 1 life" (S.lifeOf S.alice settled) (Just 21)
    -- The control: no meld, no life. Without it the case would pass on a board
    -- where the Warden had triggered on something else entirely.
    Spec.assertEqWith
      s
      "and a board where nothing melded leaves her at 20"
      (S.lifeOf S.alice (S.runPure S.identityAnswer base (do Engine.settleForPriority; Stack.resolveTop)))
      (Just 20)

-- Both cards exiled and owned by alice, bound to the slot the exile would have
-- bound, then melded into `into` -- the shape Hanweir Battlements' "exile them,
-- then meld them into Hanweir, the Writhing Township" reaches this opcode in.
meldedOn :: GameState.GameState -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
meldedOn base one two into =
  let (firstId, g1) = S.addExiledCard one S.alice base
      (secondId, g2) = S.addExiledCard two S.alice g1
      slot = SlotName.MkSlotName (Text.pack "melding")
      bound = Map.singleton slot (Set.fromList [Recipient.ToObject firstId, Recipient.ToObject secondId])
      effect = Effect.Meld (Meld.MkMeld (ObjectRef.InSlot slot) (Printing.card into))
   in (firstId, secondId, S.runPure S.identityAnswer g2 (Resolve.applyEffect S.noSource S.noSource S.alice bound Map.empty effect))

melded :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
melded = meldedOn (Setup.emptyGame S.bothPlayers)

thirdOf :: (a, b, c) -> c
thirdOf (_, _, c) = c
