{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 712.4's meld cards: Pawl.Types.Layout's Meld arm and the
-- Pawl.Engine.Card functions that read it. A meld card carries its front face
-- alone (CR 712.4b), so what this spec pins first is that the front face plays
-- exactly as a one-faced card's does -- the mana ability and the targeted grant
-- printed on Hanweir Battlements, reached through Pawl.Engine.Cost.tapForMana
-- and Pawl.Engine.Activate.activateAbility. Then the melding ability itself --
-- Pawl.Types.Effect's Meld opcode, Pawl.Engine.Event.meld and the CR 608.2d
-- choice its exile makes -- driven through the printed card.
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
import qualified Pawl.Types.CardName as CardName
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
import qualified Pawl.Types.Phase as Phase
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
  -- the melding ability has its own cases above, and what is proven here is the
  -- opcode on its own. Both halves of the pool's only meld pair sit in exile,
  -- where the card's own "exile them" puts them, and the slot naming them is a
  -- slot such an exile would have bound (CR 400.7j).
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
        Spec.assertEqWith s "and controlled by the player the effect instructed (CR 110.2a)" (Projection.controllerOf meldedId after) (Just S.alice)
      other -> Spec.assertFailure s ("expected exactly one permanent, got " <> show (length other))
    -- The cards stop being objects: one permanent, not two cards beside it.
    Spec.assertEqWith s "nothing is left in exile" (Game.zoneMembers Zone.Exile S.alice after) []
    Spec.assertEqWith s "the Battlements' own id is gone" (fmap Object.owner (Game.lookupObject bId after)) Nothing
    Spec.assertEqWith s "and the Garrison's" (fmap Object.owner (Game.lookupObject gId after)) Nothing
    -- CR 608.2h: each card ceased in a public zone it was expected to be in, so
    -- what it WAS is still answerable under the id it had. The melding ability's
    -- own source is one of these two, so a later clause of that same resolution
    -- reads through this.
    Spec.assertEqWith s "CR 608.2h the Battlements' last known card survives its id" (Game.cardOfWithLastKnown bId after) (Just (Printing.card battlements))
    Spec.assertEqWith s "and the Garrison's" (Game.cardOfWithLastKnown gId after) (Just (Printing.card garrison))
  -- CR 701.42a's verb: "put them ONTO the battlefield", which a permanent already
  -- there cannot be. The board differs from the positive case in ONE thing -- the
  -- Battlements is on the battlefield rather than in exile -- and the refusal is
  -- rule 701.42c's, so the Garrison is left in exile and the Battlements is left
  -- alone rather than deleted with no zone change (CR 603.6c).
  Spec.it s "CR 701.42a a card already on the battlefield cannot be put onto it, so nothing melds" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (bId, g1) = S.addCreature battlements S.alice base
        (gId, g2) = S.addExiledCard garrison S.alice g1
        slot = SlotName.MkSlotName (Text.pack "melding")
        bound = Map.singleton slot (Set.fromList [Recipient.ToObject bId, Recipient.ToObject gId])
        effect = Effect.Meld (Meld.MkMeld (ObjectRef.InSlot slot) (Printing.card piker))
        after = S.runPure S.identityAnswer g2 (Resolve.applyEffect S.noSource S.noSource S.alice bound Map.empty effect)
    Spec.assertEqWith s "the Garrison is still in exile" (Game.zoneMembers Zone.Exile S.alice after) [gId]
    Spec.assertEqWith s "the Battlements is still the only permanent" (Set.toList (GameState.battlefield after)) [bId]
  -- CR 701.42b: "tokens, cards that aren't meld cards, or meld cards that don't
  -- form a meld pair can't be melded", and CR 701.42c: "if an effect instructs a
  -- player to meld objects that can't be melded, they stay in their current zone"
  -- -- rule 701.42c's own Graf Rats example. The board differs from the case above
  -- in ONE thing: the counterpart is a Goblin Piker, whose layout is not Meld.
  Spec.it s "CR 701.42b/701.42c a card that is not a meld card melds nothing, and both stay put" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    piker <- S.printingOf s registry "Goblin Piker"
    let (bId, pId, after) = melded battlements piker piker
    Spec.assertEqWith s "both cards are still in exile" (List.sort (Game.zoneMembers Zone.Exile S.alice after)) (List.sort [bId, pId])
    Spec.assertEqWith s "nothing entered the battlefield" (Set.size (GameState.battlefield after)) 0
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

  -- The printed ability, end to end: "{3}{R}{R}, {T}: If you both own and control
  -- this land and a creature named Hanweir Garrison, exile them, then meld them
  -- into Hanweir, the Writhing Township." CR 712.4a puts that ability on this
  -- half of the pair, and the five Mountains are what pay for it.
  --
  -- The exile is the CARD's instruction and the meld is CR 701.42a's, which is the
  -- split that makes CR 701.42c come out right in the token case below: when the
  -- meld refuses, the cards are already where the exile left them.
  --
  -- Not implemented: the printed "exile them" is ONE instruction over both
  -- permanents (CR 608.2f), where the card file writes two Pawl.Types.Effect's
  -- MoveToZone effects and the meld reads what they exiled rather than a slot
  -- (#2498). Nothing in the pool tells the two batches from one.
  Spec.it s "CR 701.42a the melding ability exiles the pair and puts one permanent onto the battlefield" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    let (bId, g1) = S.addCreature battlements S.alice (Setup.emptyGame S.bothPlayers)
        (gId, g2) = S.addCreature garrison S.alice g1
        board = readyFor mountain g2
    case Projection.abilitiesOf bId board of
      [_, _, melding] -> do
        let after = S.runPure S.identityAnswer board (do Activate.activateAbility S.alice bId melding; Stack.resolveTop)
        Spec.assertEqWith s "one permanent named Hanweir, the Writhing Township" (S.countOnBattlefieldByName townshipName S.alice after) 1
        case namedTownship after (Game.zoneMembers Zone.Battlefield S.alice after) of
          [meldedId] -> do
            -- CR 712.8g: the melded permanent has only the combined back face's
            -- characteristics, which is where the 7/4 and the two keywords are.
            Spec.assertEqWith s "CR 712.8g it is the combined back face's 7/4" (S.powerToughnessOf meldedId after) (Just (7, 4))
            Spec.assertBool s (Projection.hasKeyword Keyword.Trample meldedId after) "with trample"
            Spec.assertBool s (Projection.hasKeyword Keyword.Haste meldedId after) "and haste"
            -- CR 701.42a's "single object represented by two cards", read back
            -- through the classifier CR 202.3c and CR 712.21 share.
            Spec.assertEqWith
              s
              "CR 701.42a both cards represent it"
              (fmap (Maybe.mapMaybe (\pid -> fmap Printing.card (Game.printingOf pid after)) . Foldable.toList . Game.componentsOf . Object.source) (Game.lookupObject meldedId after))
              (Just [Printing.card garrison, Printing.card battlements])
          other -> Spec.assertFailure s ("expected exactly one melded permanent, got " <> show (length other))
        -- Neither original is anywhere: the exile the card asked for happened, and
        -- the meld consumed what it found there.
        Spec.assertEqWith s "the land's own id is gone" (fmap Object.owner (Game.lookupObject bId after)) Nothing
        Spec.assertEqWith s "and the Garrison's" (fmap Object.owner (Game.lookupObject gId after)) Nothing
        Spec.assertEqWith s "and nothing is left in exile" (Game.zoneMembers Zone.Exile S.alice after) []
        Spec.assertEqWith s "setup: the pair was on the battlefield before the ability resolved" (S.countOnBattlefieldByName townshipName S.alice board) 0
      abilities -> Spec.assertFailure s ("expected three activated abilities on Hanweir Battlements, got " <> show (length abilities))
  -- CR 608.2d: "a creature named Hanweir Garrison" is a choice announced while
  -- the effect is applied, and with two of them it is a real one. The two boards
  -- are identical and differ only in the ANSWER, so a run that ignored the
  -- decider would give them the same outcome.
  Spec.it s "CR 608.2d with two Hanweir Garrisons the resolving controller says which one melds" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    let (bId, g1) = S.addCreature battlements S.alice (Setup.emptyGame S.bothPlayers)
        (firstG, g2) = S.addCreature garrison S.alice g1
        (secondG, g3) = S.addCreature garrison S.alice g2
        board = readyFor mountain g3
    case Projection.abilitiesOf bId board of
      [_, _, melding] -> do
        let melding_ chosen = S.runPure (choosing chosen) board (do Activate.activateAbility S.alice bId melding; Stack.resolveTop)
            survivor picked kept = fmap Object.zone (Game.lookupObject kept (melding_ picked))
            taken picked = fmap Object.zone (Game.lookupObject picked (melding_ picked))
        Spec.assertEqWith s "naming the first Garrison leaves the second on the battlefield" (survivor firstG secondG) (Just Zone.Battlefield)
        Spec.assertEqWith s "and naming the second leaves the first there instead" (survivor secondG firstG) (Just Zone.Battlefield)
        Spec.assertEqWith s "the Garrison that was named is gone" (taken firstG) Nothing
        Spec.assertEqWith s "in either run" (taken secondG) Nothing
        Spec.assertEqWith s "and either way exactly one melded permanent arrived" (fmap (S.countOnBattlefieldByName townshipName S.alice . melding_) [firstG, secondG]) [1, 1]
      abilities -> Spec.assertFailure s ("expected three activated abilities on Hanweir Battlements, got " <> show (length abilities))
  -- The printed condition, which is a gate on the whole clause: "If you both own
  -- AND control this land and a creature named Hanweir Garrison". Two boards,
  -- each differing from the melding board above in ONE thing -- no Garrison at
  -- all, and a Garrison alice controls but bob owns -- and on neither does the
  -- land exile itself for nothing. The second board is what the word "own" buys:
  -- a control-only reading would meld bob's card away.
  Spec.it s "CR 608.2c the ability does nothing without a Hanweir Garrison you both own and control" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    let resolvedOn extra =
          let (bId, g1) = S.addCreature battlements S.alice (Setup.emptyGame S.bothPlayers)
              board = readyFor mountain (extra g1)
           in case Projection.abilitiesOf bId board of
                [_, _, melding] -> (Just bId, S.runPure S.identityAnswer board (do Activate.activateAbility S.alice bId melding; Stack.resolveTop))
                _ -> (Nothing, board)
        borrowed g1 =
          let (theirs, g2) = S.addCreature garrison S.bob g1
           in S.giveControl theirs S.alice g2
        stayed (mBId, after) = (fmap (\bId -> fmap Object.zone (Game.lookupObject bId after)) mBId, S.countOnBattlefieldByName townshipName S.alice after)
    Spec.assertEqWith s "with no Garrison anywhere the land is untouched and nothing melded" (stayed (resolvedOn id)) (Just (Just Zone.Battlefield), 0)
    Spec.assertEqWith s "and a Garrison alice controls but does not own is not one the ability may name" (stayed (resolvedOn borrowed)) (Just (Just Zone.Battlefield), 0)
  -- CR 701.42c's own example, with Hanweir in place of Midnight Scavengers: the
  -- counterpart is a TOKEN copy of Hanweir Garrison, which CR 701.42b bars from
  -- melding (CR 108.2 makes a token no card at all). The card's own "exile them"
  -- still happens, so both objects reach exile, the meld writes nothing, and CR
  -- 111.8 removes the token at the next state-based check.
  Spec.it s "CR 701.42b/701.42c a token counterpart melds nothing, and the land stays exiled" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    let (bId, g1) = S.addCreature battlements S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addToken (Printing.card garrison) S.alice g1
        board = readyFor mountain g2
    case Projection.abilitiesOf bId board of
      [_, _, melding] -> do
        let after = S.runPure S.identityAnswer board (do Activate.activateAbility S.alice bId melding; Stack.resolveTop)
        Spec.assertEqWith s "CR 701.42c both objects stay in the zone the exile left them in" (List.sort (exileNames after)) (List.sort [S.nameOf (Printing.card battlements), S.nameOf (Printing.card garrison)])
        Spec.assertEqWith s "CR 701.42b nothing melded" (S.countOnBattlefieldByName townshipName S.alice after) 0
        -- CR 111.8: a token that has left the battlefield ceases to exist the next
        -- time state-based actions are checked. The land is a card and stays.
        Spec.assertEqWith s "CR 111.8 the token ceases and the land is left exiled alone" (exileNames (S.settleSba after)) [S.nameOf (Printing.card battlements)]
      abilities -> Spec.assertFailure s ("expected three activated abilities on Hanweir Battlements, got " <> show (length abilities))

-- The combined back face's name, which is what a melded permanent answers to
-- (CR 712.8g) and the one thing the three gameplay cases above count.
townshipName :: CardName.CardName
townshipName = CardName.MkCardName (Text.pack "Hanweir, the Writhing Township")

namedTownship :: GameState.GameState -> [ObjectId.ObjectId] -> [ObjectId.ObjectId]
namedTownship gs = filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just townshipName)

-- What alice owns in exile, by name. Each exiled object is a CR 400.7 incarnation
-- with an id of its own, so the ids the board started with cannot be compared
-- against; the names are what the rule's own example talks about.
exileNames :: GameState.GameState -> [CardName.CardName]
exileNames gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Exile S.alice gs)

-- alice holding priority in her own main phase with five untapped Mountains --
-- the {3}{R}{R} the melding ability costs, and nothing else the ability could
-- name.
readyFor :: Printing.Printing -> GameState.GameState -> GameState.GameState
readyFor mountain gs =
  (S.landsFor mountain S.alice 5 gs)
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = S.alice,
      GameState.priority = Just S.alice
    }

-- CR 608.2d's answer pinned by IDENTITY rather than by index: the named permanent
-- if the engine offered it, and every other prompt left to the identity answerer.
-- Filtering the offer is what makes a run that never asked distinguishable from
-- one that did -- an answerer building its own id could not tell them apart.
choosing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
choosing wanted p = case p of
  Prompt.ChoosePermanent _ _ _ candidates
    | List.elem wanted (NonEmpty.toList candidates) -> wanted
  _ -> S.identityAnswer p

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
