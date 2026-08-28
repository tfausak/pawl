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
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CommandZoneDecision as CommandZoneDecision
import qualified Pawl.Types.Daytime as Daytime
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
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
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
        Spec.assertEqWith s "owned by the shared owner of the cards that represent it (CR 110.2)" (fmap Object.owner (Game.lookupObject meldedId after)) (Just S.alice)
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
  -- (#2498).
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

  -- CR 202.3c: "the mana value of a melded permanent is calculated as though it
  -- had the combined mana cost of the front faces of each card that represents
  -- it" -- Hanweir Garrison's {2}{R} and Hanweir Battlements' none, which is 3.
  -- CR 202.3a is what admits the second half: a melded permanent is exempted by
  -- name from the 0 an object with no mana cost otherwise has.
  --
  -- Read by a CARD rather than off the projection: Void Winnower's "your
  -- opponents can't block with creatures with even mana values" is CR 509.1b
  -- narrowed by a mana value, so a melded permanent that answered 0 -- the number
  -- its own combined face prints, and the number every reading but CR 202.3c's
  -- gives it -- could not block. bob's Winnower and alice's Goblin Piker ({1}{R},
  -- 2) are the pair that makes the parity readable in both directions.
  Spec.it s "CR 202.3c a melded permanent's mana value is its components' front faces combined" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    winnower <- S.printingOf s registry "Void Winnower"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, withWinnower) = S.addCreature winnower S.bob (Setup.emptyGame S.bothPlayers)
        (pikerId, base) = S.addCreature piker S.alice withWinnower
        (mMelded, after) = meldedThrough base battlements garrison mountain
    case mMelded of
      Just meldedId -> do
        Spec.assertBool s (Combat.canBlock S.alice meldedId after) "CR 202.3c its mana value is 3, an odd one the Winnower does not stop"
        -- The same restriction biting on the same board, which is what makes the
        -- assertion above a parity read rather than a Winnower that never applied.
        Spec.assertBool s (not (Combat.canBlock S.alice pikerId after)) "and the Piker's {1}{R} is even, so the Winnower is live here"
        -- The number itself, after the behaviour that depends on it.
        Spec.assertEqWith s "the projected mana value" (PC.manaValue (Projection.project meldedId after)) (Just 3)
      Nothing -> Spec.assertFailure s "the pair should have melded"
  -- CR 202.3c's second sentence: "if a permanent is a copy of a melded permanent
  -- (even if that copy is represented by two other meld cards), the mana value of
  -- the copy is 0" -- CR 712.8g says it again. So the melded permanent and its
  -- copy report DIFFERENT numbers, and the Winnower reads both off one board:
  -- 3 is odd and blocks, the copy's 0 is even and cannot. Cackling Counterpart
  -- ("create a token that's a copy of target creature you control") is the copy,
  -- and the two Islands are what pay its {1}{U}.
  Spec.it s "CR 202.3c a copy of a melded permanent has mana value 0" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    winnower <- S.printingOf s registry "Void Winnower"
    counterpart <- S.printingOf s registry "Cackling Counterpart"
    let base = snd (S.addCreature winnower S.bob (Setup.emptyGame S.bothPlayers))
        (mMelded, after) = meldedThrough base battlements garrison mountain
    case mMelded of
      Just meldedId -> do
        let (staged, spellId) = S.handOne counterpart (S.landsFor island S.alice 2 after)
            copied = S.runPure (aimedAt meldedId) staged (do S.cast S.alice spellId; Stack.resolveTop)
        case filter (`Game.isToken` copied) (Set.toList (GameState.battlefield copied)) of
          [tokenId] -> do
            Spec.assertBool s (not (Combat.canBlock S.alice tokenId copied)) "CR 202.3c the copy's mana value is 0, which is even"
            Spec.assertBool s (Combat.canBlock S.alice meldedId copied) "while the permanent it copied still blocks at 3"
            Spec.assertEqWith s "and the copy is the melded permanent in every other respect" (Projection.namesOf tokenId copied) (Set.singleton townshipName)
          tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))
      Nothing -> Spec.assertFailure s "the pair should have melded"
  -- CR 701.27g: "an object represented by more than one card, such as a melded or
  -- merged permanent, is never considered a transformed permanent, even if it has
  -- components that are back face up." Mutagen Connoisseur's "for each
  -- transformed permanent you control" is the card that asks, as it does
  -- throughout Pawl.TransformSpec's TransformedPermanent group; the Thraben
  -- Gargoyle beside it is turned over by the same instruction and IS one, so the
  -- Connoisseur's power separates a tally that counts the melded permanent (2)
  -- from the rule's answer (1).
  Spec.it s "CR 701.27g a melded permanent is not a transformed permanent" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    connoisseur <- S.printingOf s registry "Mutagen Connoisseur"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (connoisseurId, withConnoisseur) = S.addCreature connoisseur S.alice (Setup.emptyGame S.bothPlayers)
        (gargoyleId, base) = S.addCreature gargoyle S.alice withConnoisseur
        (mMelded, after) = meldedThrough base battlements garrison mountain
    case mMelded of
      Just meldedId -> do
        let turned = transforming [gargoyleId, meldedId] after
        Spec.assertEqWith s "CR 701.27g the Connoisseur counts the Gargoyle alone" (S.powerToughnessOf connoisseurId turned) (Just (1, 5))
        -- The control: the instruction really ran, and the permanent that CAN
        -- turn over did, so the tally above is 1 rather than 0 by accident.
        Spec.assertEqWith s "CR 701.27a the Gargoyle turned over" (Projection.namesOf gargoyleId turned) (Set.singleton (CardName.MkCardName (Text.pack "Stonewing Antagonizer")))
        Spec.assertEqWith s "and the melded permanent is still on the battlefield to be counted" (S.countOnBattlefieldByName townshipName S.alice turned) 1
      Nothing -> Spec.assertFailure s "the pair should have melded"
  -- CR 712.4c: "unlike other double-faced cards, meld cards cannot be transformed
  -- or converted. Any instructions to do so are ignored", and CR 712.9 excludes
  -- meld cards by name from the permanents that can turn over. The instruction is
  -- Pawl.Types.Effect's Transform over a slot naming the melded permanent, which
  -- is the shape any card's "transform target permanent" reaches the opcode in.
  --
  -- The refusal is OVER-DETERMINED on this board and the case is a regression
  -- fence rather than a proof: the pool's combined face prints one face, so
  -- Pawl.Engine.Card.turnedOver would decline it as a one-faced card whatever CR
  -- 712.4c said. The case below is the one that separates the two readings.
  Spec.it s "CR 712.4c/712.9 the melded permanent ignores an instruction to transform" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    let (mMelded, after) = meldedThrough (Setup.emptyGame S.bothPlayers) battlements garrison mountain
    case mMelded of
      Just meldedId -> do
        let turned = transforming [meldedId] after
        Spec.assertEqWith s "its name is the combined face's still" (Projection.namesOf meldedId turned) (Set.singleton townshipName)
        Spec.assertEqWith s "CR 712.8g and so is its 7/4" (S.powerToughnessOf meldedId turned) (Just (7, 4))
        Spec.assertEqWith s "and its type line" (PC.subtypes (Projection.project meldedId turned)) (Set.fromList [Subtype.Eldrazi, Subtype.Ooze])
      Nothing -> Spec.assertFailure s "the pair should have melded"
  -- CR 712.4c asked of the OBJECT rather than of a card, which is the whole of
  -- the rule: the melded permanent's own card is the interned combined face (CR
  -- 712.8g), and nothing about that card says what pair it came from. So the
  -- board is the opcode-level meld with a DOUBLE-FACED card standing in for the
  -- combined face -- Pawl.Engine.Card.turnedOver would turn a Thraben Gargoyle
  -- over on sight -- and the only thing refusing is the two cards representing
  -- the permanent.
  --
  -- The stand-in is the same liberty the opcode cases above take (the combined
  -- face is card DATA the opcode carries); no printed meld pair combines into a
  -- double-faced card, so this is the board that makes the rule readable rather
  -- than a board Magic can reach.
  Spec.it s "CR 712.4c a melded permanent refuses the turn its own card would allow" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (_, _, after) = melded battlements garrison gargoyle
    case Set.toList (GameState.battlefield after) of
      [meldedId] -> do
        let turned = transforming [meldedId] after
            (loneId, alone) = S.addCreature gargoyle S.alice (Setup.emptyGame S.bothPlayers)
        Spec.assertEqWith s "CR 712.4c the melded permanent did not turn over" (Projection.namesOf meldedId turned) (Set.singleton (CardName.MkCardName (Text.pack "Thraben Gargoyle")))
        Spec.assertEqWith s "CR 712.8g nor did its 2/2 become the back face's 4/2" (S.powerToughnessOf meldedId turned) (Just (2, 2))
        Spec.assertEqWith s "nor its type line" (PC.subtypes (Projection.project meldedId turned)) (Set.singleton Subtype.Gargoyle)
        -- The pair: the same card, one card representing it, same instruction.
        -- CR 701.27a turns that one over, so the refusal above is the meld's.
        Spec.assertEqWith s "CR 701.27a an ordinary Thraben Gargoyle turns over" (Projection.namesOf loneId (transforming [loneId] alone)) (Set.singleton (CardName.MkCardName (Text.pack "Stonewing Antagonizer")))
      other -> Spec.assertFailure s ("expected exactly one permanent, got " <> show (length other))

  -- CR 701.27g's second exclusion where nothing else answers: a melded permanent
  -- that is BACK FACE UP. Daybound's first static ability (CR 702.145b: "if it
  -- is night and this permanent is represented by a double-faced card, it enters
  -- transformed") is the one entry rewrite that writes Object.face -- CR 616.1d
  -- ranks it, and it is not CR 712.13a, which governs only a resolving
  -- double-faced spell. CR 616.1
  -- runs over a melded permanent like any other entry -- so a combined face
  -- printing daybound enters with its back face up, `Game.isFrontFaceUp` answers
  -- False, and the cards representing it are the only thing left excluding it.
  -- That is the rule's own "even if it has components that are back face up",
  -- reached from the other side.
  --
  -- The combined face is Tovolar, Dire Overlord // Tovolar, the Midnight Scourge,
  -- the same stand-in liberty the CR 712.4c case takes: no printed meld pair
  -- combines into a daybound double-faced card, and the opcode carries the
  -- combined face as card data. The Thraben Gargoyle beside it is turned over by
  -- an ordinary instruction and IS a transformed permanent, so the Connoisseur's
  -- power separates a tally that counts the melded permanent (2) from the rule's
  -- answer (1).
  Spec.it s "CR 701.27g a melded permanent that entered with its back face up is still not one" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    connoisseur <- S.printingOf s registry "Mutagen Connoisseur"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (connoisseurId, withConnoisseur) = S.addCreature connoisseur S.alice (Setup.emptyGame S.bothPlayers)
        (gargoyleId, withGargoyle) = S.addCreature gargoyle S.alice withConnoisseur
        night = withGargoyle {GameState.daytime = Just Daytime.Night}
        (_, _, after) = meldedOn night battlements garrison tovolar
    case filter (\oid -> oid /= connoisseurId && oid /= gargoyleId) (Set.toList (GameState.battlefield after)) of
      [meldedId] -> do
        let turned = transforming [gargoyleId] after
        Spec.assertEqWith s "CR 701.27g the Connoisseur counts the Gargoyle alone" (S.powerToughnessOf connoisseurId turned) (Just (1, 5))
        -- What makes that assertion mean anything: the melded permanent really is
        -- back face up, so the first exclusion has stopped answering for it.
        Spec.assertEqWith s "CR 702.145b and the melded permanent entered transformed" (fmap Object.face (Game.lookupObject meldedId turned)) (Just (Just (CardName.MkCardName (Text.pack "Tovolar, the Midnight Scourge"))))
        -- The control: an ordinary permanent back face up on the same board is
        -- counted, so the 1 is a tally rather than a floor.
        Spec.assertEqWith s "CR 701.27a the Gargoyle is the one it counts" (Projection.namesOf gargoyleId turned) (Set.singleton (CardName.MkCardName (Text.pack "Stonewing Antagonizer")))
      other -> Spec.assertFailure s ("expected exactly one melded permanent, got " <> show (length other))
  -- CR 712.21 and the rule's own Example, with Hanweir in place of Chittering
  -- Host: "one permanent leaves the battlefield and two cards are put into the
  -- appropriate zone", so an ability that triggers "whenever a creature dies"
  -- triggers ONCE while two cards arrive in the graveyard.
  --
  -- Meren of Clan Nel Toth is that ability -- "whenever another creature you
  -- control dies, you get an experience counter" -- and her counter is what
  -- separates once from twice. The two halves of the answer discriminate in
  -- opposite directions: a split that minted one incarnation would leave one card
  -- in the graveyard, and a split that recorded one departure per card would give
  -- alice two counters.
  Spec.it s "CR 712.21 a melded permanent dies as one permanent and arrives as two cards" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    meren <- S.printingOf s registry "Meren of Clan Nel Toth"
    let (merenId, base) = S.addCreature meren S.alice (Setup.emptyGame S.bothPlayers)
        (mMelded, board) = meldedThrough base battlements garrison mountain
    case mMelded of
      Nothing -> Spec.assertFailure s "expected the melding ability to put one permanent onto the battlefield"
      Just meldedId -> do
        let dead = S.runPure S.identityAnswer board (Event.destroy Regenerability.Regenerable [meldedId])
            settled = S.runPure S.identityAnswer dead Engine.settleForPriority
            after = S.runPure S.identityAnswer settled (Stack.resolveTop >> Stack.resolveTop)
        Spec.assertEqWith
          s
          "CR 712.21 two cards are put into alice's graveyard"
          (List.sort (graveyardNames dead))
          (List.sort [CardName.MkCardName (Text.pack "Hanweir Battlements"), CardName.MkCardName (Text.pack "Hanweir Garrison")])
        Spec.assertEqWith s "CR 712.21 and Meren saw exactly one creature die" (S.playerCounterOf PlayerCounterKind.Experience S.alice after) 1
        -- The proxy behind that count, kept AFTER it: one trigger reached the
        -- stack, so the 1 above is not two triggers of which one went unresolved.
        Spec.assertEqWith s "one death trigger reached the stack" (length (GameState.stack settled)) 1
        Spec.assertEqWith s "the melded permanent itself is gone" (Game.lookupObject meldedId dead) Nothing
        Spec.assertEqWith s "setup: alice held no experience counters before it died" (S.playerCounterOf PlayerCounterKind.Experience S.alice board) 0
        Spec.assertEqWith s "setup: Meren was on the battlefield to see it" (fmap Object.zone (Game.lookupObject merenId after)) (Just Zone.Battlefield)
        Spec.assertEqWith s "setup: alice's graveyard was empty before it died" (graveyardNames board) []
  -- CR 712.21c: "If an effect can find the new object that a melded permanent
  -- becomes as it leaves the battlefield, it finds both cards. (See rule 400.7.)
  -- If that effect causes actions to be taken upon those cards, the same actions
  -- are taken upon each of them." The rule's own Mimic Vat Example is this shape
  -- -- one trigger, one choice, both cards acted on.
  --
  -- Promise of Tomorrow is the pool's producer: "whenever a creature you control
  -- dies, exile IT", where "it" is CR 400.7e's `became` slot. The trigger fires
  -- ONCE (CR 712.21's first clause) and exiles TWO cards, so the two halves of
  -- the assertion discriminate in opposite directions -- a payload that acted on
  -- the first arrival alone would leave Hanweir Battlements in the graveyard.
  Spec.it s "CR 712.21c a trigger that exiles what the melded permanent became exiles both cards" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    promise <- S.printingOf s registry "Promise of Tomorrow"
    let (_, base) = S.addCreature promise S.alice (Setup.emptyGame S.bothPlayers)
        (mMelded, board) = meldedThrough base battlements garrison mountain
        bothNames = List.sort [CardName.MkCardName (Text.pack "Hanweir Battlements"), CardName.MkCardName (Text.pack "Hanweir Garrison")]
    case mMelded of
      Nothing -> Spec.assertFailure s "expected the melding ability to put one permanent onto the battlefield"
      Just meldedId -> do
        let dead = S.runPure S.identityAnswer board (Event.destroy Regenerability.Regenerable [meldedId])
            settled = S.runPure S.identityAnswer dead Engine.settleForPriority
            after = S.runPure S.identityAnswer settled Stack.resolveTop
        Spec.assertEqWith s "CR 712.21c the trigger exiled both cards" (List.sort (exileNames after)) bothNames
        Spec.assertEqWith s "and left neither behind in the graveyard" (graveyardNames after) []
        -- The proxies behind that, both AFTER it: the trigger really did fire,
        -- exactly once, and both cards really were in the graveyard for it to
        -- find -- so the exile above is the payload's work and not the funnel's.
        Spec.assertEqWith s "one trigger reached the stack" (length (GameState.stack settled)) 1
        Spec.assertEqWith s "setup: both cards were in the graveyard when it resolved" (List.sort (graveyardNames settled)) bothNames
        Spec.assertEqWith s "setup: exile was empty once the meld had consumed the pair" (exileNames settled) []
  -- CR 903.9a over CR 712.21's split, and NOT CR 903.9c -- that rule governs only
  -- the CR 903.9b replacement (the hand and library redirect), which pawl does not
  -- make for a melded permanent (#2265). What runs here is rule 903.9a's
  -- state-based action: "if a commander is in a graveyard or in exile and that
  -- object was put into that zone since the last time state-based actions were
  -- checked, its owner may put it into the command zone." The split has already
  -- made each component an object in that graveyard, so the offer is asked of both
  -- and CR 903.3's designation -- an attribute of the CARD -- picks one.
  --
  -- BOTH DESIGNATIONS, which is the whole case rather than belt and braces: the
  -- two boards differ in nothing but which half of the pair alice's deck named,
  -- so an engine that read only the FIRST arriving card would answer them
  -- differently. Hanweir Garrison is the first component the meld recorded, so
  -- the Battlements board is the one such an engine gets wrong.
  Spec.it s "CR 903.9a a melded commander's own card goes to the command zone, whichever half melded first" $ do
    battlements <- S.printingOf s registry "Hanweir Battlements"
    garrison <- S.printingOf s registry "Hanweir Garrison"
    mountain <- S.printingOf s registry "Mountain"
    let (mMelded, board) = meldedThrough (Setup.emptyGame S.bothPlayers) battlements garrison mountain
    case mMelded >>= \meldedId -> fmap ((,) meldedId . Game.componentsOf . Object.source) (Game.lookupObject meldedId board) of
      Nothing -> Spec.assertFailure s "expected the melding ability to put one permanent onto the battlefield"
      Just (meldedId, components) -> case Foldable.toList components of
        [firstPid, secondPid] -> do
          let designating pid gs = gs {GameState.players = Map.adjust (\p -> p {Player.commander = Just pid}) S.alice (GameState.players gs)}
              run pid = S.runPure reclaiming (designating pid board) (Event.destroy Regenerability.Regenerable [meldedId] >> Engine.settleForPriority)
              nameOf pid = fmap (S.nameOf . Printing.card) (Game.printingOf pid board)
              commandNames gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Set.toList (GameState.command gs))
          Spec.assertEqWith s "CR 903.9a naming the first component puts THAT card into the command zone" (commandNames (run firstPid)) (Maybe.maybeToList (nameOf firstPid))
          Spec.assertEqWith s "and its partner is left in the graveyard" (graveyardNames (run firstPid)) (Maybe.maybeToList (nameOf secondPid))
          Spec.assertEqWith s "CR 903.9a naming the second component puts THAT card into the command zone instead" (commandNames (run secondPid)) (Maybe.maybeToList (nameOf secondPid))
          Spec.assertEqWith s "and its partner is the one left in the graveyard" (graveyardNames (run secondPid)) (Maybe.maybeToList (nameOf firstPid))
          -- What makes the pair discriminating: the two components really are two
          -- different cards, and the meld recorded them in this order.
          Spec.assertEqWith s "setup: the components are the Garrison then the Battlements" [nameOf firstPid, nameOf secondPid] [Just (S.nameOf (Printing.card garrison)), Just (S.nameOf (Printing.card battlements))]
        other -> Spec.assertFailure s ("expected two components, got " <> show (length other))

-- CR 701.27a as ONE instruction over the named permanents (CR 608.2f), through
-- the opcode a card's "transform target permanent" reaches: the slot the effect
-- reads is the one such a card's target would have bound.
transforming :: [ObjectId.ObjectId] -> GameState.GameState -> GameState.GameState
transforming oids gs =
  let slot = SlotName.MkSlotName (Text.pack "turning")
      bound = Map.singleton slot (Set.fromList (fmap Recipient.ToObject oids))
   in S.runPure S.identityAnswer gs (Resolve.applyEffect S.noSource S.noSource S.alice bound Map.empty (Effect.Transform (ObjectRef.InSlot slot)))

-- alice's Hanweir Battlements and Hanweir Garrison added to `base`, her five
-- Mountains beside them, and the printed melding ability activated and resolved:
-- the melded permanent, and the board it is on. The real activation rather than
-- the opcode, so every case reading it reads a board the game can reach.
meldedThrough :: GameState.GameState -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (Maybe ObjectId.ObjectId, GameState.GameState)
meldedThrough base battlements garrison mountain =
  let (bId, g1) = S.addCreature battlements S.alice base
      (_, g2) = S.addCreature garrison S.alice g1
      board = readyFor mountain g2
   in case Projection.abilitiesOf bId board of
        [_, _, melding] ->
          let after = S.runPure S.identityAnswer board (do Activate.activateAbility S.alice bId melding; Stack.resolveTop)
           in (Maybe.listToMaybe (namedTownship after (Game.zoneMembers Zone.Battlefield S.alice after)), after)
        _ -> (Nothing, board)

-- The name printed on Hanweir Battlements' combined back face, which is what the
-- permanent that pair melds into answers to (CR 712.8g). Spelled once here
-- rather than at each reader, since it is card data no printing in the pool
-- exposes under a name of its own. A meld into a stand-in combined face answers
-- to that card's name instead.
townshipName :: CardName.CardName
townshipName = CardName.MkCardName (Text.pack "Hanweir, the Writhing Township")

namedTownship :: GameState.GameState -> [ObjectId.ObjectId] -> [ObjectId.ObjectId]
namedTownship gs = filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just townshipName)

-- What alice owns in exile, by name. Each exiled object is a CR 400.7 incarnation
-- with an id of its own, so the ids the board started with cannot be compared
-- against; the names are what the rule's own example talks about.
exileNames :: GameState.GameState -> [CardName.CardName]
exileNames gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Exile S.alice gs)

-- exileNames' graveyard twin, for CR 712.21's "two cards are put into the
-- appropriate zone": the names alice's graveyard holds, since each arrival is a
-- CR 400.7 incarnation with an id the board never saw.
graveyardNames :: GameState.GameState -> [CardName.CardName]
graveyardNames gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Graveyard S.alice gs)

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

-- Accepts CR 903.9a's offer; everything else is the identity answerer. The
-- default LEAVES the commander where it is, so the CR 903.9a case has to say so.
reclaiming :: Prompt.Prompt r -> r
reclaiming p = case p of
  Prompt.ReturnCommander {} -> CommandZoneDecision.Returns
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
