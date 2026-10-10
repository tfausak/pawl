{-# LANGUAGE GADTs #-}

-- Covers CR 107.17's ticket counters, CR 103.2d's sheet draw
-- (Pawl.Engine.Setup), and CR 123's stickers: Pawl.Engine.Sticker,
-- Effect.PutSticker (Pawl.Engine.Resolve.Effect), the CR 123.5 write-back in
-- Pawl.Engine.Event, Filter.HasSticker and Filter.Stickered, and
-- TriggerCondition.PlacesSticker.
module Pawl.StickerSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
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
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.NameWords as NameWords
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Sticker as Sticker
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Oracle as Oracle
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.StickerSheets as StickerSheets
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilitySticker as AbilitySticker
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StickerKind as StickerKind
import qualified Pawl.Types.StickerPlacement as StickerPlacement
import qualified Pawl.Types.StickerRef as StickerRef
import qualified Pawl.Types.StickerSheet as StickerSheet
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone

-- Alice active with priority in her precombat main phase.
mainPhaseForAlice :: GameState.GameState -> GameState.GameState
mainPhaseForAlice gs = gs {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}

-- Aetheric Amplifier's second mode, "each kind of counter you have".
secondMode :: Prompt.Prompt r -> r
secondMode p = case p of
  Prompt.ChooseModes {} -> Seq.singleton (ModeIndex.MkModeIndex 1)
  _ -> S.identityAnswer p

-- The four committed sheets, in this order; fewer if one is missing, which
-- each case's first assertion catches.
committedSheets :: IO [StickerSheet.StickerSheet]
committedSheets = do
  root <- StickerSheets.defaultRoot
  loaded <- StickerSheets.loadRoot root
  let byName = Map.fromList [(StickerSheet.name sheet, sheet) | (_, Right sheet) <- loaded]
  pure (Maybe.mapMaybe (\n -> Map.lookup (Text.pack n) byName) ["Night Brushwagg Ringmaster", "Slimy Burrito Illusion", "Contortionist Otter Storm", "Ancestral Hot Dog Minotaur"])

-- One Oracle line, "{TK}{TK} — rest", as its ticket count and its rest.
ticketLine :: Text.Text -> (Natural, Text.Text)
ticketLine line =
  let (cost, rest) = Text.breakOn (Text.pack " \8212 ") line
   in (Natural.length (drop 1 (Text.splitOn (Text.pack "{TK}") cost)), Text.drop 3 rest)

-- Keeps every hand and records each CR 103.2d draw's candidates, answering
-- with the LAST, pinned by position.
sheetDraws :: Prompt.Prompt r -> State.State [[Natural]] r
sheetDraws p = case p of
  Prompt.RandomStickerSheet slots -> do
    State.modify' (NonEmpty.toList slots :)
    pure (NonEmpty.last slots)
  Prompt.DeclareMulligan {} -> pure MulliganDecision.Keep
  _ -> pure (S.identityAnswer p)

-- Setup.newGame over this matchup, with what sheetDraws was asked, in order.
startedWith :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> (GameState.GameState, [[Natural]])
startedWith matchup =
  let ((_, gs), asked) = State.runState (Engine.runGame sheetDraws (Setup.gameWith GameSettings.plain (fmap fst matchup)) (Setup.newGame S.performer matchup)) []
   in (gs, reverse asked)

chosenOf :: PlayerId.PlayerId -> GameState.GameState -> Set.Set Natural
chosenOf pid gs = foldMap Player.chosenStickerSheets (Map.lookup pid (GameState.players gs))

withSheetsDeck :: [StickerSheet.StickerSheet] -> Deck.Deck -> Deck.Deck
withSheetsDeck sheets deck = deck {Deck.stickerSheets = Seq.fromList sheets}

-- alice brings these sheets and has every one chosen, as CR 103.2d leaves
-- three or fewer.
withSheets :: [StickerSheet.StickerSheet] -> GameState.GameState -> GameState.GameState
withSheets sheets gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.stickerSheets = Seq.fromList sheets, Player.chosenStickerSheets = Set.fromList (zipWith const [0 ..] sheets)}) S.alice (GameState.players gs)}

-- alice's first available art sticker on `oid`.
stickerOn :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
stickerOn oid gs = case Sticker.available S.alice (Set.singleton StickerKind.Art) gs of
  ref : _ -> Sticker.put S.alice oid ref Nothing gs
  [] -> gs

-- One of alice's name stickers, by its sheet's position in committedSheets'
-- order and its index among that sheet's name stickers.
nameSticker :: Natural -> Natural -> StickerRef.StickerRef
nameSticker slot i = StickerRef.MkStickerRef {StickerRef.owner = S.alice, StickerRef.sheet = slot, StickerRef.kind = StickerKind.Name, StickerRef.index = i}

-- "Night", "Slimy" and "Otter".
night :: StickerRef.StickerRef
night = nameSticker 0 0

slimy :: StickerRef.StickerRef
slimy = nameSticker 1 0

otter :: StickerRef.StickerRef
otter = nameSticker 2 1

-- The names an object shows, as text.
nameTexts :: ObjectId.ObjectId -> GameState.GameState -> [Text.Text]
nameTexts oid gs = fmap CardName.unwrap (Set.toList (Projection.namesOf oid gs))

-- FILTERS the offered set, so CR 608.2b's re-read finds the target.
namingTarget :: ObjectId.ObjectId -> Prompt.Prompt r -> r
namingTarget oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter ((== Just oid) . Recipient.objectOf) offered) sets
  _ -> S.identityAnswer p

stickersIn :: Zone.Zone -> GameState.GameState -> [(Seq.Seq StickerPlacement.StickerPlacement, Timestamp.Timestamp)]
stickersIn zone gs = [(Object.stickers obj, Object.timestamp obj) | oid <- Game.zoneMembers zone S.alice gs, Just obj <- [Game.lookupObject oid gs]]

-- Answers ChooseCopyTarget with `oid`.
copying :: ObjectId.ObjectId -> Prompt.Prompt r -> r
copying oid p = case p of
  Prompt.ChooseCopyTarget {} -> Just oid
  _ -> S.identityAnswer p

-- What a placement asked: the permanents offered, the stickers offered and the
-- "may"s, each newest first.
data Offers = MkOffers {permanents :: [[ObjectId.ObjectId]], stickers :: [[StickerRef.StickerRef]], mays :: Int}

-- Accepts every "may", names `onto` where a permanent is chosen and it is
-- offered, and answers ChooseSticker with the FIRST offered, pinned by position.
placing :: Maybe ObjectId.ObjectId -> Prompt.Prompt r -> State.State Offers r
placing onto p = case p of
  Prompt.ChooseOptional {} -> do
    State.modify' (\o -> o {mays = mays o + 1})
    pure OptionalDecision.Exercises
  Prompt.ChoosePermanent _ _ _ offered -> do
    State.modify' (\o -> o {permanents = NonEmpty.toList offered : permanents o})
    pure (case onto of Just oid | List.elem oid offered -> oid; _ -> NonEmpty.head offered)
  Prompt.ChooseSticker _ _ _ offered -> do
    State.modify' (\o -> o {stickers = NonEmpty.toList offered : stickers o})
    pure (NonEmpty.head offered)
  _ -> pure (S.identityAnswer p)

-- Settle and resolve until the stack is empty.
drain :: Game.Type.Game ()
drain = do
  Engine.settleForPriority
  stack <- State.gets GameState.stack
  Monad.unless (null stack) (Stack.resolveTop >> drain)

-- A Pyrodancer enters under alice with its enters event, and everything it
-- triggers resolves under `placing onto`.
pyrodancerEnters :: Printing.Printing -> Maybe ObjectId.ObjectId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState, Offers)
pyrodancerEnters pyrodancer onto gs0 =
  let (pyro, entered) = S.entersWithTrigger pyrodancer S.alice gs0
      ((_, after), offers) = State.runState (Engine.runGame (placing onto) entered drain) (MkOffers [] [] 0)
   in (pyro, after, offers)

-- What name placements asked, oldest first: each position prompt's chooser and
-- positions, and each ChooseCardFromAmong's size.
data Asked
  = Positioned PlayerId.PlayerId [Natural]
  | LookedAt Int
  deriving (Eq, Show)

-- How a placement is answered: the permanent, the sticker, the position, and
-- which recipients a target slot takes first.
data Naming = MkNaming
  { namingOnto :: Maybe ObjectId.ObjectId,
    namingSticker :: StickerRef.StickerRef,
    namingPosition :: Natural,
    namingPrefers :: Recipient.Recipient -> Bool
  }

placingAt :: Maybe ObjectId.ObjectId -> StickerRef.StickerRef -> Natural -> Naming
placingAt onto ref k = MkNaming {namingOnto = onto, namingSticker = ref, namingPosition = k, namingPrefers = const False}

naming :: Naming -> Prompt.Prompt r -> State.State [Asked] r
naming how p = case p of
  Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
  Prompt.ChoosePermanent _ _ _ offered ->
    pure
      ( case namingOnto how of
          Just oid | List.elem oid offered -> oid
          _ -> NonEmpty.head offered
      )
  Prompt.ChooseSticker _ _ _ offered -> pure (if List.elem (namingSticker how) offered then namingSticker how else NonEmpty.head offered)
  Prompt.ChooseNamePosition _ chooser _ offered -> do
    State.modify' (Positioned chooser (NonEmpty.toList offered) :)
    pure (namingPosition how)
  Prompt.ChooseCardFromAmong _ _ _ candidates -> do
    State.modify' (LookedAt (length candidates) :)
    pure (NonEmpty.head candidates)
  Prompt.ChooseTargets _ _ _ offers -> pure (S.preferring (namingPrefers how) offers)
  _ -> pure (S.identityAnswer p)

-- `card` enters under alice with its enters event; all it triggers resolves
-- under `how`.
entersNaming :: Printing.Printing -> Naming -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState, [Asked])
entersNaming card how gs0 =
  let (oid, entered) = S.entersWithTrigger card S.alice gs0
      ((_, after), asked) = State.runState (Engine.runGame (naming how) entered drain) []
   in (oid, after, reverse asked)

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "Sticker" $ do
  Spec.it s "CR 107.17 Blorbian Buddy's {G}, {T} gets alice a ticket counter" $ do
    buddy <- S.printingOf s registry "Blorbian Buddy"
    forest <- S.printingOf s registry "Forest"
    let (buddyId, board) = S.addPermanent buddy S.alice (mainPhaseForAlice (S.landsFor forest S.alice 1 (Setup.gameWith GameSettings.plain S.bothPlayers)))
        after = case Activatable.abilitiesFor buddyId board of
          [ability] -> S.runPure S.identityAnswer board (Activate.activateAbility S.alice buddyId ability >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 107.17 alice has one ticket counter" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 1
  Spec.it s "CR 107.17 Ticket Turbotubes' {3}, {T} gets alice a ticket counter" $ do
    tubes <- S.printingOf s registry "Ticket Turbotubes"
    mountain <- S.printingOf s registry "Mountain"
    let (tubesId, board) = S.addPermanent tubes S.alice (mainPhaseForAlice (S.landsFor mountain S.alice 3 (Setup.gameWith GameSettings.plain S.bothPlayers)))
        after = case Activatable.abilitiesFor tubesId board of
          [_, tickets] -> S.runPure S.identityAnswer board (Activate.activateAbility S.alice tubesId tickets >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 107.17 alice has one ticket counter" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 1
  -- Three, so a doubling reads six and a no-op three.
  Spec.it s "CR 701.10e Aetheric Amplifier doubles alice's ticket counters" $ do
    amplifier <- S.printingOf s registry "Aetheric Amplifier"
    mountain <- S.printingOf s registry "Mountain"
    let (ampId, board) = S.addPermanent amplifier S.alice (mainPhaseForAlice (S.addPlayerCounter PlayerCounterKind.Ticket 3 S.alice (S.landsFor mountain S.alice 4 (Setup.gameWith GameSettings.plain S.bothPlayers))))
        after = case Activatable.abilitiesFor ampId board of
          [_, doubling] -> S.runPure secondMode board (Activate.activateAbility S.alice ampId doubling >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 701.10e alice has six ticket counters" (S.playerCounterOf PlayerCounterKind.Ticket S.alice after) 6
  -- CR 123.2: a sheet is three name, three art, two ability and two P/T
  -- stickers, and each structured field says what MTGJSON's text says.
  Spec.it s "CR 123.2 every sticker sheet says what its Oracle text says" $ do
    root <- StickerSheets.defaultRoot
    loaded <- StickerSheets.loadRoot root
    Spec.assertBool s (length loaded >= 4) "at least four sheets are committed"
    let offends (path, result) = case result of
          Left reason -> Just (path <> ": " <> Text.unpack reason)
          Right sheet ->
            let lines_ = foldMap Text.lines (StickerSheet.oracleText sheet)
                abilityLines = fmap ticketLine (take 2 lines_)
                ptLines = fmap ticketLine (drop 2 lines_)
                abilityStickers = Foldable.toList (StickerSheet.abilities sheet)
                ptStickers = Foldable.toList (StickerSheet.powerToughness sheet)
                ptText p = Text.pack (show (PowerToughnessSticker.power p) <> "/" <> show (PowerToughnessSticker.toughness p))
                keywordsAgree a (_, rest) =
                  let printed = [Oracle.printed k | k <- Map.keys (AbilitySticker.keywords a)]
                   in not (null (AbilitySticker.abilities a)) || any Maybe.isNothing printed || List.sort (Oracle.normalise rest) == List.sort (fmap Text.toLower (Maybe.catMaybes printed))
                checks =
                  [ ("file named for the sheet", Slug.unwrap (Slug.fromText (StickerSheet.name sheet)) <> Text.pack ".json" == Text.pack (reverse (takeWhile (/= '/') (reverse path)))),
                    ("names join to the name", Text.unwords (Foldable.toList (StickerSheet.names sheet)) == StickerSheet.name sheet),
                    ("three name, three art, two ability, two P/T stickers", (length (StickerSheet.names sheet), StickerSheet.art sheet, length abilityStickers, length ptStickers) == (3, 3, 2, 2)),
                    ("four Oracle lines", length lines_ == 4),
                    ("ability ticket costs", fmap AbilitySticker.tickets abilityStickers == fmap fst abilityLines),
                    ("P/T ticket costs", fmap PowerToughnessSticker.tickets ptStickers == fmap fst ptLines),
                    ("P/T values", fmap ptText ptStickers == fmap snd ptLines),
                    ("keyword stickers", and (zipWith keywordsAgree abilityStickers abilityLines))
                  ]
             in case [what | (what, False) <- checks] of
                  [] -> Nothing
                  failed -> Just (path <> ": " <> show failed)
    Spec.assertEqWith s "every sheet agrees with its text" (Maybe.mapMaybe offends loaded) []
  Spec.it s "CR 103.2d four sheets: three are drawn at random, one at a time" $ do
    sheets <- committedSheets
    mountain <- S.printingOf s registry "Mountain"
    let plain = Deck.fromCards (Map.singleton mountain 10)
        (gs, asked) = startedWith ((S.alice, withSheetsDeck sheets plain) NonEmpty.:| [(S.bob, plain)])
    Spec.assertEqWith s "the four committed sheets load" (length sheets) 4
    Spec.assertEqWith s "CR 103.2d three of the four sheets are chosen" (chosenOf S.alice gs) (Set.fromList [1, 2, 3])
    Spec.assertEqWith s "each drawn from those not yet drawn" asked [[0, 1, 2, 3], [0, 1, 2], [0, 1]]
    Spec.assertEqWith s "and bob, who brought none, plays without stickers" (chosenOf S.bob gs) Set.empty
  Spec.it s "CR 103.2d/123.2b three sheets are all kept, unasked" $ do
    sheets <- committedSheets
    mountain <- S.printingOf s registry "Mountain"
    let plain = Deck.fromCards (Map.singleton mountain 10)
        (gs, asked) = startedWith ((S.alice, withSheetsDeck (take 3 sheets) plain) NonEmpty.:| [(S.bob, plain)])
    Spec.assertEqWith s "CR 123.2b all three are chosen" (chosenOf S.alice gs) (Set.fromList [0, 1, 2])
    Spec.assertEqWith s "and nothing was drawn at random" asked []
  -- Review Focus 3. One board, two bob spells: a Bolt kills the Piker (public
  -- to public) and an Unsummon bounces it (public to hidden).
  Spec.it s "CR 123.5/613.7k an art sticker stays through a death, restamped, and comes off in a bounce" $ do
    sheets <- committedSheets
    piker <- S.printingOf s registry "Goblin Piker"
    bolt <- S.printingOf s registry "Lightning Bolt"
    unsummon <- S.printingOf s registry "Unsummon"
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    let base = withSheets (take 1 sheets) (S.landsFor island S.bob 1 (S.landsFor mountain S.bob 1 (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (pikerId, g1) = S.addPermanent piker S.alice base
        stickered = stickerOn pikerId g1
        placed = foldMap (Foldable.toList . Object.stickers) (Game.lookupObject pikerId stickered)
        (boltId, g2) = S.addHandCard bolt S.bob stickered
        (unsummonId, g3) = S.addHandCard unsummon S.bob g2
        bobsWindow = g3 {GameState.priority = Just S.bob}
        killed = S.settleSba (S.runPure (namingTarget pikerId) bobsWindow (S.cast S.bob boltId >> Stack.resolveTop))
        bounced = S.runPure (namingTarget pikerId) bobsWindow (S.cast S.bob unsummonId >> Stack.resolveTop)
    case (stickersIn Zone.Graveyard killed, placed) of
      ([(kept, stamp)], [placement]) -> do
        Spec.assertEqWith s "CR 123.5 the same art sticker is on the card in the graveyard" (fmap StickerPlacement.sticker (Foldable.toList kept)) [StickerPlacement.sticker placement]
        Spec.assertBool s (all (\p -> StickerPlacement.timestamp p > stamp) kept) "CR 613.7k restamped right after the card's new timestamp"
      other -> Spec.assertFailure s ("expected one graveyard card and one placement, got " <> show other)
    Spec.assertEqWith s "CR 123.5 the card bounced to hand has none" (fmap fst (stickersIn Zone.Hand bounced)) [Seq.empty]
  -- Review Focus 2. Twenty cards a deck so nobody is decked.
  Spec.it s "CR 727.2/103.2d a restart draws the sheets again and every sticker comes off" $ do
    sheets <- committedSheets
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let plain = Deck.fromCards (Map.singleton mountain 20)
        (started, _) = startedWith ((S.alice, withSheetsDeck sheets plain) NonEmpty.:| [(S.bob, plain)])
        (pikerId, withPiker) = S.addPermanent piker S.alice started
        stickered = stickerOn pikerId withPiker
        ((_, restarted), asked) = State.runState (Engine.runGame sheetDraws stickered (Setup.restartGame S.performer Set.empty S.alice)) []
        stickeredObjects g = Map.keys (Map.filter (not . Seq.null . Object.stickers) (GameState.objects g))
    Spec.assertEqWith s "the Piker was stickered before the restart" (stickeredObjects stickered) [pikerId]
    Spec.assertEqWith s "CR 727.2 no object is stickered after it" (stickeredObjects restarted) []
    Spec.assertEqWith s "CR 103.2d the restart drew three sheets again" (reverse asked) [[0, 1, 2, 3], [0, 1, 2], [0, 1]]
  -- Three readings of one board: no sticker, a sticker, and the stickered
  -- Piker bounced (CR 123.4: not sticky).
  Spec.it s "CR 123.4 Croakid Amphibonaut flies beside a stickered permanent and stops when the sticker leaves" $ do
    sheets <- committedSheets
    croakid <- S.printingOf s registry "Croakid Amphibonaut"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    island <- S.printingOf s registry "Island"
    let base = withSheets (take 1 sheets) (S.landsFor island S.bob 1 (Setup.gameWith GameSettings.plain S.bothPlayers))
        (croakidId, g1) = S.addPermanent croakid S.alice base
        (pikerId, unstickered) = S.addPermanent piker S.alice g1
        stickered = stickerOn pikerId unstickered
        (unsummonId, g2) = S.addHandCard unsummon S.bob stickered
        bounced = S.runPure (namingTarget pikerId) (g2 {GameState.priority = Just S.bob}) (S.cast S.bob unsummonId >> Stack.resolveTop)
        flies = Projection.hasKeyword Keyword.Flying croakidId
    Spec.assertEqWith s "CR 123.4 flying with no sticker, with one, and after the bounce" (flies unstickered, flies stickered, flies bounced) (False, True, False)
  -- Review Focus 5. The Clone copies the stickered Piker; then the Piker
  -- leaves. A Clone that copied the sticker would keep Croakid flying.
  Spec.it s "CR 123.1 a Clone of a stickered permanent is not stickered" $ do
    sheets <- committedSheets
    croakid <- S.printingOf s registry "Croakid Amphibonaut"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    unsummon <- S.printingOf s registry "Unsummon"
    island <- S.printingOf s registry "Island"
    let base = withSheets (take 1 sheets) (S.landsFor island S.bob 1 (Setup.gameWith GameSettings.plain S.bothPlayers))
        (croakidId, g1) = S.addPermanent croakid S.alice base
        (pikerId, g2) = S.addPermanent piker S.alice g1
        (_, staged) = S.spellOnStack clone S.alice (stickerOn pikerId g2)
        cloned = S.settleSba (S.runPure (copying pikerId) staged Stack.resolveTop)
        (unsummonId, g3) = S.addHandCard unsummon S.bob cloned
        bounced = S.runPure (namingTarget pikerId) (g3 {GameState.priority = Just S.bob}) (S.cast S.bob unsummonId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 123.1 with the stickered Piker gone, its Clone does not keep Croakid flying" (Projection.hasKeyword Keyword.Flying croakidId bounced) False
    Spec.assertEqWith s "the Clone is on the battlefield beside Croakid" (length (filter (\oid -> Set.notMember oid (GameState.battlefield base)) (Game.zoneMembers Zone.Battlefield S.alice bounced))) 2
  -- The spec's retention bullet as the whole card: Pyrodancer stickers itself
  -- (the only nonland permanent alice owns) and dies to a Bolt.
  Spec.it s "CR 123.9 whole card: Pyrodancer's art sticker is on it, and stays through its death" $ do
    sheets <- committedSheets
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let base = withSheets (take 1 sheets) (S.landsFor mountain S.bob 1 (Setup.gameWith GameSettings.plain S.bothPlayers))
        (pyro, stickered, _) = pyrodancerEnters pyrodancer Nothing base
        (boltId, g1) = S.addHandCard bolt S.bob stickered
        killed = S.settleSba (S.runPure (namingTarget pyro) (g1 {GameState.priority = Just S.bob}) (S.cast S.bob boltId >> Stack.resolveTop))
    Spec.assertEqWith s "CR 123.9 Pyrodancer's art sticker is on it" (fmap (fmap (StickerRef.kind . StickerPlacement.sticker) . Foldable.toList . Object.stickers) (Game.lookupObject pyro stickered)) (Just [StickerKind.Art])
    Spec.assertEqWith s "CR 123.5 and on the card in the graveyard" (fmap (Seq.length . fst) (stickersIn Zone.Graveyard killed)) [1]
  -- Review Focus 4. Two copies of one sheet: six art stickers, all distinct.
  -- Two placements on the Piker, then a bounce frees both.
  Spec.it s "CR 123.3/123.3a a used sticker is not offered again until its card reaches a hidden zone" $ do
    sheets <- committedSheets
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    piker <- S.printingOf s registry "Goblin Piker"
    unsummon <- S.printingOf s registry "Unsummon"
    island <- S.printingOf s registry "Island"
    let sheet = take 1 sheets
        (pikerId, g1) = S.addPermanent piker S.alice (withSheets (sheet <> sheet) (S.landsFor island S.bob 1 (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (_, g2, first) = pyrodancerEnters pyrodancer (Just pikerId) g1
        (_, g3, second) = pyrodancerEnters pyrodancer (Just pikerId) g2
        (unsummonId, g4) = S.addHandCard unsummon S.bob g3
        bounced = S.runPure (namingTarget pikerId) (g4 {GameState.priority = Just S.bob}) (S.cast S.bob unsummonId >> Stack.resolveTop)
        (_, _, third) = pyrodancerEnters pyrodancer Nothing bounced
        offered o = concat (take 1 (stickers o))
    Spec.assertEqWith s "CR 123.3 the used sticker is not offered again" (offered second) (drop 1 (offered first))
    Spec.assertEqWith s "CR 123.5 the bounce frees both" (offered third) (offered first)
    Spec.assertEqWith s "CR 123.3a two copies of one sheet offer six art stickers" (length (offered first)) 6
  -- CR 123.3b through the card's "you own": alice controls bob's Piker, and
  -- only her own nonland permanents are offered.
  Spec.it s "CR 123.3b Pyrodancer offers only a permanent alice owns" $ do
    sheets <- committedSheets
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    piker <- S.printingOf s registry "Goblin Piker"
    bears <- S.printingOf s registry "Grizzly Bears"
    let (bobsPiker, g1) = S.addPermanent piker S.bob (withSheets (take 1 sheets) (Setup.gameWith GameSettings.plain S.bothPlayers))
        (bearsId, g2) = S.addPermanent bears S.alice (S.giveControl bobsPiker S.alice g1)
        (pyro, after, offers) = pyrodancerEnters pyrodancer (Just bearsId) g2
    Spec.assertEqWith s "CR 123.3b bob's Piker is not offered, alice's two permanents are" (fmap List.sort (permanents offers)) [List.sort [pyro, bearsId]]
    Spec.assertEqWith s "and it took no sticker" (fmap (Seq.length . Object.stickers) (Game.lookupObject bobsPiker after)) (Just 0)
  -- Review Focus 1's gameplay half: no sheets, nothing to place, no "may".
  Spec.it s "CR 608.2d with no sheets Pyrodancer's may is not offered" $ do
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    let (pyro, after, offers) = pyrodancerEnters pyrodancer Nothing (Setup.gameWith GameSettings.plain S.bothPlayers)
    Spec.assertEqWith s "CR 608.2d alice was not asked" (mays offers) 0
    Spec.assertEqWith s "and nothing is stickered" (fmap (Seq.length . Object.stickers) (Game.lookupObject pyro after)) (Just 0)
  -- Two Pikers, one stickered: only it is a legal target, and it gets +2/+0
  -- and menace.
  Spec.it s "CR 123.9 Pyrodancer's ability targets only a creature with an art sticker" $ do
    sheets <- committedSheets
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (marked, g1) = S.addPermanent piker S.alice (mainPhaseForAlice (S.landsFor mountain S.alice 3 (withSheets (take 1 sheets) (Setup.gameWith GameSettings.plain S.bothPlayers))))
        (plainPiker, g2) = S.addPermanent piker S.alice g1
        (pyro, g3, _) = pyrodancerEnters pyrodancer (Just marked) g2
        board = mainPhaseForAlice g3
        recording :: Prompt.Prompt r -> State.State [ObjectId.ObjectId] r
        recording p = case p of
          Prompt.ChooseTargets _ _ _ sets -> do
            State.put (concatMap (Maybe.mapMaybe Recipient.objectOf . Set.toList . snd) (Map.elems sets))
            pure (namingTarget marked p)
          _ -> pure (S.identityAnswer p)
        ((_, after), offered) = case Activatable.abilitiesFor pyro board of
          [ability] -> State.runState (Engine.runGame recording board (Activate.activateAbility S.alice pyro ability >> Stack.resolveTop)) []
          _ -> (((), board), [])
    Spec.assertEqWith s "CR 115.1 only the Piker with an art sticker is offered" offered [marked]
    Spec.assertEqWith s "it gets +2/+0 and menace" (Projection.powerOf marked after, Projection.hasKeyword Keyword.Menace marked after) (Just 4, True)
    Spec.assertEqWith s "the other Piker is untouched" (Projection.powerOf plainPiker after) (Just 2)
  -- Divergence 6: an art placement triggers the art ability alone, so one
  -- counter and no pump; both firing would read power 2.
  Spec.it s "CR 123.9 an art sticker puts one +1/+1 counter on Wee Champion and no pump" $ do
    sheets <- committedSheets
    pyrodancer <- S.printingOf s registry "Proficient Pyrodancer"
    wee <- S.printingOf s registry "Wee Champion"
    let (weeId, g1) = S.addPermanent wee S.alice (withSheets (take 1 sheets) (Setup.gameWith GameSettings.plain S.bothPlayers))
        (pyro, after, _) = pyrodancerEnters pyrodancer Nothing g1
    Spec.assertEqWith s "CR 123.9 Wee Champion is a 1/2 with one +1/+1 counter" (Projection.powerOf weeId after, S.counterOf CounterKind.PlusOnePlusOne weeId after) (Just 1, 1)
    Spec.assertEqWith s "one art sticker went on alice's two permanents" (sum (fmap (\oid -> maybe 0 (Seq.length . Object.stickers) (Game.lookupObject oid after)) [weeId, pyro])) 1
  -- CR 123.6a over the three card names this unit adds that hold a blank.
  Spec.it s "CR 123.6a a blank is not a word, and _____-o-saurus is one" $ do
    let named = CardName.MkCardName . Text.pack
    Spec.assertEqWith s "CR 123.6a Wolf in _____ Clothing has three words" (NameWords.wordCount (named "Wolf in _____ Clothing")) 3
    Spec.assertEqWith s "CR 123.6a _____-o-saurus is one hyphenated word" (NameWords.wordCount (named "_____-o-saurus")) 1
    Spec.assertEqWith s "CR 123.6a three blanks and Trespasser are one word" (NameWords.wordCount (named "_____ _____ _____ Trespasser")) 1
  -- CR 123.6b's own example, then CR 123.6c's "fewer words" and a blank.
  Spec.it s "CR 123.6b-c a word goes after k words, before a blank that follows, or at the end" $ do
    let named = CardName.MkCardName . Text.pack
        dark k = CardName.unwrap (NameWords.insertAfter k (Text.pack "Dark") (named "Bear Cub"))
    Spec.assertEqWith s "CR 123.6b Dark Bear Cub, Bear Dark Cub, Bear Cub Dark" (fmap dark [0, 1, 2]) (fmap Text.pack ["Dark Bear Cub", "Bear Dark Cub", "Bear Cub Dark"])
    Spec.assertEqWith s "CR 123.6c after five words of two: at the end" (dark 5) (Text.pack "Bear Cub Dark")
    Spec.assertEqWith s "CR 123.6a after two words, before the blank" (CardName.unwrap (NameWords.insertAfter 2 (Text.pack "Otter") (named "Wolf in _____ Clothing"))) (Text.pack "Wolf in Otter _____ Clothing")
  Spec.it s "CR 123.6d-e letters and unique vowels ignore case, and Y is a vowel" $ do
    Spec.assertEqWith s "CR 123.6e Slimy, Otter, Ringmaster" (fmap (NameWords.uniqueVowels . Text.pack) ["Slimy", "Otter", "Ringmaster"]) [2, 2, 3]
    Spec.assertEqWith s "CR 123.6d the o's in Otter Storm" (NameWords.letterCount (Text.pack "o") (Text.pack "Otter Storm")) 2
  Spec.it s "CR 123.6 a name sticker's words come off its owner's sheet" $ do
    sheets <- committedSheets
    let gs = withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)
        art = StickerRef.MkStickerRef S.alice 2 StickerKind.Art 0
    Spec.assertEqWith s "Night, Slimy, Otter" (fmap (\ref -> Game.stickerWords ref gs) [night, slimy, otter]) (fmap (Just . Text.pack) ["Night", "Slimy", "Otter"])
    Spec.assertEqWith s "an art sticker has none" (Game.stickerWords art gs) Nothing
  -- CR 123.6c's first example, with a committed word: Otter after Fae of
  -- Wishes' second word. Exile to stack to exile is public to public (CR 123.5).
  Spec.it s "CR 123.6c Fae of Otter Wishes is cast as Granted Otter and exiled as Fae of Otter Wishes again" $ do
    sheets <- committedSheets
    fae <- S.printingOf s registry "Fae of Wishes"
    island <- S.printingOf s registry "Island"
    let base = mainPhaseForAlice (S.landsFor island S.alice 4 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (faeId, exiled) = S.addExiledCard fae S.alice base
        stickered = Sticker.put S.alice faeId otter (Just 2) exiled
        granted = CardName.MkCardName (Text.pack "Granted")
        casting = S.runPure S.identityAnswer stickered (Cast.castSpell S.manaPerformer S.alice faeId granted Facing.FaceUp)
        resolved = S.runPure S.identityAnswer casting Stack.resolveTop
        shown g = concatMap (\oid -> nameTexts oid g)
    Spec.assertEqWith s "CR 123.6c in exile it is Fae of Otter Wishes" (nameTexts faeId stickered) [Text.pack "Fae of Otter Wishes"]
    Spec.assertEqWith s "CR 123.6c on the stack it is Granted Otter" (shown casting (GameState.stack casting)) [Text.pack "Granted Otter"]
    Spec.assertEqWith s "CR 123.6c/715.3d exiled again it is Fae of Otter Wishes" (shown resolved (Game.zoneMembers Zone.Exile S.alice resolved)) [Text.pack "Fae of Otter Wishes"]
  -- Review Focus 2.
  Spec.it s "CR 123.1/707.2 a Clone of Grizzly Otter Bears is named Grizzly Bears" $ do
    sheets <- committedSheets
    bears <- S.printingOf s registry "Grizzly Bears"
    clone <- S.printingOf s registry "Clone"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        stickered = Sticker.put S.alice bearsId otter (Just 1) g1
        (_, staged) = S.spellOnStack clone S.alice stickered
        cloned = S.settleSba (S.runPure (copying bearsId) staged Stack.resolveTop)
        clones = [oid | oid <- Set.toList (GameState.battlefield cloned), Set.notMember oid (GameState.battlefield stickered)]
    Spec.assertEqWith s "CR 707.2 the Clone is named Grizzly Bears" (fmap (\oid -> nameTexts oid cloned) clones) [[Text.pack "Grizzly Bears"]]
    Spec.assertEqWith s "while the original is Grizzly Otter Bears" (nameTexts bearsId cloned) [Text.pack "Grizzly Otter Bears"]
  -- Divergence 5 (#4901): the sticker is later than Spy Kit, so it reaches every
  -- name Spy Kit gave. Goblin Piker is in the game, so its name is in the reference.
  Spec.it s "CR 123.6c/612.7 a sticker placed after Spy Kit goes into every name its host has" $ do
    sheets <- committedSheets
    bears <- S.printingOf s registry "Grizzly Bears"
    piker <- S.printingOf s registry "Goblin Piker"
    kit <- S.printingOf s registry "Spy Kit"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (_, g2) = S.addPermanent piker S.bob g1
        (kitId, g3) = S.addPermanent kit S.alice g2
        stickered = Sticker.put S.alice bearsId otter (Just 1) (S.attach kitId bearsId g3)
        shown = Set.fromList (nameTexts bearsId stickered)
    Spec.assertBool s (Set.member (Text.pack "Goblin Otter Piker") shown) "CR 612.7 the Piker's name, with the word after its first"
    Spec.assertBool s (Set.member (Text.pack "Grizzly Otter Bears") shown) "and its own, Grizzly Otter Bears"
    Spec.assertBool s (Set.notMember (Text.pack "Goblin Piker") shown) "and no name without the word"
  -- Review Focus 1, two boards differing in the order of one Aura and one
  -- sticker: CR 613.7 orders the two layer-3 effects by timestamp.
  Spec.it s "CR 123.6c/613.7 a later Witness Protection hides the word, and a sticker placed after it shows" $ do
    sheets <- committedSheets
    bears <- S.printingOf s registry "Grizzly Bears"
    protection <- S.printingOf s registry "Witness Protection"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (laterAura, g2) = S.addPermanent protection S.alice (Sticker.put S.alice bearsId otter (Just 1) g1)
        hidden = S.attach laterAura bearsId g2
        (earlierAura, g3) = S.addPermanent protection S.alice g1
        shown = Sticker.put S.alice bearsId otter (Just 1) (S.attach earlierAura bearsId g3)
    Spec.assertEqWith s "CR 612.8/123.6c Witness Protection, later, leaves only Legitimate Businessperson" (nameTexts bearsId hidden) [Text.pack "Legitimate Businessperson"]
    Spec.assertEqWith s "CR 613.7 a sticker placed after it reads Legitimate Otter Businessperson" (nameTexts bearsId shown) [Text.pack "Legitimate Otter Businessperson"]
  -- Review Focus 1's copy half, CR 123.6c's second example: Mirrorweave makes
  -- every other creature a copy of bob's Seeker.
  Spec.it s "CR 123.6c It That Betrays Otter, as a copy of Seeker of the Way, is Seeker of the Otter Way" $ do
    sheets <- committedSheets
    betrays <- S.printingOf s registry "It That Betrays"
    seeker <- S.printingOf s registry "Seeker of the Way"
    mirrorweave <- S.printingOf s registry "Mirrorweave"
    plains <- S.printingOf s registry "Plains"
    let (betraysId, g1) = S.addPermanent betrays S.alice (mainPhaseForAlice (S.landsFor plains S.alice 4 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))))
        (seekerId, g2) = S.addPermanent seeker S.bob g1
        stickered = Sticker.put S.alice betraysId otter (Just 3) g2
        (weaveId, g3) = S.addHandCard mirrorweave S.alice stickered
        copied = S.runPure (namingTarget seekerId) g3 (S.cast S.alice weaveId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 123.6c as a copy of Seeker of the Way it is Seeker of the Otter Way" (nameTexts betraysId copied) [Text.pack "Seeker of the Otter Way"]
    Spec.assertEqWith s "before, It That Betrays Otter" (nameTexts betraysId stickered) [Text.pack "It That Betrays Otter"]
  -- Review Focus 3. alice owns the Bears and bob controls them.
  Spec.it s "CR 123.6b the controller, not the placer, chooses where the word goes" $ do
    sheets <- committedSheets
    baaallerina <- S.printingOf s registry "Baaallerina"
    bears <- S.printingOf s registry "Grizzly Bears"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (_, after, asked) = entersNaming baaallerina (placingAt (Just bearsId) otter 1) (S.giveControl bearsId S.bob g1)
    Spec.assertEqWith s "CR 123.6b bob, who controls the Bears, chooses among three positions" asked [Positioned S.bob [0, 1, 2]]
    Spec.assertEqWith s "and they are Grizzly Otter Bears" (nameTexts bearsId after) [Text.pack "Grizzly Otter Bears"]
  -- Review Focus 3's other half. CR 708.2's face-down state written straight
  -- on, FaceDownSpec's posture; bob turns it up with Break Open.
  Spec.it s "CR 708.2/123.6b a face-down permanent is named Night, and face up Night Ainok Tracker" $ do
    sheets <- committedSheets
    baaallerina <- S.printingOf s registry "Baaallerina"
    tracker <- S.printingOf s registry "Ainok Tracker"
    breakOpen <- S.printingOf s registry "Break Open"
    mountain <- S.printingOf s registry "Mountain"
    let (trackerId, g1) = S.addPermanent tracker S.alice (S.landsFor mountain S.bob 2 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        hidden = g1 {GameState.objects = Map.adjust (\o -> o {Object.facing = Facing.faceDown FaceDownReason.TurnedFaceDown}) trackerId (GameState.objects g1)}
        (_, named, asked) = entersNaming baaallerina (placingAt (Just trackerId) night 0) hidden
        (breakId, g2) = S.addHandCard breakOpen S.bob named
        revealed = S.runPure (namingTarget trackerId) (g2 {GameState.priority = Just S.bob}) (S.cast S.bob breakId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 123.6b the face-down Tracker is named Night" (nameTexts trackerId named) [Text.pack "Night"]
    Spec.assertEqWith s "CR 123.6c face up it is Night Ainok Tracker" (nameTexts trackerId revealed) [Text.pack "Night Ainok Tracker"]
    Spec.assertEqWith s "CR 123.6b a nameless object has one position, so nobody was asked" asked []
  Spec.it s "CR 123.6 Baaallerina's ability targets only a creature with a name sticker" $ do
    sheets <- committedSheets
    baaallerina <- S.printingOf s registry "Baaallerina"
    piker <- S.printingOf s registry "Goblin Piker"
    island <- S.printingOf s registry "Island"
    let (marked, g1) = S.addPermanent piker S.alice (S.landsFor island S.alice 3 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (plainPiker, g2) = S.addPermanent piker S.alice g1
        (baaId, g3, _) = entersNaming baaallerina (placingAt (Just marked) night 0) g2
        board = mainPhaseForAlice g3
        recording :: Prompt.Prompt r -> State.State [ObjectId.ObjectId] r
        recording p = case p of
          Prompt.ChooseTargets _ _ _ sets -> do
            State.put (concatMap (Maybe.mapMaybe Recipient.objectOf . Set.toList . snd) (Map.elems sets))
            pure (namingTarget marked p)
          _ -> pure (S.identityAnswer p)
        ((_, after), offered) = case Activatable.abilitiesFor baaId board of
          [ability] -> State.runState (Engine.runGame recording board (Activate.activateAbility S.alice baaId ability >> Stack.resolveTop)) []
          _ -> (((), board), [])
    Spec.assertEqWith s "CR 115.1 only the Piker with a name sticker is offered" offered [marked]
    Spec.assertEqWith s "it gains flying, the other Piker does not" (Projection.hasKeyword Keyword.Flying marked after, Projection.hasKeyword Keyword.Flying plainPiker after) (True, False)
  Spec.it s "CR 123.6 whole card: Sword-Swallowing Seraph names a Piker and puts a +1/+1 counter on it" $ do
    sheets <- committedSheets
    seraph <- S.printingOf s registry "Sword-Swallowing Seraph"
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    let (pikerId, g1) = S.addPermanent piker S.alice (S.landsFor plains S.alice 2 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        (seraphId, g2, _) = entersNaming seraph (placingAt (Just pikerId) night 0) g1
        board = mainPhaseForAlice g2
        after = case Activatable.abilitiesFor seraphId board of
          [ability] -> S.runPure (namingTarget pikerId) board (Activate.activateAbility S.alice seraphId ability >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 123.6 the Piker with a name sticker has a +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 1
    Spec.assertEqWith s "it is Night Goblin Piker" (nameTexts pikerId g2) [Text.pack "Night Goblin Piker"]
  -- Review Focus 5: case. Three Islands to look at.
  Spec.it s "CR 123.6e Otter has two unique vowels, its O capital: Wizards of the _____ looks at two cards" $ do
    sheets <- committedSheets
    wizards <- S.printingOf s registry "Wizards of the _____"
    island <- S.printingOf s registry "Island"
    let (_, l1) = S.addLibraryCard island S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (_, l2) = S.addLibraryCard island S.alice l1
        (_, l3) = S.addLibraryCard island S.alice l2
        (wizardsId, after, asked) = entersNaming wizards (placingAt Nothing otter 3) l3
    Spec.assertEqWith s "CR 123.6e it looked at two cards, after three positions past three words" asked [Positioned S.alice [0, 1, 2, 3], LookedAt 2]
    Spec.assertEqWith s "CR 123.6a it is Wizards of the Otter _____" (nameTexts wizardsId after) [Text.pack "Wizards of the Otter _____"]
  -- Review Focus 5: Y, and the binding read by a "when you do" ability's
  -- target count (CR 603.12). Pikers are 2/1, so -1/-1 kills.
  Spec.it s "CR 123.6e Slimy has two unique vowels, Y one of them: Wolf in _____ Clothing kills two of bob's Pikers" $ do
    sheets <- committedSheets
    wolf <- S.printingOf s registry "Wolf in _____ Clothing"
    piker <- S.printingOf s registry "Goblin Piker"
    let (p1, g1) = S.addPermanent piker S.bob (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (p2, g2) = S.addPermanent piker S.bob g1
        (p3, g3) = S.addPermanent piker S.bob g2
        bobs r = case Recipient.objectOf r of
          Just oid -> List.elem oid [p1, p2, p3]
          Nothing -> False
        (_, after, _) = entersNaming wolf ((placingAt Nothing slimy 0) {namingPrefers = bobs}) g3
    Spec.assertEqWith s "CR 123.6e two of bob's Pikers died" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
  -- Review Focus 4. Two boards differing in the position alone.
  Spec.it s "CR 123.6a Wolf in _____ Clothing offers four positions, and the word goes before a following blank" $ do
    sheets <- committedSheets
    wolf <- S.printingOf s registry "Wolf in _____ Clothing"
    let base = withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)
        (twoId, afterTwo, asked) = entersNaming wolf (placingAt Nothing otter 2) base
        (threeId, afterThree, _) = entersNaming wolf (placingAt Nothing otter 3) base
    Spec.assertEqWith s "CR 123.6a after two words it is Wolf in Otter _____ Clothing; after three, Wolf in _____ Clothing Otter" (nameTexts twoId afterTwo, nameTexts threeId afterThree) ([Text.pack "Wolf in Otter _____ Clothing"], [Text.pack "Wolf in _____ Clothing Otter"])
    Spec.assertEqWith s "CR 123.6a the blank is not a word: four positions" asked [Positioned S.alice [0, 1, 2, 3]]
  Spec.it s "CR 123.6a _____-o-saurus is one word: two positions, and Otter puts two counters on it" $ do
    sheets <- committedSheets
    saurus <- S.printingOf s registry "_____-o-saurus"
    let (saurusId, after, asked) = entersNaming saurus (placingAt Nothing otter 1) (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
    Spec.assertEqWith s "CR 123.6e two +1/+1 counters for Otter" (S.counterOf CounterKind.PlusOnePlusOne saurusId after) 2
    Spec.assertEqWith s "CR 123.6a it is _____-o-saurus Otter" (nameTexts saurusId after) [Text.pack "_____-o-saurus Otter"]
    Spec.assertEqWith s "CR 123.6a one word, two positions" asked [Positioned S.alice [0, 1]]
  Spec.it s "CR 123.6 Trespasser gets +1/+0 for its one name sticker" $ do
    sheets <- committedSheets
    trespasser <- S.printingOf s registry "_____ _____ _____ Trespasser"
    island <- S.printingOf s registry "Island"
    let (tId, g1, asked) = entersNaming trespasser (placingAt Nothing otter 1) (S.landsFor island S.alice 4 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        board = mainPhaseForAlice g1
        after = case Activatable.abilitiesFor tId board of
          [ability] -> S.runPure S.identityAnswer board (Activate.activateAbility S.alice tId ability >> Stack.resolveTop)
          _ -> board
    Spec.assertEqWith s "CR 123.6 one name sticker: power 3" (Projection.powerOf tId after) (Just 3)
    Spec.assertEqWith s "CR 123.6a three blanks and one word: two positions" asked [Positioned S.alice [0, 1]]
  -- Two boards: Balls of Fire's own sticker triggers it; a sticker on the
  -- Bears does not, though Balls of Fire carries an Otter of its own.
  Spec.it s "CR 123.6d Otter's capital O counts: _____ Balls of Fire deals bob 1, and only for its own sticker" $ do
    sheets <- committedSheets
    balls <- S.printingOf s registry "_____ Balls of Fire"
    baaallerina <- S.printingOf s registry "Baaallerina"
    bears <- S.printingOf s registry "Grizzly Bears"
    let base = withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)
        atBob how = how {namingPrefers = (== Recipient.ToPlayer S.bob)}
        (_, own, _) = entersNaming balls (atBob (placingAt Nothing otter 0)) base
        (ballsId, g1) = S.addPermanent balls S.alice base
        (bearsId, g2) = S.addPermanent bears S.alice (Sticker.put S.alice ballsId otter (Just 0) g1)
        (_, other, _) = entersNaming baaallerina (atBob (placingAt (Just bearsId) night 0)) g2
    Spec.assertEqWith s "CR 123.6d bob takes 1 for Otter's O" (S.lifeOf S.bob own) (Just 19)
    Spec.assertEqWith s "CR 123.3 a sticker on another permanent does not trigger it" (S.lifeOf S.bob other) (Just 20)
  -- Trespasser, four tokens and one word, is the blank's negative.
  Spec.it s "CR 123.6a Angelic Harold pumps Grizzly Otter Bears, three words, and not Trespasser, whose blanks are not words" $ do
    sheets <- committedSheets
    harold <- S.printingOf s registry "Angelic Harold"
    bears <- S.printingOf s registry "Grizzly Bears"
    trespasser <- S.printingOf s registry "_____ _____ _____ Trespasser"
    let (bearsId, g1) = S.addPermanent bears S.alice (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers))
        (tId, g2) = S.addPermanent trespasser S.alice g1
        (_, after, _) = entersNaming harold (placingAt (Just bearsId) otter 1) g2
    Spec.assertEqWith s "CR 123.6a Grizzly Otter Bears is a 3/3" (Projection.powerOf bearsId after) (Just 3)
    Spec.assertEqWith s "CR 123.6a Trespasser, one word, is still a 2/1" (Projection.powerOf tId after) (Just 2)
  -- CR 613.7f gives the turned-up Tracker a new timestamp and CR 613.7k gives
  -- its sticker one right after it, now later than the Aura: the word shows.
  Spec.it s "CR 613.7k a turned-up permanent's name sticker restamps after an earlier Witness Protection" $ do
    sheets <- committedSheets
    baaallerina <- S.printingOf s registry "Baaallerina"
    tracker <- S.printingOf s registry "Ainok Tracker"
    protection <- S.printingOf s registry "Witness Protection"
    breakOpen <- S.printingOf s registry "Break Open"
    mountain <- S.printingOf s registry "Mountain"
    let (trackerId, g1) = S.addPermanent tracker S.alice (S.landsFor mountain S.bob 2 (withSheets sheets (Setup.gameWith GameSettings.plain S.bothPlayers)))
        hidden = g1 {GameState.objects = Map.adjust (\o -> o {Object.facing = Facing.faceDown FaceDownReason.TurnedFaceDown}) trackerId (GameState.objects g1)}
        (_, named, _) = entersNaming baaallerina (placingAt (Just trackerId) night 0) hidden
        (auraId, g2) = S.addPermanent protection S.alice named
        enchanted = S.attach auraId trackerId g2
        (breakId, g3) = S.addHandCard breakOpen S.bob enchanted
        revealed = S.runPure (namingTarget trackerId) (g3 {GameState.priority = Just S.bob}) (S.cast S.bob breakId >> Stack.resolveTop)
    Spec.assertEqWith s "CR 613.7k face up, the restamped sticker follows the Aura: Night Legitimate Businessperson" (nameTexts trackerId revealed) [Text.pack "Night Legitimate Businessperson"]
    Spec.assertEqWith s "CR 613.7 face down, the later Aura hides the word" (nameTexts trackerId enchanted) [Text.pack "Legitimate Businessperson"]
