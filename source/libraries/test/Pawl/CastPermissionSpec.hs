{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.PlayerEffect over effects that permit playing (CR 305.2, CR
-- 601.3): extra land drops, flash and graveyard casting from Vedalken Orrery to
-- Scout's Warning, and Void Winnower's and Oppressive Rays' restrictions. Split
-- out of Pawl.PlayerEffectSpec, which keeps the machinery.
module Pawl.CastPermissionSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.CastProhibitionSpec (equipBoard, flashBoard, flashOnOwnTurn, isActivateOf, isPlay, landDropBoard, orreryScopeBoard, playEveryLand)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as View
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.ExileLink as ExileLink
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerEffect as PlayerEffect.Type
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.VariableChoice as VariableChoice
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Exploration {G} Enchantment: "You may play an additional land on each of your
-- turns." Azusa, Lost but Seeking {2}{G} Legendary Creature -- Human Monk: "You
-- may play two additional lands on each of your turns."
--
-- TWO producers with DIFFERENT numbers, and that is the point of the group
-- rather than redundancy: one card cannot tell a real count from a
-- boolean-plus-one, because both readings answer "two". Azusa's three is what
-- separates them, and the two of them together answer four, which separates a
-- SUM from a maximum.
extraLandDropsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
extraLandDropsSpec s registry =
  Spec.describe s "ExtraLandDrops" $ do
    -- CR 109.5: the You scope. alice's Exploration is alice's, and bob playing
    -- lands on his own turn is still held to CR 305.2's one.
    Spec.it s "CR 109.5 one player's Exploration does not raise another's allowance" $ do
      mountain <- S.printingOf s registry "Mountain"
      exploration <- S.printingOf s registry "Exploration"
      let board = landDropBoard mountain [exploration] S.bob
          after = playEveryLand board
      Spec.assertEqWith s "alice's allowance is two" (PlayerEffect.landPlaysAllowed S.alice board) 2
      Spec.assertEqWith s "bob's is still one" (PlayerEffect.landPlaysAllowed S.bob board) 1
      Spec.assertEqWith s "one Mountain landed for bob" (S.countOnBattlefieldByName (S.printingName mountain) S.bob after) 1
      Spec.assertEqWith s "four are still in his hand" (S.handSize S.bob after) 4
      Spec.assertEqWith s "and his second is refused" (filter isPlay (Action.legalActions S.bob after)) []

    -- CR 604.2: the grant is re-read from the battlefield on every look, so
    -- destroying Exploration between the second land and the third takes the
    -- extra play back. The already-played tally is untouched by that, which is
    -- CR 305.2b's comparison landing on "equal" and refusing.
    Spec.it s "CR 604.2 destroying Exploration mid-turn drops the allowance back to one" $ do
      mountain <- S.printingOf s registry "Mountain"
      exploration <- S.printingOf s registry "Exploration"
      let (explorationId, board) = S.addPermanent exploration S.alice (Setup.emptyGame S.bothPlayers)
          add g _ = snd (S.addHandCard mountain S.alice g)
          withHand = List.foldl' add board [1 .. 5 :: Int]
          ready = withHand {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
          gone = S.runPure S.identityAnswer (playEveryLand ready) (Event.destroy Regenerability.Regenerable [explorationId])
      Spec.assertEqWith s "two lands were played while it stood" (GameState.landsPlayed gone) (Map.singleton S.alice 2)
      Spec.assertEqWith s "the allowance is back to one" (PlayerEffect.landPlaysAllowed S.alice gone) 1
      Spec.assertEqWith s "and CR 305.2b refuses a third" (filter isPlay (Action.legalActions S.alice gone)) []

vedalkenOrrerySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
vedalkenOrrerySpec s registry =
  Spec.describe s "VedalkenOrrery" $ do
    -- The control. Without it, every refusal below would also be true of a board
    -- where the Piker was simply unaffordable or unoffered.
    Spec.it s "CR 117.1a on alice's own turn the creature spell is castable already" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, board) = flashOnOwnTurn mountain piker []
      Spec.assertBool s (S.castable S.alice oid board) "castable"
      Spec.assertBool s (any (S.isCastOf oid) (Action.legalActions S.alice board)) "offered"

    Spec.it s "CR 117.1a on the opponent's turn it is not" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, _, board) = flashBoard mountain piker []
      Spec.assertBool s (not (S.castable S.alice oid board)) "not castable"
      Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice board))) "not offered"

    -- The whole card, on the board that just refused: CR 601.3b's permission is
    -- read beside Cast.instantSpeed, so the sorcery-speed window opens for a card
    -- that has no flash of its own.
    Spec.it s "CR 601.3b with Vedalken Orrery it is castable on the opponent's turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (oid, _, board) = flashBoard mountain piker [orrery]
      Spec.assertBool s (S.castable S.alice oid board) "castable"
      Spec.assertBool s (any (S.isCastOf oid) (Action.legalActions S.alice board)) "offered"

    -- CR 702.8a's keyword is untouched: the card the Orrery let through never
    -- gained flash, and nothing was written onto it.
    Spec.it s "CR 702.8a the Piker still has no flash of its own" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (oid, _, board) = flashBoard mountain piker [orrery]
      Spec.assertBool s (not (Cast.instantSpeed oid (S.combinedFace piker) board)) "no flash on the card"
      Spec.assertBool s (PlayerEffect.mayCastAsThoughItHadFlash S.alice oid board) "the permission is the player's"

    -- CR 604.2: the permission is gathered live off the battlefield, so removing
    -- the Orrery shuts the window again with nothing to unwind.
    Spec.it s "CR 604.2 with the Orrery gone the window shuts again" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (oid, extras, board) = flashBoard mountain piker [orrery]
          without = board {GameState.battlefield = foldr Set.delete (GameState.battlefield board) extras}
      Spec.assertBool s (S.castable S.alice oid board) "castable with it"
      Spec.assertBool s (not (S.castable S.alice oid without)) "not castable without it"

    -- CR 305.1: playing a land is a special action and is never a cast, so a
    -- permission about the timing of a CAST does not reach the Mountain in
    -- alice's hand. Action.legalActions gates a land play on being the active
    -- player, and the Orrery leaves that alone.
    Spec.it s "CR 305.1 the land in hand is still unplayable on the opponent's turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (_, _, board) = flashBoard mountain piker [orrery]
          (_, ownTurn) = flashOnOwnTurn mountain piker [orrery]
      Spec.assertBool s (any isPlay (Action.legalActions S.alice ownTurn)) "playable on her own turn"
      Spec.assertBool s (not (any isPlay (Action.legalActions S.alice board))) "not on bob's"

    -- CR 109.5 / PlayerScope.You: the Orrery says "you", so alice's does nothing
    -- for bob. The pair differs only in who controls it.
    Spec.it s "CR 109.5 alice's Orrery does not widen bob's window" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (bobsPiker, board) = orreryScopeBoard mountain piker orrery S.alice
      Spec.assertBool s (not (S.castable S.bob bobsPiker board)) "not castable"

    Spec.it s "CR 109.5 bob's own Orrery does" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (bobsPiker, board) = orreryScopeBoard mountain piker orrery S.bob
      Spec.assertBool s (S.castable S.bob bobsPiker board) "castable"

    -- CR 307.5: the reason the permission is read BESIDE Cast.instantSpeed and
    -- not inside it, nor inside Turn.sorcerySpeedWindow under it. Bonesplitter's
    -- equip ability is sorcery-speed, and the Orrery is not about abilities at
    -- all. Three boards triangulate it: the ability is genuinely offered, the
    -- opponent's turn genuinely takes it away, and the Orrery does not give it
    -- back.
    Spec.it s "CR 307.5 the equip ability is offered on alice's own turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bonesplitter <- S.printingOf s registry "Bonesplitter"
      let (equipment, board) = equipBoard mountain piker bonesplitter [] S.alice
      Spec.assertBool s (any (isActivateOf equipment) (Action.legalActions S.alice board)) "offered"

    Spec.it s "CR 307.5 and not on bob's" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bonesplitter <- S.printingOf s registry "Bonesplitter"
      let (equipment, board) = equipBoard mountain piker bonesplitter [] S.bob
      Spec.assertBool s (not (any (isActivateOf equipment) (Action.legalActions S.alice board))) "not offered"

    Spec.it s "CR 307.5 Vedalken Orrery does not give it back" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bonesplitter <- S.printingOf s registry "Bonesplitter"
      orrery <- S.printingOf s registry "Vedalken Orrery"
      let (equipment, board) = equipBoard mountain piker bonesplitter [orrery] S.bob
          (onOwnTurn, ownBoard) = equipBoard mountain piker bonesplitter [orrery] S.alice
      Spec.assertBool s (not (any (isActivateOf equipment) (Action.legalActions S.alice board))) "still not offered"
      Spec.assertBool s (any (isActivateOf onOwnTurn) (Action.legalActions S.alice ownBoard)) "and still offered on her own turn"

sigardasAidSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sigardasAidSpec s registry =
  Spec.describe s "SigardasAid" $ do
    -- The control, flashBoard's: without it every refusal below would also be
    -- true of a board where the Rollicker was unaffordable or unoffered. The
    -- Piker on the battlefield is what bestow would enchant, and it is on every
    -- board here, so the Aid stays the only difference.
    Spec.it s "CR 117.1a on alice's own turn the bestow creature spell is castable already" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      rollicker <- S.printingOf s registry "Nyxborn Rollicker"
      let (oid, board) = flashOnOwnTurn mountain rollicker [piker]
      Spec.assertBool s (S.castable S.alice oid board) "castable"
      Spec.assertBool s (any (S.isCastOf oid) (Action.legalActions S.alice board)) "offered"

    Spec.it s "CR 117.1a on the opponent's turn it is not" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      rollicker <- S.printingOf s registry "Nyxborn Rollicker"
      let (oid, _, board) = flashBoard mountain rollicker [piker]
      Spec.assertBool s (not (S.castable S.alice oid board)) "not castable"
      Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice board))) "not offered"

    -- CR 601.3b's SECOND SENTENCE, and rule 601.3b's own example: the Aid names
    -- Aura spells, the card in hand is a Satyr creature card and no Aura at all,
    -- and what carries it is the bestow choice CR 601.2b has yet to be made.
    -- Nothing on the board says Aura until that choice is considered.
    Spec.it s "CR 601.3b with Sigarda's Aid the bestow creature spell is castable on the opponent's turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      rollicker <- S.printingOf s registry "Nyxborn Rollicker"
      aid <- S.printingOf s registry "Sigarda's Aid"
      let (oid, _, board) = flashBoard mountain rollicker [piker, aid]
      Spec.assertBool s (any (S.isCastOf oid) (Action.legalActions S.alice board)) "offered"
      Spec.assertBool s (S.castable S.alice oid board) "castable"
      Spec.assertBool s (PlayerEffect.mayCastAsThoughItHadFlash S.alice oid board) "the permission reaches it"

    -- CR 601.3b's FIRST sentence still holds the line: the permission is read,
    -- rather than the sorcery-speed window being opened for everything. The pair
    -- differs only in which creature card is in the hand, and the Piker has no
    -- bestow, so no choice in its proposal can make it an Aura.
    Spec.it s "CR 601.3b a creature card with no bestow is still refused" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      aid <- S.printingOf s registry "Sigarda's Aid"
      let (oid, _, board) = flashBoard mountain piker [piker, aid]
      Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice board))) "not offered"
      Spec.assertBool s (not (S.castable S.alice oid board)) "not castable"
      Spec.assertBool s (not (PlayerEffect.mayCastAsThoughItHadFlash S.alice oid board)) "the permission does not reach it"

    -- The gameplay half, driven through the priority loop rather than by calling
    -- Cast.castSpell: S.castAnswer takes whatever Cast action it is OFFERED, so
    -- the two runs differ in the Aid and in nothing a test wrote by hand. Without
    -- it alice is offered no cast at all and simply passes.
    Spec.it s "CR 601.3b the offered cast resolves and the bestow creature enters on the opponent's turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      rollicker <- S.printingOf s registry "Nyxborn Rollicker"
      aid <- S.printingOf s registry "Sigarda's Aid"
      let (_, _, board) = flashBoard mountain rollicker [piker, aid]
          (_, _, bare) = flashBoard mountain rollicker [piker]
          play gs = S.runPure S.castAnswer gs Engine.priorityLoop
          after = play board
      Spec.assertEqWith s "the Rollicker is on the battlefield" (S.countOnBattlefieldByName (S.printingName rollicker) S.alice after) 1
      Spec.assertEqWith s "bob is still the active player" (GameState.activePlayer after) S.bob
      Spec.assertEqWith s "and without the Aid it never left her hand" (S.countOnBattlefieldByName (S.printingName rollicker) S.alice (play bare)) 0

    -- CR 702.103b is what the lookahead consults and nothing else: with the
    -- bestow card in hand and NO Aid, the window stays shut, so the choice space
    -- is not a permission of its own.
    Spec.it s "CR 601.3b without the Aid the bestow choice opens nothing" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      rollicker <- S.printingOf s registry "Nyxborn Rollicker"
      let (oid, _, board) = flashBoard mountain rollicker [piker]
      Spec.assertBool s (not (PlayerEffect.mayCastAsThoughItHadFlash S.alice oid board)) "no permission to read"

    -- CR 601.2b: the choice is only CONSIDERED. Nothing is stamped by asking, so
    -- the card in the hand is still a creature card and no Aura, on the very
    -- board that just let it through.
    Spec.it s "CR 601.3b considering the choice writes nothing onto the card" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      rollicker <- S.printingOf s registry "Nyxborn Rollicker"
      aid <- S.printingOf s registry "Sigarda's Aid"
      let (oid, _, board) = flashBoard mountain rollicker [piker, aid]
          asked = PlayerEffect.mayCastAsThoughItHadFlash S.alice oid board
          view = Projection.viewOfObject oid board
      Spec.assertBool s asked "the permission reaches it"
      Spec.assertBool s (not (Set.member Subtype.Aura (Filter.subtypes view))) "no Aura subtype"
      Spec.assertBool s (Set.member CardType.Creature (Filter.cardTypes view)) "still a creature card"
      Spec.assertBool s (Set.member Subtype.Aura (Filter.subtypes (Projection.bestowedView oid board))) "which only the hypothetical carries"

-- Synthetic Untimely Aluren {2}{G} Enchantment: "You may cast creature spells
-- with mana value 4 or greater as though they had flash." Protean Hydra ({X}{G})
-- is mana value 1 in hand (CR 202.3e), so only X can bring it under the grant.
-- flashBoard's ten Forests keep mana from being why a cast fails.
untimelyAlurenSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
untimelyAlurenSpec s registry =
  let board withAluren = do
        forest <- S.printingOf s registry "Forest"
        hydra <- S.printingOf s registry "Protean Hydra"
        aluren <- S.printingOf s registry "Synthetic Untimely Aluren"
        let (oid, _, gs) = flashBoard forest hydra [aluren | withAluren]
        pure (oid, gs)
   in Spec.describe s "UntimelyAluren" $ do
        -- CR 601.3b's second sentence over X: the pair differs in the Aluren.
        Spec.it s "CR 601.3b Untimely Aluren lets Protean Hydra begin on the opponent's turn" $ do
          (oid, gs) <- board True
          (bareOid, bare) <- board False
          Spec.assertBool s (any (S.isCastOf oid) (Action.legalActions S.alice gs)) "offered under the Aluren"
          Spec.assertBool s (not (any (S.isCastOf bareOid) (Action.legalActions S.alice bare))) "and not without it"

-- CR 601.3's search exception is untimed, so neither CR 601.3b's narrowing nor
-- its CR 601.2e re-check reaches it. Synthetic Glacial Blessing lets alice cast
-- from her library while searching; it is bob's turn, so every window a flash
-- permission could open is shut and only the exception lets the card through.
searchIsUntimedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
searchIsUntimedSpec s registry =
  let board land card grant = do
        lands <- S.printingOf s registry land
        blessing <- S.printingOf s registry "Synthetic Glacial Blessing"
        other <- S.printingOf s registry grant
        held <- S.printingOf s registry card
        let (_, gs1) = S.addPermanent blessing S.alice (S.landsInPlay lands 9)
            (_, gs2) = S.addPermanent other S.alice gs1
            (_, gs3) = S.addLibraryCard held S.alice gs2
        pure gs3 {GameState.activePlayer = S.bob, GameState.priority = Just S.alice}
      -- Takes the first offer ONCE and declines after, so a rewound cast
      -- ends the loop instead of being re-offered forever.
      searchCasting :: Natural -> Prompt.Prompt r -> State.State Bool r
      searchCasting x p = case p of
        Prompt.CastWhileSearching _ _ options -> do
          taken <- State.get
          State.put True
          pure (if taken then Nothing else Maybe.listToMaybe options)
        Prompt.ChooseX {} -> pure x
        _ -> pure (S.identityAnswer p)
      castThere x gs = snd (State.evalState (Engine.runGame (searchCasting x) gs (Cast.castWhileSearching S.manaPerformer S.alice)) False)
   in Spec.describe s "SearchIsUntimed" $ do
        -- Under the Aluren X = 2 is outside "4 or greater"; the search needs no flash.
        Spec.it s "CR 601.3 Protean Hydra is cast from the library at X = 2 beside Untimely Aluren" $ do
          gs <- board "Forest" "Protean Hydra" "Synthetic Untimely Aluren"
          Spec.assertEqWith s "the Hydra is on the stack" (length (GameState.stack (castThere 2 gs))) 1

        -- The Aid names the Rollicker only bestowed, and there is no creature to
        -- enchant; the search still offers the printed cost.
        Spec.it s "CR 601.3 Nyxborn Rollicker is cast from the library at its printed cost beside Sigarda's Aid" $ do
          gs <- board "Mountain" "Nyxborn Rollicker" "Sigarda's Aid"
          Spec.assertEqWith s "the Rollicker is on the stack" (length (GameState.stack (castThere 0 gs))) 1

-- ONE board for Yawgmoth's Will, built once and branched. alice has six untapped
-- Swamps -- three for the Will's {2}{B} and three left over, so no assertion
-- below can turn on mana -- the Will in hand, and a Sign in Blood ({B}{B}, no
-- flashback and no casting permission of its own) in her graveyard. bob holds
-- exactly the same six Swamps and the same card in HIS graveyard, which is what
-- makes the CR 109.5 scope observable: the two seats differ in the grant and in
-- nothing else. Both libraries are stocked, since Sign in Blood draws and CR
-- 104.3c would otherwise decide the game before an assertion ran.
--
-- Returns the Will, alice's graveyard card, bob's, and the board.
willBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
willBoard swamp will signInBlood =
  let lands = S.landsFor swamp S.bob 6 (S.landsInPlay swamp 6)
      stock pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard swamp pid g)) gs [1 :: Int .. 3]
      stocked = stock S.bob (stock S.alice lands)
      (willId, withWill) = S.addHandCard will S.alice stocked
      (hers, withHers) = S.addGraveyardCard signInBlood S.alice withWill
      (his, withHis) = S.addGraveyardCard signInBlood S.bob withHers
   in ( willId,
        hers,
        his,
        withHis
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- The same board with the Will cast and resolved.
willResolved :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
willResolved willId gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast S.alice willId)) Stack.resolveTop

-- CR 601.3 / Yawgmoth's Will {2}{B} Sorcery: "Until end of turn, you may play
-- lands and cast spells from your graveyard. If a card would be put into your
-- graveyard from anywhere this turn, exile that card instead."
--
-- The PLAYER-scoped half of CR 601.3's allow clause, where flashback (CastSpec's
-- Firebolt group) is the object-scoped half: the card that becomes castable
-- carries no permission of its own and never learns one.
--
-- BOTH halves of the first sentence are declared, as two arms of one clause: a
-- land is played and never cast (CR 305.1), so the play half is
-- PlayerEffect.PlayLandsFrom and the cast half is PlayerEffect.CastFrom, each
-- naming the caster's own graveyard. The last case here is the play half; the
-- unrestricted producer of that arm is Crucible of Worlds, in its own group
-- below.
yawgmothsWillSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
yawgmothsWillSpec s registry =
  let board = do
        swamp <- S.printingOf s registry "Swamp"
        will <- S.printingOf s registry "Yawgmoth's Will"
        signInBlood <- S.printingOf s registry "Sign in Blood"
        pure (willBoard swamp will signInBlood)
   in Spec.describe s "YawgmothsWill" $ do
        -- The control. Without it every refusal below would also be true of a
        -- board where Sign in Blood was simply unaffordable or the timing wrong.
        Spec.it s "CR 601.3 before the Will resolves the graveyard card is not castable" $ do
          (willId, hers, _, gs) <- board
          Spec.assertBool s (S.castable S.alice willId gs) "the Will itself is castable"
          Spec.assertBool s (not (S.castable S.alice hers gs)) "but the card in the graveyard is not"
          Spec.assertBool s (not (any (S.isCastOf hers) (Action.legalActions S.alice gs))) "and not offered"

        -- The whole card, end to end: graveyard -> stack -> EXILE. The exile is
        -- the second sentence, and the card would be back in the graveyard
        -- without it.
        Spec.it s "CR 601.3 with the Will resolved the graveyard card is cast, resolves and is exiled" $ do
          (willId, hers, _, gs) <- board
          let after = willResolved willId gs
          Spec.assertBool s (S.castable S.alice hers after) "castable from the graveyard"
          Spec.assertBool s (any (S.isCastOf hers) (Action.legalActions S.alice after)) "and offered"
          let cast = S.runPure S.identityAnswer after (S.cast S.alice hers)
              resolved = S.runPure S.identityAnswer cast Stack.resolveTop
          Spec.assertEqWith s "it drew and cost 2 life" (S.lifeOf S.alice resolved) (Just 18)
          Spec.assertEqWith s "it is not back in the graveyard" (Game.zoneMembers Zone.Graveyard S.alice resolved) []
          Spec.assertEqWith s "it was exiled instead, beside the Will" (length (Game.zoneMembers Zone.Exile S.alice resolved)) 2

        -- CR 109.5 / PlayerScope.You: alice's Will does nothing for bob, whose
        -- board is hers in every other respect. Asked of the typed question as
        -- well as of the gate, because CR 307.1's sorcery window is shut for bob
        -- on alice's turn and would refuse his cast on its own.
        Spec.it s "CR 109.5 the You scope does not reach bob's graveyard" $ do
          (willId, hers, his, gs) <- board
          let after = willResolved willId gs
              bobsTurn = after {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
          Spec.assertBool s (PlayerEffect.mayCastFrom S.alice Zone.Graveyard hers after) "alice has the permission"
          Spec.assertBool s (not (PlayerEffect.mayCastFrom S.bob Zone.Graveyard his after)) "bob does not"
          Spec.assertBool s (not (S.castable S.bob his bobsTurn)) "and it is not castable even in his own main phase"
          Spec.assertBool s (not (any (S.isCastOf his) (Action.legalActions S.bob bobsTurn))) "nor offered"

        -- CR 400.1 / 400.3: the grant says "your graveyard", and the two copies of
        -- Sign in Blood differ in nothing but whose graveyard they lie in -- so
        -- this is that word and nothing else. The case above asked whether BOB
        -- may cast his own copy; this asks whether ALICE, who holds the grant,
        -- may reach it, which is the half that decides whether a caster and a
        -- card's owner can ever come apart on this road.
        --
        -- The typed question is asked beside the gate to name WHICH conjunct
        -- refuses. It is the PERMISSION's own zone reference: Yawgmoth's Will
        -- writes PlayerRef.Relative You, so mayCastFrom resolves the pile to
        -- alice's and bob's copy is not in it. Cast.zoneCandidates offers her
        -- bob's graveyard now (see #2169) and the permission is the only thing
        -- standing between the offer and the cast, which is why the assertion
        -- below reads False where it once read True: the empty filter still says
        -- yes to bob's copy, and the reference says no.
        Spec.it s "CR 400.1 the grant does not reach the copy in bob's graveyard" $ do
          (willId, hers, his, gs) <- board
          let after = willResolved willId gs
          Spec.assertBool s (not (S.castable S.alice his after)) "alice may not cast the copy in bob's graveyard"
          Spec.assertBool s (not (any (S.isCastOf his) (Action.legalActions S.alice after))) "nor is it offered to her"
          Spec.assertBool s (S.castable S.alice hers after) "though the identical copy in her own graveyard is castable"
          Spec.assertBool s (not (PlayerEffect.mayCastFrom S.alice Zone.Graveyard his after)) "and the refusal is the permission's zone reference, not its filter"

        -- CR 514.2: "until end of turn" is the stored CR 611.2c carrier's expiry,
        -- so the grant dies at cleanup and the same board refuses the same cast.
        Spec.it s "CR 514.2 the permission ends at cleanup" $ do
          (willId, hers, _, gs) <- board
          let after = willResolved willId gs
              ended = S.runPure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
          -- TWO, one per half of the card's first sentence.
          Spec.assertEqWith s "both stored effects while they last" (length (GameState.playerEffects after)) 2
          Spec.assertEqWith s "nothing stored afterwards" (GameState.playerEffects ended) []
          Spec.assertBool s (not (PlayerEffect.mayCastFrom S.alice Zone.Graveyard hers ended)) "the permission is gone"
          Spec.assertBool s (not (S.castable S.alice hers ended)) "and the cast is refused again"

-- THREE SEATS, each with a Mountain in hand and a Swamp in their own graveyard,
-- and `present` says whether alice also controls a Crucible of Worlds. It is
-- alice's precombat main phase and nobody has played a land yet.
--
-- The Mountain in hand is what keeps every negative below from passing
-- vacuously: on its own turn each seat is offered that Mountain whatever the
-- Crucible does, so an assertion that the graveyard Swamp is absent is read off
-- a list that is never empty for want of a window. Two different basic land
-- types, so the two offers can never be mistaken for each other.
--
-- Prodigal Sorcerer sits in alice's graveyard as the nonland control: the
-- Crucible's sentence is about lands, and a permission read as "play anything
-- from your graveyard" would offer it.
--
-- Returns alice's Swamp, bob's, carol's, alice's Mountain, bob's, the Sorcerer
-- and the board.
crucibleBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
crucibleBoard swamp mountain sorcerer crucible present =
  let (hers, g1) = S.addGraveyardCard swamp S.alice S.threePlayerGame
      (his, g2) = S.addGraveyardCard swamp S.bob g1
      (theirs, g3) = S.addGraveyardCard swamp S.carol g2
      (sorcererId, g4) = S.addGraveyardCard sorcerer S.alice g3
      (herMountain, g5) = S.addHandCard mountain S.alice g4
      (hisMountain, g6) = S.addHandCard mountain S.bob g5
      (_, g7) = S.addHandCard mountain S.carol g6
      g8 = if present then snd (S.addPermanent crucible S.alice g7) else g7
   in ( hers,
        his,
        theirs,
        herMountain,
        hisMountain,
        sorcererId,
        g8
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Is this the offer to play THAT object as a land? Enumerated rather than
-- wildcarded, so a new Action constructor is named by -Werror.
playing :: ObjectId.ObjectId -> Action.Type.Action -> Bool
playing wanted action = case action of
  Action.Type.Play oid _ -> oid == wanted
  Action.Type.Pass -> False
  Action.Type.Cast {} -> False
  Action.Type.Activate _ _ -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot {} -> False
  Action.Type.Foretell _ -> False
  Action.Type.Suspend _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.RollPlanarDie -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
  Action.Type.ActivateManaAbility _ -> False

-- Crucible of Worlds {3} Artifact: "You may play lands from your graveyard." The
-- unrestricted producer of PlayerEffect.PlayLandsFromGraveyard -- one sentence, a
-- static ability of a battlefield permanent, PlayerScope.You, and nothing else on
-- the card -- where Yawgmoth's Will above grants the same arm from the stored CR
-- 611.2c carrier with a duration on it.
crucibleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
crucibleSpec s registry =
  let board present = do
        swamp <- S.printingOf s registry "Swamp"
        mountain <- S.printingOf s registry "Mountain"
        sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
        crucible <- S.printingOf s registry "Crucible of Worlds"
        pure (crucibleBoard swamp mountain sorcerer crucible present)
   in Spec.describe s "CrucibleOfWorlds" $ do
        -- The pair. Two boards differing in the Crucible and in nothing else,
        -- and the hand's Mountain is offered on both -- so the Swamp appearing
        -- is the grant and can be nothing else.
        Spec.it s "CR 305.1 the grant widens the land play to the graveyard" $ do
          (hers, _, _, herMountain, _, sorcererId, without) <- board False
          (_, _, _, _, _, _, with) <- board True
          Spec.assertEqWith s "without it only the hand is offered" (filter isPlay (Action.legalActions S.alice without)) [Action.Type.Play herMountain Nothing]
          Spec.assertEqWith
            s
            "with it the graveyard Swamp joins the Mountain"
            (filter isPlay (Action.legalActions S.alice with))
            [Action.Type.Play herMountain Nothing, Action.Type.Play hers Nothing]
          -- CR 305.1's subject is a LAND CARD, so the Sorcerer in the same
          -- graveyard is offered nothing. Implied by the equality above and
          -- asserted anyway, because it is the reading the equality is guarding
          -- against.
          Spec.assertBool s (notElem (Action.Type.Play sorcererId Nothing) (Action.legalActions S.alice with)) "the nonland card in the same graveyard is not offered"

        -- CR 109.5 at three seats, which is what tells "you" from "an opponent"
        -- and from "the next seat in turn order". Each opponent is asked in
        -- their OWN main phase, so CR 305.1's window is open and the refusal is
        -- about the scope.
        Spec.it s "CR 109.5 the You scope reaches neither opponent's graveyard" $ do
          (hers, his, theirs, _, hisMountain, _, with) <- board True
          let bobsTurn = with {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
              carolsTurn = with {GameState.activePlayer = S.carol, GameState.priority = Just S.carol}
          Spec.assertBool s (elem (Zone.Graveyard, S.alice) (PlayerEffect.playLandPiles S.alice with)) "alice has the permission"
          Spec.assertBool s (notElem (Zone.Graveyard, S.bob) (PlayerEffect.playLandPiles S.bob with)) "bob does not"
          Spec.assertBool s (notElem (Zone.Graveyard, S.carol) (PlayerEffect.playLandPiles S.carol with)) "nor carol"
          Spec.assertEqWith s "bob is offered his hand and nothing else" (filter isPlay (Action.legalActions S.bob bobsTurn)) [Action.Type.Play hisMountain Nothing]
          Spec.assertBool s (notElem (Action.Type.Play his Nothing) (Action.legalActions S.bob bobsTurn)) "not bob's own graveyard Swamp"
          Spec.assertBool s (notElem (Action.Type.Play theirs Nothing) (Action.legalActions S.carol carolsTurn)) "nor carol's"
          -- And the GRANTED player's own Swamp is not offered to them either,
          -- which is the other way a zone permission could leak: exile is
          -- shared, a graveyard is not (CR 400.1), so carol may not play out of
          -- alice's even though alice may.
          Spec.assertBool s (notElem (Action.Type.Play hers Nothing) (Action.legalActions S.carol carolsTurn)) "and carol cannot reach alice's graveyard"

        -- CR 305.2a: the count is applied ABOVE this in
        -- Pawl.Engine.Action.legalActions and the grant does not touch it. One
        -- land already played leaves the allowance equal to the tally, so BOTH
        -- offers go -- a grant read as a second allowance would leave the Swamp.
        Spec.it s "CR 305.2a a player who has played their land is offered neither zone" $ do
          (hers, _, _, _, _, _, with) <- board True
          let played = with {GameState.landsPlayed = Map.singleton S.alice 1}
          Spec.assertEqWith s "the allowance is still one" (PlayerEffect.landPlaysAllowed S.alice played) 1
          Spec.assertEqWith s "and no land play is offered at all" (filter isPlay (Action.legalActions S.alice played)) []
          Spec.assertBool s (elem (Zone.Graveyard, S.alice) (PlayerEffect.playLandPiles S.alice played)) "though the permission is still standing"
          Spec.assertBool s (notElem (Action.Type.Play hers Nothing) (Action.legalActions S.alice played)) "so the Swamp is refused by the count"

        -- CR 305.1's window. The same board one phase earlier: a grant that
        -- widened the zone must not widen the moment.
        Spec.it s "CR 305.1 the grant does not open the sorcery-speed window" $ do
          (hers, _, _, _, _, _, with) <- board True
          let upkeep = with {GameState.phase = Phase.Beginning BeginningStep.Upkeep}
          Spec.assertEqWith s "no land play is offered in her upkeep" (filter isPlay (Action.legalActions S.alice upkeep)) []
          Spec.assertBool s (notElem (Action.Type.Play hers Nothing) (Action.legalActions S.alice upkeep)) "the graveyard Swamp included"

-- Cast ONE named object and pass at every other prompt -- playOnly above for a
-- cast. Pinned to an id rather than to "whichever cast is offered", so a board
-- that stopped offering it passes rather than repairing the case with some other
-- cast.
castOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
castOnly wanted p = case p of
  Prompt.ChooseAction _ _ actions -> case filter (S.isCastOf wanted) actions of
    h : _ -> h
    [] -> Action.Type.Pass
  _ -> S.identityAnswer p

-- alice, bob and carol each have three Forests and a library whose top card is a
-- creature; alice's library holds a SECOND creature one card down, and her hand
-- holds a Rampant Growth. `top` is her library's top card and `present` whether
-- the Horde is on her battlefield, so every pair of boards below differs in one
-- of those two and in nothing else.
hordeBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
hordeBoard forest horde elves rampant top present =
  let lands = S.landsFor forest S.carol 3 (S.landsFor forest S.bob 3 (S.landsFor forest S.alice 3 S.threePlayerGame))
      -- S.addLibraryCard puts each card ON TOP of the last, so the deepest goes
      -- in first and `top` is what the permission can reach.
      (_, g1) = S.addLibraryCard forest S.alice lands
      (herDeep, g2) = S.addLibraryCard elves S.alice g1
      (herTop, g3) = S.addLibraryCard top S.alice g2
      (_, g4) = S.addLibraryCard forest S.bob g3
      (hisTop, g5) = S.addLibraryCard elves S.bob g4
      (_, g6) = S.addLibraryCard forest S.carol g5
      (theirTop, g7) = S.addLibraryCard elves S.carol g6
      (herHand, g8) = S.addHandCard rampant S.alice g7
      g9 = if present then snd (S.addPermanent horde S.alice g8) else g8
   in ( herTop,
        herDeep,
        hisTop,
        theirTop,
        herHand,
        g9
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Garruk's Horde {5}{G}{G} Creature -- Beast 7/7: "Trample / Play with the top
-- card of your library revealed. / You may cast creature spells from the top of
-- your library."
--
-- A producer of PlayerEffect.CastFrom naming a library, and the LIBRARY's entry in
-- Pawl.Engine.Cast.castZones: a CR 601.3 permission naming a zone the rules give
-- nobody, where Yawgmoth's Will above names the graveyard. The narrowing to the
-- TOP card is Cast.pileCandidates' and not the Filter's, so these cases prove
-- the two halves separately -- the second creature one card down is the one that
-- can tell them apart.
--
-- Not implemented: "Play with the top card of your library revealed", which
-- data/cards/garruks-horde.json omits -- pawl hands every answerer the whole
-- game already, so a revealed card is indistinguishable from a hidden one
-- (#1412). Neither stricter nor weaker than printed, and no case below rests on
-- it.
garruksHordeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
garruksHordeSpec s registry =
  let board top present = do
        forest <- S.printingOf s registry "Forest"
        horde <- S.printingOf s registry "Garruk's Horde"
        elves <- S.printingOf s registry "Llanowar Elves"
        rampant <- S.printingOf s registry "Rampant Growth"
        topPrinting <- S.printingOf s registry top
        pure (hordeBoard forest horde elves rampant topPrinting present)
   in Spec.describe s "GarruksHorde" $ do
        -- The pair. Two boards differing in the Horde and in nothing else, with
        -- the same Forests and the same hand on both -- so the offer appearing
        -- is the permission and can be nothing else.
        Spec.it s "CR 601.3 without the Horde the same top card is not castable" $ do
          (herTop, _, _, _, herHand, without) <- board "Llanowar Elves" False
          Spec.assertBool s (not (any (S.isCastOf herTop) (Action.legalActions S.alice without))) "the top card is not offered"
          Spec.assertBool s (not (S.castable S.alice herTop without)) "nor castable"
          Spec.assertBool s (not (PlayerEffect.mayCastFrom S.alice Zone.Library herTop without)) "and the typed question says no"
          Spec.assertBool s (any (S.isCastOf herHand) (Action.legalActions S.alice without)) "though her hand is offered on the same board"

        -- PermissionVerb.Cast: the Filter admits Dryad Arbor, a land creature,
        -- and the permission still plays no land (CR 305.1: a land is never
        -- cast). Serra Paragon's Play verb is the one that would.
        Spec.it s "CR 305.1 a creature land on top is not playable under a cast permission" $ do
          (herTop, _, _, _, _, gs) <- board "Dryad Arbor" True
          Spec.assertBool s (notElem (herTop, Nothing) (Action.playableLands S.alice gs)) "Dryad Arbor on top is not playable"
          Spec.assertBool s (PlayerEffect.mayCastFrom S.alice Zone.Library herTop gs) "though the permission's Filter matches it"

        -- The Filter half: "creature spells". The refusal is not cost or timing,
        -- which the identical card in her HAND on the same board shows.
        Spec.it s "CR 601.3 a noncreature card on top is not offered" $ do
          (herTop, _, _, _, herHand, gs) <- board "Rampant Growth" True
          Spec.assertBool s (not (any (S.isCastOf herTop) (Action.legalActions S.alice gs))) "the sorcery on top is not offered"
          Spec.assertBool s (not (S.castable S.alice herTop gs)) "nor castable"
          Spec.assertBool s (not (PlayerEffect.mayCastFrom S.alice Zone.Library herTop gs)) "the permission does not match it"
          Spec.assertBool s (any (S.isCastOf herHand) (Action.legalActions S.alice gs)) "while the same card in her hand is offered"

        -- The zone half: "the TOP of your library". The second Llanowar Elves is
        -- a creature the permission matches, and it is one card down -- so this
        -- is Cast.pileCandidates' narrowing and nothing else.
        Spec.it s "CR 601.3 only the top card is reached, not the creature beneath it" $ do
          (herTop, herDeep, _, _, _, gs) <- board "Llanowar Elves" True
          Spec.assertBool s (not (any (S.isCastOf herDeep) (Action.legalActions S.alice gs))) "the creature one card down is not offered"
          Spec.assertBool s (not (S.castable S.alice herDeep gs)) "nor castable"
          Spec.assertBool s (PlayerEffect.mayCastFrom S.alice Zone.Library herDeep gs) "so the refusal is not the permission's own filter"
          Spec.assertBool s (any (S.isCastOf herTop) (Action.legalActions S.alice gs)) "while the card above it is offered"

        -- CR 601.3's OTHER limb, on the new road: a permission widens the zone
        -- and a prohibition still closes it. Grafdigger's Cage is the pair's
        -- second permanent, under BOB, so the refusal is its PlayerScope.EachPlayer
        -- rather than anything about who controls the Horde. Pawl.CastSpec's
        -- Grafdigger's Cage group proves the same disjunct on the mid-search road.
        Spec.it s "CR 601.3 a prohibition still closes the zone the permission opened" $ do
          cage <- S.printingOf s registry "Grafdigger's Cage"
          (herTop, _, _, _, herHand, gs) <- board "Llanowar Elves" True
          let caged = snd (S.addPermanent cage S.bob gs)
          Spec.assertBool s (not (any (S.isCastOf herTop) (Action.legalActions S.alice caged))) "the top card is not offered with the Cage out"
          Spec.assertBool s (not (S.castable S.alice herTop caged)) "nor castable"
          Spec.assertBool s (PlayerEffect.mayCastFrom S.alice Zone.Library herTop caged) "so the refusal is the prohibition, not the permission"
          Spec.assertBool s (any (S.isCastOf herTop) (Action.legalActions S.alice gs)) "and the same cast is offered on the same board without it"
          Spec.assertBool s (any (S.isCastOf herHand) (Action.legalActions S.alice caged)) "while the hand the sentence does not name is untouched"

        -- CR 109.5 at three seats. Each opponent is asked in their OWN main
        -- phase, so CR 307.1's window is open and the refusal is the scope.
        Spec.it s "CR 109.5 the You scope reaches neither opponent's library" $ do
          (herTop, _, hisTop, theirTop, _, gs) <- board "Llanowar Elves" True
          let bobsTurn = gs {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
              carolsTurn = gs {GameState.activePlayer = S.carol, GameState.priority = Just S.carol}
          Spec.assertBool s (not (any (S.isCastOf hisTop) (Action.legalActions S.bob bobsTurn))) "bob is not offered his own top card"
          Spec.assertBool s (not (any (S.isCastOf theirTop) (Action.legalActions S.carol carolsTurn))) "nor carol hers"
          Spec.assertBool s (PlayerEffect.mayCastFrom S.alice Zone.Library herTop gs) "alice has the permission"
          Spec.assertBool s (not (PlayerEffect.mayCastFrom S.bob Zone.Library hisTop gs)) "bob does not"
          Spec.assertBool s (not (PlayerEffect.mayCastFrom S.carol Zone.Library theirTop gs)) "nor carol"
          -- And a library is a per-player pile (CR 400.1), so the player who
          -- HOLDS the permission cannot reach anybody else's top card either.
          Spec.assertBool s (not (any (S.isCastOf hisTop) (Action.legalActions S.alice gs))) "and alice cannot cast bob's top card"
          Spec.assertBool s (not (S.castable S.alice hisTop gs)) "nor is it castable by her"

-- alice, bob and carol each have a library whose top card is a land; alice's
-- holds a SECOND land one card down and a third at the bottom, and her hand
-- holds a Forest. `present` says whether Future Sight is on her battlefield, so
-- every pair of boards below differs in that and in nothing else. It is alice's
-- precombat main phase and nobody has played a land yet.
--
-- A DIFFERENT basic land in every slot an assertion names -- a Swamp on top, a
-- Mountain beneath it, an Island and a Plains for the opponents -- so no two
-- offers can be mistaken for each other; the Forest fills the slots nothing
-- discriminates. The Forest in her HAND is what keeps every negative from
-- passing vacuously: CR 305.1's own zone is offered on every board below, so a
-- list that lacks the library card is never a list that is empty for want of a
-- window.
--
-- Returns alice's top card, the land beneath it, bob's top card, carol's, the
-- Forest in alice's hand and the board.
futureSightBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
futureSightBoard swamp mountain forest island plains sight present =
  let -- Three Forests under alice, which is the mana the cast half's case below
      -- spends; no case here plays one, so CR 305.2a's tally starts at zero all
      -- the same.
      mana = S.landsFor forest S.alice 3 S.threePlayerGame
      -- S.addLibraryCard puts each card ON TOP of the last, so the deepest goes
      -- in first and the Swamp is what the permission can reach.
      (_, g1) = S.addLibraryCard forest S.alice mana
      (herDeep, g2) = S.addLibraryCard mountain S.alice g1
      (herTop, g3) = S.addLibraryCard swamp S.alice g2
      (hisTop, g4) = S.addLibraryCard island S.bob g3
      (theirTop, g5) = S.addLibraryCard plains S.carol g4
      (herHand, g6) = S.addHandCard forest S.alice g5
      g7 = if present then snd (S.addPermanent sight S.alice g6) else g6
   in ( herTop,
        herDeep,
        hisTop,
        theirTop,
        herHand,
        g7
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Future Sight {2}{U}{U}{U} Enchantment: "Play with the top card of your library
-- revealed. / You may play lands and cast spells from the top of your library."
--
-- The PLAY half's producer, and the play-side twin of Garruk's Horde above: a
-- land is played by CR 305.1's special action and never cast, so the top-card
-- narrowing Pawl.Engine.Cast.pileCandidates states has to be read by
-- Pawl.Engine.Action.playableLands too. The land ONE CARD DOWN is what tells that
-- narrowing from the permission itself.
--
-- Not implemented: "Play with the top card of your library revealed", which
-- data/cards/future-sight.json omits -- pawl hands every answerer the whole game
-- already, so a revealed card is indistinguishable from a hidden one (#1412).
-- Neither stricter nor weaker than printed, and no case below rests on it.
futureSightSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
futureSightSpec s registry =
  let board present = do
        swamp <- S.printingOf s registry "Swamp"
        mountain <- S.printingOf s registry "Mountain"
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        plains <- S.printingOf s registry "Plains"
        sight <- S.printingOf s registry "Future Sight"
        pure (futureSightBoard swamp mountain forest island plains sight present)
   in Spec.describe s "FutureSight" $ do
        -- The pair. Two boards differing in Future Sight and in nothing else,
        -- and the hand's Forest is offered on both -- so the Swamp appearing is
        -- the grant and can be nothing else.
        Spec.it s "CR 305.1 without Future Sight the same top card is not offered" $ do
          (herTop, _, _, _, herHand, without) <- board False
          (_, _, _, _, _, with) <- board True
          Spec.assertEqWith s "without it only the hand is offered" (filter isPlay (Action.legalActions S.alice without)) [Action.Type.Play herHand Nothing]
          Spec.assertEqWith
            s
            "with it the library's top Swamp joins the Forest"
            (filter isPlay (Action.legalActions S.alice with))
            [Action.Type.Play herHand Nothing, Action.Type.Play herTop Nothing]
          Spec.assertBool s (notElem (Zone.Library, S.alice) (PlayerEffect.playLandPiles S.alice without)) "and the permission is absent on the board without it"

        -- The zone half: "the TOP of your library". The Mountain one card down is
        -- a land the grant's pile holds, and it is refused -- so this is
        -- Cast.pileCandidates' narrowing and nothing else.
        Spec.it s "CR 305.1 only the top card is reached, not the land beneath it" $ do
          (herTop, herDeep, _, _, _, with) <- board True
          Spec.assertBool s (notElem (Action.Type.Play herDeep Nothing) (Action.legalActions S.alice with)) "the land one card down is not offered"
          Spec.assertBool s (elem (Action.Type.Play herTop Nothing) (Action.legalActions S.alice with)) "while the card above it is"
          Spec.assertBool s (elem (Zone.Library, S.alice) (PlayerEffect.playLandPiles S.alice with)) "so the refusal is not the permission, which names her library"

        -- CR 109.5 at three seats. Each opponent is asked in their OWN main
        -- phase, so CR 305.1's window is open and the refusal is the scope. A
        -- library is one player's (CR 400.1), so the seat that HOLDS the
        -- permission cannot reach anybody else's top card either.
        Spec.it s "CR 109.5 the You scope reaches neither opponent's library" $ do
          (_, _, hisTop, theirTop, _, with) <- board True
          let bobsTurn = with {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
              carolsTurn = with {GameState.activePlayer = S.carol, GameState.priority = Just S.carol}
          Spec.assertBool s (notElem (Action.Type.Play hisTop Nothing) (Action.legalActions S.bob bobsTurn)) "bob is not offered his own top card"
          Spec.assertBool s (notElem (Action.Type.Play theirTop Nothing) (Action.legalActions S.carol carolsTurn)) "nor carol hers"
          Spec.assertBool s (notElem (Action.Type.Play hisTop Nothing) (Action.legalActions S.alice with)) "and alice cannot play bob's top card"
          Spec.assertBool s (notElem (Zone.Library, S.bob) (PlayerEffect.playLandPiles S.alice with)) "her permission names her own library alone"

        -- The CAST half of the same sentence, which Future Sight states
        -- unrestricted where Garruk's Horde above narrows it to creature spells.
        -- A Llanowar Elves put on top of the same library, on the pair of boards
        -- the rest of this group uses, so the offer is the enchantment's.
        Spec.it s "CR 601.3 the same sentence's cast half reaches the top card" $ do
          elves <- S.printingOf s registry "Llanowar Elves"
          (_, _, _, _, _, with) <- board True
          (_, _, _, _, _, without) <- board False
          let (withTop, withElves) = S.addLibraryCard elves S.alice with
              (withoutTop, withoutElves) = S.addLibraryCard elves S.alice without
          Spec.assertBool s (any (S.isCastOf withTop) (Action.legalActions S.alice withElves)) "the top card is offered as a cast"
          Spec.assertBool s (not (any (S.isCastOf withoutTop) (Action.legalActions S.alice withoutElves))) "and is not offered on the board without Future Sight"

-- Cast whichever of these objects the engine offers, and pass otherwise. Pinned
-- to a LIST of ids rather than to "whichever cast is offered", so a board that
-- stopped offering them passes rather than repairing the case with some other
-- cast -- castOnly above, widened to the two cards a per-turn budget is counted
-- over.
castAnyOf :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
castAnyOf wanted p = case p of
  Prompt.ChooseAction _ _ actions -> case filter (\a -> any (`S.isCastOf` a) wanted) actions of
    h : _ -> h
    [] -> Action.Type.Pass
  _ -> S.identityAnswer p

-- alice, bob and carol each have three Forests; alice's library holds TWO Fogs
-- on top of two Forests, so the second Fog is on top the moment the first is
-- cast and CR 104.3c cannot deck her. `granting` is the permanent put onto her
-- battlefield -- Johann for the budgeted permission, Future Sight for the
-- unlimited one -- so a pair of boards differs in that and in nothing else.
--
-- `GameState.remaining` is emptied so the priority loop stops at the end of this
-- main phase: the offers below are read DURING alice's turn, where a loop that
-- ran on into bob's would have reset the very budget under test.
johannBoard :: Printing.Printing -> Printing.Printing -> Maybe Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
johannBoard forest fog granting =
  let lands = S.landsFor forest S.carol 3 (S.landsFor forest S.bob 3 (S.landsFor forest S.alice 3 S.threePlayerGame))
      -- S.addLibraryCard puts each card ON TOP of the last, so the deepest goes
      -- in first.
      (_, g1) = S.addLibraryCard forest S.alice lands
      (_, g2) = S.addLibraryCard forest S.alice g1
      (deep, g3) = S.addLibraryCard fog S.alice g2
      (top, g4) = S.addLibraryCard fog S.alice g3
      (_, g5) = S.addLibraryCard forest S.bob g4
      (_, g6) = S.addLibraryCard forest S.carol g5
      g7 = maybe g6 (\printing -> snd (S.addPermanent printing S.alice g6)) granting
   in ( top,
        deep,
        g7
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.remaining = Seq.empty
          }
      )

-- Johann, Apprentice Sorcerer {2}{U}{R} Legendary Creature -- Human Wizard
-- Sorcerer 2/5: "You may look at the top card of your library any time. / Once
-- each turn, you may cast an instant or sorcery spell from the top of your
-- library."
--
-- The producer of PermissionLimit.OnceEachTurn, and the budgeted twin of Future
-- Sight below it: one CR 601.3 permission naming the top of a library, told from
-- the unlimited one by how many casts a turn it is good for.
--
-- Not implemented: "You may look at the top card of your library any time",
-- which data/cards/johann-apprentice-sorcerer.json omits under the precedent
-- data/cards/garruks-horde.json sets -- pawl hands every answerer the whole game
-- already, so a looked-at card is indistinguishable from a hidden one (#1412).
-- Neither stricter nor weaker than printed, and no case below rests on it.
johannSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
johannSpec s registry =
  let board granting = do
        forest <- S.printingOf s registry "Forest"
        fog <- S.printingOf s registry "Fog"
        printing <- traverse (S.printingOf s registry) granting
        pure (johannBoard forest fog printing)
   in Spec.describe s "Johann" $ do
        -- The gameplay-level proof (design.md section 4), driven through the
        -- priority loop rather than by calling Cast.castSpell, which does not
        -- gate. alice takes every Fog the engine offers off the top, on two
        -- boards differing only in which permanent grants the permission. ONE
        -- Fog in the graveyard rather than two is the whole assertion, and the
        -- Future Sight board is what says the second cast was refused by the
        -- budget and not by the mana, the timing or the top-card narrowing.
        Spec.it s "CR 601.3 the once-each-turn permission casts one spell off the top and refuses the next" $ do
          (top, deep, budgeted) <- board (Just "Johann, Apprentice Sorcerer")
          (topU, deepU, unlimited) <- board (Just "Future Sight")
          let after = S.runPure (castAnyOf [top, deep]) budgeted Engine.priorityLoop
              afterU = S.runPure (castAnyOf [topU, deepU]) unlimited Engine.priorityLoop
          Spec.assertEqWith s "only one Fog left alice's library under Johann" (length (Game.zoneMembers Zone.Library S.alice after)) 3
          Spec.assertEqWith s "so exactly one Fog is in her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
          Spec.assertEqWith s "while Future Sight's unlimited permission casts both off the same library" (length (Game.zoneMembers Zone.Graveyard S.alice afterU)) 2
          Spec.assertEqWith s "and leaves it two cards shorter" (length (Game.zoneMembers Zone.Library S.alice afterU)) 2

        -- The pair's other half: WITHOUT Johann the same top card is not
        -- castable at all, so the first cast above was his permission and not
        -- some rule of the game.
        Spec.it s "CR 601.3 without Johann the top card is not castable" $ do
          (top, _, bare) <- board Nothing
          Spec.assertBool s (not (any (S.isCastOf top) (Action.legalActions S.alice bare))) "the top card is not offered"
          Spec.assertBool s (not (PlayerEffect.mayCastFrom S.alice Zone.Library top bare)) "and the typed question says no"

-- Spider-Punk {1}{R} Legendary Creature -- Spider Human Hero 2/1 (Marvel's
-- Spider-Man, 92), "Spells and abilities can't be countered". Its counter runs
-- are in data/scenarios/cast-permission.
--
-- All four of the card's printed clauses are in its file now, and only this one
-- is read here: nothing on this board prevents damage, no other Spider enters,
-- and S.addPermanent inserts Spider-Punk into the battlefield directly rather
-- than raising an entry event, so CR 702.136a's riot has no CR 614.1c
-- replacement to be. CR 615.12's clause is proved in Pawl.ReplacementSpec's
-- "Spider-Punk (CR 615.12)" group instead, where a Mending Hands shield gives it
-- something to defeat.
spiderPunkSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
spiderPunkSpec s registry =
  Spec.describe s "SpiderPunk" $ do
    -- CR 113.6g's carrier is untouched, which is what keeps the two apart:
    -- Spider-Punk's OWN card says nothing about being countered, and the
    -- protection it hands out comes from the CR 613.11 axis alone.
    Spec.it s "CR 113.6g Spider-Punk's own card field is Counterable" $ do
      punk <- S.printingOf s registry "Spider-Punk"
      Spec.assertEqWith s "the card field" (Face.counterability (S.combinedFace punk)) Counterability.Counterable

-- Jared Carthalion, True Heir {R}{G}{W} Legendary Creature -- Human Warrior 3/3
-- (Commander Legends, 281): "When Jared Carthalion enters, target opponent
-- becomes the monarch. You can't become the monarch this turn." One trigger
-- carrying both sentences, which is how the card prints them.
--
-- The card is in the pool for the second sentence, and it is the ONLY printing
-- that restricts who may be crowned -- which makes it the sole producer of CR
-- 725.4's "the next player in turn order who can become the monarch". CR 725.1
-- and CR 725.3 gate nobody, so on the ordinary route it is CR 101.2 that makes
-- the "can't" win.
--
-- Its third sentence -- "If damage would be dealt to Jared Carthalion while
-- you're the monarch, prevent that damage and put that many +1/+1 counters on
-- it" -- is transcribed too, and belongs to a different subsystem: CR 604.2's
-- gate on a printed replacement ability, proven in Pawl.ReplacementSpec's
-- "Jared Carthalion, True Heir (CR 604.2)" group. Nothing here reaches it -- no
-- case below deals damage.
--
-- Two seats and no departure, which is all the primary observable needs. Every
-- case runs on one board -- alice's Jared, her Palace Jailer and her Goblin Piker
-- on the battlefield, nobody crowned -- and differs only in which enters-the-
-- battlefield event is fed to the trigger gatherer. That is what makes each
-- negative a statement about the restriction rather than about a board that could
-- not crown anyone anyway.
--
-- Palace Jailer ("When Palace Jailer enters, you become the monarch") is the
-- second route on purpose: MonarchTarget.TheController, where Jared's own first
-- clause is MonarchTarget.InSlot and CR 725.2's steal is ControllerOfSource. All
-- three read one gate, so no case here passes through an ungated route.
jaredBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
jaredBoard jared jailer piker =
  let (jaredId, gs1) = S.addPermanent jared S.alice (Setup.emptyGame S.bothPlayers)
      (jailerId, gs2) = S.addPermanent jailer S.alice gs1
      (pikerId, gs3) = S.addPermanent piker S.alice gs2
   in (jaredId, jailerId, pikerId, gs3)

-- One permanent's CR 603.6a entry, gathered and resolved. The permanent is already
-- on the battlefield, so this feeds the event alone -- the same staging
-- ExpirySpec's monarch group uses.
etbResolved :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
etbResolved oid gs =
  let entered = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
      withEvent = S.withEvents [GameEvent.Moved (Moved.moved entered (Projection.project oid gs))] gs
   in S.runPure S.identityAnswer (S.runPure S.identityAnswer withEvent Engine.settleForPriority) Engine.priorityLoop

-- CR 725.2's crown steal, as the event it triggers off: `attacker` deals combat
-- damage to bob, who must be the monarch for the inherent ability to match.
damageToTheMonarch :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
damageToTheMonarch attacker gs =
  let dmg = DamageEvent.MkDamageEvent attacker (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing Nothing mempty False DamageKind.Combat
      withEvent = S.withEvents [GameEvent.DamageDealt dmg] gs
   in S.runPure S.identityAnswer (S.runPure S.identityAnswer withEvent Engine.settleForPriority) Engine.priorityLoop

jaredSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
jaredSpec s registry =
  let board = do
        jared <- S.printingOf s registry "Jared Carthalion, True Heir"
        jailer <- S.printingOf s registry "Palace Jailer"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (jaredBoard jared jailer piker)
   in Spec.describe s "JaredCarthalionTrueHeir" $ do
        -- The card's own first clause, which is also what puts a monarch on the
        -- board for everything below: CR 601.2c's target slot, re-read at
        -- resolution, crowning the ONLY opponent two seats offer.
        Spec.it s "CR 725.1 his enters trigger crowns the targeted opponent, and stores the restriction on his controller" $ do
          (jaredId, _, _, gs) <- board
          let after = etbResolved jaredId gs
          Spec.assertEqWith s "bob is the monarch" (GameState.monarch after) (Just S.bob)
          Spec.assertEqWith s "one stored CR 611.2c effect" (fmap ActivePlayerEffect.effect (GameState.playerEffects after)) [PlayerEffect.Type.CantBecomeMonarch]
          Spec.assertEqWith s "scoped to its controller" (fmap ActivePlayerEffect.scope (GameState.playerEffects after)) [AffectedPlayers.Scoped PlayerScope.You]
          Spec.assertEqWith s "who is alice" (fmap ActivePlayerEffect.controller (GameState.playerEffects after)) [S.alice]
          Spec.assertBool s (PlayerEffect.prohibitsBecomingMonarch S.alice after) "so alice can't become the monarch"
          Spec.assertBool s (not (PlayerEffect.prohibitsBecomingMonarch S.bob after)) "and bob still can"

        -- THE CONTROL for the case below, on the same board: with Jared's trigger
        -- never fed, the Jailer's "you become the monarch" crowns alice. Without
        -- this, the refusal below could be a Jailer whose ETB never resolved.
        Spec.it s "CR 725.1 with no restriction standing, Palace Jailer's enters trigger crowns alice" $ do
          (_, jailerId, _, gs) <- board
          Spec.assertEqWith s "alice takes the crown" (GameState.monarch (etbResolved jailerId gs)) (Just S.alice)

        -- THE PRIMARY OBSERVABLE. Two seats, no departure: an
        -- Effect.BecomeMonarch aimed at a restricted player does nothing, and CR
        -- 725.3's "the current monarch ceases to be the monarch" never fires
        -- either -- bob keeps the crown rather than the game losing it.
        Spec.it s "CR 101.2 / 725.1 the restriction stops Palace Jailer's TheController crowning outright" $ do
          (jaredId, jailerId, _, gs) <- board
          let restricted = etbResolved jaredId gs
              after = etbResolved jailerId restricted
          Spec.assertEqWith s "bob keeps the crown" (GameState.monarch after) (Just S.bob)
          Spec.assertEqWith s "and no crowning of alice was recorded" (filter (== GameEvent.BecameMonarch S.alice) (S.eventsOf after)) []

        -- CR 611.2a/514.2: the duration is the stored carrier's expiry and
        -- nothing else, so the SAME Jailer trigger crowns alice once the turn has
        -- ended. This is what says the restriction is "this turn" rather than
        -- permanent.
        Spec.it s "CR 514.2 the restriction ends at cleanup, and then the same crowning lands" $ do
          (jaredId, jailerId, _, gs) <- board
          let restricted = etbResolved jaredId gs
              ended = S.runPure S.identityAnswer restricted (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
          Spec.assertEqWith s "nothing stored" (GameState.playerEffects ended) []
          Spec.assertBool s (not (PlayerEffect.prohibitsBecomingMonarch S.alice ended)) "alice may be crowned again"
          Spec.assertEqWith s "so the Jailer's ETB now crowns her" (GameState.monarch (etbResolved jailerId ended)) (Just S.alice)

        -- The vacuity trap this issue was filed with: CR 725.2's inherent ability
        -- is SOURCELESS and reaches the crown through MonarchTarget
        -- .ControllerOfSource, a different arm from the case above. The gate is
        -- read at the one place all three arms meet, so the steal is stopped too
        -- -- and the ability still triggers and still resolves, it just crowns
        -- nobody.
        Spec.it s "CR 101.2 / 725.2 the restriction stops the sourceless crown steal as well" $ do
          (jaredId, _, pikerId, gs) <- board
          let restricted = etbResolved jaredId gs
          Spec.assertEqWith s "bob was crowned by Jared's own trigger" (GameState.monarch restricted) (Just S.bob)
          Spec.assertEqWith s "and keeps the crown through alice's combat damage" (GameState.monarch (damageToTheMonarch pikerId restricted)) (Just S.bob)

-- CR 601.3a / Void Winnower {9} Creature -- Eldrazi: "Your opponents can't cast
-- spells with even mana values. (Zero is even.)"
--
-- ONE board, built twice, and `extra` is the only thing the two ever differ by.
-- alice and bob each have nine untapped Mountains, so mana is never why a cast is
-- missing, and each holds a Goblin Piker ({1}{R}, mana value 2 -- EVEN). bob also
-- holds a Lightning Bolt ({R}, mana value 1 -- ODD) and a Molten Disaster
-- ({X}{R}{R}), whose mana value is 2 in his hand by CR 202.3e and either parity
-- once X is chosen.
--
-- The three cards bob holds are the discriminating set: the Bolt differs from the
-- Piker in PARITY alone, and the Disaster differs from the Piker in the VARIABLE
-- alone -- same seat, same mana, same moment, same even mana value. alice's own
-- Piker is the SCOPE control, since no EachPlayer reading of the ability could
-- leave it castable.
--
-- Returns (alice's Piker, bob's Piker, bob's Bolt, bob's Disaster, board).
voidWinnowerBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  [Printing.Printing] ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
voidWinnowerBoard mountain piker bolt disaster extra =
  let base = S.landsInPlay mountain 9
      withBobsLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.bob g)) base [1 .. 9 :: Int]
      (alicesPiker, gs1) = S.addHandCard piker S.alice withBobsLands
      (bobsPiker, gs2) = S.addHandCard piker S.bob gs1
      (bobsBolt, gs3) = S.addHandCard bolt S.bob gs2
      (bobsDisaster, gs4) = S.addHandCard disaster S.bob gs3
      put g printing = snd (S.addPermanent printing S.alice g)
   in ( alicesPiker,
        bobsPiker,
        bobsBolt,
        bobsDisaster,
        (List.foldl' put gs4 extra) {GameState.phase = Phase.PrecombatMain}
      )

-- Whatever that player may do, asked in their own precombat main phase with an
-- empty stack -- so a sorcery, a creature spell and an instant are all inside CR
-- 307.1's window and timing is never the reason one is missing.
askedOf :: PlayerId.PlayerId -> GameState.GameState -> [Action.Type.Action]
askedOf pid gs = Action.legalActions pid (gs {GameState.activePlayer = pid, GameState.priority = Just pid})

voidWinnowerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
voidWinnowerSpec s registry =
  Spec.describe s "VoidWinnower" $ do
    -- CR 601.3a's quality-bearing prohibition on the axis a NAME cannot answer:
    -- the two cards refused and allowed here are told apart by their mana value
    -- and by nothing else.
    Spec.it s "CR 601.3a an opponent's even spell is refused and their odd one is not" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      disaster <- S.printingOf s registry "Molten Disaster"
      winnower <- S.printingOf s registry "Void Winnower"
      let (alicesPiker, bobsPiker, bobsBolt, _, board) = voidWinnowerBoard mountain piker bolt disaster [winnower]
          (_, barePiker, _, _, bare) = voidWinnowerBoard mountain piker bolt disaster []
      Spec.assertBool s (not (any (S.isCastOf bobsPiker) (askedOf S.bob board))) "the mana value 2 spell is refused"
      Spec.assertBool s (any (S.isCastOf bobsBolt) (askedOf S.bob board)) "the mana value 1 spell, off the same lands, is not"
      Spec.assertBool s (any (S.isCastOf alicesPiker) (askedOf S.alice board)) "and the Winnower's own controller may cast that same card"
      Spec.assertBool s (any (S.isCastOf barePiker) (askedOf S.bob bare)) "the pair: with the Winnower gone bob's Piker is castable"

    -- CR 601.3a's LOOKAHEAD, and the pair is the whole case: both spells have a
    -- mana value of 2 in bob's hand (CR 202.3e), both are refused by a reading
    -- that stops at the board, and the {X} one is offered anyway because a choice
    -- bob has not yet made could take it out of the prohibited class.
    Spec.it s "CR 601.3a an {X} spell with an even mana value in hand may still be begun" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      disaster <- S.printingOf s registry "Molten Disaster"
      winnower <- S.printingOf s registry "Void Winnower"
      let (_, bobsPiker, _, bobsDisaster, board) = voidWinnowerBoard mountain piker bolt disaster [winnower]
      Spec.assertBool s (PlayerEffect.matchesObjectFrom Nothing Filter.Type.ManaValueIsEven bobsPiker board) "the fixed spell's mana value is even"
      Spec.assertBool s (PlayerEffect.matchesObjectFrom Nothing Filter.Type.ManaValueIsEven bobsDisaster board) "and so is the {X} spell's, while it sits in hand"
      Spec.assertBool s (not (any (S.isCastOf bobsPiker) (askedOf S.bob board))) "the fixed one is refused"
      Spec.assertBool s (any (S.isCastOf bobsDisaster) (askedOf S.bob board)) "and the {X} one is offered"

    -- The search's REACH, which no card in the pool pins: Void Winnower's own
    -- criterion is answered at the second sample, so a lookahead that only ever
    -- looked one step would pass every case above. A threshold criterion is the
    -- shape that needs the climb -- an {X}{R}{R} card escapes "mana value 5 or
    -- less" only at X = 4 -- and Pawl.Engine.Filter.manaValueThresholds is what
    -- tells the search how far to walk. Asked of the same real card in the same
    -- hand; only the criterion is written by the test.
    Spec.it s "CR 601.3a the search walks past every literal the criterion names" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      disaster <- S.printingOf s registry "Molten Disaster"
      let (_, bobsPiker, _, bobsDisaster, board) = voidWinnowerBoard mountain piker bolt disaster []
          cheap = Filter.Type.ManaValueAtMost 5
      Spec.assertBool s (PlayerEffect.matchesObjectFrom Nothing cheap bobsDisaster board) "the {X} spell is inside the class as it sits in hand"
      Spec.assertBool s (PlayerEffect.choiceCouldEscape S.bob Nothing cheap bobsDisaster VariableChoice.Announced board) "and a large enough X takes it out"
      Spec.assertBool s (not (PlayerEffect.choiceCouldEscape S.bob Nothing cheap bobsPiker VariableChoice.Announced board)) "while the fixed spell beside it has no choice to make"

    -- CR 601.2e with CR 202.3e's second half: CR 601.3a let bob BEGIN, and the X
    -- he announces is then judged. X = 2 leaves {X}{R}{R} at mana value 4, even,
    -- so the cast is taken back; X = 3 makes it 5 on the stack. The Winnower gone
    -- is the pair for the X = 2 announcement.
    Spec.it s "CR 601.2e an announced X that leaves the spell even takes the cast back" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      bolt <- S.printingOf s registry "Lightning Bolt"
      disaster <- S.printingOf s registry "Molten Disaster"
      winnower <- S.printingOf s registry "Void Winnower"
      let (_, _, _, bobsDisaster, board) = voidWinnowerBoard mountain piker bolt disaster [winnower]
          (_, _, _, bareDisaster, bare) = voidWinnowerBoard mountain piker bolt disaster []
          announcing :: Natural -> Prompt.Prompt r -> r
          announcing x p = case p of
            Prompt.ChooseX {} -> x
            _ -> S.identityAnswer p
          castAt x gs oid = S.runPure (announcing x) (gs {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}) (S.cast S.bob oid)
          odd' = castAt 3 board bobsDisaster
      Spec.assertEqWith s "X = 3: the spell on the stack has mana value 5" (fmap (\sid -> Filter.manaValue (Projection.viewOfObject sid odd')) (GameState.stack odd')) [Just 5]
      Spec.assertEqWith s "X = 2: mana value 4 is even, so the cast is taken back" (GameState.stack (castAt 2 board bobsDisaster)) []
      Spec.assertEqWith s "the pair: with the Winnower gone X = 2 is cast" (length (GameState.stack (castAt 2 bare bareDisaster))) 1

-- CR 601.2f / 602.2b: the MANA half of a cost increase, at the activation
-- moment. Oppressive Rays -- "{W} Enchantment -- Aura. Enchant creature.
-- Enchanted creature can't attack or block unless its controller pays {3}.
-- Activated abilities of enchanted creature cost {3} more to activate" (checked
-- against Scryfall) -- is the pool's producer, and its third line is what these
-- cases are about; the other two are Pawl.CombatEffectSpec's.
--
-- CR 303.4b is half the point of the group. The criterion is
-- Filter.IsHostOfSource and nothing else, so the tax reaches exactly the object
-- the Aura is attached to -- which the engine can only answer because
-- Pawl.Engine.PlayerEffect.matchesObjectFrom is handed the row's own source. It
-- was handed Nothing until then (see #1242), and every case below would have
-- passed the WRONG WAY: the atom would have been vacuously False and the taxed
-- Brothers would have activated for its printed cost.
--
-- TWO Brothers of Fire and not one, which is what separates the three readings a
-- single-creature board cannot tell apart -- the tax reached this object, the tax
-- reached everything, the tax reached nothing. They are the same card, so the
-- Aura is the only difference between them.
--
-- Brothers of Fire is "{1}{R}{R}, {T}: Brothers of Fire deals 1 damage to any
-- target. Brothers of Fire deals 1 damage to you", so the printed activation is
-- three mana and the taxed one is six. THREE Mountains is the discriminating
-- board: it is exactly the printed cost and one short of half the taxed one.
oppressiveRaysBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
oppressiveRaysBoard rays brothers mountain n =
  let (taxed, g1) = S.addPermanent brothers S.alice (S.landsInPlay mountain n)
      (untaxed, g2) = S.addPermanent brothers S.alice g1
      (aura, g3) = S.addPermanent rays S.alice g2
   in (taxed, untaxed, (S.attach aura taxed g3) {GameState.priority = Just S.alice})

oppressiveRaysSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
oppressiveRaysSpec s registry =
  Spec.describe s "OppressiveRays" $ do
    -- CR 118.3's offer gate, reached at an activation by CR 602.2b, asked of both
    -- creatures on ONE board: three Mountains pay the printed {1}{R}{R} and
    -- cannot pay the taxed {4}{R}{R}, so the two answers are the whole of the
    -- criterion.
    Spec.it s "CR 601.2f the enchanted creature's activation is off the menu at its printed cost" $ do
      rays <- S.printingOf s registry "Oppressive Rays"
      brothers <- S.printingOf s registry "Brothers of Fire"
      mountain <- S.printingOf s registry "Mountain"
      let (taxed, untaxed, gs) = oppressiveRaysBoard rays brothers mountain 3
          offers = Action.legalActions S.alice gs
      Spec.assertEqWith s "CR 303.4b three Mountains cannot pay the enchanted creature's {4}{R}{R}" (length (activationsOf taxed offers)) 0
      Spec.assertEqWith s "CR 303.4b and the identical creature beside it, unenchanted, activates off the same three" (length (activationsOf untaxed offers)) 1

    -- The other side of the same pair: SIX Mountains pay the taxed cost, so the
    -- refusal above is the {3} and not a "never".
    Spec.it s "CR 601.2f six Mountains do pay it, so the tax is {3} and not a prohibition" $ do
      rays <- S.printingOf s registry "Oppressive Rays"
      brothers <- S.printingOf s registry "Brothers of Fire"
      mountain <- S.printingOf s registry "Mountain"
      let (taxed, untaxed, gs) = oppressiveRaysBoard rays brothers mountain 6
          offers = Action.legalActions S.alice gs
      Spec.assertEqWith s "the enchanted creature is offered once the mana is there" (length (activationsOf taxed offers)) 1
      Spec.assertEqWith s "and the unenchanted one still is" (length (activationsOf untaxed offers)) 1

    -- The PAYMENT, which the offer cases cannot reach: CR 601.2f's total is what
    -- Pawl.Engine.Cost charges, and a gate that read the taxed total while the
    -- payment read the printed one would pass both cases above. Six Mountains on
    -- both runs, so the tapped count is the only difference.
    Spec.it s "CR 601.2h the payment charges the taxed total, not the printed one" $ do
      rays <- S.printingOf s registry "Oppressive Rays"
      brothers <- S.printingOf s registry "Brothers of Fire"
      mountain <- S.printingOf s registry "Mountain"
      let (taxed, untaxed, gs) = oppressiveRaysBoard rays brothers mountain 6
          activate oid = case Activatable.abilitiesFor oid gs of
            [ability] -> Just (S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability))
            _ -> Nothing
      case (activate taxed, activate untaxed) of
        (Just afterTaxed, Just afterUntaxed) -> do
          Spec.assertEqWith s "CR 601.2f the enchanted creature's {1}{R}{R} came to six mana" (S.tappedCount S.alice afterTaxed) 6
          Spec.assertEqWith s "and the unenchanted one's, off the same six Mountains, came to three" (S.tappedCount S.alice afterUntaxed) 3
          Spec.assertEqWith s "both activations reached the stack" (length (GameState.stack afterTaxed) + length (GameState.stack afterUntaxed)) 2
        _ -> Spec.assertFailure s "expected exactly one activated ability on each Brothers of Fire"

-- The activations offered for ONE source, so a board carrying two activatable
-- permanents can say which of them was offered.
activationsOf :: ObjectId.ObjectId -> [Action.Type.Action] -> [Action.Type.Action]
activationsOf oid =
  let isIt a = case a of
        Action.Type.Activate o _ -> o == oid
        _ -> False
   in filter isIt

-- alice has one untapped Plains and `warning`, `secondCard` in hand, in her
-- own precombat main phase with an empty stack -- plus a second Plains ON TOP
-- OF HER LIBRARY, CR 104.3c's own trap: Scout's Warning's second clause is
-- "draw a card", and a fixture that never stocks the library decks her before
-- any assertion below runs.
scoutsWarningBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
scoutsWarningBoard plains warning secondCard =
  let base = S.landsInPlay plains 1
      (_, withLibrary) = S.addLibraryCard plains S.alice base
      (warningId, g1) = S.addHandCard warning S.alice withLibrary
      (secondId, g2) = S.addHandCard secondCard S.alice g1
   in ( warningId,
        secondId,
        g2
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- scoutsWarningBoard, with `warning` already cast and resolved through the
-- REAL priority loop -- CR 601.2a's move and Pawl.Engine.Expiry.arm's
-- Duration.UntilUsed -> Expiry.WhenUsed both run, so the stored grant this
-- proves is the card's own resolution and not a hand-built stand-in.
scoutsWarningResolved :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
scoutsWarningResolved plains warning secondCard =
  let (warningId, secondId, before) = scoutsWarningBoard plains warning secondCard
      resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
   in (secondId, resolveAll (S.runPure S.identityAnswer before (S.cast S.alice warningId)))

-- CR 116.2a's window forced shut without touching whose turn it is (CR
-- 305.3's own axis, left alone either way) -- Pawl.CastSpec's own busy-stack
-- trick: a nonempty stack fails Turn.sorcerySpeedWindow's empty-stack conjunct
-- and nothing else.
busyStack :: GameState.GameState -> GameState.GameState
busyStack gs = gs {GameState.stack = [ObjectId.MkObjectId 999]}

-- Scout's Warning {W} Instant: "The next creature card you play this turn can
-- be played as though it had flash. Draw a card." Dryad Arbor is a creature
-- LAND, so its play is what #1938 says pawl's cast-only permission could not
-- reach; Mountain beside it is a land the grant's HasCardType Creature
-- criterion refuses, and Goblin Piker is an ordinary creature SPELL, proving
-- CR 601.1a's other half -- casting is playing too.
scoutsWarningSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
scoutsWarningSpec s registry =
  Spec.describe s "ScoutsWarning" $ do
    -- The control: outside the sorcery-speed window with no grant in force,
    -- Dryad Arbor is not offered.
    Spec.it s "CR 116.2a Dryad Arbor is not offered with the stack busy and no grant" $ do
      plains <- S.printingOf s registry "Plains"
      warning <- S.printingOf s registry "Scout's Warning"
      dryadArbor <- S.printingOf s registry "Dryad Arbor"
      let (_, arborId, board) = scoutsWarningBoard plains warning dryadArbor
      Spec.assertBool s (not (any (playing arborId) (Action.legalActions S.alice (busyStack board)))) "not offered"

    -- The resolution itself: CR 611.2's stored effect the card installs, and
    -- CR 611.2a's Expiry.WhenUsed the Duration.UntilUsed on the card resolved
    -- into (#3008).
    Spec.it s "CR 611.2 resolving stores one MayPlayAsThoughItHadFlash grant expiring on use" $ do
      plains <- S.printingOf s registry "Plains"
      warning <- S.printingOf s registry "Scout's Warning"
      dryadArbor <- S.printingOf s registry "Dryad Arbor"
      let (_, after) = scoutsWarningResolved plains warning dryadArbor
      Spec.assertEqWith
        s
        "one stored grant, expiring on use"
        (fmap (\a -> (ActivePlayerEffect.effect a, ActivePlayerEffect.expiry a)) (GameState.playerEffects after))
        [(PlayerEffect.Type.MayPlayAsThoughItHadFlash (Filter.Type.HasCardType CardType.Creature), Expiry.Type.WhenUsed)]

    -- WotC's own Scout's Warning / Quicken ruling: "until the turn ends or
    -- until you cast [play] a creature card ... even if you [cast/play] it at
    -- a time you normally could" -- so playing Dryad Arbor through the REAL
    -- dispatch (Pawl.Engine.Engine's Action.Type.Play arm) consumes the grant,
    -- not just the sorcery-speed clock (#3008).
    Spec.it s "CR 611.2a playing Dryad Arbor consumes the grant" $ do
      plains <- S.printingOf s registry "Plains"
      warning <- S.printingOf s registry "Scout's Warning"
      dryadArbor <- S.printingOf s registry "Dryad Arbor"
      let (_, resolved) = scoutsWarningResolved plains warning dryadArbor
          played = S.runPure S.playLandAnswer resolved Engine.priorityLoop
      Spec.assertEqWith s "the grant is gone" (GameState.playerEffects played) []

    -- The Filter side of that consumption: Mountain is a land and nothing
    -- else, so it does not match the grant's HasCardType Creature criterion
    -- and playing it leaves the grant standing -- consumption reads the
    -- criterion rather than firing on any play at all.
    Spec.it s "CR 611.2a playing a non-creature land does not consume it" $ do
      plains <- S.printingOf s registry "Plains"
      warning <- S.printingOf s registry "Scout's Warning"
      mountain <- S.printingOf s registry "Mountain"
      let (_, resolved) = scoutsWarningResolved plains warning mountain
          played = S.runPure S.playLandAnswer resolved Engine.priorityLoop
      Spec.assertEqWith s "the grant survives" (length (GameState.playerEffects played)) 1

    -- CR 514.2 / 611.2a: the "or until the turn ends" half -- an UNUSED grant
    -- still ends at cleanup exactly as AtCleanup's does, so Scout's Warning
    -- never lingers into a later turn with nothing to spend it on.
    Spec.it s "CR 514.2 the cleanup sweep drops an unused WhenUsed grant" $ do
      plains <- S.printingOf s registry "Plains"
      warning <- S.printingOf s registry "Scout's Warning"
      dryadArbor <- S.printingOf s registry "Dryad Arbor"
      let (_, resolved) = scoutsWarningResolved plains warning dryadArbor
      Spec.assertEqWith s "one stored before" (length (GameState.playerEffects resolved)) 1
      Spec.assertEqWith s "none after cleanup" (GameState.playerEffects (Expiry.dropAtCleanup resolved)) []

    -- CR 601.2e / 733.1: a cast that is rejected -- here at CR 601.2h, the mana
    -- refused -- is returned to the moment before it was proposed, and the
    -- grant it would have spent is part of that moment. Driven through
    -- Pawl.Engine.Engine's own Cast arm, where a spend ahead of
    -- Cast.castSpellWith's rewind snapshot once lost the grant: the answerer
    -- proposes the Piker ONCE, refuses every mana source, then passes.
    Spec.it s "CR 601.2e a rejected cast leaves the grant standing" $ do
      plains <- S.printingOf s registry "Plains"
      mountain <- S.printingOf s registry "Mountain"
      warning <- S.printingOf s registry "Scout's Warning"
      piker <- S.printingOf s registry "Goblin Piker"
      let (pikerId, resolved) = scoutsWarningResolved plains warning piker
          -- Two untapped Mountains, so the Piker is OFFERED (Cast.castable
          -- prices it) and the refusal happens at the payment, not the gate.
          funded = S.landsFor mountain S.alice 2 resolved
          castOfPiker action = case action of
            Action.Type.Cast oid _ _ -> oid == pikerId
            _ -> False
          -- (asked, found): pass once the cast has been proposed, and record
          -- whether it ever was.
          answerer :: Prompt.Prompt r -> State.State (Bool, Bool) r
          answerer p = case p of
            Prompt.ChooseAction _ _ actions -> do
              (asked, found) <- State.get
              let offer = List.find castOfPiker actions
              State.put (True, found || Maybe.isJust offer)
              pure (if asked then Action.Type.Pass else Maybe.fromMaybe Action.Type.Pass offer)
            Prompt.ChooseManaSource {} -> pure Nothing
            _ -> pure (S.identityAnswer p)
          ((_, after), (_, proposed)) = State.runState (Engine.runGame answerer funded Engine.priorityLoop) (False, False)
      Spec.assertEqWith s "CR 601.2e the grant survives the rejected cast" (length (GameState.playerEffects after)) 1
      Spec.assertBool s proposed "the Piker really was proposed"
      Spec.assertEqWith s "nothing was tapped for it" (S.tappedCount S.alice after) 1
      Spec.assertEqWith s "and it is back in hand" (fmap Object.zone (Game.lookupObject pikerId after)) (Just Zone.Hand)

    -- CR 601.3's door into a cast, not Pawl.Engine.Engine's: Panglacial Wurm
    -- cast during a search goes through Cast.castWhileSearching, and the grant
    -- is spent there exactly as it is by an ordinary cast -- the spend lives in
    -- Cast.castSpellWith, the one funnel every door reaches.
    Spec.it s "CR 601.3 a cast made while searching spends the grant" $ do
      forest <- S.printingOf s registry "Forest"
      plains <- S.printingOf s registry "Plains"
      warning <- S.printingOf s registry "Scout's Warning"
      panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
      let base = S.landsFor plains S.alice 1 (S.landsInPlay forest 7)
          -- The Wurm first and the Plains on top of it: Scout's Warning's second
          -- clause draws the top card (CR 104.3c's trap in scoutsWarningBoard),
          -- and the Wurm has to still be in the library for the search to offer.
          (_, withWurm) = S.addLibraryCard panglacialWurm S.alice base
          (_, withDraw) = S.addLibraryCard plains S.alice withWurm
          (warningId, g1) = S.addHandCard warning S.alice withDraw
          before = g1 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
          resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer before (S.cast S.alice warningId)) Engine.priorityLoop
          castFirst :: Prompt.Prompt r -> r
          castFirst p = case p of
            Prompt.CastWhileSearching _ _ options -> Maybe.listToMaybe options
            _ -> S.identityAnswer p
          after = S.runPure castFirst resolved (Cast.castWhileSearching S.manaPerformer S.alice)
      Spec.assertEqWith s "CR 611.2a the grant is spent by the search's own cast" (GameState.playerEffects after) []
      Spec.assertEqWith s "the grant stood before it" (length (GameState.playerEffects resolved)) 1
      Spec.assertEqWith s "and the Wurm really was cast" (length (GameState.stack after)) 1

    -- CR 708.2a: a face-down cast is a 2/2 creature spell, which is the face
    -- the cast's gate read the grant against -- and the spend reads the same
    -- face, not the Aura printed underneath. Gift of Doom is an Aura with
    -- morph, so its printed face fails HasCardType Creature and only the
    -- proposed, face-down view spends the grant.
    Spec.it s "CR 708.2a a face-down cast spends the grant off the face the gate read" $ do
      plains <- S.printingOf s registry "Plains"
      warning <- S.printingOf s registry "Scout's Warning"
      giftOfDoom <- S.printingOf s registry "Gift of Doom"
      let (giftId, resolved) = scoutsWarningResolved plains warning giftOfDoom
          funded = S.landsFor plains S.alice 3 resolved
          after = S.runPure S.identityAnswer funded (Cast.castSpell S.manaPerformer S.alice giftId (CardName.MkCardName (Text.pack "Gift of Doom")) (Facing.faceDown FaceDownReason.Morphed))
      Spec.assertEqWith s "CR 708.2a the grant is spent by the face-down creature spell" (GameState.playerEffects after) []
      Spec.assertEqWith s "and the spell really is on the stack" (length (GameState.stack after)) 1

-- The first action any of `wanted` admits, in the order `wanted` lists them, or
-- a pass -- castAnyOf above widened to a land play.
takeFirst :: [Action.Type.Action -> Bool] -> Prompt.Prompt r -> r
takeFirst wanted p = case p of
  Prompt.ChooseAction _ _ actions -> case [action | admits <- wanted, action <- actions, admits action] of
    h : _ -> h
    [] -> Action.Type.Pass
  _ -> S.identityAnswer p

-- The objects that arrived on the battlefield between two boards.
arrivedBetween :: GameState.GameState -> GameState.GameState -> [ObjectId.ObjectId]
arrivedBetween before after = Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before))

-- CR 700.4: `oid` dies, and the ability that triggered goes on the stack and
-- resolves.
diesAndResolves :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
diesAndResolves oid gs =
  let killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [oid])
      placed = S.runPure S.identityAnswer killed Engine.settleForPriority
   in S.runPure S.identityAnswer placed Stack.resolveTop

-- The names of the cards in one of `pid`'s zones.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [String]
namesIn zone pid gs = Maybe.mapMaybe (\oid -> fmap (Text.unpack . CardName.unwrap . Face.name) (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs)

-- paragonBoard's objects, named.
data ParagonBoard = MkParagonBoard
  { pbGraveForest :: ObjectId.ObjectId,
    pbBuried :: ObjectId.ObjectId,
    pbHandForest :: ObjectId.ObjectId,
    pbHeld :: ObjectId.ObjectId,
    pbParagon :: Maybe ObjectId.ObjectId,
    pbState :: GameState.GameState
  }

-- alice's precombat main at two seats: three Forests, `paragon` on her
-- battlefield when given, a Forest and `buried` in her graveyard, and `held` and
-- a Forest in her hand. `remaining` is emptied for johannBoard's reason.
paragonBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Maybe Printing.Printing -> ParagonBoard
paragonBoard forest buried held paragon =
  let lands = S.landsFor forest S.bob 3 (S.landsFor forest S.alice 3 (Setup.emptyGame S.bothPlayers))
      (_, g1) = S.addLibraryCard forest S.alice lands
      (_, g2) = S.addLibraryCard forest S.bob g1
      (graveForest, g3) = S.addGraveyardCard forest S.alice g2
      (buriedId, g4) = S.addGraveyardCard buried S.alice g3
      (handForest, g5) = S.addHandCard forest S.alice g4
      (heldId, g6) = S.addHandCard held S.alice g5
      (paragonId, g7) = case paragon of
        Nothing -> (Nothing, g6)
        Just printing -> let (oid, g) = S.addPermanent printing S.alice g6 in (Just oid, g)
   in MkParagonBoard
        { pbGraveForest = graveForest,
          pbBuried = buriedId,
          pbHandForest = handForest,
          pbHeld = heldId,
          pbParagon = paragonId,
          pbState =
            g7
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice,
                GameState.remaining = Seq.empty
              }
        }

-- Serra Paragon {2}{W}{W} Creature -- Angel 3/4: "Flying / Once during each of
-- your turns, you may play a land from your graveyard or cast a permanent spell
-- with mana value 3 or less from your graveyard. If you do, it gains 'When this
-- permanent is put into a graveyard from the battlefield, exile it and you gain
-- 2 life.'"
--
-- The producer of PermissionVerb.Play and PermissionLimit.OnceEachOfYourTurns,
-- and of CR 611.3d's rider through Affected.PlayedThisWay. Llanowar Elves is the
-- permanent spell and a Forest the land, both under the mana value.
serraParagonSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
serraParagonSpec s registry =
  let board buried held withParagon = do
        forest <- S.printingOf s registry "Forest"
        buriedCard <- S.printingOf s registry buried
        heldCard <- S.printingOf s registry held
        paragon <- S.printingOf s registry "Serra Paragon"
        pure (paragonBoard forest buriedCard heldCard (if withParagon then Just paragon else Nothing))
      playOf oid = (== Action.Type.Play oid Nothing)
   in Spec.describe s "SerraParagon" $ do
        -- CR 614.1a: Heart of Yavimaya played from the graveyard with no Forest
        -- to sacrifice goes back to the graveyard instead of entering -- but it
        -- WAS played (CR 305.1), so the one use is spent and the buried
        -- Ornithopter is not cast. Swamps, so nothing can pay the Heart; the
        -- case below is the pair.
        Spec.it s "CR 305.1 / 614.1a a graveyard land turned away by its own sacrifice still spends the use" $ do
          swamp <- S.printingOf s registry "Swamp"
          ornithopter <- S.printingOf s registry "Ornithopter"
          paragon <- S.printingOf s registry "Serra Paragon"
          heart <- S.printingOf s registry "Heart of Yavimaya"
          let b = paragonBoard swamp ornithopter swamp (Just paragon)
              (heartId, gs) = S.addGraveyardCard heart S.alice (pbState b)
              after = S.runPure (takeFirst [playOf heartId, S.isCastOf (pbBuried b)]) gs Engine.priorityLoop
          Spec.assertBool s (elem (pbBuried b) (Game.zoneMembers Zone.Graveyard S.alice after)) "the Ornithopter is still in alice's graveyard"
          Spec.assertEqWith s "and Heart of Yavimaya went back there rather than entering" (namesIn Zone.Graveyard S.alice after List.\\ namesIn Zone.Graveyard S.alice gs) []
          Spec.assertEqWith s "and nothing arrived" (arrivedBetween gs after) []

        -- Without the Paragon nothing in the graveyard is playable.
        Spec.it s "CR 305.1 without Serra Paragon the graveyard Forest is not playable" $ do
          b <- board "Llanowar Elves" "Forest" False
          Spec.assertBool s (notElem (pbGraveForest b, Nothing) (Action.playableLands S.alice (pbState b))) "the graveyard Forest is not offered"

        -- "Once during each of YOUR turns": on bob's turn, with alice holding
        -- priority, Pouncing Cheetah's flash lets the one in her hand be cast,
        -- and the one in her graveyard is not offered.
        Spec.it s "CR 601.3 the permission does not apply on another player's turn" $ do
          b <- board "Pouncing Cheetah" "Pouncing Cheetah" True
          let gs = pbState b
              bobs = gs {GameState.activePlayer = S.bob}
          Spec.assertBool s (not (any (S.isCastOf (pbBuried b)) (Action.legalActions S.alice bobs))) "the graveyard Cheetah is not offered on bob's turn"
          Spec.assertBool s (any (S.isCastOf (pbHeld b)) (Action.legalActions S.alice bobs)) "though flash offers the one in her hand"
          Spec.assertBool s (any (S.isCastOf (pbBuried b)) (Action.legalActions S.alice gs)) "and the graveyard one is offered on her own turn"

        -- The rider (CR 400.7b / 611.3d). The Elves cast from the graveyard
        -- resolve into a permanent that keeps the granted trigger after the
        -- Paragon itself is gone -- no duration is stated, so it lasts the game
        -- -- and dying exiles them and gains alice 2 life.
        Spec.it s "CR 400.7b / 611.3d the permanent a graveyard cast became keeps the rider past the Paragon" $ do
          b <- board "Llanowar Elves" "Forest" True
          let gs = pbState b
              cast = S.runPure (takeFirst [S.isCastOf (pbBuried b)]) gs Engine.priorityLoop
          case (arrivedBetween gs cast, pbParagon b) of
            ([permanent], Just paragon) -> do
              let after = diesAndResolves permanent (diesAndResolves paragon cast)
              Spec.assertEqWith s "the Elves' card was exiled" (namesIn Zone.Exile S.alice after) ["Llanowar Elves"]
              Spec.assertEqWith s "and alice gained 2 life" (S.lifeOf S.alice after) (fmap (+ 2) (S.lifeOf S.alice gs))
            (arrived, _) -> Spec.assertFailure s ("expected one arrival and a Paragon, got " <> show arrived)

        -- The linkage: the same Elves cast from her HAND, with the Paragon on
        -- the battlefield, were not cast "this way" and gain nothing.
        Spec.it s "CR 611.3d a spell cast from the hand is not given the rider" $ do
          b <- board "Forest" "Llanowar Elves" True
          let gs = pbState b
              cast = S.runPure (takeFirst [S.isCastOf (pbHeld b)]) gs Engine.priorityLoop
          case arrivedBetween gs cast of
            [permanent] -> do
              let after = diesAndResolves permanent cast
              Spec.assertEqWith s "nothing was exiled" (namesIn Zone.Exile S.alice after) []
              Spec.assertEqWith s "and alice's life is unchanged" (S.lifeOf S.alice after) (S.lifeOf S.alice gs)
            arrived -> Spec.assertFailure s ("expected one arrival, got " <> show arrived)

        -- CR 400.7i: the land played this way gains the rider too.
        Spec.it s "CR 400.7i / 611.3d the land played from the graveyard keeps the rider" $ do
          b <- board "Llanowar Elves" "Forest" True
          let gs = pbState b
              played = S.runPure (takeFirst [playOf (pbGraveForest b)]) gs Engine.priorityLoop
          case arrivedBetween gs played of
            [permanent] -> do
              let after = diesAndResolves permanent played
              Spec.assertEqWith s "the Forest was exiled" (namesIn Zone.Exile S.alice after) ["Forest"]
              Spec.assertEqWith s "and alice gained 2 life" (S.lifeOf S.alice after) (fmap (+ 2) (S.lifeOf S.alice gs))
            arrived -> Spec.assertFailure s ("expected one arrival, got " <> show arrived)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.PlayerEffect" $ do
  extraLandDropsSpec s registry
  vedalkenOrrerySpec s registry
  sigardasAidSpec s registry
  untimelyAlurenSpec s registry
  searchIsUntimedSpec s registry
  yawgmothsWillSpec s registry
  crucibleSpec s registry
  garruksHordeSpec s registry
  futureSightSpec s registry
  johannSpec s registry
  serraParagonSpec s registry
  voidWinnowerSpec s registry
  spiderPunkSpec s registry
  jaredSpec s registry
  oppressiveRaysSpec s registry
  scoutsWarningSpec s registry
  dawnhandSpec s registry
  uriangerSpec s registry

-- Dawnhand Dissident {B} Creature -- Elf Warlock 1/2 (Oracle text checked against
-- Scryfall 2026-09-29): "During your turn, you may cast creature spells from among
-- cards you own exiled with this creature by removing three counters from among
-- creatures you control in addition to paying their other costs."
--
-- The producer for PermissionPool.CardsExiledWithSource (CR 607.2a) and for a
-- CastFromZone's additionalCosts (CR 118.8 / 601.2f); "during your turn" is the
-- PlayerStaticAbility condition Paladin Class writes.
--
-- THE BOARD: alice controls the Dissident, three Forests, a Goblin Piker carrying
-- a vigilance and a +1/+1 counter and a Hill Giant carrying the given count of
-- +1/+1 counters. Exiled: her Pouncing Cheetah (flash) linked to the Dissident,
-- her second Cheetah linked to nothing, and bob's Cheetah linked to the
-- Dissident. Her hand holds a third Cheetah.
dawnhandSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dawnhandSpec s registry =
  Spec.describe s "DawnhandDissident" $ do
    Spec.it s "CR 601.3 / 118.8 the linked creature is cast by removing three counters" $ do
      (linked, _, _, _, pikerId, giantId, gs) <- dawnhandBoard s registry 2 S.alice
      cheetah <- S.printingOf s registry "Pouncing Cheetah"
      let answer = Map.fromList [(pikerId, Map.singleton dawnhandVigilance 1), (giantId, Map.singleton CounterKind.PlusOnePlusOne 2)]
          resolved = S.runPure (castLinkedAnswering linked answer) gs Engine.priorityLoop
      Spec.assertEqWith s "the linked Cheetah resolved onto alice's battlefield" (S.countOnBattlefieldByName (S.printingName cheetah) S.alice resolved) 1
      Spec.assertEqWith s "CR 601.2h the Piker's vigilance counter came off" (S.counterOf dawnhandVigilance pikerId resolved) 0
      Spec.assertEqWith s "and both of the Giant's" (S.counterOf CounterKind.PlusOnePlusOne giantId resolved) 0
      Spec.assertEqWith s "and the Piker kept its +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne pikerId resolved) 1
    Spec.it s "CR 607.2a only her own cards exiled with it are reached" $ do
      (linked, unlinked, bobs, _, _, _, gs) <- dawnhandBoard s registry 2 S.alice
      let offered oid = any (S.isCastOf oid) (Action.legalActions S.alice gs)
      Spec.assertBool s (not (offered unlinked)) "her Cheetah exiled by nothing is not offered"
      Spec.assertBool s (not (offered bobs)) "nor bob's Cheetah exiled with it"
      Spec.assertBool s (offered linked) "while her Cheetah exiled with it is"
    -- The pair: three counters among her creatures and two.
    Spec.it s "CR 118.3 two counters among her creatures do not pay for three" $ do
      (short, _, _, _, _, _, shortBoard) <- dawnhandBoard s registry 0 S.alice
      Spec.assertBool s (not (any (S.isCastOf short) (Action.legalActions S.alice shortBoard))) "the linked Cheetah is not offered with two counters"
      (enough, _, _, _, _, _, gs) <- dawnhandBoard s registry 1 S.alice
      Spec.assertBool s (any (S.isCastOf enough) (Action.legalActions S.alice gs)) "and is with three"
    -- The Cheetah's flash makes the turn observable: the same card in her hand is
    -- offered on bob's turn.
    Spec.it s "CR 611.3a on bob's turn the permission does not apply" $ do
      (linked, _, _, inHand, _, _, gs) <- dawnhandBoard s registry 2 S.bob
      Spec.assertBool s (not (any (S.isCastOf linked) (Action.legalActions S.alice gs))) "the linked Cheetah is not offered on bob's turn"
      Spec.assertBool s (any (S.isCastOf inHand) (Action.legalActions S.alice gs)) "though the Cheetah in her hand is"

-- CR 122.1b's vigilance counter.
dawnhandVigilance :: CounterKind.CounterKind Keyword.Keyword
dawnhandVigilance = CounterKind.Keyword Keyword.Vigilance

-- The board dawnhandSpec's cases share, described above it, given the Giant's
-- +1/+1 count and whose turn it is (alice holding priority on it with an empty
-- stack). Answers the linked Cheetah, the unlinked one, bob's linked one, the
-- one in her hand, the Piker and the Giant.
dawnhandBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Natural -> PlayerId.PlayerId -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
dawnhandBoard s registry giantPlusOne active = do
  dissident <- S.printingOf s registry "Dawnhand Dissident"
  cheetah <- S.printingOf s registry "Pouncing Cheetah"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  forest <- S.printingOf s registry "Forest"
  let lands = S.landsFor forest S.alice 3 (Setup.emptyGame S.bothPlayers)
      (dissidentId, g1) = S.addPermanent dissident S.alice lands
      (pikerId, g2) = S.addPermanent piker S.alice g1
      (giantId, g3) = S.addPermanent giant S.alice g2
      (linked, g4) = S.addExiledCard cheetah S.alice g3
      (unlinked, g5) = S.addExiledCard cheetah S.alice g4
      (bobs, g6) = S.addExiledCard cheetah S.bob g5
      (inHand, g7) = S.addHandCard cheetah S.alice g6
      link = ExileLink.MkExileLink {ExileLink.source = dissidentId, ExileLink.ability = Nothing}
      counted =
        S.addCounter CounterKind.PlusOnePlusOne giantPlusOne giantId
          . S.addCounter CounterKind.PlusOnePlusOne 1 pikerId
          . S.addCounter dawnhandVigilance 1 pikerId
          $ g7
  pure
    ( linked,
      unlinked,
      bobs,
      inHand,
      pikerId,
      giantId,
      counted
        { GameState.exiledWith = Map.fromList [(linked, link), (bobs, link)],
          GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = active,
          GameState.priority = Just S.alice
        }
    )

-- castOnly, answering CR 601.2h's division of the counters with the one given.
-- PINNED, so an answer the engine did not ask for cannot be repaired.
castLinkedAnswering :: ObjectId.ObjectId -> Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural) -> Prompt.Prompt r -> r
castLinkedAnswering wanted answer p = case p of
  Prompt.ChooseMixedCounterRemoval {} -> answer
  _ -> castOnly wanted p

-- Urianger Augurelt {W}{U} Legendary Creature -- Elf Advisor 1/3 (Oracle text
-- checked against Scryfall 2026-10-07): "Draw Arcanum -- {T}: Look at the top card
-- of your library. You may exile it face down. / Play Arcanum -- {T}: Until end of
-- turn, you may play cards exiled with Urianger Augurelt."
--
-- The producer for CR 601.3f: the look Draw Arcanum gives stays with the player
-- who looked (CR 406.3), while Play Arcanum's permission goes to whoever controls
-- Urianger when it is activated. Act of Treason {2}{R} -- "Gain control of target
-- creature until end of turn. Untap that creature. It gains haste until end of
-- turn." -- is both the steal and the untap, so the PAIR differs only in who
-- casts it.
--
-- THE BOARD: alice's Urianger, an Ornithopter on top of her library, and her
-- Memnite exiled FACE UP and linked to Urianger, which is the control: it shows
-- the permission reached the caster, so a refusal of the hidden card is the look.
uriangerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
uriangerSpec s registry =
  Spec.describe s "UriangerAugurelt" $ do
    Spec.it s "CR 601.3f the player who looked may cast the face-down card she exiled" $ do
      (hidden, shown, urianger, gs) <- uriangerBoard s registry S.alice
      let offered oid = any (S.isCastOf oid) (Action.legalActions S.alice gs)
      Spec.assertBool s (offered hidden) "alice is offered the Ornithopter she exiled face down"
      Spec.assertBool s (offered shown) "and the face-up Memnite"
      Spec.assertEqWith s "she controls Urianger" (View.controllerOf urianger gs) (Just S.alice)
    Spec.it s "CR 601.3f a new controller may not begin to cast a face-down card they cannot look at" $ do
      (hidden, shown, urianger, gs) <- uriangerBoard s registry S.bob
      let offered oid = any (S.isCastOf oid) (Action.legalActions S.bob gs)
      Spec.assertBool s (not (offered hidden)) "bob is not offered the Ornithopter alice exiled face down"
      -- Proxies, AFTER the behaviour: the permission is bob's, and control moved.
      Spec.assertBool s (offered shown) "though the face-up Memnite exiled with Urianger is"
      Spec.assertEqWith s "CR 613.1b Act of Treason gave bob Urianger" (View.controllerOf urianger gs) (Just S.bob)

-- The board uriangerSpec's cases share, described above it: alice activates Draw
-- Arcanum on her own turn and exiles the Ornithopter, then `caster` casts Act of
-- Treason on Urianger on their own turn and activates Play Arcanum. Answers the
-- face-down Ornithopter, the face-up Memnite, Urianger, and the board with
-- `caster` holding priority on an empty stack.
uriangerBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> PlayerId.PlayerId -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
uriangerBoard s registry caster = do
  urianger <- S.printingOf s registry "Urianger Augurelt"
  ornithopter <- S.printingOf s registry "Ornithopter"
  memnite <- S.printingOf s registry "Memnite"
  treason <- S.printingOf s registry "Act of Treason"
  mountain <- S.printingOf s registry "Mountain"
  let lands = S.landsFor mountain caster 3 (Setup.emptyGame S.bothPlayers)
      (uriangerId, g1) = S.addPermanent urianger S.alice lands
      (_, g2) = S.addLibraryCard ornithopter S.alice g1
      (shown, g3) = S.addExiledCard memnite S.alice g2
      (treasonId, g4) = S.addHandCard treason caster g3
      link = ExileLink.MkExileLink {ExileLink.source = uriangerId, ExileLink.ability = Nothing}
      ready =
        g4
          { GameState.exiledWith = Map.singleton shown link,
            GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
  case Face.activatedAbilities (S.combinedFace urianger) of
    [draw, play] -> do
      let drawn = S.runPure exilingAnswer ready (Activate.activateAbility S.alice uriangerId draw >> Stack.resolveTop)
          hidden = filter (\oid -> maybe False Object.exiledFaceDown (Game.lookupObject oid drawn)) (Set.toList (GameState.exile drawn))
          casterTurn = drawn {GameState.activePlayer = caster, GameState.priority = Just caster}
          stolen = S.runPure exilingAnswer casterTurn (S.cast caster treasonId >> Stack.resolveTop)
          permitted = S.runPure exilingAnswer stolen (Activate.activateAbility caster uriangerId play >> Stack.resolveTop)
      case hidden of
        [card] -> pure (card, shown, uriangerId, permitted)
        _ -> Spec.assertFailure s "Draw Arcanum should exile exactly one card face down"
    _ -> Spec.assertFailure s "Urianger Augurelt should print two activated abilities"

-- Takes Draw Arcanum's "you may exile it face down"; everything else as
-- S.identityAnswer.
exilingAnswer :: Prompt.Prompt r -> r
exilingAnswer p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p
