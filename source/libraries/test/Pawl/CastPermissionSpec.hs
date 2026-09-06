{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.PlayerEffect over effects that permit playing (CR 305.2, CR
-- 601.3): extra land drops, flash and graveyard casting from Vedalken Orrery to
-- Scout's Warning, and Void Winnower's and Oppressive Rays' restrictions. Split
-- out of Pawl.PlayerEffectSpec, which keeps the machinery.
module Pawl.CastPermissionSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator Pawl.Engine.Filter already claims the alias Filter.

-- Aliased Card.Type, per the project-wide convention (CardSpec): the logic
-- module Pawl.Engine.Card may later be imported and must not collide.

import Pawl.CastProhibitionSpec (equipBoard, flashBoard, flashOnOwnTurn, isActivateOf, isPlay, landDropBoard, nextTurnFor, orreryScopeBoard, playEveryLand)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.ManaCost as ManaCost
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
    -- The control. CR 305.2's "normally one", played through the same loop, so
    -- every raised number below is measured against a baseline this group
    -- established rather than an assumed one.
    Spec.it s "CR 305.2 with no effect a player plays one land and no more" $ do
      mountain <- S.printingOf s registry "Mountain"
      let board = landDropBoard mountain [] S.alice
          after = playEveryLand board
      Spec.assertEqWith s "the allowance is one" (PlayerEffect.landPlaysAllowed S.alice board) 1
      Spec.assertEqWith s "one Mountain landed" (S.countOnBattlefieldByName (S.printingName mountain) S.alice after) 1
      Spec.assertEqWith s "and FOUR are still in hand -- the gate refused, an empty hand did not" (S.handSize S.alice after) 4
      Spec.assertEqWith s "no further land play is offered" (filter isPlay (Action.legalActions S.alice after)) []

    -- Exploration's one extra. A gate that ignored the effect answers one here.
    Spec.it s "CR 305.2 Exploration raises the allowance to two" $ do
      let first = S.aliased "first land" (S.cardSetup "Mountain")
          second = S.aliased "second land" (S.cardSetup "Mountain")
          alice =
            (S.battlefield S.alice [S.permanent "Exploration"])
              { S.setupHand = Seq.fromList [first, second, S.cardSetup "Mountain", S.cardSetup "Mountain", S.cardSetup "Mountain"]
              }
          board = S.board (alice NonEmpty.:| [S.playerSetup S.bob]) S.alice S.precombatMain
          script =
            S.turn
              1
              [ S.on S.precombatMain S.alice (S.playLand (S.aliasRef "first land")),
                S.on S.precombatMain S.alice (S.playLand (S.aliasRef "second land"))
              ]
      after <- S.play s registry board script (S.priorityGame S.alice)
      Spec.assertEqWith s "the allowance is two" (PlayerEffect.landPlaysAllowed S.alice after) 2
      Spec.assertEqWith s "two Mountains landed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Mountain")) S.alice after) 2
      Spec.assertEqWith s "three are still in hand" (S.handSize S.alice after) 3
      Spec.assertEqWith s "and the third is refused" (filter isPlay (Action.legalActions S.alice after)) []

    -- THE DISCRIMINATOR between a count and a boolean. A gate written as "one,
    -- or two if any effect grants extra lands" passes the Exploration case above
    -- and stops at two here; only a gate that reads Azusa's NUMBER reaches
    -- three.
    Spec.it s "CR 305.2 Azusa's two additional lands make three, not two" $ do
      mountain <- S.printingOf s registry "Mountain"
      azusa <- S.printingOf s registry "Azusa, Lost but Seeking"
      let board = landDropBoard mountain [azusa] S.alice
          after = playEveryLand board
      Spec.assertEqWith s "the allowance is three" (PlayerEffect.landPlaysAllowed S.alice board) 3
      Spec.assertEqWith s "three Mountains landed" (S.countOnBattlefieldByName (S.printingName mountain) S.alice after) 3
      Spec.assertEqWith s "two are still in hand" (S.handSize S.alice after) 2
      Spec.assertEqWith s "and the fourth is refused" (filter isPlay (Action.legalActions S.alice after)) []

    -- CR 305.2 says continuous effects INCREASE the number, so two of them
    -- compose: one plus one plus two. Nothing here is redundant -- CR 702.18b
    -- and CR 702.11h make multiple instances redundant for a KEYWORD, and CR
    -- 305.2 states no such rule. A maximum rather than a sum answers three.
    Spec.it s "CR 305.2 Exploration and Azusa together add up to four" $ do
      mountain <- S.printingOf s registry "Mountain"
      exploration <- S.printingOf s registry "Exploration"
      azusa <- S.printingOf s registry "Azusa, Lost but Seeking"
      let board = landDropBoard mountain [exploration, azusa] S.alice
          after = playEveryLand board
      Spec.assertEqWith s "the allowance is four" (PlayerEffect.landPlaysAllowed S.alice board) 4
      Spec.assertEqWith s "four Mountains landed" (S.countOnBattlefieldByName (S.printingName mountain) S.alice after) 4
      Spec.assertEqWith s "one is still in hand" (S.handSize S.alice after) 1
      Spec.assertEqWith s "and the fifth is refused" (filter isPlay (Action.legalActions S.alice after)) []

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

    -- CR 305.2's allowance is PER TURN, and the raised one refills like the
    -- normal one: CR 703.4c's untap step clears the tally, and the next turn
    -- gets two again rather than nothing or a running total.
    Spec.it s "CR 305.2 the raised allowance refills each turn" $ do
      mountain <- S.printingOf s registry "Mountain"
      exploration <- S.printingOf s registry "Exploration"
      let firstTurn = playEveryLand (landDropBoard mountain [exploration] S.alice)
          untapped = nextTurnFor S.alice firstTurn
          secondTurn = playEveryLand untapped
      Spec.assertEqWith s "two played on the first turn" (GameState.landsPlayed firstTurn) (Map.singleton S.alice 2)
      Spec.assertEqWith s "the untap step clears the tally" (GameState.landsPlayed untapped) Map.empty
      Spec.assertEqWith s "two more on the second turn" (GameState.landsPlayed secondTurn) (Map.singleton S.alice 2)
      Spec.assertEqWith s "four Mountains in play" (S.countOnBattlefieldByName (S.printingName mountain) S.alice secondTurn) 4
      Spec.assertEqWith s "and the fifth is still in hand, refused" (S.handSize S.alice secondTurn) 1

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

    -- The gameplay half, driven through the priority loop rather than by calling
    -- Cast.castSpell: the script selects its named cast only when it is OFFERED,
    -- so the two runs differ in the Orrery and in nothing a test wrote by hand.
    -- Without it alice is offered no cast at all and simply passes.
    Spec.it s "CR 601.3b the offered cast resolves and the creature enters on the opponent's turn" $ do
      let mana1 = S.aliased "first mana" (S.permanent "Mountain")
          mana2 = S.aliased "second mana" (S.permanent "Mountain")
          spell = S.aliased "spell" (S.cardSetup "Goblin Piker")
          alice extras =
            (S.battlefield S.alice ([mana1, mana2] <> replicate 7 (S.permanent "Mountain") <> extras))
              { S.setupHand = Seq.fromList [spell, S.cardSetup "Mountain"]
              }
          setup extras = S.board (alice extras NonEmpty.:| [S.playerSetup S.bob]) S.bob S.precombatMain
          choices =
            S.noChoices
              { S.choiceManaSources =
                  Seq.fromList
                    [ Just (S.aliasRef "first mana"),
                      Just (S.aliasRef "second mana"),
                      Nothing
                    ]
              }
          script = S.turn 1 [S.on S.precombatMain S.alice (S.castAction (S.aliasRef "spell") choices)]
      after <- S.play s registry (setup [S.permanent "Vedalken Orrery"]) script (S.priorityGame S.alice)
      bare <- S.play s registry (setup []) Seq.empty (S.priorityGame S.alice)
      Spec.assertEqWith s "bob is still the active player" (GameState.activePlayer after) S.bob
      Spec.assertEqWith s "the Piker is on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 1
      Spec.assertEqWith s "and without the Orrery it never left her hand" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice bare) 0
      Spec.assertEqWith s "which is where it still is" (S.handSize S.alice bare) 2

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

-- ONE board for both halves of CR 701.6a's "a spell or ability": alice has a
-- SPELL of the caller's choosing on the stack and a settled Prodigal Sorcerer
-- whose {T} ABILITY can join it, and bob holds a Cancel for the first and a
-- Stifle for the second. `permanents` is the only difference between a run that
-- counters and a run that does not.
--
-- Shared by the Spider-Punk and Prowling Serpopard groups below, which is why
-- both the protecting permanents and the victim spell are parameters: the
-- unfiltered arm and the filtered one differ only in which victim survives.
--
-- bob's three Islands pay for whichever of the two he casts; the runs branch
-- from this state and never share mana.
counteringBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  [(PlayerId.PlayerId, Printing.Printing)] ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
counteringBoard island cancel stifle sorcerer victim permanents =
  let withLands = List.foldl' (\g _ -> snd (S.addPermanent island S.bob g)) (Setup.emptyGame S.bothPlayers) [1 .. (3 :: Int)]
      (srcId, withSorcerer) = S.addPermanent sorcerer S.alice withLands
      -- CR 302.6: settled, so the Sorcerer's {T} may be activated at all.
      settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.alice)
      addPermanent (ids, g) (who, p) = let (oid, g') = S.addPermanent p who g in (oid : ids, g')
      (permanentIds, withPermanents) = List.foldl' addPermanent ([], settled) permanents
      (victimId, onStack) = S.spellOnStack victim S.alice withPermanents
      (cancelId, withCancel) = S.addHandCard cancel S.bob onStack
      (stifleId, gs) = S.addHandCard stifle S.bob withCancel
   in (victimId, srcId, cancelId, stifleId, permanentIds, gs)

-- Every target prompt answers with this object, and CR 603.5's "may" is always
-- exercised -- so a silence below is the rule and never a declined option.
counteringAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
counteringAnswer oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- Prodigal Sorcerer's "any target" is aimed at ALICE, so the effect that must
-- not occur when the ability is countered is her own life total; Stifle's only
-- legal target is the ability, which the default interpreter picks.
counteringAtAlice :: Prompt.Prompt r -> r
counteringAtAlice p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

counteringAtAbility :: Prompt.Prompt r -> r
counteringAtAbility p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- bob casts his Cancel at alice's spell and lets it resolve.
cancelRun :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
cancelRun victimId cancelId gs =
  let answer :: Prompt.Prompt r -> r
      answer = counteringAnswer victimId
      cast = S.runPure answer gs (S.cast S.bob cancelId)
      resolved = S.runPure answer cast Stack.resolveTop
   in S.runPure answer resolved Engine.settleForPriority

-- alice activates her Sorcerer at herself, bob casts his Stifle at the ability,
-- and the stack is emptied down to the spell underneath. The first component is
-- the state once the Stifle has resolved, the second once the ability under it
-- has had its chance to resolve too.
abilityRun ::
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) ->
  ObjectId.ObjectId ->
  GameState.GameState ->
  (GameState.GameState, GameState.GameState)
abilityRun srcId ability stifleId gs =
  let activated = S.runPure counteringAtAlice (gs {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice srcId ability)
      cast = S.runPure counteringAtAbility activated (S.cast S.bob stifleId)
      stifleResolved = S.runPure counteringAtAbility cast Stack.resolveTop
      placed = S.runPure counteringAtAbility stifleResolved Engine.settleForPriority
   in (placed, S.runPure counteringAtAlice placed Stack.resolveTop)

-- Spider-Punk {1}{R} Legendary Creature -- Spider Human Hero 2/1 (Marvel's
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

        -- The ruling: "It will exile itself since it goes to the graveyard after
        -- its effect starts." Both sentences of the card in one assertion --
        -- the replacement is already in force when CR 608.2n moves the Will.
        Spec.it s "CR 608.2n / 614.1a the Will exiles itself" $ do
          (willId, hers, _, gs) <- board
          will <- S.printingOf s registry "Yawgmoth's Will"
          let after = willResolved willId gs
          -- By NAME, not by id: CR 400.7 makes the card leaving the stack a new
          -- object, so `willId` names nothing once it has moved.
          Spec.assertEqWith s "the graveyard holds only what was already there" (Game.zoneMembers Zone.Graveyard S.alice after) [hers]
          Spec.assertEqWith s "and the Will is in exile" (fmap (\o -> S.soleFaceName o after) (Game.zoneMembers Zone.Exile S.alice after)) [S.printingName will]

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

        -- The permission names a ZONE, not a TIME, which is the flashback ruling
        -- one rule over ("you can cast a sorcery using flashback only when you
        -- could normally cast a sorcery"). Read beside Cast.instantSpeed rather
        -- than inside it, so the sorcery window still has to be open.
        Spec.it s "CR 117.1a the grant does not lift the sorcery timing restriction" $ do
          (willId, hers, _, gs) <- board
          let after = willResolved willId gs
              upkeep = after {GameState.phase = Phase.Beginning BeginningStep.Upkeep}
          Spec.assertBool s (S.castable S.alice hers after) "castable in her own main phase"
          Spec.assertBool s (not (S.castable S.alice hers upkeep)) "not in her upkeep"

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

        -- CR 305.1: the play-lands half of the same sentence, on a board that
        -- differs from its own control in nothing but whether the Will resolved.
        -- The Swamp is put in the graveyard BEFORE the Will resolves, so the
        -- card's second sentence never sees it move and it is lying there for
        -- both readings.
        Spec.it s "CR 305.1 the play-lands half reaches a land in the graveyard" $ do
          (willId, _, _, gs) <- board
          swamp <- S.printingOf s registry "Swamp"
          let (landId, withLand) = S.addGraveyardCard swamp S.alice gs
              after = willResolved willId withLand
          Spec.assertBool s (notElem (Action.Type.Play landId Nothing) (Action.legalActions S.alice withLand)) "not offered before the Will resolves"
          Spec.assertBool s (elem (Action.Type.Play landId Nothing) (Action.legalActions S.alice after)) "offered once it has"

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

-- Play ONE named object and pass at every other prompt. Pinned to an id rather
-- than to "whichever land play is offered" (S.playLandAnswer), because this
-- board offers a land in a hand as well: an answerer that took the first play
-- would put the Mountain onto the battlefield and the assertions would read the
-- wrong card.
playOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
playOnly wanted p = case p of
  Prompt.ChooseAction _ _ actions -> case filter (playing wanted) actions of
    h : _ -> h
    [] -> Action.Type.Pass
  _ -> S.identityAnswer p

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
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
  Action.Type.PutCompanionIntoHand -> False
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

        -- End to end: the special action really moves the card, and CR 305.2a's
        -- tally counts it exactly as a play from hand does.
        Spec.it s "CR 305.1 playing it puts the graveyard land onto the battlefield" $ do
          (hers, _, _, _, _, _, with) <- board True
          swamp <- S.printingOf s registry "Swamp"
          let after = S.runPure (playOnly hers) with Engine.priorityLoop
          Spec.assertEqWith s "the Swamp is on the battlefield" (S.countOnBattlefieldByName (S.printingName swamp) S.alice after) 1
          Spec.assertEqWith s "her graveyard has only the Sorcerer left" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
          Spec.assertEqWith s "and CR 305.2a's tally counted it" (Map.lookup S.alice (GameState.landsPlayed after)) (Just 1)

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
-- The producer of PlayerEffect.CastFromTopOfLibrary, and the LIBRARY's entry in
-- Pawl.Engine.Cast.castZones: a CR 601.3 permission naming a zone the rules give
-- nobody, where Yawgmoth's Will above names the graveyard. The narrowing to the
-- TOP card is Cast.zoneCandidates' and not the Filter's, so these cases prove
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
        -- The whole card, end to end: library -> stack -> battlefield. Driven
        -- through Engine.priorityLoop rather than Pawl.Engine.Cast.castSpell,
        -- which is the difference between proving the permission and proving the
        -- move: `S.cast` announces whatever it is handed, so a board where the
        -- cast is never OFFERED still puts the card on the battlefield. The
        -- answerer is pinned to this id, so an unoffered cast passes instead.
        Spec.it s "CR 601.3 the top card of the library is cast and resolves" $ do
          elves <- S.printingOf s registry "Llanowar Elves"
          (herTop, _, _, _, _, gs) <- board "Llanowar Elves" True
          let resolved = S.runPure (castOnly herTop) gs Engine.priorityLoop
          Spec.assertEqWith s "the library's top card is on the battlefield" (S.countOnBattlefieldByName (S.printingName elves) S.alice resolved) 1
          Spec.assertEqWith s "and the library is one card shorter" (length (Game.zoneMembers Zone.Library S.alice resolved)) 2
          Spec.assertBool s (S.castable S.alice herTop gs) "it was castable"
          Spec.assertBool s (any (S.isCastOf herTop) (Action.legalActions S.alice gs)) "and offered"

        -- The pair. Two boards differing in the Horde and in nothing else, with
        -- the same Forests and the same hand on both -- so the offer appearing
        -- is the permission and can be nothing else.
        Spec.it s "CR 601.3 without the Horde the same top card is not castable" $ do
          (herTop, _, _, _, herHand, without) <- board "Llanowar Elves" False
          Spec.assertBool s (not (any (S.isCastOf herTop) (Action.legalActions S.alice without))) "the top card is not offered"
          Spec.assertBool s (not (S.castable S.alice herTop without)) "nor castable"
          Spec.assertBool s (not (PlayerEffect.mayCastFrom S.alice Zone.Library herTop without)) "and the typed question says no"
          Spec.assertBool s (any (S.isCastOf herHand) (Action.legalActions S.alice without)) "though her hand is offered on the same board"

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
        -- is Cast.zoneCandidates' narrowing and nothing else.
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

-- Spider-Man, 92), "Spells and abilities can't be countered". Run four ways off
-- counteringBoard above, with a Goblin Piker as the victim spell.
--
-- All four of the card's printed clauses are in its file now, and only this one
-- is read here: nothing on this board prevents damage, no other Spider enters,
-- and S.addPermanent inserts Spider-Punk into the battlefield directly rather
-- than raising an entry event, so CR 702.136a's riot has no CR 614.1c
-- replacement to be. CR 615.12's clause is proved in Pawl.ReplacementSpec's
-- "Spider-Punk (CR 615.12)" group instead, where a Mending Hands shield gives it
-- something to defeat.
spiderPunkSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spiderPunkSpec s registry =
  let withAbility act = do
        island <- S.printingOf s registry "Island"
        cancel <- S.printingOf s registry "Cancel"
        stifle <- S.printingOf s registry "Stifle"
        sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
        piker <- S.printingOf s registry "Goblin Piker"
        punk <- S.printingOf s registry "Spider-Punk"
        case Face.activatedAbilities (S.combinedFace sorcerer) of
          [] -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
          ability : _ -> act (counteringBoard island cancel stifle sorcerer piker) punk piker ability
   in Spec.describe s "SpiderPunk" $ do
        -- The CONTROL for the spell half. Without it every refusal below would
        -- also be true of a board where the Cancel never resolved at all.
        Spec.it s "CR 701.6a without Spider-Punk bob's Cancel counters alice's spell"
          . withAbility
          $ \board _ piker _ -> do
            let (victimId, _, cancelId, _, _, gs) = board []
                after = cancelRun victimId cancelId gs
            Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
            Spec.assertEqWith s "the spell is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
            Spec.assertEqWith s "and never reached the battlefield" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 0

        -- The SPELL half. CR 611.1's third clause makes Spider-Punk's sentence
        -- a rules-modifying continuous effect, and CR 101.2 makes its "can't"
        -- win: the Cancel resolves, does nothing, and CR 608.2n puts it into
        -- bob's graveyard while the spell it named stays on the stack.
        Spec.it s "CR 701.6a / 613.11 with Spider-Punk the same Cancel counters nothing"
          . withAbility
          $ \board punk _ _ -> do
            let (victimId, _, cancelId, _, _, gs) = board [(S.alice, punk)]
                after = cancelRun victimId cancelId gs
            Spec.assertEqWith s "alice's spell is still on the stack, alone" (GameState.stack after) [victimId]
            Spec.assertEqWith s "alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
            Spec.assertEqWith s "and the spent Cancel is bob's only graveyard card" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1

        -- The CONTROL for the ability half, and CR 113.9's reason it needs its
        -- own: an ability on the stack is not a spell, so nothing the spell
        -- case proves carries over.
        Spec.it s "CR 113.9 without Spider-Punk bob's Stifle counters alice's ability"
          . withAbility
          $ \board _ _ ability -> do
            let (victimId, srcId, _, stifleId, _, gs) = board []
                (placed, after) = abilityRun srcId ability stifleId gs
            Spec.assertEqWith s "the ability is gone, leaving only the Piker spell" (GameState.stack placed) [victimId]
            Spec.assertEqWith s "alice took no damage, so it never resolved" (S.lifeOf S.alice after) (Just 20)
            Spec.assertEqWith s "and bob's graveyard holds the spent Stifle alone" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1

        -- THE case Spider-Punk is in the pool for, and the half no card could
        -- reach before:
        -- Spider-Punk's clause is an ability of a BATTLEFIELD PERMANENT about
        -- other objects, where Pawl.Types.Counterability is CR 113.6g's
        -- self-referential ability of the spell itself and can say nothing
        -- about an ability at all. The ability survives the Stifle and
        -- resolves, so alice takes the 1 damage she aimed at herself.
        Spec.it s "CR 701.6a / 113.9 with Spider-Punk the ability survives the Stifle and resolves"
          . withAbility
          $ \board punk _ ability -> do
            let (victimId, srcId, _, stifleId, _, gs) = board [(S.alice, punk)]
                (placed, after) = abilityRun srcId ability stifleId gs
            Spec.assertEqWith s "the ability is still on the stack, above the Piker spell" (length (GameState.stack placed)) 2
            Spec.assertEqWith s "the spent Stifle is bob's only graveyard card" (length (Game.zoneMembers Zone.Graveyard S.bob placed)) 1
            Spec.assertEqWith s "and resolving it deals alice the 1 damage" (S.lifeOf S.alice after) (Just 19)
            Spec.assertEqWith s "leaving the Piker spell alone on the stack" (GameState.stack after) [victimId]

        -- PlayerScope.EachPlayer, and the case that tells it from
        -- PlayerScope.You: Spider-Punk's sentence has no possessive, so BOB's
        -- copy protects ALICE's spell from bob's own Cancel.
        Spec.it s "CR 109.5 EachPlayer: bob's own Spider-Punk protects alice's spell"
          . withAbility
          $ \board punk _ _ -> do
            let (victimId, _, cancelId, _, _, gs) = board [(S.bob, punk)]
                after = cancelRun victimId cancelId gs
            Spec.assertEqWith s "alice's spell is still on the stack" (GameState.stack after) [victimId]
            Spec.assertEqWith s "and alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

        -- CR 604.2: gathered live off the battlefield on every read, so
        -- destroying Spider-Punk lifts the protection in the same turn with
        -- nothing to unwind.
        Spec.it s "CR 604.2 destroying Spider-Punk makes the spell counterable again"
          . withAbility
          $ \board punk _ _ -> do
            let (victimId, _, cancelId, _, punkIds, gs) = board [(S.alice, punk)]
                gone = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable punkIds)
            Spec.assertBool s (PlayerEffect.cantBeCountered S.alice victimId gs) "protected while it stands"
            Spec.assertEqWith s "so the same Cancel counters nothing while it stands" (GameState.stack (cancelRun victimId cancelId gs)) [victimId]
            Spec.assertBool s (not (PlayerEffect.cantBeCountered S.alice victimId gone)) "not protected once it is gone"
            -- The stack is the readout, not the graveyard's size: the destroyed
            -- Spider-Punk is in that graveyard too, so a bare count could not
            -- tell a countered spell from an uncountered one.
            Spec.assertEqWith s "and the Cancel now counters, emptying the stack" (GameState.stack (cancelRun victimId cancelId gone)) []
            Spec.assertEqWith s "leaving the spell beside the destroyed Spider-Punk" (length (Game.zoneMembers Zone.Graveyard S.alice (cancelRun victimId cancelId gone))) 2

        -- CR 113.6g's carrier is untouched, which is what keeps the two apart:
        -- Spider-Punk's OWN card says nothing about being countered, and the
        -- protection it hands out comes from the CR 613.11 axis alone.
        Spec.it s "CR 113.6g Spider-Punk's own card field is Counterable" $ do
          punk <- S.printingOf s registry "Spider-Punk"
          Spec.assertEqWith s "the card field" (Face.counterability (S.combinedFace punk)) Counterability.Counterable

-- Prowling Serpopard {1}{G}{G} Creature -- Cat Snake 4/3 (Amonkhet, 180),
-- "This spell can't be countered. Creature spells you control can't be
-- countered." BOTH of the card's sentences, on the two different carriers the
-- rules give them:
--
--   * "This spell can't be countered" is CR 113.6g's self-referential ability,
--     which functions on the stack and rides the card as
--     Pawl.Types.Counterability.
--   * "Creature spells you control can't be countered" is an ability of a
--     BATTLEFIELD PERMANENT about OTHER objects, so CR 611.1's third clause
--     makes it a rules-modifying continuous effect on the CR 613.11 player
--     axis.
--
-- The second sentence is why the card is in THIS file and not only among the CR
-- 113.6g cards: it NARROWS by the victim spell's own qualities, which
-- Spider-Punk's unfiltered "Spells and abilities can't be countered" does not.
-- The whole group therefore turns on the same Cancel counting differently for a
-- CREATURE spell and a NONCREATURE one on one board -- an assertion no
-- unfiltered arm can pass.
prowlingSerpopardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
prowlingSerpopardSpec s registry =
  let withVictim name act = do
        island <- S.printingOf s registry "Island"
        cancel <- S.printingOf s registry "Cancel"
        stifle <- S.printingOf s registry "Stifle"
        sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
        victim <- S.printingOf s registry name
        cat <- S.printingOf s registry "Prowling Serpopard"
        case Face.activatedAbilities (S.combinedFace sorcerer) of
          [] -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
          ability : _ -> act (counteringBoard island cancel stifle sorcerer victim) cat ability
   in Spec.describe s "ProwlingSerpopard" $ do
        -- The CONTROL for the creature half.
        Spec.it s "CR 701.6a without Prowling Serpopard bob's Cancel counters alice's creature spell"
          . withVictim "Goblin Piker"
          $ \board _ _ -> do
            let (victimId, _, cancelId, _, _, gs) = board []
            Spec.assertEqWith s "the stack is empty" (GameState.stack (cancelRun victimId cancelId gs)) []

        -- The clause the card is in the pool for, in the direction the filter
        -- ADMITS: a creature spell alice controls matches CR 613.11's effect and
        -- CR 101.2 makes its "can't" win.
        Spec.it s "CR 701.6a / 613.11 with Prowling Serpopard alice's creature spell survives"
          . withVictim "Goblin Piker"
          $ \board cat _ -> do
            let (victimId, _, cancelId, _, _, gs) = board [(S.alice, cat)]
                after = cancelRun victimId cancelId gs
            Spec.assertEqWith s "alice's creature spell is still on the stack, alone" (GameState.stack after) [victimId]
            Spec.assertEqWith s "alice's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

        -- The CONTROL for the noncreature half, so the refusal below is a
        -- statement about the FILTER rather than about a Cancel that never
        -- resolved.
        Spec.it s "CR 701.6a without Prowling Serpopard bob's Cancel counters alice's noncreature spell"
          . withVictim "Lightning Bolt"
          $ \board _ _ -> do
            let (victimId, _, cancelId, _, _, gs) = board []
            Spec.assertEqWith s "the stack is empty" (GameState.stack (cancelRun victimId cancelId gs)) []

        -- THE case #788 is about, and the one an unfiltered arm CANNOT pass:
        -- the very same Serpopard, on the very same board, leaves alice's
        -- noncreature spell counterable, because CR 613.11's effect names only
        -- creature spells.
        Spec.it s "CR 701.6a / 613.11 the same Serpopard leaves alice's noncreature spell counterable"
          . withVictim "Lightning Bolt"
          $ \board cat _ -> do
            let (victimId, _, cancelId, _, _, gs) = board [(S.alice, cat)]
                after = cancelRun victimId cancelId gs
            Spec.assertEqWith s "the Cancel counters it, emptying the stack" (GameState.stack after) []
            -- The stack is the readout and the graveyard only corroborates it:
            -- alice's graveyard holds the countered spell and nothing else, so
            -- the Serpopard on the battlefield is not being counted here.
            Spec.assertEqWith s "leaving the countered spell in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

        -- PlayerScope.You, and the case that tells it from Spider-Punk's
        -- EachPlayer: "creature spells YOU control", so BOB's copy protects
        -- nothing of alice's.
        Spec.it s "CR 109.5 You: bob's own Prowling Serpopard does not protect alice's creature spell"
          . withVictim "Goblin Piker"
          $ \board cat _ -> do
            let (victimId, _, cancelId, _, _, gs) = board [(S.bob, cat)]
            Spec.assertEqWith s "the Cancel still counters, emptying the stack" (GameState.stack (cancelRun victimId cancelId gs)) []

        -- CR 113.9 / 701.6a's OTHER subject. An ability on the stack has no
        -- card behind it -- Game.faceOf answers Nothing for one -- so a Filter
        -- naming a CARD TYPE can never match it, and alice's Prodigal Sorcerer
        -- ability is Stifled with the Serpopard standing. That is the whole of
        -- the answer to the wrinkle a filtered arm has and the unfiltered one
        -- does not: this card's protection reaches spells only.
        Spec.it s "CR 113.9 with Prowling Serpopard alice's activated ability is still counterable"
          . withVictim "Goblin Piker"
          $ \board cat ability -> do
            let (victimId, srcId, _, stifleId, _, gs) = board [(S.alice, cat)]
                (placed, after) = abilityRun srcId ability stifleId gs
            Spec.assertEqWith s "the ability is gone, leaving only the creature spell" (GameState.stack placed) [victimId]
            Spec.assertEqWith s "alice took no damage, so it never resolved" (S.lifeOf S.alice after) (Just 20)

        -- CR 113.6g, the card's FIRST sentence, on the carrier that is not the
        -- player axis at all: a Prowling Serpopard SPELL is uncounterable with
        -- NO Serpopard on the battlefield, which no CR 613.11 effect could
        -- explain.
        Spec.it s "CR 113.6g a Prowling Serpopard spell can't be countered with none on the battlefield"
          . withVictim "Prowling Serpopard"
          $ \board cat _ -> do
            let (victimId, _, cancelId, _, _, gs) = board []
            Spec.assertEqWith s "the card field" (Face.counterability (S.combinedFace cat)) Counterability.CantBeCountered
            Spec.assertEqWith s "and the spell is still on the stack" (GameState.stack (cancelRun victimId cancelId gs)) [victimId]

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
  let dmg = DamageEvent.MkDamageEvent attacker (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Combat
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

        -- THE CONTROL for CR 725.2's route, with bob crowned by the fixture
        -- instead of by Jared's trigger: an unrestricted alice takes the crown off
        -- a creature's combat damage.
        Spec.it s "CR 725.2 with no restriction standing, combat damage to the monarch hands alice the crown" $ do
          (_, _, pikerId, gs) <- board
          Spec.assertEqWith s "alice steals it" (GameState.monarch (damageToTheMonarch pikerId (S.withMonarch S.bob gs))) (Just S.alice)

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

-- CR 107.3b's board: bob holds a Molten Disaster and controls Omniscience, so
-- the grant's {0} is among his candidates; `lands` Mountains of his decide
-- whether the printed {X}{R}{R} is another. alice's `extra` go onto her
-- battlefield beside her nine Mountains, the Winnower or nothing.
--
-- Returns (bob's Disaster, board).
omniscientBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> [Printing.Printing] -> (ObjectId.ObjectId, GameState.GameState)
omniscientBoard mountain disaster omniscience lands extra =
  let base = S.landsInPlay mountain 9
      withBobsLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.bob g)) base [1 .. lands]
      withGrant = snd (S.addPermanent omniscience S.bob withBobsLands)
      (bobsDisaster, gs) = S.addHandCard disaster S.bob withGrant
      put g printing = snd (S.addPermanent printing S.alice g)
   in (bobsDisaster, (List.foldl' put gs extra) {GameState.phase = Phase.PrecombatMain})

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

    -- CR 107.3b puts a floor under that search: a spell cast "without paying its
    -- mana cost" has 0 as its only legal X, so the choice CR 601.3a lets bob
    -- consider is not his to make and the mana value the prohibition judges is
    -- the printed one. Omniscience is bob's, the Winnower alice's, and bob has no
    -- lands, so the grant's {0} is the only candidate he can announce.
    --
    -- THREE boards off one builder, each one thing apart from its neighbour:
    -- the Winnower gone shows the free cast is otherwise offered, and nine
    -- Mountains under the Winnower show the printed {X}{R}{R} still escapes --
    -- the clamp reaches the free candidate and not the search itself.
    Spec.it s "CR 107.3b a free cast fixes X at 0, so the {X} spell is refused as even" $ do
      mountain <- S.printingOf s registry "Mountain"
      disaster <- S.printingOf s registry "Molten Disaster"
      omniscience <- S.printingOf s registry "Omniscience"
      winnower <- S.printingOf s registry "Void Winnower"
      let (refused, board) = omniscientBoard mountain disaster omniscience 0 [winnower]
          (offered, bare) = omniscientBoard mountain disaster omniscience 0 []
          (escaping, funded) = omniscientBoard mountain disaster omniscience 9 [winnower]
          -- An X of 3 wherever one is asked, so a cast that reached CR 601.2b's
          -- announcement would show in the life totals; the grant's {0} names
          -- no X, so a free Disaster deals nothing.
          answering :: Prompt.Prompt r -> r
          answering p = case p of
            Prompt.ChooseCost _ _ _ candidates ->
              Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just (ManaCost.MkManaCost [])) . Cost.Type.mana) candidates)
            Prompt.ChooseX {} -> 3
            _ -> S.identityAnswer p
          resolvedFree = S.runPure answering (S.runPure answering bare (S.cast S.bob offered)) Stack.resolveTop
          resolvedFunded = S.runPure answering (S.runPure answering funded (S.cast S.bob escaping)) Stack.resolveTop
      Spec.assertBool s (not (any (S.isCastOf refused) (askedOf S.bob board))) "the free {X} spell is refused under the Winnower"
      Spec.assertBool s (not (S.castable S.bob refused board)) "and is not castable"
      Spec.assertBool s (any (S.isCastOf offered) (askedOf S.bob bare)) "with the Winnower gone the free cast is offered"
      Spec.assertEqWith s "and resolves at X = 0: alice takes nothing" (S.lifeOf S.alice resolvedFree) (Just 20)
      Spec.assertEqWith s "and neither does bob" (S.lifeOf S.bob resolvedFree) (Just 20)
      Spec.assertBool s (any (S.isCastOf escaping) (askedOf S.bob funded)) "with nine Mountains the printed cost is still offered under the Winnower"
      Spec.assertEqWith s "and only the printed cost: the grant's even route is withheld, so the Disaster is cast at X = 3 and alice takes 3" (S.lifeOf S.alice resolvedFunded) (Just 17)
      Spec.assertEqWith s "and so does bob" (S.lifeOf S.bob resolvedFunded) (Just 17)

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
          activate oid = case Activate.abilitiesFor oid gs of
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
activationsOf oid = filter isIt
  where
    isIt a = case a of
      Action.Type.Activate o _ -> o == oid
      _ -> False

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

    -- CR 601.1a / 601.3b, the whole fix: once Scout's Warning has resolved,
    -- Dryad Arbor -- a LAND card -- is offered outside the sorcery-speed
    -- window, which Pawl.Engine.PlayerEffect.CastAsThoughItHadFlash alone
    -- could never reach (#1938).
    Spec.it s "CR 601.1a / 601.3b Dryad Arbor is offered once Scout's Warning has resolved" $ do
      plains <- S.printingOf s registry "Plains"
      warning <- S.printingOf s registry "Scout's Warning"
      dryadArbor <- S.printingOf s registry "Dryad Arbor"
      let (arborId, after) = scoutsWarningResolved plains warning dryadArbor
      Spec.assertBool s (any (playing arborId) (Action.legalActions S.alice (busyStack after))) "offered"

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

    -- CR 601.1a's other half: MayPlayAsThoughItHadFlash reaches a CAST too
    -- (Pawl.Engine.PlayerEffect.mayCastAsThoughItHadFlash's own arm), not only
    -- Action.landTimingOk -- an ordinary creature SPELL becomes castable
    -- outside the sorcery-speed window as well.
    Spec.it s "CR 601.1a the grant also widens a creature spell's cast window" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      plains <- S.printingOf s registry "Plains"
      warning <- S.printingOf s registry "Scout's Warning"
      let (pikerBase, pikerId) = S.pikerInHand mountain piker 9 Phase.PrecombatMain
          withPlains = S.landsFor plains S.alice 1 pikerBase
          -- CR 104.3c, scoutsWarningBoard's own trap: the second clause draws
          -- a card, so the library must not be empty when it resolves.
          (_, withLibrary) = S.addLibraryCard plains S.alice withPlains
          (warningId, g1) = S.addHandCard warning S.alice withLibrary
          before = g1 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
          resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
          resolved = resolveAll (S.runPure S.identityAnswer before (S.cast S.alice warningId))
      Spec.assertBool s (not (S.castable S.alice pikerId (busyStack before))) "not castable before Scout's Warning resolves"
      Spec.assertBool s (S.castable S.alice pikerId (busyStack resolved)) "castable once it has"

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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.PlayerEffect" $ do
  extraLandDropsSpec s registry
  vedalkenOrrerySpec s registry
  sigardasAidSpec s registry
  yawgmothsWillSpec s registry
  crucibleSpec s registry
  garruksHordeSpec s registry
  voidWinnowerSpec s registry
  spiderPunkSpec s registry
  prowlingSerpopardSpec s registry
  jaredSpec s registry
  oppressiveRaysSpec s registry
  scoutsWarningSpec s registry
