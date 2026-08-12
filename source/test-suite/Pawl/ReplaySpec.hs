{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Replay: record/replay transcript round-trips.
module Pawl.ReplaySpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Monarch as Monarch
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Desync as Desync
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HandActionIndex as HandActionIndex
import qualified Pawl.Types.HybridPayment as HybridPayment
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaOption as ManaOption
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MulliganOffer as MulliganOffer
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- One way of tapping a single-colour source: CR 305.6's intrinsic "{T}" cost and
-- a one-unit yield, which is what a ChooseManaYield candidate looks like for
-- every source but Sol Ring. No production tag: every source these replays tap
-- is a nonsnow one (CR 205.4g).
oneMana :: Color.Color -> ManaOption.ManaOption
oneMana color =
  ManaOption.MkManaOption
    { ManaOption.cost = Mana.intrinsicManaCost,
      ManaOption.yield = Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored color, ManaUnit.tags = Set.empty}]
    }

combatReplaySpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
combatReplaySpec s =
  let decider = Decide.deciderFor S.alice (Setup.emptyGame S.bothPlayers)
      oid = ObjectId.MkObjectId 7
      attackPrompt = Prompt.DeclareAttackers decider S.alice [oid]
      blockPrompt = Prompt.DeclareBlockers decider S.bob [oid] [oid]
      damagePrompt = Prompt.AssignCombatDamage decider S.alice oid (Map.singleton (Recipient.ToCreature oid) 0) 2
      -- CR 118.12a: Mana Leak's {3}, the cost Prompt.ChooseToPay carries.
      genericThree = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.Type.components = []}
      -- CR 725.2's own pair, one borne and one sourceless: the crown steal hung
      -- on an object and the inherent end-step draw hung on nothing.
      orderEntries =
        [ TriggerEntry.MkTriggerEntry (TriggerSource.OfObject oid) Monarch.crownSteal,
          TriggerEntry.MkTriggerEntry TriggerSource.Sourceless Monarch.endStepDraw
        ]
      -- CR 615.7: two simultaneous combat damage events one shield could cover
      -- part of, which is the batch that prompt is asked over.
      orderDamageEvents =
        [ DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.alice) 5 False False False 0 Nothing DamageKind.Combat,
          DamageEvent.MkDamageEvent (ObjectId.MkObjectId 8) (Recipient.ToPlayer S.alice) 3 False False False 0 Nothing DamageKind.Combat
        ]
      -- CR 601.2h: Jarad, Golgari Lich Lord's two halves, the printed cost whose
      -- payment order is the payer's.
      orderComponents =
        [ CostComponent.Sacrifice 1 (Filter.Type.HasSubtype Subtype.Swamp),
          CostComponent.Sacrifice 1 (Filter.Type.HasSubtype Subtype.Forest)
        ]
   in Spec.describe s "CombatReplay" $ do
        Spec.it s "attackers round-trip through the transcript" $
          Spec.assertEqWith s "round trip" (Replay.decode attackPrompt (Replay.encode attackPrompt [oid])) (Just [oid])
        Spec.it s "blockers round-trip through the transcript" $ do
          let answer = Map.singleton oid (Set.singleton oid)
          Spec.assertEqWith s "round trip" (Replay.decode blockPrompt (Replay.encode blockPrompt answer)) (Just answer)
        Spec.it s "a damage assignment round-trips through the transcript" $ do
          let answer :: Map.Map Recipient.Recipient Natural.Natural
              answer = Map.singleton (Recipient.ToCreature oid) 2
          Spec.assertEqWith s "round trip" (Replay.decode damagePrompt (Replay.encode damagePrompt answer)) (Just answer)
        Spec.it s "a mismatched response decodes to Nothing" $
          Spec.assertEqWith s "mismatch" (Replay.decode attackPrompt (Response.Shuffled [oid])) Nothing
        -- The recorded answer is 4 while the prompt's bound is 2: the transcript
        -- carries what the player SAID, and CR 601.2b lets them say more than
        -- the board can pay. A codec that folded the bound into the response
        -- would replay a different game (#417).
        Spec.it s "ChooseX records and replays a Natural" $ do
          let p = Prompt.ChooseX decider S.alice oid 2
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p 4)) (Just (4 :: Natural.Natural))
        -- CR 601.2b / 700.2a: a modal choice (Response.ChoseModes, a Seq
        -- ModeIndex) round-trips through the DecisionLog exactly like every
        -- other response -- no JSON codec is involved: Response has never had
        -- one (only Prompt/Response answers get serialized, via Replay's
        -- encode/decode, not Pawl.Codec's JSON arms).
        Spec.it s "ChooseModes records and replays a Seq ModeIndex" $ do
          let legal = Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 1]
              p = Prompt.ChooseModes decider S.alice oid legal (ModeSelection.ChooseExactly 1)
              answer = Seq.singleton (ModeIndex.MkModeIndex 1)
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
        -- CR 702.42a: whether the entwine cost was paid decides how many modes
        -- the spell has, so a transcript that lost it would replay a different
        -- spell. Both answers are checked: a decode that ignored the response
        -- and returned a fixed value would pass one leg by accident.
        Spec.it s "ChooseEntwine records and replays an EntwineDecision" $ do
          let entwineCost =
                Cost.Type.MkCost
                  { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]),
                    Cost.Type.components = []
                  }
              p = Prompt.ChooseEntwine decider S.alice oid entwineCost
          Spec.assertEqWith
            s
            "entwining round trips"
            (Replay.decode p (Replay.encode p EntwineDecision.Entwines))
            (Just EntwineDecision.Entwines)
          Spec.assertEqWith
            s
            "declining round trips"
            (Replay.decode p (Replay.encode p EntwineDecision.Declines))
            (Just EntwineDecision.Declines)
          -- Discriminating: fails if ChooseEntwine reuses another
          -- two-valued response rather than getting its own constructor.
          Spec.assertEqWith
            s
            "an optional decision is not an entwine announcement"
            (Replay.decode p (Response.ChoseOptional OptionalDecision.Exercises))
            Nothing
        Spec.it s "a short transcript declines entwine" $
          -- CR 702.42a: declining is always legal and costs nothing, so it is
          -- the least-eventful fallback when a transcript runs short.
          Spec.assertEqWith
            s
            "declines"
            ( Replay.defaultAnswer
                ( Prompt.ChooseEntwine
                    decider
                    S.alice
                    oid
                    Cost.Type.MkCost
                      { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]),
                        Cost.Type.components = []
                      }
                )
            )
            EntwineDecision.Declines
        -- CR 702.33d: whether the kicker cost was paid decides what the spell
        -- does on resolution, so a transcript that lost it would replay a
        -- different spell. Both answers are checked, for ChooseEntwine's reason.
        Spec.it s "ChooseKicker records and replays a KickerDecision" $ do
          let kickerCost =
                Cost.Type.MkCost
                  { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                    Cost.Type.components = []
                  }
              p = Prompt.ChooseKicker decider S.alice oid kickerCost
          Spec.assertEqWith
            s
            "kicking round trips"
            (Replay.decode p (Replay.encode p KickerDecision.Kicks))
            (Just KickerDecision.Kicks)
          Spec.assertEqWith
            s
            "declining round trips"
            (Replay.decode p (Replay.encode p KickerDecision.Declines))
            (Just KickerDecision.Declines)
          -- Discriminating: fails if ChooseKicker reuses the entwine
          -- announcement rather than getting its own constructor.
          Spec.assertEqWith
            s
            "an entwine announcement is not a kicker announcement"
            (Replay.decode p (Response.AnnouncedEntwine EntwineDecision.Entwines))
            Nothing
        Spec.it s "a short transcript declines the kicker" $
          -- CR 702.33a: declining is always legal and costs nothing, so it is
          -- the least-eventful fallback when a transcript runs short.
          Spec.assertEqWith
            s
            "declines"
            ( Replay.defaultAnswer
                ( Prompt.ChooseKicker
                    decider
                    S.alice
                    oid
                    Cost.Type.MkCost
                      { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]),
                        Cost.Type.components = []
                      }
                )
            )
            KickerDecision.Declines
        Spec.it s "ChooseCopyTarget records and replays a Maybe ObjectId" $ do
          let p = Prompt.ChooseCopyTarget decider S.alice oid [ObjectId.MkObjectId 7]
              answer = Just (ObjectId.MkObjectId 7)
          Spec.assertEqWith s "round-trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
        -- CR 208.2b: Primal Plasma is in no deck, so no gameplay-level test
        -- reaches Response.ChoseEntryOption through the record/replay path --
        -- this exercises the transcript codec directly, matching the shape
        -- of every other payload-carrying prompt above.
        Spec.it s "ChooseEntryOption records and replays a Natural" $ do
          let options = [EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty}]
              p = Prompt.ChooseEntryOption decider S.alice oid options
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p 1)) (Just (1 :: Natural.Natural))
        -- CR 702.136a: riot's "may", answered as a permanent enters. Its payload
        -- has the same shape as CR 603.5's resolution-time "may", so the
        -- transcript has to tell the two apart -- a shared Response constructor
        -- would replay a declined riot into a declined printed "may". Both
        -- decisions round-trip, because a codec that collapsed them would replay
        -- a +1/+1 counter as haste.
        Spec.it s "ChooseRiot records and replays an OptionalDecision, and rejects a resolution-time may" $ do
          let p = Prompt.ChooseRiot decider S.alice oid
          Monad.forM_ [OptionalDecision.Declines, OptionalDecision.Exercises] $ \decision ->
            Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p decision)) (Just decision)
          Spec.assertEqWith s "a printed may is not an answer to it" (Replay.decode p (Response.ChoseOptional OptionalDecision.Exercises)) Nothing
        -- CR 702.136a: a transcript that runs short takes the half that puts no
        -- counter on the board.
        Spec.it s "defaultAnswer declines riot's counter" $
          Spec.assertEqWith s "declines" (Replay.defaultAnswer (Prompt.ChooseRiot decider S.alice oid)) OptionalDecision.Declines
        -- CR 614.1c / 119.4: the other as-enters "may", and the one that costs
        -- something. A separate Response constructor for ChooseRiot's reason
        -- carried one step further: three OptionalDecision-valued prompts now
        -- exist, and a transcript that answered any of them must not be read as an
        -- answer to another -- so both of the others are rejected explicitly.
        Spec.it s "ChoosePayLifeOnEntry records and replays an OptionalDecision, and rejects the other as-enters mays" $ do
          let p = Prompt.ChoosePayLifeOnEntry decider S.alice oid 3
          Monad.forM_ [OptionalDecision.Declines, OptionalDecision.Exercises] $ \decision ->
            Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p decision)) (Just decision)
          Spec.assertEqWith s "a riot answer is not an answer to it" (Replay.decode p (Response.ChoseRiot OptionalDecision.Exercises)) Nothing
          Spec.assertEqWith s "nor is a printed may" (Replay.decode p (Response.ChoseOptional OptionalDecision.Exercises)) Nothing
        -- CR 614.1c: a transcript that runs short spends no life.
        Spec.it s "defaultAnswer declines the life payment" $
          Spec.assertEqWith s "declines" (Replay.defaultAnswer (Prompt.ChoosePayLifeOnEntry decider S.alice oid 3)) OptionalDecision.Declines
        -- CR 303.4k: the fourth OptionalDecision-valued prompt, and the first
        -- that is NOT an as-enters one -- Gift of Doom's "as this Aura is turned
        -- face up, you may attach it to a creature". The three above are rejected
        -- explicitly for the reason ChoosePayLifeOnEntry rejects the other two: a
        -- transcript that answered one "may" must not be read as an answer to a
        -- different one, and here the two boards a mix-up produces differ by a
        -- permanent -- CR 704.5m buries the Aura this one declines.
        Spec.it s "ChooseTurnUpAttachment records and replays an OptionalDecision, and rejects every other may" $ do
          let p = Prompt.ChooseTurnUpAttachment decider S.alice oid
          Monad.forM_ [OptionalDecision.Declines, OptionalDecision.Exercises] $ \decision ->
            Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p decision)) (Just decision)
          Spec.assertEqWith s "a riot answer is not an answer to it" (Replay.decode p (Response.ChoseRiot OptionalDecision.Exercises)) Nothing
          Spec.assertEqWith s "nor is an as-enters life payment" (Replay.decode p (Response.ChosePayLifeOnEntry OptionalDecision.Exercises)) Nothing
          Spec.assertEqWith s "nor is a printed may" (Replay.decode p (Response.ChoseOptional OptionalDecision.Exercises)) Nothing
        -- CR 303.4k: a transcript that runs short moves no permanent.
        Spec.it s "defaultAnswer declines the turn-up attachment" $
          Spec.assertEqWith s "declines" (Replay.defaultAnswer (Prompt.ChooseTurnUpAttachment decider S.alice oid)) OptionalDecision.Declines
        -- CR 614.1c / 105.1: a colour chosen as a permanent enters. Every one of
        -- the five is round-tripped, because a codec that collapsed two of them
        -- would replay a Painter's Servant naming blue as one naming white --
        -- and white is exactly what defaultAnswer falls back to, so an
        -- undistinguished White would hide the bug.
        Spec.it s "ChooseColor records and replays a Color" $ do
          let p = Prompt.ChooseColor decider S.alice oid
          Monad.forM_ [Color.White, Color.Blue, Color.Black, Color.Red, Color.Green] $ \color ->
            Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p color)) (Just color)
        -- CR 105.1: a transcript that runs short still answers with one of the
        -- five, and it is the one every decider in the suite agrees on.
        Spec.it s "defaultAnswer chooses white" $
          Spec.assertEqWith s "white" (Replay.defaultAnswer (Prompt.ChooseColor decider S.alice oid)) Color.White
        -- CR 614.1c / 305.6: a basic land type chosen as a permanent enters. All
        -- five are round-tripped for the ChooseColor test's reason -- a codec
        -- that collapsed two would replay a Convincing Mirage naming Island as
        -- one naming Mountain, and Mountain is exactly what defaultAnswer falls
        -- back to, so an undistinguished Mountain would hide the bug.
        Spec.it s "ChooseBasicLandType records and replays a Subtype" $ do
          let p = Prompt.ChooseBasicLandType decider S.alice oid
          Monad.forM_ [Subtype.Plains, Subtype.Island, Subtype.Swamp, Subtype.Mountain, Subtype.Forest] $ \subtype ->
            Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p subtype)) (Just subtype)
        -- CR 305.6: a transcript that runs short still answers with one of the
        -- five, and it is the one ChooseLandTypeSwap's fallback already names.
        Spec.it s "defaultAnswer chooses Mountain" $
          Spec.assertEqWith s "Mountain" (Replay.defaultAnswer (Prompt.ChooseBasicLandType decider S.alice oid)) Subtype.Mountain
        -- CR 201.4 / 614.1c: a card name chosen as a permanent enters (Null
        -- Chamber). Unlike the two prompts above the answer is not drawn from a
        -- fixed five, so what a collapsing codec would lose is any name at all
        -- -- and the empty name is exactly what defaultAnswer falls back to,
        -- which is why one of the round-tripped names is empty and the others
        -- are not.
        Spec.it s "ChooseCardName records and replays a CardName" $ do
          let p = Prompt.ChooseCardName decider S.alice oid (Filter.Type.And [])
          Monad.forM_ [Text.empty, Text.pack "Goblin Piker", Text.pack "Ash Barrens"] $ \name ->
            Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p (CardName.MkCardName name))) (Just (CardName.MkCardName name))
        -- CR 201.2a: a transcript that runs short answers with the name no
        -- object has, so the Chamber it replays prohibits nothing.
        Spec.it s "defaultAnswer chooses no name" $
          Spec.assertEqWith
            s
            "the empty name"
            (Replay.defaultAnswer (Prompt.ChooseCardName decider S.alice oid (Filter.Type.And [])))
            (CardName.MkCardName Text.empty)
        -- CR 614.12a: which opponent an as-enters choice names. Its answer has
        -- the same shape as ChooseDefender's, so the transcript has to tell the
        -- two apart -- a shared Response constructor would replay a combat
        -- declaration into Null Chamber's entry.
        Spec.it s "ChooseOpponent records and replays a PlayerId, and rejects a defender" $ do
          let p = Prompt.ChooseOpponent decider S.alice oid (S.bob NonEmpty.:| [])
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p S.bob)) (Just S.bob)
          Spec.assertEqWith s "a defender is not an answer to it" (Replay.decode p (Response.ChoseDefender S.bob)) Nothing
        -- CR 612.2's two word families are asked for by two prompts whose
        -- answers have the SAME shape, so the transcript has to tell them apart:
        -- a shared Response constructor would replay Magical Hack's basic land
        -- types into Artificial Evolution's creature-type ask. Each decodes its
        -- own and rejects the other.
        Spec.it s "the two CR 612 swap prompts do not answer each other" $ do
          let slot = SlotName.MkSlotName (Text.pack "target")
              landPrompt = Prompt.ChooseLandTypeSwap decider S.alice oid slot Set.empty
              creaturePrompt = Prompt.ChooseCreatureTypeSwap decider S.alice oid slot (Set.singleton Subtype.Wall)
              lands = (Subtype.Mountain, Subtype.Island)
              creatures = (Subtype.Frog, Subtype.Elf)
          Spec.assertEqWith s "land round trip" (Replay.decode landPrompt (Replay.encode landPrompt lands)) (Just lands)
          Spec.assertEqWith s "creature round trip" (Replay.decode creaturePrompt (Replay.encode creaturePrompt creatures)) (Just creatures)
          Spec.assertEqWith s "a land answer does not fill a creature ask" (Replay.decode creaturePrompt (Replay.encode landPrompt lands)) Nothing
          Spec.assertEqWith s "nor the other way round" (Replay.decode landPrompt (Replay.encode creaturePrompt creatures)) Nothing
        Spec.it s "DeclareMulligan records and replays a MulliganDecision" $ do
          let offer = MulliganOffer.MkMulliganOffer {MulliganOffer.taken = 0, MulliganOffer.bottomCount = 1}
              p = Prompt.DeclareMulligan decider S.alice offer
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p MulliganDecision.Mulligan)) (Just MulliganDecision.Mulligan)
        Spec.it s "Bottom records and replays an ordered [ObjectId]" $ do
          let p = Prompt.Bottom decider S.alice [ObjectId.MkObjectId 7, ObjectId.MkObjectId 8] 1
              answer = [ObjectId.MkObjectId 8]
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
        -- CR 103.5b: the two offers are the SAME card's first and second action,
        -- so a transcript that recorded only the card would replay the wrong one.
        Spec.it s "MulliganAction records and replays which action of which card" $ do
          let first = (ObjectId.MkObjectId 7, HandActionIndex.MkHandActionIndex 0)
              second = (ObjectId.MkObjectId 7, HandActionIndex.MkHandActionIndex 1)
              p = Prompt.MulliganAction decider S.alice [first, second]
              answer = Just second
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
          Spec.assertEqWith s "and the same card's other action replays as itself" (Replay.decode p (Replay.encode p (Just first))) (Just (Just first))
        Spec.it s "OpeningHandAction records and replays which action of which card" $ do
          let p = Prompt.OpeningHandAction decider S.alice [(ObjectId.MkObjectId 7, HandActionIndex.MkHandActionIndex 0), (ObjectId.MkObjectId 8, HandActionIndex.MkHandActionIndex 0)]
              answer = Just (ObjectId.MkObjectId 7, HandActionIndex.MkHandActionIndex 0)
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
        -- CR 603.5: both answers to a printed "may", so the transcript is
        -- proved to distinguish them -- a codec that collapsed them would
        -- replay a declined Renewed Faith as a taken one.
        Spec.it s "ChooseOptional records and replays both answers" $ do
          let p = Prompt.ChooseOptional decider S.alice oid (ModeIndex.MkModeIndex 0) (ClauseIndex.MkClauseIndex 0)
          Spec.assertEqWith s "exercised" (Replay.decode p (Replay.encode p OptionalDecision.Exercises)) (Just OptionalDecision.Exercises)
          Spec.assertEqWith s "declined" (Replay.decode p (Replay.encode p OptionalDecision.Declines)) (Just OptionalDecision.Declines)
        -- CR 603.5: a transcript that runs short must not silently take an
        -- option its author never chose.
        Spec.it s "defaultAnswer declines a may" $
          Spec.assertEqWith
            s
            "declines"
            (Replay.defaultAnswer (Prompt.ChooseOptional decider S.alice oid (ModeIndex.MkModeIndex 0) (ClauseIndex.MkClauseIndex 0)))
            OptionalDecision.Declines
        -- CR 118.12a: both answers to a resolution-time cost, so the transcript
        -- is proved to distinguish them -- a codec that collapsed them would
        -- replay a paid Mana Leak as a refused one, which is the whole card.
        Spec.it s "ChooseToPay records and replays both answers" $ do
          let p = Prompt.ChooseToPay decider S.alice oid (ModeIndex.MkModeIndex 0) (ClauseIndex.MkClauseIndex 0) genericThree
          Spec.assertEqWith s "paid" (Replay.decode p (Replay.encode p PaymentDecision.Pays)) (Just PaymentDecision.Pays)
          Spec.assertEqWith s "declined" (Replay.decode p (Replay.encode p PaymentDecision.Declines)) (Just PaymentDecision.Declines)
        -- CR 118.12a: a transcript that runs short must not spend a player's
        -- mana on a payment its author never announced.
        Spec.it s "defaultAnswer declines a resolution-time cost" $
          Spec.assertEqWith
            s
            "declines"
            (Replay.defaultAnswer (Prompt.ChooseToPay decider S.alice oid (ModeIndex.MkModeIndex 0) (ClauseIndex.MkClauseIndex 0) genericThree))
            PaymentDecision.Declines
        Spec.it s "a mismatched response does not decode as a may" $
          Spec.assertEqWith
            s
            "mismatch"
            (Replay.decode (Prompt.ChooseOptional decider S.alice oid (ModeIndex.MkModeIndex 0) (ClauseIndex.MkClauseIndex 0)) (Response.Conceded Concession.Continues))
            Nothing
        Spec.it s "defaultAnswer attacks with nothing" $
          Spec.assertEqWith s "no attacks" (Replay.defaultAnswer attackPrompt) []
        Spec.it s "defaultAnswer blocks with nothing" $
          Spec.assertEqWith s "no blocks" (Replay.defaultAnswer blockPrompt) Map.empty
        Spec.it s "defaultAnswer assigns a LEGAL division" $
          -- Total must equal the attacker's power, or the fallback would be
          -- rejected by validation and deal no damage at all.
          Spec.assertEqWith s "all to one blocker" (Replay.defaultAnswer damagePrompt) (Map.singleton (Recipient.ToCreature oid) 2)
        -- The payload mixes both kinds of entry (CR 113.7's borne trigger and
        -- CR 725.2's sourceless one), which is what the batch really looks like
        -- when the monarch controls a trigger of her own at the same moment.
        -- Each entry carries the ABILITY as well as the source (#61), so the two
        -- named here are CR 725.2's own pair.
        Spec.it s "OrderTriggers records and replays a permutation" $ do
          let p = Prompt.OrderTriggers decider S.alice orderEntries
              answer = [1, 0] :: [Natural.Natural]
          Spec.assertEqWith s "round-trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
        Spec.it s "defaultAnswer keeps the canonical order" $
          Spec.assertEqWith
            s
            "identity permutation"
            (Replay.defaultAnswer (Prompt.OrderTriggers decider S.alice orderEntries))
            [0, 1 :: Natural.Natural]
        -- CR 615.7's batch order. Discriminating against OrderTriggers, which
        -- carries the same [Natural] answer: a shared Response constructor would
        -- let one prompt's transcript entry decode as the other's.
        Spec.it s "OrderDamage records and replays a permutation" $ do
          let p = Prompt.OrderDamage decider S.alice orderDamageEvents
              answer = [1, 0] :: [Natural.Natural]
          Spec.assertEqWith s "round-trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
          Spec.assertEqWith
            s
            "an OrderTriggers transcript entry does not answer an OrderDamage"
            (Replay.decode p (Replay.encode (Prompt.OrderTriggers decider S.alice orderEntries) answer))
            Nothing
        Spec.it s "defaultAnswer keeps the batch's own order" $
          Spec.assertEqWith
            s
            "identity permutation"
            (Replay.defaultAnswer (Prompt.OrderDamage decider S.alice orderDamageEvents))
            [0, 1 :: Natural.Natural]
        -- CR 601.2h's payment order, the third prompt carrying a [Natural]
        -- permutation, and discriminating against the two above for their own
        -- stated reason: a shared Response constructor would let a trigger
        -- batch's transcript entry reorder a cost payment.
        Spec.it s "OrderCostComponents records and replays a permutation" $ do
          let p = Prompt.OrderCostComponents decider S.alice oid orderComponents
              answer = [1, 0] :: [Natural.Natural]
          Spec.assertEqWith s "round-trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
          Spec.assertEqWith
            s
            "an OrderTriggers transcript entry does not answer an OrderCostComponents"
            (Replay.decode p (Replay.encode (Prompt.OrderTriggers decider S.alice orderEntries) answer))
            Nothing
        Spec.it s "defaultAnswer keeps the cost's printed order" $
          Spec.assertEqWith
            s
            "identity permutation"
            (Replay.defaultAnswer (Prompt.OrderCostComponents decider S.alice oid orderComponents))
            [0, 1 :: Natural.Natural]
        Spec.it s "ChooseSacrifices records and replays a Set ObjectId" $ do
          let p = Prompt.ChooseSacrifices decider S.alice oid [oid, ObjectId.MkObjectId 8] 1
              answer = Set.singleton (ObjectId.MkObjectId 8)
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
        Spec.it s "defaultAnswer sacrifices the first `count` offered, in order" $
          Spec.assertEqWith
            s
            "the ascending prefix"
            (Replay.defaultAnswer (Prompt.ChooseSacrifices decider S.alice oid [oid, ObjectId.MkObjectId 8] 1))
            (Set.singleton oid)
        -- The payload is ChooseSacrifices' exactly, which is why the CROSS-decode
        -- is the assertion that matters: a shared Response constructor would let
        -- a transcript's sacrifice entry answer an exile prompt, and replay would
        -- then bin a card it should have exiled.
        Spec.it s "ChooseExilesFromGraveyard records and replays a Set ObjectId" $ do
          let p = Prompt.ChooseExilesFromGraveyard decider S.alice oid [oid, ObjectId.MkObjectId 8] 1
              answer = Set.singleton (ObjectId.MkObjectId 8)
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
          Spec.assertEqWith
            s
            "a ChooseSacrifices transcript entry does not answer it"
            (Replay.decode p (Replay.encode (Prompt.ChooseSacrifices decider S.alice oid [oid, ObjectId.MkObjectId 8] 1) answer))
            Nothing
        Spec.it s "defaultAnswer exiles the first `count` offered, in order" $
          Spec.assertEqWith
            s
            "the offered prefix"
            (Replay.defaultAnswer (Prompt.ChooseExilesFromGraveyard decider S.alice oid [oid, ObjectId.MkObjectId 8] 1))
            (Set.singleton oid)
        Spec.it s "ChooseCost records and replays a Cost" $ do
          let printed = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) []
              alternative = Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.SacrificeThis]
              p = Prompt.ChooseCost decider S.alice oid [printed, alternative]
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p alternative)) (Just alternative)
        Spec.it s "defaultAnswer takes the first offered cost (the printed one)" $ do
          let printed = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) []
              alternative = Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.SacrificeThis]
          Spec.assertEqWith
            s
            "the printed one"
            (Replay.defaultAnswer (Prompt.ChooseCost decider S.alice oid [printed, alternative]))
            printed
        -- #136 / CR 729.2: "Randomly determine which player goes first." The
        -- determination is randomness, not a choice, so the prompt carries NO
        -- Decider -- Shuffle is the only other such constructor. Recording it
        -- is what keeps a subgame replayable: the randomness lives in the
        -- interpreter, and the transcript carries what it rolled.
        Spec.it s "RandomFirstPlayer round-trips through the transcript" $ do
          let p = Prompt.RandomFirstPlayer (S.alice NonEmpty.:| [S.bob])
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p S.bob)) (Just S.bob)
        Spec.it s "a short transcript starts the head of the turn order" $
          Spec.assertEqWith
            s
            "the head"
            (Replay.defaultAnswer (Prompt.RandomFirstPlayer (S.alice NonEmpty.:| [S.bob])))
            S.alice
        -- CR 507.1 / 703.4h: the defending-player choice round-trips like every
        -- other prompt. NonEmpty because the action only runs when there is at
        -- least one candidate.
        Spec.it s "ChooseDefender round-trips through the transcript" $ do
          let p = Prompt.ChooseDefender decider S.alice (S.bob NonEmpty.:| [S.carol])
          Spec.assertEqWith s "carol round trips" (Replay.decode p (Replay.encode p S.carol)) (Just S.carol)
          -- Discriminating: both legs must round-trip. A decode that
          -- ignored the response and returned the head would pass the
          -- carol leg only by accident of which one was written first.
          Spec.assertEqWith s "bob round trips" (Replay.decode p (Replay.encode p S.bob)) (Just S.bob)
        Spec.it s "a first-player roll does not decode as a defender choice" $ do
          -- Discriminating: this is the assertion that fails if ChooseDefender
          -- reuses Response.DeterminedFirstPlayer instead of getting its own
          -- constructor. Both carry a PlayerId, so the types would not object.
          let p = Prompt.ChooseDefender decider S.alice (S.bob NonEmpty.:| [S.carol])
          Spec.assertEqWith s "mismatch" (Replay.decode p (Response.DeterminedFirstPlayer S.bob)) Nothing
        -- CR 601.2g: the mana-source choice round-trips like every other prompt.
        Spec.it s "ChooseManaSource round-trips through the transcript" $ do
          let a = ObjectId.MkObjectId 7
              b = ObjectId.MkObjectId 9
              p = Prompt.ChooseManaSource decider S.alice (a NonEmpty.:| [b])
          Spec.assertEqWith s "the second source round trips" (Replay.decode p (Replay.encode p (Just b))) (Just (Just b))
          -- Discriminating for the same reason ChooseDefender's pair is: a
          -- decode that ignored the response and returned the head would
          -- pass one leg by accident.
          Spec.assertEqWith s "the first source round trips" (Replay.decode p (Replay.encode p (Just a))) (Just (Just a))
          -- CR 118.3c: the refusal has to survive too, or a replayed transcript
          -- taps a source the player declined to.
          Spec.assertEqWith s "and so does declining" (Replay.decode p (Replay.encode p Nothing)) (Just Nothing)
        -- CR 605.3a: floating past what the cost needs is its own decision, and
        -- its own response constructor.
        Spec.it s "ChooseExtraManaSource round-trips, and does not decode as ChooseManaSource" $ do
          let a = ObjectId.MkObjectId 7
              p = Prompt.ChooseExtraManaSource decider S.alice (a NonEmpty.:| [ObjectId.MkObjectId 9])
          Spec.assertEqWith s "the extra source round trips" (Replay.decode p (Replay.encode p (Just a))) (Just (Just a))
          -- Discriminating: this fails if the two mana-source prompts share one
          -- response constructor, which would let a transcript answer the wrong
          -- window.
          Spec.assertEqWith s "mismatch" (Replay.decode p (Response.ChoseManaSource (Just a))) Nothing
        Spec.it s "a short transcript floats nothing once the cost is covered" $
          Spec.assertEqWith
            s
            "declines"
            (Replay.defaultAnswer (Prompt.ChooseExtraManaSource decider S.alice (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9])))
            Nothing
        Spec.it s "a discard choice does not decode as a mana-source choice" $ do
          -- Discriminating: this fails if ChooseManaSource reuses another
          -- ObjectId-shaped response instead of getting its own constructor.
          let p = Prompt.ChooseManaSource decider S.alice (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9])
          Spec.assertEqWith s "mismatch" (Replay.decode p (Response.ChoseDiscard [ObjectId.MkObjectId 7])) Nothing
        Spec.it s "a short transcript taps the first offered source" $
          Spec.assertEqWith
            s
            "the head"
            (Replay.defaultAnswer (Prompt.ChooseManaSource decider S.alice (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9])))
            (Just (ObjectId.MkObjectId 7))
        -- CR 605.3b / 105.4: the mana an any-colour source was tapped for is a
        -- decision, so it has to survive a transcript like any other.
        Spec.it s "ChooseManaYield round-trips through the transcript" $ do
          let black = oneMana Color.Black
              red = oneMana Color.Red
              p = Prompt.ChooseManaYield decider S.alice (ObjectId.MkObjectId 7) (black NonEmpty.:| [red])
          Spec.assertEqWith s "black round trips" (Replay.decode p (Replay.encode p black)) (Just black)
          -- Discriminating for the same reason the pair above is: a decode
          -- that returned the head would pass one leg by accident.
          Spec.assertEqWith s "red round trips" (Replay.decode p (Replay.encode p red)) (Just red)
        Spec.it s "a mana-source choice does not decode as a mana-yield choice" $ do
          -- Discriminating: this fails if ChooseManaYield reuses ChoseManaSource
          -- rather than getting its own constructor.
          let p = Prompt.ChooseManaYield decider S.alice (ObjectId.MkObjectId 7) (oneMana Color.Black NonEmpty.:| [oneMana Color.Red])
          Spec.assertEqWith s "mismatch" (Replay.decode p (Response.ChoseManaSource (Just (ObjectId.MkObjectId 7)))) Nothing
        -- CR 701.34a: who was proliferated onto is a decision, so it has to
        -- survive a transcript like any other.
        Spec.it s "ChooseProliferate round-trips through the transcript" $ do
          let a = ObjectId.MkObjectId 7
              p = Prompt.ChooseProliferate decider S.alice [a] [S.bob]
              both = (Set.singleton a, Set.singleton S.bob)
              neither = (Set.empty, Set.empty)
          Spec.assertEqWith s "taking both round trips" (Replay.decode p (Replay.encode p both)) (Just both)
          -- Discriminating: CR 701.34a's "any number" includes none, and a
          -- decode that defaulted to the offered set would pass one leg.
          Spec.assertEqWith s "declining round trips" (Replay.decode p (Replay.encode p neither)) (Just neither)
        Spec.it s "a short transcript proliferates onto nothing" $
          Spec.assertEqWith
            s
            "declines"
            (Replay.defaultAnswer (Prompt.ChooseProliferate decider S.alice [ObjectId.MkObjectId 7] [S.bob]))
            (Set.empty, Set.empty)
        -- CR 701.22a: the ordered partition a scry chose is a decision, so it
        -- has to survive a transcript like any other.
        Spec.it s "ChooseScry round-trips through the transcript" $ do
          let a = ObjectId.MkObjectId 7
              b = ObjectId.MkObjectId 9
              p = Prompt.ChooseScry decider S.alice [a, b]
              split = ([a], [b])
              swapped = ([], [b, a])
          Spec.assertEqWith s "a split round trips" (Replay.decode p (Replay.encode p split)) (Just split)
          -- Discriminating twice over: a response carrying only the bottomed
          -- cards, or only a set of them, would pass the first leg and lose the
          -- reordering this one asks for.
          Spec.assertEqWith s "so does keeping both, reordered" (Replay.decode p (Replay.encode p swapped)) (Just swapped)
        Spec.it s "a scry choice does not decode as a discard choice" $ do
          -- Discriminating: this fails if ChooseScry reuses another
          -- ObjectId-list response instead of getting its own constructor.
          let p = Prompt.ChooseDiscard decider S.alice [ObjectId.MkObjectId 7] 1
          Spec.assertEqWith s "mismatch" (Replay.decode p (Response.ChoseScry ([], [ObjectId.MkObjectId 7]))) Nothing
        Spec.it s "a short transcript leaves the scried cards where they were" $
          Spec.assertEqWith
            s
            "everything back on top, in the order it was looked at"
            (Replay.defaultAnswer (Prompt.ChooseScry decider S.alice [ObjectId.MkObjectId 7, ObjectId.MkObjectId 9]))
            ([], [ObjectId.MkObjectId 7, ObjectId.MkObjectId 9])
        -- CR 704.5j: which legend its controller kept is a decision, so it has to
        -- survive a transcript like any other.
        Spec.it s "ChooseLegend round-trips through the transcript" $ do
          let a = ObjectId.MkObjectId 7
              b = ObjectId.MkObjectId 9
              p = Prompt.ChooseLegend decider S.alice (a NonEmpty.:| [b])
          Spec.assertEqWith s "keeping the second round trips" (Replay.decode p (Replay.encode p b)) (Just b)
          -- Discriminating: a decode that ignored the response and returned
          -- the head would pass one leg by accident.
          Spec.assertEqWith s "keeping the first round trips" (Replay.decode p (Replay.encode p a)) (Just a)
        Spec.it s "a legend choice does not decode as a Ring-bearer choice" $ do
          -- Discriminating: fails if ChooseLegend reuses ChoseRingBearer rather
          -- than getting its own ObjectId-shaped constructor. The mana-source
          -- response no longer carries a bare ObjectId, so it can no longer be
          -- the foil here.
          let p = Prompt.ChooseLegend decider S.alice (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9])
          Spec.assertEqWith s "mismatch" (Replay.decode p (Response.ChoseRingBearer (ObjectId.MkObjectId 7))) Nothing
        -- CR 603.7c: which of several minted tokens "it" names is a decision, so
        -- it has to survive a transcript like any other.
        Spec.it s "ChooseBoundToken round-trips through the transcript" $ do
          let a = ObjectId.MkObjectId 7
              b = ObjectId.MkObjectId 9
              p = Prompt.ChooseBoundToken decider S.alice oid (a NonEmpty.:| [b])
          Spec.assertEqWith s "binding the second round trips" (Replay.decode p (Replay.encode p b)) (Just b)
          -- Discriminating: a decode that ignored the response and returned
          -- the head would pass one leg by accident.
          Spec.assertEqWith s "binding the first round trips" (Replay.decode p (Replay.encode p a)) (Just a)
        Spec.it s "a bound-token choice does not decode as a legend choice" $ do
          -- Discriminating: fails if ChooseBoundToken reuses ChoseLegend rather
          -- than getting its own ObjectId-shaped constructor.
          let p = Prompt.ChooseBoundToken decider S.alice oid (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9])
          Spec.assertEqWith s "mismatch" (Replay.decode p (Response.ChoseLegend (ObjectId.MkObjectId 7))) Nothing
        Spec.it s "a short transcript binds the first token minted" $
          -- CR 603.7c: every minted token is a legal referent, so the head is
          -- legal -- and it is what the engine bound before the choice existed.
          Spec.assertEqWith
            s
            "the head"
            (Replay.defaultAnswer (Prompt.ChooseBoundToken decider S.alice oid (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9])))
            (ObjectId.MkObjectId 7)
        -- CR 701.54a: which creature a tempted player made their Ring-bearer is a
        -- decision, so it has to survive a transcript like any other.
        Spec.it s "ChooseRingBearer round-trips through the transcript" $ do
          let a = ObjectId.MkObjectId 7
              b = ObjectId.MkObjectId 9
              p = Prompt.ChooseRingBearer decider S.alice (a NonEmpty.:| [b])
          Spec.assertEqWith s "designating the second round trips" (Replay.decode p (Replay.encode p b)) (Just b)
          -- Discriminating: a decode that ignored the response and returned the
          -- head would pass one leg by accident.
          Spec.assertEqWith s "designating the first round trips" (Replay.decode p (Replay.encode p a)) (Just a)
        Spec.it s "a Ring-bearer choice does not decode as a legend choice" $ do
          -- Discriminating: fails if ChooseRingBearer reuses ChoseLegend rather
          -- than getting its own ObjectId-shaped constructor. The two are the same
          -- SHAPE -- a Prompt over one ObjectId, answered from a NonEmpty of them --
          -- so nothing but a distinct constructor keeps a transcript of one from
          -- replaying as the other.
          let p = Prompt.ChooseRingBearer decider S.alice (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9])
          Spec.assertEqWith s "mismatch" (Replay.decode p (Response.ChoseLegend (ObjectId.MkObjectId 7))) Nothing
        -- CR 508.1b: what each attacking creature was announced as attacking is
        -- a decision, so it has to survive a transcript like any other -- and
        -- both arms of AttackTarget have to survive it, since a transcript that
        -- collapsed them would replay an attack on a planeswalker as an attack
        -- on its controller.
        Spec.it s "ChooseAttackTarget round-trips through the transcript" $ do
          let atBob = AttackTarget.OfPlayer S.bob
              atJace = AttackTarget.OfPlaneswalker (ObjectId.MkObjectId 9)
              p = Prompt.ChooseAttackTarget decider S.alice oid (atBob NonEmpty.:| [atJace])
          Spec.assertEqWith s "the planeswalker round trips" (Replay.decode p (Replay.encode p atJace)) (Just atJace)
          -- Discriminating: a decode that ignored the response and returned the
          -- head would pass one leg by accident.
          Spec.assertEqWith s "the defending player round trips" (Replay.decode p (Replay.encode p atBob)) (Just atBob)
        Spec.it s "an attack-target choice does not decode as a defending-player choice" $ do
          -- Discriminating: fails if ChooseAttackTarget reuses ChoseDefender
          -- rather than getting its own constructor.
          let p = Prompt.ChooseAttackTarget decider S.alice oid (AttackTarget.OfPlayer S.bob NonEmpty.:| [])
          Spec.assertEqWith s "mismatch" (Replay.decode p (Response.ChoseDefender S.bob)) Nothing
        Spec.it s "a short transcript attacks the defending player" $
          -- CR 508.1b: the defending player heads the list and is always a legal
          -- thing to attack -- and it is what every attack announced before
          -- planeswalkers existed named.
          Spec.assertEqWith
            s
            "the head"
            (Replay.defaultAnswer (Prompt.ChooseAttackTarget decider S.alice oid (AttackTarget.OfPlayer S.bob NonEmpty.:| [AttackTarget.OfPlaneswalker (ObjectId.MkObjectId 9)])))
            (AttackTarget.OfPlayer S.bob)
        -- CR 118.13a: which way a Phyrexian mana symbol was announced to be
        -- paid is a decision made as the spell is proposed, so it has to
        -- survive a transcript like any other.
        Spec.it s "AnnouncePhyrexianPayment round-trips through the transcript" $ do
          let p =
                Prompt.AnnouncePhyrexianPayment
                  decider
                  S.alice
                  oid
                  Color.Green
                  (PhyrexianPayment.PaysMana NonEmpty.:| [PhyrexianPayment.PaysLife])
          Spec.assertEqWith
            s
            "the life route round trips"
            (Replay.decode p (Replay.encode p PhyrexianPayment.PaysLife))
            (Just PhyrexianPayment.PaysLife)
          -- Discriminating for the same reason the pairs above are: a
          -- decode that ignored the response and returned the head would
          -- pass one leg by accident.
          Spec.assertEqWith
            s
            "the mana route round trips"
            (Replay.decode p (Replay.encode p PhyrexianPayment.PaysMana))
            (Just PhyrexianPayment.PaysMana)
        Spec.it s "an optional decision does not decode as a Phyrexian announcement" $ do
          -- Discriminating: fails if AnnouncePhyrexianPayment reuses another
          -- two-valued response (ChoseOptional, Conceded, DeclaredMulligan)
          -- rather than getting its own constructor.
          let p =
                Prompt.AnnouncePhyrexianPayment
                  decider
                  S.alice
                  oid
                  Color.Green
                  (PhyrexianPayment.PaysMana NonEmpty.:| [PhyrexianPayment.PaysLife])
          Spec.assertEqWith s "mismatch" (Replay.decode p (Response.ChoseOptional OptionalDecision.Exercises)) Nothing
        -- CR 118.13a again, for CR 107.4e's monocolored hybrid: which half a
        -- {2/R} was announced as decides how much mana the spell cost, so it
        -- has to survive a transcript.
        Spec.it s "AnnounceHybridPayment round-trips through the transcript" $ do
          let p =
                Prompt.AnnounceHybridPayment
                  decider
                  S.alice
                  oid
                  (ManaType.Colored Color.Red)
                  (HybridPayment.PaysTyped NonEmpty.:| [HybridPayment.PaysGeneric])
          Spec.assertEqWith
            s
            "the generic route round trips"
            (Replay.decode p (Replay.encode p HybridPayment.PaysGeneric))
            (Just HybridPayment.PaysGeneric)
          -- Discriminating for the same reason the pairs above are: a decode
          -- that ignored the response and returned the head would pass one leg
          -- by accident.
          Spec.assertEqWith
            s
            "the coloured route round trips"
            (Replay.decode p (Replay.encode p HybridPayment.PaysTyped))
            (Just HybridPayment.PaysTyped)
        -- CR 118.13a once more, for CR 107.4e's colour/colour hybrid: which half
        -- a {G/U} was announced as decides WHICH mana the spell spent, and so
        -- what floats afterwards, which has to survive a transcript.
        Spec.it s "AnnounceHybridHalf round-trips through the transcript" $ do
          let p =
                Prompt.AnnounceHybridHalf
                  decider
                  S.alice
                  oid
                  (ManaSymbol.Hybrid (ManaType.Colored Color.Green) (ManaType.Colored Color.Blue))
                  (ManaType.Colored Color.Green NonEmpty.:| [ManaType.Colored Color.Blue])
          Spec.assertEqWith
            s
            "the green half round trips"
            (Replay.decode p (Replay.encode p (ManaType.Colored Color.Green)))
            (Just (ManaType.Colored Color.Green))
          -- Discriminating for the same reason the pairs above are: a decode
          -- that ignored the response and returned the head would pass one leg
          -- by accident.
          Spec.assertEqWith
            s
            "the blue half round trips"
            (Replay.decode p (Replay.encode p (ManaType.Colored Color.Blue)))
            (Just (ManaType.Colored Color.Blue))
          -- Discriminating: fails if the colour/colour announcement rides the
          -- monocolored one's response rather than getting its own constructor,
          -- which is the nearest miss -- both are CR 601.2b announcements about
          -- a hybrid symbol, asked of the same player about the same object.
          Spec.assertEqWith
            s
            "and a monocolored announcement is not one of these"
            ( Replay.decode
                p
                ( Replay.encode
                    ( Prompt.AnnounceHybridPayment
                        decider
                        S.alice
                        oid
                        (ManaType.Colored Color.Red)
                        (HybridPayment.PaysTyped NonEmpty.:| [HybridPayment.PaysGeneric])
                    )
                    HybridPayment.PaysTyped
                )
            )
            Nothing
        Spec.it s "a short transcript announces the first offered hybrid half" $
          -- Every offered half is payable (the prompt is raised only with two
          -- payable halves), so the head is a legal answer.
          Spec.assertEqWith
            s
            "the head"
            ( Replay.defaultAnswer
                ( Prompt.AnnounceHybridHalf
                    decider
                    S.alice
                    oid
                    (ManaSymbol.Hybrid (ManaType.Colored Color.Green) (ManaType.Colored Color.Blue))
                    (ManaType.Colored Color.Blue NonEmpty.:| [ManaType.Colored Color.Green])
                )
            )
            (ManaType.Colored Color.Blue)
        -- CR 118.7e: which half of a hybrid REDUCTION its payer took decides how
        -- much came off the cost, so it has to survive a transcript too.
        Spec.it s "ChooseReductionHalf round-trips through the transcript" $ do
          let p =
                Prompt.ChooseReductionHalf
                  decider
                  S.alice
                  oid
                  (ManaSymbol.MonocoloredHybrid (ManaType.Colored Color.Black))
                  (ManaSymbol.OfType (ManaType.Colored Color.Black) NonEmpty.:| [ManaSymbol.Generic 2])
          Spec.assertEqWith
            s
            "the generic half round trips"
            (Replay.decode p (Replay.encode p (ManaSymbol.Generic 2)))
            (Just (ManaSymbol.Generic 2))
          Spec.assertEqWith
            s
            "the coloured half round trips"
            (Replay.decode p (Replay.encode p (ManaSymbol.OfType (ManaType.Colored Color.Black))))
            (Just (ManaSymbol.OfType (ManaType.Colored Color.Black)))
        -- Discriminating: fails if CR 118.7e's choice rides CR 601.2b's
        -- announcement response rather than getting its own constructor, which
        -- is the nearest miss -- both are a two-way question about a hybrid
        -- symbol, asked of the same player about the same object.
        Spec.it s "a hybrid announcement does not decode as a reduction half" $ do
          let p =
                Prompt.ChooseReductionHalf
                  decider
                  S.alice
                  oid
                  (ManaSymbol.MonocoloredHybrid (ManaType.Colored Color.Black))
                  (ManaSymbol.OfType (ManaType.Colored Color.Black) NonEmpty.:| [ManaSymbol.Generic 2])
              announcement =
                Prompt.AnnounceHybridPayment
                  decider
                  S.alice
                  oid
                  (ManaType.Colored Color.Red)
                  (HybridPayment.PaysTyped NonEmpty.:| [HybridPayment.PaysGeneric])
          Spec.assertEqWith s "mismatch" (Replay.decode p (Replay.encode announcement HybridPayment.PaysGeneric)) Nothing
          Spec.assertEqWith s "nor the other way round" (Replay.decode announcement (Replay.encode p (ManaSymbol.Generic 2))) Nothing
        Spec.it s "a Phyrexian announcement does not decode as a hybrid one" $ do
          -- Discriminating: fails if AnnounceHybridPayment reuses another
          -- two-valued response rather than getting its own constructor -- and
          -- AnnouncedPhyrexianPayment is the nearest miss, the other CR 118.13a
          -- announcement, recorded by the other arm of the same function.
          let p =
                Prompt.AnnounceHybridPayment
                  decider
                  S.alice
                  oid
                  (ManaType.Colored Color.Red)
                  (HybridPayment.PaysTyped NonEmpty.:| [HybridPayment.PaysGeneric])
              phyrexian =
                Prompt.AnnouncePhyrexianPayment
                  decider
                  S.alice
                  oid
                  Color.Green
                  (PhyrexianPayment.PaysMana NonEmpty.:| [PhyrexianPayment.PaysLife])
          Spec.assertEqWith s "mismatch" (Replay.decode p (Replay.encode phyrexian PhyrexianPayment.PaysMana)) Nothing
          Spec.assertEqWith s "nor the other way round" (Replay.decode phyrexian (Replay.encode p HybridPayment.PaysTyped)) Nothing
        Spec.it s "a short transcript announces the first offered hybrid route" $
          -- Every offered route is payable (the prompt is raised only with two
          -- payable routes), so the head is a legal answer.
          Spec.assertEqWith
            s
            "the head"
            ( Replay.defaultAnswer
                ( Prompt.AnnounceHybridPayment
                    decider
                    S.alice
                    oid
                    (ManaType.Colored Color.Red)
                    (HybridPayment.PaysGeneric NonEmpty.:| [HybridPayment.PaysTyped])
                )
            )
            HybridPayment.PaysGeneric
        Spec.it s "a short transcript announces the first offered Phyrexian route" $
          -- Every offered route is payable (the prompt is raised only with two
          -- payable routes), so the head is a legal answer.
          Spec.assertEqWith
            s
            "the head"
            ( Replay.defaultAnswer
                ( Prompt.AnnouncePhyrexianPayment
                    decider
                    S.alice
                    oid
                    Color.Green
                    (PhyrexianPayment.PaysLife NonEmpty.:| [PhyrexianPayment.PaysMana])
                )
            )
            PhyrexianPayment.PaysLife
        Spec.it s "a short transcript produces the first offered mana yield" $
          Spec.assertEqWith
            s
            "the head"
            (Replay.defaultAnswer (Prompt.ChooseManaYield decider S.alice (ObjectId.MkObjectId 7) (oneMana Color.Black NonEmpty.:| [oneMana Color.Red])))
            (oneMana Color.Black)
        Spec.it s "a short transcript defends with the first candidate" $
          -- Discriminating against a defaultAnswer that returned the active
          -- player, or a candidate not on the offered list: the first candidate
          -- is always legal, since the prompt is only asked with candidates.
          Spec.assertEqWith
            s
            "the head"
            (Replay.defaultAnswer (Prompt.ChooseDefender decider S.alice (S.bob NonEmpty.:| [S.carol])))
            S.bob
        -- #133: the concede channel round-trips like every other prompt. Note
        -- the prompt takes a PlayerId and NO Decider (CR 723.6).
        Spec.it s "Concede round-trips both ways" $ do
          let p = Prompt.Concede S.alice
          Spec.assertEqWith s "concedes" (Replay.decode p (Replay.encode p Concession.Concedes)) (Just Concession.Concedes)
          Spec.assertEqWith s "continues" (Replay.decode p (Replay.encode p Concession.Continues)) (Just Concession.Continues)
        Spec.it s "a short transcript defaults a Concede to Continues" $
          -- NOT "least eventful", unlike the arms around it: dropping a
          -- concession hands the win to the other player (CR 104.2a), which
          -- is why Replay.replay reports the desync.
          Spec.assertEqWith s "the game keeps running" (Replay.defaultAnswer (Prompt.Concede S.alice)) Concession.Continues

-- Concedes whenever asked, and otherwise takes the identity answer. Delegating
-- through a wildcard keeps this out of the -Werror exhaustiveness net.
concedeAnswer :: Prompt.Prompt r -> r
concedeAnswer p = case p of
  Prompt.Concede _ -> Concession.Concedes
  _ -> S.identityAnswer p

-- The starting state, the game program, and a transcript recorded with
-- playLandAnswer (whose choices differ from Replay's exhausted-transcript
-- fallback, keeping the assertions below honest: the transcript has to
-- actually carry the decisions).
recordedGame :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, Game.Type.Game Result.Result, GameState.GameState, [Response.Response])
recordedGame s registry = do
  matchup <- S.redRed (S.printingOf s registry)
  let start = Setup.emptyGame (fmap fst matchup)
      game = Engine.playFrom matchup
      ((_, recorded), transcript) = Replay.record S.playLandAnswer start game
  pure (start, game, recorded, transcript)

replaySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
replaySpec s registry =
  Spec.describe s "Replay" $ do
    Spec.it s "replaying a recorded game reproduces the final state" $ do
      (start, game, recorded, transcript) <- recordedGame s registry
      let ((_, replayed), desync) = Replay.replay transcript start game
      Spec.assertEqWith s "final states equal" replayed recorded
      Spec.assertEqWith s "and the transcript answered every prompt" desync Nothing
    Spec.it s "the transcript is what carries the decisions" $ do
      (start, game, recorded, _) <- recordedGame s registry
      let ((_, replayed), desync) = Replay.replay [] start game
      Spec.assertBool s (recorded /= replayed) "empty log diverges"
      -- The divergence is REPORTED rather than silent. An empty log runs out
      -- at the very first prompt, so the report names index 0.
      Spec.assertEqWith s "and says where it ran out" desync (Just (Desync.Exhausted 0))
    Spec.it s "a recorded goldfish also replays" $ do
      (start, game, _, _) <- recordedGame s registry
      let ((_, gf), gfLog) = Replay.record S.identityAnswer start game
          ((_, replayed), desync) = Replay.replay gfLog start game
      Spec.assertEqWith s "goldfish" replayed gf
      Spec.assertEqWith s "no desync" desync Nothing
    -- #144. Pawl.Engine.Replay.defaultAnswer is deliberately total, so a transcript
    -- that has drifted out of step with the prompts the engine actually asks
    -- does not crash -- it silently answers everything from the fallback and
    -- plays out a DIFFERENT game. For Prompt.Concede that fallback is
    -- Concession.Continues, so a dropped concession changes who WINS (CR
    -- 104.2a), not merely how a choice was filled. These pin the report that
    -- makes that visible: replay names the first prompt the transcript failed
    -- to answer, and nothing after it can be trusted.
    Spec.it s "a truncated transcript reports where it ran out" $ do
      (start, game, _, transcript) <- recordedGame s registry
      let (_, desync) = Replay.replay (List.init transcript) start game
      case desync of
        Just (Desync.Exhausted _) -> pure ()
        _ -> Spec.assertFailure s ("expected an Exhausted report, got " <> show desync)
    Spec.it s "a mismatched entry is reported, not silently defaulted" $ do
      (start, game, recorded, transcript) <- recordedGame s registry
      -- A response of the wrong SHAPE for the first prompt, which is the
      -- opening Prompt.Shuffle: Response.ChoseX answers Prompt.ChooseX and
      -- nothing else, so decode rejects it.
      let bogus = Response.ChoseX 0
          ((_, replayed), desync) = Replay.replay (bogus : transcript) start game
      Spec.assertEqWith s "reported at index 0, carrying the offending entry" desync (Just (Desync.Mismatched 0 bogus))
      Spec.assertBool s (recorded /= replayed) "and the replay is not the recorded game"
    -- The one that matters: a dropped concession replays to the OTHER winner.
    Spec.it s "#144 a concession lost to a desync silently flips the winner" $ do
      let base = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice}
          ((_, conceded), transcript) = Replay.record concedeAnswer base Engine.runStep
          -- One spurious entry ahead of the log is enough: decode rejects it,
          -- replay does not consume it, and every later prompt meets the same
          -- entry -- so the whole transcript is stranded behind it.
          ((_, drifted), desync) = Replay.replay (Response.ChoseX 0 : transcript) base Engine.runStep
      Spec.assertEqWith s "recorded: alice conceded, bob wins" (GameState.result conceded) (Just (Result.Won S.bob))
      Spec.assertEqWith s "replayed: the concession is gone" (GameState.result drifted) Nothing
      Spec.assertEqWith s "and the report says so" desync (Just (Desync.Mismatched 0 (Response.ChoseX 0)))
    Spec.it s "a ChooseTargets answer round-trips through the transcript" $ do
      let sets = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (1 :: Natural.Natural, Set.singleton (Recipient.ToPlayer S.bob))
          p = Prompt.ChooseTargets (Decider.MkDecider S.alice) S.alice (ObjectId.MkObjectId 0) sets
          answer = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.singleton (Recipient.ToPlayer S.bob))
      Spec.assertEqWith s "decode . encode = Just" (Replay.decode p (Replay.encode p answer)) (Just answer)
    -- CR 700.2b/603.3d: the mode chosen as Aether Channeler's ETB trigger is
    -- placed (Engine.placeOne prompts ChooseModes) records/replays exactly
    -- like a spell's ChooseModes -- a Response.ChoseModes round-trips through
    -- the DecisionLog byte-identically.
    Spec.it s "Aether Channeler's trigger ChooseModes records and replays a Seq ModeIndex" $ do
      acPrinting <- S.printingOf s registry "Aether Channeler"
      let (acId, gs) = S.addCreature acPrinting S.alice (Setup.emptyGame S.bothPlayers)
          decider = Decide.deciderFor S.alice gs
      case Face.triggeredAbilities (S.combinedFace acPrinting) of
        [ability] -> do
          let legal = Target.fillableModes Nothing acId Map.empty (TriggeredAbility.modal ability) gs
              p = Prompt.ChooseModes decider S.alice acId legal (ModeSelection.ChooseExactly 1)
              answer = Seq.singleton (ModeIndex.MkModeIndex 2)
          Spec.assertEqWith s "legal modes are 0 and 2 (bounce self-excluded)" legal (Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2])
          Spec.assertEqWith s "round trip" (Replay.decode p (Replay.encode p answer)) (Just answer)
        _ -> Spec.assertFailure s "Aether Channeler must have exactly one triggered ability"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replay" $ do
  replaySpec s registry
  combatReplaySpec s
