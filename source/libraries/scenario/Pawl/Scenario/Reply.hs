{-# LANGUAGE GADTs #-}

-- | Any prompt's answer as JSON, both ways: a scenario's generic @Answer@
-- decodes through it, and a recording encodes through it. An object is
-- written as a reference and a player as a seat label, which only a run can
-- resolve, so decoding yields 'Needs' rather than the answer itself.
module Pawl.Scenario.Reply where

import qualified Control.Monad as Monad
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Codec.CardName as Codec.CardName
import qualified Pawl.Codec.CastFromZone as Codec.CastFromZone
import qualified Pawl.Codec.CoinFace as Codec.CoinFace
import qualified Pawl.Codec.Color as Codec.Color
import qualified Pawl.Codec.CounterKind as Codec.CounterKind
import qualified Pawl.Codec.Facing as Codec.Facing
import qualified Pawl.Codec.Keyword as Codec.Keyword
import qualified Pawl.Codec.LibraryPosition as Codec.LibraryPosition
import qualified Pawl.Codec.ManaCost as Codec.ManaCost
import qualified Pawl.Codec.ManaSymbol as Codec.ManaSymbol
import qualified Pawl.Codec.ManaType as Codec.ManaType
import qualified Pawl.Codec.ManaUnit as Codec.ManaUnit
import qualified Pawl.Codec.OptionalDecision as Codec.OptionalDecision
import qualified Pawl.Codec.OutsideCard as Codec.OutsideCard
import qualified Pawl.Codec.PaymentDecision as Codec.PaymentDecision
import qualified Pawl.Codec.PrintingId as Codec.PrintingId
import qualified Pawl.Codec.Reference as Codec.Reference
import qualified Pawl.Codec.Reply as Codec.Reply
import qualified Pawl.Codec.SlotName as Codec.SlotName
import qualified Pawl.Codec.Subtype as Codec.Subtype
import qualified Pawl.Codec.Zone as Codec.Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BuybackDecision as BuybackDecision
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.CommandZoneDecision as CommandZoneDecision
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.ForageMode as ForageMode
import qualified Pawl.Types.GraveyardArrangement as GraveyardArrangement
import qualified Pawl.Types.HandActionIndex as HandActionIndex
import qualified Pawl.Types.HybridPayment as HybridPayment
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MutateSide as MutateSide
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Reference as Reference
import qualified Pawl.Types.Reply as R
import qualified Pawl.Types.RollAdjustment as RollAdjustment
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.SearchPlace as SearchPlace
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TimeTravelChoice as TimeTravelChoice

-- | An answer still waiting on the run to name its objects and players.
data Needs a
  = Done a
  | Failed Text.Text
  | NeedObject Reference.Reference (ObjectId.ObjectId -> Needs a)
  | NeedPlayer Label.Label (PlayerId.PlayerId -> Needs a)

instance Functor Needs where
  fmap f needs = case needs of
    Done a -> Done (f a)
    Failed e -> Failed e
    NeedObject r k -> NeedObject r (fmap f . k)
    NeedPlayer l k -> NeedPlayer l (fmap f . k)

instance Applicative Needs where
  pure = Done
  nf <*> na = nf >>= \f -> fmap f na

instance Monad Needs where
  needs >>= f = case needs of
    Done a -> f a
    Failed e -> Failed e
    NeedObject r k -> NeedObject r (k Monad.>=> f)
    NeedPlayer l k -> NeedPlayer l (k Monad.>=> f)

-- | How a recording names what an answer refers to.
data Namer = MkNamer
  { nameObject :: ObjectId.ObjectId -> Reference.Reference,
    namePlayer :: PlayerId.PlayerId -> Label.Label
  }

data Shape a = MkShape
  { decode :: R.Reply -> Needs a,
    encode :: Namer -> a -> R.Reply
  }

fromEither :: Either Text.Text a -> Needs a
fromEither = either Failed Done

viaCodec :: Codec.Codec a -> Shape a
viaCodec codec =
  MkShape
    (fromEither . Codec.decode codec . Codec.Reply.toValue)
    (\_ -> Either.fromRight R.Null . Codec.Reply.fromValue . Codec.encode codec)

object :: Shape ObjectId.ObjectId
object =
  MkShape
    (decode reference Monad.>=> (`NeedObject` Done))
    (\namer -> encode reference namer . nameObject namer)

player :: Shape PlayerId.PlayerId
player =
  MkShape
    (decode text Monad.>=> (\t -> NeedPlayer (Label.MkLabel t) Done))
    (\namer -> encode text namer . Label.unwrap . namePlayer namer)

natural :: Shape Natural
natural = viaCodec Common.natural

text :: Shape Text.Text
text = viaCodec Common.text

reference :: Shape Reference.Reference
reference = viaCodec Codec.Reference.codec

wrapped :: (Natural -> a) -> (a -> Natural) -> Shape a
wrapped into outOf = MkShape (fmap into . decode natural) (\n -> encode natural n . outOf)

-- | A nullary enum spelled by its constructor's name.
named :: (Show a) => [a] -> Shape a
named options =
  MkShape
    ( \v -> do
        t <- decode text v
        case filter ((== Text.unpack t) . show) options of
          x : _ -> Done x
          [] -> Failed (Text.pack ("expected one of " <> show (fmap show options)))
    )
    (\namer -> encode text namer . Text.pack . show)

unsupported :: Text.Text -> Shape a
unsupported what = MkShape (const (Failed (Text.pack "no Answer for " <> what))) (\_ _ -> R.Null)

elements :: R.Reply -> Needs [R.Reply]
elements v = case v of
  R.Array xs -> Done xs
  _ -> Failed (Text.pack "expected an array")

list :: Shape a -> Shape [a]
list shape = MkShape (elements Monad.>=> traverse (decode shape)) (\namer -> R.Array . fmap (encode shape namer))

seqOf :: Shape a -> Shape (Seq.Seq a)
seqOf shape = MkShape (fmap Seq.fromList . decode (list shape)) (\namer -> encode (list shape) namer . Foldable.toList)

setOf :: (Ord a) => Shape a -> Shape (Set.Set a)
setOf shape = MkShape (fmap Set.fromList . decode (list shape)) (\namer -> encode (list shape) namer . Set.toList)

maybeOf :: Shape a -> Shape (Maybe a)
maybeOf shape =
  MkShape
    (\v -> if v == R.Null then Done Nothing else fmap Just (decode shape v))
    (maybe R.Null . encode shape)

pairOf :: Shape a -> Shape b -> Shape (a, b)
pairOf sa sb =
  MkShape
    ( elements
        Monad.>=> \xs -> case xs of
          [a, b] -> (,) <$> decode sa a <*> decode sb b
          _ -> Failed (Text.pack "expected a pair")
    )
    (\namer (a, b) -> R.Array [encode sa namer a, encode sb namer b])

tripleOf :: Shape a -> Shape b -> Shape c -> Shape (a, b, c)
tripleOf sa sb sc =
  MkShape
    ( elements
        Monad.>=> \xs -> case xs of
          [a, b, c] -> (,,) <$> decode sa a <*> decode sb b <*> decode sc c
          _ -> Failed (Text.pack "expected a triple")
    )
    (\namer (a, b, c) -> R.Array [encode sa namer a, encode sb namer b, encode sc namer c])

-- | A map as an array of [key, value] pairs, so a key may be an object.
mapOf :: (Ord k) => Shape k -> Shape v -> Shape (Map.Map k v)
mapOf sk sv =
  MkShape
    (fmap Map.fromList . decode (list (pairOf sk sv)))
    (\namer -> encode (list (pairOf sk sv)) namer . Map.toList)

recipient :: Shape Recipient.Recipient
recipient =
  MkShape
    ( elements
        Monad.>=> \xs -> case xs of
          [tag, x] -> do
            t <- decode text tag
            case Text.unpack t of
              "Creature" -> Recipient.ToCreature <$> decode object x
              "Planeswalker" -> Recipient.ToPlaneswalker <$> decode object x
              "Battle" -> Recipient.ToBattle <$> decode object x
              "Player" -> Recipient.ToPlayer <$> decode player x
              "Object" -> Recipient.ToObject <$> decode object x
              other -> Failed (Text.pack ("no recipient kind " <> other))
          _ -> Failed (Text.pack "expected [kind, recipient]")
    )
    ( \namer r -> case r of
        Recipient.ToCreature o -> tagged "Creature" (encode object namer o)
        Recipient.ToPlaneswalker o -> tagged "Planeswalker" (encode object namer o)
        Recipient.ToBattle o -> tagged "Battle" (encode object namer o)
        Recipient.ToPlayer p -> tagged "Player" (encode player namer p)
        Recipient.ToObject o -> tagged "Object" (encode object namer o)
        Recipient.ToPile _ -> R.Null
    )

attackTarget :: Shape AttackTarget.AttackTarget
attackTarget =
  MkShape
    ( elements
        Monad.>=> \xs -> case xs of
          [tag, x] -> do
            t <- decode text tag
            case Text.unpack t of
              "Player" -> AttackTarget.OfPlayer <$> decode player x
              "Planeswalker" -> AttackTarget.OfPlaneswalker <$> decode object x
              "Battle" -> AttackTarget.OfBattle <$> decode object x
              other -> Failed (Text.pack ("no attack target kind " <> other))
          _ -> Failed (Text.pack "expected [kind, target]")
    )
    ( \namer a -> case a of
        AttackTarget.OfPlayer p -> tagged "Player" (encode player namer p)
        AttackTarget.OfPlaneswalker o -> tagged "Planeswalker" (encode object namer o)
        AttackTarget.OfBattle o -> tagged "Battle" (encode object namer o)
    )

tagged :: String -> R.Reply -> R.Reply
tagged tag x = R.Array [R.Text (Text.pack tag), x]

searchPlace :: Shape SearchPlace.SearchPlace
searchPlace =
  MkShape
    ( \v -> case v of
        R.Null -> Done SearchPlace.OutsideTheGame
        _ -> SearchPlace.InZone <$> decode (viaCodec Codec.Zone.codec) v
    )
    ( \namer p -> case p of
        SearchPlace.InZone z -> encode (viaCodec Codec.Zone.codec) namer z
        SearchPlace.OutsideTheGame -> R.Null
    )

graveyardArrangement :: Shape GraveyardArrangement.GraveyardArrangement
graveyardArrangement =
  MkShape
    ( \v -> case v of
        R.Null -> Done GraveyardArrangement.AnyOrder
        _ -> GraveyardArrangement.InOrder <$> decode (list natural) v
    )
    ( \namer a -> case a of
        GraveyardArrangement.AnyOrder -> R.Null
        GraveyardArrangement.InOrder ns -> encode (list natural) namer ns
    )

counterKind :: Shape (CounterKind.CounterKind Keyword.Keyword)
counterKind = viaCodec (Codec.CounterKind.codec Codec.Keyword.codec)

subtype :: Shape Subtype.Subtype
subtype = viaCodec Codec.Subtype.codec

-- | Every prompt's answer shape; one arm each, so a new prompt is a compile error.
shapeOf :: Prompt.Prompt r -> Shape r
shapeOf prompt = case prompt of
  Prompt.ChooseAction {} -> unsupported (Text.pack "an action")
  Prompt.Concede {} -> named [Concession.Concedes, Concession.Continues]
  Prompt.Shuffle {} -> list object
  Prompt.RandomFirstPlayer {} -> player
  Prompt.RandomObject {} -> object
  Prompt.RandomCard {} -> viaCodec Codec.CardName.codec
  Prompt.ChooseConjuredCard {} -> viaCodec Codec.CardName.codec
  Prompt.RandomDepth {} -> natural
  Prompt.RandomPlayer {} -> player
  Prompt.RollDie {} -> natural
  Prompt.LookUpCard {} -> unsupported (Text.pack "a card")
  Prompt.ReferenceCards {} -> list (viaCodec Codec.CardName.codec)
  Prompt.ReferenceNames {} -> list (viaCodec Codec.CardName.codec)
  Prompt.ChooseDieResult {} -> natural
  Prompt.RerollDie {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.AdjustDieRoll {} -> maybeOf (pairOf natural (named [RollAdjustment.Increase, RollAdjustment.Decrease]))
  Prompt.ChooseRollModifier {} -> natural
  Prompt.CallCoin {} -> viaCodec Codec.CoinFace.codec
  Prompt.ChooseCoinResult {} -> viaCodec Codec.CoinFace.codec
  Prompt.ChooseDiscard {} -> list object
  Prompt.ChooseScry {} -> pairOf (list object) (list object)
  Prompt.ChooseSurveil {} -> pairOf (list object) (list object)
  Prompt.ChooseFateseal {} -> pairOf (list object) (list object)
  Prompt.ChooseExplore {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseDefender {} -> player
  Prompt.ChooseManaSource {} -> maybeOf object
  Prompt.ChooseExtraManaSource {} -> maybeOf object
  Prompt.ReverseManaAbilities {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseManaYield {} -> unsupported (Text.pack "a mana option")
  Prompt.ChooseManaToSpend {} -> viaCodec Codec.ManaUnit.codec
  Prompt.ChooseProliferate {} -> pairOf (setOf object) (setOf player)
  Prompt.ChooseRedistribution {} -> mapOf player player
  Prompt.ChooseRingBearer {} -> object
  Prompt.ChooseBolster {} -> object
  Prompt.ChooseAmass {} -> object
  Prompt.ChooseBlight {} -> object
  Prompt.ChooseBehold {} -> object
  Prompt.ChooseVote {} -> object
  Prompt.ChooseVoteWord {} -> viaCodec Codec.SlotName.codec
  Prompt.ChooseMovedCounter {} -> counterKind
  Prompt.ChooseMovedCounters {} -> mapOf counterKind natural
  Prompt.ChooseMovedCountersAtLeastOne {} -> mapOf counterKind natural
  Prompt.ChooseDistributedMovedCounters {} -> mapOf object (mapOf counterKind natural)
  Prompt.ChooseMovedCounterOrNone {} -> maybeOf counterKind
  Prompt.ChoosePaidEnergy {} -> natural
  Prompt.ChooseNumber {} -> natural
  Prompt.ChooseReadAheadChapter {} -> natural
  Prompt.ChooseDamageSource {} -> object
  Prompt.ChooseDelayedTriggerEvent {} -> natural
  Prompt.ChooseCardInGraveyard {} -> object
  Prompt.ChooseCardInHand {} -> object
  Prompt.ChooseCardFromAmong {} -> object
  Prompt.ChooseCardsFromAmong {} -> setOf object
  Prompt.ChooseDungeon {} -> viaCodec Codec.PrintingId.codec
  Prompt.ChooseCompanion {} -> maybeOf (viaCodec Codec.OutsideCard.codec)
  Prompt.ChooseFromOutsideTheGame {} -> unsupported (Text.pack "outside cards")
  Prompt.ChooseRoom {} -> wrapped RoomIndex.MkRoomIndex RoomIndex.unwrap
  Prompt.ChooseHalf {} -> viaCodec Codec.CardName.codec
  Prompt.ChooseLegend {} -> object
  Prompt.DeclareAttackers {} -> list object
  Prompt.ChooseAttackTarget {} -> attackTarget
  Prompt.ChooseExert {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseEnlist {} -> maybeOf object
  Prompt.ChooseEncode {} -> maybeOf object
  Prompt.DeclareBlockers {} -> mapOf object (setOf object)
  Prompt.AssignCombatDamage {} -> mapOf recipient natural
  Prompt.ChooseTargets {} -> mapOf (viaCodec Codec.SlotName.codec) (setOf recipient)
  Prompt.AnnounceTargets {} -> mapOf (viaCodec Codec.SlotName.codec) natural
  Prompt.ChooseLandTypeSwap {} -> pairOf subtype subtype
  Prompt.ChooseCreatureTypeSwap {} -> pairOf subtype subtype
  Prompt.ChooseBasicLandType {} -> subtype
  Prompt.ChooseCreatureType {} -> subtype
  Prompt.ChooseSearchZones {} -> setOf searchPlace
  Prompt.Search {} -> list object
  Prompt.CastWhileSearching {} -> maybeOf (tripleOf object (viaCodec Codec.CardName.codec) (viaCodec Codec.Facing.codec))
  Prompt.ChooseX {} -> natural
  Prompt.ChooseEntwine {} -> named [EntwineDecision.Declines, EntwineDecision.Entwines]
  Prompt.ChooseMutateSide {} -> named [MutateSide.Over, MutateSide.Under]
  Prompt.ChooseForage {} -> named [ForageMode.ExileCards, ForageMode.SacrificeFood]
  Prompt.ChooseLearn {} -> unsupported (Text.pack "a learn mode")
  Prompt.ChooseTimeTravel {} -> mapOf object (named [TimeTravelChoice.Add, TimeTravelChoice.Remove])
  Prompt.ChooseClash {} -> viaCodec Codec.LibraryPosition.codec
  Prompt.ChooseKicker {} -> wrapped KickerDecision.MkKickerDecision KickerDecision.unwrap
  Prompt.ChooseBuyback {} -> named [BuybackDecision.Declines, BuybackDecision.BuysBack]
  Prompt.ChooseSplice {} -> list object
  Prompt.ChooseAssistant {} -> maybeOf player
  Prompt.ChooseAssistAmount {} -> natural
  Prompt.ReturnCommander {} -> named [CommandZoneDecision.Leaves, CommandZoneDecision.Returns]
  Prompt.ChooseCommandZoneOfferFirst {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseLibraryEnd {} -> viaCodec Codec.LibraryPosition.codec
  Prompt.ArrangeLibraryArrivals {} -> list natural
  Prompt.ArrangeLibraryCards {} -> list natural
  Prompt.ArrangeGraveyardArrivals {} -> graveyardArrangement
  Prompt.ChooseModes {} -> seqOf (wrapped ModeIndex.MkModeIndex ModeIndex.unwrap)
  Prompt.ChooseCopyTarget {} -> maybeOf object
  Prompt.ChooseEntryOption {} -> natural
  Prompt.ChooseRiot {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseUnleash {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseTribute {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChoosePayLifeOnEntry {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseRevealOnEntry {} -> maybeOf object
  Prompt.ChooseColor {} -> viaCodec Codec.Color.codec
  Prompt.ChooseManaType {} -> viaCodec Codec.ManaType.codec
  Prompt.ChooseCardName {} -> viaCodec Codec.CardName.codec
  Prompt.ChooseOpponent {} -> player
  Prompt.ChooseProtector {} -> player
  Prompt.ChoosePlayer {} -> player
  Prompt.ChooseActivePlayer {} -> player
  Prompt.OrderTriggers {} -> list natural
  Prompt.OrderDamage {} -> list natural
  Prompt.AllocateDamage {} -> list natural
  Prompt.ChooseReplacement {} -> natural
  Prompt.ChooseDredge {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseRedirect {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseSacrifices {} -> setOf object
  Prompt.ChooseExilesFromGraveyard {} -> setOf object
  Prompt.ChooseMaterials {} -> setOf object
  Prompt.ChooseCollectEvidence {} -> setOf object
  Prompt.ChooseAnyNumberToSacrifice {} -> setOf object
  Prompt.ChooseAnyNumberToReveal {} -> setOf object
  Prompt.ChooseAnyNumberOfPermanents {} -> setOf object
  Prompt.ChooseAnyNumberToDiscard {} -> setOf object
  Prompt.ChoosePermanent {} -> object
  Prompt.ChooseTapsForTotalPower {} -> setOf object
  Prompt.ChooseTaps {} -> setOf object
  Prompt.ChooseReturns {} -> setOf object
  Prompt.ChooseCounterRemoval {} -> object
  Prompt.ChooseCounterRemovalAmong {} -> mapOf object natural
  Prompt.ChooseCounterRemovalAtLeast {} -> mapOf object natural
  Prompt.ChooseCounterRemovalUpTo {} -> mapOf object natural
  Prompt.ChooseCounterDistribution {} -> mapOf object natural
  Prompt.ChooseMixedCounterRemoval {} -> mapOf object (mapOf counterKind natural)
  Prompt.ChooseAttachment {} -> object
  Prompt.ChooseTurnUpAttachment {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseCost {} -> unsupported (Text.pack "a cost")
  Prompt.ChoosePlayPermission {} -> maybeOf (pairOf object (viaCodec Codec.CastFromZone.codec))
  Prompt.OrderCostComponents {} -> list natural
  Prompt.OrderCombatTolls {} -> list natural
  Prompt.OrderComponentCards {} -> list natural
  Prompt.OrderForEach {} -> list natural
  Prompt.ChooseLoopMembers {} -> setOf recipient
  Prompt.ChooseRepeat {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.OrderTimestamps {} -> list natural
  Prompt.OrderManaActivations {} -> list natural
  Prompt.DeclareMulligan {} -> named [MulliganDecision.Mulligan, MulliganDecision.Keep]
  Prompt.Bottom {} -> list object
  Prompt.MulliganAction {} -> maybeOf (pairOf object (wrapped HandActionIndex.MkHandActionIndex HandActionIndex.unwrap))
  Prompt.OpeningHandAction {} -> maybeOf (pairOf object (wrapped HandActionIndex.MkHandActionIndex HandActionIndex.unwrap))
  Prompt.ChooseOptional {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseClause {} -> maybeOf (wrapped ClauseIndex.MkClauseIndex ClauseIndex.unwrap)
  Prompt.OfferedCast {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseOfferedCastSpell {} -> pairOf object (viaCodec Codec.CardName.codec)
  Prompt.OfferedMiracleReveal {} -> viaCodec Codec.OptionalDecision.codec
  Prompt.ChooseToPay {} -> viaCodec Codec.PaymentDecision.codec
  Prompt.AnnouncePhyrexianPayment {} -> named [PhyrexianPayment.PaysMana, PhyrexianPayment.PaysLife]
  Prompt.AnnounceHybridPayment {} -> named [HybridPayment.PaysTyped, HybridPayment.PaysGeneric]
  Prompt.AnnounceHybridHalf {} -> viaCodec Codec.ManaType.codec
  Prompt.ChooseReductionHalf {} -> viaCodec Codec.ManaSymbol.codec
  Prompt.FlipCoin {} -> viaCodec Codec.CoinFace.codec
  Prompt.ChooseReducedCost {} -> viaCodec Codec.ManaCost.codec
