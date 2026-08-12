{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.Engine.Replay where

import Control.Applicative ((<|>))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.Asked as Asked
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CommandZoneDecision as CommandZoneDecision
import qualified Pawl.Types.Concession as Concession
import Pawl.Types.Desync (Desync)
import qualified Pawl.Types.Desync as Desync
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Program as Program
import Pawl.Types.Prompt (Prompt)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.Response (Response)
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TargetCount as TargetCount

-- Flatten an answer into the log. The GADT refines 'r' per branch, so each
-- constructor pairs with the response that carries its payload.
encode :: Prompt r -> r -> Response
encode p answer = case p of
  Prompt.Shuffle _ -> Response.Shuffled answer
  Prompt.RandomFirstPlayer _ -> Response.DeterminedFirstPlayer answer
  Prompt.ChooseAction {} -> Response.ChoseAction answer
  Prompt.Concede _ -> Response.Conceded answer
  Prompt.ChooseDiscard {} -> Response.ChoseDiscard answer
  Prompt.ChooseScry {} -> Response.ChoseScry answer
  Prompt.ChooseSurveil {} -> Response.ChoseSurveil answer
  Prompt.ChooseFateseal {} -> Response.ChoseFateseal answer
  Prompt.ChooseDefender {} -> Response.ChoseDefender answer
  Prompt.ChooseManaSource {} -> Response.ChoseManaSource answer
  Prompt.ChooseExtraManaSource {} -> Response.ChoseExtraManaSource answer
  Prompt.ChooseManaYield {} -> Response.ChoseManaYield answer
  Prompt.ChooseProliferate {} -> Response.ChoseProliferation answer
  Prompt.ChooseRingBearer {} -> Response.ChoseRingBearer answer
  Prompt.ChooseRoom {} -> Response.ChoseRoom answer
  Prompt.ChooseLegend {} -> Response.ChoseLegend answer
  Prompt.DeclareAttackers {} -> Response.DeclaredAttackers answer
  Prompt.ChooseAttackTarget {} -> Response.ChoseAttackTarget answer
  Prompt.DeclareBlockers {} -> Response.DeclaredBlockers answer
  Prompt.AssignCombatDamage {} -> Response.AssignedCombatDamage answer
  Prompt.ChooseTargets {} -> Response.ChoseTargets answer
  Prompt.AnnounceTargets {} -> Response.AnnouncedTargets answer
  Prompt.ChooseLandTypeSwap {} -> Response.ChoseLandTypeSwap answer
  Prompt.ChooseCreatureTypeSwap {} -> Response.ChoseCreatureTypeSwap answer
  Prompt.SearchLibrary {} -> Response.Searched answer
  Prompt.CastWhileSearching {} -> Response.CastWhileSearched answer
  Prompt.ChooseX {} -> Response.ChoseX answer
  Prompt.ChooseEntwine {} -> Response.AnnouncedEntwine answer
  Prompt.ChooseKicker {} -> Response.AnnouncedKicker answer
  Prompt.ReturnCommander {} -> Response.ReturnedCommander answer
  Prompt.ChooseLibraryEnd {} -> Response.ChoseLibraryEnd answer
  Prompt.ArrangeLibraryArrivals {} -> Response.ArrangedLibraryArrivals answer
  Prompt.ChooseModes {} -> Response.ChoseModes answer
  Prompt.ChooseCopyTarget {} -> Response.ChoseCopyTarget answer
  Prompt.ChooseEntryOption {} -> Response.ChoseEntryOption answer
  Prompt.ChooseRiot {} -> Response.ChoseRiot answer
  Prompt.ChooseUnleash {} -> Response.ChoseUnleash answer
  Prompt.ChoosePayLifeOnEntry {} -> Response.ChosePayLifeOnEntry answer
  Prompt.ChooseColor {} -> Response.ChoseColor answer
  Prompt.ChooseCardName {} -> Response.ChoseCardName answer
  Prompt.ChooseOpponent {} -> Response.ChoseOpponent answer
  Prompt.ChooseProtector {} -> Response.ChoseProtector answer
  Prompt.ChooseBasicLandType {} -> Response.ChoseBasicLandType answer
  Prompt.OrderTriggers {} -> Response.OrderedTriggers answer
  Prompt.OrderDamage {} -> Response.OrderedDamage answer
  Prompt.OrderCostComponents {} -> Response.OrderedCostComponents answer
  Prompt.ChooseReplacement {} -> Response.ChoseReplacement answer
  Prompt.ChooseBoundToken {} -> Response.ChoseBoundToken answer
  Prompt.ChooseSacrifices {} -> Response.ChoseSacrifices answer
  Prompt.ChooseExilesFromGraveyard {} -> Response.ChoseExilesFromGraveyard answer
  Prompt.ChooseAnyNumberToSacrifice {} -> Response.ChoseSacrifices answer
  Prompt.ChooseTapsForTotalPower {} -> Response.ChoseTaps answer
  Prompt.ChooseAttachment {} -> Response.ChoseAttachment answer
  Prompt.ChooseTurnUpAttachment {} -> Response.ChoseTurnUpAttachment answer
  Prompt.ChooseCost {} -> Response.ChoseCost answer
  Prompt.DeclareMulligan {} -> Response.DeclaredMulligan answer
  Prompt.Bottom {} -> Response.PutOnBottom answer
  Prompt.MulliganAction {} -> Response.TookMulliganAction answer
  Prompt.OpeningHandAction {} -> Response.TookOpeningHandAction answer
  Prompt.ChooseOptional {} -> Response.ChoseOptional answer
  Prompt.OfferedCast {} -> Response.ChoseOfferedCast answer
  Prompt.ChooseToPay {} -> Response.ChoseToPay answer
  Prompt.AnnouncePhyrexianPayment {} -> Response.AnnouncedPhyrexianPayment answer
  Prompt.AnnounceHybridPayment {} -> Response.AnnouncedHybridPayment answer
  Prompt.AnnounceHybridHalf {} -> Response.AnnouncedHybridHalf answer
  Prompt.ChooseReductionHalf {} -> Response.ChoseReductionHalf answer

-- The inverse of 'encode'. Nothing when the logged response does not match the
-- prompt the engine is actually asking (a stale or foreign transcript).
--
-- The non-matching branches are a wildcard rather than written out. Writing
-- every (Prompt, Response) pair explicitly is quadratic in the number of
-- prompt constructors, and the exhaustiveness that protects us is on Prompt --
-- the GADT that refines r -- which is still total. A new Response constructor
-- correctly decodes to Nothing.
decode :: Prompt r -> Response -> Maybe r
decode p response = case p of
  Prompt.Shuffle _ -> case response of
    Response.Shuffled ids -> Just ids
    _ -> Nothing
  Prompt.RandomFirstPlayer _ -> case response of
    Response.DeterminedFirstPlayer pid -> Just pid
    _ -> Nothing
  Prompt.ChooseAction {} -> case response of
    Response.ChoseAction action -> Just action
    _ -> Nothing
  Prompt.Concede _ -> case response of
    Response.Conceded concession -> Just concession
    _ -> Nothing
  Prompt.ChooseDiscard {} -> case response of
    Response.ChoseDiscard ids -> Just ids
    _ -> Nothing
  Prompt.ChooseScry {} -> case response of
    Response.ChoseScry split -> Just split
    _ -> Nothing
  Prompt.ChooseSurveil {} -> case response of
    Response.ChoseSurveil split -> Just split
    _ -> Nothing
  Prompt.ChooseFateseal {} -> case response of
    Response.ChoseFateseal split -> Just split
    _ -> Nothing
  Prompt.ChooseDefender {} -> case response of
    Response.ChoseDefender pid -> Just pid
    _ -> Nothing
  Prompt.ChooseManaSource {} -> case response of
    Response.ChoseManaSource oid -> Just oid
    _ -> Nothing
  Prompt.ChooseExtraManaSource {} -> case response of
    Response.ChoseExtraManaSource oid -> Just oid
    _ -> Nothing
  Prompt.ChooseManaYield {} -> case response of
    Response.ChoseManaYield mana -> Just mana
    _ -> Nothing
  Prompt.ChooseProliferate {} -> case response of
    Response.ChoseProliferation chosen -> Just chosen
    _ -> Nothing
  Prompt.ChooseRingBearer {} -> case response of
    Response.ChoseRingBearer oid -> Just oid
    _ -> Nothing
  Prompt.ChooseRoom {} -> case response of
    Response.ChoseRoom room -> Just room
    _ -> Nothing
  Prompt.ChooseLegend {} -> case response of
    Response.ChoseLegend oid -> Just oid
    _ -> Nothing
  Prompt.DeclareAttackers {} -> case response of
    Response.DeclaredAttackers ids -> Just ids
    _ -> Nothing
  Prompt.ChooseAttackTarget {} -> case response of
    Response.ChoseAttackTarget target -> Just target
    _ -> Nothing
  Prompt.DeclareBlockers {} -> case response of
    Response.DeclaredBlockers assignment -> Just assignment
    _ -> Nothing
  Prompt.AssignCombatDamage {} -> case response of
    Response.AssignedCombatDamage assignment -> Just assignment
    _ -> Nothing
  Prompt.ChooseTargets {} -> case response of
    Response.ChoseTargets chosen -> Just chosen
    _ -> Nothing
  Prompt.AnnounceTargets {} -> case response of
    Response.AnnouncedTargets announced -> Just announced
    _ -> Nothing
  Prompt.ChooseLandTypeSwap {} -> case response of
    Response.ChoseLandTypeSwap pair -> Just pair
    _ -> Nothing
  Prompt.ChooseCreatureTypeSwap {} -> case response of
    Response.ChoseCreatureTypeSwap pair -> Just pair
    _ -> Nothing
  Prompt.SearchLibrary {} -> case response of
    Response.Searched found -> Just found
    _ -> Nothing
  Prompt.CastWhileSearching {} -> case response of
    Response.CastWhileSearched found -> Just found
    _ -> Nothing
  Prompt.ChooseX {} -> case response of
    Response.ChoseX n -> Just n
    _ -> Nothing
  Prompt.ChooseModes {} -> case response of
    Response.ChoseModes modes -> Just modes
    _ -> Nothing
  Prompt.ChooseCopyTarget {} -> case response of
    Response.ChoseCopyTarget m -> Just m
    _ -> Nothing
  Prompt.ChooseEntryOption {} -> case response of
    Response.ChoseEntryOption n -> Just n
    _ -> Nothing
  Prompt.ChooseRiot {} -> case response of
    Response.ChoseRiot d -> Just d
    _ -> Nothing
  Prompt.ChooseUnleash {} -> case response of
    Response.ChoseUnleash d -> Just d
    _ -> Nothing
  Prompt.ChoosePayLifeOnEntry {} -> case response of
    Response.ChosePayLifeOnEntry d -> Just d
    _ -> Nothing
  Prompt.ChooseColor {} -> case response of
    Response.ChoseColor c -> Just c
    _ -> Nothing
  Prompt.ChooseCardName {} -> case response of
    Response.ChoseCardName n -> Just n
    _ -> Nothing
  Prompt.ChooseOpponent {} -> case response of
    Response.ChoseOpponent pid -> Just pid
    _ -> Nothing
  Prompt.ChooseProtector {} -> case response of
    Response.ChoseProtector pid -> Just pid
    _ -> Nothing
  Prompt.ChooseBasicLandType {} -> case response of
    Response.ChoseBasicLandType t -> Just t
    _ -> Nothing
  Prompt.OrderTriggers {} -> case response of
    Response.OrderedTriggers order -> Just order
    _ -> Nothing
  Prompt.OrderDamage {} -> case response of
    Response.OrderedDamage order -> Just order
    _ -> Nothing
  Prompt.OrderCostComponents {} -> case response of
    Response.OrderedCostComponents order -> Just order
    _ -> Nothing
  Prompt.ChooseReplacement {} -> case response of
    Response.ChoseReplacement n -> Just n
    _ -> Nothing
  Prompt.ChooseBoundToken {} -> case response of
    Response.ChoseBoundToken oid -> Just oid
    _ -> Nothing
  Prompt.ChooseSacrifices {} -> case response of
    Response.ChoseSacrifices ids -> Just ids
    _ -> Nothing
  Prompt.ChooseExilesFromGraveyard {} -> case response of
    Response.ChoseExilesFromGraveyard ids -> Just ids
    _ -> Nothing
  Prompt.ChooseAnyNumberToSacrifice {} -> case response of
    Response.ChoseSacrifices ids -> Just ids
    _ -> Nothing
  Prompt.ChooseTapsForTotalPower {} -> case response of
    Response.ChoseTaps ids -> Just ids
    _ -> Nothing
  Prompt.ChooseAttachment {} -> case response of
    Response.ChoseAttachment oid -> Just oid
    _ -> Nothing
  Prompt.ChooseTurnUpAttachment {} -> case response of
    Response.ChoseTurnUpAttachment d -> Just d
    _ -> Nothing
  Prompt.ChooseCost {} -> case response of
    Response.ChoseCost cost -> Just cost
    _ -> Nothing
  Prompt.DeclareMulligan {} -> case response of
    Response.DeclaredMulligan decision -> Just decision
    _ -> Nothing
  Prompt.Bottom {} -> case response of
    Response.PutOnBottom ids -> Just ids
    _ -> Nothing
  Prompt.MulliganAction {} -> case response of
    Response.TookMulliganAction moid -> Just moid
    _ -> Nothing
  Prompt.OpeningHandAction {} -> case response of
    Response.TookOpeningHandAction moid -> Just moid
    _ -> Nothing
  Prompt.ChooseOptional {} -> case response of
    Response.ChoseOptional decision -> Just decision
    _ -> Nothing
  Prompt.OfferedCast {} -> case response of
    Response.ChoseOfferedCast decision -> Just decision
    _ -> Nothing
  Prompt.ChooseToPay {} -> case response of
    Response.ChoseToPay decision -> Just decision
    _ -> Nothing
  Prompt.AnnouncePhyrexianPayment {} -> case response of
    Response.AnnouncedPhyrexianPayment way -> Just way
    _ -> Nothing
  Prompt.AnnounceHybridPayment {} -> case response of
    Response.AnnouncedHybridPayment way -> Just way
    _ -> Nothing
  Prompt.AnnounceHybridHalf {} -> case response of
    Response.AnnouncedHybridHalf half -> Just half
    _ -> Nothing
  Prompt.ChooseReductionHalf {} -> case response of
    Response.ChoseReductionHalf half -> Just half
    _ -> Nothing
  Prompt.ChooseEntwine {} -> case response of
    Response.AnnouncedEntwine decision -> Just decision
    _ -> Nothing
  Prompt.ChooseKicker {} -> case response of
    Response.AnnouncedKicker decision -> Just decision
    _ -> Nothing
  Prompt.ReturnCommander {} -> case response of
    Response.ReturnedCommander decision -> Just decision
    _ -> Nothing
  Prompt.ChooseLibraryEnd {} -> case response of
    Response.ChoseLibraryEnd position -> Just position
    _ -> Nothing
  Prompt.ArrangeLibraryArrivals {} -> case response of
    Response.ArrangedLibraryArrivals order -> Just order
    _ -> Nothing

-- The answer used when the transcript is exhausted or does not match. Keeping
-- this total is what lets 'replay' avoid a partial escape: an over-short log
-- degrades into a deterministic default rather than crashing.
--
-- Every arm below is a legal answer but one, and most are the LEAST EVENTFUL
-- one -- but "least eventful" is a statement about the choice, never a promise
-- that the game plays out the same. 'replay' therefore reports the first prompt
-- that reached this function (Pawl.Types.Desync); a caller that ignores the
-- report is reading a different game's final state. Each arm below notes only
-- why its answer is legal, and ChooseCardName says why it deliberately is not.
defaultAnswer :: Prompt r -> r
defaultAnswer p = case p of
  Prompt.Shuffle ids -> ids
  -- CR 729.2: the head of the turn order is always a legal starting player.
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.ChooseAction _ _ actions -> case actions of
    h : _ -> h
    [] -> Action.Pass
  -- CR 104.3a: not conceding is always legal. NOT "least eventful", unlike the
  -- arms around it: this one drops a departure, and CR 104.2a then hands the
  -- win to the other player, so a transcript whose Concede answer is lost
  -- replays to the OTHER winner. That is why 'replay' reports the desync.
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n ids
  -- CR 701.22a: "any number" includes none, so leaving every looked-at card on
  -- top in the order it was already in is legal -- and it is the one answer that
  -- leaves the library exactly as it was.
  Prompt.ChooseScry _ _ looked -> ([], looked)
  -- CR 701.25a's "any number" reaches none too, so putting nothing into the
  -- graveyard and keeping the whole look on top in the order it was found leaves
  -- the board exactly as it was -- the least eventful legal answer, and the one
  -- that moves no card between zones.
  Prompt.ChooseSurveil _ _ looked -> ([], looked)
  -- ChooseScry's answer over the opponent's library, for the same reason.
  Prompt.ChooseFateseal _ _ _ looked -> ([], looked)
  -- CR 507.1: the prompt is only asked with candidates, so the head is legal.
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
  -- Any candidate pays, and the cost is still short, so taking one is the least
  -- eventful answer that can still pay: CR 118.3c's refusal would fail the
  -- payment and reverse whatever proposed it.
  Prompt.ChooseManaSource _ _ candidates -> Just (NonEmpty.head candidates)
  -- The cost is already covered, so floating more is the eventful answer and
  -- closing the window is the quiet one.
  Prompt.ChooseExtraManaSource {} -> Nothing
  -- Every offered yield is producible: tapForMana only offers what the source
  -- can make.
  Prompt.ChooseManaYield _ _ _ candidates -> NonEmpty.head candidates
  -- CR 701.34a: any number includes none, so declining is always legal.
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  -- CR 701.54a: the prompt is only raised with two or more creatures the player
  -- controls, and every one of them is a legal Ring-bearer.
  Prompt.ChooseRingBearer _ _ candidates -> NonEmpty.head candidates
  -- CR 309.5a: the prompt is only raised where two or more arrows leave the
  -- room, and every one of them is a room the marker may move into.
  Prompt.ChooseRoom _ _ _ candidates -> NonEmpty.head candidates
  -- CR 704.5j: every candidate is a legal thing to keep.
  Prompt.ChooseLegend _ _ candidates -> NonEmpty.head candidates
  -- Declining to ATTACK is not always legal -- a CR 508.1d requirement (Curse
  -- of the Nightly Hunt) can make the empty declaration the one illegal answer.
  -- Still total: Combat.declareAttackers repairs an illegal declaration to
  -- Combat.forcedAttackDeclaration rather than dropping it.
  Prompt.DeclareAttackers {} -> []
  -- CR 508.1b: the head is the defending player (Combat.attackTargets orders it
  -- first), always a legal thing to attack. The same value
  -- Combat.announceAttackTarget degrades an out-of-list answer to, which the
  -- two must agree on: neither path can observe the other.
  Prompt.ChooseAttackTarget _ _ _ options -> NonEmpty.head options
  -- Declining to BLOCK is not always legal either -- a CR 509.1c requirement
  -- (Lure) can make the empty declaration illegal -- and stays total the same
  -- way, through Combat.forcedBlockDeclaration.
  Prompt.DeclareBlockers {} -> Map.empty
  -- Must be a WELL FORMED division (Damage.wellFormedAssignment), or the attacker
  -- deals nothing. All power onto the first blocker totals power, and clears CR
  -- 702.19b's gates by never spilling past a blocker at all.
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockers = filter isCreatureRecipient (Map.keys thresholds)
        isCreatureRecipient r = case r of
          Recipient.ToCreature _ -> True
          Recipient.ToPlaneswalker _ -> False
          Recipient.ToBattle _ -> False
          Recipient.ToPlayer _ -> False
          Recipient.ToObject _ -> False
     in case blockers of
          r : _ -> Map.singleton r n
          [] -> Map.empty
  -- As many legal recipients per slot as the announced count asks for, chosen
  -- deterministically (the smallest ones). A slot with too few candidates is
  -- underfilled -- casting rejects that answer.
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(n, rs) -> Set.fromList (take (Natural.toIntSaturating n) (Set.toAscList rs))) sets
  -- CR 601.2c: announce as MANY as the board allows, matching the arm above --
  -- announcing fewer is equally legal, and a default that declined would leave
  -- every "up to N" card's effect unexercised by the specs that take this answer.
  Prompt.AnnounceTargets _ _ _ offers -> fmap (\(count, rs) -> min (TargetCount.most count) (Natural.length rs)) offers
  -- A canonical identity swap: Mountain -> Mountain changes nothing.
  Prompt.ChooseLandTypeSwap {} -> (Subtype.Mountain, Subtype.Mountain)
  -- The same identity for CR 612.2's creature-type half. Frog is a creature
  -- type (CR 205.3m) and nothing in the pool forbids it.
  --
  -- Not implemented: neither swap prompt's stated restrictions are checked
  -- against the answer that comes back (#641).
  Prompt.ChooseCreatureTypeSwap {} -> (Subtype.Frog, Subtype.Frog)
  -- CR 701.23b: failing to find is always legal.
  Prompt.SearchLibrary {} -> Nothing
  -- Declining the re-entrant cast is always legal.
  Prompt.CastWhileSearching {} -> Nothing
  -- CR 601.2b: X=0 is always payable.
  Prompt.ChooseX {} -> 0
  -- The first `count` legal modes, deterministically -- and under CR 700.2d's
  -- "You may choose the same mode more than once" the LEAST legal mode that many
  -- times, since there may be fewer legal modes than the count and the answer
  -- still has to satisfy the instruction. `count` is the FEWEST the instruction
  -- allows, so a range answers with its floor -- the smallest legal answer reaches
  -- least of the game a transcript ran out of.
  Prompt.ChooseModes _ _ _ legal selection ->
    let count = Modal.leastOf selection
     in if Modal.allowsRepeatsIn selection
          then maybe Seq.empty (Seq.replicate (Natural.toIntSaturating count)) (Set.lookupMin legal)
          else Seq.fromList (List.genericTake count (Set.toAscList legal))
  -- CR 707.5: declining to copy is always legal.
  Prompt.ChooseCopyTarget {} -> Nothing
  -- CR 208.2b: asked only when the list has two or more shapes.
  Prompt.ChooseEntryOption {} -> 0
  -- CR 702.136a: riot's "may" is a real fork, so there is no answer that changes
  -- nothing. Declining is what the rule itself treats as the default branch --
  -- "if you don't, it gains haste" -- and it is the half that puts no counter on
  -- the board, which keeps a short transcript from conjuring a bigger creature
  -- than the game had.
  Prompt.ChooseRiot {} -> OptionalDecision.Declines
  -- CR 702.98a: declining, for ChooseRiot's reason -- it is the half that puts no
  -- counter on the board.
  Prompt.ChooseUnleash {} -> OptionalDecision.Declines
  -- CR 614.1c: declining, for ChooseRiot's reason above and one more. Declining
  -- is the branch the card itself states as the default -- "if you don't, it
  -- enters tapped" -- and it is the half that spends nothing, so a transcript
  -- that ran out cannot drain a life total a real game never spent.
  Prompt.ChoosePayLifeOnEntry {} -> OptionalDecision.Declines
  -- CR 105.1: any of the five colours is a legal answer.
  Prompt.ChooseColor {} -> Color.White
  -- THE ONE ILLEGAL ANSWER, deliberately. CR 201.4 offers every card in the
  -- Oracle card reference and no card is called "", so this names nothing -- and
  -- CR 201.2a is why naming nothing is harmless: an object with no name doesn't
  -- have the same name as any other object, so the prompt's asker prohibits
  -- nothing. Every arm reaching here is already a reported desync (the
  -- transcript ran out or did not match), and conjuring a REAL card's name would
  -- silently prohibit that card in the replay instead.
  Prompt.ChooseCardName {} -> CardName.MkCardName Text.empty
  -- Every candidate is an opponent the card's own text offered, and the prompt
  -- is raised only when there are two or more.
  Prompt.ChooseOpponent _ _ _ opponents -> NonEmpty.head opponents
  -- CR 310.8a: the head of Battle.protectorCandidates, the same filter-not-trust
  -- fallback Battle.designateProtector applies to a wrong answer.
  Prompt.ChooseProtector _ _ _ candidates -> NonEmpty.head candidates
  -- CR 305.6: any of the five basic land types is legal. Mountain is what the
  -- ChooseLandTypeSwap arm above falls back to, so the two agree on which type
  -- a short transcript conjures.
  Prompt.ChooseBasicLandType {} -> Subtype.Mountain
  -- CR 603.3b: the canonical order is always a legal answer.
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  -- CR 615.7: likewise, and it is the order the batch was gathered in.
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  -- CR 601.2h: likewise, and it is the cost's PRINTED order -- what pawl paid in
  -- before the order became the payer's to choose.
  Prompt.OrderCostComponents _ _ _ components -> zipWith const [0 ..] components
  -- CR 616.1: the bucket is non-empty when this is asked, so index 0 is legal.
  Prompt.ChooseReplacement {} -> 0
  -- CR 603.7c: every minted token is a legal thing for "it" to name.
  Prompt.ChooseBoundToken _ _ _ candidates -> NonEmpty.head candidates
  -- The first `count` candidates, which the engine offers in ascending order.
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  -- CR 406.2: the first `count` candidates, the arm above's rule over the
  -- graveyard pool the engine offers in that zone's own order.
  Prompt.ChooseExilesFromGraveyard _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  -- Every candidate. The maximal subset, mirroring the arm above taking the first
  -- `count` rather than the last: a deterministic fallback, not a recommendation.
  Prompt.ChooseAnyNumberToSacrifice _ _ _ candidates -> Set.fromList candidates
  -- CR 702.122a: every candidate, the arm above's maximal subset. Legal whenever
  -- the cost is payable at all, save for a board where some candidate has
  -- NEGATIVE power and dragging it in drops the total back under the threshold
  -- -- in which case the payment goes Unpaid, which `Cost.pay` turns into a
  -- complete no-op rather than an illegal state. A deterministic fallback, not a
  -- recommendation.
  Prompt.ChooseTapsForTotalPower _ _ _ candidates _ -> Set.fromList candidates
  -- CR 701.3a: every candidate is a destination the card's own text offered.
  Prompt.ChooseAttachment _ _ _ candidates -> NonEmpty.head candidates
  -- CR 303.4k: declining, the ChooseRiot posture -- the "may" is a real fork, so
  -- there is no answer that changes nothing, and this is the half that moves no
  -- permanent. It is also the half a transcript that ran out cannot get wrong in
  -- the player's favour: CR 704.5m buries the Aura it leaves unattached.
  Prompt.ChooseTurnUpAttachment {} -> OptionalDecision.Declines
  -- The first offered candidate is the PRINTED cost for a cast from hand
  -- (Cost.costsFor puts it first), so it sacrifices nothing. A cast from the
  -- graveyard offers only CR 702.34a's flashback cost, so the head is the sole
  -- candidate and this prompt is not raised at all. Cost.firstOffered keeps
  -- this total for the empty list the engine never produces.
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  -- CR 103.5: keeping is always legal.
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  -- A legal ordered subset of the redrawn hand: deterministically the first
  -- `count`.
  Prompt.Bottom _ _ hand count -> List.genericTake count hand
  -- CR 103.5b: declining is always legal.
  Prompt.MulliganAction {} -> Nothing
  -- CR 103.6: declining is always legal.
  Prompt.OpeningHandAction {} -> Nothing
  -- CR 603.5: declining a "may" is always legal and changes nothing.
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  -- CR 608.2g: declining an offered cast is always legal, and it leaves the card
  -- exactly where the resolving effect put it.
  Prompt.OfferedCast {} -> OptionalDecision.Declines
  -- CR 118.12a: the cost rides a "may", so declining is always legal, and it
  -- spends nothing -- which keeps a short transcript from tapping a payer's board.
  Prompt.ChooseToPay {} -> PaymentDecision.Declines
  -- CR 118.13a: every offered route is payable, and the prompt is raised only
  -- where two are.
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers -> NonEmpty.head offers
  -- CR 118.13a again, for CR 107.4e's monocolored hybrid: every offered route is
  -- payable, and the prompt is raised only where two are.
  Prompt.AnnounceHybridPayment _ _ _ _ offers -> NonEmpty.head offers
  -- CR 118.13a once more, for CR 107.4e's colour/colour hybrid: every offered
  -- half is payable, and the prompt is raised only where two are.
  Prompt.AnnounceHybridHalf _ _ _ _ offers -> NonEmpty.head offers
  -- CR 118.7e: both halves are legal answers, so the first offered one is as
  -- deterministic a default as any -- and unlike the announcements above it can
  -- never make a cost unpayable, since CR 601.2f floors a reduction at {0}.
  Prompt.ChooseReductionHalf _ _ _ _ offers -> NonEmpty.head offers
  -- CR 702.42a: entwine is a "may", so declining is always legal. It also costs
  -- no mana, which keeps a short transcript from diverging into an unpayable
  -- cast.
  Prompt.ChooseEntwine {} -> EntwineDecision.Declines
  -- CR 702.33a: kicker is a "may", so declining is always legal, and declining
  -- costs no mana -- entwine's reason above, word for word.
  Prompt.ChooseKicker {} -> KickerDecision.Declines
  -- CR 903.9a is a "may", so leaving the commander where it is is always legal
  -- and is the answer that changes nothing.
  Prompt.ReturnCommander {} -> CommandZoneDecision.Leaves
  -- CR 401.2: both ends are legal. The BOTTOM, which is
  -- LibraryPosition.defaultValue and so the end every library arrival in the tree
  -- took before an effect could name one.
  Prompt.ChooseLibraryEnd {} -> LibraryPosition.defaultValue
  -- CR 401.4: the canonical order is always a legal answer, as OrderTriggers'
  -- arm above says.
  Prompt.ArrangeLibraryArrivals _ _ _ oids -> zipWith const [0 ..] oids

-- Run a game under a base interpreter, keeping every answer in order.
--
-- The game each question came from (Asked, #153) is dropped here, and both
-- halves drop it: a transcript is a positional list of ANSWERS, and a subgame's
-- questions are answered from the same list in the order they are asked -- which
-- is exactly what lets one transcript replay a game containing a CR 729 subgame.
record :: (forall r. Prompt r -> r) -> GameState -> Game a -> ((a, GameState), [Response])
record answer gs game =
  let step :: Prompt r -> State.State [Response] r
      step p = do
        let value = answer p
        State.modify' (encode p value :)
        pure value
      (outcome, logged) =
        State.runState (Program.foldProgramM (step . Asked.prompt) (State.runStateT game gs)) []
   in (outcome, reverse logged)

-- Re-run a game against a recorded transcript. Because the engine is pure and
-- every decision is a suspension, feeding back the same answers reproduces the
-- same final state.
--
-- Mirrors 'record', but yields the outcome alongside the first point at which
-- the transcript stopped answering (Nothing when it answered every prompt
-- exactly). The report is a RETURN VALUE because 'defaultAnswer' is total: a
-- transcript that has drifted out of step still produces a final state, just
-- not the recorded game's, and for Prompt.Concede that silently decides who
-- wins.
--
-- The desync is not itself an error and does not stop the run. Positional
-- replay cannot tell a prompt the engine gained from one it lost, so there is
-- no resync that is right in both directions; consuming nothing on a mismatch
-- is the conservative half of that.
replay :: [Response] -> GameState -> Game a -> ((a, GameState), Maybe Desync)
replay responses gs game =
  let step :: Prompt r -> State.State (Natural, [Response], Maybe Desync) r
      step p = do
        (asked, remaining, desync) <- State.get
        let stall d = State.put (succ asked, remaining, desync <|> Just d)
        case remaining of
          [] -> do
            stall (Desync.Exhausted asked)
            pure (defaultAnswer p)
          h : t -> case decode p h of
            Just value -> do
              State.put (succ asked, t, desync)
              pure value
            Nothing -> do
              stall (Desync.Mismatched asked h)
              pure (defaultAnswer p)
      (outcome, (_, _, reported)) =
        State.runState (Program.foldProgramM (step . Asked.prompt) (State.runStateT game gs)) (0, responses, Nothing)
   in (outcome, reported)
