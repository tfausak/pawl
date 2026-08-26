module Pawl.Types.Clause where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PayGate as PayGate

-- | CR 608.2e's own unit: one of the "multiple steps or actions, denoted by
-- separate sentences or clauses" a spell or ability may have. `effects` is a Seq
-- because CR 608.2c resolves in written order and duplicates are allowed.
--
-- The clause is what a resolution-time rider covers, and the MODE is what a
-- cast-time choice covers. That split is the whole point of this type: CR
-- 601.2b/700.2b fix the modes and CR 601.2c the targets as the spell is cast and
-- never revisit them, while CR 608.2d announces the remaining choices "while
-- applying the effect". Pawl.Types.Mode carried both jobs until a card needed
-- them apart (#335).
--
-- No `targetSlots` here, and that asymmetry is the design rather than an
-- omission: CR 601.2c fills a target slot as the spell is cast, which is the
-- mode's business, so a clause has no namespace of its own.
--
-- Parametric in `card` for Pawl.Types.Effect's reason -- Card embeds the
-- payload, so a concrete `Effect Card` here would make the two modules mutually
-- importing.
data Clause card = MkClause
  { -- | CR 608.2c's "If you do": which EARLIER clause of this mode this one
    -- hangs off, so that declining that one skips this one too. Tweeze's "you
    -- may discard a card. If you do, draw a card" is the witness -- the draw is
    -- a clause of its own naming the "may" clause's ordinal. Nothing is the
    -- unmarked case every other card in the corpus takes.
    --
    -- CR 608.2c is the whole authority: the controller "follows its instructions
    -- in the order written", and later text may modify the meaning of earlier
    -- text, so this is clause sequencing rather than a rule of its own. That is
    -- why the reference is CR 608.2e's ordinal (Pawl.Types.ClauseIndex), the one
    -- Pawl.Types.PayGate.offeredAt already names, and why it is read against the
    -- clauses of THIS mode instance (CR 700.2d).
    --
    -- Keyed on the ANSWER the named clause's own riders produced -- did its
    -- instructions run -- and never on the board afterwards, which is PayGate's
    -- posture (CR 118.12's "regardless of what events actually occurred")
    -- carried over to CR 603.5's "may". Baral, Chief of Compliance needs none of
    -- this: its "you may draw a card. If you do, discard a card" folds into one
    -- optional clause because CR 121.3 makes the draw happen whenever it is
    -- chosen, so the answer and the action cannot come apart.
    --
    -- Not implemented: an option the named clause could not legally take is
    -- offered anyway (CR 608.2d), so accepting "you may discard a card" with an
    -- empty hand admits this clause (#2167).
    --
    -- NOT CR 603.12's reflexive triggered ability, the "when you do" spelling
    -- (Pawl.Types.TriggerCondition's Reflexive): that one creates a NEW ability
    -- that uses the stack and chooses its targets at CR 603.3d, where this
    -- clause is part of the same resolution and gets no priority pass. A card
    -- printing "if you do" must not become one.
    --
    -- Not an arm of Pawl.Types.Condition either: that is a predicate over game
    -- STATE, with customers outliving the resolution -- a "for as long as"
    -- duration, a static ability's "as long as" -- where a clause ordinal names
    -- nothing.
    ifTaken :: Maybe ClauseIndex.ClauseIndex,
    -- | CR 701.46a's "if this permanent has no +1/+1 counters on it" -- a gate on
    -- THIS clause's effects rather than on the whole ability, which is why the
    -- rider rides the same carrier CR 603.5's "may" does. CR 701.37a's
    -- monstrosity (Nessian Asp) is two instructions rather than adapt's one, but
    -- its gate still covers the whole ability; Into the Wilds gates the SECOND
    -- of two clauses in one mode and Burst Lightning gates each of two, which is
    -- what makes the CLAUSE the carrier -- a mode-level gate is fixed at CR
    -- 601.2b and could not let the look happen and the put not. CR 701.47a's
    -- amass is not the witness: it landed as one Effect.Amass opcode whose gate
    -- is the engine's, so it writes no condition here.
    --
    -- Read as this clause is APPLIED (CR 608.2c's "in the order written"), not
    -- once at CR 601.2b -- so an earlier clause's effects can flip it. Nothing
    -- is the unmarked case every other card in the corpus takes.
    --
    -- NOT Pawl.Types.Effect.Replace's own Maybe Condition: that one gates
    -- whether a floating replacement effect gets INSTALLED and travels with the
    -- installed row, while this one gates whether the clause's instructions run
    -- at all.
    condition :: Maybe Condition.Condition,
    -- | CR 608.2d's "or": the sibling clause of this mode that this one is
    -- exclusive with, so exactly one of the two happens. Twiddle's "you may tap
    -- or untap target artifact, creature, or land" is the witness -- the tap and
    -- the untap are two clauses of one mode reading one target slot, and the
    -- controller announces which of them "while applying the effect". Nothing is
    -- the unmarked case every other card in the corpus takes.
    --
    -- NOT modality (CR 700.2), which needs "two or more options in a bulleted
    -- list preceded by instructions for a player to choose a number of those
    -- options", and whose modes CR 601.2b fixes as the spell is cast. Twiddle
    -- prints no list and no "choose one", so a Pawl.Types.Modal payload would
    -- announce this choice a whole cast too early.
    --
    -- Named by CR 608.2e's ordinal, the way `ifTaken` names one, and for that
    -- rider's reason: a branch carrying its own effect list would have to sit
    -- inside a Pawl.Types.Effect, which is first-order and non-recursive by
    -- design. SYMMETRIC -- each of the pair names the other -- because the
    -- announcement happens at whichever of them the resolution reaches first,
    -- and that one has to know the pair exists. Pawl.CardSpec's
    -- cardBranchesAreAsymmetric is what holds the corpus to it.
    --
    -- Binary rather than n-ary because nothing in print offers three branches
    -- this way; widening means a Set here and a longer offer in
    -- Pawl.Types.Prompt.ChooseClause, and no change to where the question is
    -- asked.
    --
    -- Independent of `optionality`, which is the pair of constraints two printed
    -- cards impose: Twiddle's "you MAY tap or untap" is one "may" wrapping an
    -- either-or, and Keys to the House's "lock or unlock a door of target Room
    -- you control" is an either-or with no "may" -- the pool's one mandatory
    -- branch pair, proved at Pawl.RoomSpec's CR 709.5g case.
    orElse :: Maybe ClauseIndex.ClauseIndex,
    -- | CR 603.5's printed "may", covering this clause's effects, and WHO it
    -- asks -- see Pawl.Types.Optionality for why the rider rides a carrier
    -- rather than wrapping each effect and why the asker rides the rider, and
    -- Pawl.Engine.Resolve.exercises for where the choice is asked.
    --
    -- A clause spanning two instructions is ONE question, which is what CR
    -- 608.2d's single announcement calls for: "you may draw a card and lose 1
    -- life" is one clause and one prompt. Two adjacent printed "may"s are two
    -- clauses and two prompts. That grouping is structural, which is why the
    -- span is the carrier and an individual instruction is not -- nothing about
    -- one instruction distinguishes the two cases.
    optionality :: Optionality.Optionality,
    -- | CR 118.12's cost paid on resolution, together with which of that rule's
    -- branches this clause's effects are: Mana Leak's "unless its controller
    -- pays {3}" (CR 118.12a's rewriting, so the effects are the "if they don't"
    -- branch) and Merfolk Seer's "you may pay {1}{U}. If you do" alike. Nothing
    -- for every card that states no such cost. The payment need not spend a
    -- resource -- CR 701.63a's endure puts +1/+1 counters on instead (Fortress
    -- Kin-Guard).
    --
    -- The ONE rider whose span is not exactly this clause. CR 118.12 offers a
    -- cost once and reads the one answer, so Don't Make a Sound's two clauses
    -- hang off one {2} -- see Pawl.Types.PayGate.offeredAt. Stymied Hopes is the
    -- other end of the same question, a gate over one clause of two.
    --
    -- The five riders are independent, and Pawl.Engine.Resolve asks them in
    -- printed order: `ifTaken` first (CR 608.2c's "If you do" prefixes the
    -- sentence, and a clause whose predecessor was declined is no question to
    -- ask), then `condition` (CR 701.46a prints its "if" ahead of the
    -- instructions), then `orElse` (a branch the controller did not pick has no
    -- "may" left to offer -- Twiddle prints one "may" over the pair, not one per
    -- branch), then the "may", then this -- a declined clause has no instruction
    -- left for a payment to qualify.
    payGate :: Maybe PayGate.PayGate,
    effects :: Seq.Seq (Effect.Effect card)
  }
  deriving (Eq, Ord, Show)
