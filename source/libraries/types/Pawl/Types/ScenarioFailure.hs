module Pawl.Types.ScenarioFailure where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Check as Check
import qualified Pawl.Types.Choices as Choices
import qualified Pawl.Types.Entry as Entry
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Move as Move
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Reference as Reference
import qualified Pawl.Types.Timed as Timed
import qualified Pawl.Types.When as When

-- | Why a scenario did not run clean. Malformed boards, prompt drift and a
-- scenario that silently stopped exercising its subject are all ordinary
-- values, never exceptions. Offers are rendered as a scenario would write them.
data ScenarioFailure
  = MkDuplicateLabel Label.Label
  | MkUnknownActivePlayer Label.Label
  | MkUnknownController Label.Label
  | MkUnknownCard CardName.CardName
  | -- | A placement's protector names no seat (CR 310.9).
    MkUnknownProtector Label.Label
  | -- | A placement shows a face its card does not have (CR 712.8), card first.
    MkUnknownFace CardName.CardName CardName.CardName
  | -- | A placement attached to a label that names nothing on the board, or to
    -- something CR 301.5 / 303.4 forbid it from being attached to.
    MkIllegalAttachment Label.Label
  | -- | A token placed off the battlefield, where CR 111.7 says it would not exist.
    MkTokenOffBattlefield CardName.CardName
  | -- | An emperor seat on no team, or a second emperor on one team (CR 809.2).
    MkIllegalEmperor Label.Label
  | -- | The monarch names no seat.
    MkUnknownMonarch Label.Label
  | -- | The reference named no live object, with whether the board did label
    -- one so -- a stale label and a typo read alike otherwise.
    MkUnknownObject Reference.Reference Bool
  | -- | A label naming a seat where only an object will do.
    MkNotAnObject Reference.Reference
  | -- | A label naming an object where only a seat will do.
    MkNotAPlayer Label.Label
  | -- | The reference resolved, but the prompt did not offer it.
    MkUnofferedObject When.When Text.Text Text.Text [Text.Text]
  | -- | A source qualifier on a prompt with no source, which nothing matches.
    MkUnexpectedQualifier When.When Text.Text Reference.Reference
  | MkNestedGamePrompt Natural Phase.Phase (Maybe Label.Label) Text.Text
  | MkUnscheduledPrompt Natural Phase.Phase (Maybe Label.Label) Text.Text [Text.Text]
  | MkUnexpectedPrompt When.When Entry.Entry Text.Text [Text.Text]
  | MkActionNotOffered When.When Move.Move [Text.Text]
  | MkAmbiguousAction When.When Move.Move [Text.Text]
  | MkUnexpectedActionChoice When.When Move.Move Text.Text
  | MkUnusedActionChoices When.When Move.Move Choices.Choices
  | -- | A move the timeline said the engine would refuse, which stood: the
    -- prompt asked next (Nothing when none was), not the one it answered.
    MkUnrefusedMove When.When Move.Move (Maybe Text.Text)
  | -- | Entries whose moment never came, at least one of them a move, and the
    -- turn and step the run stopped at.
    MkUnreachedEntries Natural Phase.Phase (Seq.Seq Timed.Timed)
  | -- | Checks whose moment never came: assertions that never ran, which must
    -- never read like ones that ran false.
    MkUnrunChecks Natural Phase.Phase (Seq.Seq Timed.Timed)
  | -- | A check that ran false, its moment (Nothing for a final check), and
    -- what was observed instead.
    MkCheckFailed (Maybe When.When) Check.Check Text.Text
  deriving (Eq, Ord, Show)
