module Pawl.Codec.ActivationRestriction where

import qualified Data.Text as Text
import qualified Pawl.Codec.PhaseSelector as PhaseSelector
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction

-- | Tagged rather than bare-nullary since CR 500.1's DuringPhase carries a
-- window; SorcerySpeed and AttackedThisStep still render as bare tags.
--
-- There is no tag for "no rider": CR 602.2's default is the EMPTY LIST on the
-- ability, which Pawl.Codec.ActivatedAbility writes by omitting the key.
--
-- DuringPhase's payload is a PAIR -- the window and the turn scope -- written
-- as a two-element array. There is deliberately no scope-less form: the axis is
-- not a default a card may omit, so a bare window payload is a decode failure
-- rather than a silent EachTurn.
--
-- The window is a PhaseSelector, so a rider naming a step nests it inside that
-- type's own `Step` tag. The codec does not also accept the older bare-phase
-- spelling: two spellings of one window is the thing Pawl.Types.PhaseSelector
-- exists to prevent.
toJson :: ActivationRestriction.ActivationRestriction -> Value.Value
toJson t = case t of
  ActivationRestriction.SorcerySpeed -> Common.nullary "SorcerySpeed"
  ActivationRestriction.DuringPhase sel sc -> Common.tagged "DuringPhase" . Just . Common.array $ [Codec.encode PhaseSelector.codec sel, TurnScope.toJson sc]
  ActivationRestriction.AttackedThisStep -> Common.nullary "AttackedThisStep"

fromJson :: Value.Value -> Either Text.Text ActivationRestriction.ActivationRestriction
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("SorcerySpeed", _) -> Right ActivationRestriction.SorcerySpeed
    ("DuringPhase", Just (Value.Array (Array.MkArray [sel, sc]))) -> ActivationRestriction.DuringPhase <$> Codec.decode PhaseSelector.codec sel <*> TurnScope.fromJson sc
    ("AttackedThisStep", _) -> Right ActivationRestriction.AttackedThisStep
    _ -> Left . Text.pack $ "unknown ActivationRestriction: " <> t
