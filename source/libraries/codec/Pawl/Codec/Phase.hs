module Pawl.Codec.Phase where

import qualified Pawl.Codec.BeginningStep as BeginningStep
import qualified Pawl.Codec.CombatStep as CombatStep
import qualified Pawl.Codec.EndingStep as EndingStep
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Phase as Phase

codec :: Codec.Codec Phase.Phase
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Beginning" BeginningStep.codec Phase.Beginning (\x -> case x of Phase.Beginning y -> Just y; _ -> Nothing),
      Arm.nullary "PrecombatMain" Phase.PrecombatMain,
      Arm.payload "Combat" CombatStep.codec Phase.Combat (\x -> case x of Phase.Combat y -> Just y; _ -> Nothing),
      Arm.nullary "PostcombatMain" Phase.PostcombatMain,
      Arm.payload "Ending" EndingStep.codec Phase.Ending (\x -> case x of Phase.Ending y -> Just y; _ -> Nothing)
    ]

tagOf :: Phase.Phase -> String
tagOf x = case x of
  Phase.Beginning {} -> "Beginning"
  Phase.PrecombatMain {} -> "PrecombatMain"
  Phase.Combat {} -> "Combat"
  Phase.PostcombatMain {} -> "PostcombatMain"
  Phase.Ending {} -> "Ending"

-- | Every step as one bare name ("DeclareAttackers"), for a document people
-- write by hand, where 'codec''s nesting says nothing the name does not. A
-- phase with no steps is named by its own constructor (CR 500.1).
flat :: Codec.Codec Phase.Phase
flat =
  let arms =
        fmap (\x -> Arm.nullary (show x) (Phase.Beginning x)) [minBound .. maxBound :: BeginningStep.BeginningStep]
          <> [Arm.nullary "PrecombatMain" Phase.PrecombatMain]
          <> fmap (\x -> Arm.nullary (show x) (Phase.Combat x)) [minBound .. maxBound :: CombatStep.CombatStep]
          <> [Arm.nullary "PostcombatMain" Phase.PostcombatMain]
          <> fmap (\x -> Arm.nullary (show x) (Phase.Ending x)) [minBound .. maxBound :: EndingStep.EndingStep]
   in Arm.keyedAnonymous flatName arms

flatName :: Phase.Phase -> String
flatName x = case x of
  Phase.Beginning y -> show y
  Phase.PrecombatMain -> "PrecombatMain"
  Phase.Combat y -> show y
  Phase.PostcombatMain -> "PostcombatMain"
  Phase.Ending y -> show y
