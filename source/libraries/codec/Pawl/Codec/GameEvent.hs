-- | The last codec under @Pawl.Codec@ to become a bundle, and the reason it was
-- last: every arm here wrote a positional array, so each owed a record before
-- 'Arm.tagged' -- which admits only single-payload arms -- could take it.
--
-- Runtime-only. This serialises a transcript, never card data, which is why the
-- move to named objects changed no file under @data/cards@.
module Pawl.Codec.GameEvent where

import qualified Pawl.Codec.AbilityTriggered as AbilityTriggered
import qualified Pawl.Codec.AttackerBlocked as AttackerBlocked
import qualified Pawl.Codec.AttackerDeclared as AttackerDeclared
import qualified Pawl.Codec.BecameDesignated as BecameDesignated
import qualified Pawl.Codec.BlockerDeclared as BlockerDeclared
import qualified Pawl.Codec.BlocksDeclared as BlocksDeclared
import qualified Pawl.Codec.ControlChanged as ControlChanged
import qualified Pawl.Codec.CounterChange as CounterChange
import qualified Pawl.Codec.Countering as Countering
import qualified Pawl.Codec.DamageEvent as DamageEvent
import qualified Pawl.Codec.DamagePrevented as DamagePrevented
import qualified Pawl.Codec.Discarded as Discarded
import qualified Pawl.Codec.Drew as Drew
import qualified Pawl.Codec.HalfUnlocked as HalfUnlocked
import qualified Pawl.Codec.LifeChange as LifeChange
import qualified Pawl.Codec.Mentored as Mentored
import qualified Pawl.Codec.Moved as Moved
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Revealed as Revealed
import qualified Pawl.Codec.SpellWasCast as SpellWasCast
import qualified Pawl.Codec.StepBegan as StepBegan
import qualified Pawl.Codec.VentureMarkerEntered as VentureMarkerEntered
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.GameEvent as GameEvent

codec :: Codec.Codec GameEvent.GameEvent
codec =
  Arm.tagged
    encode
    [ Arm.payload "Moved" Moved.codec GameEvent.Moved,
      Arm.payload "DamageDealt" DamageEvent.codec GameEvent.DamageDealt,
      Arm.payload "DamagePrevented" DamagePrevented.codec GameEvent.DamagePrevented,
      Arm.payload "StepBegan" StepBegan.codec GameEvent.StepBegan,
      Arm.payload "SpellCast" SpellWasCast.codec GameEvent.SpellCast,
      Arm.payload "BecameMonarch" PlayerId.codec GameEvent.BecameMonarch,
      Arm.payload "Discarded" Discarded.codec GameEvent.Discarded,
      Arm.payload "Drew" Drew.codec GameEvent.Drew,
      Arm.payload "AttackerDeclared" AttackerDeclared.codec GameEvent.AttackerDeclared,
      Arm.payload "BlockerDeclared" BlockerDeclared.codec GameEvent.BlockerDeclared,
      Arm.payload "AttackerBlocked" AttackerBlocked.codec GameEvent.AttackerBlocked,
      Arm.payload "BlocksDeclared" BlocksDeclared.codec GameEvent.BlocksDeclared,
      Arm.payload "Revealed" Revealed.codec GameEvent.Revealed,
      Arm.payload "SpellCountered" Countering.codec GameEvent.SpellCountered,
      Arm.payload "LifeLost" LifeChange.codec GameEvent.LifeLost,
      Arm.payload "LifeGained" LifeChange.codec GameEvent.LifeGained,
      Arm.payload "LoyaltyAbilityActivated" ObjectId.codec GameEvent.LoyaltyAbilityActivated,
      Arm.payload "CountersPut" CounterChange.codec GameEvent.CountersPut,
      Arm.payload "CountersRemoved" CounterChange.codec GameEvent.CountersRemoved,
      Arm.payload "HalfUnlocked" HalfUnlocked.codec GameEvent.HalfUnlocked,
      Arm.payload "TurnedFaceUp" ObjectId.codec GameEvent.TurnedFaceUp,
      Arm.payload "BecameDesignated" BecameDesignated.codec GameEvent.BecameDesignated,
      Arm.payload "Evolved" ObjectId.codec GameEvent.Evolved,
      Arm.payload "Mentored" Mentored.codec GameEvent.Mentored,
      Arm.payload "PermanentSacrificed" PermanentSacrificed.codec GameEvent.PermanentSacrificed,
      Arm.payload "AbilityTriggered" AbilityTriggered.codec GameEvent.AbilityTriggered,
      Arm.payload "ControlChanged" ControlChanged.codec GameEvent.ControlChanged,
      Arm.payload "VentureMarkerEntered" VentureMarkerEntered.codec GameEvent.VentureMarkerEntered
    ]
  where
    tag t = Common.tagged t . Just
    encode e = case e of
      GameEvent.Moved x -> tag "Moved" $ Codec.encode Moved.codec x
      GameEvent.DamageDealt x -> tag "DamageDealt" $ Codec.encode DamageEvent.codec x
      GameEvent.DamagePrevented x -> tag "DamagePrevented" $ Codec.encode DamagePrevented.codec x
      GameEvent.StepBegan x -> tag "StepBegan" $ Codec.encode StepBegan.codec x
      GameEvent.SpellCast x -> tag "SpellCast" $ Codec.encode SpellWasCast.codec x
      GameEvent.BecameMonarch x -> tag "BecameMonarch" $ Codec.encode PlayerId.codec x
      GameEvent.Discarded x -> tag "Discarded" $ Codec.encode Discarded.codec x
      GameEvent.Drew x -> tag "Drew" $ Codec.encode Drew.codec x
      GameEvent.AttackerDeclared x -> tag "AttackerDeclared" $ Codec.encode AttackerDeclared.codec x
      GameEvent.BlockerDeclared x -> tag "BlockerDeclared" $ Codec.encode BlockerDeclared.codec x
      GameEvent.AttackerBlocked x -> tag "AttackerBlocked" $ Codec.encode AttackerBlocked.codec x
      GameEvent.BlocksDeclared x -> tag "BlocksDeclared" $ Codec.encode BlocksDeclared.codec x
      GameEvent.Revealed x -> tag "Revealed" $ Codec.encode Revealed.codec x
      GameEvent.SpellCountered x -> tag "SpellCountered" $ Codec.encode Countering.codec x
      GameEvent.LifeLost x -> tag "LifeLost" $ Codec.encode LifeChange.codec x
      GameEvent.LifeGained x -> tag "LifeGained" $ Codec.encode LifeChange.codec x
      GameEvent.LoyaltyAbilityActivated x -> tag "LoyaltyAbilityActivated" $ Codec.encode ObjectId.codec x
      GameEvent.CountersPut x -> tag "CountersPut" $ Codec.encode CounterChange.codec x
      GameEvent.CountersRemoved x -> tag "CountersRemoved" $ Codec.encode CounterChange.codec x
      GameEvent.HalfUnlocked x -> tag "HalfUnlocked" $ Codec.encode HalfUnlocked.codec x
      GameEvent.TurnedFaceUp x -> tag "TurnedFaceUp" $ Codec.encode ObjectId.codec x
      GameEvent.BecameDesignated x -> tag "BecameDesignated" $ Codec.encode BecameDesignated.codec x
      GameEvent.Evolved x -> tag "Evolved" $ Codec.encode ObjectId.codec x
      GameEvent.Mentored x -> tag "Mentored" $ Codec.encode Mentored.codec x
      GameEvent.PermanentSacrificed x -> tag "PermanentSacrificed" $ Codec.encode PermanentSacrificed.codec x
      GameEvent.AbilityTriggered x -> tag "AbilityTriggered" $ Codec.encode AbilityTriggered.codec x
      GameEvent.ControlChanged x -> tag "ControlChanged" $ Codec.encode ControlChanged.codec x
      GameEvent.VentureMarkerEntered x -> tag "VentureMarkerEntered" $ Codec.encode VentureMarkerEntered.codec x
