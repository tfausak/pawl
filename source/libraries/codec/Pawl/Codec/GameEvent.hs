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
import qualified Pawl.Codec.BecameTarget as BecameTarget
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
import qualified Pawl.Codec.Milled as Milled
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
import qualified Pawl.Types.GameEvent as GameEvent

codec :: Codec.Codec GameEvent.GameEvent
codec =
  Arm.tagged
    [ Arm.payload "Moved" Moved.codec GameEvent.Moved (\x -> case x of GameEvent.Moved y -> Just y; _ -> Nothing),
      Arm.payload "DamageDealt" DamageEvent.codec GameEvent.DamageDealt (\x -> case x of GameEvent.DamageDealt y -> Just y; _ -> Nothing),
      Arm.payload "DamagePrevented" DamagePrevented.codec GameEvent.DamagePrevented (\x -> case x of GameEvent.DamagePrevented y -> Just y; _ -> Nothing),
      Arm.payload "StepBegan" StepBegan.codec GameEvent.StepBegan (\x -> case x of GameEvent.StepBegan y -> Just y; _ -> Nothing),
      Arm.payload "SpellCast" SpellWasCast.codec GameEvent.SpellCast (\x -> case x of GameEvent.SpellCast y -> Just y; _ -> Nothing),
      Arm.payload "BecameMonarch" PlayerId.codec GameEvent.BecameMonarch (\x -> case x of GameEvent.BecameMonarch y -> Just y; _ -> Nothing),
      Arm.payload "Discarded" Discarded.codec GameEvent.Discarded (\x -> case x of GameEvent.Discarded y -> Just y; _ -> Nothing),
      Arm.payload "Milled" Milled.codec GameEvent.Milled (\x -> case x of GameEvent.Milled y -> Just y; _ -> Nothing),
      Arm.payload "Drew" Drew.codec GameEvent.Drew (\x -> case x of GameEvent.Drew y -> Just y; _ -> Nothing),
      Arm.payload "AttackerDeclared" AttackerDeclared.codec GameEvent.AttackerDeclared (\x -> case x of GameEvent.AttackerDeclared y -> Just y; _ -> Nothing),
      Arm.payload "BlockerDeclared" BlockerDeclared.codec GameEvent.BlockerDeclared (\x -> case x of GameEvent.BlockerDeclared y -> Just y; _ -> Nothing),
      Arm.payload "AttackerBlocked" AttackerBlocked.codec GameEvent.AttackerBlocked (\x -> case x of GameEvent.AttackerBlocked y -> Just y; _ -> Nothing),
      Arm.payload "BlocksDeclared" BlocksDeclared.codec GameEvent.BlocksDeclared (\x -> case x of GameEvent.BlocksDeclared y -> Just y; _ -> Nothing),
      Arm.payload "Revealed" Revealed.codec GameEvent.Revealed (\x -> case x of GameEvent.Revealed y -> Just y; _ -> Nothing),
      Arm.payload "SpellCountered" Countering.codec GameEvent.SpellCountered (\x -> case x of GameEvent.SpellCountered y -> Just y; _ -> Nothing),
      Arm.payload "LifeLost" LifeChange.codec GameEvent.LifeLost (\x -> case x of GameEvent.LifeLost y -> Just y; _ -> Nothing),
      Arm.payload "LifeGained" LifeChange.codec GameEvent.LifeGained (\x -> case x of GameEvent.LifeGained y -> Just y; _ -> Nothing),
      Arm.payload "LoyaltyAbilityActivated" ObjectId.codec GameEvent.LoyaltyAbilityActivated (\x -> case x of GameEvent.LoyaltyAbilityActivated y -> Just y; _ -> Nothing),
      Arm.payload "CountersPut" CounterChange.codec GameEvent.CountersPut (\x -> case x of GameEvent.CountersPut y -> Just y; _ -> Nothing),
      Arm.payload "CountersRemoved" CounterChange.codec GameEvent.CountersRemoved (\x -> case x of GameEvent.CountersRemoved y -> Just y; _ -> Nothing),
      Arm.payload "HalfUnlocked" HalfUnlocked.codec GameEvent.HalfUnlocked (\x -> case x of GameEvent.HalfUnlocked y -> Just y; _ -> Nothing),
      Arm.payload "TurnedFaceUp" ObjectId.codec GameEvent.TurnedFaceUp (\x -> case x of GameEvent.TurnedFaceUp y -> Just y; _ -> Nothing),
      Arm.payload "BecameDesignated" BecameDesignated.codec GameEvent.BecameDesignated (\x -> case x of GameEvent.BecameDesignated y -> Just y; _ -> Nothing),
      Arm.payload "Evolved" ObjectId.codec GameEvent.Evolved (\x -> case x of GameEvent.Evolved y -> Just y; _ -> Nothing),
      Arm.payload "Mentored" Mentored.codec GameEvent.Mentored (\x -> case x of GameEvent.Mentored y -> Just y; _ -> Nothing),
      Arm.payload "PermanentSacrificed" PermanentSacrificed.codec GameEvent.PermanentSacrificed (\x -> case x of GameEvent.PermanentSacrificed y -> Just y; _ -> Nothing),
      Arm.payload "AbilityTriggered" AbilityTriggered.codec GameEvent.AbilityTriggered (\x -> case x of GameEvent.AbilityTriggered y -> Just y; _ -> Nothing),
      Arm.payload "ControlChanged" ControlChanged.codec GameEvent.ControlChanged (\x -> case x of GameEvent.ControlChanged y -> Just y; _ -> Nothing),
      Arm.payload "VentureMarkerEntered" VentureMarkerEntered.codec GameEvent.VentureMarkerEntered (\x -> case x of GameEvent.VentureMarkerEntered y -> Just y; _ -> Nothing),
      Arm.payload "BecameTarget" BecameTarget.codec GameEvent.BecameTarget (\x -> case x of GameEvent.BecameTarget y -> Just y; _ -> Nothing),
      Arm.payload "LeftTheGame" ObjectId.codec GameEvent.LeftTheGame (\x -> case x of GameEvent.LeftTheGame y -> Just y; _ -> Nothing)
    ]
