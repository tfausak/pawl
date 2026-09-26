-- | The last codec under @Pawl.Codec@ to become a bundle, and the reason it was
-- last: every arm here wrote a positional array, so each owed a record before
-- 'Arm.tagged' -- which admits only single-payload arms -- could take it.
--
-- Runtime-only. This serialises a transcript, never card data, which is why the
-- move to named objects changed no file under @data/cards@.
module Pawl.Codec.GameEvent where

import qualified Pawl.Codec.AbilityTriggered as AbilityTriggered
import qualified Pawl.Codec.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Codec.AttackerBlocked as AttackerBlocked
import qualified Pawl.Codec.AttackerDeclared as AttackerDeclared
import qualified Pawl.Codec.BecameAttached as BecameAttached
import qualified Pawl.Codec.BecameAttacked as BecameAttacked
import qualified Pawl.Codec.BecameBlocking as BecameBlocking
import qualified Pawl.Codec.BecameDesignated as BecameDesignated
import qualified Pawl.Codec.BecameTarget as BecameTarget
import qualified Pawl.Codec.BecameUnattached as BecameUnattached
import qualified Pawl.Codec.BlocksDeclared as BlocksDeclared
import qualified Pawl.Codec.ClassLevelChange as ClassLevelChange
import qualified Pawl.Codec.CoinFlipped as CoinFlipped
import qualified Pawl.Codec.ControlChanged as ControlChanged
import qualified Pawl.Codec.Convoking as Convoking
import qualified Pawl.Codec.CounterChange as CounterChange
import qualified Pawl.Codec.Countering as Countering
import qualified Pawl.Codec.Crewing as Crewing
import qualified Pawl.Codec.DamageEvent as DamageEvent
import qualified Pawl.Codec.DamagePrevented as DamagePrevented
import qualified Pawl.Codec.DieResult as DieResult
import qualified Pawl.Codec.Discarded as Discarded
import qualified Pawl.Codec.Drew as Drew
import qualified Pawl.Codec.Exploited as Exploited
import qualified Pawl.Codec.HalfUnlocked as HalfUnlocked
import qualified Pawl.Codec.LifeChange as LifeChange
import qualified Pawl.Codec.ManaAbilityResolved as ManaAbilityResolved
import qualified Pawl.Codec.ManaAdded as ManaAdded
import qualified Pawl.Codec.Mentored as Mentored
import qualified Pawl.Codec.Milled as Milled
import qualified Pawl.Codec.Moved as Moved
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PermanentWasSacrificed as PermanentWasSacrificed
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Revealed as Revealed
import qualified Pawl.Codec.Saddling as Saddling
import qualified Pawl.Codec.SpellWasCast as SpellWasCast
import qualified Pawl.Codec.StepBegan as StepBegan
import qualified Pawl.Codec.TappedForMana as TappedForMana
import qualified Pawl.Codec.Transformed as Transformed
import qualified Pawl.Codec.VentureMarkerEntered as VentureMarkerEntered
import qualified Pawl.Codec.ZoneChange as ZoneChange
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.GameEvent as GameEvent

codec :: Codec.Codec GameEvent.GameEvent
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Moved" Moved.codec GameEvent.Moved (\x -> case x of GameEvent.Moved y -> Just y; _ -> Nothing),
      -- CR 712.21's second card. A bare ZoneChange and no snapshot: the
      -- characteristics CR 608.2h wants are the departing permanent's, which the
      -- Moved event beside this one already carries.
      Arm.payload "CardArrived" ZoneChange.codec GameEvent.CardArrived (\x -> case x of GameEvent.CardArrived y -> Just y; _ -> Nothing),
      Arm.payload "DamageDealt" DamageEvent.codec GameEvent.DamageDealt (\x -> case x of GameEvent.DamageDealt y -> Just y; _ -> Nothing),
      Arm.payload "DamagePrevented" DamagePrevented.codec GameEvent.DamagePrevented (\x -> case x of GameEvent.DamagePrevented y -> Just y; _ -> Nothing),
      Arm.payload "StepBegan" StepBegan.codec GameEvent.StepBegan (\x -> case x of GameEvent.StepBegan y -> Just y; _ -> Nothing),
      Arm.payload "SpellCast" SpellWasCast.codec GameEvent.SpellCast (\x -> case x of GameEvent.SpellCast y -> Just y; _ -> Nothing),
      Arm.payload "BecameMonarch" PlayerId.codec GameEvent.BecameMonarch (\x -> case x of GameEvent.BecameMonarch y -> Just y; _ -> Nothing),
      Arm.payload "TookInitiative" PlayerId.codec GameEvent.TookInitiative (\x -> case x of GameEvent.TookInitiative y -> Just y; _ -> Nothing),
      Arm.payload "Discarded" Discarded.codec GameEvent.Discarded (\x -> case x of GameEvent.Discarded y -> Just y; _ -> Nothing),
      Arm.payload "Milled" Milled.codec GameEvent.Milled (\x -> case x of GameEvent.Milled y -> Just y; _ -> Nothing),
      Arm.payload "Drew" Drew.codec GameEvent.Drew (\x -> case x of GameEvent.Drew y -> Just y; _ -> Nothing),
      Arm.payload "AttackerDeclared" AttackerDeclared.codec GameEvent.AttackerDeclared (\x -> case x of GameEvent.AttackerDeclared y -> Just y; _ -> Nothing),
      Arm.payload "BecameAttacked" BecameAttacked.codec GameEvent.BecameAttacked (\x -> case x of GameEvent.BecameAttacked y -> Just y; _ -> Nothing),
      Arm.payload "AttackersDeclared" PlayerId.codec GameEvent.AttackersDeclared (\x -> case x of GameEvent.AttackersDeclared y -> Just y; _ -> Nothing),
      Arm.payload "BecameBlocking" BecameBlocking.codec GameEvent.BecameBlocking (\x -> case x of GameEvent.BecameBlocking y -> Just y; _ -> Nothing),
      Arm.payload "AttackerBlocked" AttackerBlocked.codec GameEvent.AttackerBlocked (\x -> case x of GameEvent.AttackerBlocked y -> Just y; _ -> Nothing),
      Arm.payload "AttackerUnblocked" ObjectId.codec GameEvent.AttackerUnblocked (\x -> case x of GameEvent.AttackerUnblocked y -> Just y; _ -> Nothing),
      Arm.payload "BlocksDeclared" BlocksDeclared.codec GameEvent.BlocksDeclared (\x -> case x of GameEvent.BlocksDeclared y -> Just y; _ -> Nothing),
      Arm.payload "Revealed" Revealed.codec GameEvent.Revealed (\x -> case x of GameEvent.Revealed y -> Just y; _ -> Nothing),
      Arm.payload "SpellCountered" Countering.codec GameEvent.SpellCountered (\x -> case x of GameEvent.SpellCountered y -> Just y; _ -> Nothing),
      Arm.payload "AbilityCountered" Countering.codec GameEvent.AbilityCountered (\x -> case x of GameEvent.AbilityCountered y -> Just y; _ -> Nothing),
      Arm.payload "LifeLost" LifeChange.codec GameEvent.LifeLost (\x -> case x of GameEvent.LifeLost y -> Just y; _ -> Nothing),
      Arm.payload "LifeGained" LifeChange.codec GameEvent.LifeGained (\x -> case x of GameEvent.LifeGained y -> Just y; _ -> Nothing),
      Arm.payload "LoyaltyAbilityActivated" ObjectId.codec GameEvent.LoyaltyAbilityActivated (\x -> case x of GameEvent.LoyaltyAbilityActivated y -> Just y; _ -> Nothing),
      Arm.payload "CountersPut" CounterChange.codec GameEvent.CountersPut (\x -> case x of GameEvent.CountersPut y -> Just y; _ -> Nothing),
      Arm.payload "CountersRemoved" CounterChange.codec GameEvent.CountersRemoved (\x -> case x of GameEvent.CountersRemoved y -> Just y; _ -> Nothing),
      Arm.payload "HalfUnlocked" HalfUnlocked.codec GameEvent.HalfUnlocked (\x -> case x of GameEvent.HalfUnlocked y -> Just y; _ -> Nothing),
      Arm.payload "TurnedFaceUp" ObjectId.codec GameEvent.TurnedFaceUp (\x -> case x of GameEvent.TurnedFaceUp y -> Just y; _ -> Nothing),
      Arm.payload "TurnedFaceDown" ObjectId.codec GameEvent.TurnedFaceDown (\x -> case x of GameEvent.TurnedFaceDown y -> Just y; _ -> Nothing),
      Arm.payload "Transformed" Transformed.codec GameEvent.Transformed (\x -> case x of GameEvent.Transformed y -> Just y; _ -> Nothing),
      Arm.payload "BecameDesignated" BecameDesignated.codec GameEvent.BecameDesignated (\x -> case x of GameEvent.BecameDesignated y -> Just y; _ -> Nothing),
      Arm.payload "Evolved" ObjectId.codec GameEvent.Evolved (\x -> case x of GameEvent.Evolved y -> Just y; _ -> Nothing),
      Arm.payload "Mutated" ObjectId.codec GameEvent.Mutated (\x -> case x of GameEvent.Mutated y -> Just y; _ -> Nothing),
      Arm.payload "Mentored" Mentored.codec GameEvent.Mentored (\x -> case x of GameEvent.Mentored y -> Just y; _ -> Nothing),
      Arm.payload "Exploited" Exploited.codec GameEvent.Exploited (\x -> case x of GameEvent.Exploited y -> Just y; _ -> Nothing),
      Arm.payload "Trained" ObjectId.codec GameEvent.Trained (\x -> case x of GameEvent.Trained y -> Just y; _ -> Nothing),
      Arm.payload "Convoked" Convoking.codec GameEvent.Convoked (\x -> case x of GameEvent.Convoked y -> Just y; _ -> Nothing),
      Arm.payload "Crewed" Crewing.codec GameEvent.Crewed (\x -> case x of GameEvent.Crewed y -> Just y; _ -> Nothing),
      Arm.payload "BecameCrewed" Crewing.codec GameEvent.BecameCrewed (\x -> case x of GameEvent.BecameCrewed y -> Just y; _ -> Nothing),
      Arm.payload "Saddled" Saddling.codec GameEvent.Saddled (\x -> case x of GameEvent.Saddled y -> Just y; _ -> Nothing),
      Arm.payload "PermanentSacrificed" PermanentWasSacrificed.codec GameEvent.PermanentSacrificed (\x -> case x of GameEvent.PermanentSacrificed y -> Just y; _ -> Nothing),
      Arm.payload "AbilityTriggered" AbilityTriggered.codec GameEvent.AbilityTriggered (\x -> case x of GameEvent.AbilityTriggered y -> Just y; _ -> Nothing),
      Arm.payload "ControlChanged" ControlChanged.codec GameEvent.ControlChanged (\x -> case x of GameEvent.ControlChanged y -> Just y; _ -> Nothing),
      Arm.payload "VentureMarkerEntered" VentureMarkerEntered.codec GameEvent.VentureMarkerEntered (\x -> case x of GameEvent.VentureMarkerEntered y -> Just y; _ -> Nothing),
      Arm.payload "BecameTarget" BecameTarget.codec GameEvent.BecameTarget (\x -> case x of GameEvent.BecameTarget y -> Just y; _ -> Nothing),
      Arm.payload "BecameAttached" BecameAttached.codec GameEvent.BecameAttached (\x -> case x of GameEvent.BecameAttached y -> Just y; _ -> Nothing),
      Arm.payload "BecameUnattached" BecameUnattached.codec GameEvent.BecameUnattached (\x -> case x of GameEvent.BecameUnattached y -> Just y; _ -> Nothing),
      Arm.payload "LeftTheGame" ObjectId.codec GameEvent.LeftTheGame (\x -> case x of GameEvent.LeftTheGame y -> Just y; _ -> Nothing),
      Arm.payload "Scried" PlayerId.codec GameEvent.Scried (\x -> case x of GameEvent.Scried y -> Just y; _ -> Nothing),
      -- CR 309.7's completion, with only the completing player on the wire: the
      -- rule names no card, so there is nothing else to carry.
      Arm.payload "DungeonCompleted" PlayerId.codec GameEvent.DungeonCompleted (\x -> case x of GameEvent.DungeonCompleted y -> Just y; _ -> Nothing),
      Arm.payload "Surveiled" PlayerId.codec GameEvent.Surveiled (\x -> case x of GameEvent.Surveiled y -> Just y; _ -> Nothing),
      Arm.payload "DiceRolled" PlayerId.codec GameEvent.DiceRolled (\x -> case x of GameEvent.DiceRolled y -> Just y; _ -> Nothing),
      Arm.payload "DieResultSettled" (DieResult.codec PlayerId.codec) GameEvent.DieResultSettled (\x -> case x of GameEvent.DieResultSettled y -> Just y; _ -> Nothing),
      Arm.payload "RolledToVisit" (DieResult.codec PlayerId.codec) GameEvent.RolledToVisit (\x -> case x of GameEvent.RolledToVisit y -> Just y; _ -> Nothing),
      Arm.payload "AttractionOpened" PlayerId.codec GameEvent.AttractionOpened (\x -> case x of GameEvent.AttractionOpened y -> Just y; _ -> Nothing),
      Arm.payload "PrizeClaimed" PlayerId.codec GameEvent.PrizeClaimed (\x -> case x of GameEvent.PrizeClaimed y -> Just y; _ -> Nothing),
      Arm.payload "ClassLevelSet" ClassLevelChange.codec GameEvent.ClassLevelSet (\x -> case x of GameEvent.ClassLevelSet y -> Just y; _ -> Nothing),
      Arm.payload "Plotted" ObjectId.codec GameEvent.Plotted (\x -> case x of GameEvent.Plotted y -> Just y; _ -> Nothing),
      Arm.payload "Explored" ObjectId.codec GameEvent.Explored (\x -> case x of GameEvent.Explored y -> Just y; _ -> Nothing),
      Arm.payload "Connived" ObjectId.codec GameEvent.Connived (\x -> case x of GameEvent.Connived y -> Just y; _ -> Nothing),
      Arm.payload "Exerted" ObjectId.codec GameEvent.Exerted (\x -> case x of GameEvent.Exerted y -> Just y; _ -> Nothing),
      Arm.payload "BecameTapped" ObjectId.codec GameEvent.BecameTapped (\x -> case x of GameEvent.BecameTapped y -> Just y; _ -> Nothing),
      Arm.payload "BecameUntapped" ObjectId.codec GameEvent.BecameUntapped (\x -> case x of GameEvent.BecameUntapped y -> Just y; _ -> Nothing),
      Arm.payload "TappedForMana" TappedForMana.codec GameEvent.TappedForMana (\x -> case x of GameEvent.TappedForMana y -> Just y; _ -> Nothing),
      Arm.payload "ManaAdded" ManaAdded.codec GameEvent.ManaAdded (\x -> case x of GameEvent.ManaAdded y -> Just y; _ -> Nothing),
      Arm.payload "ManaAbilityResolved" ManaAbilityResolved.codec GameEvent.ManaAbilityResolved (\x -> case x of GameEvent.ManaAbilityResolved y -> Just y; _ -> Nothing),
      Arm.payload "CoinFlipped" CoinFlipped.codec GameEvent.CoinFlipped (\x -> case x of GameEvent.CoinFlipped y -> Just y; _ -> Nothing),
      -- CR 701.54d. One player id, Scried's shape above: the rule names the
      -- tempted player and nothing else.
      Arm.payload "RingTempted" PlayerId.codec GameEvent.RingTempted (\x -> case x of GameEvent.RingTempted y -> Just y; _ -> Nothing),
      -- CR 701.68d. One player id, the arm above's shape: the rule names the
      -- blighting player and nothing else.
      Arm.payload "Blighted" PlayerId.codec GameEvent.Blighted (\x -> case x of GameEvent.Blighted y -> Just y; _ -> Nothing),
      -- CR 701.61a. One player id, the arm above's shape: the rule's two halves
      -- write their own Moved events, so the forager is all this carries.
      Arm.payload "Foraged" PlayerId.codec GameEvent.Foraged (\x -> case x of GameEvent.Foraged y -> Just y; _ -> Nothing),
      Arm.payload "Foretold" PlayerId.codec GameEvent.Foretold (\x -> case x of GameEvent.Foretold y -> Just y; _ -> Nothing),
      Arm.payload "CollectedEvidence" PlayerId.codec GameEvent.CollectedEvidence (\x -> case x of GameEvent.CollectedEvidence y -> Just y; _ -> Nothing),
      Arm.payload "GaveGift" PlayerId.codec GameEvent.GaveGift (\x -> case x of GameEvent.GaveGift y -> Just y; _ -> Nothing),
      Arm.payload "Earthbent" PlayerId.codec GameEvent.Earthbent (\x -> case x of GameEvent.Earthbent y -> Just y; _ -> Nothing),
      Arm.payload "Waterbent" PlayerId.codec GameEvent.Waterbent (\x -> case x of GameEvent.Waterbent y -> Just y; _ -> Nothing),
      Arm.payload "Airbent" PlayerId.codec GameEvent.Airbent (\x -> case x of GameEvent.Airbent y -> Just y; _ -> Nothing),
      Arm.payload "Firebent" PlayerId.codec GameEvent.Firebent (\x -> case x of GameEvent.Firebent y -> Just y; _ -> Nothing),
      -- CR 608.2n. The source object and the ability, which is the pair CR
      -- 707.10b counts by, so the payload is the same record Pawl.Types.Source's
      -- own OfAbility arm carries.
      Arm.payload "ActivatedAbilityResolved" ActivatedAbilitySource.codec GameEvent.ActivatedAbilityResolved (\x -> case x of GameEvent.ActivatedAbilityResolved y -> Just y; _ -> Nothing)
    ]

tagOf :: GameEvent.GameEvent -> String
tagOf x = case x of
  GameEvent.Moved {} -> "Moved"
  GameEvent.CardArrived {} -> "CardArrived"
  GameEvent.DamageDealt {} -> "DamageDealt"
  GameEvent.DamagePrevented {} -> "DamagePrevented"
  GameEvent.StepBegan {} -> "StepBegan"
  GameEvent.SpellCast {} -> "SpellCast"
  GameEvent.BecameMonarch {} -> "BecameMonarch"
  GameEvent.TookInitiative {} -> "TookInitiative"
  GameEvent.Discarded {} -> "Discarded"
  GameEvent.Milled {} -> "Milled"
  GameEvent.Drew {} -> "Drew"
  GameEvent.AttackerDeclared {} -> "AttackerDeclared"
  GameEvent.BecameAttacked {} -> "BecameAttacked"
  GameEvent.AttackersDeclared {} -> "AttackersDeclared"
  GameEvent.BecameBlocking {} -> "BecameBlocking"
  GameEvent.AttackerBlocked {} -> "AttackerBlocked"
  GameEvent.AttackerUnblocked {} -> "AttackerUnblocked"
  GameEvent.BlocksDeclared {} -> "BlocksDeclared"
  GameEvent.Revealed {} -> "Revealed"
  GameEvent.SpellCountered {} -> "SpellCountered"
  GameEvent.AbilityCountered {} -> "AbilityCountered"
  GameEvent.LifeLost {} -> "LifeLost"
  GameEvent.LifeGained {} -> "LifeGained"
  GameEvent.LoyaltyAbilityActivated {} -> "LoyaltyAbilityActivated"
  GameEvent.CountersPut {} -> "CountersPut"
  GameEvent.CountersRemoved {} -> "CountersRemoved"
  GameEvent.HalfUnlocked {} -> "HalfUnlocked"
  GameEvent.TurnedFaceUp {} -> "TurnedFaceUp"
  GameEvent.TurnedFaceDown {} -> "TurnedFaceDown"
  GameEvent.Transformed {} -> "Transformed"
  GameEvent.BecameDesignated {} -> "BecameDesignated"
  GameEvent.Evolved {} -> "Evolved"
  GameEvent.Mutated {} -> "Mutated"
  GameEvent.Mentored {} -> "Mentored"
  GameEvent.Exploited {} -> "Exploited"
  GameEvent.Trained {} -> "Trained"
  GameEvent.Convoked {} -> "Convoked"
  GameEvent.Saddled {} -> "Saddled"
  GameEvent.Crewed {} -> "Crewed"
  GameEvent.BecameCrewed {} -> "BecameCrewed"
  GameEvent.PermanentSacrificed {} -> "PermanentSacrificed"
  GameEvent.AbilityTriggered {} -> "AbilityTriggered"
  GameEvent.ControlChanged {} -> "ControlChanged"
  GameEvent.VentureMarkerEntered {} -> "VentureMarkerEntered"
  GameEvent.BecameTarget {} -> "BecameTarget"
  GameEvent.BecameAttached {} -> "BecameAttached"
  GameEvent.BecameUnattached {} -> "BecameUnattached"
  GameEvent.LeftTheGame {} -> "LeftTheGame"
  GameEvent.Scried {} -> "Scried"
  GameEvent.DungeonCompleted {} -> "DungeonCompleted"
  GameEvent.Surveiled {} -> "Surveiled"
  GameEvent.DiceRolled {} -> "DiceRolled"
  GameEvent.DieResultSettled {} -> "DieResultSettled"
  GameEvent.RolledToVisit {} -> "RolledToVisit"
  GameEvent.AttractionOpened {} -> "AttractionOpened"
  GameEvent.PrizeClaimed {} -> "PrizeClaimed"
  GameEvent.ClassLevelSet {} -> "ClassLevelSet"
  GameEvent.Plotted {} -> "Plotted"
  GameEvent.Explored {} -> "Explored"
  GameEvent.Connived {} -> "Connived"
  GameEvent.Exerted {} -> "Exerted"
  GameEvent.BecameTapped {} -> "BecameTapped"
  GameEvent.BecameUntapped {} -> "BecameUntapped"
  GameEvent.TappedForMana {} -> "TappedForMana"
  GameEvent.ManaAdded {} -> "ManaAdded"
  GameEvent.ManaAbilityResolved {} -> "ManaAbilityResolved"
  GameEvent.CoinFlipped {} -> "CoinFlipped"
  GameEvent.RingTempted {} -> "RingTempted"
  GameEvent.Blighted {} -> "Blighted"
  GameEvent.Foraged {} -> "Foraged"
  GameEvent.Foretold {} -> "Foretold"
  GameEvent.CollectedEvidence {} -> "CollectedEvidence"
  GameEvent.GaveGift {} -> "GaveGift"
  GameEvent.Earthbent {} -> "Earthbent"
  GameEvent.Waterbent {} -> "Waterbent"
  GameEvent.Airbent {} -> "Airbent"
  GameEvent.Firebent {} -> "Firebent"
  GameEvent.ActivatedAbilityResolved {} -> "ActivatedAbilityResolved"
