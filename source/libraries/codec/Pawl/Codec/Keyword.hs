module Pawl.Codec.Keyword where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Cycling as Cycling
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Morph as Morph
import qualified Pawl.Codec.Reinforce as Reinforce
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Keyword as Keyword

-- | This module TIES THE CODEC KNOT that Pawl.Codec.Filter's keyword parameter
-- opens, exactly as Pawl.Types.Keyword ties the data-type one: 'codec' is
-- defined partly as @Filter.codec codec@ and @Cost.codec codec@, a
-- self-referential top-level binding that Haskell's laziness ties at the
-- value level. 'Pawl.JsonSchema.Define.define' ties the same knot at the
-- schema level, by registering "Keyword" before running its body, so the
-- re-entrant calls inside 'Filter.codec' and 'Cost.codec' find it already
-- registered and return a @$ref@ instead of recursing forever.
--
-- 'Keyword.Hexproof' is 'Arm.optionalPayload'\'s first caller: CR 702.11b's
-- bare hexproof has no "value" key at all (@{"type":"Hexproof"}@) where CR
-- 702.11d's "hexproof from [quality]" does, both under the one constructor.
codec :: Codec.Codec Keyword.Keyword
codec =
  Arm.tagged
    [ Arm.nullary "Deathtouch" Keyword.Deathtouch,
      Arm.nullary "Defender" Keyword.Defender,
      Arm.nullary "DoubleStrike" Keyword.DoubleStrike,
      Arm.nullary "FirstStrike" Keyword.FirstStrike,
      Arm.nullary "Flash" Keyword.Flash,
      Arm.nullary "Flying" Keyword.Flying,
      Arm.nullary "Haste" Keyword.Haste,
      Arm.optionalPayload "Hexproof" (Filter.codec codec) Keyword.Hexproof (\x -> case x of Keyword.Hexproof y -> Just y; _ -> Nothing),
      Arm.nullary "Indestructible" Keyword.Indestructible,
      Arm.payload "Landwalk" (Filter.codec codec) Keyword.Landwalk (\x -> case x of Keyword.Landwalk y -> Just y; _ -> Nothing),
      Arm.nullary "Lifelink" Keyword.Lifelink,
      Arm.nullary "Reach" Keyword.Reach,
      Arm.nullary "Shroud" Keyword.Shroud,
      Arm.nullary "Trample" Keyword.Trample,
      Arm.nullary "TrampleOverPlaneswalkers" Keyword.TrampleOverPlaneswalkers,
      Arm.nullary "Vigilance" Keyword.Vigilance,
      Arm.nullary "Banding" Keyword.Banding,
      Arm.payload "Rampage" Common.natural Keyword.Rampage (\x -> case x of Keyword.Rampage y -> Just y; _ -> Nothing),
      Arm.nullary "Flanking" Keyword.Flanking,
      Arm.nullary "Phasing" Keyword.Phasing,
      Arm.nullary "Shadow" Keyword.Shadow,
      Arm.nullary "Horsemanship" Keyword.Horsemanship,
      Arm.nullary "Aftermath" Keyword.Aftermath,
      Arm.nullary "JumpStart" Keyword.JumpStart,
      Arm.payload "Afflict" Common.natural Keyword.Afflict (\x -> case x of Keyword.Afflict y -> Just y; _ -> Nothing),
      -- CR 702.29e's typecycling filter, absent for plain cycling: 'Common.maybe'
      -- writes JSON null rather than omitting a key, which is right here because
      -- this rides inside a POSITIONAL pair (the tuple's second slot) rather than
      -- a named field an absent key could skip.
      Arm.payload "Cycling" (Cycling.codec codec) Keyword.Cycling (\x -> case x of Keyword.Cycling y -> Just y; _ -> Nothing),
      Arm.payload "Fading" Common.natural Keyword.Fading (\x -> case x of Keyword.Fading y -> Just y; _ -> Nothing),
      Arm.payload "Kicker" (Cost.codec codec) Keyword.Kicker (\x -> case x of Keyword.Kicker y -> Just y; _ -> Nothing),
      Arm.payload "Flashback" (Cost.codec codec) Keyword.Flashback (\x -> case x of Keyword.Flashback y -> Just y; _ -> Nothing),
      Arm.nullary "Fear" Keyword.Fear,
      Arm.nullary "Intimidate" Keyword.Intimidate,
      Arm.payload "Morph" (Morph.codec codec) Keyword.Morph (\x -> case x of Keyword.Morph y -> Just y; _ -> Nothing),
      Arm.payload "Entwine" (Cost.codec codec) Keyword.Entwine (\x -> case x of Keyword.Entwine y -> Just y; _ -> Nothing),
      Arm.payload "Modular" Common.natural Keyword.Modular (\x -> case x of Keyword.Modular y -> Just y; _ -> Nothing),
      Arm.payload "Bushido" Common.natural Keyword.Bushido (\x -> case x of Keyword.Bushido y -> Just y; _ -> Nothing),
      Arm.payload "Soulshift" Common.natural Keyword.Soulshift (\x -> case x of Keyword.Soulshift y -> Just y; _ -> Nothing),
      Arm.nullary "Haunt" Keyword.Haunt,
      Arm.nullary "SplitSecond" Keyword.SplitSecond,
      Arm.payload "Vanishing" Common.natural Keyword.Vanishing (\x -> case x of Keyword.Vanishing y -> Just y; _ -> Nothing),
      Arm.payload "Poisonous" Common.natural Keyword.Poisonous (\x -> case x of Keyword.Poisonous y -> Just y; _ -> Nothing),
      Arm.payload "Annihilator" Common.natural Keyword.Annihilator (\x -> case x of Keyword.Annihilator y -> Just y; _ -> Nothing),
      Arm.payload "Reinforce" (Reinforce.codec codec) Keyword.Reinforce (\x -> case x of Keyword.Reinforce y -> Just y; _ -> Nothing),
      Arm.nullary "Persist" Keyword.Persist,
      Arm.nullary "Infect" Keyword.Infect,
      Arm.nullary "Wither" Keyword.Wither,
      Arm.nullary "Exalted" Keyword.Exalted,
      Arm.nullary "Mentor" Keyword.Mentor,
      Arm.payload "Afterlife" Common.natural Keyword.Afterlife (\x -> case x of Keyword.Afterlife y -> Just y; _ -> Nothing),
      Arm.nullary "Provoke" Keyword.Provoke,
      Arm.nullary "BattleCry" Keyword.BattleCry,
      Arm.nullary "Undying" Keyword.Undying,
      Arm.nullary "Evolve" Keyword.Evolve,
      Arm.nullary "Dethrone" Keyword.Dethrone,
      Arm.payload "Outlast" (Cost.codec codec) Keyword.Outlast (\x -> case x of Keyword.Outlast y -> Just y; _ -> Nothing),
      Arm.nullary "Prowess" Keyword.Prowess,
      Arm.nullary "Menace" Keyword.Menace,
      Arm.payload "Renown" Common.natural Keyword.Renown (\x -> case x of Keyword.Renown y -> Just y; _ -> Nothing),
      Arm.nullary "Changeling" Keyword.Changeling,
      Arm.nullary "Devoid" Keyword.Devoid,
      Arm.nullary "Ingest" Keyword.Ingest,
      Arm.nullary "Skulk" Keyword.Skulk,
      Arm.nullary "Melee" Keyword.Melee,
      Arm.payload "Crew" Common.natural Keyword.Crew (\x -> case x of Keyword.Crew y -> Just y; _ -> Nothing),
      Arm.payload "Fabricate" Common.natural Keyword.Fabricate (\x -> case x of Keyword.Fabricate y -> Just y; _ -> Nothing),
      Arm.nullary "Riot" Keyword.Riot,
      Arm.nullary "Unleash" Keyword.Unleash,
      Arm.nullary "Daybound" Keyword.Daybound,
      Arm.nullary "Nightbound" Keyword.Nightbound,
      Arm.nullary "Decayed" Keyword.Decayed,
      Arm.nullary "Training" Keyword.Training,
      Arm.payload "Miracle" (Cost.codec codec) Keyword.Miracle (\x -> case x of Keyword.Miracle y -> Just y; _ -> Nothing),
      Arm.payload "Toxic" Common.natural Keyword.Toxic (\x -> case x of Keyword.Toxic y -> Just y; _ -> Nothing),
      Arm.payload "Plot" (Cost.codec codec) Keyword.Plot (\x -> case x of Keyword.Plot y -> Just y; _ -> Nothing),
      Arm.payload "Foretell" (Cost.codec codec) Keyword.Foretell (\x -> case x of Keyword.Foretell y -> Just y; _ -> Nothing),
      Arm.nullary "StartYourEngines" Keyword.StartYourEngines
    ]
