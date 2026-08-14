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
    encode
    [ Arm.nullary "Deathtouch" Keyword.Deathtouch,
      Arm.nullary "Defender" Keyword.Defender,
      Arm.nullary "DoubleStrike" Keyword.DoubleStrike,
      Arm.nullary "FirstStrike" Keyword.FirstStrike,
      Arm.nullary "Flash" Keyword.Flash,
      Arm.nullary "Flying" Keyword.Flying,
      Arm.nullary "Haste" Keyword.Haste,
      Arm.optionalPayload "Hexproof" (Filter.codec codec) Keyword.Hexproof,
      Arm.nullary "Indestructible" Keyword.Indestructible,
      Arm.payload "Landwalk" (Filter.codec codec) Keyword.Landwalk,
      Arm.nullary "Lifelink" Keyword.Lifelink,
      Arm.nullary "Reach" Keyword.Reach,
      Arm.nullary "Shroud" Keyword.Shroud,
      Arm.nullary "Trample" Keyword.Trample,
      Arm.nullary "TrampleOverPlaneswalkers" Keyword.TrampleOverPlaneswalkers,
      Arm.nullary "Vigilance" Keyword.Vigilance,
      Arm.nullary "Banding" Keyword.Banding,
      Arm.payload "Rampage" Common.natural Keyword.Rampage,
      Arm.nullary "Flanking" Keyword.Flanking,
      Arm.nullary "Phasing" Keyword.Phasing,
      Arm.nullary "Shadow" Keyword.Shadow,
      Arm.nullary "Horsemanship" Keyword.Horsemanship,
      Arm.nullary "Aftermath" Keyword.Aftermath,
      Arm.nullary "JumpStart" Keyword.JumpStart,
      Arm.payload "Afflict" Common.natural Keyword.Afflict,
      -- CR 702.29e's typecycling filter, absent for plain cycling: 'Common.maybe'
      -- writes JSON null rather than omitting a key, which is right here because
      -- this rides inside a POSITIONAL pair (the tuple's second slot) rather than
      -- a named field an absent key could skip.
      Arm.payload "Cycling" (Cycling.codec codec) Keyword.Cycling,
      Arm.payload "Kicker" (Cost.codec codec) Keyword.Kicker,
      Arm.payload "Flashback" (Cost.codec codec) Keyword.Flashback,
      Arm.nullary "Fear" Keyword.Fear,
      Arm.nullary "Intimidate" Keyword.Intimidate,
      Arm.payload "Morph" (Morph.codec codec) Keyword.Morph,
      Arm.payload "Entwine" (Cost.codec codec) Keyword.Entwine,
      Arm.payload "Modular" Common.natural Keyword.Modular,
      Arm.payload "Bushido" Common.natural Keyword.Bushido,
      Arm.payload "Soulshift" Common.natural Keyword.Soulshift,
      Arm.nullary "Haunt" Keyword.Haunt,
      Arm.nullary "SplitSecond" Keyword.SplitSecond,
      Arm.payload "Vanishing" Common.natural Keyword.Vanishing,
      Arm.payload "Poisonous" Common.natural Keyword.Poisonous,
      Arm.payload "Annihilator" Common.natural Keyword.Annihilator,
      Arm.payload "Reinforce" (Reinforce.codec codec) Keyword.Reinforce,
      Arm.nullary "Persist" Keyword.Persist,
      Arm.nullary "Infect" Keyword.Infect,
      Arm.nullary "Wither" Keyword.Wither,
      Arm.nullary "Exalted" Keyword.Exalted,
      Arm.nullary "Mentor" Keyword.Mentor,
      Arm.payload "Afterlife" Common.natural Keyword.Afterlife,
      Arm.nullary "Provoke" Keyword.Provoke,
      Arm.nullary "BattleCry" Keyword.BattleCry,
      Arm.nullary "Undying" Keyword.Undying,
      Arm.nullary "Evolve" Keyword.Evolve,
      Arm.nullary "Dethrone" Keyword.Dethrone,
      Arm.payload "Outlast" (Cost.codec codec) Keyword.Outlast,
      Arm.nullary "Prowess" Keyword.Prowess,
      Arm.nullary "Menace" Keyword.Menace,
      Arm.payload "Renown" Common.natural Keyword.Renown,
      Arm.nullary "Changeling" Keyword.Changeling,
      Arm.nullary "Devoid" Keyword.Devoid,
      Arm.nullary "Ingest" Keyword.Ingest,
      Arm.nullary "Skulk" Keyword.Skulk,
      Arm.nullary "Melee" Keyword.Melee,
      Arm.payload "Crew" Common.natural Keyword.Crew,
      Arm.payload "Fabricate" Common.natural Keyword.Fabricate,
      Arm.nullary "Riot" Keyword.Riot,
      Arm.nullary "Unleash" Keyword.Unleash,
      Arm.nullary "Daybound" Keyword.Daybound,
      Arm.nullary "Nightbound" Keyword.Nightbound,
      Arm.nullary "Decayed" Keyword.Decayed,
      Arm.nullary "Training" Keyword.Training,
      Arm.payload "Miracle" (Cost.codec codec) Keyword.Miracle,
      Arm.payload "Toxic" Common.natural Keyword.Toxic,
      Arm.payload "Plot" (Cost.codec codec) Keyword.Plot,
      Arm.nullary "StartYourEngines" Keyword.StartYourEngines
    ]
  where
    encode k = case k of
      Keyword.Deathtouch -> Common.nullary "Deathtouch"
      Keyword.Defender -> Common.nullary "Defender"
      Keyword.DoubleStrike -> Common.nullary "DoubleStrike"
      Keyword.FirstStrike -> Common.nullary "FirstStrike"
      Keyword.Flash -> Common.nullary "Flash"
      Keyword.Flying -> Common.nullary "Flying"
      Keyword.Haste -> Common.nullary "Haste"
      -- CR 702.11b encodes as the bare tag and CR 702.11d as the tag plus its
      -- quality, rather than both carrying an explicit null: rule 702.11b's ability
      -- takes no parameter, so the card that prints it should say no more than
      -- Shroud's does.
      Keyword.Hexproof Nothing -> Common.nullary "Hexproof"
      Keyword.Hexproof (Just quality) -> Common.tagged "Hexproof" . Just $ Codec.encode (Filter.codec codec) quality
      Keyword.Indestructible -> Common.nullary "Indestructible"
      Keyword.Landwalk criterion -> Common.tagged "Landwalk" . Just $ Codec.encode (Filter.codec codec) criterion
      Keyword.Lifelink -> Common.nullary "Lifelink"
      Keyword.Reach -> Common.nullary "Reach"
      Keyword.Shroud -> Common.nullary "Shroud"
      Keyword.Trample -> Common.nullary "Trample"
      Keyword.TrampleOverPlaneswalkers -> Common.nullary "TrampleOverPlaneswalkers"
      Keyword.Vigilance -> Common.nullary "Vigilance"
      Keyword.Banding -> Common.nullary "Banding"
      Keyword.Rampage n -> Common.tagged "Rampage" . Just $ Common.encodeNatural n
      Keyword.Flanking -> Common.nullary "Flanking"
      Keyword.Phasing -> Common.nullary "Phasing"
      Keyword.Shadow -> Common.nullary "Shadow"
      Keyword.Horsemanship -> Common.nullary "Horsemanship"
      Keyword.Aftermath -> Common.nullary "Aftermath"
      Keyword.JumpStart -> Common.nullary "JumpStart"
      Keyword.Afflict n -> Common.tagged "Afflict" . Just $ Common.encodeNatural n
      Keyword.Cycling x -> Common.tagged "Cycling" . Just $ Codec.encode (Cycling.codec codec) x
      Keyword.Kicker cost -> Common.tagged "Kicker" . Just $ Codec.encode (Cost.codec codec) cost
      Keyword.Flashback cost -> Common.tagged "Flashback" . Just $ Codec.encode (Cost.codec codec) cost
      Keyword.Fear -> Common.nullary "Fear"
      Keyword.Intimidate -> Common.nullary "Intimidate"
      -- An ARRAY, as Cycling's is, because CR 702.37b's megamorph is the same
      -- keyword with a second field rather than a tag of its own.
      Keyword.Morph x -> Common.tagged "Morph" . Just $ Codec.encode (Morph.codec codec) x
      Keyword.Entwine cost -> Common.tagged "Entwine" . Just $ Codec.encode (Cost.codec codec) cost
      Keyword.Modular n -> Common.tagged "Modular" . Just $ Common.encodeNatural n
      Keyword.Bushido n -> Common.tagged "Bushido" . Just $ Common.encodeNatural n
      Keyword.Soulshift n -> Common.tagged "Soulshift" . Just $ Common.encodeNatural n
      Keyword.Haunt -> Common.nullary "Haunt"
      Keyword.SplitSecond -> Common.nullary "SplitSecond"
      Keyword.Vanishing n -> Common.tagged "Vanishing" . Just $ Common.encodeNatural n
      Keyword.Poisonous n -> Common.tagged "Poisonous" . Just $ Common.encodeNatural n
      Keyword.Annihilator n -> Common.tagged "Annihilator" . Just $ Common.encodeNatural n
      -- An ARRAY, as Cycling's and Morph's are: CR 702.77a writes both an N and a
      -- cost.
      Keyword.Reinforce x -> Common.tagged "Reinforce" . Just $ Codec.encode (Reinforce.codec codec) x
      Keyword.Persist -> Common.nullary "Persist"
      Keyword.Infect -> Common.nullary "Infect"
      Keyword.Wither -> Common.nullary "Wither"
      Keyword.Exalted -> Common.nullary "Exalted"
      Keyword.Mentor -> Common.nullary "Mentor"
      Keyword.Afterlife n -> Common.tagged "Afterlife" . Just $ Common.encodeNatural n
      Keyword.Provoke -> Common.nullary "Provoke"
      Keyword.BattleCry -> Common.nullary "BattleCry"
      Keyword.Undying -> Common.nullary "Undying"
      Keyword.Evolve -> Common.nullary "Evolve"
      Keyword.Dethrone -> Common.nullary "Dethrone"
      Keyword.Outlast cost -> Common.tagged "Outlast" . Just $ Codec.encode (Cost.codec codec) cost
      Keyword.Prowess -> Common.nullary "Prowess"
      Keyword.Menace -> Common.nullary "Menace"
      Keyword.Renown n -> Common.tagged "Renown" . Just $ Common.encodeNatural n
      Keyword.Changeling -> Common.nullary "Changeling"
      Keyword.Devoid -> Common.nullary "Devoid"
      Keyword.Ingest -> Common.nullary "Ingest"
      Keyword.Skulk -> Common.nullary "Skulk"
      Keyword.Melee -> Common.nullary "Melee"
      Keyword.Crew n -> Common.tagged "Crew" . Just $ Common.encodeNatural n
      Keyword.Fabricate n -> Common.tagged "Fabricate" . Just $ Common.encodeNatural n
      Keyword.Riot -> Common.nullary "Riot"
      Keyword.Unleash -> Common.nullary "Unleash"
      Keyword.Daybound -> Common.nullary "Daybound"
      Keyword.Nightbound -> Common.nullary "Nightbound"
      Keyword.Decayed -> Common.nullary "Decayed"
      Keyword.Training -> Common.nullary "Training"
      Keyword.Miracle cost -> Common.tagged "Miracle" . Just $ Codec.encode (Cost.codec codec) cost
      Keyword.Toxic n -> Common.tagged "Toxic" . Just $ Common.encodeNatural n
      Keyword.Plot cost -> Common.tagged "Plot" . Just $ Codec.encode (Cost.codec codec) cost
      Keyword.StartYourEngines -> Common.nullary "StartYourEngines"
