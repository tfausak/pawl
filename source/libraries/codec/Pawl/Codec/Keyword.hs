module Pawl.Codec.Keyword where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Craft as Craft
import qualified Pawl.Codec.Cycling as Cycling
import qualified Pawl.Codec.Devour as Devour
import qualified Pawl.Codec.Emerge as Emerge
import qualified Pawl.Codec.Equip as Equip
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Gift as Gift
import qualified Pawl.Codec.Morph as Morph
import qualified Pawl.Codec.PartnerText as PartnerText
import qualified Pawl.Codec.Protection as Protection
import qualified Pawl.Codec.Prototype as Prototype
import qualified Pawl.Codec.Reinforce as Reinforce
import qualified Pawl.Codec.Suspend as Suspend
import qualified Pawl.Codec.Ward as Ward
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
-- 'Keyword.Vanishing' is the same shape for CR 702.63b's sake.
codec :: Codec.Codec Keyword.Keyword
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Deathtouch" Keyword.Deathtouch,
      Arm.nullary "Defender" Keyword.Defender,
      Arm.nullary "DoubleStrike" Keyword.DoubleStrike,
      Arm.payload "Equip" (Equip.codec codec) Keyword.Equip (\x -> case x of Keyword.Equip y -> Just y; _ -> Nothing),
      Arm.nullary "FirstStrike" Keyword.FirstStrike,
      Arm.nullary "Flash" Keyword.Flash,
      Arm.nullary "Flying" Keyword.Flying,
      Arm.nullary "Haste" Keyword.Haste,
      Arm.optionalPayload "Hexproof" (Filter.codec codec) Keyword.Hexproof (\x -> case x of Keyword.Hexproof y -> Just y; _ -> Nothing),
      Arm.nullary "Indestructible" Keyword.Indestructible,
      Arm.payload "Landwalk" (Filter.codec codec) Keyword.Landwalk (\x -> case x of Keyword.Landwalk y -> Just y; _ -> Nothing),
      Arm.nullary "Lifelink" Keyword.Lifelink,
      Arm.nullary "LivingMetal" Keyword.LivingMetal,
      Arm.payload "MoreThanMeetsTheEye" (Cost.codec codec) Keyword.MoreThanMeetsTheEye (\x -> case x of Keyword.MoreThanMeetsTheEye y -> Just y; _ -> Nothing),
      Arm.payload "Protection" (Protection.codec codec) Keyword.Protection (\x -> case x of Keyword.Protection y -> Just y; _ -> Nothing),
      Arm.nullary "Reach" Keyword.Reach,
      Arm.nullary "Shroud" Keyword.Shroud,
      Arm.nullary "Trample" Keyword.Trample,
      Arm.nullary "TrampleOverPlaneswalkers" Keyword.TrampleOverPlaneswalkers,
      Arm.nullary "Vigilance" Keyword.Vigilance,
      Arm.payload "Ward" (Ward.codec codec) Keyword.Ward (\x -> case x of Keyword.Ward y -> Just y; _ -> Nothing),
      Arm.nullary "Banding" Keyword.Banding,
      Arm.payload "Rampage" Common.natural Keyword.Rampage (\x -> case x of Keyword.Rampage y -> Just y; _ -> Nothing),
      Arm.payload "CumulativeUpkeep" (Cost.codec codec) Keyword.CumulativeUpkeep (\x -> case x of Keyword.CumulativeUpkeep y -> Just y; _ -> Nothing),
      Arm.payload "Echo" (Cost.codec codec) Keyword.Echo (\x -> case x of Keyword.Echo y -> Just y; _ -> Nothing),
      Arm.nullary "Flanking" Keyword.Flanking,
      Arm.nullary "Phasing" Keyword.Phasing,
      Arm.payload "Buyback" (Cost.codec codec) Keyword.Buyback (\x -> case x of Keyword.Buyback y -> Just y; _ -> Nothing),
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
      Arm.payload "Multikicker" (Cost.codec codec) Keyword.Multikicker (\x -> case x of Keyword.Multikicker y -> Just y; _ -> Nothing),
      Arm.payload "Flashback" (Cost.codec codec) Keyword.Flashback (\x -> case x of Keyword.Flashback y -> Just y; _ -> Nothing),
      Arm.nullary "Fear" Keyword.Fear,
      Arm.nullary "Intimidate" Keyword.Intimidate,
      Arm.payload "Morph" (Morph.codec codec) Keyword.Morph (\x -> case x of Keyword.Morph y -> Just y; _ -> Nothing),
      Arm.payload "Amplify" Common.natural Keyword.Amplify (\x -> case x of Keyword.Amplify y -> Just y; _ -> Nothing),
      Arm.payload "Entwine" (Cost.codec codec) Keyword.Entwine (\x -> case x of Keyword.Entwine y -> Just y; _ -> Nothing),
      Arm.payload "Modular" Common.natural Keyword.Modular (\x -> case x of Keyword.Modular y -> Just y; _ -> Nothing),
      Arm.nullary "Sunburst" Keyword.Sunburst,
      Arm.payload "Dredge" Common.natural Keyword.Dredge (\x -> case x of Keyword.Dredge y -> Just y; _ -> Nothing),
      Arm.payload "Bushido" Common.natural Keyword.Bushido (\x -> case x of Keyword.Bushido y -> Just y; _ -> Nothing),
      Arm.payload "Soulshift" Common.natural Keyword.Soulshift (\x -> case x of Keyword.Soulshift y -> Just y; _ -> Nothing),
      -- Rule 702.54b's bloodthirst X is the ABSENT "value" key, vanishing's
      -- spelling below and for its reason: the two forms are one constructor, and
      -- rule 702.54a's N form (Bloodrage Vampire's
      -- @{"type":"Bloodthirst","value":1}@) is untouched by this arm being
      -- optional.
      Arm.optionalPayload "Bloodthirst" Common.natural Keyword.Bloodthirst (\x -> case x of Keyword.Bloodthirst y -> Just y; _ -> Nothing),
      Arm.nullary "Haunt" Keyword.Haunt,
      Arm.nullary "SplitSecond" Keyword.SplitSecond,
      Arm.payload "Suspend" (Suspend.codec codec) Keyword.Suspend (\x -> case x of Keyword.Suspend y -> Just y; _ -> Nothing),
      -- CR 702.63b's numberless vanishing is the ABSENT "value" key, hexproof's
      -- spelling and for its reason: the two forms are one constructor, and the
      -- N-carrying form (Waning Wurm's @{"type":"Vanishing","value":2}@) is
      -- untouched by this arm being optional.
      Arm.optionalPayload "Vanishing" Common.natural Keyword.Vanishing (\x -> case x of Keyword.Vanishing y -> Just y; _ -> Nothing),
      Arm.payload "Fortify" (Cost.codec codec) Keyword.Fortify (\x -> case x of Keyword.Fortify y -> Just y; _ -> Nothing),
      Arm.payload "Ninjutsu" (Cost.codec codec) Keyword.Ninjutsu (\x -> case x of Keyword.Ninjutsu y -> Just y; _ -> Nothing),
      Arm.payload "Frenzy" Common.natural Keyword.Frenzy (\x -> case x of Keyword.Frenzy y -> Just y; _ -> Nothing),
      Arm.payload "Poisonous" Common.natural Keyword.Poisonous (\x -> case x of Keyword.Poisonous y -> Just y; _ -> Nothing),
      Arm.payload "Champion" (Filter.codec codec) Keyword.Champion (\x -> case x of Keyword.Champion y -> Just y; _ -> Nothing),
      Arm.payload "Annihilator" Common.natural Keyword.Annihilator (\x -> case x of Keyword.Annihilator y -> Just y; _ -> Nothing),
      Arm.nullary "Cascade" Keyword.Cascade,
      Arm.nullary "Storm" Keyword.Storm,
      Arm.nullary "Gravestorm" Keyword.Gravestorm,
      Arm.nullary "Conspire" Keyword.Conspire,
      Arm.payload "Affinity" (Filter.codec codec) Keyword.Affinity (\x -> case x of Keyword.Affinity y -> Just y; _ -> Nothing),
      Arm.payload "Cleave" (Cost.codec codec) Keyword.Cleave (\x -> case x of Keyword.Cleave y -> Just y; _ -> Nothing),
      Arm.payload "Awaken" (Cost.codec codec) Keyword.Awaken (\x -> case x of Keyword.Awaken y -> Just y; _ -> Nothing),
      Arm.payload "Emerge" (Emerge.codec codec) Keyword.Emerge (\x -> case x of Keyword.Emerge y -> Just y; _ -> Nothing),
      Arm.payload "Evoke" (Cost.codec codec) Keyword.Evoke (\x -> case x of Keyword.Evoke y -> Just y; _ -> Nothing),
      Arm.payload "Dash" (Cost.codec codec) Keyword.Dash (\x -> case x of Keyword.Dash y -> Just y; _ -> Nothing),
      Arm.payload "Blitz" (Cost.codec codec) Keyword.Blitz (\x -> case x of Keyword.Blitz y -> Just y; _ -> Nothing),
      Arm.payload "Warp" (Cost.codec codec) Keyword.Warp (\x -> case x of Keyword.Warp y -> Just y; _ -> Nothing),
      Arm.payload "Surge" (Cost.codec codec) Keyword.Surge (\x -> case x of Keyword.Surge y -> Just y; _ -> Nothing),
      Arm.payload "Prowl" (Cost.codec codec) Keyword.Prowl (\x -> case x of Keyword.Prowl y -> Just y; _ -> Nothing),
      Arm.payload "Freerunning" (Cost.codec codec) Keyword.Freerunning (\x -> case x of Keyword.Freerunning y -> Just y; _ -> Nothing),
      Arm.payload "Spectacle" (Cost.codec codec) Keyword.Spectacle (\x -> case x of Keyword.Spectacle y -> Just y; _ -> Nothing),
      Arm.nullary "Ravenous" Keyword.Ravenous,
      Arm.payload "Squad" (Cost.codec codec) Keyword.Squad (\x -> case x of Keyword.Squad y -> Just y; _ -> Nothing),
      Arm.payload "Offspring" (Cost.codec codec) Keyword.Offspring (\x -> case x of Keyword.Offspring y -> Just y; _ -> Nothing),
      Arm.payload "Gift" Gift.codec Keyword.Gift (\x -> case x of Keyword.Gift y -> Just y; _ -> Nothing),
      Arm.payload "Replicate" (Cost.codec codec) Keyword.Replicate (\x -> case x of Keyword.Replicate y -> Just y; _ -> Nothing),
      Arm.payload "Graft" Common.natural Keyword.Graft (\x -> case x of Keyword.Graft y -> Just y; _ -> Nothing),
      Arm.payload "Absorb" Common.natural Keyword.Absorb (\x -> case x of Keyword.Absorb y -> Just y; _ -> Nothing),
      Arm.payload "Recover" (Cost.codec codec) Keyword.Recover (\x -> case x of Keyword.Recover y -> Just y; _ -> Nothing),
      Arm.payload "Ripple" Common.natural Keyword.Ripple (\x -> case x of Keyword.Ripple y -> Just y; _ -> Nothing),
      Arm.payload "Casualty" Common.natural Keyword.Casualty (\x -> case x of Keyword.Casualty y -> Just y; _ -> Nothing),
      Arm.payload "Teamwork" Common.natural Keyword.Teamwork (\x -> case x of Keyword.Teamwork y -> Just y; _ -> Nothing),
      Arm.payload "WebSlinging" (Cost.codec codec) Keyword.WebSlinging (\x -> case x of Keyword.WebSlinging y -> Just y; _ -> Nothing),
      Arm.payload "Sneak" (Cost.codec codec) Keyword.Sneak (\x -> case x of Keyword.Sneak y -> Just y; _ -> Nothing),
      Arm.payload "Hideaway" Common.natural Keyword.Hideaway (\x -> case x of Keyword.Hideaway y -> Just y; _ -> Nothing),
      Arm.payload "Reinforce" (Reinforce.codec codec) Keyword.Reinforce (\x -> case x of Keyword.Reinforce y -> Just y; _ -> Nothing),
      Arm.nullary "Persist" Keyword.Persist,
      Arm.nullary "Infect" Keyword.Infect,
      Arm.nullary "Wither" Keyword.Wither,
      Arm.payload "Devour" (Devour.codec codec) Keyword.Devour (\x -> case x of Keyword.Devour y -> Just y; _ -> Nothing),
      Arm.nullary "Exalted" Keyword.Exalted,
      Arm.payload "Unearth" (Cost.codec codec) Keyword.Unearth (\x -> case x of Keyword.Unearth y -> Just y; _ -> Nothing),
      Arm.payload "Embalm" (Cost.codec codec) Keyword.Embalm (\x -> case x of Keyword.Embalm y -> Just y; _ -> Nothing),
      Arm.payload "Eternalize" (Cost.codec codec) Keyword.Eternalize (\x -> case x of Keyword.Eternalize y -> Just y; _ -> Nothing),
      Arm.nullary "Mentor" Keyword.Mentor,
      Arm.payload "Afterlife" Common.natural Keyword.Afterlife (\x -> case x of Keyword.Afterlife y -> Just y; _ -> Nothing),
      Arm.nullary "Provoke" Keyword.Provoke,
      Arm.nullary "BattleCry" Keyword.BattleCry,
      Arm.nullary "LivingWeapon" Keyword.LivingWeapon,
      Arm.nullary "Undying" Keyword.Undying,
      Arm.nullary "Evolve" Keyword.Evolve,
      Arm.nullary "Dethrone" Keyword.Dethrone,
      Arm.nullary "Extort" Keyword.Extort,
      Arm.nullary "Fuse" Keyword.Fuse,
      Arm.nullary "Increment" Keyword.Increment,
      Arm.payload "LevelUp" (Cost.codec codec) Keyword.LevelUp (\x -> case x of Keyword.LevelUp y -> Just y; _ -> Nothing),
      Arm.payload "Outlast" (Cost.codec codec) Keyword.Outlast (\x -> case x of Keyword.Outlast y -> Just y; _ -> Nothing),
      Arm.nullary "Prowess" Keyword.Prowess,
      Arm.nullary "Exploit" Keyword.Exploit,
      Arm.nullary "Menace" Keyword.Menace,
      Arm.payload "Renown" Common.natural Keyword.Renown (\x -> case x of Keyword.Renown y -> Just y; _ -> Nothing),
      Arm.nullary "Changeling" Keyword.Changeling,
      Arm.nullary "Devoid" Keyword.Devoid,
      Arm.nullary "Ingest" Keyword.Ingest,
      Arm.nullary "Myriad" Keyword.Myriad,
      Arm.nullary "Skulk" Keyword.Skulk,
      Arm.payload "Escalate" (Cost.codec codec) Keyword.Escalate (\x -> case x of Keyword.Escalate y -> Just y; _ -> Nothing),
      Arm.nullary "Melee" Keyword.Melee,
      Arm.payload "Crew" Common.natural Keyword.Crew (\x -> case x of Keyword.Crew y -> Just y; _ -> Nothing),
      Arm.payload "Saddle" Common.natural Keyword.Saddle (\x -> case x of Keyword.Saddle y -> Just y; _ -> Nothing),
      Arm.payload "Fabricate" Common.natural Keyword.Fabricate (\x -> case x of Keyword.Fabricate y -> Just y; _ -> Nothing),
      Arm.nullary "Partner" Keyword.Partner,
      Arm.payload "PartnerText" PartnerText.codec Keyword.PartnerText (\x -> case x of Keyword.PartnerText y -> Just y; _ -> Nothing),
      Arm.payload "PartnerWith" CardName.codec Keyword.PartnerWith (\x -> case x of Keyword.PartnerWith y -> Just y; _ -> Nothing),
      Arm.nullary "ChooseABackground" Keyword.ChooseABackground,
      Arm.nullary "DoctorsCompanion" Keyword.DoctorsCompanion,
      Arm.nullary "Riot" Keyword.Riot,
      Arm.nullary "Unleash" Keyword.Unleash,
      Arm.nullary "Daybound" Keyword.Daybound,
      Arm.nullary "Nightbound" Keyword.Nightbound,
      Arm.nullary "Decayed" Keyword.Decayed,
      Arm.nullary "Training" Keyword.Training,
      Arm.nullary "Compleated" Keyword.Compleated,
      Arm.payload "Reconfigure" (Cost.codec codec) Keyword.Reconfigure (\x -> case x of Keyword.Reconfigure y -> Just y; _ -> Nothing),
      Arm.payload "Miracle" (Cost.codec codec) Keyword.Miracle (\x -> case x of Keyword.Miracle y -> Just y; _ -> Nothing),
      Arm.nullary "ReadAhead" Keyword.ReadAhead,
      Arm.payload "Prototype" Prototype.codec Keyword.Prototype (\x -> case x of Keyword.Prototype y -> Just y; _ -> Nothing),
      Arm.nullary "ForMirrodin" Keyword.ForMirrodin,
      Arm.payload "Toxic" Common.natural Keyword.Toxic (\x -> case x of Keyword.Toxic y -> Just y; _ -> Nothing),
      Arm.payload "Disguise" (Cost.codec codec) Keyword.Disguise (\x -> case x of Keyword.Disguise y -> Just y; _ -> Nothing),
      Arm.payload "Plot" (Cost.codec codec) Keyword.Plot (\x -> case x of Keyword.Plot y -> Just y; _ -> Nothing),
      Arm.payload "Foretell" (Cost.codec codec) Keyword.Foretell (\x -> case x of Keyword.Foretell y -> Just y; _ -> Nothing),
      Arm.nullary "Demonstrate" Keyword.Demonstrate,
      Arm.payload "Escape" (Cost.codec codec) Keyword.Escape (\x -> case x of Keyword.Escape y -> Just y; _ -> Nothing),
      Arm.payload "Companion" (Filter.codec codec) Keyword.Companion (\x -> case x of Keyword.Companion y -> Just y; _ -> Nothing),
      Arm.nullary "Ascend" Keyword.Ascend,
      Arm.nullary "Storied" Keyword.Storied,
      Arm.nullary "Exhaust" Keyword.Exhaust,
      Arm.nullary "Boast" Keyword.Boast,
      Arm.payload "Mobilize" Common.natural Keyword.Mobilize (\x -> case x of Keyword.Mobilize y -> Just y; _ -> Nothing),
      Arm.payload "Firebending" Common.natural Keyword.Firebending (\x -> case x of Keyword.Firebending y -> Just y; _ -> Nothing),
      Arm.nullary "StartYourEngines" Keyword.StartYourEngines,
      Arm.nullary "Bargain" Keyword.Bargain,
      Arm.payload "Craft" (Craft.codec codec) Keyword.Craft (\x -> case x of Keyword.Craft y -> Just y; _ -> Nothing),
      Arm.nullary "Spree" Keyword.Spree,
      Arm.nullary "JobSelect" Keyword.JobSelect,
      Arm.nullary "Tiered" Keyword.Tiered,
      Arm.nullary "Exert" Keyword.Exert,
      Arm.nullary "Enlist" Keyword.Enlist,
      Arm.payload "Bestow" (Cost.codec codec) Keyword.Bestow (\x -> case x of Keyword.Bestow y -> Just y; _ -> Nothing),
      Arm.nullary "Station" Keyword.Station,
      Arm.payload "Mutate" (Cost.codec codec) Keyword.Mutate (\x -> case x of Keyword.Mutate y -> Just y; _ -> Nothing),
      Arm.nullary "UmbraArmor" Keyword.UmbraArmor,
      Arm.nullary "Epic" Keyword.Epic,
      Arm.nullary "Convoke" Keyword.Convoke,
      Arm.nullary "Delve" Keyword.Delve,
      Arm.nullary "Undaunted" Keyword.Undaunted,
      Arm.nullary "Improvise" Keyword.Improvise,
      Arm.nullary "Retrace" Keyword.Retrace,
      Arm.payload "Mayhem" (Cost.codec codec) Keyword.Mayhem (\x -> case x of Keyword.Mayhem y -> Just y; _ -> Nothing),
      Arm.payload "Madness" (Cost.codec codec) Keyword.Madness (\x -> case x of Keyword.Madness y -> Just y; _ -> Nothing),
      Arm.nullary "Rebound" Keyword.Rebound,
      Arm.payload "Scavenge" (Cost.codec codec) Keyword.Scavenge (\x -> case x of Keyword.Scavenge y -> Just y; _ -> Nothing),
      Arm.payload "Transmute" (Cost.codec codec) Keyword.Transmute (\x -> case x of Keyword.Transmute y -> Just y; _ -> Nothing),
      Arm.payload "Transfigure" (Cost.codec codec) Keyword.Transfigure (\x -> case x of Keyword.Transfigure y -> Just y; _ -> Nothing),
      Arm.payload "Encore" (Cost.codec codec) Keyword.Encore (\x -> case x of Keyword.Encore y -> Just y; _ -> Nothing),
      Arm.payload "Disturb" (Cost.codec codec) Keyword.Disturb (\x -> case x of Keyword.Disturb y -> Just y; _ -> Nothing),
      Arm.payload "Harmonize" (Cost.codec codec) Keyword.Harmonize (\x -> case x of Keyword.Harmonize y -> Just y; _ -> Nothing)
    ]

tagOf :: Keyword.Keyword -> String
tagOf x = case x of
  Keyword.Deathtouch {} -> "Deathtouch"
  Keyword.Defender {} -> "Defender"
  Keyword.DoubleStrike {} -> "DoubleStrike"
  Keyword.Equip {} -> "Equip"
  Keyword.FirstStrike {} -> "FirstStrike"
  Keyword.Flash {} -> "Flash"
  Keyword.Flying {} -> "Flying"
  Keyword.Haste {} -> "Haste"
  Keyword.Hexproof {} -> "Hexproof"
  Keyword.Indestructible {} -> "Indestructible"
  Keyword.Landwalk {} -> "Landwalk"
  Keyword.Lifelink {} -> "Lifelink"
  Keyword.LivingMetal {} -> "LivingMetal"
  Keyword.MoreThanMeetsTheEye {} -> "MoreThanMeetsTheEye"
  Keyword.Protection {} -> "Protection"
  Keyword.Reach {} -> "Reach"
  Keyword.Shroud {} -> "Shroud"
  Keyword.Trample {} -> "Trample"
  Keyword.TrampleOverPlaneswalkers {} -> "TrampleOverPlaneswalkers"
  Keyword.Vigilance {} -> "Vigilance"
  Keyword.Ward {} -> "Ward"
  Keyword.Banding {} -> "Banding"
  Keyword.Rampage {} -> "Rampage"
  Keyword.CumulativeUpkeep {} -> "CumulativeUpkeep"
  Keyword.Echo {} -> "Echo"
  Keyword.Flanking {} -> "Flanking"
  Keyword.Phasing {} -> "Phasing"
  Keyword.Buyback {} -> "Buyback"
  Keyword.Shadow {} -> "Shadow"
  Keyword.Horsemanship {} -> "Horsemanship"
  Keyword.Aftermath {} -> "Aftermath"
  Keyword.JumpStart {} -> "JumpStart"
  Keyword.Afflict {} -> "Afflict"
  Keyword.Cycling {} -> "Cycling"
  Keyword.Fading {} -> "Fading"
  Keyword.Kicker {} -> "Kicker"
  Keyword.Multikicker {} -> "Multikicker"
  Keyword.Flashback {} -> "Flashback"
  Keyword.Fear {} -> "Fear"
  Keyword.Intimidate {} -> "Intimidate"
  Keyword.Morph {} -> "Morph"
  Keyword.Amplify {} -> "Amplify"
  Keyword.Entwine {} -> "Entwine"
  Keyword.Modular {} -> "Modular"
  Keyword.Sunburst {} -> "Sunburst"
  Keyword.Dredge {} -> "Dredge"
  Keyword.Bushido {} -> "Bushido"
  Keyword.Soulshift {} -> "Soulshift"
  Keyword.Bloodthirst {} -> "Bloodthirst"
  Keyword.Haunt {} -> "Haunt"
  Keyword.SplitSecond {} -> "SplitSecond"
  Keyword.Suspend {} -> "Suspend"
  Keyword.Vanishing {} -> "Vanishing"
  Keyword.Fortify {} -> "Fortify"
  Keyword.Ninjutsu {} -> "Ninjutsu"
  Keyword.Frenzy {} -> "Frenzy"
  Keyword.Poisonous {} -> "Poisonous"
  Keyword.Champion {} -> "Champion"
  Keyword.Annihilator {} -> "Annihilator"
  Keyword.Cascade {} -> "Cascade"
  Keyword.Storm {} -> "Storm"
  Keyword.Gravestorm {} -> "Gravestorm"
  Keyword.Conspire {} -> "Conspire"
  Keyword.Affinity {} -> "Affinity"
  Keyword.Cleave {} -> "Cleave"
  Keyword.Awaken {} -> "Awaken"
  Keyword.Emerge {} -> "Emerge"
  Keyword.Evoke {} -> "Evoke"
  Keyword.Dash {} -> "Dash"
  Keyword.Blitz {} -> "Blitz"
  Keyword.Warp {} -> "Warp"
  Keyword.Surge {} -> "Surge"
  Keyword.Prowl {} -> "Prowl"
  Keyword.Freerunning {} -> "Freerunning"
  Keyword.Spectacle {} -> "Spectacle"
  Keyword.Ravenous {} -> "Ravenous"
  Keyword.Squad {} -> "Squad"
  Keyword.Offspring {} -> "Offspring"
  Keyword.Gift {} -> "Gift"
  Keyword.Replicate {} -> "Replicate"
  Keyword.Graft {} -> "Graft"
  Keyword.Absorb {} -> "Absorb"
  Keyword.Recover {} -> "Recover"
  Keyword.Ripple {} -> "Ripple"
  Keyword.Casualty {} -> "Casualty"
  Keyword.Teamwork {} -> "Teamwork"
  Keyword.WebSlinging {} -> "WebSlinging"
  Keyword.Sneak {} -> "Sneak"
  Keyword.Hideaway {} -> "Hideaway"
  Keyword.Reinforce {} -> "Reinforce"
  Keyword.Persist {} -> "Persist"
  Keyword.Infect {} -> "Infect"
  Keyword.Wither {} -> "Wither"
  Keyword.Devour {} -> "Devour"
  Keyword.Exalted {} -> "Exalted"
  Keyword.Unearth {} -> "Unearth"
  Keyword.Embalm {} -> "Embalm"
  Keyword.Eternalize {} -> "Eternalize"
  Keyword.Mentor {} -> "Mentor"
  Keyword.Afterlife {} -> "Afterlife"
  Keyword.Provoke {} -> "Provoke"
  Keyword.BattleCry {} -> "BattleCry"
  Keyword.LivingWeapon {} -> "LivingWeapon"
  Keyword.Undying {} -> "Undying"
  Keyword.Evolve {} -> "Evolve"
  Keyword.Dethrone {} -> "Dethrone"
  Keyword.Extort {} -> "Extort"
  Keyword.Fuse {} -> "Fuse"
  Keyword.Increment {} -> "Increment"
  Keyword.LevelUp {} -> "LevelUp"
  Keyword.Outlast {} -> "Outlast"
  Keyword.Prowess {} -> "Prowess"
  Keyword.Exploit {} -> "Exploit"
  Keyword.Menace {} -> "Menace"
  Keyword.Renown {} -> "Renown"
  Keyword.Changeling {} -> "Changeling"
  Keyword.Devoid {} -> "Devoid"
  Keyword.Ingest {} -> "Ingest"
  Keyword.Myriad {} -> "Myriad"
  Keyword.Skulk {} -> "Skulk"
  Keyword.Escalate {} -> "Escalate"
  Keyword.Melee {} -> "Melee"
  Keyword.Crew {} -> "Crew"
  Keyword.Saddle {} -> "Saddle"
  Keyword.Fabricate {} -> "Fabricate"
  Keyword.Partner {} -> "Partner"
  Keyword.PartnerText {} -> "PartnerText"
  Keyword.PartnerWith {} -> "PartnerWith"
  Keyword.ChooseABackground {} -> "ChooseABackground"
  Keyword.DoctorsCompanion {} -> "DoctorsCompanion"
  Keyword.Riot {} -> "Riot"
  Keyword.Unleash {} -> "Unleash"
  Keyword.Daybound {} -> "Daybound"
  Keyword.Nightbound {} -> "Nightbound"
  Keyword.Decayed {} -> "Decayed"
  Keyword.Training {} -> "Training"
  Keyword.Compleated {} -> "Compleated"
  Keyword.Reconfigure {} -> "Reconfigure"
  Keyword.Miracle {} -> "Miracle"
  Keyword.ReadAhead {} -> "ReadAhead"
  Keyword.Prototype {} -> "Prototype"
  Keyword.ForMirrodin {} -> "ForMirrodin"
  Keyword.Toxic {} -> "Toxic"
  Keyword.Disguise {} -> "Disguise"
  Keyword.Plot {} -> "Plot"
  Keyword.Foretell {} -> "Foretell"
  Keyword.Demonstrate {} -> "Demonstrate"
  Keyword.Escape {} -> "Escape"
  Keyword.Companion {} -> "Companion"
  Keyword.Ascend {} -> "Ascend"
  Keyword.Storied {} -> "Storied"
  Keyword.Exhaust {} -> "Exhaust"
  Keyword.Boast {} -> "Boast"
  Keyword.Mobilize {} -> "Mobilize"
  Keyword.Firebending {} -> "Firebending"
  Keyword.StartYourEngines {} -> "StartYourEngines"
  Keyword.Bargain {} -> "Bargain"
  Keyword.Craft {} -> "Craft"
  Keyword.Spree {} -> "Spree"
  Keyword.JobSelect {} -> "JobSelect"
  Keyword.Tiered {} -> "Tiered"
  Keyword.Exert {} -> "Exert"
  Keyword.Enlist {} -> "Enlist"
  Keyword.Bestow {} -> "Bestow"
  Keyword.Station {} -> "Station"
  Keyword.Mutate {} -> "Mutate"
  Keyword.UmbraArmor {} -> "UmbraArmor"
  Keyword.Epic {} -> "Epic"
  Keyword.Convoke {} -> "Convoke"
  Keyword.Delve {} -> "Delve"
  Keyword.Undaunted {} -> "Undaunted"
  Keyword.Improvise {} -> "Improvise"
  Keyword.Retrace {} -> "Retrace"
  Keyword.Mayhem {} -> "Mayhem"
  Keyword.Madness {} -> "Madness"
  Keyword.Rebound {} -> "Rebound"
  Keyword.Scavenge {} -> "Scavenge"
  Keyword.Transmute {} -> "Transmute"
  Keyword.Transfigure {} -> "Transfigure"
  Keyword.Encore {} -> "Encore"
  Keyword.Disturb {} -> "Disturb"
  Keyword.Harmonize {} -> "Harmonize"
