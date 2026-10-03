# Reduces MTGJSON's AllPrintings.json to one record per single-faced card name,
# the input `pawl ingest` reads (#9). Run as
#   jq -f script/ingest/candidates.jq _scratch/AllPrintings.json > _scratch/candidates.json
{
  version: .meta.version,
  cards: [
    .data[]
    | select(.type != "token" and .type != "memorabilia")
    | .cards[]
    | select(.language == "English" and .layout == "normal" and .side == null)
    | select((.isFunny // false) == false and (.isRebalanced // false) == false)
    | {name, manaCost, colorIndicator, supertypes, types, subtypes, power, toughness, text}
  ]
  | group_by(.name)
  | map(.[0])
}
