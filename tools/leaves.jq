# cat data/languages/english.json | jq -r -L tools 'include "leaves"; leaves' > ui-texts
# cat data/freshdb.json | jq -r 'map (.names.english | select(.) | if type=="object" then .name else . end) | join("\n")'

def leaves:
  if type == "array" or type == "object"
  then .[] | leaves
  else .
  end
;
