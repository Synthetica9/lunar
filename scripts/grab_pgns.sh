rm concat.pgn

gh run list --json 'workflowName,databaseId' --jq ".[].databaseId" | \
  while read -r line; do
    TEMP="$(mktemp -d)"
    gh run download -n pgn -D "$TEMP" > /dev/stderr
    cat "$TEMP"/out.pgn
    rm -r "$TEMP"
  done >> concat.pgn

cat out.pgn >> concat.pgn
