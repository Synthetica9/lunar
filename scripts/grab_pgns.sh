mkdir -p pgns

gh run list --limit 1000 --json 'workflowName,databaseId' \
  --jq '.[] | select(.workflowName == "Self-Play") | .databaseId' | \
  while read -r line; do
    [[ -f "pgns/$line.pgn" ]] && continue
    TEMP="$(mktemp -d)"
    echo "Grabbing $line.pgn"
    gh run download -D "$TEMP" $line
    find "$TEMP" -name '*.pgn' -exec cat {} \; > pgns/$line.pgn
    rm -r "$TEMP"
  done

cat out.pgn pgns/*.pgn > concat.pgn
