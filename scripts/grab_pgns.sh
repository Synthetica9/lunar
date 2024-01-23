rm concat.pgn

mkdir -p pgns

gh run list --limit 1000 --json 'workflowName,databaseId' \
  --jq '.[] | select(.workflowName == "Self-Play") | .databaseId' | \
  while read -r line; do
    TEMP="$(mktemp -d)"
    [[ -f "pgns/$line.pgn" ]] && continue
    echo "Grabbing $line.pgn"
    gh run download -n pgn -D "$TEMP" $line || touch $TEMP/out.pgn
    mv $TEMP/out.pgn "pgns/$line.pgn" || continue
    rm -r "$TEMP"
  done

cat out.pgn pgns/*.pgn >> concat.pgn
