main() {
  SEARCH_RESULTS="$(buku -f 1 --nc -p)"
  SELECTION=$(echo "$SEARCH_RESULTS" | tr ' ' '\n' | dmenu -p '> ')
  if [ -n "$SELECTION" ]; then
    buku -o $SELECTION
  fi
}

main

exit 0
