main() {
    SEARCH_RESULTS="$(@bukuBinary@ -f 1 --nc -p)"
    SELECTION=$( echo "$SEARCH_RESULTS" | tr ' ' '\n' | @dmenuBinary@ -p '> ')
    if [ -n "$SELECTION" ]; then
        @bukuBinary@ -o $SELECTION
    fi
}

main

exit 0
