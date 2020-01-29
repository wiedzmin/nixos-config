collect_tags () {
    taglist=()
    sep=''${1:-,}
    tagcloud=$(@bukuBinary@ --np --st | \
                   @awkBinary@ '{$NF=""; print $0}' | \
                   @cutBinary@ -d ' ' -f2 | sort -u )
    while true; do
        tag=$(echo $tagcloud | tr ' ' '\n' | @dmenuBinary@ -p '> ' -mesg "Add tag" -custom)
        keep_going=$?
        if [[ $keep_going -ne 0 ]]; then
            break
        fi
        tag=$(echo "$tag" | tr -d '[:space:]')
        taglist+=("$tag$sep")
        tagcloud=( "''${tagcloud[@]/$tag}" )
    done
}

declare -A MODES

MODES=(
    ["urls"]="@bukuBinary@ -f 1 --np --st"
    ["titles"]="@bukuBinary@ -f 3 --np --st"
)
DEFAULT_MODE=urls

ask_for_mode() {
    for i in "''${!MODES[@]}"
    do
        echo "$i"
    done
}

list_search_results() {
    for i in "''${SEARCH_RESULTS[@]}"
    do
        echo "$i"
    done
}

OPEN_ALL="Alt+0"
HELP_COLOR="#774477"

main() {
    collect_tags ","
    MODE=$( (ask_for_mode) | @dmenuBinary@ -i -p "Mode" )
    if [ -z $MODE ]; then
        MODE=$DEFAULT_MODE
    fi
    BUKU_CMD=''${MODES[$MODE]}
    if [[ $(echo "''${taglist}" | wc -l) -eq 0 ]]; then
        exit 1
    fi
    BUKU_CMD="$BUKU_CMD ''${taglist[@]}"
    BUKU_CMD=''${BUKU_CMD%?}
    SEARCH_RESULTS="$($BUKU_CMD)"
    LEGEND="Select an entry or use <span color='$HELP_COLOR'>$OPEN_ALL</span> to open all bookmarks. You could open maximum @contentBookmarksBatchOpenThreshold@ bookmarks at once."
    SELECTION=$( echo "$SEARCH_RESULTS" | tr ' ' '\n' | \
                     @dmenuBinary@ -p '> ' -mesg "''${LEGEND}" -kb-custom-10 "''${OPEN_ALL}")
    ROFI_EXIT=$?
    if [[ $ROFI_EXIT -eq 10 ]]; then
        if [[ $(echo "''${taglist}" | wc -l) -gt @contentBookmarksBatchOpenThreshold@ ]]; then
            exit 1
        else
            SELECTION=$SEARCH_RESULTS
        fi
    fi

    SELECTION=$( (list_search_results) | @awkBinary@ '{print $1}' )
    @bukuBinary@ -o $SELECTION
}

main

exit 0
