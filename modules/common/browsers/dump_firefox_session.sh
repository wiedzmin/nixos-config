DUMP_NAME=$1
SESSIONSTORE_PATH=@firefoxSessionstorePath@
TS=$(date '+%Y-%m-%d_%H-%M-%S')

SESSION_DBFILE="$SESSIONSTORE_PATH/previous.jsonlz4"
if [ -f "$SESSIONSTORE_PATH/recovery.jsonlz4" ]; then
    SESSION_DBFILE="$SESSIONSTORE_PATH/recovery.jsonlz4"
fi

if [ -z "$DUMP_NAME" ]; then
    SESSION_DUMPFILE="@firefoxSessionsNameTemplate@-$TS.org"
else
    SESSION_DUMPFILE="$DUMP_NAME.org"
fi

TAB_COUNT=$(dejsonlz4 $SESSION_DBFILE | \
                jq -j '.windows[].tabs[].entries[] | .url, "\n"' | \
                sed 's/^/\* /g' | tee \
@firefoxSessionsPath@/$SESSION_DUMPFILE | wc -l)

dunstify -u normal "[Firefox] Saved session ($TAB_COUNT tabs)."
