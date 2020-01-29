DUMP_NAME=$1
SESSIONSTORE_PATH=@firefoxSessionstorePath@
TS=$(@dateBinary@ '+%Y-%m-%d_%H-%M-%S')

SESSION_DBFILE="$SESSIONSTORE_PATH/previous.jsonlz4"
if [ -f "$SESSIONSTORE_PATH/recovery.jsonlz4" ]; then
    SESSION_DBFILE="$SESSIONSTORE_PATH/recovery.jsonlz4"
fi

if [ -z "$DUMP_NAME" ]; then
    SESSION_DUMPFILE="@firefoxSessionsNameTemplate@-$TS.org"
else
    SESSION_DUMPFILE="$DUMP_NAME.org"
fi

TAB_COUNT=$(@dejsonlz4Binary@ $SESSION_DBFILE | \
                @jqBinary@ -j '.windows[].tabs[].entries[] | .url, "\n"' | \
                @sedBinary@ 's/^/\* /g' | @teeBinary@ \
@firefoxSessionsPath@/$SESSION_DUMPFILE | @wcBinary@ -l)

@dunstifyBinary@ -u normal "[Firefox] Saved session ($TAB_COUNT tabs)."
