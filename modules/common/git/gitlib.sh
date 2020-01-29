WIP_RE=wip

execute_hook_items() {
    hook=$1
    hooks_basedir=$(pwd)/@gitHooksDirname@

    if [ -z $hook ]; then
        echo "no hook provided, exiting"
        exit 1
    fi

    if [ ! -d "$hooks_basedir" ]; then
        # repo hooks were not initialized, simply exit with success
        exit 0
    fi

    IS_CLEAN=true
    TARGET_DIR="$hooks_basedir/$hook"

    shopt -s execfail

    for TARGET_PATH in $(@fdBinary@ . -L --max-depth 1 --type f $TARGET_DIR | @sortBinary@)
    do
        if [ -x "$TARGET_PATH" ]; then # Run as an executable file
            "$TARGET_PATH" "$@"
        elif [ -f "$TARGET_PATH" ]; then  # Run as a Shell script
            @bashBinary@ "$TARGET_PATH" "$@"
        fi
        exitcode=$?
        if (( exitcode != 0 )); then
            IS_CLEAN=false
            @gitHooksShortCircuitPatch@
        fi
    done
    if [[ "$IS_CLEAN" == "false" ]]; then
        return 1;
    fi
    return 0;
}

check_for_wip() {
    RESULTS=$(@gitBinary@ shortlog "@{u}.." | @grepBinary@ -w $WIP_RE);
    if [[ ! -z "$RESULTS" ]]; then
        echo "Found commits with stop snippets:"
        echo "$RESULTS"
        return 1;
    fi
    return 0;
}

check_for_secrets() { # https://github.com/Luis-Hebendanz/nix-configs/blob/master/git.nix#L25
    RESULTS=$(@gitSecretsBinary@ --scan --cached 2>&1)
    if [[ -z "$RESULTS" ]]; then
        return 0;
    fi
    echo "Found secret snippets:"
    echo "$RESULTS"
    return 1;
}

check_org_delete_treshold() {
    CURRENT_REV=`@gitBinary@ rev-parse HEAD`
    PREVIOUS_REV=`@gitBinary@ rev-parse HEAD^1`

    OUTFILE="@orgWarningsFilename@"
    THRESHOLD=250

    MESSAGE="** commit ''${CURRENT_REV} deleted more than ''${THRESHOLD} lines in a file!"

    DETAILS="#+BEGIN_SRC sh :results output
      cd @orgWarningsFiledir@
      echo \"commit ''${CURRENT_REV}\"
      @gitBinary@ diff --stat \"''${PREVIOUS_REV}\" \"''${CURRENT_REV}\"
    #+END_SRC"

    @gitBinary@ diff --numstat "''${PREVIOUS_REV}" "''${CURRENT_REV}" | \
        cut -f 2 | \
        while read line
        do test "$line" -gt "''${THRESHOLD}" && \
                echo "''${MESSAGE}\n<`date '+%Y-%m-%d %H:%M'` +1d>\n\n''${DETAILS}\n" >> \
"''${OUTFILE}"; \
        done
}
