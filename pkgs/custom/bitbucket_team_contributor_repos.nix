{ bash, coreutils, curl, gawk, gnugrep, gnused, jq, pass, ... }:
''
    #!${bash}/bin/bash

    PASS_PATH=$1
    if [ -z "PASS_PATH" ]; then
        echo "No credentials provided"
        exit 1
    fi
    TEAM=$2
    PROJECTS_EXCLUDE=$3
    CREDENTIALS=$(${pass}/bin/pass $PASS_PATH | ${coreutils}/bin/tr '\n' ' ' | ${gawk}/bin/awk '{print $3 ":" $1}')
    RESULT=$(${curl}/bin/curl -s -u \
           $CREDENTIALS "https://api.bitbucket.org/2.0/repositories?role=contributor&pagelen=200" | \
           ${jq}/bin/jq -r '.values[] | select(.project.name != null) | "\(.links.clone[0].href)~\(.project.name)"')
    if [[ ! -z $TEAM ]]; then
        RESULT=$(printf "%s\n" $RESULT | ${gnugrep}/bin/grep $TEAM)
    fi
    if [[ ! -z $PROJECTS_EXCLUDE ]]; then
        GREP_CLAUSES=$(echo $PROJECTS_EXCLUDE | ${gnused}/bin/sed "s/,/\|/g")
        RESULT=$(printf "%s\n" $RESULT | ${gnugrep}/bin/grep -i -v -E $GREP_CLAUSES)
    fi
    for REPO in $RESULT; do
        echo $REPO | ${coreutils}/bin/cut -f1 -d~
    done
''
