{config, pkgs, lib, ...}:
with import ../../../toolbox/util.nix {inherit lib config pkgs;};
with import ../const.nix {inherit config pkgs;};
let
    bookshelfPath = "/home/${userName}/bookshelf";
    bookReaderUsePdftools = true;
    buku_batch_open_treshold = 20;
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            rofi_bookshelf = pkgs.writeShellScriptBin "rofi_bookshelf" ''
                IFS=$'\n'
                BOOKS=$(${pkgs.findutils}/bin/find ${bookshelfPath} -name "*.pdf"${ if !bookReaderUsePdftools then
                        " -o -name \"*.djvu\"" else ""})

                list_books() {
                    for i in "''${BOOKS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    SELECTED_BOOK=$( (list_books) | ${pkgs.rofi}/bin/rofi -dmenu -p "EBook " )
                    if [ -n "$SELECTED_BOOK" ]; then
                    ${if bookReaderUsePdftools then ''
                        ${pkgs.emacs}/bin/emacsclient --eval "(find-file \"$SELECTED_BOOK\")" >& /dev/null
                        sleep 0.5
                        ${pkgs.wmctrl}/bin/wmctrl -l -x | grep -E "emacs\.Emacs.+(pdf|djvu)$" | ${pkgs.gawk}/bin/awk '{print $1}' | \
                                                       ${pkgs.findutils}/bin/xargs ${pkgs.wmctrl}/bin/wmctrl -i -a
                    '' else ''
                        ${pkgs.zathura}/bin/zathura "$SELECTED_BOOK" & >& /dev/null
                    ''}
                    fi
                }

                main

                exit 0
            '';
            rofi_buku_functions = pkgs.writeShellScriptBin "rofi_buku_functions" ''
                _rofi () {
                    ${pkgs.rofi}/bin/rofi -dmenu -i -no-levenshtein-sort -width 1000 "$@"
                }

                is_url () {
                    url_regex='(https?|ftp|file)://[-A-Za-z0-9\+&@#/%?=~_|!:,.;]*[-A-Za-z0-9\+&@#/%=~_|]'
                    url_candidate=$1
                    if [[ $url_candidate =~ $url_regex ]]
                    then
                        return 0
                    fi
                    return 1
                }

                collect_tags () {
                    taglist=()
                    sep=''${1:-,}
                    tagcloud=$(${pkgs.buku}/bin/buku --np --st | \
                               ${pkgs.gawk}/bin/awk '{$NF=""; print $0}' | \
                               ${pkgs.coreutils}/bin/cut -d ' ' -f2 | sort -u )
                    while true; do
                        tag=$(echo $tagcloud | tr ' ' '\n' | _rofi -p '> ' -mesg "Add tag" -custom)
                        keep_going=$?
                        if [[ $keep_going -ne 0 ]]; then
                            break
                        fi
                        tag=$(echo "$tag" | tr -d '[:space:]')
                        taglist+=("$tag$sep")
                        tagcloud=( "''${tagcloud[@]/$tag}" )
                    done
                }
            '';
            rofi_buku_add = pkgs.writeShellScriptBin "rofi_buku_add" ''
                . ${pkgs.rofi_buku_functions}/bin/rofi_buku_functions

                sleep_sec=''${1:-1}

                add_mark () {
                    inserturl=$(echo -e "$(${pkgs.xsel}/bin/xsel -o -b)" | _rofi -p '> ' -mesg "Use URL below or type manually")
                    if [[ $? -ne 0 ]]; then
                        exit
                    fi
                    is_url $inserturl
                    if [[ $? -ne 0 ]]; then
                        ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "URL is not valid, exiting"
                        exit
                    fi

                    add_tags
                }

                add_tags () {
                    collect_tags ","
                    if [[ $(echo "''${taglist}" | wc -l) -gt 0 ]]; then
                        ${pkgs.buku}/bin/buku -a ''${inserturl} ''${taglist[@]}
                    else
                        ${pkgs.buku}/bin/buku -a ''${inserturl}
                    fi
                }

                main() {
                    sleep $sleep_sec
                    add_mark
                    ${pkgs.dunst}/bin/dunstify -t 5000 "Bookmark added: $inserturl"
                }

                main

                exit 0
            '';
            rofi_buku_search_tag = pkgs.writeShellScriptBin "rofi_buku_search_tag" ''
                . ${pkgs.rofi_buku_functions}/bin/rofi_buku_functions

                declare -A MODES

                MODES=(
                  ["urls"]="${pkgs.buku}/bin/buku -f 1 --np --st"
                  ["titles"]="${pkgs.buku}/bin/buku -f 3 --np --st"
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
                    MODE=$( (ask_for_mode) | ${pkgs.rofi}/bin/rofi -dmenu -p "Mode" )
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
                    LEGEND="Select an entry or use <span color='$HELP_COLOR'>$OPEN_ALL</span> to open all bookmarks. You could open maximum ${builtins.toString buku_batch_open_treshold} bookmarks at once."
                    SELECTION=$( echo "$SEARCH_RESULTS" | tr ' ' '\n' | \
                                 _rofi -p '> ' -mesg "''${LEGEND}" -kb-custom-10 "''${OPEN_ALL}")
                    ROFI_EXIT=$?
                    if [[ $ROFI_EXIT -eq 10 ]]; then
                        if [[ $(echo "''${taglist}" | wc -l) -gt ${builtins.toString buku_batch_open_treshold} ]]; then
                            exit 1
                        else
                            SELECTION=$SEARCH_RESULTS
                        fi
                    fi

                    SELECTION=$( (list_search_results) | ${pkgs.gawk}/bin/awk '{print $1}' )
                    ${pkgs.buku}/bin/buku -o $SELECTION
                }

                main

                exit 0
            '';
            rofi_buku_search_url = pkgs.writeShellScriptBin "rofi_buku_search_url" ''
                . ${pkgs.rofi_buku_functions}/bin/rofi_buku_functions

                main() {
                    SEARCH_RESULTS="$(${pkgs.buku}/bin/buku -f 1 --nc -p)"
                    SELECTION=$( echo "$SEARCH_RESULTS" | tr ' ' '\n' | _rofi -p '> ')
                    if [ -n "$SELECTION" ]; then
                        ${pkgs.buku}/bin/buku -o $SELECTION
                    fi
                }

                main

                exit 0
            '';
            rofi_buku_entrypoint = pkgs.writeShellScriptBin "rofi_buku_entrypoint" ''
                declare -A ROFI_BUKU_SCRIPTS

                # TODO: try to make more declarative
                ROFI_BUKU_SCRIPTS=(
                  ["add bookmark"]="${pkgs.rofi_buku_add}/bin/rofi_buku_add"
                  ["search by tag(s)"]="${pkgs.rofi_buku_search_tag}/bin/rofi_buku_search_tag"
                  ["search by url"]="${pkgs.rofi_buku_search_url}/bin/rofi_buku_search_url"
                )

                list_rofi_buku_scripts() {
                    for i in "''${!ROFI_BUKU_SCRIPTS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    ROFI_BUKU_SCRIPT="''${ROFI_BUKU_SCRIPTS[$( (list_rofi_buku_scripts) | ${pkgs.rofi}/bin/rofi -dmenu -p "Script" )]}"
                    if [ -n "ROFI_BUKU_SCRIPT" ]; then
                        $ROFI_BUKU_SCRIPT
                    fi
                }

                main

                exit 0
            '';
            rofi_insert_snippet = pkgs.writeShellScriptBin "rofi_insert_snippet" ''
                ask_for_snippets() {
                    SNIPPETS=$(</home/${userName}/${config.common.snippets.file})
                    for i in "''${SNIPPETS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    SNIPPET=$( (ask_for_snippets) | ${pkgs.rofi}/bin/rofi -dmenu -p "Snippet" )
                    if [ ! -n "$SNIPPET" ]; then
                        exit 1
                    fi
                    echo $SNIPPET | tr -d '\n' | ${pkgs.xsel}/bin/xsel -i --clipboard
                    ${pkgs.xdotool}/bin/xdotool type -- "$(${pkgs.xsel}/bin/xsel -bo)"
                }

                main

                exit 0
            '';
            # TODO: think of free-form option(s)
            rofi_dbms = pkgs.writeShellScriptBin "rofi_dbms" ''
                . ${pkgs.misc_lib}/bin/misc_lib

                enforce_vpn

                declare -A DBMS_TRAITS

                DBMS_TRAITS=(
                ${builtins.concatStringsSep "\n"
                  (pkgs.stdenv.lib.mapAttrsToList
                      (alias: meta: "  [\"${alias}\"]=\"${meta.ip} ${meta.command} ${meta.user} ${meta.passwordPassPath}\"")
                        (config.job.dbmsTraits))}
                )

                MYCLI_BINARY=${pkgs.mycli}/bin/mycli
                PGCLI_BINARY=${pkgs.pgcli}/bin/pgcli

                list_dbms_traits() {
                    for i in "''${!DBMS_TRAITS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    DBMS_META="''${DBMS_TRAITS[$( (list_dbms_traits) | ${pkgs.rofi}/bin/rofi -dmenu -p "Connect" )]}"
                    if [ -n "$DBMS_META" ]; then
                        DBMS_IP=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f1 -d\ )
                        DBMS_COMMAND=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f2 -d\ )
                        DBMS_USER=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f3 -d\ )
                        DBMS_PASSWORD_PASS_PATH=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f4 -d\ )
                        DBMS_PASSWORD=$(${pkgs.pass}/bin/pass $DBMS_PASSWORD_PASS_PATH)
                        CLI_BINARY_VARNAME="''${DBMS_COMMAND^^}_BINARY"
                        CLI_EXECUTABLE="''${!CLI_BINARY_VARNAME}"
                        if [ "$DBMS_COMMAND" == "mycli" ]; then
                            ${pkgs.tmux}/bin/tmux new-window "$CLI_EXECUTABLE --host $DBMS_IP --user $DBMS_USER --password $DBMS_PASSWORD"
                        elif [ "$DBMS_COMMAND" == "pgcli" ]; then
                            ${pkgs.tmux}/bin/tmux new-window "PGPASSWORD=$DBMS_PASSWORD $CLI_EXECUTABLE --host $DBMS_IP --user $DBMS_USER"
                        fi

                    fi
                }

                main

                exit 0
            '';
            rofi_entrypoint = pkgs.writeShellScriptBin "rofi_entrypoint" ''
                declare -A ROFI_SCRIPTS

                # TODO: try to make more declarative
                ROFI_SCRIPTS=(
                  ["rofi_bookshelf"]="${pkgs.rofi_bookshelf}/bin/rofi_bookshelf"
                  ["rofi_buku_search_tag"]="${pkgs.rofi_buku_search_tag}/bin/rofi_buku_search_tag"
                  ["rofi_buku_search_url"]="${pkgs.rofi_buku_search_url}/bin/rofi_buku_search_url"
                  ["rofi_containerized_services_discovery"]="${pkgs.rofi_containerized_services_discovery}/bin/rofi_containerized_services_discovery"
                  ["rofi_ctop"]="${pkgs.rofi_ctop}/bin/rofi_ctop"
                  ["rofi_dbms"]="${pkgs.rofi_dbms}/bin/rofi_dbms"
                  ["rofi_docker_stacks_info"]="${pkgs.rofi_docker_stacks_info}/bin/rofi_docker_stacks_info"
                  ["rofi_extra_hosts_traits"]="${pkgs.rofi_extra_hosts_traits}/bin/rofi_extra_hosts_traits"
                  ["rofi_insert_snippet"]="${pkgs.rofi_insert_snippet}/bin/rofi_insert_snippet"
                  ["rofi_jnettop"]="${pkgs.rofi_jnettop}/bin/rofi_jnettop"
                  ["rofi_remote_docker_logs"]="${pkgs.rofi_remote_docker_logs}/bin/rofi_remote_docker_logs"
                  ["rofi_searchengines_prompt"]="${pkgs.rofi_searchengines_prompt}/bin/rofi_searchengines_prompt"
                  ["rofi_searchengines_selection"]="${pkgs.rofi_searchengines_selection}/bin/rofi_searchengines_selection"
                  ["rofi_ssh_custom_user"]="${pkgs.rofi_ssh_custom_user}/bin/rofi_ssh_custom_user"
                  ["rofi_webjumps"]="${pkgs.rofi_webjumps}/bin/rofi_webjumps"
                )

                list_rofi_scripts() {
                    for i in "''${!ROFI_SCRIPTS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    ROFI_SCRIPT="''${ROFI_SCRIPTS[$( (list_rofi_scripts) | ${pkgs.rofi}/bin/rofi -dmenu -p "Script" )]}"
                    if [ -n "$ROFI_SCRIPT" ]; then
                        $ROFI_SCRIPT
                    fi
                }

                main

                exit 0
            '';
            rescale-wallpaper = pkgs.writeShellScriptBin "rescale-wallpaper" ''
                ${pkgs.feh}/bin/feh --bg-fill ${config.sys.wallpaper.baseDir}/${config.sys.wallpaper.current}
            '';
            kill-compton = pkgs.writeShellScriptBin "kill-compton" ''
                ${pkgs.procps}/bin/pkill -f compton
            '';
        };
    };
}
