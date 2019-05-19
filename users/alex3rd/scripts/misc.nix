{config, pkgs, lib, ...}:
with import ../../../util.nix {inherit lib config pkgs;};
with import ../const.nix {inherit config pkgs;};
let
    buku_batch_open_treshold = 20;
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            systemctl-status = pkgs.writeShellScriptBin "systemctl-status" ''
                if [ -z "$1" ]
                then
                    echo -e ""
                else
                    STATUS=`${pkgs.systemd}/bin/systemctl status $1 | awk 'NR==3 {print $2}'`
                    if [ $STATUS == "inactive" ]
                    then
                        echo -e ""
                    else
                        if [ -z "$2" ]
                        then
                            echo -e "[*]"
                        else
                            echo -e $2
                        fi
                    fi
                fi
            '';
            misc_lib = pkgs.writeShellScriptBin "misc_lib" ''
                enforce_vpn() {
                    VPN_STATUS=$(${pkgs.systemctl-status}/bin/systemctl-status openvpn-jobvpn.service)
                    if [[ "$VPN_STATUS" == "" ]]; then
                        ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "VPN is off, turn it on and retry"
                        exit 1
                    fi
                }
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
        };
    };
}
