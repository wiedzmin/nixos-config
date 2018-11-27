{config, pkgs, ...}:

let
    bookshelfPath = "${config.users.extraUsers.alex3rd.home}/bookshelf";
    autorandrProfilesPath = "${config.users.extraUsers.alex3rd.home}/.config/autorandr";
    tmuxpSessionsPath = "${config.users.extraUsers.alex3rd.home}/tmuxp";
    bookReaderUsePdftools = true;
    currentUser = "alex3rd";
    previousUser = "octocat";
    screenshotDateFormat = "%Y-%m-%d-%T";
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            list_bookshelf_reader = pkgs.writeShellScriptBin "list_bookshelf_reader" ''
                if [ -n "$1" ]
                then
                    coproc (${pkgs.zathura}/bin/zathura "$1" & >& /dev/null &)
                    exit;
                fi

                ${pkgs.findutils}/bin/find ${bookshelfPath} -name "*.pdf" -o -name "*.djvu"
            '';
            list_bookshelf_pdftools = pkgs.writeShellScriptBin "list_bookshelf_pdftools" ''
                if [ -n "$1" ]
                then
                    coproc (${pkgs.emacs}/bin/emacsclient --eval "(find-file \"$1\")" >& /dev/null)
                    sleep 0.5
                    ${pkgs.wmctrl}/bin/wmctrl -l | grep -E "emacs.+(pdf|djvu)$" | ${pkgs.gawk}/bin/awk '{print $1}' | ${pkgs.findutils}/bin/xargs ${pkgs.wmctrl}/bin/wmctrl -i -a
                    exit;
                fi

                ${pkgs.findutils}/bin/find ${bookshelfPath} -name "*.pdf" -o -name "*.djvu"
            '';
            rofi_list_bookshelf = pkgs.writeShellScriptBin "rofi_list_bookshelf" ''
                ${if bookReaderUsePdftools then ''
                    ${pkgs.rofi}/bin/rofi -modi books:${pkgs.list_bookshelf_pdftools}/bin/list_bookshelf_pdftools -show books
                '' else ''
                    ${pkgs.rofi}/bin/rofi -modi books:${pkgs.list_bookshelf_reader}/bin/list_bookshelf_reader -show books
                ''}
            '';
            list_autorandr_profiles = pkgs.writeShellScriptBin "list_autorandr_profiles" ''
                if [ -n "$1" ]
                then
                    coproc (${pkgs.autorandr}/bin/autorandr --load "$1" & >& /dev/null)
                    exit;
                fi

                # TODO: think of migrating to `fd`
                ${pkgs.findutils}/bin/find ${autorandrProfilesPath} -mindepth 1 -maxdepth 1 -type d -exec basename {} \;
            '';
            rofi_list_autorandr_profiles = pkgs.writeShellScriptBin "rofi_list_autorandr_profiles" ''
                ${pkgs.rofi}/bin/rofi -modi autorandr:${pkgs.list_autorandr_profiles}/bin/list_autorandr_profiles \
                                      -show autorandr
            '';
            # TODO: think of moving under user(s)
            list_tmuxp_sessions = pkgs.writeShellScriptBin "list_tmuxp_sessions" ''
                if [ -n "$1" ]
                then
                    ${pkgs.tmuxp}/bin/tmuxp load -y -d ${tmuxpSessionsPath}/$1.yml >/dev/null 2>&1 &
                    exit;
                else
                    # TODO: think of migrating to `fd`
                    ${pkgs.findutils}/bin/find ${tmuxpSessionsPath} -mindepth 1 -maxdepth 1 -type l -exec basename {} .yml \;
                fi
            '';
            rofi_list_tmuxp_sessions = pkgs.writeShellScriptBin "rofi_list_tmuxp_sessions" ''
                ${pkgs.rofi}/bin/rofi -modi tmuxp:${pkgs.list_tmuxp_sessions}/bin/list_tmuxp_sessions \
                                      -show tmuxp
            '';
            rofi_ssh_custom_user = pkgs.writeShellScriptBin "rofi_ssh_custom_user" ''
                # TODO: provide freeform option or predefined list on Nix level
                USERS=(
                  "root"
                  "${currentUser}"
                  "${previousUser}"
                )

                ask_for_user() {
                    for i in "''${USERS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    USER=$( (ask_for_user) | ${pkgs.rofi}/bin/rofi -dmenu -p "User" )
                    if [ ! -n "$USER" ]; then
                        exit 1
                    fi
                    HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Host" )
                    if [ -n "$HOST" ]; then
                        ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et $USER@$HOST"
                    fi
                }

                main

                exit 0
            '';
            rofi_webjumps = pkgs.writeShellScriptBin "rofi_webjumps" ''
                declare -A webjumps

                webjumps=(
                ${(builtins.concatStringsSep
                   "\n" (pkgs.stdenv.lib.mapAttrsToList
                              (url: browsercmd: "  [\"" + url + "\"]=\"" + browsercmd + "\"")
                              (config.job.webjumps // config.misc.webjumps)))}

                )

                list_webjumps() {
                    for i in "''${!webjumps[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    webjump=$( (list_webjumps) | ${pkgs.rofi}/bin/rofi -dmenu -p "Jump to" )
                    if [ -n "$webjump" ]; then
                        ''${webjumps[$webjump]} "$webjump"
                    fi
                }

                main

                exit 0
            '';
            rofi_searchengines_prompt = pkgs.writeShellScriptBin "rofi_searchengines_prompt" ''
                declare -A searchengines

                searchengines=(
                ${(builtins.concatStringsSep
                   "\n" (pkgs.stdenv.lib.mapAttrsToList
                              (title: searchengine: "  [\"" + title + "\"]=\"" + searchengine + "\"")
                              config.misc.searchEngines))}
                )

                list_searchengines() {
                    index=1
                    for i in "''${!searchengines[@]}"
                    do
                        echo "$index $i"
                        (( index++ ))
                    done
                }

                main() {
                    selected_engine=$( (list_searchengines) | ${pkgs.rofi}/bin/rofi -dmenu -i -p "Search" | ${pkgs.gawk}/bin/awk '{print $2}')
                    if [ ! -n "$selected_engine" ]; then
                        exit 1
                    fi
                    query=$( (echo ) | rofi  -dmenu -matching fuzzy -location 0 -p "Query" )
                    if [ -n "$query" ]; then
                        url="''${searchengines[$selected_engine]}$query"
                        ${config.misc.defaultBrowserCmd} "$url"
                    fi
                }

                main

                exit 0
            '';
            rofi_searchengines_selection = pkgs.writeShellScriptBin "rofi_searchengines_selection" ''
                declare -A searchengines

                searchengines=(
                ${(builtins.concatStringsSep
                   "\n" (pkgs.stdenv.lib.mapAttrsToList
                              (title: searchengine: "  [\"" + title + "\"]=\"" + searchengine + "\"")
                              config.misc.searchEngines))}
                )

                list_searchengines() {
                    index=1
                    for i in "''${!searchengines[@]}"
                    do
                        echo "$index $i"
                        (( index++ ))
                    done
                }

                main() {
                    selected_engine=$( (list_searchengines) | ${pkgs.rofi}/bin/rofi -dmenu -i -p "Search" | ${pkgs.gawk}/bin/awk '{print $2}')
                    if [ ! -n "$selected_engine" ]; then
                        exit 1
                    fi
                    query=$(${pkgs.xclip}/bin/xclip -o)
                    if [ -n "$query" ]; then
                        url="''${searchengines[$selected_engine]}$query"
                        ${config.misc.defaultBrowserCmd} "$url"
                    fi
                }

                main

                exit 0
            '';
            rofi_ref_extra_hosts = pkgs.writeShellScriptBin "rofi_ref_extra_hosts" ''
                declare -A extra_hosts

                extra_hosts=(
                ${builtins.concatStringsSep
                           "\n"
                           (pkgs.stdenv.lib.mapAttrsToList
                                (ip: names: "  [\"${names}\"]=\"${ip}\"")
                                (config.job.extra_hosts // config.misc.extra_hosts))}
                )

                list_extra_hosts() {
                    for i in "''${!extra_hosts[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    selected_host=$( (list_extra_hosts) | ${pkgs.rofi}/bin/rofi -dmenu -p "Select" )
                    if [ -n "$selected_host" ]; then
                        echo -n "$selected_host ''${extra_hosts[$selected_host]}" | xclip -i -selection clipboard
                    fi
                }

                main

                exit 0
            '';
            rofi_view_service_journal = pkgs.writeShellScriptBin "rofi_view_service_journal" ''
                SERVICE_CONTEXTS=(
                  "system"
                  "user"
                )

                ask_for_context() {
                    for i in "''${SERVICE_CONTEXTS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    CONTEXT=$( (ask_for_context) | ${pkgs.rofi}/bin/rofi -dmenu -p "Context" )
                    if [ ! -n "$CONTEXT" ]; then
                        exit 1
                    fi
                    SERVICE=$(${pkgs.systemd}/bin/systemctl $([[ "$CONTEXT" == "user" ]] && echo --user) list-unit-files | \
                              grep -v target | ${pkgs.gawk}/bin/awk '{print $1}' | \
                              ${pkgs.rofi}/bin/rofi -dmenu -p "Service")
                    if [ -n "$SERVICE" ]; then
                        ${pkgs.tmux}/bin/tmux new-window "${pkgs.systemd}/bin/journalctl $([[ "$CONTEXT" == "user" ]] && echo --user) -u $SERVICE"
                    fi
                }

                main

                exit 0
            '';
        };
    };
}
