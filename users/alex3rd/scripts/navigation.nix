{config, pkgs, lib, ...}:

let
    bookshelfPath = "${config.users.extraUsers.alex3rd.home}/bookshelf";
    autorandrProfilesPath = "${config.users.extraUsers.alex3rd.home}/.config/autorandr";
    tmuxpSessionsPath = "${config.users.extraUsers.alex3rd.home}/tmuxp";
    bookReaderUsePdftools = true;
    currentUser = "alex3rd";
    previousUser = "octocat";
    screenshotDateFormat = "%Y-%m-%d-%T";
    dockerStackPsCustomFormat = "{{.Name}}   {{.Image}}   {{.Node}} {{.DesiredState}}   {{.CurrentState}}";
    useDockerStackPsCustomFormat = false;
    dockerStackShowOnlyRunning = true;
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            rofi_list_bookshelf = pkgs.writeShellScriptBin "rofi_list_bookshelf" ''
                books=(
                $(${pkgs.findutils}/bin/find ${bookshelfPath} -name "*.pdf" -o -name "*.djvu")
                )

                list_books() {
                    for i in "''${books[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    selected_book=$( (list_books) | ${pkgs.rofi}/bin/rofi -dmenu -p "EBook " )
                    if [ -n "$selected_book" ]; then
                    ${if bookReaderUsePdftools then ''
                        ${pkgs.emacs}/bin/emacsclient --eval "(find-file \"$selected_book\")" >& /dev/null
                        sleep 0.5
                        ${pkgs.wmctrl}/bin/wmctrl -l | grep -E "emacs.+(pdf|djvu)$" | ${pkgs.gawk}/bin/awk '{print $1}' | \
                                                       ${pkgs.findutils}/bin/xargs ${pkgs.wmctrl}/bin/wmctrl -i -a
                    '' else ''
                        ${pkgs.zathura}/bin/zathura "$selected_book" & >& /dev/null
                    ''}
                    fi
                }

                main

                exit 0
            '';
            rofi_list_autorandr_profiles = pkgs.writeShellScriptBin "rofi_list_autorandr_profiles" ''
                # TODO: think of migrating to `fd`
                autorandr_profiles=(
                $(${pkgs.findutils}/bin/find ${autorandrProfilesPath} -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
                )

                list_autorandr_profiles() {
                    for i in "''${autorandr_profiles[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    selected_profile=$( (list_autorandr_profiles) | ${pkgs.rofi}/bin/rofi -dmenu -p "Profile " )
                    if [ -n "$selected_profile" ]; then
                        ${pkgs.autorandr}/bin/autorandr --load "$selected_profile" & >& /dev/null
                    fi
                }

                main

                exit 0
            '';
            # TODO: think of moving under user(s)
            rofi_list_tmuxp_sessions = pkgs.writeShellScriptBin "rofi_list_tmuxp_sessions" ''
                # TODO: think of migrating to `fd`
                tmuxp_sessions=(
                $(${pkgs.findutils}/bin/find ${tmuxpSessionsPath} -mindepth 1 -maxdepth 1 -type l -exec basename {} .yml \;)
                )

                list_tmuxp_sessions() {
                    for i in "''${tmuxp_sessions[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    selected_session=$( (list_tmuxp_sessions) | ${pkgs.rofi}/bin/rofi -dmenu -p "Profile " )
                    if [ -n "$selected_session" ]; then
                        ${pkgs.tmuxp}/bin/tmuxp load -y -d ${tmuxpSessionsPath}/$selected_session.yml >/dev/null 2>&1 &
                    fi
                }

                main

                exit 0
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
                                (ip: meta: builtins.concatStringsSep "\n"
                                           (map (hostname: "  [\"${hostname}\"]=\"${ip}\"") meta.hostNames))
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
            rofi_insert_snippet = pkgs.writeShellScriptBin "rofi_insert_snippet" ''
                ask_for_snippets() {
                    SNIPPETS=$(<${config.users.extraUsers.alex3rd.home}/${config.common.snippetsFile})
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
            rofi_list_job_docker_stacks_ps = pkgs.writeShellScriptBin "rofi_list_job_docker_stacks_ps" ''
                swarm_nodes=(
                ${builtins.concatStringsSep "\n"
                           (lib.mapAttrsToList (ip: meta: builtins.head meta.hostNames)
                                               (lib.filterAttrs (n: v: v.inSwarm == true)
                                                                 config.job.extra_hosts))}
                )
                swarm_nodes_count=''${#swarm_nodes[@]}
                selected_node=''${swarm_nodes[$(( ( RANDOM % $swarm_nodes_count ) ))]}

                ask_for_stacks() {
                    STACKS=$(${pkgs.openssh}/bin/ssh ${config.job.infra.default_remote_user}@$selected_node \
                                                     "docker stack ls | awk '{if(NR>1)print $1}'" | \
                                                     ${pkgs.gawk}/bin/awk '{print $1}')
                    for i in "''${STACKS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    STACK=$( (ask_for_stacks) | ${pkgs.rofi}/bin/rofi -dmenu -p "View stack status" )
                    if [ -n "$STACK" ]; then
                       ${pkgs.openssh}/bin/ssh ${config.job.infra.default_remote_user}@$selected_node \
                       "docker stack ps $STACK \
                       ${ if dockerStackShowOnlyRunning then "--filter \\\"desired-state=Running\\\"" else ""} \
                       ${ if useDockerStackPsCustomFormat then "--format \\\"${dockerStackPsCustomFormat}\\\"" else ""}" | \
                       ${pkgs.yad}/bin/yad --text-info # FIXME: output is too wide
                    fi
                }

                main

                exit 0
            '';
            rofi_docker_show_container_traits = pkgs.writeShellScriptBin "rofi_docker_show_container_traits" ''
                declare -A container_traits

                container_traits=(
                  ["name"]='{{index (split .Name "/") 1}}'
                  ["created"]='{{.Created}}'
                  ["path + args"]='{{.Path}} :: {{.Args}}'
                  ["stats"]='{{println .State.Status}} {{.State.StartedAt}} <--> {{println .State.FinishedAt}} restarts: {{.RestartCount}}'
                  ["ports"]='{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}} --> {{range $ifnum, $ifdef:=$mappings}}{{$ifnum}}) {{$ifdef.HostIp}}:{{$ifdef.HostPort}}{{end}}{{end}}'
                  ["mounts"]='{{range $i, $mountpoint :=.Mounts}}{{with $mountpoint}}{{.Type}} {{.Destination}} --> {{.Source}} RW:{{.RW}}{{end}}{{end}}'
                  ["env"]='{{range $entry :=.Config.Env}}{{with $entry}}{{println .}}{{end}}{{end}}'
                  ["cmd"]='{{index .Config.Cmd 0}}'
                  ["image"]='{{.Config.Image}}'
                  ["volumes"]='{{range $vol, $data :=.Config.Volumes}}{{$vol}}: {{$data}}{{end}}'
                  ["entrypoint"]='{{index .Config.Entrypoint 0}}'
                  ["labels"]='{{range $name, $value :=.Config.Labels}}{{$name}}: {{println $value}}{{end}}'
                  ["net: ip"]='{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}'
                  ["net: gateway"]='{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.Gateway}}{{end}}'
                  ["net: names"]='{{range $network, $settings :=.NetworkSettings.Networks}}{{$network}}/{{println $settings.Aliases}}{{end}}'
                )

                list_container_traits() {
                    for i in "''${!container_traits[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/uniq | ${pkgs.rofi}/bin/rofi -dmenu -p "Host" )
                    if [ -n $HOST ]; then
                        if [ "$HOST" == "localhost" ]; then
                            eval $(${pkgs.docker-machine}/bin/docker-machine env -u)
                        else
                            eval $(${pkgs.docker-machine}/bin/docker-machine env $HOST)
                        fi
                    selected_container=$( ${pkgs.docker}/bin/docker ps -a --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                    if [ -n "$selected_container" ]; then
                        selected_trait=$( (list_container_traits) | ${pkgs.rofi}/bin/rofi -dmenu -p "Inspect" )
                        if [ -n "$selected_trait" ]; then
                            inspect_command="${pkgs.docker}/bin/docker inspect $selected_container --format='"''${container_traits[$selected_trait]}"'"
                            eval `echo $inspect_command` | ${pkgs.xclip}/bin/xclip -i -r -selection clipboard
                        fi
                    fi
                    fi
                }

                main

                exit 0
            '';
            rofi_view_remote_docker_logs = pkgs.writeShellScriptBin "rofi_view_remote_docker_logs" ''
                # TODO: abstract away creds/hosts details and rework accordingly
                ask_for_logs() {
                    LOGS=$(${pkgs.openssh}/bin/ssh ${config.job.infra.default_remote_user}@${config.job.infra.logs_host} "find ${config.job.infra.remote_docker_logs_path}/ -maxdepth 1 -size +0 -type f | grep -v gz")
                    for i in "''${LOGS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    LOG=$( (ask_for_logs) | ${pkgs.rofi}/bin/rofi -dmenu -p "View log" )
                    if [ -n "$LOG" ]; then
                       ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et \
                       ${config.job.infra.default_remote_user}@${config.job.infra.logs_host} \
                       -c 'tail -f $LOG'"
                    fi
                }

                main

                exit 0
            '';
        };
    };
}
