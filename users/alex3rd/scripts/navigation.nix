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
    sedPlaceholderChar = "_";
    dockerContainerShellExecutable = "/bin/bash";
    firefoxOpenPageCmd = "${pkgs.firefox-bin}/bin/firefox --new-window";
    chromiumOpenPageCmd = "${pkgs.chromium}/bin/chromium";
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            rofi_bookshelf = pkgs.writeShellScriptBin "rofi_bookshelf" ''
                BOOKS=(
                $(${pkgs.findutils}/bin/find ${bookshelfPath} -name "*.pdf" -o -name "*.djvu")
                )

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
                        ${pkgs.emacs}/bin/emacsclient --eval "(find-file \"$selected_book\")" >& /dev/null
                        sleep 0.5
                        ${pkgs.wmctrl}/bin/wmctrl -l | grep -E "emacs.+(pdf|djvu)$" | ${pkgs.gawk}/bin/awk '{print $1}' | \
                                                       ${pkgs.findutils}/bin/xargs ${pkgs.wmctrl}/bin/wmctrl -i -a
                    '' else ''
                        ${pkgs.zathura}/bin/zathura "$SELECTED_BOOK" & >& /dev/null
                    ''}
                    fi
                }

                main

                exit 0
            '';
            rofi_autorandr_profiles = pkgs.writeShellScriptBin "rofi_autorandr_profiles" ''
                AUTORANDR_PROFILES=(
                $(${pkgs.findutils}/bin/find ${autorandrProfilesPath} -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
                )

                list_autorandr_profiles() {
                    for i in "''${AUTORANDR_PROFILES[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    SELECTED_PROFILE=$( (list_autorandr_profiles) | ${pkgs.rofi}/bin/rofi -dmenu -p "Profile " )
                    if [ -n "$SELECTED_PROFILE" ]; then
                        ${pkgs.autorandr}/bin/autorandr --load "$SELECTED_PROFILE" & >& /dev/null
                    fi
                }

                main

                exit 0
            '';
            rofi_tmuxp_sessions = pkgs.writeShellScriptBin "rofi_tmuxp_sessions" ''
                TMUXP_SESSIONS=(
                $(${pkgs.findutils}/bin/find ${tmuxpSessionsPath} -mindepth 1 -maxdepth 1 -type l -exec basename {} .yml \;)
                )

                list_tmuxp_sessions() {
                    for i in "''${TMUXP_SESSIONS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    SELECTED_SESSION=$( (list_tmuxp_sessions) | ${pkgs.rofi}/bin/rofi -dmenu -p "Profile " )
                    if [ -n "$SELECTED_SESSION" ]; then
                        ${pkgs.tmuxp}/bin/tmuxp load -y -d ${tmuxpSessionsPath}/$SELECTED_SESSION.yml >/dev/null 2>&1 &
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
                declare -A WEBJUMPS

                WEBJUMPS=(
                ${(builtins.concatStringsSep
                   "\n" (pkgs.stdenv.lib.mapAttrsToList
                              (url: browsercmd: "  [\"" + url + "\"]=\"" + browsercmd + "\"")
                              (config.job.webjumps // config.misc.webjumps)))}

                )

                list_webjumps() {
                    for i in "''${!WEBJUMPS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    WEBJUMP=$( (list_webjumps) | ${pkgs.rofi}/bin/rofi -dmenu -p "Jump to" )
                    if [ -n "$WEBJUMP" ]; then
                        ''${WEBJUMPS[$WEBJUMP]} "$WEBJUMP"
                    fi
                }

                main

                exit 0
            '';
            rofi_searchengines_prompt = pkgs.writeShellScriptBin "rofi_searchengines_prompt" ''
                declare -A SEARCHENGINES

                SEARCHENGINES=(
                ${(builtins.concatStringsSep
                   "\n" (pkgs.stdenv.lib.mapAttrsToList
                              (title: searchengine: "  [\"" + title + "\"]=\"" + searchengine + "\"")
                              config.misc.searchEngines))}
                )

                list_searchengines() {
                    INDEX=1
                    for i in "''${!SEARCHENGINES[@]}"
                    do
                        echo "$INDEX $i"
                        (( INDEX++ ))
                    done
                }

                main() {
                    SELECTED_ENGINE=$( (list_searchengines) | ${pkgs.rofi}/bin/rofi -dmenu -i -p "Search" | ${pkgs.gawk}/bin/awk '{print $2}')
                    if [ ! -n "$SELECTED_ENGINE" ]; then
                        exit 1
                    fi
                    QUERY=$( (echo ) | rofi  -dmenu -matching fuzzy -location 0 -p "Query" )
                    if [ -n "$QUERY" ]; then
                        URL="''${SEARCHENGINES[$SELECTED_ENGINE]}$QUERY"
                        ${config.misc.defaultBrowserCmd} "$URL"
                    fi
                }

                main

                exit 0
            '';
            rofi_searchengines_selection = pkgs.writeShellScriptBin "rofi_searchengines_selection" ''
                declare -A SEARCHENGINES

                SEARCHENGINES=(
                ${(builtins.concatStringsSep
                   "\n" (pkgs.stdenv.lib.mapAttrsToList
                              (title: searchengine: "  [\"" + title + "\"]=\"" + searchengine + "\"")
                              config.misc.searchEngines))}
                )

                list_searchengines() {
                    INDEX=1
                    for i in "''${!SEARCHENGINES[@]}"
                    do
                        echo "$INDEX $i"
                        (( INDEX++ ))
                    done
                }

                main() {
                    SELECTED_ENGINE=$( (list_searchengines) | ${pkgs.rofi}/bin/rofi -dmenu -i -p "Search" | ${pkgs.gawk}/bin/awk '{print $2}')
                    if [ ! -n "$SELECTED_ENGINE" ]; then
                        exit 1
                    fi
                    QUERY=$(${pkgs.xclip}/bin/xclip -o)
                    if [ -n "$QUERY" ]; then
                        URL="''${SEARCHENGINES[$SELECTED_ENGINE]}$QUERY"
                        ${config.misc.defaultBrowserCmd} "$URL"
                    fi
                }

                main

                exit 0
            '';
            rofi_extra_hosts_traits = pkgs.writeShellScriptBin "rofi_extra_hosts_traits" ''
                declare -A EXTRA_HOSTS

                EXTRA_HOSTS=(
                ${builtins.concatStringsSep "\n"
                  (pkgs.stdenv.lib.mapAttrsToList
                      (ip: meta: builtins.concatStringsSep "\n"
                        (map (hostname: "  [\"${hostname}\"]=\"IP:${sedPlaceholderChar}${ip} ${
                          if meta.hasDocker then "Docker:${sedPlaceholderChar}✓" else "Docker:${sedPlaceholderChar}✗"} ${
                          if meta.inSwarm then "Swarm:${sedPlaceholderChar}✓" else "Swarm:${sedPlaceholderChar}✗"}\"")
                          meta.hostNames))
                   (config.job.extra_hosts // config.misc.extra_hosts))}
                )

                list_extra_hosts() {
                    for i in "''${!EXTRA_HOSTS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    SELECTED_HOST=$( (list_extra_hosts) | ${pkgs.rofi}/bin/rofi -dmenu -p "Select" )
                    if [ -n "$SELECTED_HOST" ]; then
                        RESULT="$SELECTED_HOST ''${EXTRA_HOSTS[$SELECTED_HOST]}"
                        RESULT_NEWLINES=$(echo $RESULT | ${pkgs.gnused}/bin/sed 's/ /\n/g' | \
                                                         ${pkgs.gnused}/bin/sed 's/${sedPlaceholderChar}/ /g')
                        IP=$(echo $RESULT | ${pkgs.gawk}/bin/awk '{print $2}' | \
                                            ${pkgs.gnused}/bin/sed 's/${sedPlaceholderChar}/ /g')
                        echo "$RESULT_NEWLINES" > /tmp/extra_host
                        echo "$IP" | ${pkgs.gawk}/bin/awk '{print $2}'| ${pkgs.xclip}/bin/xclip -i -r -selection clipboard
                        ${pkgs.yad}/bin/yad --filename /tmp/extra_host --text-info
                        rm /tmp/extra_host
                    fi
                }

                main

                exit 0
            '';
            rofi_service_journal = pkgs.writeShellScriptBin "rofi_service_journal" ''
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
            # TODO: think of decoupling from job infra
            rofi_docker_stacks_info = pkgs.writeShellScriptBin "rofi_docker_stacks_info" ''
                SWARM_NODES=(
                ${builtins.concatStringsSep "\n"
                           (lib.mapAttrsToList (ip: meta: builtins.head meta.hostNames)
                                               (lib.filterAttrs (n: v: v.inSwarm == true)
                                                                 config.job.extra_hosts))}
                )
                SWARM_NODES_COUNT=''${#SWARM_NODES[@]}
                # SELECTED_NODE=''${SWARM_NODES[$(( ( RANDOM % $SWARM_NODES_COUNT ) ))]}
                SELECTED_NODE=${config.job.infra.docker_swarm_leader_host}

                docker_stack_ps_params() {
                    echo ${ if dockerStackShowOnlyRunning then "--filter \\\"desired-state=Running\\\"" else ""}
                         ${ if useDockerStackPsCustomFormat then " --format \\\"${dockerStackPsCustomFormat}\\\""
                            else "" }
                }

                MODES=(
                  "status"
                  "logs"
                )

                ask_for_mode() {
                    for i in "''${MODES[@]}"
                    do
                        echo "$i"
                    done
                }

                ask_for_stack() {
                    STACKS=$(${pkgs.openssh}/bin/ssh ${config.job.infra.default_remote_user}@$SELECTED_NODE \
                                                     "docker stack ls | awk '{if(NR>1)print $1}'" | \
                                                     ${pkgs.gawk}/bin/awk '{print $1}')
                    for i in "''${STACKS[@]}"
                    do
                        echo "$i"
                    done
                }

                show_stack_status() {
                    STACK=$1
                    ${pkgs.openssh}/bin/ssh ${config.job.infra.default_remote_user}@$SELECTED_NODE \
                    "docker stack ps $STACK $(docker_stack_ps_params)" > /tmp/docker_stack_status
                    ${pkgs.yad}/bin/yad --filename /tmp/docker_stack_status --text-info
                    rm /tmp/docker_stack_status
                }

                ask_for_stack_task() {
                    STACK=$1
                    TASKS=$(${pkgs.openssh}/bin/ssh ${config.job.infra.default_remote_user}@$SELECTED_NODE \
                    "docker stack ps $STACK $(docker_stack_ps_params)" | awk '{if(NR>1)print $0}')
                    SERVICE=$(${pkgs.openssh}/bin/ssh ${config.job.infra.default_remote_user}@$SELECTED_NODE \
                    "docker service ls --format='{{.Name}}' | grep $STACK ")
                    TASKS="''${SERVICE}
                ''${TASKS}"
                    for i in "''${TASKS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    MODE=$( (ask_for_mode) | ${pkgs.rofi}/bin/rofi -dmenu -p "Mode" )
                    STACK=$( (ask_for_stack) | ${pkgs.rofi}/bin/rofi -dmenu -p "Stack" )
                    case "$MODE" in
                        status)
                            show_stack_status $STACK
                            ;;
                        logs)
                            TASK=$( (ask_for_stack_task $STACK) | ${pkgs.rofi}/bin/rofi -dmenu -p "Task" | ${pkgs.gawk}/bin/awk '{print $1}' )
                            ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et \
                                                              ${config.job.infra.default_remote_user}@$SELECTED_NODE \
                                                              -c 'docker service logs --follow $TASK'"
                            ;;
                        *)
                            echo "Unknown mode: $MODE"
                            exit 1
                            ;;
                    esac
                }

                main

                exit 0
            '';
            rofi_docker_container_traits = pkgs.writeShellScriptBin "rofi_docker_container_traits" ''
                declare -A CONTAINER_TRAITS

                CONTAINER_TRAITS=(
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

                CONTAINER_STATUSES=(
                  "alive"
                  "all"
                )

                list_container_statuses() {
                    for i in "''${CONTAINER_STATUSES[@]}"
                    do
                        echo "$i"
                    done
                }

                list_container_traits() {
                    for i in "''${!CONTAINER_TRAITS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/uniq | ${pkgs.rofi}/bin/rofi -dmenu -p "Host" )
                    if [ ! -z "$HOST" ]; then
                        if [ "$HOST" == "localhost" ]; then
                            eval $(${pkgs.docker-machine}/bin/docker-machine env -u)
                        else
                            eval $(${pkgs.docker-machine}/bin/docker-machine env $HOST)
                        fi
                        CONTAINER_STATUS=$( (list_container_statuses) | ${pkgs.rofi}/bin/rofi -dmenu -p "Status" )
                        if [ -z "$container_status" ]; then
                            exit 1
                        fi
                        if [ "$CONTAINER_STATUS" == "all" ]; then
                            SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps -a --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                        else
                            SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                        fi
                        if [ -n "$SELECTED_CONTAINER" ]; then
                            SELECTED_TRAIT=$( (list_container_traits) | ${pkgs.rofi}/bin/rofi -dmenu -p "Inspect" )
                            if [ -n "$SELECTED_TRAIT" ]; then
                                INSPECT_COMMAND="${pkgs.docker}/bin/docker inspect $SELECTED_CONTAINER --format='"''${CONTAINER_TRAITS[$SELECTED_TRAIT]}"'"
                                eval `echo $INSPECT_COMMAND` | ${pkgs.xclip}/bin/xclip -i -r -selection clipboard
                                eval `echo $INSPECT_COMMAND` > /tmp/docker_traits
                                ${pkgs.yad}/bin/yad --filename /tmp/docker_traits --text-info
                                rm /tmp/docker_traits
                            fi
                        fi
                    fi
                }

                main

                exit 0
            '';
            rofi_remote_docker_logs = pkgs.writeShellScriptBin "rofi_remote_docker_logs" ''
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
            rofi_docker_shell = pkgs.writeShellScriptBin "rofi_docker_shell" ''
                main() {
                    HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/uniq | ${pkgs.rofi}/bin/rofi -dmenu -p "Host" )
                    if [ -n $HOST ]; then
                        if [ "$HOST" == "localhost" ]; then
                            eval $(${pkgs.docker-machine}/bin/docker-machine env -u)
                        else
                            eval $(${pkgs.docker-machine}/bin/docker-machine env $HOST)
                        fi
                        SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                        if [ -n "$SELECTED_CONTAINER" ]; then
                            ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et \
                            ${config.job.infra.default_remote_user}@$HOST \
                            -c 'docker exec -it $SELECTED_CONTAINER ${dockerContainerShellExecutable}'"
                        fi
                    fi
                }

                main

                exit 0
            '';
            # TODO: think of free-form option(s)
            rofi_dbms = pkgs.writeShellScriptBin "rofi_dbms" ''
                declare -A DBMS_TRAITS

                DBMS_TRAITS=(
                ${builtins.concatStringsSep "\n"
                  (pkgs.stdenv.lib.mapAttrsToList
                      (ip: meta: "  [\"${meta.alias}\"]=\"${ip} ${meta.command} ${meta.user} ${meta.passwordPassPath}\"")
                        (config.job.dbms_traits))}
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
                        CLI_BINARY_VARNAME="''${DBMS_COMMAND}_BINARY"
                        CLI_EXECUTABLE="''${!CLI_BINARY_VARNAME}"
                        ${pkgs.tmux}/bin/tmux new-window "$CLI_EXECUTABLE --host $DBMS_IP --user $DBMS_USER --password $DBMS_PASSWORD"
                    fi
                }

                main

                exit 0
            '';
            rofi_containerized_services_discovery = pkgs.writeShellScriptBin "rofi_containerized_services_discovery" ''
                # TODO: think how to restrict networks/ports output (maybe pick first ones)
                main() {
                    eval $(${pkgs.docker-machine}/bin/docker-machine env -u) # ensure we cosidering only local containers
                    SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                    if [ ! -z "$SELECTED_CONTAINER" ]; then
                        CONTAINER_IP=$(${pkgs.docker}/bin/docker inspect $SELECTED_CONTAINER --format='{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}')
                        EXPOSED_PORT=$(${pkgs.docker}/bin/docker inspect $SELECTED_CONTAINER --format='{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}}{{end}}' | ${pkgs.coreutils}/bin/cut -f1 -d/)
                        ${firefoxOpenPageCmd} http://$CONTAINER_IP:$EXPOSED_PORT
                    fi
                }

                main

                exit 0
            '';
        };
    };
}
