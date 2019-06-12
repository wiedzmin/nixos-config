{config, pkgs, lib, ...}:
with import ../../../../../util.nix {inherit lib config pkgs;};
with import ../../../const.nix {inherit config pkgs;};
let
    dockerContainerShellExecutable = "/bin/bash";
    rofi_autorandr_profiles = pkgs.writeShellScriptBin "rofi_autorandr_profiles" ''
        . ${pkgs.misc_lib}/bin/misc_lib

        AUTORANDR_PROFILES_PATH=''${1:-$HOME/.config/autorandr}

        AUTORANDR_PROFILES=(
        $(${pkgs.fd}/bin/fd --type d . $AUTORANDR_PROFILES_PATH -x echo '{/}' | ${pkgs.gnugrep}/bin/grep -ve "\.d")
        )

        main() {
            SELECTED_PROFILE=$( (show_list "''${AUTORANDR_PROFILES[@]}") | ${pkgs.rofi}/bin/rofi -dmenu -p "Profile " )
            if [ -n "$SELECTED_PROFILE" ]; then
                ${pkgs.autorandr}/bin/autorandr --load "$SELECTED_PROFILE" & >& /dev/null
            fi
        }

        main

        exit 0
    '';
    rofi_tmuxp_sessions = pkgs.writeShellScriptBin "rofi_tmuxp_sessions" ''
        . ${pkgs.misc_lib}/bin/misc_lib

        TMUXP_SESSIONS_PATH=''${1:-$HOME/tmuxp}

        TMUXP_SESSIONS=(
        $(${pkgs.fd}/bin/fd --maxdepth 1 --type l '.yml' $TMUXP_SESSIONS_PATH -x echo '{/.}')
        )

        main() {
            SELECTED_SESSION=$( (show_list "''${TMUXP_SESSIONS[@]}") | ${pkgs.rofi}/bin/rofi -dmenu -p "Profile " )
            if [ -n "$SELECTED_SESSION" ]; then
                ${pkgs.tmuxp}/bin/tmuxp load -y -d $TMUXP_SESSIONS_PATH/$SELECTED_SESSION.yml >/dev/null 2>&1 &
            fi
        }

        main

        exit 0
    '';
    rofi_service_journal = pkgs.writeShellScriptBin "rofi_service_journal" ''
        . ${pkgs.misc_lib}/bin/misc_lib

        SERVICE_CONTEXTS=(
          "system"
          "user"
        )

        main() {
            CONTEXT=$( (show_list "''${SERVICE_CONTEXTS[@]}") | ${pkgs.rofi}/bin/rofi -dmenu -p "Context" )
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
    # TODO: review and fix
    rofi_docker_container_traits = pkgs.writeShellScriptBin "rofi_docker_container_traits" ''
        . ${pkgs.misc_lib}/bin/misc_lib

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

        main() {
            HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/uniq | ${pkgs.rofi}/bin/rofi -dmenu -p "Host" )
            if [ ! -z "$HOST" ]; then
                if [ "$HOST" == "localhost" ]; then
                    eval $(${pkgs.docker-machine}/bin/docker-machine env -u)
                else
                    eval $(${pkgs.docker-machine}/bin/docker-machine env $HOST)
                fi
                CONTAINER_STATUS=$( (show_list "''${CONTAINER_STATUSES[@]}") | ${pkgs.rofi}/bin/rofi -dmenu -p "Status" )
                if [ -z "$CONTAINER_STATUS" ]; then
                    exit 1
                fi
                if [ "$CONTAINER_STATUS" == "all" ]; then
                    SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps -a --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                else
                    SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                fi
                if [ -n "$SELECTED_CONTAINER" ]; then
                    SELECTED_TRAIT=$( (show_mapping_keys "$(declare -p CONTAINER_TRAITS)") | ${pkgs.rofi}/bin/rofi -dmenu -p "Inspect" )
                    if [ -n "$SELECTED_TRAIT" ]; then
                        INSPECT_COMMAND="${pkgs.docker}/bin/docker inspect $SELECTED_CONTAINER --format='"''${CONTAINER_TRAITS[$SELECTED_TRAIT]}"'"
                        eval `echo $INSPECT_COMMAND` | tr -d '\n' | ${pkgs.xsel}/bin/xsel -i --clipboard
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
    rofi_docker_shell = pkgs.writeShellScriptBin "rofi_docker_shell" ''
        . ${pkgs.misc_lib}/bin/misc_lib

        main() {
            HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/uniq | ${pkgs.rofi}/bin/rofi -dmenu -p "Host" )
            if [ -n $HOST ]; then
                if [ "$HOST" == "localhost" ]; then
                    eval $(${pkgs.docker-machine}/bin/docker-machine env -u)
                else
                    enforce_vpn
                    eval $(${pkgs.docker-machine}/bin/docker-machine env $HOST)
                fi
                SELECTED_CONTAINER=$( ${pkgs.docker}/bin/docker ps --format '{{.Names}}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Container" )
                if [ -n "$SELECTED_CONTAINER" ]; then
                    ${pkgs.tmux}/bin/tmux new-window "${pkgs.eternal-terminal}/bin/et \
                    ${config.job.infra.defaultRemoteUser}@$HOST \
                    -c 'docker exec -it $SELECTED_CONTAINER ${dockerContainerShellExecutable}'"
                fi
            fi
        }

        main

        exit 0
    '';
    bookshelfPath = "/home/${userName}/bookshelf";
    rofi_bookshelf = pkgs.writeShellScriptBin "rofi_bookshelf" ''
        . ${pkgs.misc_lib}/bin/misc_lib

        IFS=$'\n'
        BOOKS=$(${pkgs.fd}/bin/fd --full-path ${bookshelfPath} -e pdf -e djvu)

        main() {
            SELECTED_BOOK=$( (show_list "''${BOOKS[@]}") | ${pkgs.rofi}/bin/rofi -dmenu -p "EBook " )
            if [ -n "$SELECTED_BOOK" ]; then
                ${pkgs.zathura}/bin/zathura "$SELECTED_BOOK" & >& /dev/null
            fi
        }

        main

        exit 0
    '';
    # working with screenshots requires xclip, since xsel is MIME-agnostic
    screenshot_active_window = pkgs.writeShellScriptBin "screenshot_active_window" ''
        CONFIGFILE=''${1:-$HOME/.config/screenshots/screenshots.yml}
        SCREENSHOTS_PATH=$(${pkgs.shyaml}/bin/shyaml -gy screenshots.path $CONFIGFILE)
        DATE_FORMAT=$(${pkgs.shyaml}/bin/shyaml -gy screenshots.date_format $CONFIGFILE)
        ${pkgs.maim}/bin/maim -o -i $(${pkgs.xdotool}/bin/xdotool getactivewindow) --format png /dev/stdout | \
            ${pkgs.coreutils}/bin/tee $SCREENSHOTS_PATH/screenshot-$(date $DATE_FORMAT.png | ${pkgs.coreutils}/bin/tr -d '[:cntrl:]') | \
            ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
    '';
    screenshot_region = pkgs.writeShellScriptBin "screenshot_region" ''
        CONFIGFILE=''${1:-$HOME/.config/screenshots/screenshots.yml}
        SCREENSHOTS_PATH=$(${pkgs.shyaml}/bin/shyaml -gy screenshots.path $CONFIGFILE)
        DATE_FORMAT=$(${pkgs.shyaml}/bin/shyaml -gy screenshots.date_format $CONFIGFILE)
        ${pkgs.maim}/bin/maim -o -s --format png /dev/stdout | \
            ${pkgs.coreutils}/bin/tee $SCREENSHOTS_PATH/screenshot-$(date $DATE_FORMAT.png | ${pkgs.coreutils}/bin/tr -d '[:cntrl:]') | \
            ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
    '';
    screenshot_full = pkgs.writeShellScriptBin "screenshot_full" ''
        CONFIGFILE=''${1:-$HOME/.config/screenshots/screenshots.yml}
        SCREENSHOTS_PATH=$(${pkgs.shyaml}/bin/shyaml -gy screenshots.path $CONFIGFILE)
        DATE_FORMAT=$(${pkgs.shyaml}/bin/shyaml -gy screenshots.date_format $CONFIGFILE)
        ${pkgs.maim}/bin/maim -o --format png /dev/stdout | \
            ${pkgs.coreutils}/bin/tee $SCREENSHOTS_PATH/screenshot-$(date $DATE_FORMAT.png | ${pkgs.coreutils}/bin/tr -d '[:cntrl:]') | \
            ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
    '';
    status_uptime = pkgs.writeShellScriptBin "status_uptime" ''
        ${pkgs.procps}/bin/w | ${pkgs.gnused}/bin/sed -r '1 s/.*up *(.*),.*user.*/\1/g;q'
    '';
    show_uptime_info = pkgs.writeShellScriptBin "show_uptime_info" ''
        ${pkgs.dunst}/bin/dunstify -t 7000 "Uptime: $(${status_uptime}/bin/status_uptime)"
    '';
    nasConfigPath = "$HOME/.config/synology/nas.yml";
    mount_nas_volume = pkgs.writeShellScriptBin "mount_nas_volume" ''
        CONFIGFILE=${nasConfigPath}
        if [[ ! -f $CONFIGFILE ]]; then
            ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
            exit 1
        fi
        NAS_HOSTNAME=$(${pkgs.shyaml}/bin/shyaml -gy nas.hostname $CONFIGFILE)
        NAS_ADMIN_LOGIN=$(${pkgs.shyaml}/bin/shyaml -gy nas.users.admin.login $CONFIGFILE)
        NAS_ADMIN_PASSWORD=$(${pkgs.shyaml}/bin/shyaml -gy nas.users.admin.password $CONFIGFILE)
        NAS_MOUNT_PATH=$(${pkgs.shyaml}/bin/shyaml -gy nas.mount.basedir $CONFIGFILE)

        NAS_ONLINE=$(${pkgs.netcat}/bin/nc -z $NAS_HOSTNAME 22 2 -w 2 2>&1)
        if [ -z "$NAS_ONLINE" ]; then
            ${pkgs.dunst}/bin/dunstify -t 7000 -u critical "Cannot access NAS, network error"
            exit 1
        fi

        VOLUME=$1
        ALREADY_MOUNTED=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1 | grep $VOLUME)
        if [[ ! -z $ALREADY_MOUNTED ]]; then
            ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Volume '$VOLUME' already mounted"
            exit 1
        fi
        mkdir -p $NAS_MOUNT_PATH/$VOLUME
        ${pkgs.afpfs-ng}/bin/mount_afp afp://$NAS_ADMIN_LOGIN:$NAS_ADMIN_PASSWORD@$NAS_HOSTNAME/$VOLUME \
            $NAS_MOUNT_PATH/$VOLUME
        if [[ $? -eq 0 ]]; then
            ${pkgs.dunst}/bin/dunstify -t 3000 "Volume '$VOLUME' succesfully mounted"
        else
            ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Error mounting volume '$VOLUME'"
        fi
    '';
    unmount_nas_volume = pkgs.writeShellScriptBin "unmount_nas_volume" ''
        CONFIGFILE=${nasConfigPath}
        if [[ ! -f $CONFIGFILE ]]; then
            ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
            exit 1
        fi
        NAS_MOUNT_PATH=$(${pkgs.shyaml}/bin/shyaml -gy nas.mount.basedir $CONFIGFILE)

        VOLUME=$1
        YET_MOUNTED=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1 | grep $VOLUME)
        if [[ ! -z $YET_MOUNTED ]]; then
            fusermount -u $NAS_MOUNT_PATH/$VOLUME
            ${pkgs.dunst}/bin/dunstify -t 3000 "Volume $VOLUME succesfully unmounted!"
        else
            ${pkgs.dunst}/bin/dunstify -t 7000 "Volume '$VOLUME' already unmounted!"
        fi
    '';
    rofi_mount_nas_volume = pkgs.writeShellScriptBin "rofi_mount_nas_volume" ''
        . ${pkgs.misc_lib}/bin/misc_lib

        CONFIGFILE=${nasConfigPath}
        if [[ ! -f $CONFIGFILE ]]; then
            ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
            exit 1
        fi
        NAS_VOLUMES=$(${pkgs.shyaml}/bin/shyaml -gy nas.volumes $CONFIGFILE)

        nas_volumes=(
        $NAS_VOLUMES
        )

        main() {
            selected_volume=$( (show_list "''${nas_volumes[@]}") | ${pkgs.rofi}/bin/rofi -dmenu -p "Mount: " )
            if [ -n "$selected_volume" ]; then
                ${mount_nas_volume}/bin/mount_nas_volume "$selected_volume"
            fi
        }

        main

        exit 0
    '';
    rofi_unmount_nas_volume = pkgs.writeShellScriptBin "rofi_unmount_nas_volume" ''
        . ${pkgs.misc_lib}/bin/misc_lib

        mounted_nas_volumes=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1)

        main() {
            selected_volume=$( (show_list "''${mounted_nas_volumes[@]}") | ${pkgs.rofi}/bin/rofi -dmenu -p "Unmount: " )
            if [ -n "$selected_volume" ]; then
                ${unmount_nas_volume}/bin/unmount_nas_volume "$selected_volume"
            fi
        }

        main

        exit 0
    '';
    force_unmount_nas = pkgs.writeShellScriptBin "force_unmount_nas" ''
        CONFIGFILE=${nasConfigPath}
        if [[ ! -f $CONFIGFILE ]]; then
            ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Missing config file, exiting"
            exit 1
        fi
        NAS_HOSTNAME=$(${pkgs.shyaml}/bin/shyaml -gy nas.hostname $CONFIGFILE)

        mounted_nas_volumes=$(cat /etc/mtab | grep $NAS_HOSTNAME | cut -d ' '  -f 1)
        for i in "''${mounted_nas_volumes[@]}"
        do
            ${pkgs.unmount_nas_volume}/bin/unmount_nas_volume "$i"
        done
    '';
    rofi_ssh_custom_user = pkgs.writeShellScriptBin "rofi_ssh_custom_user" ''
        . ${pkgs.misc_lib}/bin/misc_lib

        # TODO: provide freeform option or predefined list on Nix level
        USERS=(
          "root"
          "${userName}"
          "${userNamePrevious}"
        )

        main() {
            USER=$( (show_list "''${USERS[@]}") | ${pkgs.rofi}/bin/rofi -dmenu -p "User" )
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
        . ${pkgs.misc_lib}/bin/misc_lib

        ${listOfSetsToShellHashtable (config.job.webjumps ++ config.misc.webjumps) "url" "WEBJUMPS" true}

        main() {
            WEBJUMP=$( (show_mapping_keys "$(declare -p WEBJUMPS)") | ${pkgs.rofi}/bin/rofi -dmenu -p "Jump to" )
            if [ -n "$WEBJUMP" ]; then
                ''${WEBJUMPS[$WEBJUMP]} "$WEBJUMP"
            fi
        }

        main

        exit 0
    '';
    rofi_searchengines_prompt = pkgs.writeShellScriptBin "rofi_searchengines_prompt" ''
        ${listOfSetsToShellHashtable (config.misc.searchEngines) "engine" "SEARCHENGINES" true}

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
        ${listOfSetsToShellHashtable (config.misc.searchEngines) "engine" "SEARCHENGINES" true}

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
            QUERY=$(${pkgs.xsel}/bin/xsel -o)
            if [ -n "$QUERY" ]; then
                URL="''${SEARCHENGINES[$SELECTED_ENGINE]}$QUERY"
                ${config.misc.defaultBrowserCmd} "$URL"
            fi
        }

        main

        exit 0
    '';
in
rec {
    volumeAmount = 10;
    backlightAmount = 10;

    volumeDeltaFraction = 0.1;
    volumeDeltaPercents = 10;
    playerDeltaSeconds = 10;

    rofiWindow = "${pkgs.rofi}/bin/rofi -show window";
    screenshotActiveWindow = "${screenshot_active_window}/bin/screenshot_active_window";
    lockScreen = "${pkgs.i3lock-color}/bin/i3lock-color -c 232729 && ${pkgs.xorg.xset}/bin/xset dpms force off";
    screenshotFull = "${screenshot_full}/bin/screenshot_full";
    screenshotRegion = "${screenshot_region}/bin/screenshot_region";
    rofiSearchenginesSelection = "${rofi_searchengines_selection}/bin/rofi_searchengines_selection";
    rofiSearchenginesPrompt = "${rofi_searchengines_prompt}/bin/rofi_searchengines_prompt";
    rofiDockerShell = "${rofi_docker_shell}/bin/rofi_docker_shell";
    rofiServiceJournal = "${rofi_service_journal}/bin/rofi_service_journal";
    rofiRemoteDockerLogs = "${pkgs.rofi_remote_docker_logs}/bin/rofi_remote_docker_logs";
    rofiDockerStacksInfo = "${pkgs.rofi_docker_stacks_info}/bin/rofi_docker_stacks_info";
    rofiDbms = "${pkgs.rofi_dbms}/bin/rofi_dbms";
    rofiSshCustomUser = "${rofi_ssh_custom_user}/bin/rofi_ssh_custom_user";
    rofiSsh = "${pkgs.rofi}/bin/rofi -show ssh";
    rofiWebjumps = "${rofi_webjumps}/bin/rofi_webjumps";
    rofiCombiRun = "${pkgs.rofi}/bin/rofi -combi-modi drun,run -show combi -modi combi";
    showCurrentSystemHash = "${pkgs.show_current_system_hash}/bin/show_current_system_hash";
    networkmanagerMenu = "${pkgs.networkmanager_dmenu}/bin/networkmanager_dmenu"; # using rofi, despite naming
    mergeXresources = "${pkgs.xorg.xrdb}/bin/xrdb -merge $HOME/.Xresources";
    rofiAutorandrProfiles = "${rofi_autorandr_profiles}/bin/rofi_autorandr_profiles";
    rofiDockerContainerTraits = "${rofi_docker_container_traits}/bin/rofi_docker_container_traits";
    rofiExtraHostsTraits = "${pkgs.rofi_extra_hosts_traits}/bin/rofi_extra_hosts_traits";
    rofiMountNasVolume = "${rofi_mount_nas_volume}/bin/rofi_mount_nas_volume";
    rofiTmuxpSessions = "${rofi_tmuxp_sessions}/bin/rofi_tmuxp_sessions";
    rofiUnmountNasVolume = "${rofi_unmount_nas_volume}/bin/rofi_unmount_nas_volume";
    rofiBookshelf = "${rofi_bookshelf}/bin/rofi_bookshelf";
    showUptimeInfo = "${show_uptime_info}/bin/show_uptime_info";
    rofiBukuAdd = "${pkgs.rofi_buku_add}/bin/rofi_buku_add";
    jogWifi = "${pkgs.networkmanager}/bin/nmcli radio wifi off && ${pkgs.networkmanager}/bin/nmcli radio wifi on";
    rofiPass = "${pkgs.rofi-pass}/bin/rofi-pass";
    jogEmacs = "${pkgs.procps}/bin/pkill -SIGUSR2 emacs";


    sctlRestart = "${pkgs.systemd}/bin/systemctl restart";
    sctlStop = "${pkgs.systemd}/bin/systemctl stop";
    sctlUserRestart = "${pkgs.systemd}/bin/systemctl --user restart";
    sctlUserStop = "${pkgs.systemd}/bin/systemctl --user stop";
    pctl = "${pkgs.playerctl}/bin/playerctl --all-players";
    pactl = "${pkgs.pulseaudio}/bin/pactl";
    pctlRaiseVolume = "${pctl} volume ${builtins.toString volumeDeltaFraction}+";
    pctlLowerVolume = "${pctl} volume ${builtins.toString volumeDeltaFraction}-";
    pulseRaiseVolume = "${pactl} set-sink-volume 0 +${builtins.toString volumeDeltaPercents}%";
    pulseLowerVolume = "${pactl} set-sink-volume 0 -${builtins.toString volumeDeltaPercents}%";
    pctlSeekForward = "${pctl} position ${builtins.toString playerDeltaSeconds}+";
    pctlSeekBackward = "${pctl} position ${builtins.toString playerDeltaSeconds}-";
    light = "${pkgs.light}/bin/light";
    brightnessUp = "${light} -A ${toString backlightAmount}";
    brightnessDown = "${light} -U ${toString backlightAmount}";
    brightnessMax = "${light} -S 100";
    brightnessMin = "${light} -S 20";

    xmobar = "${pkgs.xmobar}/bin/xmobar";
    fontDefault = "${config.sys.fonts.main.name}:${config.sys.fonts.main.weightKeyword}=${config.sys.fonts.main.weight}:${config.sys.fonts.main.sizeKeyword}=${config.sys.fonts.size.Dzen}";
    fontTabbed = "${config.sys.fonts.main.name}:${config.sys.fonts.main.weightKeyword}=${config.sys.fonts.main.weight}:${config.sys.fonts.main.sizeKeyword}=${config.sys.fonts.size.Dzen}";
}
