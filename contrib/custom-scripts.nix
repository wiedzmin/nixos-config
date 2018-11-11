{config, pkgs, ...}:

let
    bookshelfPath = "${config.users.extraUsers.alex3rd.home}/bookshelf";
    autorandrProfilesPath = "${config.users.extraUsers.alex3rd.home}/.config/autorandr";
    tmuxpSessionsPath = "${config.users.extraUsers.alex3rd.home}/tmuxp";
    bookReaderUsePdftools = true;
    currentUser = "alex3rd";
    previousUser = "octocat";
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            hddtemp-script = pkgs.writeShellScriptBin "hddtemp-script" ''
                ${pkgs.netcat}/bin/nc localhost 7634 | ${pkgs.gawk}/bin/awk -F\| '{print ($4)}'
            '';
            status_bat_info = pkgs.writeShellScriptBin "status_bat_info" ''
                BAT_NAME="BAT0"
                UPOWER_ENERGY_FULL_DESIGN=$(${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "energy-full-design:" | cut -f 14 -d " ")
                UPOWER_ENERGY_FULL_FACT=$(${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "energy-full:" | cut -f 14 -d " ")
                UPOWER_PERCENTAGE=$(${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "percentage:" | cut -f 15 -d " ")
                UPOWER_STATE=$(${pkgs.upower}/bin/upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "state:" | cut -f 20 -d " ")
                case $UPOWER_STATE in
                    fully-charged)
                        st="="
                        ;;
                    discharging)
                        st="▼"
                        ;;
                    charging)
                        st="▲"
                        ;;
                esac
                echo $st$UPOWER_PERCENTAGE
            '';
            status_uptime = pkgs.writeShellScriptBin "status_uptime" ''
                ${pkgs.procps}/bin/w | ${pkgs.gnused}/bin/sed -r '1 s/.*up *(.*),.*user.*/\1/g;q'
            '';
            # see https://blog.jeaye.com/2017/07/30/nixos-revisited/
            optimize-nix = pkgs.writeShellScriptBin "optimize-nix" ''
                set -eu

                # Delete everything from this profile that isn't currently needed
                ${pkgs.nix}/bin/nix-env --delete-generations old

                # Delete generations older than a week
                ${pkgs.nix}/bin/nix-collect-garbage
                ${pkgs.nix}/bin/nix-collect-garbage --delete-older-than 7d

                # Optimize
                ${pkgs.nix}/bin/nix-store --gc --print-dead
                ${pkgs.nix}/bin/nix-store --optimise
            '';
            docker-machine-export = pkgs.writeShellScriptBin "docker-machine-export" ''
                if [ -z "$1" ]; then
                  echo "Usage: machine-export.sh MACHINE_NAME"
                  echo ""
                  echo "Exports the specified docker-machine to a MACHINE_NAME.zip file"
                  echo "Note: This script requires you to have the same \$MACHINE_STORAGE_PATH/certs available on all host systems"
                  exit 0
                fi

                machine_name=$1

                ${pkgs.docker-machine}/bin/docker-machine status $machine_name 2>&1 > /dev/null
                if [ $? -ne 0 ]; then
                  echo "No such machine found"
                  exit 1
                fi

                set -e

                MACHINE_STORAGE_PATH="$(MACHINE_STORAGE_PATH:-"$HOME/.docker/machine")"
                machine_path="$MACHINE_STORAGE_PATH/machines/$machine_name"
                tmp_path="/tmp/machine-export-$(date +%s%3)"

                # copy to /tmp and strip out $MACHINE_STORAGE_PATH
                ${pkgs.coreutils}/bin/mkdir -p $tmp_path
                ${pkgs.coreutils}/bin/cp -r "$machine_path" "$tmp_path"
                ${pkgs.perl}/bin/perl -pi -e "s|$MACHINE_STORAGE_PATH|__MACHINE__STORAGE_PATH__|g" $tmp_path/$machine_name/config.json

                # create zip
                ${pkgs.coreutils}/bin/rm -f "$machine_name.zip"
                ${pkgs.zip}/bin/zip -rj "$machine_name.zip" "$tmp_path/$machine_name" > /dev/null

                echo "Exported machine to $machine_name.zip"

                # cleanup
                ${pkgs.coreutils}/bin/rm -rf $tmp_path
            '';
            docker-machine-import = pkgs.writeShellScriptBin "docker-machine-import" ''
                set -e

                if [ -z "$1" ]; then
                  echo "Usage: docker-machine-import.sh MACHINE_NAME.zip"
                  echo ""
                  echo "Imports an exported machine from a MACHINE_NAME.zip file"
                  echo "Note: This script requires you to have the same \$MACHINE_STORAGE_PATH/certs available on all host systems"
                  exit 0
                fi

                machine_archive="$1"
                machine_name="$machine_archive/.zip/"
                MACHINE_STORAGE_PATH="$(MACHINE_STORAGE_PATH:-"$HOME/.docker/machine")"
                machine_path="$MACHINE_STORAGE_PATH/machines/$machine_name"

                if [ -d "$machine_path" ]; then
                  echo "$machine_name already exists"
                  exit 1
                fi

                ${pkgs.coreutils}/bin/rm -rf "$machine_name"
                ${pkgs.unzip}/bin/unzip "$machine_archive" -d "$machine_name" > /dev/null
                ${pkgs.perl}/bin/perl -pi -e "s|__MACHINE__STORAGE_PATH__|$MACHINE_STORAGE_PATH|g" $machine_name/config.json
                ${pkgs.coreutils}/bin/mv "$machine_name" "$MACHINE_STORAGE_PATH/machines"

                echo "Imported $machine_name to docker-machine ($machine_path)"
            '';
            wifi-status = pkgs.writeShellScriptBin "wifi-status" ''
                essid=`${pkgs.wirelesstools}/bin/iwgetid -r`
                strength=$((`awk 'NR==3 {print substr($3, 1, length($3)-1)}' /proc/net/wireless`*100/70))
                quality_color=
                case 1 in
                    $((strength < 30)))
                        quality_color=red
                        ;;
                    $((strength >= 30 && strength < 70)))
                        quality_color=yellow
                        ;;
                    $((strength >= 70 && strength <= 100)))
                        quality_color=green
                        ;;
                esac
                echo $essid: "<fc=$quality_color>$strength</fc>%"
            '';
            systemctl-status = pkgs.writeShellScriptBin "systemctl-status" ''
                if [ -z "$1" ]
                then
                    echo -e ""
                else
                    status=`${pkgs.systemd}/bin/systemctl status $1 | awk 'NR==3 {print $2}'`
                    if [ $status == "inactive" ]
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
            rescale-wallpaper = pkgs.writeShellScriptBin "rescale-wallpaper" ''
                ${pkgs.feh}/bin/feh --bg-fill ${config.x11.wallpapers_dir}/${config.x11.current_wallpaper}
            '';
            shell-capture = pkgs.writeShellScriptBin "shell-capture" ''
                TEMPLATE="$1"
                if [[ ! -n $TEMPLATE ]]
                then
                    exit 1
                fi
                TITLE="$*"
                if [[ -n $TMUX ]]
                then
                    TITLE=$(tmux display-message -p '#S')
                    tmux send -X copy-pipe-and-cancel "xclip -i -selection primary"
                fi

                if [[ -n $TITLE ]]
                then
                    emacsclient -n "org-protocol://capture?template=$TEMPLATE&title=$TITLE"
                else
                    emacsclient -n "org-protocol://capture?template=$TEMPLATE"
                fi
            '';
            is-git-repo = pkgs.writeShellScriptBin "is-git-repo" ''
                 ${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null
            '';
            git-fetch-batch = pkgs.writeShellScriptBin "git-fetch-batch" ''
                set -euo pipefail
                BASE_PATH=$1
                if [[ ! -n $BASE_PATH ]]
                then
                    exit 1
                fi

                set -x

                for item in $(find $BASE_PATH -type d -name ".git")
                do
                    cd $item/..
                    echo "Processing $(basename `pwd`)..."
                    REMOTES=$(${pkgs.git}/bin/git remote)
                    if [[ "origin" =~ $REMOTES ]]; then
                        ${pkgs.git}/bin/git fetch origin &
                        wait $!
                        ${pkgs.git}/bin/git rebase --autostash &
                        wait $!
                    fi
                done
            '';
            git-save-wip-batch = pkgs.writeShellScriptBin "git-save-wip-batch" ''
                GIT_REPOS=
                ADJUST_GIT_REPOS=
                case $# in
                    0 )
                        exit 1
                        ;;
                    1 )
                        BASE_PATH=$1
                        GIT_REPOS=$(find $BASE_PATH -type d -name ".git")
                        ADJUST_GIT_REPOS=1
                        ;;
                    * )
                        GIT_REPOS=$@
                        ADJUST_GIT_REPOS=0
                        ;;
                esac

                if [[ $(DISPLAY=:0 ${pkgs.xprintidle-ng}/bin/xprintidle-ng) -lt $((3600*1000)) ]]; then
                    exit 0
                fi

                set -x

                for item in $GIT_REPOS
                do
                    echo "$item"
                    if [[ $ADJUST_GIT_REPOS -eq 1 ]]; then
                         cd $item/..
                    else
                         cd $item
                    fi
                    echo "Processing $(basename `pwd`)..."
                    ${pkgs.gitAndTools.stgit}/bin/stg init
                    ${pkgs.gitAndTools.stgit}/bin/stg repair

                    if [[ ! -z $(${pkgs.git}/bin/git status --porcelain) ]]; then
                        ${pkgs.git}/bin/git add .
                        PATCH_DESC="WIP $(date -R)"
                        ${pkgs.gitAndTools.stgit}/bin/stg new -m "$PATCH_DESC"
                        ${pkgs.gitAndTools.stgit}/bin/stg refresh
                    fi
                done
            '';
            mount_nas_volume = pkgs.writeShellScriptBin "mount_nas_volume" ''
                NAS_ONLINE=$(${pkgs.netcat}/bin/nc -z ${config.fs.storage.hostname} 22 2 -w 2 2>&1)
                if [ -z "$NAS_ONLINE" ]; then
                    ${pkgs.libnotify}/bin/notify-send -t 7000 -u critical "Cannot access NAS, network error"
                    exit 1
                fi

                VOLUME=$1
                ALREADY_MOUNTED=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1 | grep $VOLUME)
                if [[ ! -z $ALREADY_MOUNTED ]]; then
                    ${pkgs.libnotify}/bin/notify-send -t 5000 -u critical "Volume '$VOLUME' already mounted"
                    exit 1
                fi
                mkdir -p ${config.fs.storage.local_mount_base}/$VOLUME
                ${pkgs.afpfs-ng}/bin/mount_afp \
                    afp://${config.fs.storage.primary_user}:${config.fs.storage.primary_user_password}@${config.fs.storage.hostname}/$VOLUME \
                    ${config.fs.storage.local_mount_base}/$VOLUME
                if [[ $? -eq 0 ]]; then
                    ${pkgs.libnotify}/bin/notify-send -t 3000 "Volume '$VOLUME' succesfully mounted"
                else
                    ${pkgs.libnotify}/bin/notify-send -t 5000 -u critical "Error mounting volume '$VOLUME'"
                fi
            '';
            unmount_nas_volume = pkgs.writeShellScriptBin "unmount_nas_volume" ''
                VOLUME=$1
                YET_MOUNTED=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1 | grep $VOLUME)
                if [[ ! -z $YET_MOUNTED ]]; then
                    fusermount -u ${config.fs.storage.local_mount_base}/$VOLUME
                    ${pkgs.libnotify}/bin/notify-send -t 3000 "Volume $VOLUME succesfully unmounted!"
                else
                    ${pkgs.libnotify}/bin/notify-send -t 7000 "Volume '$VOLUME' already unmounted!"
                fi
            '';
            maybe_ssh_host = pkgs.writeShellScriptBin "maybe_ssh_host" ''
                # tmux: pane_tty: pts/5
                pane_tty=$(${pkgs.tmux}/bin/tmux display-message -p '#{pane_tty}' | ${pkgs.coreutils}/bin/cut -c 6-)

                # get IP/hostname according to pane_tty
                remote_session=$(${pkgs.procps}/bin/pgrep -t $pane_tty -a -f "ssh " | ${pkgs.gawk}/bin/awk 'NF>1{print $NF}')

                if [[ "$remote_session" != "" ]]; then
                    echo $remote_session
                else
                    echo $(whoami)@$(${pkgs.nettools}/bin/hostname)
                fi
            '';
            pass_curl_helper = pkgs.writeShellScriptBin "pass_curl_helper" ''
                PASS_ENTRY=$1
                echo $(${pkgs.pass}/bin/pass $PASS_ENTRY | ${pkgs.coreutils}/bin/tr '\n' ' ' | \
                     ${pkgs.gawk}/bin/awk '{print $3 ":" $1}')
            '';
            bitbucket_team_contributor_repos = pkgs.writeShellScriptBin "bitbucket_team_contributor_repos" ''
                # TODO: think of keyword args or likewise
                TEAM=$1
                PROJECTS_EXCLUDE=$2
                CREDENTIALS=$(${pkgs.pass_curl_helper}/bin/pass_curl_helper alex3rd/webservices/social/programming/bitbucket.com.web)
                RESULT=$(${pkgs.curl}/bin/curl -s -u \
                       $CREDENTIALS "https://api.bitbucket.org/2.0/repositories?role=contributor&pagelen=200" | \
                       ${pkgs.jq}/bin/jq -r '.values[] | select(.project.name != null) | "\(.links.clone[0].href)~\(.project.name)"')
                if [[ ! -z $TEAM ]]; then
                    RESULT=$(printf "%s\n" $RESULT | grep $TEAM)
                fi
                if [[ ! -z $PROJECTS_EXCLUDE ]]; then
                    GREP_CLAUSES=$(echo $PROJECTS_EXCLUDE | ${pkgs.gnused}/bin/sed "s/,/\|/g")
                    RESULT=$(printf "%s\n" $RESULT | grep -i -v -E $GREP_CLAUSES)
                fi
                for repo in $RESULT; do
                    echo $repo | ${pkgs.coreutils}/bin/cut -f1 -d~
                done
            '';
            tar_encrypt = pkgs.writeShellScriptBin "tar_encrypt" ''
                # encrypts files/directory with default key identity to tar stream

                IN=$1
                if [[ -z $IN ]]; then
                    echo "no input file provided, exiting"
                    exit 1
                fi
                SOURCE_BASENAME=''${IN%.*}
                ${pkgs.gnupg}/bin/gpgtar -r "${config.common.primaryGpgKeyID}" -u "${config.common.primaryGpgKeyID}" \
                                         -e $IN > $SOURCE_BASENAME.tar.gpg
            '';
            tar_decrypt = pkgs.writeShellScriptBin "tar_decrypt" ''
                # decrypts files/directory from tar stream with default key identity

                IN=$1
                if [[ -z $IN ]]; then
                    echo "no input file provided, exiting"
                    exit 1
                fi
                WD=`pwd`
                SOURCE_BASENAME=''${IN%.*}
                ${pkgs.gnupg}/bin/gpg --output $WD/$SOURCE_BASENAME_decrypted.tar --decrypt $IN && \
                                        tar xf $WD/$SOURCE_BASENAME_decrypted.tar && \
                                        rm $WD/$SOURCE_BASENAME_decrypted.tar
            '';
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
                    USER=$( (ask_for_user) | ${pkgs.rofi}/bin/rofi -dmenu -p "User: " )
                    if [ ! -n "$USER" ]; then
                        exit 1
                    fi
                    HOST=$( cat /etc/hosts | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.rofi}/bin/rofi -dmenu -p "Host: " )
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
                    webjump=$( (list_webjumps) | ${pkgs.rofi}/bin/rofi -dmenu -p "Jump to: " )
                    if [ -n "$webjump" ]; then
                        ''${webjumps[$webjump]} "$webjump"
                    fi
                }

                main

                exit 0
            '';
            watch_dunst = pkgs.writeShellScriptBin "watch_dunst" ''
                DUNST_COUNT=$(${pkgs.procps}/bin/pgrep -x -c .dunst-wrapped)
                if [ $DUNST_COUNT -gt 1 ]; then
                    ${pkgs.logger}/bin/logger "Too much dunsts ($DUNST_COUNT), shooting down..."
                    pkill -x -KILL .dunst-wrapped
                    exit 0
                fi
            '';
            rofi_mount_nas_volume = pkgs.writeShellScriptBin "rofi_mount_nas_volume" ''
                nas_volumes=(
                ${builtins.concatStringsSep "\n" config.misc.nas_volumes}
                )

                list_nas_volumes() {
                    for i in "''${nas_volumes[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    selected_volume=$( (list_nas_volumes) | ${pkgs.rofi}/bin/rofi -dmenu -p "Mount: " )
                    if [ -n "$selected_volume" ]; then
                        ${pkgs.mount_nas_volume}/bin/mount_nas_volume "$selected_volume"
                    fi
                }

                main

                exit 0
            '';
            rofi_unmount_nas_volume = pkgs.writeShellScriptBin "rofi_unmount_nas_volume" ''
                mounted_nas_volumes=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1)

                list_mounted_volumes() {
                    for i in "''${mounted_nas_volumes[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    selected_volume=$( (list_mounted_volumes) | ${pkgs.rofi}/bin/rofi -dmenu -p "Unmount: " )
                    if [ -n "$selected_volume" ]; then
                        ${pkgs.unmount_nas_volume}/bin/unmount_nas_volume "$selected_volume"
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
                    selected_engine=$( (list_searchengines) | ${pkgs.rofi}/bin/rofi -dmenu -i -p "Search: " | ${pkgs.gawk}/bin/awk '{print $2}')
                    if [ ! -n "$selected_engine" ]; then
                        exit 1
                    fi
                    query=$( (echo ) | rofi  -dmenu -matching fuzzy -location 0 -p "Query: " )
                    if [ -n "$query" ]; then
                        url="''${searchengines[$selected_engine]}$query"
                        ${config.misc.defaultBrowserCmd} "$url"
                    fi
                }

                main

                exit 0
            '';
       };
    };
}
