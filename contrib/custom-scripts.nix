{config, pkgs, ...}:

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
            status_cpu_temp = pkgs.writeShellScriptBin "status_cpu_temp" ''
                t=$(${pkgs.lm_sensors}/bin/sensors | ${pkgs.gawk}/bin/awk '/Core\ 0/ {gsub(/\+/,"",$3); gsub(/\..+/,"",$3)    ; print $3}')
                tc=$C0
                case 1 in
                    $((t <= 50)))
                        tc=$C2
                        ;;
                    $((t >= 87)))
                        tc=$C3
                        ;;
                    $((t >= 105)))
                        tc=$C4
                        ;;
                esac
                echo "$t°C"
            '';
            status_uptime = pkgs.writeShellScriptBin "status_uptime" ''
                ${pkgs.procps}/bin/w | ${pkgs.gnused}/bin/sed -r '1 s/.*up *(.*),.*user.*/\1/g;q'
            '';
            zip2targz = pkgs.writeShellScriptBin "zip2targz" ''
                tmpdir=`${pkgs.coreutils}/bin/mktemp -d`
                #Copy the zip to the temporary directory
                ${pkgs.coreutils}/bin/cp "$1" $tmpdir/
                #Unzip
                (cd $tmpdir && ${pkgs.unzip}/bin/unzip -q "$1")
                #Remove the original zipfile because we don't want that to be tar'd
                ${pkgs.coreutils}/bin/rm "$tmpdir/$1"
                #Tar the files
                outfilename=$(echo "$1" | ${pkgs.util-linux}/bin/rev | ${pkgs.coreutils}/bin/cut -d. -f2- | ${pkgs.util-linux}/bin/rev).tar
                (cd $tmpdir && ${pkgs.gnutar}/bin/tar cf "$outfilename" *)
                ${pkgs.coreutils}/bin/mv "$tmpdir/$outfilename" .
                ${pkgs.gzip}/bin/gzip ./$outfilename
                #Remove the temporary directory
                ${pkgs.coreutils}/bin/rm -rf $tmpdir
                #Print what we did
                echo "Converted $1 to $outfilename.gz"
            '';
            # see https://blog.jeaye.com/2017/07/30/nixos-revisited/
            optimize-nix = pkgs.writeShellScriptBin "optimize-nix" ''
                set -eu

                # Delete everything from this profile that isn't currently needed
                nix-env --delete-generations old

                # Delete generations older than a week
                nix-collect-garbage
                nix-collect-garbage --delete-older-than 7d

                # Optimize
                nix-store --gc --print-dead
                nix-store --optimise
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
                strength=`awk 'NR==3 {print substr($3, 1, length($3)-1)}' /proc/net/wireless`
                quality_color=
                case 1 in
                    $((strength < 25)))
                        quality_color=red
                        ;;
                    $((strength >= 25 && strength < 50)))
                        quality_color=yellow
                        ;;
                    $((strength >= 50 && strength < 70)))
                        quality_color=green
                        ;;
                    $((strength >= 70)))
                        quality_color=purple
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
                VOLUME=$1
                ALREADY_MOUNTED=$(cat /etc/mtab | grep catscan | cut -d ' '  -f 1 | grep $VOLUME)
                if [[ ! -z $ALREADY_MOUNTED ]]; then
                    ${pkgs.libnotify}/bin/notify-send -u critical "Volume '$VOLUME' already mounted"
                    exit 1
                fi
                mkdir -p ${config.fs.storage.local_mount_base}/$VOLUME
                ${pkgs.afpfs-ng}/bin/mount_afp \
                    afp://${config.fs.storage.primary_user}:${config.fs.storage.primary_user_password}@${config.fs.storage.hostname}/$VOLUME \
                    ${config.fs.storage.local_mount_base}/$VOLUME
                if [[ -z $? ]]; then
                    ${pkgs.libnotify}/bin/notify-send "Volume '$VOLUME' succesfully mounted"
                else
                    ${pkgs.libnotify}/bin/notify-send -u critical "Error mounting volume '$VOLUME'"
                fi
            '';
            unmount_nas_volume = pkgs.writeShellScriptBin "unmount_nas_volume" ''
                VOLUME=$1
                fusermount -u ${config.fs.storage.local_mount_base}/$VOLUME
                ${pkgs.libnotify}/bin/notify-send "Volume $VOLUME succesfully unmounted!"
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
            git_checkout_fuzzy = pkgs.writeShellScriptBin "git_checkout_fuzzy" ''
                # checkout git branch (including remotes)
                if [[ ! -z $(${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null) ]]; then
                    branches=$(${pkgs.git}/bin/git branch --all -vv) &&
                        branch=$(echo "$branches" | ${pkgs.fzf}/bin/fzf-tmux -- \
                                      --ansi -d $(( 2 + $(wc -l <<< "$branches") )) +m) || exit
                        if [ "x$branch" != "x" ]
                        then
                            ${pkgs.git}/bin/git checkout $(echo "$branch" | ${pkgs.gawk}/bin/awk '{print $1}' |
                            ${pkgs.gnused}/bin/sed "s/.* //" | ${pkgs.gnused}/bin/sed "s#remotes/[^/]*/##")
                        fi
                fi
            '';
            git_checkout_commit_fuzzy = pkgs.writeShellScriptBin "git_checkout_commit_fuzzy" ''
                # checkout git commit
                if [[ ! -z $(${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null) ]]; then
                    commits=$(${pkgs.git}/bin/git log --pretty=oneline --abbrev-commit --reverse)
                    commit=$(echo "$commits" | ${pkgs.fzf}/bin/fzf --tac +s +m -e)
                    if [ "x$commit" != "x" ]
                    then
                        ${pkgs.git}/bin/git checkout $(echo "$commit" | ${pkgs.gnused}/bin/sed "s/ .*//")
                    fi
                fi
            '';
            git_browse_commits_fuzzy = pkgs.writeShellScriptBin "git_browse_commits_fuzzy" ''
                # git commit browser
                if [[ ! -z $(${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null) ]]; then
                    while out=$(${pkgs.git}/bin/git log --decorate=short --graph --oneline --color=always |
                                ${pkgs.fzf}/bin/fzf --ansi --multi --no-sort --reverse --query="$q" --print-query);
                                do
                        q=$(head -1 <<< "$out")
                        while read sha; do
                            [ -n "$sha" ] && ${pkgs.git}/bin/git show --color=always $sha | less -R
                        done < <(${pkgs.gnused}/bin/sed '1d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" |
                        ${pkgs.gawk}/bin/awk '{print $1}')
                    done
                fi
            '';
            kill_process_fuzzy = pkgs.writeShellScriptBin "kill_process_fuzzy" ''
                # kill process
                pid=$(ps -ef | ${pkgs.gnused}/bin/sed 1d | ${pkgs.fzf}/bin/fzf | ${pkgs.gawk}/bin/awk '{print $2}')
                if [ "x$pid" != "x" ]
                then
                    kill -''${1:-9} $pid
                fi
            '';
            edit_file_fuzzy = pkgs.writeShellScriptBin "edit_file_fuzzy" ''
                # Open the selected file with the default editor
                # - Bypass fuzzy finder if there's only one match (--select-1)
                # - Exit if there's no match (--exit-0)
                openfile=$(${pkgs.ripgrep}/bin/rg -g "*" --files | ${pkgs.fzf}/bin/fzf-tmux -d''${FZF_TMUX_HEIGHT:-40%})
                [[ -n "$openfile" ]] && ''${EDITOR:-vim} "''${openfile}"
            '';
       };
    };
}
