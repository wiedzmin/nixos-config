{config, pkgs, ...}:

let
    screenshotsPath = "/home/alex3rd/screenshots";
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            shell-capture = pkgs.writeShellScriptBin "shell-capture" ''
                TEMPLATE="$1"
                if [[ ! -n $TEMPLATE ]]
                then
                    exit 1
                fi
                TITLE="$*"
                if [[ -n $TMUX ]]
                then
                    TITLE=$(${pkgs.tmux}/bin/tmux display-message -p '#S')
                    ${pkgs.tmux}/bin/tmux send -X copy-pipe-and-cancel "${pkgs.xsel}/bin/xsel -i --primary"
                fi

                if [[ -n $TITLE ]]
                then
                    emacsclient -n "org-protocol://capture?template=$TEMPLATE&title=$TITLE"
                else
                    emacsclient -n "org-protocol://capture?template=$TEMPLATE"
                fi
            '';
            pass_curl_helper = pkgs.writeShellScriptBin "pass_curl_helper" ''
                PASS_ENTRY=$1
                echo $(${pkgs.pass}/bin/pass $PASS_ENTRY | ${pkgs.coreutils}/bin/tr '\n' ' ' | \
                     ${pkgs.gawk}/bin/awk '{print $3 ":" $1}')
            '';
            lockscreen = pkgs.writeShellScriptBin "lockscreen" ''
                ${pkgs.xkb-switch}/bin/xkb-switch -s us && ${pkgs.i3lock-color}/bin/i3lock-color -c 232729 && ${pkgs.xorg.xset}/bin/xset dpms force off
            '';
            tar_encrypt = pkgs.writeShellScriptBin "tar_encrypt" ''
                IN=$1
                KEY_ID=$2
                if [[ -z $IN ]]; then
                    echo "no input file provided, exiting"
                    exit 1
                fi
                SOURCE_BASENAME=''${IN%.*}
                ${pkgs.gnupg}/bin/gpgtar -r "$KEY_ID" -u "$KEY_ID" -e $IN > $SOURCE_BASENAME.tar.gpg
            '';
            tar_decrypt = pkgs.writeShellScriptBin "tar_decrypt" ''
                IN=$1
                if [[ -z $IN ]]; then
                    echo "no input file provided, exiting"
                    exit 1
                fi
                WD=`pwd`
                SOURCE_BASENAME=''${IN%.*}
                ${pkgs.gnupg}/bin/gpg --output $WD/$SOURCE_BASENAME_decrypted.tar --decrypt $IN && \
                                        ${pkgs.gnutar}/bin/tar xf $WD/$SOURCE_BASENAME_decrypted.tar && \
                                        rm $WD/$SOURCE_BASENAME_decrypted.tar
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
            order_screenshots = pkgs.writeShellScriptBin "order_screenshots" ''
                declare -A REGEXP_TO_DATECMD
                REGEXP_TO_DATECMD=(
                  ["screenshot-[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}_[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:11:4} ''${FILENAME:16:2} ''${FILENAME:19:2}'
                  ["screenshot-[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:11:4} ''${FILENAME:16:2} ''${FILENAME:19:2}'
                  ["[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}_[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}_[0-9]\+x[0-9]\+_scrot"]='echo ''${FILENAME:0:4} ''${FILENAME:5:2} ''${FILENAME:8:2}'
                  ["[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{6\}_[0-9]\+x[0-9]\+_scrot"]='echo ''${FILENAME:0:4} ''${FILENAME:5:2} ''${FILENAME:8:2}'
                  ["screenshot-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{4\}-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:17:4} ''${FILENAME:14:2} ''${FILENAME:11:2}'
                  # ["screenshot-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}\\ [0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}"]='echo ''${FILENAME:20:4} ''${FILENAME:25:2} ''${FILENAME:28:2}'
                  ["screenshot-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}_[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}"]='echo ''${FILENAME:20:4} ''${FILENAME:25:2} ''${FILENAME:28:2}'
                )

                for regexp in "''${!REGEXP_TO_DATECMD[@]}"
                do
                    FILELIST=$(ls -a ${screenshotsPath} | grep -e $regexp)
                    DATECMD=''${REGEXP_TO_DATECMD[$regexp]}
                    for FILENAME in $FILELIST
                    do
                        DATE_PART=($(eval $DATECMD))
                        YEAR=''${DATE_PART[0]}
                        MONTH=''${DATE_PART[1]}
                        DAY=''${DATE_PART[2]}
                        DEST_PATH=${screenshotsPath}/$YEAR/$MONTH/$DAY
                        mkdir -p ${screenshotsPath}/$YEAR/$MONTH/$DAY
                        echo "moving $FILENAME to $DEST_PATH"
                        mv ${screenshotsPath}/$FILENAME ${screenshotsPath}/$YEAR/$MONTH/$DAY
                    done
                done

                exit 0
            '';
       };
    };
}
