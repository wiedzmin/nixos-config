{config, pkgs, ...}:

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
                    ${pkgs.tmux}/bin/tmux send -X copy-pipe-and-cancel "${pkgs.xclip}/bin/xclip -i -selection primary"
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
            watch_dunst = pkgs.writeShellScriptBin "watch_dunst" ''
                FORCE=$1
                DUNST_COUNT=$(${pkgs.procps}/bin/pgrep -x -c .dunst-wrapped)
                if [ $DUNST_COUNT -gt 1 ]; then
                    ${pkgs.logger}/bin/logger "Too much dunsts ($DUNST_COUNT), shooting down..."
                    ${pkgs.procps}/bin/pkill -x -KILL .dunst-wrapped
                    exit 0
                elif [ "$FORCE" == "force" ]; then
                    ${pkgs.procps}/bin/pkill -x -KILL .dunst-wrapped
                    exit 0
                fi
            '';
            lockscreen = pkgs.writeShellScriptBin "lockscreen" ''
                ${pkgs.xkblayout-state}/bin/xkblayout-state set 0; ${pkgs.i3lock-color}/bin/i3lock-color -c 232729; ${pkgs.xorg.xset}/bin/xset dpms force off
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
       };
    };
}
